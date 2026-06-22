#
#
#           Lengc Scalar Replacement of Aggregates (SROA)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## A local object that is *only ever* touched through `o.field` is replaced by one
## scalar local per field; every `o.field` becomes a direct scalar:
##
## ```
## var o = T(f: a, g: b)        var `sroa.1 = a
## use o.f                  →   var `sroa.2 = b
## o.g = c                      use `sroa.1
##                              `sroa.2 = c
## ```
##
## This is the dual of `copyprop`: copyprop tracks whole-variable identity (flow
## sensitively); SROA tracks per-field *decomposition* (flow-insensitively — once
## an object is provably scalarizable, every field access is rewritten regardless
## of control flow). So there is no `Tracker` here; the whole thing is two linear
## scans plus a patch emit.
##
## ## Legality — why this needs no aliasing / type machinery
##
## We scalarize a local `var o = (oconstr …)` iff:
##
## 1. **Non-escaping.** `o` appears *only* as the base of a `(dot o field)`. Any
##    bare `o` (whole-object read, `o = x`, `(call f o)`, `(ret o)`), and any
##    `addr` whose root is `o` (`addr o`, `addr o.f`), disqualifies it. This single
##    rule also rules out objects with destructors or non-trivial copies: their
##    lowered `=destroy` / `=copy` appears as `addr o` / a whole-object use, so they
##    never survive — and the surviving fields are therefore trivially destructible.
##
## 2. **Every accessed field is initialised by the constructor.** If `o.x` is read
##    but `x` is not a `(kv x …)` of the `oconstr`, we bail. For a `case`/variant
##    object this is exactly what protects us: reading a field of the *other*
##    variant references a field the constructor never set, so the object is left
##    alone. With this rule, independent per-field scalars are sound even though
##    variant fields share storage — well-typed access never cross-reads them.
##
## All constructor field *values* are preserved (one `var` decl each, in order), so
## side effects and evaluation order are unchanged; fields that end up unused become
## ordinary dead stores that `copyprop`/DSE clean up afterwards. Run this *before*
## `copyprop` so the decomposition's fresh scalar copies get propagated away.
##
## Restrictions (kept deliberately simple, like `copyprop`): only `var` locals
## whose initializer is a pure `(oconstr T (kv …)…)` of `kv` fields (a constructor
## with an inheritance/base part is left alone); a name declared twice is skipped.

import std / [tables, sets, hashes, assertions, algorithm]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore (incl. rootOf, symId)
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/substructureKind
import ".." / ".." / "models" / tags          # VarTagId for synthesis
import patchsets

# ---- nifcore helpers ------------------------------------------------------

proc child0(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

# ---- context --------------------------------------------------------------

type
  Candidate = object
    declPos: int                       ## position of the `(var :o …)` decl
    fieldOrder: seq[SymId]             ## fields in constructor order
    valuePos: Table[SymId, int]        ## field -> position of its `kv` value
    names: Table[SymId, string]        ## field -> fresh scalar name (survivors)
    accessed: HashSet[SymId]           ## fields actually read/written via `.`
    disqualified: bool                 ## escaped → leave the object alone

  Context = object
    orig: ptr TokenBuf
    candidates: Table[SymId, Candidate]
    accesses: seq[(SymId, SymId, int)] ## (object, field, dot-node position)
    patchset: Patchset
    synth: seq[TokenBuf]
    suffix: string
    counter: int

proc freshScalarName(c: var Context): string =
  inc c.counter
  result = "`sroa." & $c.counter & "." & c.suffix

# ---- candidate collection -------------------------------------------------

proc recordOconstr(c: var Context; oSym: SymId; declPos: int; oconstr: Cursor) =
  ## Record `var oSym = (oconstr T (kv f v)…)` as a candidate. Bails (records
  ## nothing) on a base/inheritance part or a malformed `kv`; a name seen twice is
  ## disqualified outright (we can't tell the two objects apart by SymId).
  if oSym in c.candidates:
    c.candidates[oSym].disqualified = true
    return
  var cand = Candidate(declPos: declPos,
                       valuePos: initTable[SymId, int](),
                       names: initTable[SymId, string](),
                       accessed: initHashSet[SymId]())
  var ok = true
  var oc = oconstr
  oc.into:
    if oc.hasMore: skip oc                       # type T
    while oc.hasMore:
      if oc.kind == TagLit and oc.substructureKind == KvU:
        var kv = oc
        var fSym = SymId(0)
        var vPos = -1
        kv.into:
          if kv.hasMore:
            if kv.kind == Symbol: fSym = symId(kv)
            skip kv                              # field name
          if kv.hasMore:
            vPos = cursorToPosition(c.orig[], kv)
            skip kv                              # value
          while kv.hasMore: skip kv              # optional inherited-depth int
        if fSym != SymId(0) and vPos >= 0 and fSym notin cand.valuePos:
          cand.fieldOrder.add fSym
          cand.valuePos[fSym] = vPos
        else:
          ok = false
        skip oc
      else:
        ok = false                               # base part / stray child → bail
        skip oc
  if ok and cand.fieldOrder.len > 0:
    c.candidates[oSym] = cand

proc collectCandidates(c: var Context; n: Cursor) =
  if n.kind != TagLit: return
  if n.stmtKind == VarS:
    var v = n
    var oSym = SymId(0)
    var hasOconstr = false
    var oconstrCur = default(Cursor)
    v.into:
      if v.hasMore:
        if v.kind == SymbolDef: oSym = symId(v)
        skip v                                   # name
      if v.hasMore: skip v                       # pragmas
      if v.hasMore: skip v                       # type
      if v.hasMore:
        if v.kind == TagLit and v.exprKind == OconstrC:
          hasOconstr = true
          oconstrCur = v
        skip v                                   # initializer
      while v.hasMore: skip v
    if oSym != SymId(0) and hasOconstr:
      recordOconstr(c, oSym, cursorToPosition(c.orig[], n), oconstrCur)
  var m = n
  m.loopInto:
    collectCandidates(c, m)
    skip m

# ---- legality / use classification ---------------------------------------

proc classify(c: var Context; n: Cursor) =
  ## Walk the body. A candidate that appears anywhere except as the base of a
  ## `(dot o field)` is disqualified; otherwise the accessed field is recorded.
  case n.kind
  of Symbol:
    let s = symId(n)
    if s in c.candidates:
      c.candidates[s].disqualified = true        # a bare whole-object use
  of TagLit:
    case n.exprKind
    of DotC:
      let base = child0(n)
      if base.kind == Symbol and symId(base) in c.candidates:
        let oSym = symId(base)
        var f = base
        skip f
        if f.kind == Symbol:
          let fSym = symId(f)
          c.candidates[oSym].accessed.incl fSym
          c.accesses.add (oSym, fSym, cursorToPosition(c.orig[], n))
        else:
          c.candidates[oSym].disqualified = true  # `o.(weird)` → give up
      else:
        var m = n                                 # nested base (`o.a.b`, deref, …)
        m.loopInto:
          classify(c, m)
          skip m
    of AddrC:
      let r = rootOf(child0(n))                   # `addr o` / `addr o.f` escapes o
      if r != SymId(0) and r in c.candidates:
        c.candidates[r].disqualified = true
      var m = n
      m.loopInto:
        classify(c, m)
        skip m
    else:
      var m = n
      m.loopInto:
        classify(c, m)
        skip m
  else:
    discard

# ---- synthesis ------------------------------------------------------------

proc emitRewritten(c: var Context; dest: var TokenBuf; n: Cursor) =
  ## Copy the subtree at `n` into `dest`, replacing every `(dot X f)` whose object
  ## `X` is a survivor with its scalar. Needed because a replaced object decl's
  ## value subtrees are spliced verbatim — the main patchset never rewalks them.
  if n.kind == TagLit:
    if n.exprKind == DotC:
      let base = child0(n)
      if base.kind == Symbol and symId(base) in c.candidates and
         c.candidates[symId(base)].names.len > 0:
        let oSym = symId(base)
        var f = base
        skip f
        if f.kind == Symbol and symId(f) in c.candidates[oSym].names:
          dest.addSymUse c.candidates[oSym].names[symId(f)]
          return
    let tag = n.cursorTagId
    let li = rawLineInfo(n)
    dest.openTag tag
    if li.isValid: dest.appendLineInfo li
    var m = n
    m.loopInto:
      emitRewritten(c, dest, m)
      skip m
    dest.closeTag()
  else:
    dest.addSubtree n

proc buildFieldDecl(c: var Context; name: string; value: Cursor): int =
  ## Synthesize `(var :name . . <rewritten value>)`.
  result = c.synth.len
  var buf = createTokenBuf(16, c.orig[].pool, c.orig[].tags)
  buf.openTag TagId(ord(VarTagId))
  let li = rawLineInfo(value)
  if li.isValid: buf.appendLineInfo li
  buf.addSymDef name
  buf.addDotToken()                              # pragmas
  buf.addDotToken()                              # type — inferred from value
  emitRewritten(c, buf, value)
  buf.closeTag()
  c.synth.add ensureMove(buf)

proc buildSymUse(c: var Context; name: string): int =
  result = c.synth.len
  var buf = createTokenBuf(2, c.orig[].pool, c.orig[].tags)
  buf.addSymUse name
  c.synth.add ensureMove(buf)

proc synthCursor(c: var Context; idx: int): Cursor {.inline.} =
  cursorAt(c.synth[idx], 0)

# ---- public entry ---------------------------------------------------------

proc runScalarize*(buf: var TokenBuf; moduleSuffix = "M") =
  ## Scalar-replace non-escaping local objects in a single proc body. Names the
  ## field scalars `` `sroa.<n>.<suffix> ``; pass a per-body-unique suffix (the
  ## driver uses `bodySuffix`) so two bodies' scalars get distinct module symbols.
  var c = Context(orig: addr buf,
                  candidates: initTable[SymId, Candidate](),
                  accesses: @[],
                  patchset: initPatchset(addr buf),
                  synth: @[],
                  suffix: moduleSuffix,
                  counter: 0)
  block:
    let n = beginRead(buf)
    collectCandidates(c, n)
  if c.candidates.len == 0: return
  block:
    let n = beginRead(buf)
    classify(c, n)

  # Survivors: not escaped, and every accessed field was constructor-initialised.
  var survPairs: seq[(int, SymId)] = @[]
  for oSym, cand in c.candidates:
    if cand.disqualified: continue
    var ok = true
    for f in cand.accessed:
      if f notin cand.valuePos: ok = false; break
    if ok: survPairs.add (cand.declPos, oSym)
  if survPairs.len == 0: return
  survPairs.sort(proc(a, b: (int, SymId)): int = cmp(a[0], b[0]))

  # Assign scalar names in source order (deterministic output / codegen).
  for it in survPairs:
    let oSym = it[1]
    let fields = c.candidates[oSym].fieldOrder
    for f in fields:
      let nm = freshScalarName(c)
      c.candidates[oSym].names[f] = nm

  # Replace each object decl with its field-scalar decls (constructor order).
  for it in survPairs:
    let oSym = it[1]
    let declPos = c.candidates[oSym].declPos
    let fields = c.candidates[oSym].fieldOrder
    var first = true
    for f in fields:
      let name = c.candidates[oSym].names[f]
      let value = cursorAt(c.orig[], c.candidates[oSym].valuePos[f])
      let idx = buildFieldDecl(c, name, value)
      if first:
        c.patchset.addSubst(declPos, synthCursor(c, idx)); first = false
      else:
        c.patchset.addInsert(declPos, synthCursor(c, idx))

  # Rewrite every field access to its scalar (survivors only have names).
  for acc in c.accesses:
    let (oSym, fSym, pos) = acc
    if oSym in c.candidates and fSym in c.candidates[oSym].names:
      let idx = buildSymUse(c, c.candidates[oSym].names[fSym])
      c.patchset.addSubst(pos, synthCursor(c, idx))

  if not c.patchset.isEmpty:
    var nb = c.patchset.apply()
    buf = ensureMove(nb)

# ---- self-tests -----------------------------------------------------------

when isMainModule:
  proc parse(src: string): TokenBuf =
    parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())

  proc canon(src: string): string =
    var b = parse(src)
    toString(b)

  template chk(input, expected: string) =
    var buf = parse(input)
    runScalarize buf
    let got = toString(buf)
    let want = canon(expected)
    doAssert got == want, "MISMATCH\n  got:  " & got & "\n  want: " & want

  template assertUnchanged(input: string) =
    var buf = parse(input)
    let before = toString(buf)
    runScalarize buf
    doAssert toString(buf) == before, "expected unchanged:\n  " & input

  block two_fields_exploded:
    chk(
      "(stmts (var :o.0.M . . (oconstr T.0.M (kv f.0.M 1) (kv g.0.M 2))) " &
      "(asgn x.0.M (dot o.0.M f.0.M)) (asgn y.0.M (dot o.0.M g.0.M)))",
      "(stmts (var :`sroa.1.M . . 1) (var :`sroa.2.M . . 2) " &
      "(asgn x.0.M `sroa.1.M) (asgn y.0.M `sroa.2.M))")

  block field_write_then_read:
    chk(
      "(stmts (var :o.0.M . . (oconstr T.0.M (kv f.0.M 1))) " &
      "(asgn (dot o.0.M f.0.M) 5) (asgn x.0.M (dot o.0.M f.0.M)))",
      "(stmts (var :`sroa.1.M . . 1) " &
      "(asgn `sroa.1.M 5) (asgn x.0.M `sroa.1.M))")

  block whole_object_use_disqualifies:
    # `o` is passed whole to a call → it must materialize → left alone.
    assertUnchanged(
      "(stmts (var :o.0.M . . (oconstr T.0.M (kv f.0.M 1))) (call use.0.M o.0.M))")

  block addr_of_field_disqualifies:
    assertUnchanged(
      "(stmts (var :o.0.M . . (oconstr T.0.M (kv f.0.M 1))) " &
      "(call use.0.M (addr (dot o.0.M f.0.M))))")

  block uninitialised_field_access_disqualifies:
    # `o.g` is read but the constructor never set `g` (think: other variant arm).
    assertUnchanged(
      "(stmts (var :o.0.M . . (oconstr T.0.M (kv f.0.M 1))) " &
      "(asgn x.0.M (dot o.0.M g.0.M)))")

  block nested_value_rewritten:
    # `o`'s `f` is initialised from `p.a`; both scalarize and the dependency is
    # rewritten inside the spliced decl value.
    chk(
      "(stmts (var :p.0.M . . (oconstr U.0.M (kv a.0.M 1))) " &
      "(var :o.0.M . . (oconstr T.0.M (kv f.0.M (dot p.0.M a.0.M)))) " &
      "(asgn x.0.M (dot o.0.M f.0.M)))",
      "(stmts (var :`sroa.1.M . . 1) (var :`sroa.2.M . . `sroa.1.M) " &
      "(asgn x.0.M `sroa.2.M))")

  block unused_object_keeps_value_side_effects:
    # `o` is never used, but its field value is a call: explode so the call stays
    # (as a dead store that DSE can then judge), dropping the aggregate.
    chk(
      "(stmts (var :o.0.M . . (oconstr T.0.M (kv f.0.M (call mk.0.M)))) " &
      "(call other.0.M))",
      "(stmts (var :`sroa.1.M . . (call mk.0.M)) (call other.0.M))")

  block nested_field_access_through_scalar:
    # `o.inner` is itself an object; `o.inner.x` becomes `(dot `sroa.1 x)`.
    chk(
      "(stmts (var :o.0.M . . (oconstr T.0.M (kv inner.0.M q.0.M))) " &
      "(asgn r.0.M (dot (dot o.0.M inner.0.M) x.0.M)))",
      "(stmts (var :`sroa.1.M . . q.0.M) " &
      "(asgn r.0.M (dot `sroa.1.M x.0.M)))")

  echo "scalarizer.nim: all self-tests passed"
