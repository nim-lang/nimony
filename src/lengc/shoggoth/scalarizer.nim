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
## We scalarize a local `var o = (oconstr …)` or `var o = L` (`L` a pure
## aggregate lvalue, including a whole-object copy `var o = src`) iff:
##
## 1. **Non-escaping.** `o` appears *only* as the base of a `(dot o field)`. Any
##    bare `o` (whole-object read, `o = x`, `(call f o)`, `(ret o)`) — except
##    the forms listed under "Objects assigned and read as a whole" below — and
##    any `addr` whose root is `o` (`addr o`, `addr o.f`), disqualifies it. This single
##    rule also rules out objects with destructors or non-trivial copies: their
##    lowered `=destroy` / `=copy` appears as `addr o` / a whole-object use, so they
##    never survive — and the surviving fields are therefore trivially destructible.
##    A whole-object *copy* `var tmp = src` is a load, not an escape of `tmp`:
##    `tmp.f` becomes a scalar initialised from `src.f`. That is the inliner's
##    by-value aggregate param (`var tmp = s` for a 16-byte string).
##
## 2. **Every accessed field is initialised.** Constructor candidates need the
##    field in the `(oconstr)`; load candidates initialise each accessed field
##    from `L.f` at the copy. If `o.x` is read but `x` is not a `(kv x …)` of
##    the `oconstr`, we bail. For a `case`/variant object this is exactly what
##    protects us: reading a field of the *other* variant references a field
##    the constructor never set, so the object is left alone. With this rule,
##    independent per-field scalars are sound even though variant fields share
##    storage — well-typed access never cross-reads them.
##
## All constructor field *values* are preserved (one `var` decl each, in order), so
## side effects and evaluation order are unchanged; fields that end up unused become
## ordinary dead stores that `copyprop`/DSE clean up afterwards. Run this *before*
## `copyprop` so the decomposition's fresh scalar copies get propagated away.
##
## Restrictions (kept deliberately simple, like `copyprop`): `var` locals whose
## initializer is a pure `(oconstr T (kv …)…)` of `kv` fields, or a pure
## aggregate lvalue (`src`, `p.a`, …), or none; a constructor with an
## inheritance/base part is left alone; a name declared twice is skipped.
##
## ## Objects assigned and read as a whole
##
## Rule 1 has one relaxation, for what an inlined callee leaves behind:
## `var t: V; … t = V(x: a, y: b); … o = t; … o.x`. An object of a *plain*
## object type — no base, no union branch, no field pragma — may also be
##
## * assigned as a whole, `o = RHS`, where RHS is a constructor naming every
##   field or a pure lvalue, and does not read `o` itself: the assignment
##   becomes one per field, `` `sroa.x = a `` (`o = V(x: o.y, y: o.x)` would read
##   a field the previous field's assignment already overwrote);
## * read as a whole as the value of an `asgn`/`var`/`kv`, a call argument or
##   the operand of `ret`/`discard`. Into an object that is decomposed itself the
##   copy goes per field; anywhere else the value is rebuilt — a total
##   `(oconstr V (kv x `sroa.x) …)` in place, or in a temp declared before the
##   statement for an operand.
##
## Such an object becomes one scalar per field of its *type*, so the rebuild
## can name them all. Whether that pays is a cost question, see `scalarize`:
## a rebuild costs what the scalars saved, and on arkham every scalar that is
## live next to others costs a callee-saved register.
##
## Loads with a variable index, `var o = things[i]`, read their fields through
## the element's address taken once (`bindBase`): the stride of an array of
## objects is no addressing mode's scale, so each `things[i].f` would pay a
## multiply and an add.

import std / [tables, sets, hashes, assertions, algorithm]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore (incl. rootOf, symId)
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/substructureKind
import ".." / ".." / "lib" / symparser        # derivedName -- one spelling for a derived name
import ".." / ".." / "models" / tags          # VarTagId for synthesis
import patchsets
import accesspaths                            # indexesVariably
import ".." / nifmodules                      # MainModule (type context, threaded through)
import ".." / typenav                         # lookupField — the scalar's declared type

include ".." / ".." / "lib" / compat2         # getOrQuit (host Nim)

# ---- nifcore helpers ------------------------------------------------------

proc child0(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

# ---- inline constructor projection ----------------------------------------
#
# `(dot (oconstr T (kv f v)…) f)` → `v`. This is SROA for an object that is
# *never bound to a var* — a temporary the source projected one field out of.
# It needs no escape analysis (an inline constructor result is single-use by
# construction) and no flow analysis. Dropping the unprojected `kv` values is
# sound because xelim only leaves an `oconstr` inline when none of its children
# is a call (`isComplex`, xelim.nim) — so every dropped value is pure and dead.
# A constructor with an inheritance/base part is left alone, mirroring the
# var-based pass.

proc projectField(oconstr: Cursor; field: SymId; val: var Cursor): bool =
  ## True (and sets `val` to a cursor at the value) iff `oconstr` is
  ## `(oconstr T (kv f v)…)` with no base part and a `kv` keyed by `field`.
  var found = false
  var bail = false
  var oc = oconstr
  oc.into:
    if oc.hasMore: skip oc                          # type T
    while oc.hasMore:
      if not found and not bail and oc.kind == TagLit and
         oc.substructureKind == KvU:
        var kv = oc
        var fSym = SymId(0)
        var v = default(Cursor)
        var haveV = false
        kv.into:
          if kv.hasMore:
            if kv.kind == Symbol: fSym = symId(kv)
            skip kv                                 # field name
          if kv.hasMore:
            v = kv; haveV = true
            skip kv                                 # value
          while kv.hasMore: skip kv                 # optional inherited-depth int
        if fSym == field and haveV:
          val = v; found = true
        skip oc
      elif not found and not bail and oc.kind == TagLit:
        bail = true                                 # base/inheritance part → bail
        skip oc
      else:
        skip oc                                     # consume the rest (rem == 0)
  result = found and not bail

proc collectProjections(orig: ptr TokenBuf; ps: var Patchset; n: Cursor): int =
  ## Record a subst for each `(dot (oconstr …) field)` whose field is present.
  ## Does not recurse into a folded node, so nested/chained `a.f.g` collapse one
  ## layer per `runConstructorProjection` round (fixpoint handles the rest).
  result = 0
  if n.kind != TagLit: return
  if n.exprKind == DotC:
    let base = child0(n)
    if base.kind == TagLit and base.exprKind == OconstrC:
      var f = base
      skip f                                        # past the oconstr to the field
      if f.kind == Symbol:
        var val = default(Cursor)
        if projectField(base, symId(f), val):
          ps.addSubst(cursorToPosition(orig[], n), val)
          return 1                                  # whole dot replaced
  var m = n
  m.loopInto:
    result += collectProjections(orig, ps, m)
    skip m

proc runConstructorProjection*(buf: var TokenBuf): bool {.discardable.} =
  ## Fold field projections off inline (never-bound) object constructors.
  ## Iterated to a fixpoint so chains (`T(a: U(x: 1)).a.x`) collapse fully;
  ## each round removes at least one `oconstr`, so it terminates. Mints no new
  ## symbols. Returns whether it folded anything.
  result = false
  var rounds = 0
  while rounds < 100:
    inc rounds
    var ps = initPatchset(addr buf)
    let folded = block:
      let n = beginRead(buf)
      collectProjections(addr buf, ps, n)
    if folded == 0: break
    var nb = ps.apply()
    buf = ensureMove(nb)
    result = true

# ---- context --------------------------------------------------------------

type
  InitKind = enum
    ikCtor                             ## `var o = (oconstr T (kv f v)…)`
    ikLoad                             ## `var o = L`, `L` a pure aggregate lvalue
    ikNone                             ## `var o: T` without an initializer

  ReadKind = enum
    rkInPlace                          ## the whole value of an `asgn`, a `var` or a `kv`:
                                       ## the object is rebuilt where it stands
    rkHoisted                          ## a call argument, the operand of `ret`/`discard`:
                                       ## rebuilt into a temp declared before the statement

  WholeRead = object
    ## A bare `o` that reads the whole object.
    obj: SymId
    pos: int                           ## the symbol
    stmtPos: int                       ## the statement-list entry enclosing it
    owner: SymId                       ## the candidate that statement declares or assigns
                                       ## as a whole, 0 ⇒ none. A scalarized owner re-emits
                                       ## its statement, which rebuilds the read in passing
    kind: ReadKind

  Candidate = object
    declPos: int                       ## position of the `(var :o …)` decl
    typePos: int                       ## position of the type `T` whose fields the scalars
                                       ## stand for — the `oconstr`'s type operand, or (for a
                                       ## load or an uninitialised candidate) the decl's own
                                       ## type slot (-1 ⇒ absent); `lookupField` resolves each
                                       ## scalar's declared type through it
    init: InitKind
    loadPos: int                       ## ikLoad: position of `L`, else -1
    fieldOrder: seq[SymId]             ## the fields that get a scalar, in emission order
    valuePos: Table[SymId, int]        ## field -> position of its `kv` value (ikCtor)
    names: Table[SymId, string]        ## field -> fresh scalar name (survivors)
    dotPos: Table[SymId, int]          ## field -> position of a `(dot o f …)` in the body,
                                       ## reused verbatim (field symbol + inherited-depth
                                       ## operand) when synthesising a load scalar's `L.f`
    accessed: HashSet[SymId]           ## fields actually read/written via `.`
    accessOrder: seq[SymId]            ## the same, in order of first access
    fieldReads: int                    ## how many of those accesses read the field
    defs: seq[int]                     ## `(asgn o RHS)` statements: whole assignments
    selfDef: bool                      ## one of them reads `o` in its RHS
    reads: seq[int]                    ## its whole reads, indices into `Context.reads`
    fedBy: seq[SymId]                  ## the objects read whole into its own
                                       ## decl or assignments, in place
    whole: bool                        ## assigned or read as a whole somewhere. Such an
                                       ## object becomes one scalar per field of its *type*,
                                       ## so a total `oconstr` over them rebuilds it
    disqualified: bool                 ## escaped → leave the object alone
    first, last: int                   ## positions of the first and last touch: its
                                       ## initialised decl, a def, an access, a read
    declDepth: int                     ## loops enclosing the decl
    loopUse: bool                      ## touched in a loop the decl is outside of

  Context = object
    orig: ptr TokenBuf
    candidates: Table[SymId, Candidate]
    accesses: seq[(SymId, SymId, int)] ## (object, field, dot-node position)
    reads: seq[WholeRead]
    calls: seq[(int, int)]             ## every call: its position and where it ends,
                                       ## by position (the walk is pre-order)
    loopDepth: int
    patchset: Patchset
    synth: seq[TokenBuf]
    counter: int
    m: ptr MainModule                  ## module type context; nil ⇒ emit `.` and let the
                                       ## backend re-infer (the pre-existing behaviour, kept
                                       ## for the self-tests, which parse a body in isolation)
    knownFields: Table[SymId, seq[SymId]]
                                       ## without `m`: the self-tests' object types
    base: string                       ## while one load is decomposed: the temp holding
                                       ## its source's address, "" ⇒ none (`bindBase`)

proc freshScalarName(c: var Context): string =
  ## A field scalar is a local of the body being scalarized, so its name is a
  ## local's: `<ident>.<disamb>` and nothing else. Two bodies both minting
  ## `` `sroa.1 `` is no more a problem than both having a `result.0`.
  inc c.counter
  result = "`sroa." & $c.counter

proc cand(c: var Context; s: SymId): var Candidate {.inline.} =
  c.candidates.getOrQuit(s)

proc isSurvivor(c: var Context; s: SymId): bool =
  s != SymId(0) and s in c.candidates and c.cand(s).names.len > 0

# ---- candidate collection -------------------------------------------------

proc newCandidate(declPos, typePos: int; init: InitKind; loadPos = -1): Candidate =
  Candidate(declPos: declPos, typePos: typePos, init: init, loadPos: loadPos,
            first: high(int), last: -1,
            valuePos: initTable[SymId, int](),
            names: initTable[SymId, string](),
            dotPos: initTable[SymId, int](),
            accessed: initHashSet[SymId]())

proc addCandidate(c: var Context; oSym: SymId; x: sink Candidate) =
  ## A name seen twice is disqualified outright: we can't tell the two objects
  ## apart by SymId.
  if oSym in c.candidates:
    c.cand(oSym).disqualified = true
  else:
    c.candidates[oSym] = x

proc readKvs(c: var Context; oconstr: Cursor; order: var seq[SymId];
             values: var Table[SymId, int]): bool =
  ## The `(kv f v)` children of `oconstr`, in order. False on a base/inheritance
  ## part, a malformed `kv` or a field named twice.
  result = true
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
        if fSym != SymId(0) and vPos >= 0 and fSym notin values:
          order.add fSym
          values[fSym] = vPos
        else:
          result = false
        skip oc
      else:
        result = false                           # base part / stray child → bail
        skip oc

proc recordOconstr(c: var Context; oSym: SymId; declPos: int; oconstr: Cursor) =
  ## Record `var oSym = (oconstr T (kv f v)…)` as a candidate. Bails (records
  ## nothing) on a base/inheritance part or a malformed `kv`.
  var cand = newCandidate(declPos, cursorToPosition(c.orig[], child0(oconstr)), ikCtor)
  if readKvs(c, oconstr, cand.fieldOrder, cand.valuePos) and cand.fieldOrder.len > 0:
    addCandidate(c, oSym, cand)

proc isPureLvaluePath(n: Cursor): bool =
  ## True for an lvalue built only from symbols, literals and the address-forming
  ## operators — i.e. one we may re-evaluate once per field without duplicating a
  ## side effect. A call anywhere inside makes it false.
  case n.kind
  of Symbol, IntLit, UIntLit, CharLit: result = true
  of TagLit:
    if n.exprKind notin {DotC, AtC, PatC, DerefC}: return false
    result = true
    var m = n
    m.loopInto:
      if not isPureLvaluePath(m): return false
      skip m
  else: result = false

proc recordDecl(c: var Context; n: Cursor) =
  ## The `(var …)` `n` declares a candidate when it is a local object with a
  ## constructor, a pure lvalue or nothing as its initializer. Its uses follow
  ## it in the walk: a local is declared before anything names it.
  var v = n
  var oSym = SymId(0)
  var oconstrCur = default(Cursor)
  var init = ikNone
  var declTypePos = -1
  var loadPos = -1
  var known = false
  v.into:
    if v.hasMore:
      if v.kind == SymbolDef: oSym = symId(v)
      skip v                                   # name
    if v.hasMore: skip v                       # pragmas
    if v.hasMore:
      # The decl's own type slot names the object type for a load or an
      # uninitialised candidate. An empty slot (`.`) leaves us nothing to
      # resolve the fields through.
      if not (v.kind == DotToken): declTypePos = cursorToPosition(c.orig[], v)
      skip v                                   # type
    if v.hasMore:
      if v.kind == TagLit and v.exprKind == OconstrC:
        init = ikCtor; known = true
        oconstrCur = v
      elif declTypePos >= 0 and (
           v.kind == Symbol or
           (v.kind == TagLit and v.exprKind in {DotC, AtC, PatC, DerefC} and
            isPureLvaluePath(v))):
        # Whole-object copy `var o = src` or a field/deref path. A Symbol is
        # the inliner's `var tmp = s` for a by-value aggregate param.
        init = ikLoad; known = true
        loadPos = cursorToPosition(c.orig[], v)
      elif declTypePos >= 0 and v.kind == DotToken:
        # `var o: T`: the inliner's result slot, assigned by the callee's `ret`.
        init = ikNone; known = true
      skip v                                   # initializer
    while v.hasMore: skip v
  if oSym != SymId(0) and known:
    let declPos = cursorToPosition(c.orig[], n)
    case init
    of ikCtor: recordOconstr(c, oSym, declPos, oconstrCur)
    of ikLoad: addCandidate(c, oSym, newCandidate(declPos, declTypePos, ikLoad, loadPos))
    of ikNone: addCandidate(c, oSym, newCandidate(declPos, declTypePos, ikNone))

# ---- legality / use classification ---------------------------------------

proc addrEscapeRoot(n: Cursor): SymId =
  ## The symbol whose storage the address expression `n` (the operand of an
  ## `addr`/`haddr`) denotes, or 0 when the chain passes through a dereference
  ## first. `(haddr (pat (dot o f) i))` is the address of an element of the array
  ## `o.f` points to — not of `o`'s storage. There `o` is merely read, exactly
  ## like any other field load, so it must not disqualify the object. Only a
  ## chain of `dot`/`at` selections reaching `o` directly aliases `o` itself.
  var cur = n
  while true:
    case cur.kind
    of Symbol: return symId(cur)
    of TagLit:
      case cur.exprKind
      of DerefC, PatC: return SymId(0)         # behind a pointer: not o's storage
      of DotC, AtC: cur = child0(cur)
      else: return rootOf(cur)                 # anything else: stay conservative
    else: return SymId(0)

proc isCandidateSym(c: var Context; n: Cursor): bool {.inline.} =
  n.kind == Symbol and symId(n) in c.candidates

proc touch(c: var Context; s: SymId; pos: int) =
  if pos < c.cand(s).first: c.cand(s).first = pos
  if pos > c.cand(s).last: c.cand(s).last = pos
  if c.loopDepth > c.cand(s).declDepth: c.cand(s).loopUse = true

proc noteRead(c: var Context; n: Cursor; stmtPos: int; owner: SymId; kind: ReadKind) =
  ## `n` is a bare candidate symbol in a position that reads the whole object.
  let s = symId(n)
  touch(c, s, cursorToPosition(c.orig[], n))
  c.cand(s).whole = true
  if s == owner: c.cand(s).selfDef = true          # `o = f(o)`
  c.cand(s).reads.add c.reads.len
  if kind == rkInPlace and owner != SymId(0) and owner != s:
    c.cand(owner).fedBy.add s
  c.reads.add WholeRead(obj: s, pos: cursorToPosition(c.orig[], n),
                        stmtPos: stmtPos, owner: owner, kind: kind)

proc classify(c: var Context; n: Cursor; stmtPos: int; owner: SymId)

proc classifyValue(c: var Context; n: Cursor; stmtPos: int; owner: SymId) =
  ## `n` is a value stored as a whole: an `asgn`'s source, a `var`'s initializer,
  ## a `kv`'s value. A bare object there is rebuilt in place.
  if isCandidateSym(c, n):
    noteRead(c, n, stmtPos, owner, rkInPlace)
  else:
    classify(c, n, stmtPos, owner)

proc classifyOperand(c: var Context; n: Cursor; stmtPos: int; owner: SymId) =
  ## `n` is a call argument or the operand of `ret`/`discard`, read by value.
  ## A bare object there is rebuilt into a temp before the statement.
  if isCandidateSym(c, n):
    noteRead(c, n, stmtPos, owner, rkHoisted)
  else:
    classify(c, n, stmtPos, owner)

proc noteAccess(c: var Context; dot: Cursor; oSym, owner: SymId; isRead: bool) =
  var f = child0(dot)
  skip f
  if f.kind == Symbol:
    let fSym = symId(f)
    let at = cursorToPosition(c.orig[], dot)
    touch(c, oSym, at)
    if oSym == owner: c.cand(oSym).selfDef = true   # `o = T(a: o.b, …)`
    if fSym notin c.cand(oSym).accessed:
      c.cand(oSym).accessOrder.add fSym
      c.cand(oSym).dotPos[fSym] = at
    c.cand(oSym).accessed.incl fSym
    if isRead:
      let reads = c.cand(oSym).fieldReads + 1
      c.cand(oSym).fieldReads = reads
    c.accesses.add (oSym, fSym, at)
  else:
    c.cand(oSym).disqualified = true                 # `o.(weird)` → give up

proc classifyTarget(c: var Context; n: Cursor; stmtPos: int; owner: SymId) =
  ## The storage spine of an assignment target: the candidate field it ends
  ## in is written, not read. Indices along the spine are read as usual.
  if n.kind == TagLit and n.exprKind in {DotC, AtC}:
    let base = child0(n)
    if n.exprKind == DotC and isCandidateSym(c, base):
      noteAccess(c, n, symId(base), owner, false)
    else:
      var m = n
      m.into:
        if m.hasMore:
          classifyTarget(c, m, stmtPos, owner); skip m   # the spine
        while m.hasMore:
          classify(c, m, stmtPos, owner); skip m
  else:
    classify(c, n, stmtPos, owner)

proc classifyKids(c: var Context; n: Cursor; stmtPos: int; owner: SymId) =
  var m = n
  m.loopInto:
    classify(c, m, stmtPos, owner)
    skip m

proc classify(c: var Context; n: Cursor; stmtPos: int; owner: SymId) =
  ## Walk the body. A candidate may appear as the base of a `(dot o field)`
  ## (the accessed field is recorded), as the target of a whole assignment, or
  ## bare where its whole value is read (`classifyValue`/`classifyOperand`).
  ## Anywhere else it is disqualified. `stmtPos` is the statement-list entry
  ## enclosing `n`, `owner` the candidate that statement declares or assigns.
  case n.kind
  of Symbol:
    let s = symId(n)
    if s in c.candidates:
      c.cand(s).disqualified = true                  # a use we don't understand
  of TagLit:
    case n.stmtKind
    of StmtsS, ScopeS:
      var m = n
      m.loopInto:
        classify(c, m, cursorToPosition(c.orig[], m), SymId(0))
        skip m
      return
    of AsgnS:
      var m = n
      m.into:
        let here = cursorToPosition(c.orig[], n)
        var own = SymId(0)
        if isCandidateSym(c, m):
          own = symId(m)                             # `o = RHS`: a whole assignment
          c.cand(own).defs.add here
          c.cand(own).whole = true
          touch(c, own, here)
        else:
          classifyTarget(c, m, stmtPos, owner)
        skip m
        if m.hasMore:
          classifyValue(c, m, stmtPos, own)
          skip m
        while m.hasMore:
          classify(c, m, stmtPos, owner)
          skip m
      return
    of VarS:
      recordDecl(c, n)
      var m = n
      m.into:
        var own = SymId(0)
        if m.kind == SymbolDef and symId(m) in c.candidates and
           c.cand(symId(m)).declPos == cursorToPosition(c.orig[], n):
          own = symId(m)
          c.cand(own).declDepth = c.loopDepth
          if c.cand(own).init != ikNone: touch(c, own, c.cand(own).declPos)
        skip m                                       # name
        if m.hasMore:
          classify(c, m, stmtPos, owner); skip m     # pragmas
        if m.hasMore:
          classify(c, m, stmtPos, owner); skip m     # type
        if m.hasMore:
          classifyValue(c, m, stmtPos, own); skip m  # initializer
        while m.hasMore:
          classify(c, m, stmtPos, owner); skip m
      return
    of RetS, DiscardS:
      var m = n
      m.loopInto:
        classifyOperand(c, m, stmtPos, owner)
        skip m
      return
    of LoopS, WhileS:
      inc c.loopDepth
      classifyKids(c, n, stmtPos, owner)
      dec c.loopDepth
      return
    else: discard
    case n.exprKind
    of DotC:
      let base = child0(n)
      if isCandidateSym(c, base):
        noteAccess(c, n, symId(base), owner, true)
      else:
        classifyKids(c, n, stmtPos, owner)           # nested base (`o.a.b`, deref, …)
    of AddrC, HaddrC:
      let r = addrEscapeRoot(child0(n))              # `addr o` / `addr o.f` escapes o
      if r != SymId(0) and r in c.candidates:
        c.cand(r).disqualified = true
      classifyKids(c, n, stmtPos, owner)
    of CallC:
      var e = n
      skip e
      c.calls.add (cursorToPosition(c.orig[], n), cursorToPosition(c.orig[], e))
      var m = n
      m.into:
        if m.hasMore:
          classify(c, m, stmtPos, owner); skip m     # callee
        while m.hasMore:
          classifyOperand(c, m, stmtPos, owner)
          skip m
    else:
      if n.substructureKind == KvU:
        var m = n
        m.into:
          if m.hasMore: skip m                       # field name
          if m.hasMore:
            classifyValue(c, m, stmtPos, owner); skip m
          while m.hasMore: skip m                    # inherited-depth int
      else:
        classifyKids(c, n, stmtPos, owner)
  else:
    discard

# ---- legality -------------------------------------------------------------

proc typeFields(c: var Context; typePos: int): seq[SymId] =
  ## The fields of the object type at `typePos` in declaration order, or nothing
  ## unless it is a plain object: no base type, no union branch, no field
  ## pragma (a bit field's scalar would not truncate what is stored into it).
  ## Those are the types a total `oconstr` over the field scalars rebuilds.
  result = @[]
  if typePos < 0: return
  let t = cursorAt(c.orig[], typePos)
  if c.m == nil:
    if t.kind == Symbol: result = c.knownFields.getOrDefault(symId(t))
    return
  var body = navigateToObjectBody(c.m[], t)
  if body.kind != TagLit or body.typeKind != ObjectT: return
  var ok = true
  body.into:
    if body.hasMore:
      if body.kind != DotToken: ok = false           # inheritance
      skip body
    while body.hasMore:
      if ok and body.kind == TagLit and body.substructureKind == FldU:
        let fd = takeFieldDecl(body)
        if fd.name.kind == SymbolDef and fd.pragmas.kind == DotToken:
          result.add symId(fd.name)
        else:
          ok = false
      else:
        ok = false
        skip body
  if not ok: result.setLen 0

proc sameFields(order: seq[SymId]; fields: seq[SymId]): bool =
  ## `order` names every field of `fields` exactly once (it names each at most
  ## once already, `readKvs`).
  result = order.len == fields.len
  if result:
    for f in order:
      if f notin fields: return false

proc wholeDefOk(c: var Context; defPos: int; fields: seq[SymId]): bool =
  ## `(asgn o RHS)` decomposes into one assignment per field: RHS is a total
  ## `oconstr` or a pure lvalue. (That it does not read `o` itself is
  ## `selfDef`: the field assignments run one after another, and
  ## `o = T(a: o.b, b: o.a)` would read a field the previous one already
  ## overwrote.)
  var a = cursorAt(c.orig[], defPos)
  inc a                                              # into the asgn: the target
  skip a                                             # the value
  if a.kind == TagLit and a.exprKind == OconstrC:
    var order: seq[SymId] = @[]
    var values = initTable[SymId, int]()
    result = readKvs(c, a, order, values) and sameFields(order, fields)
  else:
    result = isPureLvaluePath(a)

proc fieldwiseOk(c: var Context; oSym: SymId): bool =
  ## `oSym` is only ever touched through `o.f`: one scalar per accessed field,
  ## initialised from the constructor's value or from `L.f`. Fills in its
  ## `fieldOrder` for a load: the fields accessed, ordered by first access so
  ## the emitted decls are deterministic.
  if c.cand(oSym).init == ikCtor:
    result = true
    let accessed = c.cand(oSym).accessed
    for f in accessed:
      if f notin c.cand(oSym).valuePos: result = false  # read but never constructed
  else:
    let order = c.cand(oSym).accessOrder
    c.cand(oSym).fieldOrder = order
    result = c.cand(oSym).fieldOrder.len > 0         # a load nobody reads by field: leave it

proc wholeOk(c: var Context; oSym: SymId): bool =
  ## `oSym` is assigned or read as a whole: a scalar per field of its type,
  ## every access naming one of them, every whole assignment decomposable, a
  ## constructor initializer total. Fills in its `fieldOrder`.
  let fields = typeFields(c, c.cand(oSym).typePos)
  result = fields.len > 0 and not c.cand(oSym).selfDef
  let accessed = c.cand(oSym).accessed
  for f in accessed:
    if f notin fields: result = false
  let defs = c.cand(oSym).defs
  for d in defs:
    if result and not wholeDefOk(c, d, fields): result = false
  if result:
    if c.cand(oSym).init == ikCtor:
      result = sameFields(c.cand(oSym).fieldOrder, fields)
    else:
      c.cand(oSym).fieldOrder = fields

proc survives(c: var Context; oSym: SymId): bool =
  ## Is it legal to scalarize `oSym`? Whether it pays is decided afterwards.
  if c.cand(oSym).disqualified: result = false
  elif c.cand(oSym).whole: result = wholeOk(c, oSym)
  else: result = fieldwiseOk(c, oSym)

# ---- synthesis ------------------------------------------------------------

proc emitRewritten(c: var Context; dest: var TokenBuf; n: Cursor)

proc emitRebuilt(c: var Context; dest: var TokenBuf; oSym: SymId) =
  ## The whole value of the scalarized `oSym`: `(oconstr T (kv f `sroa.f)…)`,
  ## total over the type's fields, as `oconstr` is.
  let typePos = c.cand(oSym).typePos
  dest.openTag TagId(ord(OconstrTagId))
  dest.addSubtree cursorAt(c.orig[], typePos)
  let fields = c.cand(oSym).fieldOrder
  for f in fields:
    dest.openTag TagId(ord(KvTagId))
    dest.addSymUse f
    dest.addSymUse c.cand(oSym).names.getOrQuit(f)
    dest.closeTag()
  dest.closeTag()

proc emitLoadBase(c: var Context; dest: var TokenBuf; src: Cursor) =
  ## The object a load reads its fields from: `src` itself, or `(deref p)`
  ## when `bindBase` took its address.
  if c.base.len > 0:
    dest.openTag TagId(ord(DerefTagId))
    dest.addSymUse c.base
    dest.closeTag()
  else:
    emitRewritten(c, dest, src)

proc emitField(c: var Context; dest: var TokenBuf; src: Cursor; f: SymId) =
  ## `src.f`, for the pure lvalue `src`: the scalar itself when `src` is a
  ## scalarized object.
  if src.kind == Symbol and isSurvivor(c, symId(src)):
    dest.addSymUse c.cand(symId(src)).names.getOrQuit(f)
  else:
    dest.openTag TagId(ord(DotTagId))
    emitLoadBase(c, dest, src)
    dest.addSymUse f
    dest.addIntLit 0                                 # inherited depth: a plain object
    dest.closeTag()

proc emitRewritten(c: var Context; dest: var TokenBuf; n: Cursor) =
  ## Copy the subtree at `n` into `dest`, replacing every `(dot X f)` whose object
  ## `X` is a survivor with its scalar, and every bare survivor (a whole read)
  ## with its rebuilt value. Needed because a replaced object decl's value
  ## subtrees are spliced verbatim — the main patchset never rewalks them.
  if n.kind == Symbol:
    if isSurvivor(c, symId(n)):
      emitRebuilt(c, dest, symId(n))
    else:
      dest.addSubtree n
  elif n.kind == TagLit:
    if n.exprKind == DotC:
      let base = child0(n)
      if base.kind == Symbol and isSurvivor(c, symId(base)):
        let oSym = symId(base)
        var f = base
        skip f
        if f.kind == Symbol and symId(f) in c.cand(oSym).names:
          dest.addSymUse c.cand(oSym).names.getOrQuit(symId(f))
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

proc emitScalarType(c: var Context; buf: var TokenBuf; typePos: int; f: SymId) =
  ## Spell out the scalar's declared type instead of leaving the slot empty for the
  ## backend to re-infer. The right type is the field's declared type in `T`, not the
  ## initializer's: the scalar stands in for every `o.f`, so it must have `o.f`'s type.
  ## Re-inference from the value gets this wrong whenever the two differ — a narrow
  ## field initialized from a wider literal would silently widen, changing where the
  ## arithmetic wraps — and it fails outright on a value whose type the backend cannot
  ## navigate in this module's context (the same hole `cse.emitTempType` was written to
  ## close). Without a type context (the self-tests) the slot is left empty.
  if c.m == nil:
    buf.addDotToken()
  else:
    assert typePos >= 0, "oconstr without a type"
    buf.addSubtree lookupField(c.m[], cursorAt(c.orig[], typePos), f)

proc openSynth(c: var Context; tag: TagId; li: NifLineInfo): TokenBuf =
  result = createTokenBuf(24, c.orig[].pool, c.orig[].tags)
  result.openTag tag
  if li.isValid: result.appendLineInfo li

proc finishSynth(c: var Context; buf: sink TokenBuf): Cursor =
  buf.closeTag()
  c.synth.add ensureMove(buf)
  result = cursorAt(c.synth[c.synth.len - 1], 0)

proc buildFieldDecl(c: var Context; oSym, f: SymId): Cursor =
  ## `(var :`sroa.f . <field type> <init>)` for the field `f` of the survivor
  ## `oSym`. The init is the constructor's value, `L.f` for a load, nothing for
  ## an uninitialised object.
  let decl = cursorAt(c.orig[], c.cand(oSym).declPos)
  var buf = openSynth(c, TagId(ord(VarTagId)), rawLineInfo(decl))
  buf.addSymDef c.cand(oSym).names.getOrQuit(f)
  buf.addDotToken()                                  # pragmas
  emitScalarType(c, buf, c.cand(oSym).typePos, f)    # declared type
  case c.cand(oSym).init
  of ikCtor:
    emitRewritten(c, buf, cursorAt(c.orig[], c.cand(oSym).valuePos.getOrQuit(f)))
  of ikLoad:
    let src = cursorAt(c.orig[], c.cand(oSym).loadPos)
    if f in c.cand(oSym).dotPos:
      # One of the body's own `(dot o f …)` nodes, reused so the field symbol
      # and the inherited-depth operand are spelled exactly as the backend
      # expects; only its base `o` is swapped for the initializer `L`.
      let dotNode = cursorAt(c.orig[], c.cand(oSym).dotPos.getOrQuit(f))
      if src.kind == Symbol and isSurvivor(c, symId(src)):
        emitField(c, buf, src, f)
      else:
        buf.openTag dotNode.cursorTagId
        let li = rawLineInfo(dotNode)
        if li.isValid: buf.appendLineInfo li
        var d = dotNode
        d.into:
          if d.hasMore: skip d                       # base `o` …
          emitLoadBase(c, buf, src)                  # … replaced by `L`
          while d.hasMore:
            buf.addSubtree d                         # field symbol, inherited-depth int
            skip d
        buf.closeTag()
    else:
      emitField(c, buf, src, f)
  of ikNone:
    buf.addDotToken()
  result = finishSynth(c, ensureMove(buf))

proc buildFieldAsgn(c: var Context; oSym, f: SymId; at: Cursor; value: Cursor;
                    project: bool): Cursor =
  ## `(asgn `sroa.f V)`: `V` is `value` itself, or `value.f` when `project`.
  var buf = openSynth(c, TagId(ord(AsgnTagId)), rawLineInfo(at))
  buf.addSymUse c.cand(oSym).names.getOrQuit(f)
  if project: emitField(c, buf, value, f)
  else: emitRewritten(c, buf, value)
  result = finishSynth(c, ensureMove(buf))

proc replaceWhole(c: var Context; pos: int; stmts: seq[Cursor]) =
  ## The statement at `pos` becomes `stmts`, in order.
  for i in 0 ..< stmts.len:
    if i == 0: c.patchset.addSubst(pos, stmts[i])
    else: c.patchset.addInsert(pos, stmts[i])

proc addressOnce(c: var Context; src: Cursor): bool =
  ## Is the address of the load source `src` worth computing once for all its
  ## fields? It is when it scales a variable index: an element of an array of
  ## objects has a stride no addressing mode scales by, so each `src.f` pays a
  ## multiply and an add — nine of them for `var obj = things[i]`. Only memory
  ## behind a pointer or a global qualifies: the address of a local would
  ## pin it to the stack for every other pass.
  result = false
  if src.kind != TagLit or not indexesVariably(src): return
  let r = addrEscapeRoot(src)
  if r == SymId(0):
    result = true                                    # behind a pointer
  elif c.m != nil:
    let d = c.m[].getDeclOrNil(r)
    result = d != nil and d.kind in {ConstY, GvarY}

proc bindBase(c: var Context; typePos: int; src, at: Cursor; fields: int;
              stmts: var seq[Cursor]) =
  ## `(var :`sroa.p . (ptr T) (addr src))` ahead of a load's field reads, which
  ## then read through it (`emitLoadBase`), when `addressOnce` says so.
  c.base = ""
  if fields < 2 or not addressOnce(c, src): return
  let p = freshScalarName(c)
  var buf = openSynth(c, TagId(ord(VarTagId)), rawLineInfo(at))
  buf.addSymDef p
  buf.addDotToken()                                  # pragmas
  if c.m == nil or typePos < 0:
    buf.addDotToken()
  else:
    buf.openTag TagId(ord(PtrTagId))
    buf.addSubtree cursorAt(c.orig[], typePos)
    buf.closeTag()
  buf.openTag TagId(ord(AddrTagId))
  emitRewritten(c, buf, src)
  buf.closeTag()
  stmts.add finishSynth(c, ensureMove(buf))
  c.base = p

proc emitDef(c: var Context; oSym: SymId; defPos: int) =
  ## `(asgn o RHS)` → one `(asgn `sroa.f …)` per field: a constructor's values
  ## in its own order (it evaluates them in that order), a load's `RHS.f` in
  ## field order.
  let at = cursorAt(c.orig[], defPos)
  var rhs = at
  inc rhs
  skip rhs
  var stmts: seq[Cursor] = @[]
  if rhs.kind == TagLit and rhs.exprKind == OconstrC:
    var order: seq[SymId] = @[]
    var values = initTable[SymId, int]()
    discard readKvs(c, rhs, order, values)
    for f in order:
      stmts.add buildFieldAsgn(c, oSym, f, at, cursorAt(c.orig[], values.getOrQuit(f)), false)
  else:
    let fields = c.cand(oSym).fieldOrder
    bindBase(c, c.cand(oSym).typePos, rhs, at, fields.len, stmts)
    for f in fields:
      stmts.add buildFieldAsgn(c, oSym, f, at, rhs, true)
    c.base = ""
  replaceWhole(c, defPos, stmts)

proc emitRead(c: var Context; r: WholeRead) =
  ## A whole read of the survivor `r.obj` outside a re-emitted statement.
  if isSurvivor(c, r.owner): return                  # re-emitted with the owner
  case r.kind
  of rkInPlace:
    var buf = createTokenBuf(24, c.orig[].pool, c.orig[].tags)
    emitRebuilt(c, buf, r.obj)
    c.synth.add ensureMove(buf)
    c.patchset.addSubst(r.pos, cursorAt(c.synth[c.synth.len - 1], 0))
  of rkHoisted:
    # `(var :`sroa.t . T (oconstr …))` before the statement, which reads the
    # temp instead: the temp is evaluated where the statement starts, and
    # nothing in a statement writes a local before its operands are read.
    let tmp = freshScalarName(c)
    let at = cursorAt(c.orig[], r.pos)
    var buf = openSynth(c, TagId(ord(VarTagId)), rawLineInfo(at))
    buf.addSymDef tmp
    buf.addDotToken()                                # pragmas
    buf.addSubtree cursorAt(c.orig[], c.cand(r.obj).typePos)
    emitRebuilt(c, buf, r.obj)
    c.patchset.addInsert(r.stmtPos, finishSynth(c, ensureMove(buf)))
    var use = createTokenBuf(2, c.orig[].pool, c.orig[].tags)
    use.addSymUse tmp
    c.synth.add ensureMove(use)
    c.patchset.addSubst(r.pos, cursorAt(c.synth[c.synth.len - 1], 0))

# ---- public entry ---------------------------------------------------------

proc crossesCall(c: var Context; oSym: SymId): bool =
  ## Does a call run while `oSym` holds a value it reads later? A call its
  ## last use is an argument of does not count: nothing is live after it.
  result = false
  let first = c.cand(oSym).first
  let last = c.cand(oSym).last
  var i = 0                                          # the first call after `first`
  var hi = c.calls.len
  while i < hi:
    let mid = (i + hi) div 2
    if c.calls[mid][0] <= first: i = mid + 1
    else: hi = mid
  while i < c.calls.len and c.calls[i][0] < last:
    if c.calls[i][1] <= last: return true
    inc i

proc scalarize(buf: var TokenBuf; m: ptr MainModule; temps: var int;
               knownFields: Table[SymId, seq[SymId]]): bool =
  result = false
  var c = Context(orig: addr buf,
                  candidates: initTable[SymId, Candidate](),
                  accesses: @[],
                  reads: @[],
                  patchset: initPatchset(addr buf),
                  synth: @[],
                  counter: temps,
                  m: m,
                  knownFields: knownFields)
  block:
    let n = beginRead(buf)
    classify(c, n, cursorToPosition(buf, n), SymId(0))
  if c.candidates.len == 0: return

  var syms: seq[SymId] = @[]
  for oSym in c.candidates.keys: syms.add oSym
  var legal = initHashSet[SymId]()
  for oSym in syms:
    if survives(c, oSym): legal.incl oSym
  # An object assigned or read as a whole is only worth decomposing when the
  # scalars replace memory traffic, and arkham charges for every scalar: with
  # five volatile registers, one that is live next to anything else, or across
  # a call, gets a callee-saved register — a save and a restore on every entry
  # to the proc. So the object must
  #
  # * have its fields read one by one: through `o.f`, or by being copied as a
  #   whole into an object that is decomposed itself;
  # * never be rebuilt — passed whole to a call, returned, stored into memory
  #   — unless it is used in a loop it is declared outside of, where the
  #   rebuild is paid once and the saved loads every iteration;
  # * not live across a call, under the same loop exemption.
  #
  # `t = V(…); ret t`, a `result` built field by field, `enterScope`'s
  # `CursorScope` (one field read, then returned) and `reinternLineInfo`'s
  # `fname` (live across a call) all stay in memory; a raytracer's color,
  # accumulated over the lights and returned, and the chains of inlined
  # `Vec3` results feeding it, are decomposed.
  var useful = initHashSet[SymId]()
  for oSym in legal:
    if not c.cand(oSym).whole or c.cand(oSym).loopUse or not crossesCall(c, oSym):
      useful.incl oSym
  # Whether an object pays depends on the owners its whole reads feed, so a
  # drop is propagated to those it fed, and only to those.
  var work: seq[SymId] = @[]
  for oSym in useful: work.add oSym
  while work.len > 0:
    let oSym = work.pop()
    if oSym notin useful or not c.cand(oSym).whole: continue
    var feeds = false
    var rebuilt = false
    let reads = c.cand(oSym).reads
    for ri in reads:
      let r = c.reads[ri]
      if r.kind == rkInPlace and r.owner in useful: feeds = true
      else: rebuilt = true
    if not (c.cand(oSym).fieldReads > 0 or feeds) or
       (rebuilt and not c.cand(oSym).loopUse):
      useful.excl oSym
      # What `oSym` was fed in place is now rebuilt for it: ask again.
      let fedBy = c.cand(oSym).fedBy
      for s in fedBy: work.add s
  var survPairs: seq[(int, SymId)] = @[]
  for oSym in syms:
    if oSym in useful: survPairs.add (c.cand(oSym).declPos, oSym)
  # A whole read hoisted out of a statement that a survivor re-emits would
  # be lost with the statement. No survivor's statement has one — its value is
  # a constructor or a pure lvalue — so this only keeps the invariant obvious.
  var dropped = false
  for ri in 0 ..< c.reads.len:
    let r = c.reads[ri]
    if r.kind == rkHoisted and r.owner in useful:
      c.cand(r.obj).disqualified = true
      dropped = true
  if dropped:
    var kept: seq[(int, SymId)] = @[]
    for it in survPairs:
      if not c.cand(it[1]).disqualified: kept.add it
    survPairs = kept
  if survPairs.len == 0: return
  survPairs.sort(proc(a, b: (int, SymId)): int = cmp(a[0], b[0]))

  # Assign scalar names in source order (deterministic output / codegen).
  for it in survPairs:
    let oSym = it[1]
    let fields = c.cand(oSym).fieldOrder
    for f in fields:
      let nm = freshScalarName(c)
      c.cand(oSym).names[f] = nm

  for it in survPairs:
    let oSym = it[1]
    # Each object decl becomes its field-scalar decls.
    var decls: seq[Cursor] = @[]
    let fields = c.cand(oSym).fieldOrder
    if c.cand(oSym).init == ikLoad:
      let src = cursorAt(c.orig[], c.cand(oSym).loadPos)
      bindBase(c, c.cand(oSym).typePos, src, cursorAt(c.orig[], c.cand(oSym).declPos),
               fields.len, decls)
    for f in fields:
      decls.add buildFieldDecl(c, oSym, f)
    c.base = ""
    replaceWhole(c, c.cand(oSym).declPos, decls)
    # Each whole assignment becomes one assignment per field.
    let defs = c.cand(oSym).defs
    for d in defs: emitDef(c, oSym, d)

  # Rewrite every field access to its scalar (survivors only have names).
  # By index: iterating `c.accesses` directly would borrow `c` for the whole
  # loop, and the body mutates it.
  for ai in 0 ..< c.accesses.len:
    let (oSym, fSym, pos) = c.accesses[ai]
    if isSurvivor(c, oSym) and fSym in c.cand(oSym).names:
      var use = createTokenBuf(2, c.orig[].pool, c.orig[].tags)
      use.addSymUse c.cand(oSym).names.getOrQuit(fSym)
      c.synth.add ensureMove(use)
      c.patchset.addSubst(pos, cursorAt(c.synth[c.synth.len - 1], 0))

  for ri in 0 ..< c.reads.len:
    let r = c.reads[ri]
    if isSurvivor(c, r.obj): emitRead(c, r)
  temps = c.counter

  if not c.patchset.isEmpty:
    var nb = c.patchset.apply()
    buf = ensureMove(nb)
    result = true

proc runScalarize*(buf: var TokenBuf; m: ptr MainModule; temps: var int): bool {.discardable.} =
  ## Scalar-replace non-escaping local objects in a single proc body. Names the
  ## field scalars `` `sroa.<n> ``, which are locals of that body.
  ## `m` is the module type context each scalar's declared type is resolved through;
  ## without it the type slots are left empty and the backend re-infers them.
  ## `temps` is the body's `` `sroa `` counter: a body the pipeline scalarizes
  ## again must not mint a name the previous round already declared.
  ## Returns whether it changed `buf`.
  result = scalarize(buf, m, temps, initTable[SymId, seq[SymId]]())

# ---- self-tests -----------------------------------------------------------

proc runScalarize*(buf: var TokenBuf; m: ptr MainModule = nil) =
  var temps = 0
  runScalarize(buf, m, temps)

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
      "(stmts (var :`sroa.1 . . 1) (var :`sroa.2 . . 2) " &
      "(asgn x.0.M `sroa.1) (asgn y.0.M `sroa.2))")

  block field_write_then_read:
    chk(
      "(stmts (var :o.0.M . . (oconstr T.0.M (kv f.0.M 1))) " &
      "(asgn (dot o.0.M f.0.M) 5) (asgn x.0.M (dot o.0.M f.0.M)))",
      "(stmts (var :`sroa.1 . . 1) " &
      "(asgn `sroa.1 5) (asgn x.0.M `sroa.1))")

  block whole_object_use_disqualifies:
    # `o` is passed whole to a call → it must materialize → left alone.
    assertUnchanged(
      "(stmts (var :o.0.M . . (oconstr T.0.M (kv f.0.M 1))) (call use.0.M o.0.M))")

  block addr_of_object_disqualifies:
    assertUnchanged(
      "(stmts (var :o.0.M . . (oconstr T.0.M (kv f.0.M 1))) " &
      "(call use.0.M (addr o.0.M)))")

  block addr_of_field_disqualifies:
    assertUnchanged(
      "(stmts (var :o.0.M . . (oconstr T.0.M (kv f.0.M 1))) " &
      "(call use.0.M (addr (dot o.0.M f.0.M))))")

  block copy_from_symbol_load:
    # `var o = src` (the inliner's by-value param copy). Fields come from `src.f`.
    chk(
      "(stmts (var :o.0.M . T.0.M src.0.M) (asgn x.0.M (dot o.0.M f.0.M)))",
      "(stmts (var :`sroa.1 . . (dot src.0.M f.0.M)) (asgn x.0.M `sroa.1))")

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
      "(stmts (var :`sroa.1 . . 1) (var :`sroa.2 . . `sroa.1) " &
      "(asgn x.0.M `sroa.2))")

  block unused_object_keeps_value_side_effects:
    # `o` is never used, but its field value is a call: explode so the call stays
    # (as a dead store that DSE can then judge), dropping the aggregate.
    chk(
      "(stmts (var :o.0.M . . (oconstr T.0.M (kv f.0.M (call mk.0.M)))) " &
      "(call other.0.M))",
      "(stmts (var :`sroa.1 . . (call mk.0.M)) (call other.0.M))")

  block nested_field_access_through_scalar:
    # `o.inner` is itself an object; `o.inner.x` becomes `(dot `sroa.1 x)`.
    chk(
      "(stmts (var :o.0.M . . (oconstr T.0.M (kv inner.0.M q.0.M))) " &
      "(asgn r.0.M (dot (dot o.0.M inner.0.M) x.0.M)))",
      "(stmts (var :`sroa.1 . . q.0.M) " &
      "(asgn r.0.M (dot `sroa.1 x.0.M)))")

  # ---- #3: inline constructor projection ----------------------------------

  template chkProj(input, expected: string) =
    var buf = parse(input)
    runConstructorProjection buf
    let got = toString(buf)
    let want = canon(expected)
    doAssert got == want, "MISMATCH\n  got:  " & got & "\n  want: " & want

  template assertProjUnchanged(input: string) =
    var buf = parse(input)
    let before = toString(buf)
    runConstructorProjection buf
    doAssert toString(buf) == before, "expected unchanged:\n  " & input

  block project_first_field:
    chkProj(
      "(stmts (asgn x.0.M (dot (oconstr T.0.M (kv f.0.M 1) (kv g.0.M 2)) f.0.M)))",
      "(stmts (asgn x.0.M 1))")

  block project_second_field_drops_first:
    chkProj(
      "(stmts (asgn x.0.M (dot (oconstr T.0.M (kv f.0.M 1) (kv g.0.M 2)) g.0.M)))",
      "(stmts (asgn x.0.M 2))")

  block project_chain_collapses:
    # `T(a: U(x: 7)).a.x` → 7, exercising the fixpoint (two rounds).
    chkProj(
      "(stmts (asgn r.0.M (dot (dot (oconstr T.0.M " &
      "(kv a.0.M (oconstr U.0.M (kv x.0.M 7)))) a.0.M) x.0.M)))",
      "(stmts (asgn r.0.M 7))")

  block project_symbol_value:
    chkProj(
      "(stmts (asgn x.0.M (dot (oconstr T.0.M (kv f.0.M y.0.M)) f.0.M)))",
      "(stmts (asgn x.0.M y.0.M))")

  block project_absent_field_unchanged:
    # field `g` is not in the constructor (other variant arm) → leave it alone.
    assertProjUnchanged(
      "(stmts (asgn x.0.M (dot (oconstr T.0.M (kv f.0.M 1)) g.0.M)))")

  block project_base_part_unchanged:
    # a constructor with an inheritance/base part is left alone.
    assertProjUnchanged(
      "(stmts (asgn x.0.M (dot (oconstr T.0.M (oconstr Base.0.M (kv b.0.M 1)) " &
      "(kv f.0.M 2)) f.0.M)))")

  block projection_then_scalarize_compose:
    # projection feeds the var-based pass: the folded value becomes the scalar.
    var buf = parse(
      "(stmts (var :o.0.M . . (oconstr T.0.M " &
      "(kv f.0.M (dot (oconstr U.0.M (kv x.0.M 5)) x.0.M)))) " &
      "(asgn r.0.M (dot o.0.M f.0.M)))")
    runConstructorProjection buf
    runScalarize buf
    let got = toString(buf)
    let want = canon("(stmts (var :`sroa.1 . . 5) (asgn r.0.M `sroa.1))")
    doAssert got == want, "MISMATCH\n  got:  " & got & "\n  want: " & want

  # ---- whole assignments and whole reads ---------------------------------
  # `V` is a plain object with the fields `x` and `y`.

  proc runWithV(buf: var TokenBuf) =
    var known = initTable[SymId, seq[SymId]]()
    known[buf.pool.symId("V.0.M")] = @[buf.pool.symId("x.0.M"), buf.pool.symId("y.0.M")]
    var temps = 0
    discard scalarize(buf, nil, temps, known)

  template chkV(input, expected: string) =
    var buf = parse(input)
    runWithV buf
    let got = toString(buf)
    let want = canon(expected)
    doAssert got == want, "MISMATCH\n  got:  " & got & "\n  want: " & want

  template assertUnchangedV(input: string) =
    var buf = parse(input)
    let before = toString(buf)
    runWithV buf
    doAssert toString(buf) == before, "expected unchanged:\n  " & input

  const ctorV = "(oconstr V.0.M (kv x.0.M 1) (kv y.0.M 2))"

  block uninitialised_field_scalars:
    # `var o: V` touched only through `o.x`: no whole use, no type needed.
    chk(
      "(stmts (var :o.0.M . V.0.M .) (asgn (dot o.0.M x.0.M 0) 1) " &
      "(asgn r.0.M (dot o.0.M x.0.M 0)))",
      "(stmts (var :`sroa.1 . . .) (asgn `sroa.1 1) (asgn r.0.M `sroa.1))")

  block whole_assignment_from_constructor:
    chkV(
      "(stmts (var :o.0.M . V.0.M .) (asgn o.0.M " & ctorV & ") " &
      "(asgn r.0.M (dot o.0.M x.0.M 0)))",
      "(stmts (var :`sroa.1 . . .) (var :`sroa.2 . . .) " &
      "(asgn `sroa.1 1) (asgn `sroa.2 2) (asgn r.0.M `sroa.1))")

  block whole_copy_between_scalarized_objects:
    # The inliner's `t = V(…); o = t`: both decompose, the copy goes per field.
    chkV(
      "(stmts (var :t.0.M . V.0.M .) " &
      "(asgn t.0.M (oconstr V.0.M (kv x.0.M a.0.M) (kv y.0.M b.0.M))) " &
      "(var :o.0.M . V.0.M .) (asgn o.0.M t.0.M) (asgn r.0.M (dot o.0.M y.0.M 0)))",
      "(stmts (var :`sroa.1 . . .) (var :`sroa.2 . . .) " &
      "(asgn `sroa.1 a.0.M) (asgn `sroa.2 b.0.M) " &
      "(var :`sroa.3 . . .) (var :`sroa.4 . . .) " &
      "(asgn `sroa.3 `sroa.1) (asgn `sroa.4 `sroa.2) (asgn r.0.M `sroa.4))")

  block constructed_and_returned_unchanged:
    # `t = V(…); ret t`: no field is used on its own, so nothing is gained.
    assertUnchangedV(
      "(stmts (var :o.0.M . V.0.M .) (asgn o.0.M " & ctorV & ") (ret o.0.M))")

  block fields_only_stored_unchanged:
    # A `result` built field by field and returned: nothing reads a field.
    assertUnchangedV(
      "(stmts (var :o.0.M . V.0.M .) (asgn o.0.M " & ctorV & ") " &
      "(asgn (dot o.0.M x.0.M 0) 3) (ret o.0.M))")

  block rebuilt_outside_a_loop_unchanged:
    # Read field by field, but then stored, returned or passed whole: the
    # rebuild costs what the scalars save, and their registers cost more.
    assertUnchangedV(
      "(stmts (var :o.0.M . V.0.M .) (asgn o.0.M " & ctorV & ") " &
      "(asgn r.0.M (dot o.0.M y.0.M 0)) (asgn (deref p.0.M) o.0.M))")
    assertUnchangedV(
      "(stmts (var :o.0.M . V.0.M .) (asgn o.0.M " & ctorV & ") " &
      "(asgn r.0.M (dot o.0.M y.0.M 0)) (ret o.0.M))")
    assertUnchangedV(
      "(stmts (var :o.0.M . V.0.M src.0.M) (asgn (dot o.0.M y.0.M 0) 5) " &
      "(asgn q.0.M (dot o.0.M x.0.M 0)) (asgn r.0.M (call f.0.M o.0.M)))")

  block live_across_a_call_unchanged:
    assertUnchangedV(
      "(stmts (var :t.0.M . V.0.M .) (asgn t.0.M " & ctorV & ") " &
      "(call g.0.M) (asgn r.0.M (dot t.0.M x.0.M 0)))")

  block loop_accumulator_rebuilt_at_ret:
    # Accumulated in a loop that calls, returned after it: the scalars win
    # every iteration, the rebuild happens once. `t` is the inlined callee's
    # result, consumed whole by the accumulator.
    chkV(
      "(stmts (var :o.0.M . V.0.M src.0.M) " &
      "(loop (stmts (call g.0.M) (var :t.0.M . V.0.M .) " &
      "(asgn t.0.M (oconstr V.0.M (kv x.0.M (dot o.0.M y.0.M 0)) (kv y.0.M 1))) " &
      "(asgn o.0.M t.0.M) (continue c.0.M))) (ret o.0.M))",
      "(stmts (var :`sroa.1 . . (dot src.0.M x.0.M 0)) " &
      "(var :`sroa.2 . . (dot src.0.M y.0.M 0)) " &
      "(loop (stmts (call g.0.M) (var :`sroa.3 . . .) (var :`sroa.4 . . .) " &
      "(asgn `sroa.3 `sroa.2) (asgn `sroa.4 1) " &
      "(asgn `sroa.1 `sroa.3) (asgn `sroa.2 `sroa.4) (continue c.0.M))) " &
      "(var :`sroa.5 . V.0.M (oconstr V.0.M (kv x.0.M `sroa.1) (kv y.0.M `sroa.2))) " &
      "(ret `sroa.5))")

  block indexed_load_reads_through_its_address:
    # `var o = p[i]`, two fields read: the element's address is taken once.
    chk(
      "(stmts (var :o.0.M . V.0.M (pat p.0.M i.0.M)) " &
      "(asgn r.0.M (dot o.0.M x.0.M 0)) (asgn s.0.M (dot o.0.M y.0.M 0)))",
      "(stmts (var :`sroa.3 . . (addr (pat p.0.M i.0.M))) " &
      "(var :`sroa.1 . . (dot (deref `sroa.3) x.0.M 0)) " &
      "(var :`sroa.2 . . (dot (deref `sroa.3) y.0.M 0)) " &
      "(asgn r.0.M `sroa.1) (asgn s.0.M `sroa.2))")

  block constant_index_load_unchanged_shape:
    # `p[0]` addresses for free: no pointer temp.
    chk(
      "(stmts (var :o.0.M . V.0.M (pat p.0.M 0)) " &
      "(asgn r.0.M (dot o.0.M x.0.M 0)) (asgn s.0.M (dot o.0.M y.0.M 0)))",
      "(stmts (var :`sroa.1 . . (dot (pat p.0.M 0) x.0.M 0)) " &
      "(var :`sroa.2 . . (dot (pat p.0.M 0) y.0.M 0)) " &
      "(asgn r.0.M `sroa.1) (asgn s.0.M `sroa.2))")

  block self_referencing_assignment_unchanged:
    # `o = V(x: o.y, y: o.x)`: per-field assignments would read an overwritten field.
    assertUnchangedV(
      "(stmts (var :o.0.M . V.0.M .) (asgn o.0.M (oconstr V.0.M " &
      "(kv x.0.M (dot o.0.M y.0.M 0)) (kv y.0.M (dot o.0.M x.0.M 0)))) (ret o.0.M))")

  block partial_constructor_unchanged:
    assertUnchangedV(
      "(stmts (var :o.0.M . V.0.M .) (asgn o.0.M (oconstr V.0.M (kv x.0.M 1))) " &
      "(ret o.0.M))")

  block unknown_type_unchanged:
    # No field list for `W`: a whole use cannot be rebuilt.
    assertUnchangedV(
      "(stmts (var :o.0.M . W.0.M .) (asgn o.0.M (oconstr W.0.M (kv x.0.M 1))) " &
      "(ret o.0.M))")

  block only_copied_unchanged:
    # Nothing reads or writes a field: decomposing only to rebuild buys nothing.
    assertUnchangedV("(stmts (var :o.0.M . V.0.M src.0.M) (ret o.0.M))")

  block call_result_unchanged:
    assertUnchangedV(
      "(stmts (var :o.0.M . V.0.M .) (asgn o.0.M (call mk.0.M)) " &
      "(asgn r.0.M (dot o.0.M x.0.M 0)))")

  echo "scalarizer.nim: all self-tests passed"
