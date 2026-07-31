#
#
#       NIFC Induction-Variable Strength Reduction (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## nifcore induction-variable strength reduction. Replaces `(at arr iv)` accesses
## inside a loop with `(deref p)` for a pointer `p` initialized before the loop
## and advanced one element per iteration. See `induction_variables_legacy.nim`
## (the parked nifcursors original) for the full pattern/rewrite description; this
## version runs over **nifcore** cursors and uses [patchsets](patchsets.nim) for
## the structural mutations.

import std / [tables, sets, assertions]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind, tag enums
import ".." / ".." / "models" / tags          # *TagId ordinals for synthesis
import patchsets
import ".." / nifmodules                      # MainModule (type context, threaded through)
import ".." / typenav                         # getNominalType — the temp's declared type

type
  AccessInfo = object
    arrSym: SymId
    pos: int           # position of the `(at arr iv)` subtree in orig
    atCursor: Cursor   # cursor at the same position (for `addSubtree`)

  Context = object
    orig: ptr TokenBuf
    patchset: Patchset
    synth: seq[TokenBuf]
    tempCounter: int
    moduleSuffix: string
    m: ptr MainModule  ## module type context; nil ⇒ emit `.` and let the backend
                       ## re-infer (what the self-tests, which parse a body in
                       ## isolation, still do)

proc createContext(orig: ptr TokenBuf; moduleSuffix: string;
                   m: ptr MainModule): Context =
  Context(orig: orig,
          patchset: initPatchset(orig),
          synth: @[],
          tempCounter: 0,
          moduleSuffix: moduleSuffix,
          m: m)

proc freshTempName(c: var Context): string =
  inc c.tempCounter
  result = "iv.p." & $c.tempCounter & "." & c.moduleSuffix

# ---- pattern detection ----------------------------------------------------

proc isIvIncPattern(c: Cursor; outIvSym: var SymId): bool =
  ## Matches `(asgn lhs (add T lhs 1))`. Sets `outIvSym` to `lhs`'s id.
  if c.kind != TagLit: return false
  if c.stmtKind notin {AsgnS, StoreS}: return false
  var probe = c
  inc probe                                  # past `(asgn`
  if probe.kind != Symbol: return false
  let lhsSym = symId(probe)
  skip probe                                 # past lhs
  if probe.kind != TagLit or probe.exprKind != AddC: return false
  var rhs = probe
  inc rhs                                    # past `(add`
  skip rhs                                   # past the type
  if rhs.kind != Symbol or symId(rhs) != lhsSym: return false
  skip rhs                                   # past `iv`
  if rhs.kind != IntLit: return false
  if intVal(rhs) != 1: return false
  outIvSym = lhsSym
  return true

proc loopBodyCursor(loopCursor: Cursor): Cursor =
  ## Cursor at the loop body. Handles `(while cond body)` and
  ## `(loop pre cond body after)`.
  result = loopCursor
  case loopCursor.stmtKind
  of WhileS:
    inc result                               # past `(while`
    skip result                              # past cond
  of LoopS:
    inc result                               # past `(loop`
    skip result                              # past before-cond
    skip result                              # past cond
  else: discard

proc countWritesOf(start: Cursor; sym: SymId; counter: var int) =
  ## Counts Symbol-LHS assignments to `sym` anywhere in the subtree at `start`,
  ## including nested loops.
  if not start.hasMore: return
  case start.kind
  of TagLit:
    if start.stmtKind in {AsgnS, StoreS}:
      var lhs = start
      inc lhs
      if lhs.kind == Symbol and symId(lhs) == sym:
        inc counter
    var n = start
    n.loopInto:
      countWritesOf(n, sym, counter)
      skip n
  else: discard

proc scanBody(c: var Context; n: Cursor;
              candidates: var seq[tuple[ivSym: SymId, incPos: int]];
              addrTaken: var HashSet[SymId]) =
  ## Single pre-scan: collects iv-inc candidates AND every symbol whose address
  ## is taken anywhere in the body.
  if not n.hasMore or n.kind != TagLit: return
  var ivSym = SymId(0)
  if isIvIncPattern(n, ivSym):
    candidates.add (ivSym, cursorToPosition(c.orig[], n))
    return                                   # iv-inc's children can't matter
  if n.exprKind in AddrKinds:
    var probe = n
    inc probe                                # past `(addr`/`(haddr`
    while probe.hasMore:
      if probe.kind == Symbol:
        addrTaken.incl symId(probe)
        break
      elif probe.kind == TagLit:
        inc probe                            # descend into chain
      else:
        inc probe
    return
  var m = n
  m.loopInto:
    scanBody(c, m, candidates, addrTaken)
    skip m

proc findIV(c: var Context; body: Cursor): tuple[ivSym: SymId, incPos: int, found: bool] =
  ## Identifies the loop body's induction variable (exactly one
  ## `(asgn iv (add T iv 1))`, no other writes, address not taken).
  result = (SymId(0), 0, false)
  if body.kind != TagLit: return

  var candidates: seq[tuple[ivSym: SymId, incPos: int]] = @[]
  var addrTaken = initHashSet[SymId]()
  if body.stmtKind == StmtsS:
    var m = body
    m.loopInto:
      scanBody(c, m, candidates, addrTaken)
      skip m
  else:
    scanBody(c, body, candidates, addrTaken)

  if candidates.len != 1: return
  let (ivSym, incPos) = candidates[0]
  if ivSym in addrTaken: return              # IV's address escapes — unsafe

  var writes = 0
  countWritesOf(body, ivSym, writes)
  if writes != 1: return

  result = (ivSym, incPos, true)

# ---- access collection ----------------------------------------------------

proc collectAccesses(c: var Context; start: Cursor; ivSym: SymId;
                     accesses: var seq[AccessInfo]) =
  ## Walks the subtree at `start` collecting `(at SYMBOL ivSym)` accesses;
  ## does not descend into nested loops.
  if not start.hasMore: return
  case start.kind
  of TagLit:
    if start.stmtKind in {WhileS, LoopS}:
      return
    if start.exprKind == AtC:
      var arr = start
      inc arr                                # past `(at`
      var idx = arr
      skip idx                               # past arr
      if arr.kind == Symbol and idx.kind == Symbol and symId(idx) == ivSym:
        accesses.add AccessInfo(arrSym: symId(arr),
                                pos: cursorToPosition(c.orig[], start),
                                atCursor: start)
        return                               # don't recurse into the matched at
    var n = start
    n.loopInto:
      collectAccesses(c, n, ivSym, accesses)
      skip n
  else: discard

# ---- synthesis -----------------------------------------------------------

proc synthBuf(c: var Context; cap: int): TokenBuf =
  ## A scratch buffer sharing `orig`'s pool/tags so synthesized subtrees splice
  ## in bulk and their symbols/tags resolve identically.
  createTokenBuf(cap, c.orig[].pool, c.orig[].tags)

proc addPtrVarDecl(c: var Context; p: string; atCursor: Cursor): int =
  ## Synthesize `(var :p . . (addr <atCursor>))`.
  result = c.synth.len
  let info = rawLineInfo(atCursor)
  var buf = synthBuf(c, 16)
  buf.openTag TagId(ord(VarTagId))
  if info.isValid: buf.appendLineInfo info
  buf.addSymDef p
  buf.addDotToken()                          # pragmas
  # Declared type: `(ptr typeof <atCursor>)`. Spelled out rather than left to the
  # backend's re-inference, for the same reason as `cse.emitTempType`.
  if c.m == nil:
    buf.addDotToken()                        # no type context: self-tests only
  else:
    buf.openTag TagId(ord(PtrTagId))
    buf.addSubtree getNominalType(c.m[], atCursor)
    buf.closeTag()
  buf.openTag TagId(ord(AddrTagId))
  buf.addSubtree atCursor
  buf.closeTag()                             # close addr
  buf.closeTag()                             # close var
  c.synth.add ensureMove(buf)

proc addPtrInc(c: var Context; p: string; info: NifLineInfo): int =
  ## Synthesize `(asgn p (addr (pat p 1)))`.
  result = c.synth.len
  var buf = synthBuf(c, 10)
  buf.openTag TagId(ord(AsgnTagId))
  if info.isValid: buf.appendLineInfo info
  buf.addSymUse p
  buf.openTag TagId(ord(AddrTagId))
  buf.openTag TagId(ord(PatTagId))
  buf.addSymUse p
  buf.addIntLit 1
  buf.closeTag()                             # close pat
  buf.closeTag()                             # close addr
  buf.closeTag()                             # close asgn
  c.synth.add ensureMove(buf)

proc addDeref(c: var Context; p: string; info: NifLineInfo): int =
  ## Synthesize `(deref p)`.
  result = c.synth.len
  var buf = synthBuf(c, 4)
  buf.openTag TagId(ord(DerefTagId))
  if info.isValid: buf.appendLineInfo info
  buf.addSymUse p
  buf.closeTag()
  c.synth.add ensureMove(buf)

# ---- main traversal -------------------------------------------------------

proc tr(c: var Context; n: var Cursor)   # forward

proc transformLoop(c: var Context; n: var Cursor) =
  let loopPos = cursorToPosition(c.orig[], n)
  let body = loopBodyCursor(n)
  let ivInfo = findIV(c, body)
  if ivInfo.found:
    var accesses: seq[AccessInfo] = @[]
    collectAccesses(c, body, ivInfo.ivSym, accesses)
    var arrToP = initTable[SymId, string]()
    for acc in accesses:
      var p: string
      if acc.arrSym in arrToP:
        p = arrToP[acc.arrSym]
      else:
        p = freshTempName(c)
        arrToP[acc.arrSym] = p
        let varIdx = addPtrVarDecl(c, p, acc.atCursor)
        c.patchset.addInsert(loopPos, cursorAt(c.synth[varIdx], 0))
        let incIdx = addPtrInc(c, p, rawLineInfo(acc.atCursor))
        c.patchset.addInsert(ivInfo.incPos, cursorAt(c.synth[incIdx], 0))
      let derefIdx = addDeref(c, p, rawLineInfo(acc.atCursor))
      c.patchset.addSubst(acc.pos, cursorAt(c.synth[derefIdx], 0))

  # Recurse into the loop body so nested loops are transformed too.
  n.loopInto:
    tr(c, n)

proc registerDecl(c: var Context; n: Cursor) =
  ## Records a local's declared type in the current typenav scope, so that a
  ## later `(at LOCAL iv)` can be typed. `optdriver.rebuildTree` opens the scope
  ## and registers the proc's *params*; the body's locals are ours to add, and
  ## since `tr` walks in source order every decl is registered before the loops
  ## that can use it. Mirrors the `VarS` case of `aliasing.walk`.
  ##
  ## Without this, a body-local array whose type is a nominal symbol — e.g. the
  ## 32-byte word buffer a `char` set operation desugars to,
  ## `` `t.0.IAarrayAuS8ZS32. `` rather than a literal `(array …)` — resolves to
  ## the `(err)` sentinel and `addPtrVarDecl` bakes `(ptr (err))` into the
  ## declared type, which the backends then reject.
  if c.m == nil: return
  var probe = n
  var nameStart = default(Cursor)
  var typeStart = default(Cursor)
  probe.into:
    nameStart = probe
    skip probe                             # name
    if probe.hasMore: skip probe           # pragmas
    if probe.hasMore:
      typeStart = probe
      skip probe                           # type
    while probe.hasMore: skip probe
  if nameStart.kind == SymbolDef and not cursorIsNil(typeStart):
    c.m[].registerLocal(nameStart.symId, typeStart)

proc tr(c: var Context; n: var Cursor) =
  if not n.hasMore: return
  case n.kind
  of TagLit:
    case n.stmtKind
    of WhileS, LoopS:
      transformLoop(c, n)
    of VarS, GvarS, TvarS, ConstS:
      registerDecl(c, n)
      n.loopInto:
        tr(c, n)
    of ScopeS:
      if c.m != nil: c.m[].openScope()
      n.loopInto:
        tr(c, n)
      if c.m != nil: c.m[].closeScope()
    else:
      n.loopInto:
        tr(c, n)
  else:
    inc n

# ---- public entry --------------------------------------------------------

proc runInductionVariables*(buf: var TokenBuf; moduleSuffix = "M";
                            m: ptr MainModule = nil) =
  ## Two-phase IV strength reduction: record rewrite patches, then rebuild
  ## `buf` with pointer decls hoisted before each loop, pointer bumps before
  ## the iv-inc, and `(deref p)` substituted for each `(at arr iv)` access.
  var ctx = createContext(addr buf, moduleSuffix, m)
  var n = beginRead(buf)
  tr(ctx, n)
  if not ctx.patchset.isEmpty:
    var newBuf = ctx.patchset.apply()
    buf = ensureMove(newBuf)

# ---- self-tests ----------------------------------------------------------

when isMainModule:
  proc parse(src: string): TokenBuf =
    parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())

  proc canon(src: string): string =
    var b = parse(src)
    toString(b)

  template assertSame(buf: var TokenBuf; expected: string) =
    let got = toString(buf)
    let want = canon(expected)
    doAssert got == want, "MISMATCH\n  got:  " & got & "\n  want: " & want

  template assertUnchanged(input: string) =
    var buf = parse(input)
    let before = toString(buf)
    runInductionVariables buf
    doAssert toString(buf) == before, "expected unchanged:\n  " & input

  block simple_iv:
    var buf = parse(
      "(stmts (asgn i.0.M 0) (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))")
    runInductionVariables buf
    assertSame(buf,
      "(stmts (asgn i.0.M 0) (var :iv.p.1.M . . (addr (at arr.0.M i.0.M))) " &
      "(while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (deref iv.p.1.M)) " &
      "(asgn iv.p.1.M (addr (pat iv.p.1.M 1))) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block multiple_accesses_same_array:
    var buf = parse(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) (asgn y.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))")
    runInductionVariables buf
    assertSame(buf,
      "(stmts (var :iv.p.1.M . . (addr (at arr.0.M i.0.M))) " &
      "(while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (deref iv.p.1.M)) " &
      "(asgn y.0.M (deref iv.p.1.M)) (asgn iv.p.1.M (addr (pat iv.p.1.M 1))) " &
      "(asgn i.0.M (add (i 32) i.0.M 1)))))")

  block multiple_arrays:
    var buf = parse(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr1.0.M i.0.M)) (asgn y.0.M (at arr2.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))")
    runInductionVariables buf
    assertSame(buf,
      "(stmts (var :iv.p.1.M . . (addr (at arr1.0.M i.0.M))) " &
      "(var :iv.p.2.M . . (addr (at arr2.0.M i.0.M))) " &
      "(while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (deref iv.p.1.M)) " &
      "(asgn y.0.M (deref iv.p.2.M)) (asgn iv.p.1.M (addr (pat iv.p.1.M 1))) " &
      "(asgn iv.p.2.M (addr (pat iv.p.2.M 1))) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block iv_used_outside_at:
    assertUnchanged(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (call use.0.M i.0.M) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block no_iv_no_transform:
    assertUnchanged("(stmts (while c.0.M (stmts (asgn x.0.M 1))))")

  block iv_written_twice_disqualifies:
    assertUnchanged(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M 7) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block iv_addr_taken_disqualifies:
    assertUnchanged(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (call use.0.M (addr i.0.M)) (asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block nested_loops_inner_only:
    var buf = parse(
      "(stmts (while c.0.M (stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))))")
    runInductionVariables buf
    assertSame(buf,
      "(stmts (while c.0.M (stmts (var :iv.p.1.M . . (addr (at arr.0.M i.0.M))) " &
      "(while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (deref iv.p.1.M)) " &
      "(asgn iv.p.1.M (addr (pat iv.p.1.M 1))) (asgn i.0.M (add (i 32) i.0.M 1)))))))")

  echo "induction_variables.nim: all self-tests passed"
