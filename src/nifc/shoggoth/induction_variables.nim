#
#
#           NIFC Induction-Variable Strength Reduction
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Replace `(at arr iv)` accesses inside a loop with `(deref p)` for
## a pointer `p` that is initialized once before the loop and advanced
## by one element per iteration. The expensive `arr + iv * sizeof(elem)`
## computation becomes a single pointer addition.
##
## Pattern detected (the MVP):
##
## ```
## (while cond
##   (stmts
##     … uses of `(at arr iv)` …
##     (asgn iv (add T iv 1))))
## ```
##
## The loop body's unique write to `iv` must be `(asgn iv (add T iv 1))`.
## Additionally, the IV's address must **not** be taken in the body —
## otherwise a call (or any pointer-write through the alias) could
## mutate it and break the "increments by exactly 1 per iteration"
## invariant. Other steps, non-Symbol array bases, and
## `(at (other-expr) iv)` patterns are not handled — flagged as
## follow-ups.
##
## Rewrite:
##
## ```
## (var :p . . (addr (at arr iv)))       ; inserted before the while
## (while cond
##   (stmts
##     … `(deref p)` …
##     (asgn p (addr (pat p 1)))          ; inserted before the iv-inc
##     (asgn iv (add T iv 1))))
## ```
##
## All structural mutations go through the shared
## [patchsets](patchsets.nim) module.

import std / [tables, sets, hashes, assertions]
include "../../lib" / nifprelude
import nifstreams, nifcursors
import ".." / nifc_model
import ".." / ".." / models / tags
import patchsets, nifrender

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

proc createContext(orig: ptr TokenBuf; moduleSuffix: string): Context =
  Context(orig: orig,
          patchset: initPatchset(orig),
          synth: @[],
          tempCounter: 0,
          moduleSuffix: moduleSuffix)

proc freshTempSym(c: var Context): SymId =
  inc c.tempCounter
  result = pool.syms.getOrIncl("iv.p." & $c.tempCounter & "." & c.moduleSuffix)

# ---- pattern detection ----------------------------------------------------

proc isIvIncPattern(c: Cursor; outIvSym: var SymId): bool =
  ## Matches `(asgn lhs (add T lhs 1))`. Sets `outIvSym` to `lhs.symId`
  ## on success.
  if c.kind != ParLe: return false
  if c.stmtKind notin {AsgnS, StoreS}: return false
  var probe = c
  inc probe                                  # past `(asgn`
  if probe.kind != Symbol: return false
  let lhsSym = probe.symId
  skip probe                                 # past lhs
  if probe.kind != ParLe or probe.exprKind != AddC: return false
  var rhs = probe
  inc rhs                                    # past `(add`
  skip rhs                                   # past the type
  if rhs.kind != Symbol or rhs.symId != lhsSym: return false
  skip rhs                                   # past `iv`
  if rhs.kind != IntLit: return false
  if pool.integers[rhs.intId] != 1: return false
  outIvSym = lhsSym
  return true

proc loopBodyCursor(loopCursor: Cursor): Cursor =
  ## Returns a cursor at the loop body subtree. Works for `(while cond
  ## body)` and `(loop pre cond body after)`.
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
  ## Counts Symbol-LHS assignments to `sym` anywhere in the subtree at
  ## `start`. Nested loops are included so that a write to the IV
  ## inside an inner loop disqualifies the outer IV analysis.
  if not start.hasMore: return
  case start.kind
  of ParLe:
    if start.stmtKind in {AsgnS, StoreS}:
      var lhs = start
      inc lhs
      if lhs.kind == Symbol and lhs.symId == sym:
        inc counter
    var n = start
    n.loopInto:
      countWritesOf(n, sym, counter)
      skip n
  else: discard

proc scanBody(c: var Context; n: Cursor;
              candidates: var seq[tuple[ivSym: SymId, incPos: int]];
              addrTaken: var HashSet[SymId]) =
  ## Single pre-scan: collects iv-inc candidates AND every symbol whose
  ## address is taken anywhere in the body. `findIV` then rules out any
  ## candidate whose address escapes.
  if not n.hasMore or n.kind != ParLe: return
  var ivSym = SymId(0)
  if isIvIncPattern(n, ivSym):
    candidates.add (ivSym, cursorToPosition(c.orig[], n))
    return                                   # iv-inc's children can't matter
  if n.exprKind == AddrC:
    # Capture the addressed symbol (first Symbol in the lvalue chain)
    # and stop — the children of `(addr …)` aren't independent uses.
    var probe = n
    inc probe                                # past `(addr`
    while probe.hasMore:
      if probe.kind == Symbol:
        addrTaken.incl probe.symId
        break
      elif probe.kind == ParLe:
        inc probe                            # descend into chain
      else:
        inc probe
    return
  var m = n
  m.loopInto:
    scanBody(c, m, candidates, addrTaken)
    skip m

proc findIV(c: var Context; body: Cursor): tuple[ivSym: SymId, incPos: int, found: bool] =
  ## Identifies the induction variable of a loop body. Requires:
  ## - exactly one `(asgn iv (add T iv 1))` in the body;
  ## - no other writes to `iv` anywhere in the body;
  ## - `iv`'s address is not taken in the body (otherwise a call or
  ##   any pointer-write through the alias could break the invariant).
  result = (SymId(0), 0, false)
  if body.kind != ParLe: return

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
  ## Walks the subtree at `start` collecting `(at SYMBOL ivSym)`
  ## accesses. Does not descend into nested loops — those have their
  ## own IV analysis.
  if not start.hasMore: return
  case start.kind
  of ParLe:
    if start.stmtKind in {WhileS, LoopS}:
      return
    if start.exprKind == AtC:
      var arr = start
      inc arr                                # past `(at`
      var idx = arr
      skip idx                               # past arr
      if arr.kind == Symbol and idx.kind == Symbol and idx.symId == ivSym:
        accesses.add AccessInfo(arrSym: arr.symId,
                                pos: cursorToPosition(c.orig[], start),
                                atCursor: start)
        return                               # don't recurse into the matched at
    var n = start
    n.loopInto:
      collectAccesses(c, n, ivSym, accesses)
      skip n
  else: discard

# ---- synthesis -----------------------------------------------------------

proc addPtrVarDecl(c: var Context; p: SymId; atCursor: Cursor): int =
  ## Synthesize `(var :p . . (addr <atCursor>))`.
  result = c.synth.len
  let info = atCursor.info
  var buf = createTokenBuf(16)
  buf.addParLe(TagId(ord(VarTagId)), info)
  buf.addSymDef p, info
  buf.addDotToken()                          # pragmas
  buf.addDotToken()                          # type — inferred from init
  buf.addParLe(TagId(ord(AddrTagId)), info)
  buf.addSubtree atCursor
  buf.addParRi()                             # close addr
  buf.addParRi()                             # close var
  c.synth.add buf

proc addPtrInc(c: var Context; p: SymId; info: PackedLineInfo): int =
  ## Synthesize `(asgn p (addr (pat p 1)))`.
  result = c.synth.len
  var buf = createTokenBuf(10)
  buf.addParLe(TagId(ord(AsgnTagId)), info)
  buf.addSymUse p, info
  buf.addParLe(TagId(ord(AddrTagId)), info)
  buf.addParLe(TagId(ord(PatTagId)), info)
  buf.addSymUse p, info
  buf.addIntLit 1, info
  buf.addParRi()                             # close pat
  buf.addParRi()                             # close addr
  buf.addParRi()                             # close asgn
  c.synth.add buf

proc addDeref(c: var Context; p: SymId; info: PackedLineInfo): int =
  ## Synthesize `(deref p)`.
  result = c.synth.len
  var buf = createTokenBuf(4)
  buf.addParLe(TagId(ord(DerefTagId)), info)
  buf.addSymUse p, info
  buf.addParRi()
  c.synth.add buf

# ---- main traversal -------------------------------------------------------

proc tr(c: var Context; n: var Cursor)   # forward

proc transformLoop(c: var Context; n: var Cursor) =
  let loopPos = cursorToPosition(c.orig[], n)
  let body = loopBodyCursor(n)
  let ivInfo = findIV(c, body)
  if ivInfo.found:
    var accesses: seq[AccessInfo] = @[]
    collectAccesses(c, body, ivInfo.ivSym, accesses)
    var arrToP = initTable[SymId, SymId]()
    for acc in accesses:
      var p: SymId
      if acc.arrSym in arrToP:
        p = arrToP[acc.arrSym]
      else:
        p = freshTempSym(c)
        arrToP[acc.arrSym] = p
        let varIdx = addPtrVarDecl(c, p, acc.atCursor)
        c.patchset.addInsert(loopPos, cursorAt(c.synth[varIdx], 0))
        let incIdx = addPtrInc(c, p, acc.atCursor.info)
        c.patchset.addInsert(ivInfo.incPos, cursorAt(c.synth[incIdx], 0))
      let derefIdx = addDeref(c, p, acc.atCursor.info)
      c.patchset.addSubst(acc.pos, cursorAt(c.synth[derefIdx], 0))

  # Recurse into the loop body so we can transform nested loops too.
  n.loopInto:
    tr(c, n)

proc tr(c: var Context; n: var Cursor) =
  if not n.hasMore: return
  case n.kind
  of ParLe:
    if n.stmtKind in {WhileS, LoopS}:
      transformLoop(c, n)
    else:
      n.loopInto:
        tr(c, n)
  else:
    inc n

# ---- public entry --------------------------------------------------------

proc runInductionVariables*(buf: var TokenBuf; moduleSuffix = "M") =
  ## Two-phase IV strength reduction: identify loops with a recognized
  ## IV pattern, record the rewrite patches, then rebuild `buf` with
  ## pointer declarations hoisted before each such loop, pointer bumps
  ## inserted before the iv-inc, and `(deref p)` substituted for each
  ## `(at arr iv)` access.
  var ctx = createContext(addr buf, moduleSuffix)
  var n = beginRead(buf)
  tr(ctx, n)
  if not ctx.patchset.isEmpty:
    var newBuf = ctx.patchset.apply()
    buf = ensureMove(newBuf)

# ---- self-tests ----------------------------------------------------------

when isMainModule:
  proc parse(src: string): TokenBuf =
    var stream = nifstreams.openFromBuffer(src, "M")
    result = fromStream(stream)

  template assertUnchanged(input: string) =
    var buf = parse(input)
    let before = render(buf)
    runInductionVariables buf
    assertRender(buf, before)

  discard pool.syms.getOrIncl("i.0.M")
  discard pool.syms.getOrIncl("x.0.M")
  discard pool.syms.getOrIncl("y.0.M")
  discard pool.syms.getOrIncl("c.0.M")
  discard pool.syms.getOrIncl("arr.0.M")
  discard pool.syms.getOrIncl("arr1.0.M")
  discard pool.syms.getOrIncl("arr2.0.M")
  discard pool.syms.getOrIncl("use.0.M")
  discard pool.syms.getOrIncl("lt.0.M")

  block simple_iv:
    var buf = parse(
      "(stmts (asgn i.0.M 0) (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))")
    runInductionVariables buf
    assertRender(buf, """
(stmts
(asgn i.0.M 0)
(var :iv.p.1.M . .
(addr
(at arr.0.M i.0.M)))
(while
(lt
(i 32)i.0.M 10)
(stmts
(asgn x.0.M
(deref iv.p.1.M))
(asgn iv.p.1.M
(addr
(pat iv.p.1.M 1)))
(asgn i.0.M
(add
(i 32)i.0.M 1)))))""")

  block multiple_accesses_same_array:
    var buf = parse(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) (asgn y.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))")
    runInductionVariables buf
    assertRender(buf, """
(stmts
(var :iv.p.1.M . .
(addr
(at arr.0.M i.0.M)))
(while
(lt
(i 32)i.0.M 10)
(stmts
(asgn x.0.M
(deref iv.p.1.M))
(asgn y.0.M
(deref iv.p.1.M))
(asgn iv.p.1.M
(addr
(pat iv.p.1.M 1)))
(asgn i.0.M
(add
(i 32)i.0.M 1)))))""")

  block multiple_arrays:
    var buf = parse(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr1.0.M i.0.M)) (asgn y.0.M (at arr2.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))")
    runInductionVariables buf
    assertRender(buf, """
(stmts
(var :iv.p.1.M . .
(addr
(at arr1.0.M i.0.M)))
(var :iv.p.2.M . .
(addr
(at arr2.0.M i.0.M)))
(while
(lt
(i 32)i.0.M 10)
(stmts
(asgn x.0.M
(deref iv.p.1.M))
(asgn y.0.M
(deref iv.p.2.M))
(asgn iv.p.1.M
(addr
(pat iv.p.1.M 1)))
(asgn iv.p.2.M
(addr
(pat iv.p.2.M 1)))
(asgn i.0.M
(add
(i 32)i.0.M 1)))))""")

  block iv_used_outside_at:
    # IV is incremented but no `(at … iv)` accesses → no rewrite.
    assertUnchanged(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (call use.0.M i.0.M) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block no_iv_no_transform:
    assertUnchanged("(stmts (while c.0.M (stmts (asgn x.0.M 1))))")

  block iv_written_twice_disqualifies:
    # IV is incremented AND assigned to a constant → disqualified.
    assertUnchanged(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M 7) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block iv_addr_taken_disqualifies:
    # `(addr i)` inside the body — a callee (or any pointer-write
    # through the resulting alias) could mutate `i`, breaking the
    # `i += 1` invariant. The transformation must be skipped.
    assertUnchanged(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (call use.0.M (addr i.0.M)) (asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block nested_loops_inner_only:
    # Outer loop has no IV pattern; inner does. The var decl is hoisted
    # *inside* the outer body, just before the inner while.
    var buf = parse(
      "(stmts (while c.0.M (stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))))")
    runInductionVariables buf
    assertRender(buf, """
(stmts
(while c.0.M
(stmts
(var :iv.p.1.M . .
(addr
(at arr.0.M i.0.M)))
(while
(lt
(i 32)i.0.M 10)
(stmts
(asgn x.0.M
(deref iv.p.1.M))
(asgn iv.p.1.M
(addr
(pat iv.p.1.M 1)))
(asgn i.0.M
(add
(i 32)i.0.M 1)))))))""")

  echo "induction_variables.nim: all self-tests passed"
