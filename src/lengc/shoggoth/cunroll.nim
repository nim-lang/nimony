#
#           NIFC Complete Loop Unrolling (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.

## Complete unrolling of small constant-trip counted loops (1..8 iterations).
## Unrolls one loop per trial so `optdriver` runs cleanup and keeps the result
## only when its token count stays within `CunrollTokenSlack` of original.

import std / tables
import ".." / ".." / "lib" / [nifcoreparse, nifcdecl, lengaa]
import ".." / ".." / "models" / tags
import patchsets, countedloops

const
  MaxTrips* = 8             ## Maximum constant trip count eligible for unrolling.
  MaxBodyTokens* = 80       ## Skip large loop bodies before running trial cleanup.
  CunrollTokenSlack* = 16   ## Slack over original proc tokens: ~13..19 loop-control
                            ## tokens removed plus per-copy `scope` wrappers (0 bytes).
  ForbiddenStmts = {LoopS, WhileS, BreakS, ContinueS, TryS, OnerrS, EmitS,
                    ProcS, TypeS, GvarS, TvarS, MflagS, VflagS, JtrueS}
  ForbiddenPragmas = {StaticP, ExportcP, ImportcP, RegisterP}

type
  Plan = object
    counterSym: SymId
    isUnsigned: bool
    tokens: int
    startVal: int64
    defs, labels, jumps: seq[SymId]
    freshNames: seq[string]

  Unroller = object
    orig: ptr TokenBuf
    root: Cursor
    facts: LocalFacts
    skipCount, seen, counter, targetPos: int
    replacement: TokenBuf

proc scanBody(node: Cursor; plan: var Plan): bool =
  ## Verify `node` is small and duplicable, recording defs, labels and jumps.
  inc plan.tokens
  if plan.tokens > MaxBodyTokens: return false
  if node.kind == SymbolDef:
    if node.symId == plan.counterSym: return false
    if node.symId notin plan.defs: plan.defs.add node.symId
    return true
  if node.kind != TagLit: return true
  let stmt = node.stmtKind
  if stmt in ForbiddenStmts or node.pragmaKind in ForbiddenPragmas: return false
  if stmt == LabS and node.childCursor.kind == SymbolDef: plan.labels.add node.childCursor.symId
  elif stmt == JmpS and node.childCursor.kind == Symbol: plan.jumps.add node.childCursor.symId
  elif stmt in {LabS, JmpS}: return false
  var idx = 0
  var child = node
  result = true
  child.loopInto:
    if idx != fieldSlot(node) and result: result = scanBody(child, plan)
    inc idx
    skip child

proc emitInt(dest: var TokenBuf; val: int64; isUnsigned: bool) =
  ## Emit a signed or unsigned integer literal token.
  if isUnsigned: dest.addUIntLit cast[uint64](val)
  else: dest.addIntLit val

proc emitCopy(dest: var TokenBuf; node: Cursor; plan: Plan; iterVal: int64) =
  ## Copy `node`, freshening locals/labels and substituting the counter.
  if node.kind == TagLit:
    dest.openTag node.cursorTagId
    if rawLineInfo(node).isValid: dest.appendLineInfo rawLineInfo(node)
    var idx = 0
    var child = node
    child.loopInto:
      if idx == fieldSlot(node): dest.addSubtree child
      else: emitCopy(dest, child, plan, iterVal)
      inc idx
      skip child
    dest.closeTag()
  elif node.kind == Symbol and node.symId == plan.counterSym: emitInt(dest, iterVal, plan.isUnsigned)
  elif plan.freshNames.len > 0 and node.kind in {Symbol, SymbolDef}:
    let defIdx = plan.defs.find(node.symId)
    if defIdx < 0: dest.addSubtree node
    elif node.kind == Symbol: dest.addSymUse plan.freshNames[defIdx]
    else: dest.addSymDef plan.freshNames[defIdx]
  else: dest.addSubtree node

proc buildUnrolled(ctx: var Unroller; target: Cursor; loop: DetectedLoop;
                   stmts: seq[Cursor]; plan: var Plan; trips: int): TokenBuf =
  ## Emit `trips` scoped copies of `stmts` plus the final counter store if live.
  result = createTokenBuf(64, ctx.orig[].pool, ctx.orig[].tags)
  let dir = if loop.kind == CountDown: -1'i64 else: 1'i64
  result.openTag TagId(ord(StmtsTagId))
  for iter in 0 ..< trips:
    plan.freshNames.setLen 0
    if iter > 0:
      for _ in plan.defs:
        inc ctx.counter
        plan.freshNames.add "`cu." & $ctx.counter
    result.openTag TagId(ord(ScopeTagId))
    for stmt in stmts: emitCopy(result, stmt, plan, plan.startVal + int64(iter) * dir)
    result.closeTag()
  if countUses(ctx.root, loop.counterSym) > countUses(target, loop.counterSym):
    result.openTag TagId(ord(AsgnTagId))
    result.addSymUse loop.counterSym
    emitInt(result, plan.startVal + int64(trips) * dir, plan.isUnsigned)
    result.closeTag()
  result.closeTag()

proc trackInit(inits: var Table[SymId, Cursor]; stmt: Cursor) =
  ## Update straight-line counter initializers across `var` or `asgn`.
  let lhs = stmt.childCursor
  let target = if lhs.kind in {Symbol, SymbolDef}: lhs.symId else: SymId(0)
  var stale: seq[SymId] = @[]
  for sym in inits.keys:
    if sym == target or countWrites(stmt, sym) > 0: stale.add sym
  for sym in stale: inits.del sym
  let val = startValue(stmt, target)
  if target != SymId(0) and not cursorIsNil(val): inits[target] = val

proc tryCandidate(ctx: var Unroller; stmt: Cursor; inits: Table[SymId, Cursor]): bool =
  ## Match `stmt` as an eligible counted loop; unroll or skip it.
  result = false
  let loop = detectLoop(if stmt.stmtKind == IteS: guardedLoop(stmt) else: unwrapSingleLoop(stmt))
  if loop.kind == NoLoop or not ctx.facts.isOwned(loop.counterSym): return
  let startCur = inits.getOrDefault(loop.counterSym)
  if cursorIsNil(startCur) or (stmt.stmtKind == IteS and
     not sameTest(stmt.childCursor, loop.cond, loop.counterSym, startCur)): return
  var plan = Plan(counterSym: loop.counterSym)
  let trips = constTripCount(loop, startCur, plan.startVal, plan.isUnsigned)
  let stmts = if trips in 1 .. MaxTrips: bodyStmts(loop) else: @[]
  if stmts.len == 0: return
  for bodyStmt in stmts:
    if not scanBody(bodyStmt, plan): return
  for target in plan.jumps:
    if target notin plan.labels: return
  for lbl in plan.labels:
    if countUses(ctx.root, lbl) > countUses(stmt, lbl): return
  if ctx.seen < ctx.skipCount: inc ctx.seen
  else:
    ctx.replacement = buildUnrolled(ctx, stmt, loop, stmts, plan, int(trips))
    ctx.targetPos = cursorToPosition(ctx.orig[], stmt)
  result = true

proc walk(ctx: var Unroller; node: Cursor; inits: var Table[SymId, Cursor]) =
  ## Walk `node` in source order and unroll the next eligible counted loop.
  if ctx.targetPos >= 0 or node.kind != TagLit or node.stmtKind in {ProcS, TypeS}: return
  let stmt = node.stmtKind
  if stmt in {VarS, AsgnS}: trackInit(inits, node)
  elif stmt in {LoopS, ScopeS, IteS} and tryCandidate(ctx, node, inits): inits.clear()
  else:
    if stmt != StmtsS: inits.clear()
    var nested = default(Table[SymId, Cursor])
    var child = node
    child.loopInto:
      if stmt == StmtsS: walk(ctx, child, inits)
      else:
        if stmt != ScopeS: nested.clear()
        walk(ctx, child, nested)
      skip child

proc tryUnrollOne*(buf: var TokenBuf; skipCount: int;
                   outBuf: var TokenBuf; minted: var int): bool =
  ## Unroll the next eligible loop after skipping `skipCount` earlier candidates.
  result = false
  let root = beginRead(buf)
  if root.kind != TagLit: return
  var ctx = Unroller(orig: addr buf, root: root, facts: analyseLocals(root),
                     skipCount: skipCount, counter: minted, targetPos: -1)
  var inits = default(Table[SymId, Cursor])
  walk(ctx, root, inits)
  if ctx.targetPos >= 0:
    var patchset = initPatchset(addr buf)
    patchset.addSubst(ctx.targetPos, cursorAt(ctx.replacement, 0))
    outBuf = patchset.apply()
    minted = ctx.counter
    result = true

when isMainModule:
  import std / [assertions, strutils]

  proc parse(src: string): TokenBuf =
    parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())

  proc flat(s: string): string =
    result = ""
    var prevSpace = false
    for ch in s:
      if ch in {' ', '\n', '\r', '\t'}:
        if not prevSpace and result.len > 0 and result[^1] != '(':
          result.add ' '
          prevSpace = true
      else:
        if prevSpace and ch == ')': result.setLen result.len - 1
        result.add ch
        prevSpace = false

  proc unrollOnce(src: string; skipCount = 0): (string, bool) =
    var orig = parse(src)
    var outBuf = TokenBuf()
    var minted = 0
    let ok = tryUnrollOne(orig, skipCount, outBuf, minted)
    if ok: (flat(toString(outBuf, includeLineInfo = false)), true)
    else: (flat(toString(orig, includeLineInfo = false)), false)

  block unrolls_bare_and_guarded_vec4:
    let (got, ok) = unrollOnce(
      "(stmts (var :i.0 . (i 64) 0) " &
      "(stmts (loop (stmts (asgn s.0 (add (i 64) s.0 (at a.0 i.0))) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 4))))))")
    doAssert ok and "(loop" notin got, got
    doAssert "(at a.0 0)" in got and "(at a.0 3)" in got, got
    doAssert "(asgn i.0" notin got, "dead counter must not emit final store: " & got

  block unrolls_guarded_with_intervening_var:
    let (got, ok) = unrollOnce(
      "(stmts (var :i.0 . (i 64) 0) (var :s.0 . (i 64) 0) " &
      "(ite (lt 0 3) (stmts (loop (stmts (asgn s.0 (add (i 64) s.0 i.0)) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 3))))) .))")
    doAssert ok and "(loop" notin got and "(ite" notin got, got
    doAssert got.count("(scope") == 3, got

  block unrolls_inside_scope:
    let (got, ok) = unrollOnce(
      "(scope (var :i.0 . (i 64) 0) " &
      "(loop (stmts (asgn s.0 (add (i 64) s.0 i.0)) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 3)))))")
    doAssert ok and "(loop" notin got and got.count("(scope") == 4, got

  block preserves_live_counter_final_value:
    let (got, ok) = unrollOnce(
      "(stmts (var :i.0 . (i 64) 0) " &
      "(stmts (loop (stmts (asgn s.0 (add (i 64) s.0 i.0)) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 4))))) " &
      "(asgn r.0 (add (i 64) s.0 i.0)))")
    doAssert ok and "(asgn i.0 4)" in got, got

  block unrolls_countdown_inclusive_until_and_unsigned:
    let (downGot, downOk) = unrollOnce(
      "(stmts (var :i.0 . (i 64) 2) " &
      "(stmts (loop (stmts (asgn s.0 (add (i 64) s.0 i.0)) " &
      "(asgn i.0 (sub (i 64) i.0 1)) (continue (le 0 i.0))))) " &
      "(asgn r.0 i.0))")
    doAssert downOk and downGot.count("(scope") == 3 and "(asgn i.0 -1)" in downGot, downGot
    let (uGot, uOk) = unrollOnce(
      "(stmts (var :i.0 . (u 32) 0u) " &
      "(stmts (loop (stmts (asgn s.0 (add (u 32) s.0 i.0)) " &
      "(asgn i.0 (add (u 32) i.0 1u)) (continue (neq i.0 3u))))) " &
      "(asgn r.0 i.0))")
    doAssert uOk and uGot.count("(scope") == 3 and "(asgn i.0 3u)" in uGot, uGot

  block skips_trip_zero_and_trip_nine:
    let (_, ok0) = unrollOnce(
      "(stmts (var :i.0 . (i 64) 0) " &
      "(ite (lt 0 0) (stmts (loop (stmts (asgn x.0 i.0) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 0))))) .))")
    doAssert not ok0, "trip 0 must be left alone"
    let (_, ok9) = unrollOnce(
      "(stmts (var :i.0 . (i 64) 0) " &
      "(stmts (loop (stmts (asgn s.0 (add (i 64) s.0 i.0)) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 9))))))")
    doAssert not ok9, "trip 9 must be left alone"

  block freshens_locals_labels_and_keeps_fields:
    let (got, ok) = unrollOnce(
      "(stmts (var :i.0 . (i 64) 0) " &
      "(stmts (loop (stmts " &
      "(var :b.0 . (i 64) (add (i 64) (dot o.0 b.0 0) i.0)) " &
      "(ite (eq b.0 0) (stmts (jmp done.0)) .) (lab :done.0) " &
      "(asgn s.0 (add (i 64) s.0 b.0)) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 2))))))")
    doAssert ok and "(loop" notin got, got
    doAssert got.count("(dot o.0 b.0 0)") == 2, got
    doAssert got.count(":b.0") == 1 and ":`cu." in got, got
    doAssert got.count(":done.0") == 1, got

  block skips_break_continue_jmp_try_static_and_entry_jmp:
    let withBreak =
      "(stmts (var :i.0 . (i 64) 0) (stmts (loop (stmts " &
      "(ite (eq i.0 2) (stmts (break)) .) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 4))))))"
    doAssert not unrollOnce(withBreak)[1]
    let withInnerContinue =
      "(stmts (var :i.0 . (i 64) 0) (stmts (loop (stmts " &
      "(ite (eq i.0 2) (stmts (continue .)) .) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 4))))))"
    doAssert not unrollOnce(withInnerContinue)[1]
    let withEscapeJmp =
      "(stmts (var :i.0 . (i 64) 0) (stmts (loop (stmts " &
      "(ite (eq i.0 2) (stmts (jmp exit.0)) .) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 4))))) (lab :exit.0))"
    doAssert not unrollOnce(withEscapeJmp)[1]
    let withEntryJmp =
      "(stmts (var :i.0 . (i 64) 0) (ite flag.0 (stmts (jmp inside.0)) .) " &
      "(stmts (loop (stmts (lab :inside.0) (asgn s.0 i.0) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 3))))))"
    doAssert not unrollOnce(withEntryJmp)[1]
    let withTry =
      "(stmts (var :i.0 . (i 64) 0) (stmts (loop (stmts " &
      "(try (stmts (asgn s.0 i.0)) .) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 3))))))"
    doAssert not unrollOnce(withTry)[1]
    let withStatic =
      "(stmts (var :i.0 . (i 64) 0) (stmts (loop (stmts " &
      "(var :st.0 (pragmas (static)) (i 64) 0) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 3))))))"
    doAssert not unrollOnce(withStatic)[1]

  block skips_exposed_and_clobbered_counter:
    let withAddr =
      "(stmts (var :i.0 . (i 64) 0) (var :p.0 . (ptr (i 64)) (addr i.0)) " &
      "(stmts (loop (stmts (asgn s.0 (add (i 64) s.0 i.0)) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 3))))))"
    doAssert not unrollOnce(withAddr)[1]
    let withEmit =
      "(stmts (var :i.0 . (i 64) 0) " &
      "(stmts (loop (stmts (asgn s.0 (add (i 64) s.0 i.0)) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 3))))) (emit i.0))"
    doAssert not unrollOnce(withEmit)[1]
    let src =
      "(stmts (var :i.0 . (i 64) 0) " &
      "(if (elif flag.0 (stmts (asgn i.0 2)))) " &
      "(ite (lt i.0 4) (stmts (loop (stmts (asgn s.0 (add (i 64) s.0 i.0)) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 4))))) .))"
    doAssert not unrollOnce(src)[1]
    let withHaddr =
      "(stmts (var :i.0 . (i 64) 0) (var :t.0 . (i 64) (call mut.0 (haddr i.0))) " &
      "(stmts (loop (stmts (asgn s.0 (add (i 64) s.0 i.0)) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 3))))))"
    doAssert not unrollOnce(withHaddr)[1]
    let withScopeMut =
      "(stmts (var :i.0 . (i 64) 0) (scope (asgn i.0 2)) " &
      "(stmts (loop (stmts (asgn s.0 (add (i 64) s.0 i.0)) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 3))))))"
    doAssert not unrollOnce(withScopeMut)[1]

  block skip_count_selects_second_loop:
    let src =
      "(stmts (var :i.0 . (i 64) 0) " &
      "(stmts (loop (stmts (asgn a.0 (add (i 64) a.0 i.0)) " &
      "(asgn i.0 (add (i 64) i.0 1)) (continue (lt i.0 2))))) " &
      "(var :j.0 . (i 64) 0) " &
      "(stmts (loop (stmts (asgn b.0 (add (i 64) b.0 j.0)) " &
      "(asgn j.0 (add (i 64) j.0 1)) (continue (lt j.0 2))))))"
    let (got, ok) = unrollOnce(src, skipCount = 1)
    doAssert ok and got.count("(loop") == 1, got
    doAssert "(asgn a.0 (add (i 64)a.0 i.0))" in got, got
    doAssert "(asgn b.0 (add (i 64)b.0 1))" in got, got

  echo "cunroll.nim: all self-tests passed"
