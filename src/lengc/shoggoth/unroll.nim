#
#
#           NIFC Full Loop Unrolling (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Fully unrolls a counted loop whose trip count is a compile-time constant.
## The shape is what `lengcgen.trLoop` makes of a `for i in a..<b` (inlined
## iterator and `inc` included):
##
##   (var :i . (i 64) START)
##   (while (true)
##     (scope
##       (if (elif (lt i LIMIT) (stmts BODY… (asgn i (add (i 64) i STEP)) …))
##           (else (stmts … (jmp Lexit))))))
##
## becomes, for the trip count `t`,
##
##   (stmts (scope BODY[i := START]) (scope BODY[i := START+STEP]) …
##          (stmts …else…[i := START+t*STEP]))
##
## Every copy reads its iteration's index as a LITERAL, so `(at a i)` becomes
## `(at a 2)`, `(mul (i 64) i 3)` folds, and the compare-and-branch per
## iteration is gone. The increment is dropped; the copies after the first get
## fresh names for their declarations and labels (`` `ur.<n> ``).
##
## Soundness gates:
##  * START, LIMIT and STEP are integer literals, STEP > 0, the test is `lt` or
##    `le` with the induction variable on the left, and `i` is declared, with
##    START as its initializer, by the statement right before the loop with a
##    signed integer type;
##  * the increment is the ONLY assignment to `i` in the loop, and it is on the
##    body's straight-line path (not under an `if`, `case` or inner loop) — so
##    every iteration that does not leave the loop advances `i` by STEP exactly
##    once;
##  * no jump skips the increment: a jump in front of it may not target a label
##    behind it. Jumps are forward and scoped (multi-join IR), so a jump out of
##    the body is an exit and stays one; a label inside the body is reached only
##    from its own copy;
##  * `i` is not address-taken and not read after the loop: an early exit
##    leaves the loop with an `i` no copy materializes;
##  * the else arm ends in its jump out of the loop (it runs once, at the end);
##  * `t <= MaxUnrollTrip` and `t * span(BODY) <= MaxUnrollTokens`.
##
## Innermost loops first, one per round; an unrolled inner loop counts with
## its full size towards the enclosing loop's budget.

import std / [assertions, tables, sets]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/substructureKind
import ".." / ".." / "models" / tags         # tag ids for synthesis
import patchsets

const
  MaxUnrollTrip = 16       ## at most this many copies
  MaxUnrollTokens = 512    ## trip count × body span
  MaxRounds = 32
  ValueLimit = 1'i64 shl 40  ## keeps the trip arithmetic far from overflow

type
  Loop = object
    whilePos: int            ## the `while` statement (orig position)
    iv: SymId
    start, step, trip: int64
    headPos: int             ## the list holding PRE… and the test's `if`
    ifPos: int               ## the test's `if`
    thenPos: int             ## the elif arm's body
    elsePos: int             ## the else arm's body
    incPos: int              ## the increment `asgn`

  Context = object
    orig: ptr TokenBuf
    counter: int
    addrTaken: HashSet[SymId]
    uses: Table[SymId, int]  ## occurrences of every symbol in the proc body

proc child0(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

proc pos(c: Context; n: Cursor): int {.inline.} = cursorToPosition(c.orig[], n)

proc spanOf(c: Context; n: Cursor): int =
  var e = n
  skip e
  result = c.pos(e) - c.pos(n)

# ── literals ─────────────────────────────────────────────────────────────────

proc intLitOf(n: Cursor; v: var int64): bool =
  ## An integer literal, possibly wrapped as `(conv T LIT)` or `(suf LIT "…")`
  ## — the inliner leaves `inc`'s step as `(conv (i 64) 1)`.
  case n.kind
  of IntLit:
    v = intVal(n)
    result = v > -ValueLimit and v < ValueLimit
  of TagLit:
    case n.exprKind
    of ConvC:
      var it = child0(n)
      skip it                                    # the type
      result = intLitOf(it, v)
    of SufC:
      result = intLitOf(child0(n), v)
    else: result = false
  else: result = false

proc isSignedIntType(n: Cursor): bool =
  n.kind == TagLit and n.typeKind == IT

# ── proc-wide facts ──────────────────────────────────────────────────────────

proc slotRoot(c: Cursor): SymId =
  result = SymId(0)
  var n = c
  while true:
    case n.kind
    of Symbol: return n.symId
    of TagLit:
      case n.exprKind
      of DotC, AtC: inc n
      of ConvC, CastC:
        inc n; skip n
      else: return SymId(0)
    else: return SymId(0)

proc collectFacts(c: var Context; n: Cursor) =
  case n.kind
  of Symbol:
    c.uses.mgetOrPut(n.symId, 0) += 1
  of TagLit:
    if n.exprKind in AddrKinds:
      let s = slotRoot(child0(n))
      if s != SymId(0): c.addrTaken.incl s
    var it = n
    it.loopInto:
      collectFacts(c, it)
      skip it
  else: discard

proc countUses(n: Cursor; s: SymId): int =
  case n.kind
  of Symbol: result = (if n.symId == s: 1 else: 0)
  of TagLit:
    result = 0
    var it = n
    it.loopInto:
      result += countUses(it, s)
      skip it
  else: result = 0

# ── matching the counted-loop shape ──────────────────────────────────────────

proc loopHead(n: Cursor; head, ifn: var Cursor): bool =
  ## The loop body is `(scope PRE… (if …))`, possibly under more single-child
  ## `scope`/`stmts` wrappers: PRE runs every iteration in front of the test
  ## (the inliner leaves empty label scopes there). Finds the list holding the
  ## test (`head`) and the test's `if` (`ifn`).
  result = false
  var cur = n
  while cur.kind == TagLit and cur.stmtKind in {ScopeS, StmtsS}:
    var last = default(Cursor)
    var count = 0
    var it = cur
    it.loopInto:
      last = it
      inc count
      skip it
    if count == 0: return
    if last.kind == TagLit and last.stmtKind == IfS:
      head = cur
      ifn = last
      return true
    if count != 1: return
    cur = last

proc endsInJmp(n: Cursor): bool =
  ## Does the statement list `n` end, on its straight-line path, in a `jmp`?
  if n.kind != TagLit: return false
  case n.stmtKind
  of JmpS: result = true
  of StmtsS, ScopeS:
    var last = default(Cursor)
    var any = false
    var it = n
    it.loopInto:
      last = it
      any = true
      skip it
    result = any and endsInJmp(last)
  else: result = false

proc scanAssigns(n: Cursor; iv: SymId; straight: bool;
                 incs: var seq[(int, bool)]; bad: var bool; orig: ptr TokenBuf) =
  ## Every write to `iv` under `n`: plain `asgn` targets are recorded with
  ## whether they are on the straight-line path; anything else that could
  ## write it (a declaration, a write through a projection) is `bad`.
  if n.kind != TagLit: return
  case n.stmtKind
  of AsgnS:
    let dst = child0(n)
    if dst.kind == Symbol and dst.symId == iv:
      incs.add (cursorToPosition(orig[], n), straight)
    elif slotRoot(dst) == iv:
      bad = true
  of VarS, ConstS, GvarS, TvarS:
    let d = child0(n)
    if d.kind == SymbolDef and d.symId == iv: bad = true
  of JtrueS:
    var it = n
    it.loopInto:
      if it.kind == Symbol and it.symId == iv: bad = true
      skip it
    return
  else: discard
  let inner = straight and n.stmtKind in {StmtsS, ScopeS}
  var it = n
  it.loopInto:
    scanAssigns(it, iv, inner, incs, bad, orig)
    skip it

proc collectLabels(n: Cursor; after: int; labels: var HashSet[SymId];
                   orig: ptr TokenBuf) =
  ## Labels defined under `n` behind position `after`.
  if n.kind != TagLit: return
  if n.stmtKind == LabS and cursorToPosition(orig[], n) > after:
    let d = child0(n)
    if d.kind == SymbolDef: labels.incl d.symId
  var it = n
  it.loopInto:
    collectLabels(it, after, labels, orig)
    skip it

proc jumpsInto(n: Cursor; before: int; labels: HashSet[SymId];
               orig: ptr TokenBuf): bool =
  ## Does a `jmp` under `n`, in front of position `before`, target `labels`?
  if n.kind != TagLit: return false
  if cursorToPosition(orig[], n) >= before: return false
  if n.stmtKind == JmpS:
    let t = child0(n)
    return t.kind == Symbol and t.symId in labels
  result = false
  var it = n
  it.loopInto:
    if jumpsInto(it, before, labels, orig): return true
    skip it

proc tripCount(start, limit, step: int64; inclusive: bool): int64 =
  if inclusive:
    result = if start > limit: 0'i64 else: (limit - start) div step + 1
  else:
    result = if start >= limit: 0'i64 else: (limit - start + step - 1) div step

proc matchLoop(c: Context; prev, w: Cursor; loop: var Loop): bool =
  ## Is `w` (with `prev` the statement in front of it) a fully unrollable
  ## counted loop? Fills `loop` when it is.
  result = false
  if w.kind != TagLit or w.stmtKind != WhileS: return
  if prev.kind != TagLit or prev.stmtKind != VarS: return
  # (var :i pragmas (i N) START)
  var iv = SymId(0)
  var start = 0'i64
  block initDecl:
    var it = child0(prev)
    if it.kind != SymbolDef: return
    iv = it.symId
    skip it                                      # name
    skip it                                      # pragmas
    if not isSignedIntType(it): return
    skip it                                      # type
    if not intLitOf(it, start): return
  if iv in c.addrTaken: return
  # (while (true) (scope PRE… (if (elif COND THEN) (else ELSE))))
  var cond = child0(w)
  if cond.kind != TagLit or cond.exprKind != TrueC: return
  var body = cond
  skip body
  var head = default(Cursor)
  var ifn = default(Cursor)
  if not loopHead(body, head, ifn): return
  var elifArm = child0(ifn)
  if elifArm.kind != TagLit or elifArm.substructureKind != ElifU: return
  var elseArm = elifArm
  skip elseArm
  if elseArm.kind != TagLit or elseArm.substructureKind != ElseU: return
  block onlyTwoArms:
    var arms = 0
    var it = ifn
    it.loopInto:
      inc arms
      skip it
    if arms != 2: return
  # COND: (lt i LIMIT) / (le i LIMIT)
  let test = child0(elifArm)
  if test.kind != TagLit or test.exprKind notin {LtC, LeC}: return
  let lhs = child0(test)
  if lhs.kind != Symbol or lhs.symId != iv: return
  var rhs = lhs
  skip rhs
  var limit = 0'i64
  if not intLitOf(rhs, limit): return
  var thenBody = test
  skip thenBody
  let elseBody = child0(elseArm)
  if not endsInJmp(elseBody): return
  # the one increment, straight-line, `(asgn i (add T i STEP))`
  var incs: seq[(int, bool)] = @[]
  var bad = false
  scanAssigns(thenBody, iv, true, incs, bad, c.orig)
  if bad or incs.len != 1 or not incs[0][1]: return
  block otherWrites:
    # neither PRE nor the exit arm may write the induction variable
    var others: seq[(int, bool)] = @[]
    var it = head
    it.loopInto:
      if c.pos(it) != c.pos(ifn): scanAssigns(it, iv, true, others, bad, c.orig)
      skip it
    scanAssigns(elseBody, iv, true, others, bad, c.orig)
    if bad or others.len > 0: return
  let incPos = incs[0][0]
  var step = 0'i64
  block incShape:
    let inc = cursorAt(c.orig[], incPos)
    var v = child0(inc)
    skip v                                       # the target
    if v.kind != TagLit or v.exprKind != AddC: return
    var it = child0(v)
    skip it                                      # the result type
    if it.kind != Symbol or it.symId != iv: return
    skip it
    if not intLitOf(it, step) or step <= 0: return
  # no jump skips the increment
  block jumps:
    var behind = initHashSet[SymId]()
    collectLabels(thenBody, incPos, behind, c.orig)
    if behind.len > 0 and jumpsInto(thenBody, incPos, behind, c.orig): return
  # `i` is read only by the loop and its declaration
  if c.uses.getOrDefault(iv, 0) != countUses(w, iv): return
  let trip = tripCount(start, limit, step, test.exprKind == LeC)
  if trip > MaxUnrollTrip: return
  if trip * c.spanOf(head) > MaxUnrollTokens: return
  loop = Loop(whilePos: c.pos(w), iv: iv, start: start, step: step, trip: trip,
              headPos: c.pos(head), ifPos: c.pos(ifn),
              thenPos: c.pos(thenBody), elsePos: c.pos(elseBody), incPos: incPos)
  result = true

proc scanLoops(c: Context; n: Cursor; found: var seq[Loop]) =
  ## Post-order: an inner loop is offered before the loop around it.
  if n.kind != TagLit or found.len > 0: return
  var prev = default(Cursor)
  var havePrev = false
  var it = n
  it.loopInto:
    scanLoops(c, it, found)
    if found.len > 0: return
    if havePrev and n.stmtKind in {StmtsS, ScopeS}:
      var loop = default(Loop)
      if matchLoop(c, prev, it, loop):
        found.add loop
        return
    prev = it
    havePrev = true
    skip it

# ── emission ─────────────────────────────────────────────────────────────────

proc collectDefs(c: var Context; n: Cursor; rename: var Table[SymId, string]) =
  case n.kind
  of SymbolDef:
    if not rename.hasKey(n.symId):
      inc c.counter
      rename[n.symId] = "`ur." & $c.counter
  of TagLit:
    var it = n
    it.loopInto:
      collectDefs(c, it, rename)
      skip it
  else: discard

proc emitCopy(c: Context; dest: var TokenBuf; n: Cursor; loop: Loop;
              value: int64; rename: Table[SymId, string]) =
  ## Copy `n` with the induction variable read as `value`, the increment
  ## dropped and declarations renamed per `rename`.
  case n.kind
  of TagLit:
    if c.pos(n) == loop.incPos: return
    let li = rawLineInfo(n)
    dest.openTag n.cursorTagId
    if li.isValid: dest.appendLineInfo li
    var it = n
    it.loopInto:
      emitCopy(c, dest, it, loop, value, rename)
      skip it
    dest.closeTag()
  of Symbol:
    if n.symId == loop.iv: dest.addIntLit value
    elif rename.hasKey(n.symId): dest.addSymUse rename.getOrDefault(n.symId)
    else: dest.addSubtree n
  of SymbolDef:
    if rename.hasKey(n.symId): dest.addSymDef rename.getOrDefault(n.symId)
    else: dest.addSubtree n
  else:
    dest.addSubtree n

proc applyLoop(c: var Context; loop: Loop): TokenBuf =
  ## Iteration `k < trip` is `PRE; THEN` with `i = START + k*STEP`; the last
  ## round, `k = trip`, is `PRE; ELSE` — the test failing is how the loop left.
  ## One rename table per round covers PRE together with the arm after it, so
  ## an arm that reads a PRE declaration reads its own round's.
  let head = cursorAt(c.orig[], loop.headPos)
  let thenBody = cursorAt(c.orig[], loop.thenPos)
  let elseBody = cursorAt(c.orig[], loop.elsePos)
  var repl = createTokenBuf(256, c.orig[].pool, c.orig[].tags)
  repl.openTag TagId(ord(StmtsTagId))
  var value = loop.start
  for k in 0'i64 .. loop.trip:
    var rename = initTable[SymId, string]()
    let arm = if k < loop.trip: thenBody else: elseBody
    if k > 0:
      var it = head
      it.loopInto:
        if c.pos(it) != loop.ifPos: collectDefs(c, it, rename)
        skip it
      collectDefs(c, arm, rename)
    repl.openTag TagId(ord(ScopeTagId))
    var it = head
    it.loopInto:
      if c.pos(it) == loop.ifPos: emitCopy(c, repl, arm, loop, value, rename)
      else: emitCopy(c, repl, it, loop, value, rename)
      skip it
    repl.closeTag()
    if k < loop.trip: value += loop.step
  repl.closeTag()
  var ps = initPatchset(c.orig)
  ps.addSubst(loop.whilePos, cursorAt(repl, 0))
  result = ps.apply()

# ── public entry ─────────────────────────────────────────────────────────────

proc runUnroll*(buf: var TokenBuf): int {.discardable.} =
  ## Fixpoint: unroll one loop per round (innermost first), rebuild, rescan.
  ## Returns the number of loops unrolled.
  result = 0
  var minted = 0
  for round in 0 ..< MaxRounds:
    var c = Context(orig: addr buf, counter: minted,
                    addrTaken: initHashSet[SymId](),
                    uses: initTable[SymId, int]())
    collectFacts(c, beginRead(buf))
    var found: seq[Loop] = @[]
    scanLoops(c, beginRead(buf), found)
    if found.len == 0: break
    var newBuf = applyLoop(c, found[0])
    minted = c.counter
    buf = ensureMove(newBuf)
    inc result

# ── self-tests ───────────────────────────────────────────────────────────────

when isMainModule:
  import std / strutils

  proc parse(src: string): TokenBuf =
    parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())

  proc unrolled(src: string): string =
    var b = parse(src)
    discard runUnroll(b)
    toString(b)

  proc canon(src: string): string =
    var b = parse(src)
    toString(b)

  proc loopSrc(start, limit: string; test = "lt"; step = "(conv (i 64) 1)";
               body = "(asgn (at a.0 i.0) (mul (i 64) i.0 3))";
               after = ""; elseBody = "(scope (jmp L.0))"): string =
    "(stmts (var :i.0 . (i 64) " & start & ") " &
    "(while (true) (scope (if (elif (" & test & " i.0 " & limit & ") " &
    "(stmts " & body & " (scope (asgn i.0 (add (i 64) i.0 " & step & ")) " &
    "(lab :rl.0)))) " &
    "(else (stmts " & elseBody & "))))) (lab :L.0)" & after & ")"

  block counted_loop_unrolled:
    let got = unrolled(loopSrc("0", "4"))
    doAssert "(while" notin got, got
    for k in 0 .. 3:
      doAssert got.count("(at a.0 " & $k & ")") == 1, got
    doAssert "(at a.0 4)" notin got, got
    doAssert "(asgn i.0" notin got, got           # the increments are gone
    doAssert got.count("(jmp L.0)") == 1, got     # the exit, once
    doAssert got.count(":rl.0") == 1, got         # labels renamed per copy
    doAssert got.count(":`ur.") == 3, got

  block inclusive_and_step:
    let got = unrolled(loopSrc("1", "7", test = "le", step = "2"))
    for k in [1, 3, 5, 7]:
      doAssert got.count("(at a.0 " & $k & ")") == 1, got
    doAssert "(at a.0 9)" notin got and "(while" notin got, got

  block zero_trip:
    let got = unrolled(loopSrc("5", "5"))
    doAssert "(while" notin got and "(at a.0" notin got, got
    doAssert got.count("(jmp L.0)") == 1, got

  block exit_arm_sees_final_value:
    let got = unrolled(loopSrc("0", "3",
      elseBody = "(asgn b.0 i.0) (scope (jmp L.0))"))
    doAssert "(asgn b.0 3)" in got, got

  block too_many_iterations:
    let src = loopSrc("0", "100")
    doAssert unrolled(src) == canon(src)

  block non_literal_limit:
    let src = loopSrc("0", "n.0")
    doAssert unrolled(src) == canon(src)

  block iv_used_after_loop:
    # an early exit would leave `i.0` at a value no copy materializes
    let src = loopSrc("0", "4", after = " (asgn b.0 i.0)")
    doAssert unrolled(src) == canon(src)

  block iv_assigned_in_body:
    let src = loopSrc("0", "4", body = "(asgn i.0 2)")
    doAssert unrolled(src) == canon(src)

  block conditional_increment:
    let src =
      "(stmts (var :i.0 . (i 64) 0) " &
      "(while (true) (scope (if (elif (lt i.0 4) " &
      "(stmts (if (elif c.0 (stmts (asgn i.0 (add (i 64) i.0 1)))))))" &
      "(else (stmts (jmp L.0)))))) (lab :L.0))"
    doAssert unrolled(src) == canon(src)

  block jump_over_increment:
    let src =
      "(stmts (var :i.0 . (i 64) 0) " &
      "(while (true) (scope (if (elif (lt i.0 4) " &
      "(stmts (if (elif c.0 (stmts (jmp S.0)))) " &
      "(asgn i.0 (add (i 64) i.0 1)) (lab :S.0))) " &
      "(else (stmts (jmp L.0)))))) (lab :L.0))"
    doAssert unrolled(src) == canon(src)

  block break_out_of_body_kept:
    # an early exit is a forward jump past the loop: it stays one per copy
    let got = unrolled(loopSrc("0", "2",
      body = "(if (elif c.0 (stmts (jmp L.0))))"))
    doAssert "(while" notin got, got
    doAssert got.count("(jmp L.0)") == 3, got     # two copies + the exit arm

  block address_taken_iv:
    let src = loopSrc("0", "4", after = " (var :p.0 . (ptr (i 64)) (haddr i.0))")
    doAssert unrolled(src) == canon(src)

  block nested_loops_inner_first:
    let inner =
      "(var :j.0 . (i 64) 0) " &
      "(while (true) (scope (if (elif (lt j.0 2) " &
      "(stmts (asgn (at m.0 j.0) i.0) (asgn j.0 (add (i 64) j.0 1)))) " &
      "(else (stmts (jmp K.0)))))) (lab :K.0)"
    let got = unrolled(loopSrc("0", "2", body = inner))
    doAssert "(while" notin got, got
    # m[j] = i for every (i, j): four stores with literal index and value
    let flat = got.multiReplace((" ", ""), ("\n", ""))
    for i in 0 .. 1:
      for j in 0 .. 1:
        doAssert flat.count("(asgn(atm.0" & $j & ")" & $i & ")") == 1, got

  block pre_test_statements_run_every_round:
    # a statement in front of the test runs once per iteration AND once more
    # in the round that leaves the loop; its declaration is renamed per round
    let src =
      "(stmts (var :i.0 . (i 64) 0) " &
      "(while (true) (scope (var :p.1 . (i 64) (mul (i 64) i.0 2)) " &
      "(if (elif (lt i.0 2) (stmts (asgn (at a.0 i.0) p.1) " &
      "(asgn i.0 (add (i 64) i.0 1)))) " &
      "(else (stmts (asgn b.0 p.1) (jmp L.0)))))) (lab :L.0))"
    let got = unrolled(src)
    doAssert "(while" notin got, got
    doAssert got.count("(var :") == 4, got       # i.0 + three rounds of p
    let flat = got.multiReplace((" ", ""), ("\n", ""))
    doAssert flat.count("(muli642)") == 0 and flat.count("(mul(i64)22)") == 1, got
    doAssert flat.count("(asgnb.0`ur.") == 1, got  # the exit reads ITS round's p

  echo "unroll.nim: all self-tests passed"
