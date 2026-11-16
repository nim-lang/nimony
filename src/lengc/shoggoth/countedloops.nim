#
#
#           NIFC Counted-Loop Recognition (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Recognizes counted loops: a counter that only the loop's own step changes,
## compared against a bound on every back-edge. Every shoggoth pass that wants
## "the loop runs `i` from `a` to `b`" asks here rather than growing its own
## matcher.
##
## The shape is the one hexer's `for` lowering produces once loop rotation
## (#2995) has moved the test to the back-edge; `..<`, `..` and `countup` all
## arrive as
##
##   (var :IV . T START)                     ; or (asgn IV START)
##   (ite TEST'                               ; TEST, or TEST with IV := START
##     (stmts (loop (stmts
##       BODY*
##       (asgn IV (add T IV STEP))            ; the last statement, seen through
##                                            ; `(stmts …)`/`(scope …)`
##       (continue TEST))))
##     .)
##
## with TEST one of
##
##   (lt IV STOP)          CountUpExclusive
##   (le IV STOP)          CountUp
##   (neq IV STOP)         CountUpUntil      ; also `(not (eq IV STOP))`
##   (le STOP IV)          CountDown         ; with `(sub T IV STEP)`
##
## `detectLoop` matches the bare `(loop …)`: the body runs at least once, and
## `start` is unknown. `detectGuardedLoop` matches the `ite` around it, which
## makes zero iterations possible, and reads `start` off the statement before
## it.
##
## **What a match promises.** Inside the loop the counter is written only by
## the step, and its address is not taken there. What the match cannot see is
## the rest of the proc: a pointer taken to the counter before the loop still
## reaches it, so a caller that relies on the counter being private checks that
## the address never escapes. Nor does a match say `STOP` or `STEP` are
## loop-invariant, or that the step cannot wrap past `STOP`: those are the
## caller's questions too.

import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/typeKind

type
  LoopKind* = enum
    NoLoop             ## not a counted loop
    CountUp            ## `IV <= STOP`, counting up by `STEP`
    CountUpExclusive   ## `IV < STOP`, counting up by `STEP`
    CountUpUntil       ## `IV != STOP`, counting up by `STEP`
    CountDown          ## `STOP <= IV`, counting down by `STEP`

  DetectedLoop* = object
    ## Cursors into the loop matched; valid as long as its buffer is unchanged.
    kind*: LoopKind
    counter*: Cursor   ## the counter in `test`: a `Symbol`
    start*: Cursor     ## its value on entry; nil when not known
    stop*: Cursor      ## the bound `test` compares against
    step*: Cursor      ## what each iteration adds (or, counting down, subtracts)
    typ*: Cursor       ## the counter's type, as the step's `add`/`sub` spells it
    cond*: Cursor      ## the back-edge condition: `(continue COND)`
    test*: Cursor      ## the comparison in `cond`: the `eq` of `(not (eq …))`,
                       ## otherwise `cond` itself
    update*: Cursor    ## the step: `(asgn IV (add T IV STEP))`
    loop*: Cursor      ## the `(loop …)`
    guard*: Cursor     ## the `(ite …)` around it; nil from `detectLoop`

proc counterSym*(d: DetectedLoop): SymId {.inline.} = symId(d.counter)

proc isOneLit(n: Cursor): bool =
  ## `1`, possibly suffixed, parenthesized or converted.
  case n.kind
  of IntLit: intVal(n) == 1
  of UIntLit: uintVal(n) == 1'u64
  of TagLit:
    if n.exprKind in {SufC, ParC}: isOneLit(n.childCursor)
    elif n.exprKind == ConvC:
      var c = n.childCursor
      skip c                                 # the type
      isOneLit(c)
    else: false
  else: false

proc isUnitStep*(d: DetectedLoop): bool {.inline.} =
  ## The counter moves by one per iteration.
  isOneLit(d.step)

proc isSym(n: Cursor; s: SymId): bool {.inline.} =
  n.kind == Symbol and symId(n) == s

proc mentions(n: Cursor; s: SymId): bool =
  if n.kind == TagLit:
    var it = n.childCursor
    while it.hasMore:
      if mentions(it, s): return true
      skip it
    result = false
  else:
    result = isSym(n, s)

proc binaryOperands(n: Cursor; a, b: var Cursor): bool =
  ## `(op A B)`, exactly two operands and no type child.
  var c = n.childCursor
  result = false
  if c.hasMore and not (c.kind == TagLit and c.typeKind != NoType):
    a = c
    skip c
    if c.hasMore:
      b = c
      skip c
      result = not c.hasMore

proc parseTest(d: var DetectedLoop) =
  ## Classify `d.cond`; sets `kind`, `test`, `counter` and `stop`.
  d.test = d.cond
  let negated = d.cond.kind == TagLit and d.cond.exprKind == NotC
  if negated:
    let inner = d.cond.childCursor
    if inner.kind == TagLit and inner.exprKind == EqC: d.test = inner
    else: return
  if d.test.kind != TagLit: return
  var a, b = default(Cursor)
  if not binaryOperands(d.test, a, b): return
  case d.test.exprKind
  of LtC, LeC:
    if a.kind == Symbol and not mentions(b, symId(a)):
      d.kind = if d.test.exprKind == LtC: CountUpExclusive else: CountUp
      d.counter = a
      d.stop = b
    elif d.test.exprKind == LeC and b.kind == Symbol and not mentions(a, symId(b)):
      d.kind = CountDown
      d.counter = b
      d.stop = a
  of EqC, NeqC:
    if (d.test.exprKind == EqC) == negated and
       a.kind == Symbol and not mentions(b, symId(a)):
      d.kind = CountUpUntil
      d.counter = a
      d.stop = b
  else: discard

proc lastStmt(n: Cursor): Cursor =
  ## The statement `n` ends with, seen through `(stmts …)`/`(scope …)`; nil
  ## when that ends in an empty one.
  result = n
  while result.kind == TagLit and result.stmtKind in {StmtsS, ScopeS}:
    var it = result.childCursor
    if not it.hasMore: return default(Cursor)
    while it.hasMore:
      result = it
      skip it

proc parseUpdate(d: var DetectedLoop; n: Cursor): bool =
  ## `(asgn IV (add T IV STEP))`, `sub` when counting down.
  result = false
  if not cursorIsNil(n) and n.kind == TagLit and n.stmtKind == AsgnS:
    var c = n.childCursor
    if isSym(c, d.counterSym):
      skip c
      let want = if d.kind == CountDown: SubC else: AddC
      if c.kind == TagLit and c.exprKind == want:
        let op = c
        skip c
        if not c.hasMore:
          var e = op.childCursor
          let typ = e
          skip e
          if isSym(e, d.counterSym):
            skip e
            if e.hasMore and not mentions(e, d.counterSym):
              let step = e
              skip e
              if not e.hasMore:
                d.typ = typ
                d.step = step
                d.update = n
                result = true

proc countWrites(n: Cursor; s: SymId): int =
  ## Assignments to `s` itself, and addresses taken of it, anywhere under `n`.
  result = 0
  if n.kind == TagLit:
    if n.stmtKind == AsgnS and isSym(n.childCursor, s): inc result
    elif n.exprKind in AddrKinds and isSym(n.childCursor, s): inc result
    var it = n.childCursor
    while it.hasMore:
      result += countWrites(it, s)
      skip it

proc detectLoop*(loop: Cursor): DetectedLoop =
  ## The bottom-tested `(loop (stmts … (asgn IV …) (continue TEST)))`.
  result = DetectedLoop(kind: NoLoop)
  if loop.kind != TagLit or loop.stmtKind != LoopS: return
  let body = loop.childCursor
  if body.kind != TagLit or body.stmtKind != StmtsS: return
  var it = body.childCursor
  var beforeLast = default(Cursor)
  var last = default(Cursor)
  while it.hasMore:
    beforeLast = last
    last = it
    skip it
  if cursorIsNil(beforeLast) or last.kind != TagLit or last.stmtKind != ContinueS:
    return
  var d = DetectedLoop(kind: NoLoop, cond: last.childCursor, loop: loop)
  parseTest(d)
  if d.kind != NoLoop and parseUpdate(d, lastStmt(beforeLast)) and
     countWrites(body, d.counterSym) == 1:
    result = d

proc startValue(prev: Cursor; s: SymId): Cursor =
  ## What `(var :IV PRAGMAS T START)` or `(asgn IV START)` sets the counter to.
  result = default(Cursor)
  if not cursorIsNil(prev) and prev.kind == TagLit:
    var c = prev.childCursor
    if prev.stmtKind == VarS and c.kind == SymbolDef and symId(c) == s:
      skip c                                 # name
      skip c                                 # pragmas
      skip c                                 # type
      if c.hasMore and c.kind != DotToken: result = c
    elif prev.stmtKind == AsgnS and isSym(c, s):
      skip c
      result = c

proc sameTest(g, cond: Cursor; s: SymId; start: Cursor): bool =
  ## `g` is `cond`, or `cond` with the counter replaced by `start` (copy
  ## propagation substitutes the entry value into the guard).
  if isSym(cond, s) and not cursorIsNil(start) and sameTest(g, start, SymId(0), start):
    result = true
  elif g.kind != cond.kind:
    result = false
  elif g.kind == TagLit:
    result = cursorTagId(g) == cursorTagId(cond)
    var a = g.childCursor
    var b = cond.childCursor
    while result and a.hasMore and b.hasMore:
      result = sameTest(a, b, s, start)
      skip a
      skip b
    result = result and not a.hasMore and not b.hasMore
  else:
    result = g.load == cond.load

proc detectGuardedLoop*(guard: Cursor; prev = default(Cursor)): DetectedLoop =
  ## `(ite TEST' (stmts (loop …)) .)`, the rotated loop with its entry test.
  ## `prev` is the statement before the `ite` when there is one: where `start`
  ## comes from.
  result = DetectedLoop(kind: NoLoop)
  if guard.kind != TagLit or guard.stmtKind != IteS: return
  var c = guard.childCursor
  let g = c
  skip c
  var arm = c
  skip c
  if c.hasMore and c.kind != DotToken: return    # an else arm
  while arm.kind == TagLit and arm.stmtKind in {StmtsS, ScopeS}:
    var it = arm.childCursor
    if not it.hasMore: return
    arm = it
    skip it
    if it.hasMore: return                        # more than the loop
  var d = detectLoop(arm)
  if d.kind == NoLoop: return
  d.start = startValue(prev, d.counterSym)
  if sameTest(g, d.cond, d.counterSym, d.start):
    d.guard = guard
    result = d

proc addBodyStmts(n: Cursor; dest: var seq[Cursor]) =
  if n.kind == TagLit and n.stmtKind in {StmtsS, ScopeS}:
    var it = n.childCursor
    while it.hasMore:
      addBodyStmts(it, dest)
      skip it
  else:
    dest.add n

proc bodyStmts*(d: DetectedLoop): seq[Cursor] =
  ## The loop's statements other than the step and the back-edge, seen through
  ## `(stmts …)`/`(scope …)`, in order.
  result = @[]
  var it = d.loop.childCursor.childCursor
  while it.hasMore:
    var next = it
    skip next
    if next.hasMore: addBodyStmts(it, result)  # all but the `continue`
    it = next
  discard result.pop()                       # the step: `lastStmt` found it last

# ---- self-tests ----------------------------------------------------------

when isMainModule:
  import std / [assertions, strutils]

  proc parse(src: string): TokenBuf =
    parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())

  proc guarded(buf: var TokenBuf): DetectedLoop =
    ## `(stmts PREV (ite …))`
    let prev = beginRead(buf).childCursor
    var g = prev
    skip g
    detectGuardedLoop(g, prev)

  proc render(n: Cursor): string =
    var b = createTokenBuf(8, n.pool, n.tags)
    b.addSubtree n
    toString(b)

  const Loop = "(loop (stmts (stmts (scope (scope (call use.0.M i.0.M)) " &
    "(asgn i.0.M (add (i 64) i.0.M 1)))) (continue $1)))"

  proc rotated(test, guard: string; start = "(var :i.0.M . (i 64) 0)"): string =
    "(stmts " & start & " (ite " & guard & " (stmts " & Loop.replace("$1", test) &
      ") .))"

  block count_up_exclusive:
    var buf = parse(rotated("(lt i.0.M n.0.M)", "(lt i.0.M n.0.M)"))
    let d = guarded(buf)
    doAssert d.kind == CountUpExclusive
    doAssert render(d.counter) == "i.0.M"
    doAssert render(d.start) == "0"
    doAssert render(d.stop) == "n.0.M"
    doAssert render(d.step) == "1" and d.isUnitStep
    doAssert render(d.typ) == "(i 64)"
    doAssert d.update.stmtKind == AsgnS
    let body = bodyStmts(d)
    doAssert body.len == 1 and render(body[0]) == "(call use.0.M i.0.M)"

  block count_up_inclusive:
    var buf = parse(rotated("(le i.0.M n.0.M)", "(le i.0.M n.0.M)"))
    doAssert guarded(buf).kind == CountUp

  block count_up_until:
    var buf = parse(rotated("(not (eq i.0.M n.0.M))", "(not (eq i.0.M n.0.M))"))
    let d = guarded(buf)
    doAssert d.kind == CountUpUntil and d.test.exprKind == EqC
    var buf2 = parse(rotated("(neq i.0.M n.0.M)", "(neq i.0.M n.0.M)"))
    doAssert guarded(buf2).kind == CountUpUntil

  block count_down:
    var buf = parse(("(stmts (var :i.0.M . (i 64) n.0.M) (ite (le 0 i.0.M) (stmts " &
      "(loop (stmts (call use.0.M i.0.M) (asgn i.0.M (sub (i 64) i.0.M 2)) " &
      "(continue (le 0 i.0.M))))) .))"))
    let d = guarded(buf)
    doAssert d.kind == CountDown
    doAssert render(d.stop) == "0" and render(d.step) == "2" and not d.isUnitStep

  block guard_with_start_substituted:
    # copy propagation has put the entry value into the guard
    var buf = parse(rotated("(lt i.0.M n.0.M)", "(lt 0 n.0.M)"))
    doAssert guarded(buf).kind == CountUpExclusive

  block guard_differs:
    var buf = parse(rotated("(lt i.0.M n.0.M)", "(lt i.0.M m.0.M)"))
    doAssert guarded(buf).kind == NoLoop

  block start_unknown:
    var buf = parse(rotated("(lt i.0.M n.0.M)", "(lt i.0.M n.0.M)", "(call f.0.M)"))
    let d = guarded(buf)
    doAssert d.kind == CountUpExclusive and cursorIsNil(d.start)

  block bare_loop:
    var buf = parse(Loop.replace("$1", "(lt i.0.M n.0.M)"))
    let d = detectLoop(beginRead(buf))
    doAssert d.kind == CountUpExclusive and cursorIsNil(d.guard)

  block counter_written_in_body:
    var buf = parse("(loop (stmts (asgn i.0.M 7) (asgn i.0.M (add (i 64) i.0.M 1)) " &
      "(continue (lt i.0.M n.0.M))))")
    doAssert detectLoop(beginRead(buf)).kind == NoLoop

  block counter_address_taken_in_body:
    var buf = parse("(loop (stmts (call f.0.M (addr i.0.M)) " &
      "(asgn i.0.M (add (i 64) i.0.M 1)) (continue (lt i.0.M n.0.M))))")
    doAssert detectLoop(beginRead(buf)).kind == NoLoop

  block step_not_last:
    var buf = parse("(loop (stmts (asgn i.0.M (add (i 64) i.0.M 1)) (call use.0.M i.0.M) " &
      "(continue (lt i.0.M n.0.M))))")
    doAssert detectLoop(beginRead(buf)).kind == NoLoop

  block wrong_direction:
    # counting up against a `STOP <= IV` test is no countdown
    var buf = parse("(loop (stmts (asgn i.0.M (add (i 64) i.0.M 1)) " &
      "(continue (le 0 i.0.M))))")
    doAssert detectLoop(beginRead(buf)).kind == NoLoop

  block bound_mentions_counter:
    var buf = parse("(loop (stmts (asgn i.0.M (add (i 64) i.0.M 1)) " &
      "(continue (lt i.0.M (add (i 64) i.0.M 1)))))")
    doAssert detectLoop(beginRead(buf)).kind == NoLoop

  echo "countedloops.nim: all self-tests passed"
