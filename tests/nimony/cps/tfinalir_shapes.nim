import std / syncio

# The Final IR shapes the CPS state machine has to cut, one per construct.
# `nj.nim` never handed any of these to `coro_transform`: it lowered `case` to
# an `ite` chain, `try` to a guard cfvar, and `break`/`continue` to `jtrue`, so
# the state machine only ever saw `loop`/`ite`/`store`. Reading `finalir`
# instead means meeting `case`, `try`/`except`/`fin` and `lab`/`jmp` head on.
#
#  - a suspension point inside ONE branch of a `case`
#  - `try` inside a suspending loop, with `break` and `continue` crossing a
#    state boundary (the loop exit is a Final IR `(jmp)` to a `(lab)` in a
#    LATER state proc, which `repairCrossStateJumps` has to convert)
#  - a nested `try` whose inner handler raises again: that `raise` belongs to
#    the OUTER try, not the inner one it lexically sits in

proc raiser(x: int) {.raises.} =
  if x < 0:
    raise SyntaxError

proc work(msg: string) {.passive.} =
  suspend()
  echo msg

proc pick(x: int) {.passive.} =
  case x
  of 0: work("case 0")
  of 1: echo "case 1"
  else: work("case else")

proc loopy() {.passive.} =
  var i = 0
  while i < 4:
    inc i
    if i == 2:
      continue
    try:
      raiser(if i == 3: -1 else: 1)
      work("loop ok " & $i)
    except:
      echo "loop caught " & $i
    if i == 4:
      break
  echo "loopy done " & $i

proc nested() {.passive.} =
  try:
    try:
      raiser(-1)
    except:
      echo "inner"
      raiser(-1)
  except:
    work("outer")

proc main() {.passive.} =
  pick(0)
  pick(1)
  pick(2)
  loopy()
  nested()

main()
