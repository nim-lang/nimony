import std / syncio

# Issue #2366: an early `return` inside a `while` was clobbered by the code
# after the loop, which ran anyway and overwrote the result — `3` instead of
# `2`, silently. Under `nj.nim` the post-loop statements were guarded only by
# the loop-exit cfvar and not by the return flag, so leaving the loop through
# a `return` still fell into them.
#
# It also pins the fixed point in `repairCrossStateJumps`. All four elements
# below are load-bearing, and together they make ONE round of that repair
# insufficient: the `and` in the loop condition and the loop's own exit start
# out in the same state, so converting the exit to a state transition PLANTS
# the boundary that splits the condition's `jmp`/`lab` pair. A single pass
# leaves a `goto` naming a label that has moved to the next state proc — the
# C compiler says "label used but not defined".

var gMode = 2

proc pass(): int {.passive.} =
  result = 1

proc ensure(): int {.passive.} =
  if gMode == 1:
    discard pass()      # a suspension inside the `if` arm
  elif gMode == 3:
    return 1            # an early return in the `elif`
  var claimed = 1
  var polls = 0
  while claimed == 1 and polls < 3:
    discard pass()
    polls = polls + 1
    if polls == 1:
      return 2          # the early return in the loop — this one must win
    claimed = 0
  if claimed == 1:
    return 3            # must be skipped; the miscompile landed here
  result = 4

proc main() {.passive.} =
  gMode = 1
  echo ensure()
  gMode = 2
  echo ensure()
  gMode = 3
  echo ensure()

main()
