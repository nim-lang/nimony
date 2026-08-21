# A `while` whose condition has a short-circuit spine goes through the
# two-target condition compiler (`Cx`, doc/final_ir.md) instead of
# materialising a bool. `xelim.trWhile` emits
#
#   while true: Cjmp(cond)(bodyLab, true); (break); (lab bodyLab); <body>
#
# The exit is the FALL-THROUGH and the body sits behind the jump, because the
# only way out of a loop is `(break)` and a label placed after the body would
# also catch the fall-off-the-body edge.
#
# What this pins:
#  - the condition is re-evaluated every iteration, not hoisted out of the loop
#    (the bug `twhile_cond_temp.nim` guards, now on the jump path);
#  - short-circuiting still short-circuits, per iteration;
#  - a value with a destructor built inside a conditionally-evaluated operand is
#    destroyed once per evaluation — its `(scope …)` is left both by falling out
#    of it and by the guard's `(jmp bodyLab)`.

import std / [syncio, assertions]

type Box = object
  s: string

var evals = 0
var destroys = 0

proc `=destroy`(b: Box) =
  if b.s.len > 0: inc destroys

proc `=dup`(b: Box): Box = Box(s: b.s)
proc `=copy`(dest: var Box; src: Box) = dest.s = src.s

proc make(tag: string): Box =
  inc evals
  result = Box(s: tag)

proc main =
  # `i < 3` is the always-evaluated leftmost leaf; the second operand needs an
  # owning temp, so it is emitted behind the guard in a scope of its own.
  var i = 0
  while i < 3 and make("abc").s.len == 3:
    inc i
  echo "iterations: ", i
  # evaluated on i = 0, 1, 2; on i = 3 the left operand already failed
  echo "evals: ", evals
  echo "destroys: ", destroys
  assert i == 3
  assert evals == 3
  assert evals == destroys

  # `or`: a true left operand must skip the right one entirely.
  evals = 0
  destroys = 0
  var j = 0
  while j < 2 or make("z").s.len == 0:
    inc j
  echo "or iterations: ", j
  echo "or evals: ", evals
  assert j == 2
  assert evals == 1        # only the run where `j < 2` was false
  assert evals == destroys

main()
