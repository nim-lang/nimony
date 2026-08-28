# `(ret r)` sunk into the arms of the `if` that produced `r`.
#
# Repro:   bin/nimony c -r -d:danger tests/nimony/opt/topt_sink_returns.nim
# Expected (see .output): 4 1 1000000 42 5
#
#   (var :r T .)                          (if (elif c (stmts … (ret E1)))
#   (if (elif c (stmts … (asgn r E1)))        (else (stmts … (ret E2))))
#       (else (stmts … (asgn r E2))))
#   (ret r)
#
# `r` disappears with the rewrite — one local and one copy per site — and a call
# that was an arm's own tail becomes ADJACENT to a `ret`, which is what
# the fold needs — `trSink` then `trFold`, the two rounds of `runTailCalls` in
# `shoggoth/tailcalls.nim`. In nimsem that is 339 sites and 111 tail calls, against the 10
# the adjacent form finds on its own.
#
# `count` is the reason to care beyond code size: sunk, its recursive arm is a
# tail call, so it runs in constant stack. Unsunk it is two million frames and
# dies of a stack overflow — which is what this test does if the rewrite stops
# firing, loudly rather than subtly.
#
# `noSink` is the shape that must NOT be rewritten: its `if` has no `else`, so
# a path leaves it without assigning and still needs the `ret` that follows.

import std / syncio

proc callee(a, b: int): int {.noinline.} = a * b + 1

proc tailCond(a, b: int): int {.noinline.} =
  if a > b: callee(a, b) else: a

proc count(n, acc: int): int {.noinline.} =
  if n == 0: acc else: count(n - 1, acc + 1)

proc noSink(a: int): int {.noinline.} =
  ## no `else`: the fallthrough path reaches the final return
  var r = 42
  if a > 100:
    r = callee(a, 2)
  r

proc nested(a: int): int {.noinline.} =
  ## an arm whose own tail is another `if` — not modelled yet, must stay correct
  if a > 0:
    if a > 10: 5 else: 6
  else: 7

proc main =
  echo tailCond(3, 1), " ", tailCond(1, 7), " ", count(1_000_000, 0), " ",
       noSink(1), " ", nested(50)

main()
