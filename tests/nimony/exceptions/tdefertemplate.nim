# A `defer` a template expands to protects the rest of the CALLER's scope, as in
# Nim: the expansion is a `(stmts …)`, which is not a scope. It used to fire at
# the end of the expansion instead (arkham's `bridgeStep` relies on this).

import std/syncio

var depth = 0
proc push() = inc depth
proc pop() = dec depth

template step() =
  push()
  defer: pop()

proc f(x: int): int =
  step()
  result = depth
  if x > 3: return 7

template twoSteps() =
  step()
  step()

proc g(): int =
  twoSteps()
  result = depth

proc h(cond: bool): int =
  result = 0
  if cond:
    step()
    result = depth
  result = result * 10 + depth

echo f(1), " ", f(5), " ", depth
echo g(), " ", depth
echo h(true), " ", h(false), " ", depth

# An `if`/`case` EXPRESSION after a `defer`: its branches are values, and the
# `defer` lowering used to rewrap them as statements, leaving the expression's
# temp unassigned ("cannot prove that `x has been initialized").
proc k(c: bool; i: int): int =
  defer: pop()
  let x = if c: 1 else: 2
  let y = case i
          of 0: 10
          else: 20
  result = x + y

echo k(true, 0), " ", k(false, 1)
