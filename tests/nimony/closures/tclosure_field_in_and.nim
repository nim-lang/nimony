# A closure held in a FIELD, called in the RHS of an `and`, ICEs hexer:
#
#   [Bug] could not find symbol: `llTemp.N
#     hexer/xelim.nim(241) trAnd -> trExprInto -> getType
#     nimony/typenav.nim(275) tupatType -> decls.nim(46) bug
#
# `llTemp.N` is minted in hexer/lambdalifting.nim for an "expression callee"
# (a closure in a field/var, not a direct proc symbol) to bind the (fn, env)
# tuple to a temp. trAnd lowers `and` into short-circuit control flow, moving
# the RHS operand into a branch — and the temp's declaration is then not
# findable from its use.
#
# MINIMISED. Only two elements are required:
#   1. the callee is a closure held in a FIELD (a closure PARAMETER in the
#      same position compiles fine)
#   2. the call is in the RHS operand of an `and` in an `if` condition
#
# NOT required — each still ICEs without it: a `while` loop, a `!= nil`
# guard, `{.feature: "lenientnils".}`, and `ref` (a value object fails too).
# Rewriting the `and` as nested ifs compiles (the workaround).

import std / [syncio, assertions]

type
  H = proc(): bool {.closure.}
  B = object
    h: H

proc callsFieldInAnd(b: B, guard: int): int =
  result = 0
  if guard >= 0 and b.h():
    inc result

proc main =
  let t = 1
  let b = B(h: proc(): bool {.closure.} = t >= 0)
  assert callsFieldInAnd(b, 0) == 1
  assert callsFieldInAnd(b, -1) == 0
  echo "ok"

main()
