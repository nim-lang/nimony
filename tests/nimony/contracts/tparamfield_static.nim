# issue #2944 under `staticContracts`: the shapes whose contracts the prover
# must prove, with a field named like the parameter.

{.feature: "staticContracts".}

import std/syncio

type Big = object
  a, b: int

proc sum(b: Big): int = b.a + b.b
proc idx(b: Big; i: int): int {.requires: 0 <= i and i < b.b.} = b.a + i
proc at(b: Big; s: array[4, int]): int =
  result = 0
  if 0 <= b.b and b.b < 4: result = s[b.b]

let x = Big(a: 1, b: 2)
echo sum(Big(a: 1, b: 2)), " ", sum(x)
if 0 <= x.a and x.a < x.b: echo idx(x, x.a)
echo at(Big(a: 0, b: 3), [1, 2, 3, 4])
