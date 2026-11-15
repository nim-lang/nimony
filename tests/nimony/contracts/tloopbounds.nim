# A location a loop overwrites keeps a constant bound when every write in the
# body stores a value of a constant interval: a literal, a constructor of such
# values, a call's `.ensures`, or a local of the body declared with one. At the
# head it lies in the hull of that and of its bound on entry. The nearest-hit
# search of `bench/raytracer.nim` has this shape.

{.feature: "staticContracts".}

import std/syncio
type Hit = object
  thing: int
  dist: float64

proc probe(i: int): Hit {.ensures: result.thing >= -1 and result.thing < 3.} =
  result = Hit(thing: -1, dist: 0.0)

proc scalar(c: bool): int {.ensures: result >= -1 and result < 6.} =
  var k = -1
  for i in 0 ..< 3:
    if c: k = 5
  result = k

proc whileLoop(n: int): Hit {.ensures: result.thing >= -1 and result.thing < 3.} =
  var best = Hit(thing: 0, dist: 0.0)
  var i = 0
  while i < n:
    let h = probe(i)
    if h.thing != -1: best = h
    inc i
  return best

proc nestedLoops(n: int): Hit {.ensures: result.thing >= -1 and result.thing < 3.} =
  var best = Hit(thing: -1, dist: 0.0)
  for i in 0 ..< 3:
    for j in 0 ..< 3:
      let h = probe(j)
      best = h
  result = best

proc constCtorInLoop(n: int): Hit {.ensures: result.thing >= 0 and result.thing <= 2.} =
  var best = Hit(thing: 0, dist: 0.0)
  for i in 0 ..< n:
    best = Hit(thing: 2, dist: 1.0)
  result = best

echo scalar(true), " ", whileLoop(3).thing, " ", nestedLoops(2).thing, " ", constCtorInLoop(2).thing
