# `.ensures` about a field of an object result: the prover keeps `x.f` as a
# location of its own when `x` is constructed (`T(f: v)`) or copied (`x = y`),
# and a bound on it survives a loop that overwrites `x` when every back-edge
# re-establishes it (a candidate invariant, `analyseLoopBody`). The shape is
# `bench/raytracer.nim`'s nearest-hit search.

{.feature: "staticContracts".}

import std/syncio

type
  Hit = object
    thing: int
    dist: float64

const things = [10, 20, 30]

proc probe(thing: int; d: float64): Hit {.requires: thing >= 0 and thing < 3,
    ensures: result.thing >= -1 and result.thing < 3.} =
  if d < 0.0:
    return Hit(thing: -1, dist: 0.0)
  return Hit(thing: thing, dist: d + float64(things[thing]))

proc viaLocal(d: float64): Hit {.ensures: result.thing >= -1 and result.thing < 3.} =
  let h = probe(2, d)
  return h

proc viaJoin(d: float64): Hit {.ensures: result.thing >= -1 and result.thing < 3.} =
  var best = Hit(thing: -1, dist: 0.0)
  let h = probe(1, d)
  if d < 5.0:
    best = h
  result = best

proc nearest(d: float64): Hit {.ensures: result.thing >= -1 and result.thing < 3.} =
  var closest = 1e9
  var best = Hit(thing: -1, dist: 0.0)
  for i in 0 ..< 3:
    let h = probe(i, d)
    if h.thing != -1 and h.dist < closest:
      best = h
      closest = h.dist
  return best

proc use(h: Hit): int {.requires: h.thing >= 0 and h.thing < 3.} =
  things[h.thing]

let h = nearest(1.0)
if h.thing != -1:
  echo use(h)
echo viaLocal(1.0).thing, " ", viaJoin(1.0).thing
