# The fields of a constructed (`T(f: v)`) or copied (`x = y`) object value are
# locations of their own, so an `.ensures` about `result.f` can be proven: by a
# `result = T(...)`, a `return T(...)`, a local that is returned, a join of two
# such stores, and nested constructors. A copy keeps its fields when its source
# is written afterwards.

{.feature: "staticContracts".}

import std/syncio

type
  Hit = object
    thing: int
    dist: float64
  Outer = object
    inner: Hit
    n: int

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

proc copySurvives(x: int): Hit {.ensures: result.thing >= 0 and result.thing < 3.} =
  var a = Hit(thing: 1, dist: 0.0)
  var b = a
  a.thing = x
  result = b

proc nested(x: int): Outer {.requires: x >= 0 and x < 3,
    ensures: result.inner.thing >= 0 and result.inner.thing < 3.} =
  let h = Hit(thing: x, dist: 0.0)
  return Outer(inner: h, n: 1)

proc use(h: Hit): int {.requires: h.thing >= 0 and h.thing < 3.} =
  things[h.thing]

let h = viaLocal(1.0)
if h.thing != -1:
  echo use(h)
echo viaJoin(1.0).thing, " ", probe(0, -1.0).thing, " ", copySurvives(7).thing,
  " ", nested(2).inner.thing
