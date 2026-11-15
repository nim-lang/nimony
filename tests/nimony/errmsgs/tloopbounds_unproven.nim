# What the loop-head bounds must not claim: a write of an unbounded value, a
# field written after the copy, a copied local written before the copy, a write
# outside the stated interval, and an unbounded value on entry.

type Hit = object
  thing: int
  dist: float64

proc probe(i: int): Hit {.ensures: result.thing >= -1 and result.thing < 3.} =
  result = Hit(thing: -1, dist: 0.0)

proc unboundedWrite(x: int): Hit {.ensures: result.thing >= -1 and result.thing < 3.} =
  var best = Hit(thing: -1, dist: 0.0)
  for i in 0 ..< 3:
    if i == 1: best = Hit(thing: x, dist: 0.0)
  result = best

proc fieldAfterCopy(x: int): Hit {.ensures: result.thing >= -1 and result.thing < 3.} =
  var best = Hit(thing: -1, dist: 0.0)
  for i in 0 ..< 3:
    best = probe(i)
    best.thing = x
  result = best

proc localMutated(x: int): Hit {.ensures: result.thing >= -1 and result.thing < 3.} =
  var best = Hit(thing: -1, dist: 0.0)
  for i in 0 ..< 3:
    var h = probe(i)
    h.thing = x
    best = h
  result = best

proc scalarTooTight(c: bool): int {.ensures: result >= -1 and result < 6.} =
  var k = -1
  for i in 0 ..< 3:
    if c: k = 6
  result = k

proc entryUnbounded(x: int): Hit {.ensures: result.thing >= -1 and result.thing < 3.} =
  var best = Hit(thing: x, dist: 0.0)
  for i in 0 ..< 3:
    best = probe(i)
  result = best

discard unboundedWrite(1)
discard fieldAfterCopy(1)
discard localMutated(1)
discard scalarTooTight(true)
discard entryUnbounded(1)
