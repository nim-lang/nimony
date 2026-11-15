# `{.invariant: cond.}` as the first statement of a loop body: proven on entry
# and at every back-edge (`continue` included), assumed at the head of every
# iteration, and so known after the loop. Nested loops have their own.

{.feature: "staticContracts".}

import std/syncio
type Hit = object
  thing: int

proc probe(i: int): Hit {.ensures: result.thing >= -1 and result.thing < 3.} =
  result = Hit(thing: -1)

proc nearest(n: int): Hit {.ensures: result.thing >= -1 and result.thing < 3.} =
  var best = Hit(thing: -1)
  for i in 0 ..< n:
    {.invariant: best.thing >= -1 and best.thing < 3.}
    let h = probe(i)
    if h.thing != -1: best = h
  result = best

proc whileLoop(n: int): int {.ensures: result >= 0 and result < 10.} =
  var k = 0
  var i = 0
  while i < n:
    ## comments may precede it
    {.invariant: k >= 0 and k < 10.}
    if i mod 2 == 0: k = 9
    else: k = 0
    inc i
  result = k

proc nestedInv(n: int): int {.ensures: result >= 0 and result <= 5.} =
  var a = 0
  for i in 0 ..< n:
    {.invariant: a >= 0 and a <= 5.}
    var b = 1
    for j in 0 ..< n:
      {.invariant: b >= 1 and b <= 2.}
      b = 2
    a = b + 3
  result = a

proc withContinue(n: int): int {.ensures: result >= 0 and result <= 3.} =
  var k = 0
  for i in 0 ..< n:
    {.invariant: k >= 0 and k <= 3.}
    if i == 2:
      k = 3
      continue
    k = 1
  result = k

echo nearest(3).thing, " ", whileLoop(3), " ", nestedInv(2), " ", withContinue(4)
