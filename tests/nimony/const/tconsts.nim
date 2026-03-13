import std/assertions

proc test =
  const myPair = (a: 10, b: 20)
  assert myPair[0] == 10
  assert myPair.b == 20

test()