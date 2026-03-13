# Test: basic borrow checking - borrowing from simple paths should work.

import std/assertions

type
  Obj = object
    x: int
    y: int

proc modify(a: var int) =
  a = 10

proc readVal(a: int): int = a

# Borrowing from a simple variable is fine:
proc testSimple =
  var a = 5
  modify(a)
  let r = readVal(a)
  assert r == 10

testSimple()

# Borrowing from a field path is fine:
proc testField =
  var o = Obj(x: 1, y: 2)
  modify(o.x)
  assert o.x == 10

testField()

# Passing different fields is fine (no overlap):
proc swap(a: var int, b: var int) =
  let tmp = a
  a = b
  b = tmp

proc testDisjointFields =
  var o = Obj(x: 1, y: 2)
  swap(o.x, o.y)
  assert o.x == 2
  assert o.y == 1

testDisjointFields()
