import std/[syncio, assertions]
{.feature: "assumeSync".}  # test program: globals shared freely

# The hooks of an object with a `case` section handle each field exactly once,
# including the field that follows the section.

var destroyed = 0
var copied = 0

type
  Counted = object
    id: int

proc `=destroy`(x: Counted) =
  inc destroyed

proc `=wasMoved`(x: var Counted) =
  x.id = 0

proc `=copy`(dest: var Counted; src: Counted) =
  inc copied
  dest.id = src.id

type
  K = enum kA, kB
  Holder = object
    case kind: K
    of kA: discard
    of kB: n: int
    tail: Counted

proc leaveScope() =
  let h = Holder(kind: kB, n: 1, tail: Counted(id: 1))
  assert h.n == 1

destroyed = 0
leaveScope()
assert destroyed == 1

proc copyOver() =
  let a = Holder(kind: kB, n: 1, tail: Counted(id: 1))
  var b = Holder(kind: kA, tail: Counted(id: 2))
  b = a
  assert b.tail.id == 1
  assert a.tail.id == 1

destroyed = 0
copied = 0
copyOver()
# the old `b.tail` goes once, then `a.tail` and `b.tail` at the end of the scope
assert destroyed == 3
assert copied == 1

proc moveOver() =
  let a = Holder(kind: kB, n: 1, tail: Counted(id: 1))
  var b = Holder(kind: kA, tail: Counted(id: 2))
  b = a
  assert b.tail.id == 1

destroyed = 0
copied = 0
moveOver()
# `a` is moved into `b`: the old `b.tail` goes once, the moved value at the end
assert destroyed == 2
assert copied == 0
