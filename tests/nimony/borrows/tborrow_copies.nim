# A by-value argument that owns no heap memory is a copy: mutating the
# location it was read from cannot change it under the callee's feet, so the
# call-site alias check leaves it alone. The by-pointer and heap-sharing
# forms of these calls are rejected in tborrow_errors.nim.

import std/assertions

type
  Obj = object
    x: int
    y: int
  Lexer = object
    pos: int
    errors: seq[string]
  R = object
    id: int

proc take(a: var int, b: int) =
  a = b

proc takeFieldAndObj(a: var int, b: Obj) =
  a = b.x + b.y

proc resize(s: var seq[int], val: int) =
  s.add val

proc error(L: var Lexer; pos: int; msg: string) =
  L.errors.add msg

proc forwarded(L: var Lexer) =
  # `L` is forwarded as is (#2766); `L.pos` is an `int` copy
  error L, L.pos, "x"

proc `=destroy`(x: R) = discard
proc `=wasMoved`(x: var R) = x.id = 0
proc takeLent(x: var R): lent R = x
proc keep(x: sink R): R = x
proc eat2(a: sink R; b: sink R): int = a.id + b.id
proc lentId(x: var R): lent int = x.id
proc fi(a: int; b: sink R): int = a + b.id

proc copiedOut =
  # The `lent` result is copied into the seq: `x` may be sunk afterwards.
  var x = R(id: 1)
  let s = @[takeLent(x)]
  assert keep(x).id + s[0].id == 2
  # A `sink` or plain-data parameter gets a copy of the borrow, not the borrow.
  var y = R(id: 2)
  assert eat2(takeLent(y), y) == 4
  var z = R(id: 3)
  assert fi(lentId(z), z) == 6

proc run =
  var a = 5
  take(a, a)
  var o = Obj(x: 1, y: 2)
  takeFieldAndObj(o.x, o)
  var s = @[1, 2, 3]
  resize(s, s[0])
  var L = Lexer(pos: 3)
  forwarded L
  assert a == 5 and o.x == 3 and s.len == 4 and L.errors.len == 1

run()
copiedOut()
