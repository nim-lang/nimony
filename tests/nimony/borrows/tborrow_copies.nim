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
