# issue #2669: the guard-free body of a `.requires` routine renames its
# parameters; a field with a parameter's name must keep its own.

import std/syncio
type
  MyObjectKind* = enum
    kindA,
    kindB,

  MyObject* = object
    case kind*: MyObjectKind
    of kindA:
      discard
    of kindB:
      foo*: int
      bar*: int

proc myObject*(f, bar: int): MyObject {.requires: f != 0.} =
  MyObject(kind: kindB, foo: f, bar: bar)

proc getBar(foo: MyObject; bar: int): int {.requires: bar > 0.} =
  type Local = object
    bar: int
  let l = Local(bar: bar)
  result = foo.bar + l.bar + (bar: bar).bar

let x = myObject(1, 2)
echo x.foo, " ", x.bar, " ", getBar(x, 3)
