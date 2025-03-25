import std/syncio

type Foo = enum a, b, c, d, e, f

for i in low(Foo) .. high(Foo):
  echo i
