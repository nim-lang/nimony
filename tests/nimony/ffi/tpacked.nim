import std/assertions

type
  Foo {.packed.} = object
    c: char
    x: int

var x = Foo(c: 'a', x: 123)
assert x.c == 'a' and x.x == 123
