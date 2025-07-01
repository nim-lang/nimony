type
  Foo {.union.} = object
    x: int
    c: char

var x: Foo
x.c = 'a'
