# issue #691

import std/assertions

var a: seq[array[1, int]] = @[[1]]
var b: seq[array[1, array[1, int]]] = @[[[2]]]
var c: array[1, seq[int]] = [@[3]]
var d: array[1, seq[array[1, int]]] = [@[[4]]]

assert a[0][0] == 1
assert b[0][0][0] == 2
assert c[0][0] == 3
assert d[0][0][0] == 4

type
  Foo = object
    x: int

  Bar = object
    a: array[1, Foo]
    b: seq[Foo]
    c: array[1, seq[Foo]]
    d: seq[array[1, Foo]]

var e: array[1, Foo] = [Foo(x: 5)]
assert e[0].x == 5
var f: seq[Foo] = @[Foo(x: 6)]
assert f[0].x == 6

var g = Bar(a: [Foo(x: 7)], b: @[Foo(x: 8)], c: [@[Foo(x: 9)]], d: @[[Foo(x: 10)]])
assert g.a[0].x == 7
assert g.b[0].x == 8
assert g.c[0][0].x == 9
assert g.d[0][0].x == 10
