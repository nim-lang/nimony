# `newSeq[T]()` makes no element, so it needs no default value for `T`: a seq
# of a not-nil ref starts empty and is filled by `add`. The sized form keeps
# its `HasDefault` guard, and a plain `newSeq[int]()` still resolves.
import std/syncio

type Foo = ref object
  x: int

var a = newSeq[Foo]()
a.add Foo(x: 3)
var b = newSeq[int]()
b.add 7
var c = newSeq[int](2)
echo a.len, " ", a[0].x, " ", b.len, " ", b[0], " ", c.len, " ", c[1]
