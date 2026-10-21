# issue #2737: seq's hooks need the element's `=dup`, so a seq of a type
# whose `=copy` is `.error` cannot be copied; moving it is fine
type Foo = object
  x: int
proc `=copy`(dst: var Foo; src: Foo) {.error.}
proc `=destroy`(x: Foo) = discard
type Wrap = object
  s: seq[Foo]
type
  Inner = object
    f: Foo
  SInner = object
    s: seq[Inner]
proc main =
  var t: seq[Foo] = @[]
  t.add Foo(x: 1)
  var moved = move t
  moved.add Foo(x: 2)
  var u = moved
  var w = Wrap(s: moved)
  var w2 = w
  discard u.len + w.s.len + w2.s.len
  var a: seq[Inner] = @[]
  var b = a
  var c = SInner(s: a)
  var d = c
  discard a.len + b.len + c.s.len + d.s.len
main()
