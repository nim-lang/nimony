import std/syncio

type
  Foo = object

proc `=destroy`(x: Foo) =
  echo "destroyed"

proc go() =
  let a = (1, 2, 3, Foo())
  let (b, c, d, _) = a
  discard b
  discard c
  discard d

go()
