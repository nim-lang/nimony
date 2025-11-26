import std/[assertions, syncio]

proc test1(prc: proc ()) =
  prc()

test1() do ():
  echo "test"

proc test2(x: int; prc: proc (): int) =
  assert prc() == x

test2(123) do () -> int:
  123

proc testparam1(prc: proc(x: int)) =
  prc(456)

testparam1() do (x: int):
  assert x == 456

proc testparam2(x: int; prc: proc (x, y: int): int) =
  assert prc(111, 222) == x

testparam2(333) do (x, y: int) -> int:
  assert x == 111
  assert y == 222
  333

proc testgenerics1[T](x: T; prc: proc (x: T)) =
  prc(x)

testgenerics1(444) do (x: int):
  assert x == 444

proc testgenerics2[T, U](x: T; y: U; prc: proc (x: T; y: U): U): U =
  prc(x, y)

var res = testgenerics2(555, "foo") do (x: int; y: string) -> string:
  assert x == 555
  assert y == "foo"
  "bar"
assert res == "bar"

proc testLocalProc() =
  proc test(prc: proc (x: int)) =
    prc(666)

  test() do (x: int):
    assert x == 666

testLocalProc()

template testtmpl1(x: untyped): untyped =
  x

testtmpl1() do:
  echo "testtmpl1"

template testtmpl2(x, y: untyped): untyped =
  x
  y

testtmpl2() do:
  echo "testtmpl2 1"
do:
  echo "testtmpl2 2"
