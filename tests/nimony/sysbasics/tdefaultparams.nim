proc foo1(x: int, y: float = 1.2) =
  let m = x

proc foo2(x = 3, y = 7) =
  let s = x
  let j = y

proc foo3(x: int, y: float, z: int = 23) =
  discard

proc foo4(x: int, y: float, z: int = 23, a: float = 1.2) =
  discard

proc foo5[T](x: T, y: int = 7) =
  discard

proc foo6[T](x: T = 3, y: int = 7) =
  discard

proc foo[T](x: T, y: T = T(7)) =
  let s = x
  let j = y

foo(3)
foo2()
foo2(1)
foo3(1, 2.3)
foo4(1, 234.34)

foo2(2, 3)

foo1(34)

foo5(1.3)
foo6(4)
foo6[int]()
foo6()
