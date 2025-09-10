import std / syncio

proc p1[T](x, y: T): T = 8

var x = p1[int](23, 89)

proc useGlobal =
  inc x
  echo x

useGlobal()
