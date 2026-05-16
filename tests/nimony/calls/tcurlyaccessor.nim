
import std / [syncio]

type
  Box[T] = object
    val: T
    fallback: T

proc `{}`*[T](b: Box[T]; i: int): T =
  if i == 0: b.val else: b.fallback

proc `{}=`*[T](b: var Box[T]; i: int; v: T) =
  if i == 0: b.val = v else: b.fallback = v

var b = Box[int](val: 10, fallback: 99)
echo b{0}
echo b{1}
b{0} = 42
b{1} = 7
echo b{0}
echo b{1}
