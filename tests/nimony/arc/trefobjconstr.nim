
import std / [syncio]

type
  GObj[T] = ref object
    data: T

  AObj = ref object
    data: string

proc use[T](x: T) = discard

proc main =
  var obj = GObj[string](data: "abc")

  var a = AObj(data: "abc")

  var objB = GObj[int](data: 456)
  use obj
  use a
  use objB

main()
