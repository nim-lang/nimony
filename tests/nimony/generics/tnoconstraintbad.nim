import std/syncio

type Addable = concept
  proc `+`(a, b: Self): Self

proc `+=?`[T: Addable](a: var T, b: T) =
  a = a + b

proc mean[T](vs: seq[T]): float =
  for x in vs:
    result +=? x
  result / vs.len.float

echo @[1.0, 2.0].mean
