import std/syncio

template foo(x: varargs[string, `$`]) =
  for e in unpack():
    echo e

type A = enum a0, a1, a2
type B = enum b0, b1, b2

foo(a0, b2)
