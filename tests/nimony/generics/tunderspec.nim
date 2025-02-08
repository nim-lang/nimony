type HasDefault* = concept
  proc default(_: typedesc[Self]): Self

proc default2[T: HasDefault](): T = default(T)
proc foo(x: int) = discard
foo(default2())
