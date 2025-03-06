# issue #693

proc foo[T](): T = discard
proc bar[T](x: T) = discard

# Compiles without errors
proc baz1[T, U]() =
  bar[(T, U)](foo[(T, U)]())

# Compiles without errors
proc baz2[T]() =
  bar(foo[(T, T)]())

# Error: call depth limit reached
proc baz3[T]() =
  bar[(T, T)](foo[(T, T)]())

baz1[int, int]()
baz2[int]()
baz3[int]()

proc barrec[T, U](x: T, y: U) =
  if false:
    barrec[T, U](x, y)
    barrec[T, T](x, x)
    barrec[U, T](y, x)
    barrec[U, U](y, y)

barrec[int, float](1, 2)
