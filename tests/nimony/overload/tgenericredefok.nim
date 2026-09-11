type
  A = concept
    proc foo(x: Self)

  B = concept
    proc bar(x: Self)

proc p[T: A](x: T) = discard
proc p[U: B](x: U) = discard
