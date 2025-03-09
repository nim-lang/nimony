proc foo[T](x, y: T) = discard
proc bar[T](t: (T, T)) = foo(t[0], t[0])
