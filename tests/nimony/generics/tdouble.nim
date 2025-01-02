proc foo[T](x: T): T = x
proc bar[T](x: T): T =
  foo(x)

proc main() =
  let x = bar(123)
