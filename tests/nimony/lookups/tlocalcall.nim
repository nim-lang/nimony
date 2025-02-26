proc foo(x: int) = discard
proc foo(y: float) = discard
proc bar(x: uint8) = discard

proc main =
  let foo: proc (x: uint8) = bar
  foo(123) # does not consider overloads, always matches local

main()
