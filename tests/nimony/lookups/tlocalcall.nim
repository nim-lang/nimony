proc foo(x: int) = discard
proc foo(y: float) = discard
proc bar(x: uint8) = discard

proc main =
  when false: # when proc types work
    let foo = bar
    foo(123) # does not consider overloads, always matches local

main()
