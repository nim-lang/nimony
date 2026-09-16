# A subscript whose operand is ill-typed must report the operand's error.
# It used to pass the unchecked subscript through, which lost the diagnostic
# and crashed a later phase instead ("callee type not params").

proc f(s: string): tuple[s: string, i: int] = (s, 0)

proc main =
  var outp = ""
  outp = f(1)[0]
  outp = f(undeclaredThing)[0]
  discard outp

main()
