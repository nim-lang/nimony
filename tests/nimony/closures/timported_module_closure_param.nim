# issue #2096
import deps/mcallclosure

proc main() =
  var x = 3
  proc cl(y: int): int {.closure.} = x * y
  callClosure(cl)

main()
