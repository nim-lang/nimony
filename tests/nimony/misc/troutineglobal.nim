# A `var x {.global.}` in a routine is one location for the whole program:
# sem moves it to module scope. Inlining `counter` used to give every inlined
# call its own copy (#3167); with `result = calls` the inliner also folded the
# global into a temporary of the caller.

when defined(nimony):
  {.feature: "assumeSync".}
import std/syncio

proc counter(): int =
  var calls {.global.}: int
  inc calls
  result = calls * 1

proc counterDirect(): int =
  var calls {.global.}: int
  inc calls
  result = calls

# one global per instance
proc gen[T](x: T): int =
  var n {.global.}: int
  inc n
  result = n

# the initializer runs once
var inits = 0
proc startVal(): int =
  inc inits
  result = 100

proc withInit(): int =
  var v {.global.} = startVal()
  inc v
  result = v

proc main() =
  echo counter(), " ", counter(), " ", counter()
  echo counterDirect(), " ", counterDirect(), " ", counterDirect()
  echo gen(1), " ", gen(2), " ", gen("a"), " ", gen("b")
  echo withInit(), " ", withInit(), " inits=", inits

main()
