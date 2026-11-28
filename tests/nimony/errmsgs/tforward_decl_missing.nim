# A forward declaration needs an implementation, and they are matched by
# tokens: spelling a type through an alias does not implement it.

type MyInt = int

proc noBody(x: int): int

proc aliased(x: MyInt): int
proc aliased(x: int): int = x + 1

proc outer(): int =
  proc inner(x: int): int
  result = 0

proc ext(x: cint): cint {.importc: "abs", header: "<stdlib.h>".}
proc nope(x: int) {.error: "nope".}
