## Test forward declarations

# Forward declaration
proc foo(): int

proc bar(): int =
  result = foo()  # Call forward declared proc

# Implementation
proc foo(): int =
  result = 42

# Each kind of routine, forward declared and implemented later.
proc gen[T](x: T): T
type Base = ref object of RootObj
method meth(x: Base): int {.base.}
iterator count(): int
func setOut(x: out int)
proc stop() {.noreturn.}
proc clamp(x: int): int {.ensures: 0 <= result.}
proc exported*(): int
when true:
  proc inWhen(): int
when false:
  proc neverImplemented(): int
proc fromInclude(): int
include deps / forward_decl_include
proc fromIncluder(): int = 20

proc gen[T](x: T): T = x
method meth(x: Base): int {.base.} = 3
iterator count(): int =
  yield 4
func setOut(x: out int) = x = 5
proc stop() {.noreturn.} =
  while true: discard
proc clamp(x: int): int {.ensures: 0 <= result.} = (if x < 0: 0 else: x)
proc exported*(): int = 6
proc inWhen(): int = 7

# Routines without a body that are not forward declarations.
proc cAbs(x: cint): cint {.importc: "abs", header: "<stdlib.h>".}
proc cLabs(x: clong): clong {.header: "<stdlib.h>", importc: "labs".}
type Meters = distinct int
proc `+`(a, b: Meters): Meters {.borrow.}
proc forbidden() {.error: "do not call".}
template marker() {.pragma.}

# Test
import std/assertions
assert foo() == 42
assert bar() == 42
assert gen(2) == 2
assert Base().meth == 3
for i in count(): assert i == 4
var v: int
setOut(v)
assert v == 5
assert clamp(-3) == 0
assert exported() == 6
assert inWhen() == 7
assert fromInclude() == 21
assert fromIncluder() == 20
assert cAbs(-8) == 8
assert cLabs(-9) == 9
assert int(Meters(1) + Meters(2)) == 3
if v == 0: stop()
