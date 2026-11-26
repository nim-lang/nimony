## A forward declaration is implemented by a routine whose signature is the
## same after sem, however it is spelled.

import std/assertions

proc renamedTypevar[T](x: T): T
proc renamedTypevar[U](x: U): U = x

proc constrained[T: SomeInteger](x: T): T
proc constrained[T: SomeInteger](x: T): T = x

proc viaAlias(x: cint): cint
proc viaAlias(x: int32): int32 = x * 2

proc voidSpelled(x: var int): void
proc voidSpelled(x: var int) = inc x

proc groupedParams(x: int; y: int): int
proc groupedParams(x, y: int): int = x + y

proc defaulted(x: int = 1): int
proc defaulted(x = 1): int = x

proc viaCallback(cb: proc (x: int): int): int
proc viaCallback(cb: proc (y: int): int): int = cb(1)
proc succ1(z: int): int = z + 1

proc unusedCallback(cb: proc (x: int): int): int
proc unusedCallback(cb: proc (y: int): int): int = cb(1)

proc unusedVoid(): void
proc unusedVoid() = discard
proc unusedGeneric[T](x: T): T
proc unusedGeneric[U](x: U): U = x

assert renamedTypevar(2) == 2
assert constrained(2) == 2
assert viaAlias(4) == 8
var called = 0
voidSpelled(called)
assert called == 1
assert groupedParams(1, 2) == 3
assert defaulted() == 1
assert viaCallback(succ1) == 2
