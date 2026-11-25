# Default values cannot refer to other parameters.
type Cb = proc (x: int): int {.nimcall.}

proc p(a: int; w = a): int = w
proc typed(a: int; w: int = a + 1): int = w
proc inBlock(a: int; w = (block: a)): int = w
proc viaCallee(cb: Cb; x = cb(1)): int = x
proc viaLiteral(a: int; w = (proc (): int = a)()): int = w
proc gen[T](a: T; w = a): T = w
template t(a: int; w = a): int = w
type P = proc (a: int; w = a): int {.nimcall.}
