# Regression: the `proc` typeclass as a constraint on a VALUE parameter.
#
# `[T: proc]` was only ever exercised through `typedesc` (see
# ttypeclassmatches), and on a value parameter it crashed hexer outright —
# "into requires cursor at TagLit" — before the proc was ever instantiated.
# The bare typeclass is `(proctype)` with NO children, so walking it to the
# pragmas slot ran off the end of the node; `procHasPragma` now recognises the
# childless form (it can carry no pragmas) instead of walking it.
import std/syncio

proc bothProcs*[T: proc](a, b: T): bool = true
proc oneProc*[T: proc](a: T): int = 7
proc oneIter*[T: iterator](a: T): int = 9

proc plain(x: int) = discard

type
  PlainH = proc(x: int)
  ClosureH = proc(x: int) {.closure.}

proc mkClosure(): ClosureH =
  result = proc(x: int) {.closure.} = discard

let p: PlainH = plain
let c = mkClosure()

echo bothProcs(p, p)      # a plain proc type
echo bothProcs(c, c)      # a .closure proc type — the shape that crashed
echo oneProc(c)

# `[T: iterator]` crashed the same way, one layer along: trProctype classified
# the childless `(itertype)` as a closure iterator and walked to its params tag.
iterator counter(): int {.closure.} =
  yield 1
let it: iterator (): int {.closure.} = counter
echo oneIter(it)
