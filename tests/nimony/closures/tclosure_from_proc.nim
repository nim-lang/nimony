## A plain proc stored in a closure is called with an env like any closure:
## the fn slot holds the routine's thunk, which has the closure's signature.
## The importer of the closure's type used to call the bare proc without an
## env instead, a signature mismatch that wasm's `call_indirect` traps on
## (#2755).

import std/syncio
import deps/mclosureholder

proc named(n: int): int = n + 1

let a = Holder(f: proc (n: int): int = n + 2)
let b = Holder(f: named)
echo callHere(a, 10), " ", callHere(b, 10)
echo a.f(10), " ", b.f(10)

# the thunk keeps `.raises`
proc raiser(n: int): int {.raises.} =
  if n < 0: raise Failure
  result = n * 2

var r: proc (n: int): int {.closure, raises.} = raiser
try:
  echo r(4)
  echo r(-1)
except:
  echo "raised"

# a void proc, and the same routine converted twice shares its thunk
proc bump(x: var int) = inc x
proc twice(): int =
  result = 0
  var v1: proc (x: var int) {.closure.} = bump
  var v2: proc (x: var int) {.closure.} = bump
  v1(result)
  v2(result)
echo twice()

# a routine converts as an element of a tuple constructor too, also where the
# constructor is a routine's result
type Setup = proc (n: int): int {.closure.}
let u: (Setup, int) = (named, 3)
let un: tuple[a: Setup] = (a: named)
proc mkReturn(): (Setup, int) =
  return (named, 4)
proc mkResult(): (Setup, int) =
  result = (named, 5)
let r1 = mkReturn()
let r2 = mkResult()
echo u[0](u[1]), " ", un.a(1), " ", r1[0](r1[1]), " ", r2[0](r2[1])
