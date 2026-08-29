# Closures with environment capture — heap-allocated (proc-ptr, env-ptr)
# pairs whose env is a RootObj-derived type carrying RTTI. The wasm codegen
# must serialize that RTTI (vtable + method table + destroy info) as const
# data; a nil method-table slot and a bare-pointer method-table container
# were the two gaps.
import std/syncio

type Maker = proc(x: int): int {.closure.}

proc mkAdder(n: int): Maker =
  result = proc(x: int): int {.closure.} = x + n

proc apply(f: Maker; v: int): int = f(v)

let add10 = mkAdder(10)
let add3 = mkAdder(3)
echo apply(add10, 5)          # 15
echo apply(add3, 5)           # 8
echo add10(100)               # 110

# mutable capture across calls
proc counter(): proc(): int {.closure.} =
  var c = 0
  result = proc(): int {.closure.} = (c = c + 1; c)

let next = counter()
echo next()                   # 1
echo next()                   # 2

# a closure capturing an aggregate (seq)
proc collector(): proc(v: int): int {.closure.} =
  var xs: seq[int] = @[]
  result = proc(v: int): int {.closure.} =
    xs.add v
    var s = 0
    for x in xs: s = s + x
    s

let col = collector()
echo col(10)                  # 10
echo col(5)                   # 15
echo col(100)                 # 115

# closures in a seq, dispatched by index
var fns: seq[Maker] = @[]
var k = 1
while k <= 3:
  fns.add mkAdder(k * 10)
  k = k + 1
echo fns[0](1), " ", fns[1](1), " ", fns[2](1)   # 11 21 31
