# A `.closure` proctype in a `.closure` iterator's signature: as the yielded
# type and as a parameter type.
import std/syncio

type Thunk = proc (): int {.closure, noSideEffect.}

func thunk(k: int): Thunk =
  result = proc (): int {.closure, noSideEffect.} = k * 10

iterator thunks(n: int): Thunk {.closure.} =
  var i = 0
  while i < n:
    yield thunk(i)
    inc i

iterator callTwice(f: Thunk): int {.closure.} =
  yield f()
  yield f() + 1

proc run() =
  for f in thunks(3): echo f()
  let it = thunks
  for f in it(2): echo f()
  for x in callTwice(thunk(7)): echo x

run()
