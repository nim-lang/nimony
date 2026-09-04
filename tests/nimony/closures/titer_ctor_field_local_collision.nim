import std/syncio

# Regression: an object constructor inside a `.closure` ITERATOR whose field
# NAME collides with a parameter/local lifted into the coroutine frame crashed
# lengc with
#   [Error] expected field name but got: (dot (deref `this) x)
# coro_transform's `coroTr` sent `OconstrX` to `coroTrSons`, which recursed
# into the `(kv FIELD value)` pairs and rewrote the FIELD-identity symbol into
# a `(dot (deref this) …)` frame access when it shared a SymId with a lifted
# local of the same spelling. Fixed with the same KvU field-identity guard as
# lambdalifting pass 1 / pass 2 and the DotX/DdotX selector guard.

type
  Ctx = object
    x: int
    y: int

iterator gen(x: int): int {.closure.} =
  let c = Ctx(x: x, y: x + 1)   # field name `x` collides with the frame-lifted `x`
  yield c.x
  yield c.y

proc run() =
  let it = gen
  for v in it(10):
    echo v

run()
