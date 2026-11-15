import std/syncio

# A checked operation as a piece of an `emit` inside `{.keepOverflowFlag.}`:
# it is lowered to a `keepovf` in front of the `emit`, which then names its
# temporary. C back end only: arkham has no `emit`.

proc e(a, b, c: int): int =
  var r = 0
  {.keepOverflowFlag.}:
    {.emit: [r, " = ", a + b * c, ";"].}
  result = r

echo e(1, 2, 3)
