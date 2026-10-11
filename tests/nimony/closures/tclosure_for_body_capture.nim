## A closure declared in a `for` body captures a local of that body. The
## iterator inliner copies the body once per `yield` with fresh names for its
## locals; the closure's uses of them must be renamed with it (they were not:
## hexer reported "[Bug] could not find symbol: v.0").

import std/syncio

proc sumViaClosures(n: int): int =
  result = 0
  for d in 0 ..< n:
    var v = d * 10
    proc get(): int {.closure.} = v
    result += get()

iterator twice(): int =
  yield 1
  yield 2

proc perYieldCopies(): int =
  # Two yields: the body, closure included, is copied twice; each copy's
  # closure must see its own copy of `v`.
  result = 0
  for d in twice():
    var v = d * 100
    proc get(): int {.closure.} = v
    result += get()

echo sumViaClosures(4)
echo perYieldCopies()
