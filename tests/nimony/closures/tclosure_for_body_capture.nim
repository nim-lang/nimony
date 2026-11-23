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

# A closure capturing the loop variable itself sees the last value the loop
# variable had, not the iterator's counter after the loop (#350: 3 3 3).
var cbs: seq[proc () {.closure.}] = @[]
for i in 0 ..< 3:
  cbs.add proc () {.closure.} = echo i
for cb in cbs: cb()

iterator words(): string =
  var s = ""
  var k = 0
  while k < 2:
    s = "w" & $k
    yield s
    inc k
  s = "after"

proc captureYieldedString() =
  var cbs: seq[proc () {.closure.}] = @[]
  for w in words():
    cbs.add proc () {.closure.} = echo w
  for cb in cbs: cb()

captureYieldedString()

# A nested routine that does not mention the loop variable keeps the alias:
# no copy of the yielded local.
type Counted = object
  v: int
proc `=copy`(d: var Counted; s: Counted) =
  echo "copy"
  d.v = s.v
iterator counted(): Counted =
  var o = Counted(v: 0)
  while o.v < 3:
    yield o
    inc o.v
proc sumCounted() =
  var total = 0
  for o in counted():
    proc helper(x: int): int = x + 1
    total += helper(o.v)
  echo total
sumCounted()
