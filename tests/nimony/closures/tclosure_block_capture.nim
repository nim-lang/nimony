## A closure capturing a local of an enclosing `block`. The Final IR turns the
## block into a `(scope …)`, and typenav's capture count used to stop at the
## first scope that declared the local, reporting no capture at all.
import std/syncio

proc main =
  block:
    var a = 5
    proc inner(x: int): int {.closure.} =
      result = a + x
    echo inner(3)
    a = 10
    echo inner(3)

main()
