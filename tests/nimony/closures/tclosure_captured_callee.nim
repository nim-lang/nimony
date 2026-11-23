# a let holding a closure, captured and CALLED by another closure
import std/syncio

proc make(): proc (): int {.closure.} =
  let inner = proc (): int {.closure.} = 41
  result = proc (): int {.closure.} = inner() + 1

let f = make()
echo f()

# ... and also called directly by the routine that owns it (#343)
proc main() =
  var n = 0
  let inner = proc () {.closure.} = inc n
  inner()
  let outer = proc () {.closure.} = inner()
  outer()
  echo n

main()
