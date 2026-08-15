# a let holding a closure, captured and CALLED by another closure
import std/syncio

proc make(): proc (): int {.closure.} =
  let inner = proc (): int {.closure.} = 41
  result = proc (): int {.closure.} = inner() + 1

let f = make()
echo f()
