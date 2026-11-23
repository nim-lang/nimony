# a let holding a closure, captured and CALLED by another closure
import std/syncio

proc make(): proc (): int {.closure.} =
  let inner = proc (): int {.closure.} = 41
  result = proc (): int {.closure.} = inner() + 1

let f = make()
echo f()

# ... and also called directly by the routine that owns it (#343): the
# direct call reads it from the env like any other use
proc main() =
  var n = 0
  let inner = proc () {.closure.} = inc n
  inner()
  let outer = proc () {.closure.} = inner()
  outer()
  echo n

proc mainVar() =
  var n = 0
  var inner = proc () {.closure.} = inc n
  inner()
  let outer = proc () {.closure.} = inner()
  outer()
  inner = proc () {.closure.} = n += 10
  inner()
  outer()
  echo n

proc escaping(): proc (): int {.closure.} =
  # the env is on the heap: the capturing closure escapes
  var n = 0
  let inner = proc (): int {.closure.} =
    inc n
    result = n
  discard inner()
  result = proc (): int {.closure.} = inner() * 100

main()
mainVar()
let e = escaping()
echo e()
echo e()
