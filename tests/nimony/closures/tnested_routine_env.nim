# Each call of a nested routine gets its own environment for the locals that
# its closures capture (#3160).
import std/syncio

proc outer() =
  proc make(x: int): proc (): int {.closure.} =
    result = proc (): int {.closure.} = x
  let a = make(1)
  let b = make(2)
  echo a(), " ", b()
outer()

proc counters() =
  proc counter(start: int): proc (): int {.closure.} =
    var n = start
    result = proc (): int {.closure.} =
      inc n
      result = n
  let a = counter(0)
  let b = counter(10)
  discard a()
  echo a(), " ", b()
counters()

proc sequential() =
  var fns: seq[proc (): int {.closure.}] = @[]
  proc visit(depth: int): proc (): int {.closure.} =
    let mine = depth * 10
    result = proc (): int {.closure.} = mine
  for i in 0..2: fns.add visit(i)
  for f in fns: echo f()
sequential()
