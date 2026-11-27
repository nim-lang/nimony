# A closure captures the locals of a single routine only: a nested routine
# whose own locals are captured cannot also use an enclosing routine's (#3160).
import std/syncio

proc recursive(base: int) =
  var fns: seq[proc (): int {.closure.}] = @[]
  proc visit(depth: int) {.closure.} =
    let mine = depth * 10 + base
    fns.add(proc (): int {.closure.} = mine)
    if depth < 2: visit(depth + 1)
  visit(0)
  for f in fns: echo f()
recursive(1)

proc twoLevels() =
  var a = 5
  proc mid(y: int): proc (): int {.closure.} =
    var b = y
    proc inner(): int {.closure.} = a + b
    result = inner
  echo mid(1)()
twoLevels()

proc forwarded() =
  var a = 5
  proc helper(): int {.closure.} = a
  proc make(x: int): proc (): int {.closure.} =
    discard helper()
    result = proc (): int {.closure.} = x
  echo make(1)()
forwarded()
