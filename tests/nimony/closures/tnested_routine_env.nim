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
    proc sib(): int {.closure.} = inner() * 2
    result = sib
  let p = mid(1)
  let q = mid(2)
  a = 100
  echo p(), " ", q()
twoLevels()

proc stackEnvs(): int =
  var total = 0
  proc step(k: int) {.closure.} =
    var local = k
    proc bump() {.closure.} =
      total += local
      inc local
    bump()
    bump()
  step(1)
  step(10)
  result = total
echo stackEnvs()

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

proc ownAndOuter() =
  var a = 0
  proc visit(x: int) {.closure.} =
    inc a
    let f = proc (): int {.closure.} = x
    a += f()
  visit(5)
  visit(7)
  echo a
ownAndOuter()

proc iterInstances() =
  # each instance of a `.closure` iterator has its own environment
  var fs: seq[proc (): int {.closure.}] = @[]
  iterator gen(x: int): int {.closure.} =
    var mine = x
    fs.add(proc (): int {.closure.} = mine)
    yield 0
  let a = gen
  for _ in a(1): discard
  let b = gen
  for _ in b(2): discard
  echo fs[0](), " ", fs[1]()
iterInstances()

proc iterInterleaved() =
  # the instances run interleaved, each closure keeps its own instance's local
  var fs: seq[proc (): int {.closure.}] = @[]
  iterator gen(x: int): int {.closure.} =
    var mine = x
    fs.add(proc (): int {.closure.} = mine)
    yield mine
    mine = mine * 10
    yield mine
  let a = gen
  let b = gen
  for v in a(1):
    echo "a ", v
    for w in b(2):
      echo "b ", w, " / ", fs[0](), " ", fs[fs.len-1]()
  echo fs.len
  for f in fs: echo f()
iterInterleaved()

proc iterParamInstances() =
  var fs: seq[proc (): int {.closure.}] = @[]
  iterator gen(x: int): int {.closure.} =
    fs.add(proc (): int {.closure.} = x)
    yield 0
  let a = gen
  let b = gen
  for _ in a(1): discard
  for _ in b(2): discard
  echo fs[0](), " ", fs[1]()
iterParamInstances()
