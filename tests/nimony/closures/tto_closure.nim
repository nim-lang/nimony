# issue #1948
# tests convertion from non closure procs to closure proc type params
# related to `ToClosureX` tag

import std/assertions

var testNonClosureVar = 0
proc nonClosure() = testNonClosureVar = 123
proc testNonClosure(c: proc () {.closure.}) =
  c()

testNonClosure(nonClosure)
assert testNonClosureVar == 123

proc nonClosureInt(x: int): int = x * 3

proc testNonClosureInt(clsr: proc (x: int): int {.closure.}) =
  assert clsr(123) == 369

testNonClosureInt(nonClosureInt)
