# A closure in an inline iterator captures the iterator's locals and has a
# `for` of its own: the loop's temps must not take the names the inlined copy
# gave those locals (fuzzers#113, fuzzers#34).
import std/syncio

iterator lenInFor(): int {.sideEffect.} =
  var s = @[4, 5, 6]
  let c = proc () {.closure.} =
    for x in 0..<1: echo s.len
  c(); yield 1
iterator lenBeforeFor(): int {.sideEffect.} =
  var s = @[1, 2]
  let c = proc () {.closure.} =
    echo s.len
    for x in 0..<1: discard
  c(); yield 1
iterator intInFor(): int {.sideEffect.} =
  var w = 2
  let c = proc () {.closure.} =
    for i in 0..<1: echo "w = ", w
  c(); yield 1

for x in lenInFor(): discard
for x in lenBeforeFor(): discard
for x in intInFor(): discard
