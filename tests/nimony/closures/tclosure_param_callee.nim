# closure call through a PARAM, inside another closure
import std/syncio

proc apply(f: proc (x: int): int {.closure.}): proc (x: int): int {.closure.} =
  result = proc (x: int): int {.closure.} = f(x) * 2

let g = apply(proc (x: int): int {.closure.} = x + 1)
echo g(20)
