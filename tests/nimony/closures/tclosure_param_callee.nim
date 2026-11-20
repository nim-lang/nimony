# closure call through a PARAM, inside another closure
import std/syncio

proc apply(f: proc (x: int): int {.closure.}): proc (x: int): int {.closure.} =
  result = proc (x: int): int {.closure.} = f(x) * 2

let g = apply(proc (x: int): int {.closure.} = x + 1)
echo g(20)

# a `sink` proc-type param called directly: the callee's type is `(sink proctype ...)`
proc sinkNimcall(t: sink proc (x: int): int {.nimcall.}): int = t(3)
proc sinkClosure(t: sink proc (x: int): int {.closure.}): int = t(4)
proc sinkOperands(t: sink proc (x: int): int {.nimcall.}): int = t(5) + t(6)
proc inc1(x: int): int = x + 1
echo sinkNimcall(inc1)
echo sinkClosure(proc (x: int): int {.closure.} = x * 10)
echo sinkOperands(inc1)
proc sinkCaptured(t: sink proc (x: int): int {.closure.}): proc (): int {.closure.} =
  result = proc (): int {.closure.} = t(7)
let sc = sinkCaptured(proc (x: int): int = x * 2)
echo sc()
