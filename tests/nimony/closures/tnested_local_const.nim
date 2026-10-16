# A routine-level const that a nested routine reads moves out with the routine
# when lambdalifting lifts it (#2641).
import std/syncio

proc first(values: openArray[int]): int = values[0]

proc test(): int =
  const table = [42]
  const k = 3
  proc run(): int = first(table) + k
  run() + first(table)

proc deep(): int =
  const t = [1, 2]
  proc mid(): int =
    proc inner(): int = first(t) + t[1]
    inner()
  mid()

proc withCapture(x: int): int =
  const t = [10]
  proc c(): int {.closure.} = first(t) + x
  c()

proc sameName(): int =
  const table = [7]
  proc run(): int = first(table)
  run()

block:
  const bt = [5]
  proc r(): int = first(bt)
  echo r()

echo test(), " ", deep(), " ", withCapture(1), " ", sameName()
