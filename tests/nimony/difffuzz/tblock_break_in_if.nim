# planetis-m/nimony-bugs-fuzzers#116: on the native back end an `ite` arm that starts
# with a jump (here `break b`) was emitted as that jump alone, dropping its label and the rest.
import std/syncio

proc f(d: int): int {.noinline.} =
  result = 1
  if d < 0:
    block b:
      break b
    result = d + 5

echo f(-1)
