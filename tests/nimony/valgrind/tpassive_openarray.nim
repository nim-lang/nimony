# Data a coroutine's frame points at has to live in the frame. A state proc is
# not the coroutine's activation record: it runs, it returns, and its stack is
# gone, while the frame survives until the coroutine is resumed. A literal has
# no address of its own, so something has to give it storage to be addressed
# through — and if that storage is a state proc's local while the `openArray`
# built from it lives in the frame and is read from a later state, the length
# comes through intact and the data pointer points into a dead stack slot. The
# symptom is a right-sized string of garbage.
#
# It lives HERE, in the valgrind category, and not next to the other cps tests,
# because reading a dead stack slot is only *sometimes* wrong output: whether
# the bytes are still the ones you wrote is a matter of what has run since.
# Built against the unfixed compiler this file prints the expected lines and
# passes — memcheck is what turns it into a test, reporting the read of
# uninitialised memory whatever the stale bytes happened to hold. So the
# `.output` below says the behaviour is right and the category says the memory
# is, and only the two together pin this bug down.

import std / [syncio]

func show(data: openArray[char]): string =
  result = ""
  for i in 0..<data.len: result.add data[i]

proc take(data: openArray[char]) {.passive.} =
  echo "[", show(data), "] len=", data.len

proc chain() {.passive.} =
  # Three in a row: the first used to survive by accident on some targets, so
  # one call is not enough to pin this down.
  take("aaa")
  take("bbb")
  take("ccc")

chain()

proc step() {.passive.} = discard

proc borrowAcrossSuspension() {.passive.} =
  # No passive call carries the view here: the temporary is created in one
  # state and read in the next, which is the same bug without an argument.
  let oa: openArray[char] = "hello"
  step()
  echo "[", show(oa), "]"

borrowAcrossSuspension()

iterator gen(): string {.closure.} =
  # Closure iterators are cut into states by the same transform.
  let oa: openArray[char] = "world"
  yield "first"
  yield show(oa)

for v in gen(): echo "[", v, "]"

proc borrowFromLocal() {.passive.} =
  # The other half of the rule: an lvalue already has storage, so the pass
  # leaves it alone and the ordinary escape analysis has to be what keeps it
  # alive. `toOpenArray` is `.inline`, so the `addr buf` it takes is in this
  # body before the cut and pins `buf` to the frame like any other `addr`.
  var buf: array[5, char] = ['b', 'o', 'r', 'r', 'w']
  let oa: openArray[char] = buf
  step()
  echo "[", show(oa), "]"

borrowFromLocal()

proc addrOfLocal() {.passive.} =
  # The general rule the fix rests on: an address taken of a local must pin
  # that local to the frame, whatever states the pointer is read in.
  var x = 42
  let p = addr x
  step()
  p[] = 99
  step()
  echo "x=", x
addrOfLocal()
