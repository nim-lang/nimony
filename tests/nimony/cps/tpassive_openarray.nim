# The const-ref temporaries a coroutine's arguments borrow from have to live in
# the FRAME. A literal has no address of its own, so `constparams` gives it a
# temporary to borrow one from — and until `hoistConstRefTemps` ran ahead of the
# transform, that temporary was a local of one state proc while the `openArray`
# built from it lived in the frame and was read from a later state. The length
# came through intact and the data pointer pointed into a dead stack slot, so
# the symptom was a right-sized string of garbage.

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
