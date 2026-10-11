# What a coroutine frame's constructor puts in a field it cannot yet fill.
#
# A local that lives across a suspension point becomes a field of the frame,
# and the frame is built by ONE total `(oconstr …)` — every field named, the
# ones the body has not reached yet given their type's default. Two of those
# defaults used to be wrong in a way only a backend that types its stores
# could see, so this file is in `tests/nimony/closures`, which the native
# backend runs on every host:
#
# * A `var openArray[T]` field. `openArray` is a `{.view.}` object, so
#   `lengcgen` passes it BY VALUE under `var` — the field is the two-word
#   struct, not a pointer. `defaultvalues` typed it as a pointer anyway and
#   emitted `(nil <struct>)`, which is not a Leng value: `(nil T)` is a null
#   POINTER. C accepted it because `.field = 0` is C's `{0}`; arkham refused
#   it outright ("expected object, got nil").
#
# * A `pointer` field initialised from a large unsigned literal. The cast
#   staged the integer into a register already bound at the POINTER type, so
#   `(mov ptrtmp -1)` reached nifasm, which admits only `0`/`(nil)` there.
#
# Both are frame-construction bugs, so the test is not that the values are
# read — it is that the proc compiles and the values that ARE assigned later
# survive the suspension intact.

import std / syncio

proc step() {.passive.} = discard

proc scratch(buf: var array[8, char]; n: int): var openArray[char] =
  ## The `std/socket` shape: a staging area handed out as a writable view.
  toOpenArray(buf, 0, n - 1)

proc fill(dest: var openArray[char]; c: char): int =
  for i in 0..<dest.len: dest[i] = c
  result = dest.len

proc viewInFrame() {.passive.} =
  # `scratch(buf, 7)` is a TEMP of type `var openArray[char]`. Every local of a
  # passive proc lives in its frame, so the temp is a frame field and the
  # frame's constructor has to give it a default — which is the whole bug. This
  # is `httpconn.sendHead`'s `writeHead(c.sock.scratch(need), 0, m)`, reduced.
  var buf: array[8, char] = ['.', '.', '.', '.', '.', '.', '.', '!']
  let n = fill(scratch(buf, 7), 'x')
  step()
  var s = ""
  for i in 0..<buf.len: s.add buf[i]
  echo "filled=", n, " buf=[", s, "]"

viewInFrame()

const Sentinel = 0xffff_ffff_ffff_ffff'u64
  ## io_uring's cancel user-data: the shape that produced `(mov ptrtmp -1)`.

proc sentinelAcrossSuspension() {.passive.} =
  # `nil pointer`: the field is nilable, which is what lets the frame's
  # constructor default it at all — a non-nil `pointer` has no default and the
  # compiler says so.
  var p: nil pointer = nil
  step()
  p = cast[pointer](Sentinel)
  step()
  echo "sentinel=", cast[uint64](p) == Sentinel
  p = cast[pointer](0'u64)
  echo "cleared=", p == nil

sentinelAcrossSuspension()
