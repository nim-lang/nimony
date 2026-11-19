# Indices the prover must not take for granted under `staticContracts`: a block
# copy `p - start` whose 17 steps do not fit a 16-slot block, a mask that can
# reach `len`, a length read through a `ref` that an alias may have changed,
# and an index after `x = 0'i32` or `x = cast[int8](addr y)`: a number is no
# non-nil address. The proven sides are `blockSum` and `ringAt` in
# `tests/nimony/contracts/tprovershapes.nim`.

{.feature: "staticContracts".}

proc blockTooWide(data: openArray[int]) =
  var blk = default(array[16, int])
  if data.len >= 17:
    let start = 0
    for p in start ..< start + 17:
      blk[p - start] = data[p]

proc maskedByLen(data: seq[int]; at: int): int =
  # masking with `len` rather than `len - 1` can reach `len` itself
  result = 0
  if data.len > 0:
    result = data[at and data.len]

type
  Buf = ref object
    data: seq[int]

proc clearAll(x: Buf) =
  x.data.setLen 0

proc throughAlias(a, b: Buf): int =
  # a field reached through a `ref` is no location: `b` may be `a`
  result = 0
  if a.data.len > 0:
    clearAll(b)
    result = a.data[0]

proc throughPointer(a: Buf; p: ptr seq[int]): int =
  # ... and `p` may point at `a.data`
  result = 0
  if a.data.len > 0:
    p[].setLen 0
    result = a.data[0]

proc afterSuffixedZero(at: int): int =
  var x = 1'i32
  x = 0'i32
  var arr = default(array[8, int])
  result = 0
  if at >= 0 and at <= 8:
    result = arr[at] + int(x)

proc afterCastAddr(k: int): int =
  var buf = default(array[4, uint8])
  var arr = default(array[8, int])
  var x = 1'i8
  result = 0
  if k >= 0 and k < 4:
    x = cast[int8](addr buf[k])
    if x <= 8:
      result = arr[x - 1'i8]

var wide = default(array[20, int])
blockTooWide(wide)
discard maskedByLen(@[1, 2], 3)
var shared = Buf(data: @[1])
discard throughAlias(shared, shared)
discard throughPointer(shared, addr shared.data)
discard afterSuffixedZero(8)
discard afterCastAddr(0)
