# Indices the prover must not take for granted under `staticContracts`: a block
# copy `p - start` whose 17 steps do not fit a 16-slot block, and a mask that
# can reach `len`. The proven sides are `blockSum` and `ringAt` in
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

var wide = default(array[20, int])
blockTooWide(wide)
discard maskedByLen(@[1, 2], 3)
