# A block copy indexed by `p - start` is proven only when the facts bound the
# difference: 17 steps from `start` do not fit a 16-slot block. The proven side
# is `blockSum` in `tests/nimony/contracts/tprovershapes.nim`.

{.feature: "staticContracts".}

proc blockTooWide(data: openArray[int]) =
  var blk = default(array[16, int])
  if data.len >= 17:
    let start = 0
    for p in start ..< start + 17:
      blk[p - start] = data[p]

var wide = default(array[20, int])
blockTooWide(wide)
