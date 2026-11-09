# Near misses of `tests/nimony/contracts/twrappedoperand.nim`: a wrapped
# `uint64` product or sum is some `uint64`, and no more than that.

{.feature: "staticContracts".}

proc shiftTooShort(x: uint64; a: array[256, int]): int =
  # shifted by 55, a `uint64` is `0..511`
  result = a[int((x * 3'u64) shr 55)]

proc quotientTooSmall(x: uint64; a: array[256, int]): int =
  # ... and so is its quotient by `2^55`
  result = a[int((x + 3'u64) div 36028797018963968'u64)]

proc signedShift(x: int; a: array[256, int]): int =
  # a signed product may be negative
  result = a[(x * 3) shr 56]
