# A conversion that can change its operand's value (`uint32(a)` of a negative
# `a`) does not make the result that operand: no fact about `a` is a fact
# about `k`, through any shape that reads `k`.

{.feature: "staticContracts".}

proc upperOnly(a: int32; arr: array[10, int]): int =
  result = 0
  let k = uint32(a)
  if a < 10'i32:
    result = arr[k]

proc bothEnds(a: int32; arr: array[10, int]): int =
  result = 0
  let k = uint32(a)
  if a >= -5'i32 and a < 10'i32:
    result = arr[k]

proc platformWord(a: int; arr: array[10, int]): int =
  result = 0
  let k = uint(a)
  if a >= -5 and a < 10:
    result = arr[k]

proc convertedInside(a: int32; arr: array[10, int]): int =
  result = 0
  if a >= -5'i32 and a < 10'i32:
    let k = uint32(a)
    result = arr[k]

proc backToInt(a: int32; arr: array[10, int]): int =
  result = 0
  let k = uint32(a)
  if a >= -5'i32 and a < 10'i32:
    result = arr[int(k)]

proc backToIntUpperOnly(a: int32; arr: array[10, int]): int =
  result = 0
  let k = uint32(a)
  if a < 10'i32:
    result = arr[int(k)]

proc quotient(a: int32; arr: array[10, int]): int =
  result = 0
  let k = uint32(a)
  if a >= -5'i32 and a < 10'i32:
    result = arr[k div 1'u32]

proc masked(a: int32; arr: array[10, int]): int =
  result = 0
  let k = uint32(a)
  if a >= -5'i32 and a < 10'i32:
    result = arr[(k and 0xFFFF'u32)]

proc shifted(a: int32; arr: array[10, int]): int =
  result = 0
  let k = uint32(a)
  if a >= -5'i32 and a < 10'i32:
    result = arr[k shr 0]

proc sum(a: int32; arr: array[10, int]): int =
  result = 0
  let k = uint32(a)
  if a >= -5'i32 and a < 10'i32:
    result = arr[int(k) + 0]

type
  Signed = enum sNeg = -1, sZero = 0, sOne = 1
  Wide = enum wa = 0, wb = 200

proc negativeOrdinal(e: Signed; arr: array[10, int]): int =
  # `uint32(sNeg)` is `high(uint32)`
  result = 0
  let k = uint32(e)
  if e <= sZero:
    result = arr[k]

proc negativeOrdinalInside(e: Signed; arr: array[10, int]): int =
  result = 0
  if e <= sZero:
    let k = uint32(e)
    result = arr[k]

proc ordinalPastTarget(e: Wide; arr: array[10, int]): int =
  # `int8(wb)` is `-56`
  result = 0
  let k = int8(e)
  if e >= wb and e <= wb:
    result = arr[int(k) - 195]

proc ordinalPastTargetInline(e: Wide; arr: array[10, int]): int =
  result = 0
  if e >= wb and e <= wb:
    result = arr[int(int8(e)) - 195]
