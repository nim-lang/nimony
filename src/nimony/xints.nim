#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Integer based arithmetic for compilers. Was `int128` in the old
## compiler but this one here is simpler and supports `NaN` for easy
## error handling.

type
  xint* = object
    nan: bool # overflow or invalid operation
    neg: bool # value is negative
    val: uint64

proc zero*(): xint = xint(val: 0)

proc createNaN*(): xint = xint(nan: true)

proc isNaN*(x: xint): bool {.inline.} = x.nan
proc isNegative*(x: xint): bool {.inline.} = x.neg

proc asSigned*(x: xint; err: var bool): int64 =
  result = 0
  if x.nan:
    err = true
  else:
    if x.neg:
      if x.val > uint64(high(int64))+1:
        err = true
      else:
        result = -int64(x.val)
    else:
      if x.val > uint64(high(int64)):
        err = true
      else:
        result = int64(x.val)

proc asUnsigned*(x: xint; err: var bool): uint64 =
  result = 0'u64
  if x.nan:
    err = true
  else:
    if x.neg and x.val != 0'u64:
      err = true
    else:
      result = x.val

proc `-`*(a: xint): xint =
  xint(nan: a.nan, neg: not a.neg, val: a.val)

proc negate*(a: var xint) {.inline.} = a.neg = not a.neg

proc `+`*(a, b: xint): xint =
  if a.nan or b.nan:
    return xint(nan: true)

  # Both positive
  if not a.neg and not b.neg:
    result = xint(neg: false, val: a.val + b.val)
    if result.val < a.val:  # Overflow
      result.nan = true
    return result

  # Both negative
  if a.neg and b.neg:
    result = xint(neg: true, val: a.val + b.val)
    if result.val < a.val:  # Overflow
      result.nan = true
    return result

  # One negative, one positive - use subtraction
  if a.neg:
    # a is negative, b is positive
    if a.val > b.val:
      result = xint(neg: true, val: a.val - b.val)
    else:
      result = xint(neg: false, val: b.val - a.val)
  else:
    # a is positive, b is negative
    if b.val > a.val:
      result = xint(neg: true, val: b.val - a.val)
    else:
      result = xint(neg: false, val: a.val - b.val)

proc `-`*(a, b: xint): xint =
  # Subtraction is addition of the negation
  result = a + (-b)

proc `*`*(a, b: xint): xint =
  if a.nan or b.nan:
    return xint(nan: true)

  result = xint(
    nan: false,
    neg: a.neg != b.neg,  # XOR of signs
    val: a.val * b.val
  )

  # Check for overflow (requires casting to uint128 in Nim)
  if result.val div a.val != b.val:
    result.nan = true

proc `div`*(a, b: xint): xint =
  if a.nan or b.nan or b.val == 0:
    return xint(nan: true)

  result = xint(
    nan: false,
    neg: a.neg != b.neg,  # XOR of signs
    val: a.val div b.val
  )

proc `$`*(a: xint): string =
  if a.nan: "nan"
  elif a.neg: "-" & $a.val
  else: $a.val

proc createXint*(x: int64): xint =
  if x == low(int64):
    xint(neg: true, val: uint64(high(int64)) + 1'u64)
  elif x < 0:
    xint(neg: true, val: uint64(-x))
  else:
    xint(neg: false, val: uint64(x))

proc createXint*(x: uint64): xint =
  xint(neg: false, val: uint64(x))

proc `shl`*(a: xint, b: int): xint =
  if a.nan or b < 0:
    return xint(nan: true)

  result = xint(
    nan: false,
    neg: a.neg,
    val: a.val shl b
  )

  # Check for overflow
  if b >= 64 or (a.val shl b) shr b != a.val:
    result.nan = true

proc `shr`*(a: xint, b: int): xint =
  # keeps `neg` the same, so sign preserving (ashr)
  if a.nan or b < 0:
    return xint(nan: true)

  result = xint(
    nan: false,
    neg: a.neg,
    val: a.val shr b
  )

proc `and`*(a, b: xint): xint =
  xint(
    nan: a.nan or b.nan,
    neg: a.neg and b.neg,
    val: a.val and b.val
  )

proc `or`*(a, b: xint): xint =
  xint(
    nan: a.nan or b.nan,
    neg: a.neg or b.neg,
    val: a.val or b.val
  )

proc `xor`*(a, b: xint): xint =
  xint(
    nan: a.nan or b.nan,
    neg: a.neg xor b.neg,
    val: a.val xor b.val
  )

proc `not`*(a: xint): xint =
  xint(
    nan: a.nan,
    neg: a.neg,
    val: not a.val
  )

proc `mod`*(a, b: xint): xint =
  # Modulo operation
  if a.nan or b.nan or b.val == 0:
    return xint(nan: true)

  result = xint(
    nan: false,
    # Modulo sign follows dividend's sign in most programming languages
    neg: a.neg,
    val: a.val mod b.val
  )

# Mask operations
proc mask*(a: xint, bits: int, signed: bool = false): xint =
  # Create a mask with specified number of bits
  if a.nan or bits < 0 or bits > 64:
    return xint(nan: true)

  let mask = if bits == 64: high(uint64) else: ((1'u64 shl bits) - 1)
  result = xint(
    nan: false,
    neg: a.neg,
    val: a.val and mask
  )
  if signed and result.val shr (bits - 1) != 0:
    # two's complement
    result.val = (not result.val and mask) + 1
    result.neg = not result.neg

# Additional helper for bit manipulation
proc getBit*(a: xint, pos: int): xint =
  # Get bit at specific position (0-indexed from least significant bit)
  if a.nan or pos < 0 or pos >= 64:
    return xint(nan: true)

  xint(
    nan: false,
    neg: false,
    val: uint64((a.val shr pos) and 1)
  )

proc setBit*(a: xint, pos: int): xint =
  # Set bit at specific position
  if a.nan or pos < 0 or pos >= 64:
    return xint(nan: true)

  xint(
    nan: false,
    neg: a.neg,
    val: a.val or (1'u64 shl pos)
  )

proc clearBit*(a: xint, pos: int): xint =
  # Clear bit at specific position
  if a.nan or pos < 0 or pos >= 64:
    return xint(nan: true)

  xint(
    nan: false,
    neg: a.neg,
    val: a.val and not(1'u64 shl pos)
  )


proc `==`*(a, b: xint): bool =
  if a.nan:
    return b.nan
  elif b.nan:
    return false

  # Compare sign and value
  a.neg == b.neg and a.val == b.val

proc `<`*(a, b: xint): bool =
  if a.nan or b.nan:
    return false

  # Different signs
  if a.neg and not b.neg:
    return true
  if not a.neg and b.neg:
    return false

  # Same sign
  if a.neg:
    # For negative numbers, larger absolute value means smaller number
    a.val > b.val
  else:
    # For positive numbers, smaller absolute value means smaller number
    a.val < b.val

proc `<=`*(a, b: xint): bool =
  a < b or a == b

proc abs*(a: xint): xint =
  xint(
    nan: a.nan,
    neg: false,
    val: a.val
  )

proc min*(a, b: xint): xint =
  if a.nan or b.nan:
    return xint(nan: true)

  if a < b: a else: b

proc max*(a, b: xint): xint =
  if a.nan or b.nan:
    return xint(nan: true)

  if a > b: a else: b

proc succ*(a: xint): xint = a + createXint 1'u64
proc pred*(a: xint): xint = a - createXint 1'u64

proc inc*(x: var xint) = x = x + createXint 1'u64

proc toFloat64*(x: xint): float64 =
  if x.nan:
    NaN
  elif x.neg:
    -float64(x.val)
  else:
    float64(x.val)

when isMainModule:
  var a = createXint(10'i64)
  var b = createXint(-5'i64)
  echo a + b   # Outputs: "5"
  echo a - b   # Outputs: "15"
  echo a * b   # Outputs: "-50"
  echo a div b   # Outputs: "-2"
