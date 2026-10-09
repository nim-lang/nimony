## Native (libc-free) implementations of floating-point rounding and remainder.
## The bit-level `copySign`, `floor`, `ceil`, `round`, and `trunc` routines follow
## Rust compiler-builtins/libm's generic routines; floor/ceil/trunc derive from
## musl. Floating-point `mod` adapts the significand reduction in generic/fmod.rs.
## Sources: `library/compiler-builtins/libm/src/math/generic/{copysign,floor,ceil,round,trunc,fmod}.rs`;
## musl `src/math/{floorf,ceilf,trunc}.c`.
## Copyright (c) 2018 Jorge Aparicio (Rust compiler-builtins/libm).
## Copyright © 2005-2020 Rich Felker, et al. (musl-derived portions).
## See `/tmp/rust/library/compiler-builtins/libm/LICENSE.txt` for the
## applicable MIT and MIT OR Apache-2.0 license notices.

import std/math/common

# Constants for bit manipulation
const FLOAT32_SIGN_MASK = 0x80000000'u32
const FLOAT64_SIGN_MASK = 0x8000000000000000'u64
const FLOAT32_EXP_MASK = 0x7f800000'u32
const FLOAT64_EXP_MASK = 0x7ff0000000000000'u64
const FLOAT32_SIG_MASK = 0x007fffff'u32
const FLOAT64_SIG_MASK = 0x000fffffffffffff'u64
const FLOAT32_SIG_BITS = 23
const FLOAT64_SIG_BITS = 52
const FLOAT32_EXP_BIAS = 127
const FLOAT64_EXP_BIAS = 1023
const FLOAT32_IMPLICIT_BIT = 0x00800000'u32
const FLOAT64_IMPLICIT_BIT = 0x0010000000000000'u64

func getExpUnbiased(x: float32): int {.inline.} =
  ## Get unbiased exponent of f32
  let ix = cast[uint32](x)
  int((ix and FLOAT32_EXP_MASK) shr FLOAT32_SIG_BITS) - FLOAT32_EXP_BIAS

func getExpUnbiased(x: float64): int {.inline.} =
  ## Get unbiased exponent of f64
  let ix = cast[uint64](x)
  int((ix and FLOAT64_EXP_MASK) shr FLOAT64_SIG_BITS) - FLOAT64_EXP_BIAS

func isSignNegative(x: float32): bool {.inline.} =
  (cast[uint32](x) and FLOAT32_SIGN_MASK) != 0

func isSignNegative(x: float64): bool {.inline.} =
  (cast[uint64](x) and FLOAT64_SIGN_MASK) != 0

func isSignPositive(x: float32): bool {.inline.} =
  not isSignNegative(x)

func isSignPositive(x: float64): bool {.inline.} =
  not isSignNegative(x)

## Sign of Y, magnitude of X
##
## Constructs a number with the magnitude (absolute value) of its
## first argument, `x`, and the sign of its second argument, `y`.
func copySign*(x, y: float32): float32 =
  var ux = cast[uint32](x)
  let uy = cast[uint32](y)
  ux = ux and (not FLOAT32_SIGN_MASK)
  ux = ux or (uy and FLOAT32_SIGN_MASK)
  cast[float32](ux)

func copySign*(x, y: float64): float64 =
  var ux = cast[uint64](x)
  let uy = cast[uint64](y)
  ux = ux and (not FLOAT64_SIGN_MASK)
  ux = ux or (uy and FLOAT64_SIGN_MASK)
  cast[float64](ux)

## Floor
##
## Finds the nearest integer less than or equal to `x`.
func floor*(x: float32): float32 =
  var ix = cast[uint32](x)
  let e = getExpUnbiased(x)

  # If the represented value has no fractional part, no truncation is needed.
  if e >= FLOAT32_SIG_BITS:
    return x

  if e >= 0:
    # |x| >= 1.0
    let m = FLOAT32_SIG_MASK shr uint32(e)
    if (ix and m) == 0'u32:
      # Portion to be masked is already zero; no adjustment needed.
      return x

    if isSignNegative(x):
      ix = ix + m

    ix = ix and (not m)
    return cast[float32](ix)
  else:
    # |x| < 1.0, zero or inexact with truncation
    if (ix and (not FLOAT32_SIGN_MASK)) == 0'u32:
      return x

    if isSignPositive(x):
      # 0.0 <= x < 1.0; rounding down goes toward +0.0.
      return 0'f32
    else:
      # -1.0 < x < 0.0; rounding down goes toward -1.0.
      return -1'f32

func floor*(x: float64): float64 =
  var ix = cast[uint64](x)
  let e = getExpUnbiased(x)

  # If the represented value has no fractional part, no truncation is needed.
  if e >= FLOAT64_SIG_BITS:
    return x

  if e >= 0:
    # |x| >= 1.0
    let m = FLOAT64_SIG_MASK shr uint32(e)
    if (ix and m) == 0'u64:
      # Portion to be masked is already zero; no adjustment needed.
      return x

    if isSignNegative(x):
      ix = ix + m

    ix = ix and (not m)
    return cast[float64](ix)
  else:
    # |x| < 1.0, zero or inexact with truncation
    if (ix and (not FLOAT64_SIGN_MASK)) == 0'u64:
      return x

    if isSignPositive(x):
      # 0.0 <= x < 1.0; rounding down goes toward +0.0.
      return 0.0
    else:
      # -1.0 < x < 0.0; rounding down goes toward -1.0.
      return -1.0

## Ceil
##
## Finds the nearest integer greater than or equal to `x`.
func ceil*(x: float32): float32 =
  var ix = cast[uint32](x)
  let e = getExpUnbiased(x)

  # If the represented value has no fractional part, no truncation is needed.
  if e >= FLOAT32_SIG_BITS:
    return x

  if e >= 0:
    # |x| >= 1.0
    let m = FLOAT32_SIG_MASK shr uint32(e)
    if (ix and m) == 0'u32:
      # Portion to be masked is already zero; no adjustment needed.
      return x

    if isSignPositive(x):
      ix = ix + m

    ix = ix and (not m)
    return cast[float32](ix)
  else:
    # |x| < 1.0, raise an inexact exception since truncation will happen (unless x == 0).
    if (ix and (not FLOAT32_SIGN_MASK)) == 0'u32:
      return x

    if isSignNegative(x):
      # -1.0 < x <= -0.0; rounding up goes toward -0.0.
      return -0'f32
    elif (ix shl 1) != 0'u32:
      # 0.0 < x < 1.0; rounding up goes toward +1.0.
      return 1'f32
    else:
      # +0.0 remains unchanged
      return x

func ceil*(x: float64): float64 =
  var ix = cast[uint64](x)
  let e = getExpUnbiased(x)

  # If the represented value has no fractional part, no truncation is needed.
  if e >= FLOAT64_SIG_BITS:
    return x

  if e >= 0:
    # |x| >= 1.0
    let m = FLOAT64_SIG_MASK shr uint32(e)
    if (ix and m) == 0'u64:
      # Portion to be masked is already zero; no adjustment needed.
      return x

    if isSignPositive(x):
      ix = ix + m

    ix = ix and (not m)
    return cast[float64](ix)
  else:
    # |x| < 1.0, raise an inexact exception since truncation will happen (unless x == 0).
    if (ix and (not FLOAT64_SIGN_MASK)) == 0'u64:
      return x

    if isSignNegative(x):
      # -1.0 < x <= -0.0; rounding up goes toward -0.0.
      return -0.0
    elif (ix shl 1) != 0'u64:
      # 0.0 < x < 1.0; rounding up goes toward +1.0.
      return 1.0
    else:
      # +0.0 remains unchanged
      return x

## Trunc
##
## Rounds the number toward 0 to the closest integral value.
## This effectively removes the decimal part of the number, leaving the integral part.
func truncStatus(x: float32): tuple[val: float32, status: int] {.inline.} =
  let xi = cast[uint32](x)
  let e = getExpUnbiased(x)

  # The represented value has no fractional part, so no truncation is needed
  if e >= FLOAT32_SIG_BITS:
    return (x, 0)

  let clearMask = if e < 0:
    # If the exponent is negative, the result will be zero so we clear everything
    # except the sign.
    not FLOAT32_SIGN_MASK
  else:
    # Otherwise, we keep `e` fractional bits and clear the rest.
    FLOAT32_SIG_MASK shr uint32(e)

  let cleared = xi and clearMask
  let status = if cleared == 0'u32: 0 else: 1

  # Now zero the bits we need to truncate and return.
  (cast[float32](xi xor cleared), status)

func truncStatus(x: float64): tuple[val: float64, status: int] {.inline.} =
  let xi = cast[uint64](x)
  let e = getExpUnbiased(x)

  # The represented value has no fractional part, so no truncation is needed
  if e >= FLOAT64_SIG_BITS:
    return (x, 0)

  let clearMask = if e < 0:
    # If the exponent is negative, the result will be zero so we clear everything
    # except the sign.
    not FLOAT64_SIGN_MASK
  else:
    # Otherwise, we keep `e` fractional bits and clear the rest.
    FLOAT64_SIG_MASK shr uint32(e)

  let cleared = xi and clearMask
  let status = if cleared == 0'u64: 0 else: 1

  # Now zero the bits we need to truncate and return.
  (cast[float64](xi xor cleared), status)

func trunc*(x: float32): float32 =
  truncStatus(x).val

func trunc*(x: float64): float64 =
  truncStatus(x).val

## Round
##
## Round `x` to the nearest integer, breaking ties away from zero.
func round*(x: float32): float32 =
  let f0p5 = 0.5'f32  # 0.5
  let f0p25 = 0.25'f32  # 0.25
  # Approximation of EPSILON for f32: 2^-23
  let actualEps = 1.1920928955078125e-7'f32  # 2^-23

  truncStatus(x + copySign(f0p5 - f0p25 * actualEps, x)).val

func round*(x: float64): float64 =
  let f0p5 = 0.5  # 0.5
  let f0p25 = 0.25  # 0.25
  # Approximation of EPSILON for f64: 2^-52
  let actualEps = 2.220446049250313080847e-16  # 2^-52

  truncStatus(x + copySign(f0p5 - f0p25 * actualEps, x)).val

## Reduce a significand modulo another after multiplying it by a power of two.
func reduceMod32(x, exponent: uint32, y: uint32): uint32 {.inline.} =
  var rem = x
  if rem >= (y shl 1):
    rem = rem mod y
  var n = exponent
  while n > 0'u32:
    if rem >= y:
      rem = rem - y
    rem = rem shl 1
    dec n
  if rem >= y:
    rem = rem - y
  rem

func reduceMod64(x: uint64, exponent: uint32, y: uint64): uint64 {.inline.} =
  var rem = x
  if rem >= (y shl 1):
    rem = rem mod y
  var n = exponent
  while n > 0'u32:
    if rem >= y:
      rem = rem - y
    rem = rem shl 1
    dec n
  if rem >= y:
    rem = rem - y
  rem

## Modulo
##
## Computes x modulo y (remainder of x/y), using significand reduction to avoid
## overflowing the quotient or losing low remainder bits.
func `mod`*(x, y: float32): float32 =
  let sx = cast[uint32](x) and FLOAT32_SIGN_MASK
  let ux = cast[uint32](x) and (not FLOAT32_SIGN_MASK)
  let uy = cast[uint32](y) and (not FLOAT32_SIGN_MASK)

  # Cases that return NaN:
  #   NaN % _
  #   Inf % _
  #     _ % NaN
  #     _ % 0
  let xNanOrInf = (ux and FLOAT32_EXP_MASK) == FLOAT32_EXP_MASK
  let yNanOrZero = ((uy - 1'u32) and FLOAT32_EXP_MASK) == FLOAT32_EXP_MASK
  if xNanOrInf or yNanOrZero:
    return (x * y) / (x * y)

  if ux < uy:
    # |x| < |y|
    return x

  let satX = if ux >= FLOAT32_IMPLICIT_BIT: ux - FLOAT32_IMPLICIT_BIT else: 0'u32
  let satY = if uy >= FLOAT32_IMPLICIT_BIT: uy - FLOAT32_IMPLICIT_BIT else: 0'u32
  let num = ux - (satX and FLOAT32_EXP_MASK)
  let den = uy - (satY and FLOAT32_EXP_MASK)
  let ex = satX shr FLOAT32_SIG_BITS
  let ey = satY shr FLOAT32_SIG_BITS
  let rem = reduceMod32(num, ex - ey, den)
  if rem == 0'u32:
    return cast[float32](sx)

  var top = rem
  var log2rem = 0'u32
  while top > 1'u32:
    top = top shr 1
    inc log2rem
  let maxShift = uint32(FLOAT32_SIG_BITS) - log2rem
  let shift = if ey < maxShift: ey else: maxShift
  let bits = (rem shl shift) + ((ey - shift) shl FLOAT32_SIG_BITS)
  cast[float32](sx or bits)

func `mod`*(x, y: float64): float64 =
  let sx = cast[uint64](x) and FLOAT64_SIGN_MASK
  let ux = cast[uint64](x) and (not FLOAT64_SIGN_MASK)
  let uy = cast[uint64](y) and (not FLOAT64_SIGN_MASK)

  # Cases that return NaN:
  #   NaN % _
  #   Inf % _
  #     _ % NaN
  #     _ % 0
  let xNanOrInf = (ux and FLOAT64_EXP_MASK) == FLOAT64_EXP_MASK
  let yNanOrZero = ((uy - 1'u64) and FLOAT64_EXP_MASK) == FLOAT64_EXP_MASK
  if xNanOrInf or yNanOrZero:
    return (x * y) / (x * y)

  if ux < uy:
    # |x| < |y|
    return x

  let satX = if ux >= FLOAT64_IMPLICIT_BIT: ux - FLOAT64_IMPLICIT_BIT else: 0'u64
  let satY = if uy >= FLOAT64_IMPLICIT_BIT: uy - FLOAT64_IMPLICIT_BIT else: 0'u64
  let num = ux - (satX and FLOAT64_EXP_MASK)
  let den = uy - (satY and FLOAT64_EXP_MASK)
  let ex = uint32(satX shr FLOAT64_SIG_BITS)
  let ey = uint32(satY shr FLOAT64_SIG_BITS)
  let rem = reduceMod64(num, ex - ey, den)
  if rem == 0'u64:
    return cast[float64](sx)

  var top = rem
  var log2rem = 0'u32
  while top > 1'u64:
    top = top shr 1
    inc log2rem
  let maxShift = uint32(FLOAT64_SIG_BITS) - log2rem
  let shift = if ey < maxShift: ey else: maxShift
  let bits = (rem shl shift) + (uint64(ey - shift) shl FLOAT64_SIG_BITS)
  cast[float64](sx or bits)
