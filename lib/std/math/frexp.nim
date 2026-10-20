## Freestanding (`nimNativeIo`, libc-free) implementations of `signbit`,
## `classify`, `isNaN` and `frexp`, operating directly on IEEE-754 bit patterns.
## The normal-value `frexp` bit manipulation follows Rust compiler-builtins/libm's
## `generic/frexp.rs` and FreeBSD/SunPro fdlibm's `s_frexp.c`; subnormals use a
## Nimony-specific pure-Nim scaling/recursion path instead of Rust's bit normalization.
## Classification and sign tests implement the IEEE-754 binary32/binary64
## field definitions and the C `fpclassify`/`signbit`/`isnan` contracts.
## Sources: FreeBSD `/usr/src/lib/msun/src/s_frexp.c`;
## Rust `library/compiler-builtins/libm/src/math/generic/frexp.rs`.
## Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
## Copyright (c) 2018 Jorge Aparicio (Rust compiler-builtins/libm).
## Developed at SunPro, a Sun Microsystems, Inc. business.
## Permission to use, copy, modify, and distribute this software is freely
## granted, provided that this notice is preserved.
import std/math/common

# The `c_fp*` codes are arbitrary but self-consistent with `c_fpclassify`.
const
  c_fpNormal = 0
  c_fpSubnormal = 1
  c_fpZero = 2
  c_fpInfinite = 3
  c_fpNan = 4

func c_signbit[T: SomeFloat](x: T): int =
  when T is float32: result = (if (cast[uint32](x) shr 31) != 0'u32: 1 else: 0)
  else: result = (if (cast[uint64](x) shr 63) != 0'u64: 1 else: 0)

func c_isnan[T: SomeFloat](x: T): int =
  when T is float32:
    let b = cast[uint32](x)
    result = (if ((b shr 23) and 0xFF'u32) == 0xFF'u32 and (b and 0x7FFFFF'u32) != 0'u32: 1 else: 0)
  else:
    let b = cast[uint64](x)
    result = (if ((b shr 52) and 0x7FF'u64) == 0x7FF'u64 and (b and 0xFFFFFFFFFFFFF'u64) != 0'u64: 1 else: 0)

func c_fpclassify[T: SomeFloat](x: T): int =
  when T is float32:
    let b = cast[uint32](x)
    let exp = (b shr 23) and 0xFF'u32
    let mant = b and 0x7FFFFF'u32
    result =
      if exp == 0xFF'u32: (if mant == 0'u32: c_fpInfinite else: c_fpNan)
      elif exp == 0'u32: (if mant == 0'u32: c_fpZero else: c_fpSubnormal)
      else: c_fpNormal
  else:
    let b = cast[uint64](x)
    let exp = (b shr 52) and 0x7FF'u64
    let mant = b and 0xFFFFFFFFFFFFF'u64
    result =
      if exp == 0x7FF'u64: (if mant == 0'u64: c_fpInfinite else: c_fpNan)
      elif exp == 0'u64: (if mant == 0'u64: c_fpZero else: c_fpSubnormal)
      else: c_fpNormal

func signbit*[T: SomeFloat](x: T): bool {.inline.} =
  ## Returns true if `x` is negative, false otherwise.
  runnableExamples:
    assert not signbit(0.0)
    #assert signbit(0.0 * -1.0)
    assert signbit(-0.1)
    assert not signbit(0.1)

  c_signbit(x) != 0

func classify*[T: SomeFloat](x: T): FloatClass {.inline.} =
  ## Classifies a floating point value.
  ##
  ## Returns `x`'s class as specified by the `FloatClass enum<#FloatClass>`_.
  runnableExamples:
    assert classify(0.3) == fcNormal
    assert classify(0.0) == fcZero
    assert classify(0.3 / 0.0) == fcInf
    assert classify(-0.3 / 0.0) == fcNegInf

  let r = c_fpclassify(x)
  if r == c_fpNormal:
    result = fcNormal
  elif r == c_fpSubnormal:
    result = fcSubnormal
  elif r == c_fpZero:
    result = if signbit(x): fcNegZero else: fcZero
  elif r == c_fpNan:
    result = fcNan
  elif r == c_fpInfinite:
    result = if signbit(x): fcNegInf else: fcInf
  else:
    # can be implementation-defined type
    result = fcNan

func isNaN*[T: SomeFloat](x: T): bool {.inline.} =
  ## Returns whether `x` is a `NaN`, more efficiently than via `classify(x) == fcNan`.
  runnableExamples:
    assert NaN.isNaN
    assert not Inf.isNaN
    assert not isNaN(3.1415926)

  c_isnan(x) != 0

func frexp*(x: float32): tuple[frac: float32, exp: int] {.inline.} =
  ## Splits `x` into a normalized fraction `frac` and an integral power of 2 `exp`,
  ## such that `abs(frac) in 0.5..<1` and `x == frac * 2 ^ exp`, except for special
  ## cases shown below.
  runnableExamples:
    assert frexp(8'f32) == (0.5'f32, 4)
    assert frexp(-8'f32) == (-0.5'f32, 4)
    assert frexp(0'f32) == (0'f32, 0)

    # special cases:
    assert frexp(-0'f32).frac.signbit # signbit preserved for +-0
    assert frexp(Inf) == (Inf, 0) # +- Inf preserved
    assert frexp(NaN).frac.isNaN

  var y: uint32 = cast[uint32](x)
  let ee = (y shr 23) and 0xFF'u32
  var exponent: int = 0

  if ee == 0:
    if x != 0.0'f32:
      let scaled = frexp((x * (1'u64 shl 63).float32 * 2.0).float64)
      exponent = scaled.exp - 64
      return (scaled.frac.float32, exponent)
    else:
      return (x, 0)
  elif ee == 0xFF'u32:
    return (x, 0)

  exponent = int(ee) - 0x7E
  y = y and 0x807FFFFF'u32
  y = y or 0x3F000000'u32
  return (cast[float32](y), exponent)

func frexp*(x: float64): tuple[frac: float64, exp: int] {.inline.} =
  ## Splits `x` into a normalized fraction `frac` and an integral power of 2 `exp`,
  ## such that `abs(frac) in 0.5..<1` and `x == frac * 2 ^ exp`, except for special
  ## cases shown below.
  runnableExamples:
    assert frexp(8.0) == (0.5, 4)
    assert frexp(-8.0) == (-0.5, 4)
    assert frexp(0.0) == (0.0, 0)

    # special cases:
    assert frexp(-0.0).frac.signbit # signbit preserved for +-0
    assert frexp(Inf) == (Inf, 0) # +- Inf preserved
    assert frexp(NaN).frac.isNaN

  var y: uint64 = cast[uint64](x)
  let ee = (y shr 52) and 0x7FF'u64
  var exponent: int = 0

  if ee == 0:
    if x != 0.0:
      let scaled = frexp(x * (1'u64 shl 63).float64 * 2.0)
      exponent = scaled.exp - 64
      return (scaled.frac, exponent)
    else:
      return (x, 0)
  elif ee == 0x7FF'u64:
    return (x, 0)

  exponent = int(ee) - 0x3FE
  y = y and 0x800FFFFFFFFFFFFF'u64
  y = y or 0x3FE0000000000000'u64
  return (cast[float64](y), exponent)
