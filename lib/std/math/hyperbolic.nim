## Native (libc-free) implementations of `expm1`, `sinh`, `cosh`, `tanh`,
## `arcsinh`, `arccosh` and `arctanh`. The hyperbolic formulas and range handling
## follow Rust compiler-builtins/libm's corresponding f32/f64 routines; `expm1`
## is adapted from FreeBSD/SunPro fdlibm `s_expm1.c`.
## Rust sources: `library/compiler-builtins/libm/src/math/{expm1,expm1f,sinh,sinhf,cosh,coshf,tanh,tanhf,asinh,asinhf,acosh,acoshf,atanh,atanhf,k_expo2,k_expo2f}.rs`.
## Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
## Copyright (c) 2018 Jorge Aparicio (Rust compiler-builtins/libm code).
## Permission to use, copy, modify, and distribute this software is freely
## granted, provided that this notice is preserved.
## Selected instead of `math/cmath` by `std/math` when `nimNativeIo` is defined.
import std/math/common
import std/math/exponential
import std/math/power

# Helper functions for hyperbolic implementations

const EXPM1_O_THRESHOLD: float64 = 7.09782712893383973096e+02
const EXPM1_LN2_HI: float64 = 6.93147180369123816490e-01
const EXPM1_LN2_LO: float64 = 1.90821492927058770002e-10
const EXPM1_INVLN2: float64 = 1.44269504088896338700e+00
const EXPM1_Q1: float64 = -3.33333333333331316428e-02
const EXPM1_Q2: float64 = 1.58730158725481460165e-03
const EXPM1_Q3: float64 = -7.93650757867487942473e-05
const EXPM1_Q4: float64 = 4.00821782732936239552e-06
const EXPM1_Q5: float64 = -2.01099218183624371326e-07

const EXPM1F_LN2_HI: float32 = 6.9313812256e-01'f32
const EXPM1F_LN2_LO: float32 = 9.0580006145e-06'f32
const EXPM1F_INV_LN2: float32 = 1.4426950216e+00'f32
const EXPM1F_Q1: float32 = -3.3333212137e-2'f32
const EXPM1F_Q2: float32 = 1.5807170421e-3'f32

const LN2_HYPERBOLIC: float64 = 0.693147180559945309417232121458176568
const LN2_HYPERBOLIC_F32: float32 = 0.693147180559945309417232121458176568'f32

func expm1*(x: float64): float64 =
  ## Exponential, base *e*, of x-1 (f64)
  ##
  ## Calculates the exponential of `x` and subtract 1, that is, *e* raised
  ## to the power `x` minus 1 (where *e* is the base of the natural
  ## system of logarithms, approximately 2.71828).
  ## The result is accurate even for small values of `x`,
  ## where using `exp(x)-1` would lose many significant digits.
  var x = x
  var ui = cast[uint64](x)
  let hx = ((ui shr 32) and 0x7fffffff'u64).uint32
  let sign = (ui shr 63).int32

  # filter out huge and non-finite argument
  if hx >= 0x4043687A'u32:
    # if |x|>=56*ln2
    if x != x:
      return x
    if sign != 0:
      return -1.0
    if x > EXPM1_O_THRESHOLD:
      let large_float = cast[float64](0x7fe0000000000000'u64)
      x = x * large_float
      return x

  # argument reduction
  var hi, lo: float64
  var k: int32
  var c: float64
  if hx > 0x3fd62e42'u32:
    # if  |x| > 0.5 ln2
    if hx < 0x3FF0A2B2'u32:
      # and |x| < 1.5 ln2
      if sign == 0:
        hi = x - EXPM1_LN2_HI
        lo = EXPM1_LN2_LO
        k = 1
      else:
        hi = x + EXPM1_LN2_HI
        lo = -EXPM1_LN2_LO
        k = -1
    else:
      k = int32(EXPM1_INVLN2 * x + (if sign != 0: -0.5 else: 0.5))
      let t = float64(k)
      hi = x - t * EXPM1_LN2_HI
      lo = t * EXPM1_LN2_LO
    x = hi - lo
    c = (hi - x) - lo
  elif hx < 0x3c900000'u32:
    # |x| < 2**-54, return x
    if hx < 0x00100000'u32:
      # Note: This would raise inexact exception if x != 0
      # We can't easily do that in Nim, so we just continue
      discard 0
    return x
  else:
    c = 0.0
    k = 0

  # x is now in primary range
  let hfx = 0.5 * x
  let hxs = x * hfx
  let r1 = 1.0 + hxs * (EXPM1_Q1 + hxs * (EXPM1_Q2 + hxs * (EXPM1_Q3 + hxs * (EXPM1_Q4 + hxs * EXPM1_Q5))))
  let t = 3.0 - r1 * hfx
  var e = hxs * ((r1 - t) / (6.0 - x * t))
  if k == 0:
    # c is 0
    return x - (x * e - hxs)
  e = x * (e - c) - c
  e = e - hxs
  # exp(x) ~ 2^k (x_reduced - e + 1)
  if k == -1:
    return 0.5 * (x - e) - 0.5
  if k == 1:
    if x < -0.25:
      return -2.0 * (e - (x + 0.5))
    return 1.0 + 2.0 * (x - e)
  ui = (0x3ff'u64 + uint64(k)) shl 52  # 2^k
  let twopk = cast[float64](ui)
  if k < 0 or k > 56:
    # suffice to return exp(x)-1
    var y = x - e + 1.0
    if k == 1024:
      let large_float = cast[float64](0x7fe0000000000000'u64)
      y = y * 2.0 * large_float
    else:
      y = y * twopk
    return y - 1.0
  ui = (0x3ff'u64 - uint64(k)) shl 52  # 2^-k
  let uf = cast[float64](ui)
  if k < 20:
    return (x - e + (1.0 - uf)) * twopk
  else:
    return (x - (e + uf) + 1.0) * twopk

func expm1*(x: float32): float32 =
  ## Exponential, base *e*, of x-1 (f32)
  ##
  ## Calculates the exponential of `x` and subtract 1, that is, *e* raised
  ## to the power `x` minus 1 (where *e* is the base of the natural
  ## system of logarithms, approximately 2.71828).
  ## The result is accurate even for small values of `x`,
  ## where using `exp(x)-1` would lose many significant digits.
  var x = x
  let x1p127 = cast[float32](0x7f000000'u32)
  var hx = cast[uint32](x)
  let sign = (hx shr 31) != 0
  hx = hx and 0x7fffffff'u32

  # filter out huge and non-finite argument
  if hx >= 0x4195b844'u32:
    # if |x|>=27*ln2
    if hx > 0x7f800000'u32:
      # NaN
      return x
    if sign:
      return -1.0'f32
    if hx > 0x42b17217'u32:
      # x > log(FLT_MAX)
      x = x * cast[float32](0x7f000000'u32)
      return x

  var k: int32
  var hi, lo: float32
  var c = 0'f32
  # argument reduction
  if hx > 0x3eb17218'u32:
    # if  |x| > 0.5 ln2
    if hx < 0x3F851592'u32:
      # and |x| < 1.5 ln2
      if not sign:
        hi = x - EXPM1F_LN2_HI
        lo = EXPM1F_LN2_LO
        k = 1
      else:
        hi = x + EXPM1F_LN2_HI
        lo = -EXPM1F_LN2_LO
        k = -1
    else:
      k = int32(EXPM1F_INV_LN2 * x + (if sign: -0.5'f32 else: 0.5'f32))
      let t = float32(k)
      hi = x - t * EXPM1F_LN2_HI
      lo = t * EXPM1F_LN2_LO
    x = hi - lo
    c = (hi - x) - lo
  elif hx < 0x33000000'u32:
    # when |x|<2**-25, return x
    if hx < 0x00800000'u32:
      # Note: This would raise inexact exception if x != 0
      # We can't easily do that in Nim, so we just continue
      discard 0
    return x
  else:
    k = 0

  # x is now in primary range
  let hfx = 0.5'f32 * x
  let hxs = x * hfx
  let r1 = 1.0'f32 + hxs * (EXPM1F_Q1 + hxs * EXPM1F_Q2)
  let t = 3.0'f32 - r1 * hfx
  var e = hxs * ((r1 - t) / (6.0'f32 - x * t))
  if k == 0:
    # c is 0
    return x - (x * e - hxs)
  e = x * (e - c) - c
  e = e - hxs
  # exp(x) ~ 2^k (x_reduced - e + 1)
  if k == -1:
    return 0.5'f32 * (x - e) - 0.5'f32
  if k == 1:
    if x < -0.25'f32:
      return -2.0'f32 * (e - (x + 0.5'f32))
    return 1.0'f32 + 2.0'f32 * (x - e)
  let twopk = cast[float32](((0x7f'u32 + uint32(k)) shl 23))
  if k < 0 or k > 56:
    # suffice to return exp(x)-1
    var y = x - e + 1.0'f32
    if k == 128:
      y = y * 2.0'f32 * cast[float32](0x7f000000'u32)
    else:
      y = y * twopk
    return y - 1.0'f32
  let uf = cast[float32](((0x7f'u32 - uint32(k)) shl 23))
  if k < 23:
    return (x - e + (1.0'f32 - uf)) * twopk
  else:
    return (x - (e + uf) + 1.0'f32) * twopk

func k_expo2*(x: float64): float64 =
  ## Compute exp(x)/2 for x >= log(DBL_MAX), slightly better than 0.5*exp(x/2)*exp(x/2)
  const K: int32 = 2043
  let k_ln2 = cast[float64](0x40962066151add8b'u64)
  # note that k is odd and scale*scale overflows
  let scale_bits: uint64 = (0x3ff'u64 + uint64(K div 2)) shl 20
  let scale = cast[float64](scale_bits shl 32)
  # exp(x - k ln2) * 2**(k-1)
  exp(x - k_ln2) * scale * scale

func k_expo2f*(x: float32): float32 =
  ## Compute expf(x)/2 for x >= log(FLT_MAX), slightly better than 0.5f*expf(x/2)*expf(x/2)
  const K: int32 = 235
  let k_ln2 = cast[float32](0x4322e3bc'u32)
  # note that k is odd and scale*scale overflows
  let scale_bits: uint32 = (0x7f'u32 + uint32(K div 2)) shl 23
  let scale = cast[float32](scale_bits)
  # exp(x - k ln2) * 2**(k-1)
  exp(x - k_ln2) * scale * scale

# sinh(x) = (exp(x) - 1/exp(x))/2
#         = (exp(x)-1 + (exp(x)-1)/exp(x))/2
#         = x + x^3/6 + o(x^5)

func sinh*(x: float32): float32 =
  ## The hyperbolic sine of `x` (f32).
  var h = 0.5'f32
  var ix = cast[uint32](x)
  if (ix shr 31) != 0:
    h = -h
  # |x|
  ix = ix and 0x7fffffff'u32
  let absx = cast[float32](ix)
  let w = ix

  # |x| < log(FLT_MAX)
  if w < 0x42b17217'u32:
    let t = expm1(absx)
    if w < 0x3f800000'u32:
      if w < (0x3f800000'u32 - (12'u32 shl 23)):
        return x
      return h * (2.0'f32 * t - t * t / (t + 1.0'f32))
    return h * (t + t / (t + 1.0'f32))

  # |x| > logf(FLT_MAX) or nan
  2.0'f32 * h * k_expo2f(absx)

func sinh*(x: float64): float64 =
  ## The hyperbolic sine of `x` (f64).
  var h = 0.5
  var ui = cast[uint64](x)
  if (ui shr 63) != 0:
    h = -h
  # |x|
  ui = ui and 0x7fffffffffffffff'u64
  let absx = cast[float64](ui)
  let w = (ui shr 32).uint32

  # |x| < log(DBL_MAX)
  if w < 0x40862e42'u32:
    let t = expm1(absx)
    if w < 0x3ff00000'u32:
      if w < 0x3ff00000'u32 - (26'u32 shl 20):
        # note: inexact and underflow are raised by expm1
        # note: this branch avoids spurious underflow
        return x
      return h * (2.0 * t - t * t / (t + 1.0))
    # note: |x|>log(0x1p26)+eps could be just h*exp(x)
    return h * (t + t / (t + 1.0))

  # |x| > log(DBL_MAX) or nan
  # note: the result is stored to handle overflow
  let t = 2.0 * h * k_expo2(absx)
  t

# Hyperbolic cosine (f64)
#
# Computes the hyperbolic cosine of the argument x.
# Is defined as `(exp(x) + exp(-x))/2`
# Angles are specified in radians.

func cosh*(x: float32): float32 =
  ## Hyperbolic cosine (f32)
  ##
  ## Computes the hyperbolic cosine of the argument x.
  ## Is defined as `(exp(x) + exp(-x))/2`
  ## Angles are specified in radians.
  var x = x
  # |x|
  var ix = cast[uint32](x)
  ix = ix and 0x7fffffff'u32
  x = cast[float32](ix)
  let w = ix

  # |x| < log(2)
  if w < 0x3f317217'u32:
    if w < (0x3f800000'u32 - (12'u32 shl 23)):
      let x1p120 = cast[float32](0x7b800000'u32)  # 0x1p120f === 2 ^ 120
      discard x + x1p120
      return 1.0'f32
    let t = expm1(x)
    return 1.0'f32 + t * t / (2.0'f32 * (1.0'f32 + t))

  # |x| < log(FLT_MAX)
  if w < 0x42b17217'u32:
    let t = exp(x)
    return 0.5'f32 * (t + 1.0'f32 / t)

  # |x| > log(FLT_MAX) or nan
  k_expo2f(x)

func cosh*(x: float64): float64 =
  ## Hyperbolic cosine (f64)
  ##
  ## Computes the hyperbolic cosine of the argument x.
  ## Is defined as `(exp(x) + exp(-x))/2`
  ## Angles are specified in radians.
  var x = x
  # |x|
  var ix = cast[uint64](x)
  ix = ix and 0x7fffffffffffffff'u64
  x = cast[float64](ix)
  let w = (ix shr 32).uint32

  # |x| < log(2)
  if w < 0x3fe62e42'u32:
    if w < 0x3ff00000'u32 - (26'u32 shl 20):
      let x1p120 = cast[float64](0x4770000000000000'u64)
      discard x + x1p120
      return 1.0
    let t = expm1(x)  # exponential minus 1
    return 1.0 + t * t / (2.0 * (1.0 + t))

  # |x| < log(DBL_MAX)
  if w < 0x40862e42'u32:
    let t = exp(x)
    # note: if x>log(0x1p26) then the 1/t is not needed
    return 0.5 * (t + 1.0 / t)

  # |x| > log(DBL_MAX) or nan
  k_expo2(x)

# tanh(x) = (exp(x) - exp(-x))/(exp(x) + exp(-x))
#         = (exp(2*x) - 1)/(exp(2*x) - 1 + 2)
#         = (1 - exp(-2*x))/(exp(-2*x) - 1 + 2)

func tanh*(x: float32): float32 =
  ## The hyperbolic tangent of `x` (f32).
  ##
  ## `x` is specified in radians.
  var x = x
  var ix = cast[uint32](x)
  let sign = (ix shr 31) != 0
  ix = ix and 0x7fffffff'u32
  x = cast[float32](ix)
  let w = ix

  let tt = if w > 0x3f0c9f54'u32:
    # |x| > log(3)/2 ~= 0.5493 or nan
    if w > 0x41200000'u32:
      # |x| > 10
      1.0'f32 + 0.0'f32 / x
    else:
      let t = expm1(2.0'f32 * x)
      1.0'f32 - 2.0'f32 / (t + 2.0'f32)
  elif w > 0x3e82c578'u32:
    # |x| > log(5/3)/2 ~= 0.2554
    let t = expm1(2.0'f32 * x)
    t / (t + 2.0'f32)
  elif w >= 0x00800000'u32:
    # |x| >= 0x1p-126
    let t = expm1(-2.0'f32 * x)
    -t / (t + 2.0'f32)
  else:
    # |x| is subnormal
    discard 0
    x

  if sign: -tt else: tt

func tanh*(x: float64): float64 =
  ## The hyperbolic tangent of `x` (f64).
  ##
  ## `x` is specified in radians.
  var x = x
  var ui = cast[uint64](x)
  let sign = (ui shr 63) != 0
  ui = ui and 0x7fffffffffffffff'u64
  x = cast[float64](ui)
  let w = (ui shr 32).uint32

  var t: float64
  if w > 0x3fe193ea'u32:
    # |x| > log(3)/2 ~= 0.5493 or nan
    if w > 0x40340000'u32:
      # |x| > 20 or nan
      # note: this branch avoids raising overflow
      t = 1.0 - 0.0 / x
    else:
      t = expm1(2.0 * x)
      t = 1.0 - 2.0 / (t + 2.0)
  elif w > 0x3fd058ae'u32:
    # |x| > log(5/3)/2 ~= 0.2554
    t = expm1(2.0 * x)
    t = t / (t + 2.0)
  elif w >= 0x00100000'u32:
    # |x| >= 0x1p-1022, up to 2ulp error in [0.1,0.2554]
    t = expm1(-2.0 * x)
    t = -t / (t + 2.0)
  else:
    # |x| is subnormal
    # note: the branch above would not raise underflow in [0x1p-1023,0x1p-1022)
    discard 0
    t = x

  if sign: -t else: t

# asinh(x) = sign(x)*log(|x|+sqrt(x*x+1)) ~= x - x^3/6 + o(x^5)

func arcsinh*(x: float32): float32 =
  ## Inverse hyperbolic sine (f32)
  ##
  ## Calculates the inverse hyperbolic sine of `x`.
  ## Is defined as `sgn(x)*log(|x|+sqrt(x*x+1))`.
  var x = x
  let u = cast[uint32](x)
  let i = u and 0x7fffffff'u32
  let sign = (u shr 31) != 0

  # |x|
  x = cast[float32](i)

  if i >= 0x3f800000'u32 + (12'u32 shl 23):
    # |x| >= 0x1p12 or inf or nan
    x = ln(x) + LN2_HYPERBOLIC_F32
  elif i >= 0x3f800000'u32 + (1'u32 shl 23):
    # |x| >= 2
    x = ln(2.0'f32 * x + 1.0'f32 / (sqrt(x * x + 1.0'f32) + x))
  elif i >= 0x3f800000'u32 - (12'u32 shl 23):
    # |x| >= 0x1p-12, up to 1.6ulp error in [0.125,0.5]
    x = log1p(x + x * x / (sqrt(x * x + 1.0'f32) + 1.0'f32))
  else:
    # |x| < 0x1p-12, raise inexact if x!=0
    let x1p120 = cast[float32](0x7b800000'u32)
    discard x + x1p120

  if sign: -x else: x

func arcsinh*(x: float64): float64 =
  ## Inverse hyperbolic sine (f64)
  ##
  ## Calculates the inverse hyperbolic sine of `x`.
  ## Is defined as `sgn(x)*log(|x|+sqrt(x*x+1))`.
  var x = x
  var u = cast[uint64](x)
  let e = ((u shr 52) and 0x7ff'u64).int
  let sign = (u shr 63) != 0

  # |x|
  u = u and (not (0'u64)) shr 1
  x = cast[float64](u)

  if e >= 0x3ff + 26:
    # |x| >= 0x1p26 or inf or nan
    x = ln(x) + LN2_HYPERBOLIC
  elif e >= 0x3ff + 1:
    # |x| >= 2
    x = ln(2.0 * x + 1.0 / (sqrt(x * x + 1.0) + x))
  elif e >= 0x3ff - 26:
    # |x| >= 0x1p-26, up to 1.6ulp error in [0.125,0.5]
    x = log1p(x + x * x / (sqrt(x * x + 1.0) + 1.0))
  else:
    # |x| < 0x1p-26, raise inexact if x != 0
    let x1p120 = cast[float64](0x4770000000000000'u64)
    discard x + x1p120

  if sign: -x else: x

# acosh(x) = log(x + sqrt(x*x-1))
# x must be a number greater than or equal to 1.

func arccosh*(x: float32): float32 =
  ## Inverse hyperbolic cosine (f32)
  ##
  ## Calculates the inverse hyperbolic cosine of `x`.
  ## Is defined as `log(x + sqrt(x*x-1))`.
  ## `x` must be a number greater than or equal to 1.
  let ux = cast[uint32](x)

  # x < 1 domain error is handled in the called functions
  if (ux and not (0x80000000'u32)) < cast[uint32](2.0'f32):
    # |x| < 2, invalid if x < 1
    # up to 2ulp error in [1,1.125]
    let x_1 = x - 1.0'f32
    log1p(x_1 + sqrt(x_1 * x_1 + 2.0'f32 * x_1))
  elif ux < cast[uint32]((1'u32 shl 12).float32):
    # 2 <= x < 0x1p12
    ln(2.0'f32 * x - 1.0'f32 / (x + sqrt(x * x - 1.0'f32)))
  else:
    # x >= 0x1p12 or x <= -2 or nan
    ln(x) + LN2_HYPERBOLIC_F32

func arccosh*(x: float64): float64 =
  ## Inverse hyperbolic cosine (f64)
  ##
  ## Calculates the inverse hyperbolic cosine of `x`.
  ## Is defined as `log(x + sqrt(x*x-1))`.
  ## `x` must be a number greater than or equal to 1.
  let ux = cast[uint64](x)

  # x < 1 domain error is handled in the called functions
  if (ux and not (0x8000000000000000'u64)) < cast[uint64](2.0):
    # |x| < 2, invalid if x < 1
    # up to 2ulp error in [1,1.125]
    let x_1 = x - 1.0
    log1p(x_1 + sqrt(x_1 * x_1 + 2.0 * x_1))
  elif ux < cast[uint64]((1'u64 shl 26).float64):
    # 2 <= x < 0x1p26
    ln(2.0 * x - 1.0 / (x + sqrt(x * x - 1.0)))
  else:
    # x >= 0x1p26 or x <= -2 or nan
    ln(x) + LN2_HYPERBOLIC

# atanh(x) = log((1+x)/(1-x))/2 = log1p(2x/(1-x))/2 ~= x + x^3/3 + o(x^5)

func arctanh*(x: float32): float32 =
  ## Inverse hyperbolic tangent (f32)
  ##
  ## Calculates the inverse hyperbolic tangent of `x`.
  ## Is defined as `log((1+x)/(1-x))/2 = log1p(2x/(1-x))/2`.
  var x = x
  var u = cast[uint32](x)
  let sign = (u shr 31) != 0

  # |x|
  u = u and 0x7fffffff'u32
  x = cast[float32](u)

  if u < 0x3f800000'u32 - (1'u32 shl 23):
    if u < 0x3f800000'u32 - (32'u32 shl 23):
      # handle underflow
      if u < (1'u32 shl 23):
        discard 0
    else:
      # |x| < 0.5, up to 1.7ulp error
      x = 0.5'f32 * log1p(2.0'f32 * x + 2.0'f32 * x * x / (1.0'f32 - x))
  else:
    # avoid overflow
    x = 0.5'f32 * log1p(2.0'f32 * (x / (1.0'f32 - x)))

  if sign: -x else: x

func arctanh*(x: float64): float64 =
  ## Inverse hyperbolic tangent (f64)
  ##
  ## Calculates the inverse hyperbolic tangent of `x`.
  ## Is defined as `log((1+x)/(1-x))/2 = log1p(2x/(1-x))/2`.
  var x = x
  var u = cast[uint64](x)
  let e = ((u shr 52) and 0x7ff'u64).int
  let sign = (u shr 63) != 0

  # |x|
  var y = cast[float64](u and 0x7fffffffffffffff'u64)

  if e < 0x3ff - 1:
    if e < 0x3ff - 32:
      # handle underflow
      if e == 0:
        discard 0
    else:
      # |x| < 0.5, up to 1.7ulp error
      y = 0.5 * log1p(2.0 * y + 2.0 * y * y / (1.0 - y))
  else:
    # avoid overflow
    y = 0.5 * log1p(2.0 * (y / (1.0 - y)))

  if sign: -y else: y
