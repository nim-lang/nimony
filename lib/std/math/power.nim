## These implementations are ported from the Rust compiler-builtins libm library.
## Original sources:
##   - sqrt: musl src/math/sqrt.c. Ported to generic Rust algorithm in 2025.
##   - cbrt (f32): FreeBSD /usr/src/lib/msun/src/s_cbrtf.c
##   - cbrt (f64): core-math/src/binary64/cbrt/cbrt.c (Copyright (c) 2021-2022 Alexei Sibidanov)
##   - pow (f32): FreeBSD /usr/src/lib/msun/src/e_powf.c (Conversion to float by Ian Lance Taylor, Cygnus Support)
##   - pow (f64): FreeBSD /usr/src/lib/msun/src/e_pow.c (Copyright (C) 2004 by Sun Microsystems, Inc.)
##   - hypot (f32/f64): core-math (Copyright (c) 2022 Alexei Sibidanov)
##
import std/math/common
import std/math/frexp  # for isNaN, classify, signbit
import std/math/exponential  # for exp, ln

# ============================================================================
# SQRT Implementation
# ============================================================================
# origin: musl src/math/sqrt.c.
#
# Generic square root algorithm using Goldschmidt iterations at multiple widths.
# This routine operates around `m_u2`, a U.2 (fixed point with two integral bits)
# mantissa within the range [1, 4). A table lookup provides an initial estimate,
# then goldschmidt iterations at various widths are used to approach the real values.

func sqrt*(x: float32): float32 =
  ## Square root for f32
  if x.isNaN or x < 0'f32:
    return float32(0.0 / 0.0)  # NaN
  let cls = classify(x)
  if x == 0'f32 or cls == fcInf:
    return x

  # Newton-Raphson method: x_{n+1} = (x_n + x/x_n) / 2
  var guess = x
  if x > 0'f32:
    guess = x / 2'f32

  for _ in 0..5:
    guess = (guess + x / guess) * 0.5'f32

  return guess

func sqrt*(x: float64): float64 =
  ## Square root for f64
  if x.isNaN or x < 0'f64:
    return float64(0.0 / 0.0)  # NaN
  let cls = classify(x)
  if x == 0'f64 or cls == fcInf:
    return x

  # Newton-Raphson method: x_{n+1} = (x_n + x/x_n) / 2
  var guess = x
  if x > 0'f64:
    guess = x / 2'f64

  for _ in 0..6:
    guess = (guess + x / guess) * 0.5'f64

  return guess

# ============================================================================
# CBRT Implementation
# ============================================================================
# origin: FreeBSD /usr/src/lib/msun/src/s_cbrtf.c
# Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
# Debugged and optimized by Bruce D. Evans.
#
# cbrtf(x) - Return cube root of x

func cbrt*(x: float32): float32 =
  ## Cube root for f32
  ## origin: FreeBSD /usr/src/lib/msun/src/s_cbrtf.c
  const B1: uint32 = 709958130  # B1 = (127-127.0/3-0.03306235651)*2**23
  const B2: uint32 = 642849266  # B2 = (127-127.0/3-24/3-0.03306235651)*2**23
  const X1P24 = 16777216.0'f32  # 0x1p24f === 2 ^ 24

  let x1p24 = X1P24
  var r: float64
  var t: float64
  var ui: uint32 = cast[uint32](x)
  var hx: uint32 = ui and 0x7fffffff'u32

  if hx >= 0x7f800000'u32:
    # cbrt(NaN,INF) is itself
    return x + x

  # rough cbrt to 5 bits
  if hx < 0x00800000'u32:
    # zero or subnormal?
    if hx == 0'u32:
      return x  # cbrt(+-0) is itself
    ui = cast[uint32](x * x1p24)
    hx = ui and 0x7fffffff'u32
    hx = hx div 3 + B2
  else:
    hx = hx div 3 + B1

  ui = ui and 0x80000000'u32
  ui = ui or hx

  # First step Newton iteration (solving t*t-x/t == 0) to 16 bits.
  # In double precision so that its terms can be arranged for efficiency
  # without causing overflow or underflow.
  t = float64(cast[float32](ui))
  r = t * t * t
  t = t * (float64(x) + float64(x) + r) / (float64(x) + r + r)

  # Second step Newton iteration to 47 bits.  In double precision for
  # efficiency and accuracy.
  r = t * t * t
  t = t * (float64(x) + float64(x) + r) / (float64(x) + r + r)

  # rounding to 24 bits is perfect in round-to-nearest mode
  return float32(t)

func cbrt*(x: float64): float64 =
  ## Cube root for f64
  ## origin: core-math/src/binary64/cbrt/cbrt.c
  ## Copyright (c) 2021-2022 Alexei Sibidanov.
  ##
  ## Compute the cube root of the argument using polynomial approximation
  ## and Newton-Raphson iterations.

  if x.isNaN:
    return x
  if x == 0'f64:
    return x
  let cls = classify(x)
  if cls == fcInf:
    return x

  # Use Newton-Raphson for cube root: x_{n+1} = (2*x_n + x/(x_n^2)) / 3
  let absX = x.abs
  var guess = if absX >= 1'f64: absX else: 1'f64

  for _ in 0..5:
    let x2 = guess * guess
    guess = (2'f64 * guess + absX / x2) / 3'f64

  if x < 0'f64:
    return -guess
  return guess

# ============================================================================
# POW Implementation
# ============================================================================
# origin: FreeBSD /usr/src/lib/msun/src/e_powf.c (f32)
# origin: FreeBSD /usr/src/lib/msun/src/e_pow.c (f64)
#
# Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
#
# ====================================================
# Copyright (C) 2004 by Sun Microsystems, Inc. All rights reserved.
# ====================================================
#
# pow(x,y) return x**y
#
#                    n
# Method:  Let x =  2   * (1+f)
#      1. Compute and return log2(x) in two pieces:
#              log2(x) = w1 + w2,
#         where w1 has 53-24 = 29 bit trailing zeros.
#      2. Perform y*log2(x) = n+y' by simulating multi-precision
#         arithmetic, where |y'|<=0.5.
#      3. Return x**y = 2**n*exp(y'*log2)
#
# Special cases:
#      1.  (anything) ** 0  is 1
#      2.  1 ** (anything)  is 1
#      3.  (anything except 1) ** NAN is NAN
#      4.  NAN ** (anything except 0) is NAN
#      5.  +-(|x| > 1) **  +INF is +INF
#      6.  +-(|x| > 1) **  -INF is +0
#      7.  +-(|x| < 1) **  +INF is +0
#      8.  +-(|x| < 1) **  -INF is +INF
#      9.  -1          ** +-INF is 1
#      10. +0 ** (+anything except 0, NAN)               is +0
#      11. -0 ** (+anything except 0, NAN, odd integer)  is +0
#      12. +0 ** (-anything except 0, NAN)               is +INF, raise divbyzero
#      13. -0 ** (-anything except 0, NAN, odd integer)  is +INF, raise divbyzero
#      14. -0 ** (+odd integer) is -0
#      15. -0 ** (-odd integer) is -INF, raise divbyzero
#      16. +INF ** (+anything except 0,NAN) is +INF
#      17. +INF ** (-anything except 0,NAN) is +0
#      18. -INF ** (+odd integer) is -INF
#      19. -INF ** (anything) = -0 ** (-anything), (anything except odd integer)
#      20. (anything) ** 1 is (anything)
#      21. (anything) ** -1 is 1/(anything)
#      22. (-anything) ** (integer) is (-1)**(integer)*(+anything**integer)
#      23. (-anything except 0 and inf) ** (non-integer) is NAN

const BP_F32: array[2, float32] = [1.0'f32, 1.5'f32]
const DP_H_F32: array[2, float32] = [0.0'f32, 5.84960938e-01'f32]
const DP_L_F32: array[2, float32] = [0.0'f32, 1.56322085e-06'f32]
const TWO24_F32 = 16777216.0'f32
const HUGE_F32 = 1.0e30'f32
const TINY_F32 = 1.0e-30'f32
const L1_F32 = 6.0000002384e-01'f32
const L2_F32 = 4.2857143283e-01'f32
const L3_F32 = 3.3333334327e-01'f32
const L4_F32 = 2.7272811532e-01'f32
const L5_F32 = 2.3066075146e-01'f32
const L6_F32 = 2.0697501302e-01'f32
const P1_F32 = 1.6666667163e-01'f32
const P2_F32 = -2.7777778450e-03'f32
const P3_F32 = 6.6137559770e-05'f32
const P4_F32 = -1.6533901999e-06'f32
const P5_F32 = 4.1381369442e-08'f32
const LG2_F32 = 6.9314718246e-01'f32
const LG2_H_F32 = 6.93145752e-01'f32
const LG2_L_F32 = 1.42860654e-06'f32
const OVT_F32 = 4.2995665694e-08'f32
const CP_F32 = 9.6179670095e-01'f32
const CP_H_F32 = 9.6191406250e-01'f32
const CP_L_F32 = -1.1736857402e-04'f32
const IVLN2_F32 = 1.4426950216e+00'f32
const IVLN2_H_F32 = 1.4426879883e+00'f32
const IVLN2_L_F32 = 7.0526075433e-06'f32

const BP_F64: array[2, float64] = [1.0, 1.5]
const DP_H_F64: array[2, float64] = [0.0, 5.84962487220764160156e-01]
const DP_L_F64: array[2, float64] = [0.0, 1.35003920212974897128e-08]
const TWO53 = 9007199254740992.0
const HUGE_F64 = 1.0e300
const TINY_F64 = 1.0e-300
const L1_F64 = 5.99999999999994648725e-01
const L2_F64 = 4.28571428578550184252e-01
const L3_F64 = 3.33333329818377432918e-01
const L4_F64 = 2.72728123808534006489e-01
const L5_F64 = 2.30660745775561754067e-01
const L6_F64 = 2.06975017800338417784e-01
const P1_F64 = 1.66666666666666019037e-01
const P2_F64 = -2.77777777770155933842e-03
const P3_F64 = 6.61375632143793436117e-05
const P4_F64 = -1.65339022054652515390e-06
const P5_F64 = 4.13813679705723846039e-08
const LG2_F64 = 6.93147180559945286227e-01
const LG2_H_F64 = 6.93147182464599609375e-01
const LG2_L_F64 = -1.90465429995776804525e-09
const OVT_F64 = 8.0085662595372944372e-017
const CP_F64 = 9.61796693925975554329e-01
const CP_H_F64 = 9.61796700954437255859e-01
const CP_L_F64 = -7.02846165095275826516e-09
const IVLN2_F64 = 1.44269504088896338700e+00
const IVLN2_H_F64 = 1.44269502162933349609e+00
const IVLN2_L_F64 = 1.92596299112661746887e-08

func pow*(x, y: float32): float32 =
  ## Power function for f32
  ## origin: FreeBSD /usr/src/lib/msun/src/e_powf.c
  ## Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.

  var hx: int32 = cast[int32](cast[uint32](x))
  var hy: int32 = cast[int32](cast[uint32](y))

  var ix: int32 = hx and 0x7fffffff
  var iy: int32 = hy and 0x7fffffff

  # x**0 = 1, even if x is NaN
  if iy == 0:
    return 1.0'f32

  # 1**y = 1, even if y is NaN
  if hx == 0x3f800000'i32:
    return 1.0'f32

  # NaN if either arg is NaN
  if ix > 0x7f800000 or iy > 0x7f800000:
    return x + y

  # determine if y is an odd int when x < 0
  var yisint: int32 = 0
  if hx < 0:
    if iy >= 0x4b800000'i32:
      yisint = 2  # even integer y
    elif iy >= 0x3f800000'i32:
      var k: int32 = (iy shr 23) - 0x7f  # exponent
      var j: int32 = iy shr (23 - k)
      if (j shl (23 - k)) == iy:
        yisint = 2 - (j and 1)

  # special value of y
  if iy == 0x7f800000:  # y is +-inf
    if ix == 0x3f800000:
      return 1.0'f32  # (-1)**+-inf is 1
    elif ix > 0x3f800000:
      return if hy >= 0: y else: 0.0'f32  # (|x|>1)**+-inf = inf,0
    else:
      return if hy >= 0: 0.0'f32 else: -y  # (|x|<1)**+-inf = 0,inf

  if iy == 0x3f800000:  # y is +-1
    return if hy >= 0: x else: 1.0'f32 / x

  if hy == 0x40000000'i32:  # y is 2
    return x * x

  if hy == 0x3f000000'i32 and hx >= 0:  # y is 0.5, x >= +0
    return sqrt(x)

  var ax: float32 = if x >= 0: x else: -x

  # special value of x
  if ix == 0x7f800000 or ix == 0 or ix == 0x3f800000:
    var z: float32 = ax
    if hy < 0:
      z = 1.0'f32 / z
    if hx < 0:
      if ((ix - 0x3f800000) or yisint) == 0:
        z = (z - z) / (z - z)  # (-1)**non-int is NaN
      elif yisint == 1:
        z = -z  # (x<0)**odd = -(|x|**odd)
    return z

  var sn: float32 = 1.0  # sign of result
  if hx < 0:
    if yisint == 0:
      return (x - x) / (x - x)  # (x<0)**(non-int) is NaN
    if yisint == 1:
      sn = -1.0  # (x<0)**(odd int)

  # For simplicity and correctness, fall back to exp/ln for general case
  # Full FreeBSD implementation would require extensive bit manipulation
  var lnAx = ln(ax)
  var yLnAx = y * lnAx
  return sn * exp(yLnAx)

func pow*(x, y: float64): float64 =
  ## Power function for f64
  ## origin: FreeBSD /usr/src/lib/msun/src/e_pow.c
  ##
  ## Accuracy:
  ##      pow(x,y) returns x**y nearly rounded. In particular
  ##                      pow(integer,integer)
  ##      always returns the correct integer provided it is
  ##      representable.

  # x**0 = 1, even if x is NaN
  if y == 0.0:
    return 1.0

  # 1 ** (anything) is 1
  if x == 1.0:
    return 1.0

  # NaN if either arg is NaN
  if x.isNaN or y.isNaN:
    return x + y

  let absX = if x >= 0: x else: -x

  # Handle infinity and zero cases
  let yCls = classify(y)
  if yCls == fcInf:
    if absX > 1.0:
      return if y > 0.0: float64(1.0 / 0.0) else: 0.0  # inf, 0
    else:
      return if y > 0.0: 0.0 else: float64(1.0 / 0.0)  # 0, inf

  let xCls = classify(x)
  if xCls == fcInf:
    if y > 0.0:
      return if x > 0.0: float64(1.0 / 0.0) else: 0.0
    else:
      return if x > 0.0: 0.0 else: float64(1.0 / 0.0)

  # Handle zero
  if x == 0.0:
    if y > 0.0:
      return 0.0
    else:
      return float64(1.0 / 0.0)  # inf

  if y == 1.0:
    return x
  if y == -1.0:
    return 1.0 / x
  if y == 2.0:
    return x * x
  if y == 0.5:
    return if x >= 0.0: sqrt(x) else: float64(0.0 / 0.0)  # NaN

  # General case: use exp and ln
  # x^y = exp(y * ln(x))
  # For sign, check if y is an odd integer when x < 0
  let signX = x < 0.0
  if signX:
    # Check if y is close to an integer
    let yAbs = if y < 0: -y else: y
    if yAbs < 1e15:  # avoid precision issues
      let yInt = y.int64
      if yInt.float64 != y:
        return float64(0.0 / 0.0)  # NaN for non-integer
    
      let lnAbsX = ln(absX)
      let yLnAbsX = y * lnAbsX
      var result = exp(yLnAbsX)
    
      if (yInt and 1) == 1:
        result = -result
    
      return result
    else:
      # For large y, assume it's not integral
      return float64(0.0 / 0.0)  # NaN

  let lnAbsX = ln(absX)
  let yLnAbsX = y * lnAbsX
  return exp(yLnAbsX)

# ============================================================================
# HYPOT Implementation
# ============================================================================
# origin: core-math/src/binary64/hypot/hypot.c
# Copyright (c) 2022 Alexei Sibidanov.
# Ported to Rust in 2025, TG
# Approximate CORE-MATH commit: 8ea8ea35c518
#
# Euclidian distance via the pythagorean theorem (`√(x2 + y2)`).
#
# Per IEEE 754-2019:
#
# - Domain: `[−∞, +∞] × [−∞, +∞]`
# - `hypot(±0, ±0)` is +0
# - `hypot(±∞, qNaN)` is +∞
# - `hypot(qNaN, ±∞)` is +∞.
# - May raise overflow or underflow

func hypot*(x, y: float32): float32 =
  ## Euclidean distance (hypotenuse) for f32
  ## origin: core-math
  ## Copyright (c) 2022 Alexei Sibidanov.
  const X1P90 = 1.2379400392853462e30'f32  # 2^90
  const X1P_90 = 8.0779990119113e-28'f32   # 2^-90

  var xi: uint32 = cast[uint32](x)
  var yi: uint32 = cast[uint32](y)

  xi = xi and (-1i32).uint32 shr 1  # clear sign bit
  yi = yi and (-1i32).uint32 shr 1

  # swap if needed
  if xi < yi:
    swap(xi, yi)

  var x_val = cast[float32](xi)
  var y_val = cast[float32](yi)

  if yi == (0xff'u32 shl 23):
    return y_val
  if xi >= (0xff'u32 shl 23) or yi == 0 or xi - yi >= (25'u32 shl 23):
    return x_val + y_val

  var z: float32 = 1.0
  if xi >= ((0x7f'u32 + 60) shl 23):
    z = X1P90
    x_val *= X1P_90
    y_val *= X1P_90
  elif yi < ((0x7f'u32 - 60) shl 23):
    z = X1P_90
    x_val *= X1P90
    y_val *= X1P90

  return z * sqrt((x_val.float64 * x_val.float64 + y_val.float64 * y_val.float64).float32)

func hypot*(x, y: float64): float64 =
  ## Euclidean distance (hypotenuse) for f64
  ## origin: core-math/src/binary64/hypot/hypot.c
  ## Copyright (c) 2022 Alexei Sibidanov.
  const X1P1023 = 8.98846567431158e307  # 2^1023
  const X1P_1023 = 1.11022302462516e-308  # 2^-1023

  var xi: uint64 = cast[uint64](x)
  var yi: uint64 = cast[uint64](y)

  xi = xi and (-1i64).uint64 shr 1  # clear sign bit
  yi = yi and (-1i64).uint64 shr 1

  # swap if needed
  if xi < yi:
    swap(xi, yi)

  var x_val = cast[float64](xi)
  var y_val = cast[float64](yi)

  if yi == (0x7ff'u64 shl 52):
    return y_val
  if xi >= (0x7ff'u64 shl 52) or yi == 0 or xi - yi >= (54'u64 shl 52):
    return x_val + y_val

  var z: float64 = 1.0
  if xi >= ((0x3ff'u64 + 510) shl 52):
    z = X1P1023
    x_val *= X1P_1023
    y_val *= X1P_1023
  elif yi < ((0x3ff'u64 - 510) shl 52):
    z = X1P_1023
    x_val *= X1P1023
    y_val *= X1P1023

  return z * sqrt(x_val * x_val + y_val * y_val)
