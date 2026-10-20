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
# Exponent-scaled Newton iteration: frexp reduces the input to a compact
# significand range, avoiding overflow and underflow in the iteration.

func powerOfTwo64(exp: int): float64 {.inline.} =
  ## Exact power of two for the bounded exponent ranges used by the roots.
  cast[float64](uint64(exp + 1023) shl 52)

func sqrtFinite64(x: float64): float64 =
  # Reduce the significand to [0.5, 1) and make its exponent even, keeping
  # every Newton step in a compact range regardless of the input magnitude.
  var part = frexp(x)
  if part.exp mod 2 != 0:
    part.frac *= 2.0
    dec part.exp

  var guess = 1.0
  for _ in 0..7:
    guess = (guess + part.frac / guess) * 0.5
  # Compensate the final rounded Newton step using its residual.
  guess += (part.frac - guess * guess) / (2.0 * guess)
  guess * powerOfTwo64(part.exp div 2)

func sqrt*(x: float32): float32 =
  ## Square root for f32
  if x.isNaN or x < 0'f32:
    return float32(0.0 / 0.0)  # NaN
  let cls = classify(x)
  if x == 0'f32 or cls == fcInf:
    return x

  return float32(sqrtFinite64(float64(x)))

func sqrt*(x: float64): float64 =
  ## Square root for f64
  if x.isNaN or x < 0'f64:
    return float64(0.0 / 0.0)  # NaN
  let cls = classify(x)
  if x == 0'f64 or cls == fcInf:
    return x

  return sqrtFinite64(x)

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
  ## Cube root for f64, using exponent-scaled Newton iteration.
  if x == 0'f64 or classify(x) in {fcInf, fcNegInf, fcNan}:
    return x

  # Keep the Newton iteration in a compact range. Starting at abs(x) makes
  # convergence take hundreds of steps for large inputs and loses accuracy
  # even for small integers such as 8.
  var part = frexp(x.abs)
  var rem = part.exp mod 3
  if rem < 0:
    rem += 3
  part.frac *= powerOfTwo64(rem)
  let scaleExp = part.exp - rem

  var guess = 1.0
  for _ in 0..6:
    let guess2 = guess * guess
    guess = (2.0 * guess + part.frac / guess2) / 3.0
  guess += (part.frac - guess * guess * guess) / (3.0 * guess * guess)

  result = guess * powerOfTwo64(scaleExp div 3)
  if signbit(x):
    result = -result

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

func powScale32(x: float32, n: int32): float32 {.inline.} =
  ## Scale by 2^n, including subnormal results, as fdlibm's scalbnf does.
  if n < -126:
    let scale = cast[float32]((n + 25 + 127).uint32 shl 23)
    return (x * scale) * cast[float32](0x33000000'u32) # 2^-25
  if n > 127:
    return (x * 2'f32) * cast[float32](0x7f000000'u32) # 2^127
  x * cast[float32]((n + 127).uint32 shl 23)

func pow*(x, y: float32): float32 =
  ## Port of FreeBSD msun e_powf.c (fdlibm), converted by Ian Lance Taylor.
  var hx = cast[int32](cast[uint32](x))
  let hy = cast[int32](cast[uint32](y))
  var ix = hx and 0x7fffffff'i32
  let iy = hy and 0x7fffffff'i32
  var ax, z, zH, zL, pH, pL, y1, t1, t2, r, sn, t, u, v, w: float32
  var i, j, k, n, yisint, isBits: int32

  if iy == 0: return 1'f32
  if hx == 0x3f800000'i32: return 1'f32
  if ix > 0x7f800000'i32 or iy > 0x7f800000'i32: return x + y

  yisint = 0
  if hx < 0:
    if iy >= 0x4b800000'i32:
      yisint = 2
    elif iy >= 0x3f800000'i32:
      k = (iy shr 23) - 0x7f
      j = iy shr (23 - k)
      if (j shl (23 - k)) == iy:
        yisint = 2 - (j and 1)

  if ix == 0 and hx < 0:
    if hy < 0:
      return if yisint == 1: -Inf.float32 else: Inf.float32
    return if yisint == 1: cast[float32](0x80000000'u32) else: 0'f32

  if iy == 0x7f800000'i32:
    if ix == 0x3f800000'i32: return 1'f32
    if ix > 0x3f800000'i32:
      return if hy >= 0: y else: 0'f32
    return if hy >= 0: 0'f32 else: -y
  if iy == 0x3f800000'i32: return if hy >= 0: x else: 1'f32 / x
  if hy == 0x40000000'i32: return x * x
  if hy == 0x3f000000'i32 and hx >= 0: return sqrt(x)

  ax = x.abs
  if ix == 0x7f800000'i32 or ix == 0 or ix == 0x3f800000'i32:
    z = ax
    if hy < 0: z = 1'f32 / z
    if hx < 0:
      if ((ix - 0x3f800000'i32) or yisint) == 0:
        z = (z - z) / (z - z)
      elif yisint == 1:
        z = cast[float32](cast[uint32](z) xor 0x80000000'u32)
    return z

  sn = 1'f32
  if hx < 0:
    if yisint == 0: return (x - x) / (x - x)
    if yisint == 1: sn = -1'f32

  if iy > 0x4d000000'i32:
    if ix < 0x3f7ffff8'i32: return if hy < 0: sn * HUGE_F32 * HUGE_F32 else: sn * TINY_F32 * TINY_F32
    if ix > 0x3f800007'i32: return if hy > 0: sn * HUGE_F32 * HUGE_F32 else: sn * TINY_F32 * TINY_F32
    t = ax - 1'f32
    w = (t * t) * (0.5'f32 - t * (0.333333333333'f32 - t * 0.25'f32))
    u = IVLN2_H_F32 * t
    v = t * IVLN2_L_F32 - w * IVLN2_F32
    t1 = u + v
    isBits = cast[int32](cast[uint32](t1) and 0xfffff000'u32)
    t1 = cast[float32](cast[uint32](isBits))
    t2 = v - (t1 - u)
  else:
    var s2, sH, sL, tH, tL: float32
    n = 0
    if ix < 0x00800000'i32:
      ax *= TWO24_F32
      n -= 24
      ix = cast[int32](cast[uint32](ax))
    n += (ix shr 23) - 0x7f
    j = ix and 0x007fffff'i32
    ix = j or 0x3f800000'i32
    if j <= 0x1cc471'i32:
      k = 0
    elif j < 0x5db3d7'i32:
      k = 1
    else:
      k = 0
      n += 1
      ix -= 0x00800000'i32
    ax = cast[float32](cast[uint32](ix))

    u = ax - BP_F32[k]
    v = 1'f32 / (ax + BP_F32[k])
    let s = u * v
    sH = s
    isBits = cast[int32](cast[uint32](sH) and 0xfffff000'u32)
    sH = cast[float32](cast[uint32](isBits))
    isBits = cast[int32]((((cast[uint32](ix) shr 1) and 0xfffff000'u32) or 0x20000000'u32))
    tH = cast[float32](cast[uint32](isBits) + 0x00400000'u32 + (cast[uint32](k) shl 21))
    tL = ax - (tH - BP_F32[k])
    sL = v * ((u - sH * tH) - sH * tL)
    s2 = s * s
    r = s2 * s2 * (L1_F32 + s2 * (L2_F32 + s2 * (L3_F32 + s2 * (L4_F32 + s2 * (L5_F32 + s2 * L6_F32)))))
    r += sL * (sH + s)
    s2 = sH * sH
    tH = 3'f32 + s2 + r
    isBits = cast[int32](cast[uint32](tH) and 0xfffff000'u32)
    tH = cast[float32](cast[uint32](isBits))
    tL = r - ((tH - 3'f32) - s2)
    u = sH * tH
    v = sL * tH + tL * s
    pH = u + v
    isBits = cast[int32](cast[uint32](pH) and 0xfffff000'u32)
    pH = cast[float32](cast[uint32](isBits))
    pL = v - (pH - u)
    zH = CP_H_F32 * pH
    zL = CP_L_F32 * pH + pL * CP_F32 + DP_L_F32[k]
    t = float32(n)
    t1 = ((zH + zL) + DP_H_F32[k]) + t
    isBits = cast[int32](cast[uint32](t1) and 0xfffff000'u32)
    t1 = cast[float32](cast[uint32](isBits))
    t2 = zL - (((t1 - t) - DP_H_F32[k]) - zH)

  isBits = cast[int32](cast[uint32](y) and 0xfffff000'u32)
  y1 = cast[float32](cast[uint32](isBits))
  pL = (y - y1) * t1 + y * t2
  pH = y1 * t1
  z = pL + pH
  j = cast[int32](cast[uint32](z))
  if j > 0x43000000'i32: return sn * HUGE_F32 * HUGE_F32
  if j == 0x43000000'i32 and pL + OVT_F32 > z - pH: return sn * HUGE_F32 * HUGE_F32
  if (j and 0x7fffffff'i32) > 0x43160000'i32: return sn * TINY_F32 * TINY_F32
  if j == cast[int32](0xc3160000'u32) and pL <= z - pH: return sn * TINY_F32 * TINY_F32

  i = j and 0x7fffffff'i32
  k = (i shr 23) - 0x7f
  n = 0
  if i > 0x3f000000'i32:
    n = j + (0x00800000'i32 shr (k + 1))
    k = ((n and 0x7fffffff'i32) shr 23) - 0x7f
    t = cast[float32](cast[uint32](n) and not (0x007fffff'u32 shr k))
    n = ((n and 0x007fffff'i32) or 0x00800000'i32) shr (23 - k)
    if j < 0: n = -n
    pH -= t
  t = pL + pH
  isBits = cast[int32](cast[uint32](t) and 0xffff8000'u32)
  t = cast[float32](cast[uint32](isBits))
  u = t * LG2_H_F32
  v = (pL - (t - pH)) * LG2_F32 + t * LG2_L_F32
  z = u + v
  w = v - (z - u)
  t = z * z
  t1 = z - t * (P1_F32 + t * (P2_F32 + t * (P3_F32 + t * (P4_F32 + t * P5_F32))))
  r = (z * t1) / (t1 - 2'f32) - (w + z * w)
  z = 1'f32 - (r - z)
  j = cast[int32](cast[uint32](z))
  j += n shl 23
  if (j shr 23) <= 0:
    z = powScale32(z, n)
  else:
    z = cast[float32](cast[uint32](j))
  sn * z

func powLowWord(x: float64): uint32 {.inline.} =
  cast[uint32](cast[uint64](x) and 0xffffffff'u64)

func powHighWord(x: float64): uint32 {.inline.} =
  cast[uint32](cast[uint64](x) shr 32)

func powWithLowWord(x: float64, low: uint32): float64 {.inline.} =
  cast[float64]((cast[uint64](x) and 0xffffffff00000000'u64) or uint64(low))

func powWithHighWord(x: float64, high: uint32): float64 {.inline.} =
  cast[float64]((cast[uint64](x) and 0xffffffff'u64) or (uint64(high) shl 32))

func powScale64(x: float64, n: int32): float64 {.inline.} =
  ## Scale by 2^n, including subnormal results, as fdlibm's scalbn does.
  if n < -1022:
    let scale = cast[float64]((n + 54 + 1023).uint64 shl 52)
    return (x * scale) * cast[float64](0x3c90000000000000'u64) # 2^-54
  if n > 1023:
    return (x * 2.0) * cast[float64](0x7fe0000000000000'u64) # 2^1023
  x * cast[float64]((n + 1023).uint64 shl 52)

func pow*(x, y: float64): float64 =
  ## Port of FreeBSD msun e_pow.c (fdlibm).
  var t1, t2: float64
  let hx = cast[int32](powHighWord(x))
  let lx = powLowWord(x)
  let hy = cast[int32](powHighWord(y))
  let ly = powLowWord(y)
  var ix = hx and 0x7fffffff'i32
  let iy = hy and 0x7fffffff'i32
  var yisint, k, j: int32

  if (cast[uint32](iy) or ly) == 0'u32: return 1.0
  if hx == 0x3ff00000'i32 and lx == 0'u32: return 1.0
  if ix > 0x7ff00000'i32 or (ix == 0x7ff00000'i32 and lx != 0'u32) or
     iy > 0x7ff00000'i32 or (iy == 0x7ff00000'i32 and ly != 0'u32):
    return x + y

  yisint = 0
  if hx < 0:
    if iy >= 0x43400000'i32:
      yisint = 2
    elif iy >= 0x3ff00000'i32:
      k = (iy shr 20) - 0x3ff
      if k > 20:
        j = cast[int32](ly shr (52 - k))
        if (j shl (52 - k)) == cast[int32](ly):
          yisint = 2 - (j and 1)
      elif ly == 0'u32:
        j = iy shr (20 - k)
        if (j shl (20 - k)) == iy:
          yisint = 2 - (j and 1)

  if ix == 0 and lx == 0'u32 and hx < 0:
    if hy < 0:
      return if yisint == 1: -Inf else: Inf
    return if yisint == 1: cast[float64](0x8000000000000000'u64) else: 0.0

  if ly == 0'u32:
    if iy == 0x7ff00000'i32:
      if ((ix - 0x3ff00000'i32) or cast[int32](lx)) == 0:
        return 1.0
      if ix >= 0x3ff00000'i32:
        return if hy >= 0: y else: 0.0
      return if hy >= 0: 0.0 else: -y
    if iy == 0x3ff00000'i32:
      return if hy >= 0: x else: 1.0 / x
    if hy == 0x40000000'i32:
      return x * x
    if hy == 0x3fe00000'i32 and hx >= 0:
      return sqrt(x)

  var ax = x.abs
  if lx == 0'u32 and (ix == 0x7ff00000'i32 or ix == 0 or ix == 0x3ff00000'i32):
    var z = ax
    if hy < 0: z = 1.0 / z
    if hx < 0:
      if ((ix - 0x3ff00000'i32) or yisint) == 0:
        z = (z - z) / (z - z)
      elif yisint == 1:
        z = cast[float64](cast[uint64](z) xor 0x8000000000000000'u64)
    return z

  var sn = 1.0
  if hx < 0:
    if yisint == 0: return (x - x) / (x - x)
    if yisint == 1: sn = -1.0

  if iy > 0x41e00000'i32:
    if iy > 0x43f00000'i32:
      if ix <= 0x3fefffff'i32:
        return if hy < 0: HUGE_F64 * HUGE_F64 else: TINY_F64 * TINY_F64
      if ix >= 0x3ff00000'i32:
        return if hy > 0: HUGE_F64 * HUGE_F64 else: TINY_F64 * TINY_F64
    if ix < 0x3fefffff'i32:
      return if hy < 0: sn * HUGE_F64 * HUGE_F64 else: sn * TINY_F64 * TINY_F64
    if ix > 0x3ff00000'i32:
      return if hy > 0: sn * HUGE_F64 * HUGE_F64 else: sn * TINY_F64 * TINY_F64
    let t = ax - 1.0
    let w = (t * t) * (0.5 - t * (0.3333333333333333333333 - t * 0.25))
    let u = IVLN2_H_F64 * t
    let v = t * IVLN2_L_F64 - w * IVLN2_F64
    t1 = powWithLowWord(u + v, 0'u32)
    t2 = v - (t1 - u)
  else:
    var n: int32 = 0
    if ix < 0x00100000'i32:
      ax *= TWO53
      n -= 53
      ix = cast[int32](powHighWord(ax))
    n += (ix shr 20) - 0x3ff
    j = ix and 0x000fffff'i32
    ix = j or 0x3ff00000'i32
    var interval: int32
    if j <= 0x3988e'i32:
      interval = 0
    elif j < 0xbb67a'i32:
      interval = 1
    else:
      interval = 0
      n += 1
      ix -= 0x00100000'i32
    ax = powWithHighWord(ax, cast[uint32](ix))

    let bp = BP_F64[interval]
    let u = ax - bp
    let v = 1.0 / (ax + bp)
    let ss = u * v
    let sH = powWithLowWord(ss, 0'u32)
    let tHbits = ((cast[uint32](ix) shr 1) or 0x20000000'u32) + 0x00080000'u32 + (cast[uint32](interval) shl 18)
    let tH = cast[float64](uint64(tHbits) shl 32)
    let tL = ax - (tH - bp)
    let sL = v * ((u - sH * tH) - sH * tL)
    let s2 = ss * ss
    var r = s2 * s2 * (L1_F64 + s2 * (L2_F64 + s2 * (L3_F64 + s2 * (L4_F64 + s2 * (L5_F64 + s2 * L6_F64)))))
    r += sL * (sH + ss)
    let s2h = sH * sH
    let tH2 = powWithLowWord(3.0 + s2h + r, 0'u32)
    let tL2 = r - ((tH2 - 3.0) - s2h)
    let u2 = sH * tH2
    let v2 = sL * tH2 + tL2 * ss
    let pH = powWithLowWord(u2 + v2, 0'u32)
    let pL = v2 - (pH - u2)
    let zH = CP_H_F64 * pH
    let zL = CP_L_F64 * pH + pL * CP_F64 + DP_L_F64[interval]
    let nf = float64(n)
    t1 = powWithLowWord(((zH + zL) + DP_H_F64[interval]) + nf, 0'u32)
    t2 = zL - (((t1 - nf) - DP_H_F64[interval]) - zH)

  let y1 = powWithLowWord(y, 0'u32)
  let pL = (y - y1) * t1 + y * t2
  var pH = y1 * t1
  let z = pL + pH
  var jWord = cast[int32](powHighWord(z))
  let iWord = cast[int32](powLowWord(z))

  if jWord >= 0x40900000'i32:
    if ((jWord - 0x40900000'i32) or iWord) != 0: return sn * HUGE_F64 * HUGE_F64
    if pL + OVT_F64 > z - pH: return sn * HUGE_F64 * HUGE_F64
  elif (jWord and 0x7fffffff'i32) >= 0x4090cc00'i32:
    if ((cast[uint32](jWord) - 0xc090cc00'u32) or cast[uint32](iWord)) != 0'u32:
      return sn * TINY_F64 * TINY_F64
    if pL <= z - pH: return sn * TINY_F64 * TINY_F64

  let iz = jWord and 0x7fffffff'i32
  k = (iz shr 20) - 0x3ff
  var n: int32 = 0
  if iz > 0x3fe00000'i32:
    n = jWord + (0x00100000'i32 shr (k + 1))
    k = ((n and 0x7fffffff'i32) shr 20) - 0x3ff
    let t = cast[float64](uint64(cast[uint32](n and not (0x000fffff'i32 shr k))) shl 32)
    n = ((n and 0x000fffff'i32) or 0x00100000'i32) shr (20 - k)
    if jWord < 0: n = -n
    pH -= t

  let t = powWithLowWord(pL + pH, 0'u32)
  let u = t * LG2_H_F64
  let v = (pL - (t - pH)) * LG2_F64 + t * LG2_L_F64
  var z2 = u + v
  let w = v - (z2 - u)
  let tSquare = z2 * z2
  let tPoly = z2 - tSquare * (P1_F64 + tSquare * (P2_F64 + tSquare * (P3_F64 + tSquare * (P4_F64 + tSquare * P5_F64))))
  let r = (z2 * tPoly) / (tPoly - 2.0) - (w + z2 * w)
  z2 = 1.0 - (r - z2)
  jWord = cast[int32](powHighWord(z2))
  jWord += n shl 20
  if (jWord shr 20) <= 0:
    z2 = powScale64(z2, n)
  else:
    z2 = powWithHighWord(z2, cast[uint32](jWord))
  sn * z2

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
  ## Euclidean distance (hypotenuse) for f32.
  if classify(x) in {fcInf, fcNegInf} or classify(y) in {fcInf, fcNegInf}:
    return float32(Inf)
  if x.isNaN or y.isNaN:
    return x + y

  let ax = float64(x.abs)
  let ay = float64(y.abs)
  let large = max(ax, ay)
  let small = min(ax, ay)
  if large == 0.0:
    return 0'f32
  let ratio = small / large
  float32(large * sqrtFinite64(1.0 + ratio * ratio))

func hypot*(x, y: float64): float64 =
  ## Euclidean distance (hypotenuse) for f64.
  if classify(x) in {fcInf, fcNegInf} or classify(y) in {fcInf, fcNegInf}:
    return Inf
  if x.isNaN or y.isNaN:
    return x + y

  let ax = x.abs
  let ay = y.abs
  let large = max(ax, ay)
  let small = min(ax, ay)
  if large == 0.0:
    return 0.0
  let ratio = small / large
  large * sqrtFinite64(1.0 + ratio * ratio)
