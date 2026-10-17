## Native (libc-free) implementations of exponential and logarithmic functions.
## `exp` follows FreeBSD/SunPro fdlibm's range reduction and polynomial; its
## exponent scaling is adapted to handle subnormals without constructing an
## invalid exponent field. The logarithm routines follow the corresponding
## fdlibm algorithms and coefficients, as carried by Rust compiler-builtins/libm.
## Original C sources: FreeBSD `/usr/src/lib/msun/src/{e_exp,e_expf,e_log,e_log2,e_log10,s_log1p}.c`.
## Rust references: `library/compiler-builtins/libm/src/math/{exp,expf,log,log2,log10,log1p,scalbn}.rs`.
## Copyright (C) 1993-2004 by Sun Microsystems, Inc. All rights reserved.
## Copyright (c) 2018 Jorge Aparicio (Rust compiler-builtins/libm port).
## Permission to use, copy, modify, and distribute this software is freely
## granted, provided that this notice is preserved.
import std/math/common
import std/math/frexp

# exp(x) - Exponential base e
# Returns the exponential of x.
#
# Method:
#   1. Argument reduction:
#      Reduce x to an r so that |r| <= 0.5*ln2 ~ 0.34658.
#      Given x, find r and integer k such that x = k*ln2 + r, |r| <= 0.5*ln2.
#
#   2. Approximation of exp(r) by a special rational function on [0,0.34658]:
#      Write R(r**2) = r*(exp(r)+1)/(exp(r)-1) = 2 + r*r/6 - r**4/360 + ...
#      We use a Remez algorithm to generate a polynomial of degree 5 to approximate R.
#      The computation of exp(r) becomes:
#         exp(r) = 1 + r*c(r)/(2 - c(r))
#      where c(r) = r - (P1*r^2 + P2*r^4 + ... + P5*r^10)
#
#   3. Scale back to obtain exp(x):
#      exp(x) = 2^k * exp(r)
#
# Special cases:
#      exp(INF) is INF, exp(NaN) is NaN;
#      exp(-INF) is 0, and for finite argument, only exp(0)=1 is exact.
#
# Accuracy: error is always less than 1 ulp (unit in the last place).

const EXP_F32_LN2_HI = 6.9314575195e-01'f32  # 0x3f317200
const EXP_F32_LN2_LO = 1.4286067653e-06'f32  # 0x35bfbe8e
const EXP_F32_INV_LN2 = 1.4426950216e+00'f32 # 0x3fb8aa3b
const EXP_F32_P1 = 1.6666625440e-1'f32       # 0xaaaa8f.0p-26
const EXP_F32_P2 = -2.7667332906e-3'f32      # -0xb55215.0p-32

const EXP_F64_LN2_HI = 6.93147180369123816490e-01  # 0x3fe62e42, 0xfee00000
const EXP_F64_LN2_LO = 1.90821492927058770002e-10  # 0x3dea39ef, 0x35793c76
const EXP_F64_INV_LN2 = 1.44269504088896338700e+00 # 0x3ff71547, 0x652b82fe
const EXP_F64_P1 = 1.66666666666666019037e-01      # 0x3FC55555, 0x5555553E
const EXP_F64_P2 = -2.77777777770155933842e-03     # 0xBF66C16C, 0x16BEBD93
const EXP_F64_P3 = 6.61375632143793436117e-05      # 0x3F11566A, 0xAF25DE2C
const EXP_F64_P4 = -1.65339022054652515390e-06     # 0xBEBBBD41, 0xC5D26BF1
const EXP_F64_P5 = 4.13813679705723846039e-08      # 0x3E663769, 0x72BEA4D0

# Scale by a power of two without constructing an exponent field that is
# subnormal (or negative). The extra multiply in the low-exponent path lets
# the final operation perform the correctly rounded subnormal conversion.
func expScale(y: float32, k: int32): float32 {.inline.} =
  if k < -126:
    let highScale = cast[float32]((k + 25 + 127).uint32 shl 23)
    return (y * highScale) * cast[float32](0x33000000'u32) # 2^-25
  if k > 127:
    return (y * 2'f32) * cast[float32](0x7f000000'u32) # 2 * 2^127
  let scale = cast[float32]((k + 127).uint32 shl 23)
  y * scale

func expScale(y: float64, k: int32): float64 {.inline.} =
  if k < -1022:
    let highScale = cast[float64]((k + 54 + 1023).uint64 shl 52)
    return (y * highScale) * cast[float64](0x3c90000000000000'u64) # 2^-54
  if k > 1023:
    return (y * 2.0) * cast[float64](0x7fe0000000000000'u64) # 2 * 2^1023
  let scale = cast[float64]((k + 1023).uint64 shl 52)
  y * scale

func exp*(x: float32): float32 =
  ## Exponential, base *e* (float32)
  ## Calculate the exponential of x, that is, e raised to the power x
  var x = x
  let x1p127 = cast[float32](0x7f000000'u32)  # 2^127
  var hx = cast[uint32](x)
  let sign = (hx shr 31).int32
  let signb = sign != 0
  hx = hx and 0x7fffffff'u32

  # special cases
  if hx >= 0x42aeac50'u32:
    if hx > 0x7f800000'u32:
      return x  # NaN
    if (hx >= 0x42b17218'u32) and not signb:
      return x * x1p127  # overflow: x >= 88.72
    if signb and hx >= 0x42cff1b5'u32:
      return 0'f32  # underflow: x <= -103.97

  let k: int32
  let hi: float32
  let lo: float32
  if hx > 0x3eb17218'u32:
    if hx > 0x3f851592'u32:
      k = int32(EXP_F32_INV_LN2 * x + (if signb: -0.5'f32 else: 0.5'f32))
    else:
      k = 1 - sign - sign
    let kf = float32(k)
    hi = x - kf * EXP_F32_LN2_HI
    lo = kf * EXP_F32_LN2_LO
    x = hi - lo
  elif hx > 0x39000000'u32:
    k = 0
    hi = x
    lo = 0'f32
  else:
    return 1'f32 + x

  let xx = x * x
  let c = x - xx * (EXP_F32_P1 + xx * EXP_F32_P2)
  let y = 1'f32 + (x * c / (2'f32 - c) - lo + hi)

  if k == 0: y else: expScale(y, k)

func exp*(x: float64): float64 =
  ## Exponential, base *e* (float64)
  ## Calculate the exponential of x, that is, e raised to the power x
  var x = x
  let ui = cast[uint64](x)
  let hx = (ui shr 32).uint32
  let ax = hx and 0x7fffffff'u32
  let lx = (ui and 0xffffffff'u64).uint32
  let signb = (ui shr 63) != 0

  if ax >= 0x40862e42'u32:
    if ax >= 0x7ff00000'u32:
      if (ax and 0x000fffff'u32) != 0 or lx != 0:
        return x + x # NaN
      return if signb: 0.0 else: x # +/- infinity
    if signb:
      if x <= -745.13321910194110842:
        return 0.0
    elif x > 709.782712893384:
      return Inf # overflow

  var k: int32
  if ax > 0x3fd62e42'u32:
    if ax < 0x3ff0a2b2'u32:
      k = 1 - 2 * int32(ui shr 63)
    else:
      k = int32(EXP_F64_INV_LN2 * x + (if signb: -0.5 else: 0.5))
  elif ax > 0x3e300000'u32:
    k = 0
  else:
    return 1.0 + x

  let kf = float64(k)
  let hi = x - kf * EXP_F64_LN2_HI
  let lo = kf * EXP_F64_LN2_LO
  let xx = hi - lo

  let t = xx * xx
  let c = xx - t * (EXP_F64_P1 + t * (EXP_F64_P2 + t * (EXP_F64_P3 +
               t * (EXP_F64_P4 + t * EXP_F64_P5))))
  let y = 1.0 + (xx * c / (2.0 - c) - lo + hi)

  if k == 0: y else: expScale(y, k)

# log(x) - Natural logarithm
# Return the logarithm of x
#
# Method :
#   1. Argument Reduction: find k and f such that x = 2^k * (1+f),
#      where sqrt(2)/2 < 1+f < sqrt(2).
#
#   2. Approximation of log(1+f).
#      Let s = f/(2+f); based on log(1+f) = log(1+s) - log(1-s)
#             = 2s + 2/3 s^3 + 2/5 s^5 + ...,
#             = 2s + s*R
#      We use Remez to generate a polynomial of degree 14 to approximate R.
#      The maximum error of this polynomial approximation is bounded by 2^-58.45.
#
#   3. Finally, log(x) = k*ln2 + log(1+f) = k*ln2_hi+(f-(hfsq-(s*(hfsq+R)+k*ln2_lo)))
#
# Special cases:
#      log(x) is NaN with signal if x < 0 (including -INF);
#      log(+INF) is +INF; log(0) is -INF with signal;
#      log(NaN) is that NaN with no signal.
#
# Accuracy: error is always less than 1 ulp (unit in the last place).

const LOG_F32_LN2_HI = 6.9314575195e-01'f32   # 0x3f317200
const LOG_F32_LN2_LO = 1.4286067653e-06'f32   # 0x35bfbe8e
const LOG_F32_LG1 = 0.66666662693f32
const LOG_F32_LG2 = 0.40000972152f32
const LOG_F32_LG3 = 0.28498786688f32
const LOG_F32_LG4 = 0.24279078841f32

const LOG_F64_LN2_HI = 6.93147180369123816490e-01  # 3fe62e42 fee00000
const LOG_F64_LN2_LO = 1.90821492927058770002e-10  # 3dea39ef 35793c76
const LOG_F64_LG1 = 6.666666666666735130e-01      # 3FE55555 55555593
const LOG_F64_LG2 = 3.999999999940941908e-01      # 3FD99999 9997FA04
const LOG_F64_LG3 = 2.857142874366239149e-01      # 3FD24924 94229359
const LOG_F64_LG4 = 2.222219843214978396e-01      # 3FCC71C5 1D8E78AF
const LOG_F64_LG5 = 1.818357216161805012e-01      # 3FC74664 96CB03DE
const LOG_F64_LG6 = 1.531383769920937332e-01      # 3FC39A09 D078C69F
const LOG_F64_LG7 = 1.479819860511658591e-01      # 3FC2F112 DF3E5244

func ln*(x: float32): float32 =
  ## The natural logarithm of x (float32).
  var x = x
  if isNaN(x):
    return x
  if x == 0'f32:
    return float32(-Inf)
  if x < 0'f32:
    return float32(NaN)
  if x == float32(Inf):
    return float32(Inf)
  if x == 1'f32:
    return 0'f32

  let x1p25 = cast[float32](0x4c000000'u32)  # 2^25
  var ui = cast[uint32](x)
  var hx = ui and 0x7fffffff'u32
  var k = 0'i32

  if hx < 0x00800000'u32:
    if (ui shl 1) == 0:
      return float32(-Inf)
    if (ui shr 31) != 0:
      return float32(NaN)
    k = k - 25
    x = x * x1p25
    ui = cast[uint32](x)
    hx = ui and 0x7fffffff'u32
  elif hx >= 0x7f800000'u32:
    return x
  elif hx == 0x3f800000'u32:
    return 0'f32

  hx = hx + 0x3f800000'u32 - 0x3f3504f3'u32
  k = k + ((hx shr 23).int32 - 0x7f)
  hx = (hx and 0x007fffff'u32) + 0x3f3504f3'u32
  ui = hx
  x = cast[float32](ui)

  let f = x - 1'f32
  let hfsq = 0.5'f32 * f * f
  let s = f / (2'f32 + f)
  let z = s * s
  let w = z * z
  let t1 = w * (LOG_F32_LG2 + w * LOG_F32_LG4)
  let t2 = z * (LOG_F32_LG1 + w * (LOG_F32_LG3))
  let r = t2 + t1
  let dk = float32(k)
  return s * (hfsq + r) + dk * LOG_F32_LN2_LO - hfsq + f + dk * LOG_F32_LN2_HI

func ln*(x: float64): float64 =
  ## The natural logarithm of x (float64).
  var x = x
  if isNaN(x):
    return x
  if x == 0.0:
    return -Inf
  if x < 0.0:
    return NaN
  if x == Inf:
    return Inf
  if x == 1.0:
    return 0.0

  let x1p54 = cast[float64](0x4350000000000000'u64)  # 2^54
  var ui = cast[uint64](x)
  var hx = (ui shr 32).uint32
  var k = 0'i32

  if hx < 0x00100000'u32:
    if (ui shl 1) == 0:
      return -Inf
    if (hx shr 31) != 0:
      return NaN
    k = k - 54
    x = x * x1p54
    ui = cast[uint64](x)
    hx = (ui shr 32).uint32
  elif hx >= 0x7ff00000'u32:
    return x
  elif hx == 0x3ff00000'u32 and (ui and 0xffffffff'u64) == 0:
    return 0.0

  hx = hx + 0x3ff00000'u32 - 0x3fe6a09e'u32
  k = k + ((hx shr 20).int32 - 0x3ff)
  hx = (hx and 0x000fffff'u32) + 0x3fe6a09e'u32
  ui = ((hx.uint64) shl 32) or (ui and 0xffffffff'u64)
  x = cast[float64](ui)

  let f = x - 1.0
  let hfsq = 0.5 * f * f
  let s = f / (2.0 + f)
  let z = s * s
  let w = z * z
  let t1 = w * (LOG_F64_LG2 + w * (LOG_F64_LG4 + w * LOG_F64_LG6))
  let t2 = z * (LOG_F64_LG1 + w * (LOG_F64_LG3 + w * (LOG_F64_LG5 + w * LOG_F64_LG7)))
  let r = t2 + t1
  let dk = float64(k)
  return s * (hfsq + r) + dk * LOG_F64_LN2_LO - hfsq + f + dk * LOG_F64_LN2_HI

# log2(x) = ln(x) / ln(2)
# Return the base 2 logarithm of x.
# Reduces x to 2^k (1+f) and calculates r = log(1+f) - f + f*f/2
# as in log.c, then combines and scales in extra precision:
#    log2(x) = (f - f*f/2 + r)/log(2) + k

const LOG2_F32_IVLN2HI = 1.4428710938e+00'f32 # 0x3fb8b000
const LOG2_F32_IVLN2LO = -1.7605285393e-04'f32 # 0xb9389ad4
const LOG2_F32_LG1 = 0.66666662693f32
const LOG2_F32_LG2 = 0.40000972152f32
const LOG2_F32_LG3 = 0.28498786688f32
const LOG2_F32_LG4 = 0.24279078841f32

const LOG2_F64_IVLN2HI = 1.44269504072144627571e+00 # 0x3ff71547, 0x65200000
const LOG2_F64_IVLN2LO = 1.67517131648865118353e-10 # 0x3de705fc, 0x2eefa200
const LOG2_F64_LG1 = 6.666666666666735130e-01      # 3FE55555 55555593
const LOG2_F64_LG2 = 3.999999999940941908e-01      # 3FD99999 9997FA04
const LOG2_F64_LG3 = 2.857142874366239149e-01      # 3FD24924 94229359
const LOG2_F64_LG4 = 2.222219843214978396e-01      # 3FCC71C5 1D8E78AF
const LOG2_F64_LG5 = 1.818357216161805012e-01      # 3FC74664 96CB03DE
const LOG2_F64_LG6 = 1.531383769920937332e-01      # 3FC39A09 D078C69F
const LOG2_F64_LG7 = 1.479819860511658591e-01      # 3FC2F112 DF3E5244

func log2*(x: float32): float32 =
  ## The base-2 logarithm of x (float32).
  var x = x
  let x1p25f = cast[float32](0x4c000000'u32)  # 2^25

  var ui = cast[uint32](x)
  var ix = ui
  var k = 0'i32

  if ix < 0x00800000'u32 or (ix shr 31) > 0:
    if (ix shl 1) == 0:
      return -1'f32 / (x * x)  # log(+-0)=-inf
    if (ix shr 31) > 0:
      return (x - x) / 0'f32  # log(-#) = NaN
    k = k - 25
    x = x * x1p25f
    ui = cast[uint32](x)
    ix = ui
  elif ix >= 0x7f800000'u32:
    return x
  elif ix == 0x3f800000'u32:
    return 0'f32

  ix = ix + 0x3f800000'u32 - 0x3f3504f3'u32
  k = k + ((ix shr 23).int32 - 0x7f)
  ix = (ix and 0x007fffff'u32) + 0x3f3504f3'u32
  ui = ix
  x = cast[float32](ui)

  let f = x - 1'f32
  let s = f / (2'f32 + f)
  let z = s * s
  let w = z * z
  let t1 = w * (LOG2_F32_LG2 + w * LOG2_F32_LG4)
  let t2 = z * (LOG2_F32_LG1 + w * (LOG2_F32_LG3))
  let r = t2 + t1
  let hfsq = 0.5'f32 * f * f

  var hi = f - hfsq
  ui = cast[uint32](hi)
  ui = ui and 0xfffff000'u32
  hi = cast[float32](ui)
  let lo = f - hi - hfsq + s * (hfsq + r)
  let dk = float32(k)
  return (lo + hi) * LOG2_F32_IVLN2LO + lo * LOG2_F32_IVLN2HI + hi * LOG2_F32_IVLN2HI + dk

func log2*(x: float64): float64 =
  ## The base-2 logarithm of x (float64).
  var x = x
  let x1p54 = cast[float64](0x4350000000000000'u64)   # 2^54

  var ui = cast[uint64](x)
  var hx = (ui shr 32).uint32
  var k = 0'i32

  if hx < 0x00100000'u32 or (hx shr 31) > 0:
    if (ui shl 1) == 0:
      return -1.0 / (x * x)  # log(+-0)=-inf
    if (hx shr 31) > 0:
      return (x - x) / 0.0  # log(-#) = NaN
    k = k - 54
    x = x * x1p54
    ui = cast[uint64](x)
    hx = (ui shr 32).uint32
  elif hx >= 0x7ff00000'u32:
    return x
  elif hx == 0x3ff00000'u32 and (ui shl 32) == 0:
    return 0.0

  hx = hx + 0x3ff00000'u32 - 0x3fe6a09e'u32
  k = k + ((hx shr 20).int32 - 0x3ff)
  hx = (hx and 0x000fffff'u32) + 0x3fe6a09e'u32
  ui = ((hx.uint64) shl 32) or (ui and 0xffffffff'u64)
  x = cast[float64](ui)

  let f = x - 1.0
  let s = f / (2.0 + f)
  let z = s * s
  let w = z * z
  let t1 = w * (LOG2_F64_LG2 + w * (LOG2_F64_LG4 + w * LOG2_F64_LG6))
  let t2 = z * (LOG2_F64_LG1 + w * (LOG2_F64_LG3 + w * (LOG2_F64_LG5 + w * LOG2_F64_LG7)))
  let r = t2 + t1

  var hi = f - 0.5 * f * f
  ui = cast[uint64](hi)
  ui = ui and (0xffffffff'i64 shl 32).uint64
  hi = cast[float64](ui)
  let lo = f - hi - 0.5 * f * f + s * (0.5 * f * f + r)

  let y = float64(k)
  var w_val = y + hi * LOG2_F64_IVLN2HI
  let val_lo = (y - w_val) + hi * LOG2_F64_IVLN2HI + (lo + hi) * LOG2_F64_IVLN2LO
  return val_lo + w_val

# log10(x) = ln(x) / ln(10)
# Return the base 10 logarithm of x.
# Reduces x to 2^k (1+f) and calculates r = log(1+f) - f + f*f/2
# as in log.c, then combines and scales in extra precision:
#    log10(x) = (f - f*f/2 + r)/log(10) + k*log10(2)

const LOG10_F32_IVLN10HI = 4.3432617188e-01'f32  # 0x3ede6000
const LOG10_F32_IVLN10LO = -3.1689971365e-05'f32 # 0xb804ead9
const LOG10_F32_LOG10_2HI = 3.0102920532e-01'f32 # 0x3e9a2080
const LOG10_F32_LOG10_2LO = 7.9034151668e-07'f32 # 0x355427db
const LOG10_F32_LG1 = 0.66666662693f32
const LOG10_F32_LG2 = 0.40000972152f32
const LOG10_F32_LG3 = 0.28498786688f32
const LOG10_F32_LG4 = 0.24279078841f32

const LOG10_F64_IVLN10HI = 4.34294481878168880939e-01 # 0x3fdbcb7b, 0x15200000
const LOG10_F64_IVLN10LO = 2.50829467116452752298e-11 # 0x3dbb9438, 0xca9aadd5
const LOG10_F64_LOG10_2HI = 3.01029995663611771306e-01 # 0x3FD34413, 0x509F6000
const LOG10_F64_LOG10_2LO = 3.69423907715893078616e-13 # 0x3D59FEF3, 0x11F12B36
const LOG10_F64_LG1 = 6.666666666666735130e-01      # 3FE55555 55555593
const LOG10_F64_LG2 = 3.999999999940941908e-01      # 3FD99999 9997FA04
const LOG10_F64_LG3 = 2.857142874366239149e-01      # 3FD24924 94229359
const LOG10_F64_LG4 = 2.222219843214978396e-01      # 3FCC71C5 1D8E78AF
const LOG10_F64_LG5 = 1.818357216161805012e-01      # 3FC74664 96CB03DE
const LOG10_F64_LG6 = 1.531383769920937332e-01      # 3FC39A09 D078C69F
const LOG10_F64_LG7 = 1.479819860511658591e-01      # 3FC2F112 DF3E5244

func log10*(x: float32): float32 =
  ## The base-10 logarithm of x (float32).
  var x = x
  let x1p25f = cast[float32](0x4c000000'u32)  # 2^25

  var ui = cast[uint32](x)
  var ix = ui
  var k = 0'i32

  if ix < 0x00800000'u32 or (ix shr 31) > 0:
    if (ix shl 1) == 0:
      return -1'f32 / (x * x)  # log(+-0)=-inf
    if (ix shr 31) > 0:
      return (x - x) / 0'f32  # log(-#) = NaN
    k = k - 25
    x = x * x1p25f
    ui = cast[uint32](x)
    ix = ui
  elif ix >= 0x7f800000'u32:
    return x
  elif ix == 0x3f800000'u32:
    return 0'f32

  ix = ix + 0x3f800000'u32 - 0x3f3504f3'u32
  k = k + ((ix shr 23).int32 - 0x7f)
  ix = (ix and 0x007fffff'u32) + 0x3f3504f3'u32
  ui = ix
  x = cast[float32](ui)

  let f = x - 1'f32
  let s = f / (2'f32 + f)
  let z = s * s
  let w = z * z
  let t1 = w * (LOG10_F32_LG2 + w * LOG10_F32_LG4)
  let t2 = z * (LOG10_F32_LG1 + w * (LOG10_F32_LG3))
  let r = t2 + t1
  let hfsq = 0.5'f32 * f * f

  var hi = f - hfsq
  ui = cast[uint32](hi)
  ui = ui and 0xfffff000'u32
  hi = cast[float32](ui)
  let lo = f - hi - hfsq + s * (hfsq + r)
  let dk = float32(k)
  return dk * LOG10_F32_LOG10_2LO + (lo + hi) * LOG10_F32_IVLN10LO + lo * LOG10_F32_IVLN10HI + hi * LOG10_F32_IVLN10HI + dk * LOG10_F32_LOG10_2HI

func log10*(x: float64): float64 =
  ## The base-10 logarithm of x (float64).
  var x = x
  let x1p54 = cast[float64](0x4350000000000000'u64)   # 2^54

  var ui = cast[uint64](x)
  var hx = (ui shr 32).uint32
  var k = 0'i32

  if hx < 0x00100000'u32 or (hx shr 31) > 0:
    if (ui shl 1) == 0:
      return -1.0 / (x * x)  # log(+-0)=-inf
    if (hx shr 31) > 0:
      return (x - x) / 0.0  # log(-#) = NaN
    k = k - 54
    x = x * x1p54
    ui = cast[uint64](x)
    hx = (ui shr 32).uint32
  elif hx >= 0x7ff00000'u32:
    return x
  elif hx == 0x3ff00000'u32 and (ui shl 32) == 0:
    return 0.0

  hx = hx + 0x3ff00000'u32 - 0x3fe6a09e'u32
  k = k + ((hx shr 20).int32 - 0x3ff)
  hx = (hx and 0x000fffff'u32) + 0x3fe6a09e'u32
  ui = ((hx.uint64) shl 32) or (ui and 0xffffffff'u64)
  x = cast[float64](ui)

  let f = x - 1.0
  let hfsq = 0.5 * f * f
  let s = f / (2.0 + f)
  let z = s * s
  let w = z * z
  let t1 = w * (LOG10_F64_LG2 + w * (LOG10_F64_LG4 + w * LOG10_F64_LG6))
  let t2 = z * (LOG10_F64_LG1 + w * (LOG10_F64_LG3 + w * (LOG10_F64_LG5 + w * LOG10_F64_LG7)))
  let r = t2 + t1

  var hi = f - hfsq
  ui = cast[uint64](hi)
  ui = ui and (0xffffffff'i64 shl 32).uint64
  hi = cast[float64](ui)
  let lo = f - hi - hfsq + s * (hfsq + r)

  var val_hi = hi * LOG10_F64_IVLN10HI
  let dk = float64(k)
  let y = dk * LOG10_F64_LOG10_2HI
  var val_lo = dk * LOG10_F64_LOG10_2LO + (lo + hi) * LOG10_F64_IVLN10LO + lo * LOG10_F64_IVLN10HI

  var w_val = y + val_hi
  val_lo = val_lo + (y - w_val) + val_hi
  val_hi = w_val

  return val_lo + val_hi

# log1p(x) = ln(1 + x) - accurate for small x
# Return the natural logarithm of 1+x.
#
# Method :
#   1. Argument Reduction: find k and f such that
#                      1+x = 2^k * (1+f),
#      where sqrt(2)/2 < 1+f < sqrt(2).
#
#      Note. If k=0, then f=x is exact. However, if k!=0, then f
#      may not be representable exactly. In that case, a correction
#      term is needed. Let u=1+x rounded. Let c = (1+x)-u, then
#      log(1+x) - log(u) ~ c/u. Thus, we proceed to compute log(u),
#      and add back the correction term c/u.
#
#   2. Approximation of log(1+f): See log.c
#
#   3. Finally, log1p(x) = k*ln2 + log(1+f) + c/u. See log.c
#
# Special cases:
#      log1p(x) is NaN with signal if x < -1 (including -INF);
#      log1p(+INF) is +INF; log1p(-1) is -INF with signal;
#      log1p(NaN) is that NaN with no signal.

const LOG1P_F32_LN2_HI = 6.9313812256e-01'f32  # 0x3f317180
const LOG1P_F32_LN2_LO = 9.0580006145e-06'f32  # 0x3717f7d1
const LOG1P_F32_LG1 = 0.66666662693f32
const LOG1P_F32_LG2 = 0.40000972152f32
const LOG1P_F32_LG3 = 0.28498786688f32
const LOG1P_F32_LG4 = 0.24279078841f32

const LOG1P_F64_LN2_HI = 6.93147180369123816490e-01  # 3fe62e42 fee00000
const LOG1P_F64_LN2_LO = 1.90821492927058770002e-10  # 3dea39ef 35793c76
const LOG1P_F64_LG1 = 6.666666666666735130e-01      # 3FE55555 55555593
const LOG1P_F64_LG2 = 3.999999999940941908e-01      # 3FD99999 9997FA04
const LOG1P_F64_LG3 = 2.857142874366239149e-01      # 3FD24924 94229359
const LOG1P_F64_LG4 = 2.222219843214978396e-01      # 3FCC71C5 1D8E78AF
const LOG1P_F64_LG5 = 1.818357216161805012e-01      # 3FC74664 96CB03DE
const LOG1P_F64_LG6 = 1.531383769920937332e-01      # 3FC39A09 D078C69F
const LOG1P_F64_LG7 = 1.479819860511658591e-01      # 3FC2F112 DF3E5244

func log1p*(x: float32): float32 =
  ## Computes ln(1 + x) accurately for small x (float32).
  var x = x
  var ui = cast[uint32](x)
  var ix = ui
  var k = 1'i32

  if ix < 0x3ed413d0'u32 or (ix shr 31) > 0:
    if ix >= 0xbf800000'u32:
      if x == -1'f32:
        return x / 0'f32  # log1p(-1)=+inf
      return (x - x) / 0'f32  # log1p(x<-1)=NaN
    if (ix shl 1) < (0x33800000'u32 shl 1):
      return x  # |x| < 2**-24
    if ix <= 0xbe95f619'u32:
      k = 0
  elif ix >= 0x7f800000'u32:
    return x

  var f = 0'f32
  var c = 0'f32
  if k > 0:
    ui = cast[uint32](1'f32 + x)
    var iu = ui
    iu = iu + 0x3f800000'u32 - 0x3f3504f3'u32
    k = ((iu shr 23).int32 - 0x7f)
    if k < 25:
      c = if k >= 2:
            1'f32 - (cast[float32](ui) - x)
          else:
            x - (cast[float32](ui) - 1'f32)
      c = c / cast[float32](ui)
    else:
      c = 0'f32
    iu = (iu and 0x007fffff'u32) + 0x3f3504f3'u32
    ui = iu
    f = cast[float32](ui) - 1'f32

  let s = f / (2'f32 + f)
  let z = s * s
  let w = z * z
  let t1 = w * (LOG1P_F32_LG2 + w * LOG1P_F32_LG4)
  let t2 = z * (LOG1P_F32_LG1 + w * (LOG1P_F32_LG3))
  let r = t2 + t1
  let hfsq = 0.5'f32 * f * f
  let dk = float32(k)
  return s * (hfsq + r) + (dk * LOG1P_F32_LN2_LO + c) - hfsq + f + dk * LOG1P_F32_LN2_HI

func log1p*(x: float64): float64 =
  ## Computes ln(1 + x) accurately for small x (float64).
  var x = x
  var ui = cast[uint64](x)
  var hx = (ui shr 32).uint32
  var k = 1'i32

  if hx < 0x3fda827a'u32 or (hx shr 31) > 0:
    if hx >= 0xbff00000'u32:
      if x == -1.0:
        return x / 0.0  # log1p(-1) = -inf
      return (x - x) / 0.0  # log1p(x<-1) = NaN
    if (hx shl 1) < (0x3ca00000'u32 shl 1):
      return x  # |x| < 2**-53
    if hx <= 0xbfd2bec4'u32:
      k = 0
  elif hx >= 0x7ff00000'u32:
    return x

  var f = 0.0
  var c = 0.0
  if k > 0:
    ui = cast[uint64](1.0 + x)
    var hu = (ui shr 32).uint32
    hu = hu + 0x3ff00000'u32 - 0x3fe6a09e'u32
    k = ((hu shr 20).int32 - 0x3ff)
    if k < 54:
      c = if k >= 2:
            1.0 - (cast[float64](ui) - x)
          else:
            x - (cast[float64](ui) - 1.0)
      c = c / cast[float64](ui)
    else:
      c = 0.0
    hu = (hu and 0x000fffff'u32) + 0x3fe6a09e'u32
    ui = ((hu.uint64) shl 32) or (ui and 0xffffffff'u64)
    f = cast[float64](ui) - 1.0

  let hfsq = 0.5 * f * f
  let s = f / (2.0 + f)
  let z = s * s
  let w = z * z
  let t1 = w * (LOG1P_F64_LG2 + w * (LOG1P_F64_LG4 + w * LOG1P_F64_LG6))
  let t2 = z * (LOG1P_F64_LG1 + w * (LOG1P_F64_LG3 + w * (LOG1P_F64_LG5 + w * LOG1P_F64_LG7)))
  let r = t2 + t1
  let dk = float64(k)
  return s * (hfsq + r) + (dk * LOG1P_F64_LN2_LO + c) - hfsq + f + dk * LOG1P_F64_LN2_HI
