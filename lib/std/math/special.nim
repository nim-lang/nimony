## Native (libc-free) implementations of error and gamma functions.
## Implementations based on FreeBSD/SunPro libm and Rust's compiler-builtins.
##
## origin: FreeBSD /usr/src/lib/msun/src/s_erf.c, s_erff.c, e_lgamma_r.c, e_lgammaf_r.c
## origin: FreeBSD /usr/src/lib/msun/src/e_tgamma.c, e_tgammaf.c
## Copyright (C) 1993-2004 by Sun Microsystems, Inc. All rights reserved.
## Permission to use, copy, modify, and distribute this
## software is freely granted, provided that this notice is preserved.
##

import std/math/common
import std/math/exponential
import std/math/power
import std/math/rounding
import std/math/trig

# Helper constants and functions for bit manipulation
{.push inline.}
func getHighWord(x: float64): uint32 =
  ## Get the high word (upper 32 bits) of a float64
  let bits = cast[uint64](x)
  ((bits shr 32) and 0xffffffff'u64).uint32

func setLowWord(x: float64, low: uint32): float64 =
  ## Set the low word (lower 32 bits) of a float64, keeping the high word
  let bits = cast[uint64](x)
  let high = (bits and 0xffffffff00000000'u64)
  cast[float64](high or (low.uint64 and 0xffffffff'u64))

func absFloat64(x: float64): float64 =
  if x < 0.0: -x else: x

func absFloat32(x: float32): float32 =
  if x < 0.0: -x else: x
{.pop.}

# ============================================================================
# ERROR FUNCTION (erf, erfc) - f64 version
# ============================================================================
# origin: FreeBSD /usr/src/lib/msun/src/s_erf.c
#
# Coefficients and approximation methods from SunPro

const ERX: float64 = 8.45062911510467529297e-01
const EFX8: float64 = 1.02703333676410069053e+00
const PP0: float64 = 1.28379167095512558561e-01
const PP1: float64 = -3.25042107247001499370e-01
const PP2: float64 = -2.84817495755985104766e-02
const PP3: float64 = -5.77027029648944159157e-03
const PP4: float64 = -2.37630166566501626084e-05
const QQ1: float64 = 3.97917223959155352819e-01
const QQ2: float64 = 6.50222499887672944485e-02
const QQ3: float64 = 5.08130628187576562776e-03
const QQ4: float64 = 1.32494738004321644526e-04
const QQ5: float64 = -3.96022827877536812320e-06
const PA0: float64 = -2.36211856075265944077e-03
const PA1: float64 = 4.14856118683748331666e-01
const PA2: float64 = -3.72207876035701323847e-01
const PA3: float64 = 3.18346619901161753674e-01
const PA4: float64 = -1.10894694282396677476e-01
const PA5: float64 = 3.54783043256182359371e-02
const PA6: float64 = -2.16637559486879084300e-03
const QA1: float64 = 1.06420880400844228286e-01
const QA2: float64 = 5.40397917702171048937e-01
const QA3: float64 = 7.18286544141962662868e-02
const QA4: float64 = 1.26171219808761642112e-01
const QA5: float64 = 1.36370839120290507362e-02
const QA6: float64 = 1.19844998467991074170e-02
const RA0: float64 = -9.86494403484714822705e-03
const RA1: float64 = -6.93858572707181764372e-01
const RA2: float64 = -1.05586262253232909814e+01
const RA3: float64 = -6.23753324503260060396e+01
const RA4: float64 = -1.62396669462573470355e+02
const RA5: float64 = -1.84605092906711035994e+02
const RA6: float64 = -8.12874355063065934246e+01
const RA7: float64 = -9.81432934416914548592e+00
const SA1: float64 = 1.96512716674392571292e+01
const SA2: float64 = 1.37657754143519042600e+02
const SA3: float64 = 4.34565877475229228821e+02
const SA4: float64 = 6.45387271733267880336e+02
const SA5: float64 = 4.29008140027567833386e+02
const SA6: float64 = 1.08635005541779435134e+02
const SA7: float64 = 6.57024977031928170135e+00
const SA8: float64 = -6.04244152148580987438e-02
const RB0: float64 = -9.86494292470009928597e-03
const RB1: float64 = -7.99283237680523006574e-01
const RB2: float64 = -1.77579549177547519889e+01
const RB3: float64 = -1.60636384855821916062e+02
const RB4: float64 = -6.37566443368389627722e+02
const RB5: float64 = -1.02509513161107724954e+03
const RB6: float64 = -4.83519191608651397019e+02
const SB1: float64 = 3.03380607434824582924e+01
const SB2: float64 = 3.25792512996573918826e+02
const SB3: float64 = 1.53672958608443695994e+03
const SB4: float64 = 3.19985821950859553908e+03
const SB5: float64 = 2.55305040643316442583e+03
const SB6: float64 = 4.74528541206955367215e+02
const SB7: float64 = -2.24409524465858183362e+01

func erfc1_f64(x: float64): float64 =
  let s = absFloat64(x) - 1.0
  let p = PA0 + s * (PA1 + s * (PA2 + s * (PA3 + s * (PA4 + s * (PA5 + s * PA6)))))
  let q = 1.0 + s * (QA1 + s * (QA2 + s * (QA3 + s * (QA4 + s * (QA5 + s * QA6)))))
  1.0 - ERX - p / q

func erfc2_f64(ix: uint32, x: float64): float64 =
  var x_abs = absFloat64(x)
  let s = 1.0 / (x_abs * x_abs)
  var r: float64
  var big_s: float64

  if ix < 0x3ff40000u32:
    return erfc1_f64(x)

  if ix < 0x4006db6du32:
    r = RA0 + s * (RA1 + s * (RA2 + s * (RA3 + s * (RA4 + s * (RA5 + s * (RA6 + s * RA7))))))
    big_s = 1.0 + s * (SA1 + s * (SA2 + s * (SA3 + s * (SA4 + s * (SA5 + s * (SA6 + s * (SA7 + s * SA8)))))))
  else:
    r = RB0 + s * (RB1 + s * (RB2 + s * (RB3 + s * (RB4 + s * (RB5 + s * RB6)))))
    big_s = 1.0 + s * (SB1 + s * (SB2 + s * (SB3 + s * (SB4 + s * (SB5 + s * (SB6 + s * SB7))))))

  let z = setLowWord(x_abs, 0u32)
  exp(-z * z - 0.5625) * exp((z - x_abs) * (z + x_abs) + r / big_s) / x_abs

## Error function (f64)
##
## Calculates an approximation to the "error function", which estimates
## the probability that an observation will fall within x standard
## deviations of the mean (assuming a normal distribution).
func erf*(x: float64): float64 =
  let u = cast[uint64](x)
  let sign = (u shr 63).int
  var ix = getHighWord(x)
  ix = ix and 0x7fffffff

  if ix >= 0x7ff00000u32:
    return 1.0 - 2.0 * float64(sign) + 1.0 / x

  if ix < 0x3feb0000u32:
    if ix < 0x3e300000u32:
      return 0.125 * (8.0 * x + EFX8 * x)
    let z = x * x
    let r = PP0 + z * (PP1 + z * (PP2 + z * (PP3 + z * PP4)))
    let s_val = 1.0 + z * (QQ1 + z * (QQ2 + z * (QQ3 + z * (QQ4 + z * QQ5))))
    let y = r / s_val
    return x + x * y

  var y: float64
  if ix < 0x40180000u32:
    y = 1.0 - erfc2_f64(ix, x)
  else:
    let x1p_1022 = cast[float64](0x0010000000000000u64)
    y = 1.0 - x1p_1022

  if sign != 0: -y else: y

## Complementary error function (f64)
##
## Calculates the complementary probability.
## Is `1 - erf(x)`. Is computed directly, so that you can use it to avoid
## the loss of precision that would result from subtracting
## large probabilities (on large `x`) from 1.
func erfc*(x: float64): float64 =
  let u = cast[uint64](x)
  let sign = (u shr 63).int
  var ix = getHighWord(x)
  ix = ix and 0x7fffffff

  if ix >= 0x7ff00000u32:
    return 2.0 * float64(sign) + 1.0 / x

  if ix < 0x3feb0000u32:
    if ix < 0x3c700000u32:
      return 1.0 - x
    let z = x * x
    let r = PP0 + z * (PP1 + z * (PP2 + z * (PP3 + z * PP4)))
    let s_val = 1.0 + z * (QQ1 + z * (QQ2 + z * (QQ3 + z * (QQ4 + z * QQ5))))
    let y = r / s_val
    if sign != 0 or ix < 0x3fd00000u32:
      return 1.0 - (x + x * y)
    return 0.5 - (x - 0.5 + x * y)

  if ix < 0x403c0000u32:
    if sign != 0:
      return 2.0 - erfc2_f64(ix, x)
    else:
      return erfc2_f64(ix, x)

  let x1p_1022 = cast[float64](0x0010000000000000u64)
  if sign != 0:
    2.0 - x1p_1022
  else:
    x1p_1022 * x1p_1022

# ============================================================================
# ERROR FUNCTION (erf, erfc) - f32 version
# ============================================================================
# origin: FreeBSD /usr/src/lib/msun/src/s_erff.c

const ERXF: float32 = 8.4506291151e-01'f32
const EFX8F: float32 = 1.0270333290e+00'f32
const PP0F: float32 = 1.2837916613e-01'f32
const PP1F: float32 = -3.2504209876e-01'f32
const PP2F: float32 = -2.8481749818e-02'f32
const PP3F: float32 = -5.7702702470e-03'f32
const PP4F: float32 = -2.3763017452e-05'f32
const QQ1F: float32 = 3.9791721106e-01'f32
const QQ2F: float32 = 6.5022252500e-02'f32
const QQ3F: float32 = 5.0813062117e-03'f32
const QQ4F: float32 = 1.3249473704e-04'f32
const QQ5F: float32 = -3.9602282413e-06'f32
const PA0F: float32 = -2.3621185683e-03'f32
const PA1F: float32 = 4.1485610604e-01'f32
const PA2F: float32 = -3.7220788002e-01'f32
const PA3F: float32 = 3.1834661961e-01'f32
const PA4F: float32 = -1.1089469492e-01'f32
const PA5F: float32 = 3.5478305072e-02'f32
const PA6F: float32 = -2.1663755178e-03'f32
const QA1F: float32 = 1.0642088205e-01'f32
const QA2F: float32 = 5.4039794207e-01'f32
const QA3F: float32 = 7.1828655899e-02'f32
const QA4F: float32 = 1.2617121637e-01'f32
const QA5F: float32 = 1.3637083583e-02'f32
const QA6F: float32 = 1.1984500103e-02'f32
const RA0F: float32 = -9.8649440333e-03'f32
const RA1F: float32 = -6.9385856390e-01'f32
const RA2F: float32 = -1.0558626175e+01'f32
const RA3F: float32 = -6.2375331879e+01'f32
const RA4F: float32 = -1.6239666748e+02'f32
const RA5F: float32 = -1.8460508728e+02'f32
const RA6F: float32 = -8.1287437439e+01'f32
const RA7F: float32 = -9.8143291473e+00'f32
const SA1F: float32 = 1.9651271820e+01'f32
const SA2F: float32 = 1.3765776062e+02'f32
const SA3F: float32 = 4.3456588745e+02'f32
const SA4F: float32 = 6.4538726807e+02'f32
const SA5F: float32 = 4.2900814819e+02'f32
const SA6F: float32 = 1.0863500214e+02'f32
const SA7F: float32 = 6.5702495575e+00'f32
const SA8F: float32 = -6.0424413532e-02'f32
const RB0F: float32 = -9.8649431020e-03'f32
const RB1F: float32 = -7.9928326607e-01'f32
const RB2F: float32 = -1.7757955551e+01'f32
const RB3F: float32 = -1.6063638306e+02'f32
const RB4F: float32 = -6.3756646729e+02'f32
const RB5F: float32 = -1.0250950928e+03'f32
const RB6F: float32 = -4.8351919556e+02'f32
const SB1F: float32 = 3.0338060379e+01'f32
const SB2F: float32 = 3.2579251099e+02'f32
const SB3F: float32 = 1.5367296143e+03'f32
const SB4F: float32 = 3.1998581543e+03'f32
const SB5F: float32 = 2.5530502930e+03'f32
const SB6F: float32 = 4.7452853394e+02'f32
const SB7F: float32 = -2.2440952301e+01'f32

func erfc1_f32(x: float32): float32 =
  let s = absFloat32(x) - 1.0'f32
  let p = PA0F + s * (PA1F + s * (PA2F + s * (PA3F + s * (PA4F + s * (PA5F + s * PA6F)))))
  let q = 1.0'f32 + s * (QA1F + s * (QA2F + s * (QA3F + s * (QA4F + s * (QA5F + s * QA6F)))))
  1.0'f32 - ERXF - p / q

func erfc2_f32(ix: uint32, x: float32): float32 =
  var x_abs = absFloat32(x)
  let s = 1.0'f32 / (x_abs * x_abs)
  var r: float32
  var big_s: float32

  if ix < 0x3fa00000u32:
    return erfc1_f32(x)

  if ix < 0x4036db6du32:
    r = RA0F + s * (RA1F + s * (RA2F + s * (RA3F + s * (RA4F + s * (RA5F + s * (RA6F + s * RA7F))))))
    big_s = 1.0'f32 + s * (SA1F + s * (SA2F + s * (SA3F + s * (SA4F + s * (SA5F + s * (SA6F + s * (SA7F + s * SA8F)))))))
  else:
    r = RB0F + s * (RB1F + s * (RB2F + s * (RB3F + s * (RB4F + s * (RB5F + s * RB6F)))))
    big_s = 1.0'f32 + s * (SB1F + s * (SB2F + s * (SB3F + s * (SB4F + s * (SB5F + s * (SB6F + s * SB7F))))))

  let ix_bits = cast[uint32](x_abs)
  let z = cast[float32](ix_bits and 0xffffe000u32)

  exp(-z * z - 0.5625'f32) * exp((z - x_abs) * (z + x_abs) + r / big_s) / x_abs

## Error function (f32)
##
## Calculates an approximation to the "error function", which estimates
## the probability that an observation will fall within x standard
## deviations of the mean (assuming a normal distribution).
func erf*(x: float32): float32 =
  let ix_bits = cast[uint32](x)
  let sign = (ix_bits shr 31).int
  var ix = ix_bits and 0x7fffffff

  if ix >= 0x7f800000u32:
    return 1.0'f32 - 2.0'f32 * float32(sign) + 1.0'f32 / x

  if ix < 0x3f580000u32:
    if ix < 0x31800000u32:
      return 0.125'f32 * (8.0'f32 * x + EFX8F * x)
    let z = x * x
    let r = PP0F + z * (PP1F + z * (PP2F + z * (PP3F + z * PP4F)))
    let s_val = 1.0'f32 + z * (QQ1F + z * (QQ2F + z * (QQ3F + z * (QQ4F + z * QQ5F))))
    let y = r / s_val
    return x + x * y

  var y: float32
  if ix < 0x40c00000u32:
    y = 1.0'f32 - erfc2_f32(ix, x)
  else:
    let x1p_120 = cast[float32](0x03800000u32)
    y = 1.0'f32 - x1p_120

  if sign != 0: -y else: y

## Complementary error function (f32)
##
## Calculates the complementary probability.
## Is `1 - erf(x)`. Is computed directly, so that you can use it to avoid
## the loss of precision that would result from subtracting
## large probabilities (on large `x`) from 1.
func erfc*(x: float32): float32 =
  let ix_bits = cast[uint32](x)
  let sign = (ix_bits shr 31).int
  var ix = ix_bits and 0x7fffffff

  if ix >= 0x7f800000u32:
    return 2.0'f32 * float32(sign) + 1.0'f32 / x

  if ix < 0x3f580000u32:
    if ix < 0x23800000u32:
      return 1.0'f32 - x
    let z = x * x
    let r = PP0F + z * (PP1F + z * (PP2F + z * (PP3F + z * PP4F)))
    let s_val = 1.0'f32 + z * (QQ1F + z * (QQ2F + z * (QQ3F + z * (QQ4F + z * QQ5F))))
    let y = r / s_val
    if sign != 0 or ix < 0x3e800000u32:
      return 1.0'f32 - (x + x * y)
    return 0.5'f32 - (x - 0.5'f32 + x * y)

  if ix < 0x41e00000u32:
    if sign != 0:
      return 2.0'f32 - erfc2_f32(ix, x)
    else:
      return erfc2_f32(ix, x)

  let x1p_120 = cast[float32](0x03800000u32)
  if sign != 0:
    2.0'f32 - x1p_120
  else:
    x1p_120 * x1p_120

# ============================================================================
# GAMMA FUNCTION
# ============================================================================
# "A Precision Approximation of the Gamma Function" - Cornelius Lanczos (1964)
# "Lanczos Implementation of the Gamma Function" - Paul Godfrey (2001)
# "An Analysis of the Lanczos Gamma Approximation" - Glendon Ralph Pugh (2004)

const PI: float64 = 3.141592653589793238462643383279502884
const N: int = 12
const GMHALF: float64 = 5.524680040776729583740234375
const SNUM: array[N + 1, float64] = [
  23531376880.410759688572007674451636754734846804940,
  42919803642.649098768957899047001988850926355848959,
  35711959237.355668049440185451547166705960488635843,
  17921034426.037209699919755754458931112671403265390,
  6039542586.3520280050642916443072979210699388420708,
  1439720407.3117216736632230727949123939715485786772,
  248874557.86205415651146038641322942321632125127801,
  31426415.585400194380614231628318205362874684987640,
  2876370.6289353724412254090516208496135991145378768,
  186056.26539522349504029498971604569928220784236328,
  8071.6720023658162106380029022722506138218516325024,
  210.82427775157934587250973392071336271166969580291,
  2.5066282746310002701649081771338373386264310793408,
]
const SDEN: array[N + 1, float64] = [
  0.0,
  39916800.0,
  120543840.0,
  150917976.0,
  105258076.0,
  45995730.0,
  13339535.0,
  2637558.0,
  357423.0,
  32670.0,
  1925.0,
  66.0,
  1.0,
]
const FACT: array[23, float64] = [
  1.0, 1.0, 2.0, 6.0, 24.0, 120.0, 720.0, 5040.0, 40320.0, 362880.0,
  3628800.0, 39916800.0, 479001600.0, 6227020800.0, 87178291200.0,
  1307674368000.0, 20922789888000.0, 355687428096000.0, 6402373705728000.0,
  121645100408832000.0, 2432902008176640000.0, 51090942171709440000.0,
  1124000727777607680000.0,
]

func s_tgamma(x: float64): float64 =
  var num = 0.0
  var den = 0.0

  if x < 8.0:
    for i in countdown(N, 0):
      num = num * x + SNUM[i]
      den = den * x + SDEN[i]
  else:
    for i in 0..N:
      num = num / x + SNUM[i]
      den = den / x + SDEN[i]

  num / den

func sinpi_tgamma(mut_x: float64): float64 =
  var x = mut_x
  var n: int

  x = x * 0.5
  x = 2.0 * (x - floor(x))

  n = (4.0 * x).int
  n = (n + 1) div 2
  x -= float64(n) * 0.5

  x *= PI
  case n
  of 1: return cos(x)
  of 2: return sin(-x)
  of 3: return -cos(x)
  else: return sin(x)

## The Gamma function (f64).
func gamma*(x: float64): float64 =
  let u = cast[uint64](x)
  let sign = (u shr 63) != 0
  var ix = ((u shr 32).uint32) and 0x7fffffff

  # special cases
  if ix >= 0x7ff00000u32:
    return x + Inf

  if ix < ((0x3ff - 54) shl 20).uint32:
    return 1.0 / x

  # integer arguments
  if x == floor(x):
    if sign:
      return 0.0 / 0.0
    if x <= 23.0:
      return FACT[int(x) - 1]

  # x >= 172: tgamma(x)=inf with overflow
  # x =< -184: tgamma(x)=+-0 with underflow
  if ix >= 0x40670000u32:
    if sign:
      let x1p_126 = cast[float64](0x3810000000000000u64)
      discard ((x1p_126 / x).float32)
      if floor(x) * 0.5 == floor(x * 0.5):
        return 0.0
      else:
        return -0.0
    let x1p1023 = cast[float64](0x7fe0000000000000u64)
    return x * x1p1023

  let absx = if sign: -x else: x

  # handle the error of x + g - 0.5
  var y = absx + GMHALF
  var dy: float64
  if absx > GMHALF:
    dy = y - absx
    dy -= GMHALF
  else:
    dy = y - GMHALF
    dy -= absx

  let z = absx - 0.5
  var r = s_tgamma(absx) * exp(-y)

  if x < 0.0:
    # reflection formula for negative x
    r = -PI / (sinpi_tgamma(absx) * absx * r)
    dy = -dy

  r += dy * (GMHALF + 0.5) * r / y
  let pow_y = pow(y, 0.5 * z)
  y = r * pow_y * pow_y
  y

## The Gamma function (f32).
func gamma*(x: float32): float32 =
  gamma(x.float64).float32

# ============================================================================
# LOG GAMMA FUNCTION
# ============================================================================
# origin: FreeBSD /usr/src/lib/msun/src/e_lgamma_r.c

const PIF: float64 = 3.14159265358979311600e+00
const A0F: float64 = 7.72156649015328655494e-02
const A1F: float64 = 3.22467033424113591611e-01
const A2F: float64 = 6.73523010531292681824e-02
const A3F: float64 = 2.05808084325167332806e-02
const A4F: float64 = 7.38555086081402883957e-03
const A5F: float64 = 2.89051383673415629091e-03
const A6F: float64 = 1.19270763183362067845e-03
const A7F: float64 = 5.10069792153511336608e-04
const A8F: float64 = 2.20862790713908385557e-04
const A9F: float64 = 1.08011567247583939954e-04
const A10F: float64 = 2.52144565451257326939e-05
const A11F: float64 = 4.48640949618915160150e-05
const TCF: float64 = 1.46163214496836224576e+00
const TFF: float64 = -1.21486290535849611461e-01
const TTF: float64 = -3.63867699703950536541e-18
const T0F: float64 = 4.83836122723810047042e-01
const T1F: float64 = -1.47587722994593911752e-01
const T2F: float64 = 6.46249402391333854778e-02
const T3F: float64 = -3.27885410759859649565e-02
const T4F: float64 = 1.79706750811820387126e-02
const T5F: float64 = -1.03142241298341437450e-02
const T6F: float64 = 6.10053870246291332635e-03
const T7F: float64 = -3.68452016781138256760e-03
const T8F: float64 = 2.25964780900612472250e-03
const T9F: float64 = -1.40346469989232843813e-03
const T10F: float64 = 8.81081882437654011382e-04
const T11F: float64 = -5.38595305356740546715e-04
const T12F: float64 = 3.15632070903625950361e-04
const T13F: float64 = -3.12754168375120860518e-04
const T14F: float64 = 3.35529192635519073543e-04
const U0F: float64 = -7.72156649015328655494e-02
const U1F: float64 = 6.32827064025093366517e-01
const U2F: float64 = 1.45492250137234768737e+00
const U3F: float64 = 9.77717527963372745603e-01
const U4F: float64 = 2.28963728064692451092e-01
const U5F: float64 = 1.33810918536787660377e-02
const V1F: float64 = 2.45597793713041134822e+00
const V2F: float64 = 2.12848976379893395361e+00
const V3F: float64 = 7.69285150456672783825e-01
const V4F: float64 = 1.04222645593369134254e-01
const V5F: float64 = 3.21709242282423911810e-03
const S0F: float64 = -7.72156649015328655494e-02
const S1F: float64 = 2.14982415960608852501e-01
const S2F: float64 = 3.25778796408930981787e-01
const S3F: float64 = 1.46350472652464452805e-01
const S4F: float64 = 2.66422703033638609560e-02
const S5F: float64 = 1.84028451407337715652e-03
const S6F: float64 = 3.19475326584100867617e-05
const R1F: float64 = 1.39200533467621045958e+00
const R2F: float64 = 7.21935547567138069525e-01
const R3F: float64 = 1.71933865632803078993e-01
const R4F: float64 = 1.86459191715652901344e-02
const R5F: float64 = 7.77942496381893596434e-04
const R6F: float64 = 7.32668430744625636189e-06
const W0F: float64 = 4.18938533204672725052e-01
const W1F: float64 = 8.33333333333329678849e-02
const W2F: float64 = -2.77777777728775536470e-03
const W3F: float64 = 7.93650558643019558500e-04
const W4F: float64 = -5.95187557450339963135e-04
const W5F: float64 = 8.36339918996282139126e-04
const W6F: float64 = -1.63092934096575273989e-03

func sin_pi_lgamma(mut_x: float64): float64 =
  var x = mut_x
  var n: int

  x = 2.0 * (x * 0.5 - floor(x * 0.5))

  n = (x * 4.0).int
  n = (n + 1) div 2
  x -= float64(n) * 0.5
  x *= PIF

  case n
  of 1: return cos(x)
  of 2: return sin(-x)
  of 3: return -cos(x)
  else: return sin(x)

func lgamma_impl(mut_x: float64): tuple[res: float64, sign: int] =
  var x = mut_x
  let u = cast[uint64](x)
  var signgam = 1
  let sign = (u shr 63) != 0
  var ix = ((u shr 32).uint32) and 0x7fffffff

  # purge off +-inf, NaN, +-0, tiny and negative arguments
  if ix >= 0x7ff00000u32:
    return (x * x, signgam)

  if ix < ((0x3ff - 70) shl 20).uint32:
    if sign:
      x = -x
      signgam = -1
    return (-ln(x), signgam)

  var nadj: float64
  if sign:
    x = -x
    let t = sin_pi_lgamma(x)
    if t == 0.0:
      return (1.0 / (x - x), signgam)
    if t > 0.0:
      signgam = -1
    nadj = ln(PIF / (sin_pi_lgamma(x) * x))
  else:
    nadj = 0.0

  var r: float64
  if (ix == 0x3ff00000u32 or ix == 0x40000000u32) and (u and 0xffffffff'u64) == 0:
    r = 0.0
  elif ix < 0x40000000u32:
    var y: float64
    var i: int
    if ix <= 0x3feccccc:
      r = -ln(x)
      if ix >= 0x3FE76944u32:
        y = 1.0 - x
        i = 0
      elif ix >= 0x3FCDA661u32:
        y = x - (TCF - 1.0)
        i = 1
      else:
        y = x
        i = 2
    else:
      r = 0.0
      if ix >= 0x3FFBB4C3u32:
        y = 2.0 - x
        i = 0
      elif ix >= 0x3FF3B4C4u32:
        y = x - TCF
        i = 1
      else:
        y = x - 1.0
        i = 2
  
    case i
    of 0:
      let z = y * y
      let p1 = A0F + z * (A2F + z * (A4F + z * (A6F + z * (A8F + z * A10F))))
      let p2 = z * (A1F + z * (A3F + z * (A5F + z * (A7F + z * (A9F + z * A11F)))))
      let p = y * p1 + p2
      r += p - 0.5 * y
    of 1:
      let z = y * y
      let w = z * y
      let p1 = T0F + w * (T3F + w * (T6F + w * (T9F + w * T12F)))
      let p2 = T1F + w * (T4F + w * (T7F + w * (T10F + w * T13F)))
      let p3 = T2F + w * (T5F + w * (T8F + w * (T11F + w * T14F)))
      let p = z * p1 - (TTF - w * (p2 + y * p3))
      r += TFF + p
    of 2:
      let p1 = y * (U0F + y * (U1F + y * (U2F + y * (U3F + y * (U4F + y * U5F)))))
      let p2 = 1.0 + y * (V1F + y * (V2F + y * (V3F + y * (V4F + y * V5F))))
      r += -0.5 * y + p1 / p2
    else:
      discard
  elif ix < 0x40200000u32:
    let i_val = x.int
    let y = x - float64(i_val)
    let p = y * (S0F + y * (S1F + y * (S2F + y * (S3F + y * (S4F + y * (S5F + y * S6F))))))
    let q = 1.0 + y * (R1F + y * (R2F + y * (R3F + y * (R4F + y * (R5F + y * R6F)))))
    r = 0.5 * y + p / q
    var z = 1.0
    if i_val >= 7:
      z *= y + 6.0
    if i_val >= 6:
      z *= y + 5.0
    if i_val >= 5:
      z *= y + 4.0
    if i_val >= 4:
      z *= y + 3.0
    if i_val >= 3:
      z *= y + 2.0
      r += ln(z)
  elif ix < 0x43900000u32:
    let t = ln(x)
    let z = 1.0 / x
    let y = z * z
    let w = W0F + z * (W1F + y * (W2F + y * (W3F + y * (W4F + y * (W5F + y * W6F)))))
    r = (x - 0.5) * (t - 1.0) + w
  else:
    r = x * (ln(x) - 1.0)

  if sign:
    r = nadj - r

  (r, signgam)

## The natural logarithm of the Gamma function (f64).
func lgamma*(x: float64): float64 =
  lgamma_impl(x).res

## The natural logarithm of the Gamma function (f32).
func lgamma*(x: float32): float32 =
  lgamma_impl(x.float64).res.float32
