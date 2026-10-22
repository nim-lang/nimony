## Native (libc-free) implementations of `sin`, `cos`, `tan`, `arcsin`, `arccos`,
## `arctan` and `arctan2`. The polynomial kernels and inverse-trig approximations
## follow fdlibm as carried by Rust compiler-builtins/libm; the medium reducer
## follows `e_rem_pio2.c`. Large-argument reduction is based on the Payne-Hanek
## algorithm in fdlibm's `k_rem_pio2.c`; the fixed-point Nim reducer is a new
## implementation, not a direct code port. Both reducers return the remainder
## as a head + tail pair, which the kernels consume (`k_sin.c`, `k_cos.c`,
## `k_tan.c`) — collapsing it to one double costs over a ulp.
## Sources: [FreeBSD fdlibm k_rem_pio2.c](https://github.com/freebsd/freebsd-src/blob/main/lib/msun/src/k_rem_pio2.c),
## [Rust compiler-builtins rem_pio2_large.rs](https://github.com/rust-lang/rust/blob/main/library/compiler-builtins/libm/src/math/rem_pio2_large.rs).
## Other original C sources are FreeBSD `/usr/src/lib/msun/src/{s_sin,s_cos,k_sin,k_cos,e_asin,e_acos,s_atan,e_atan2,e_rem_pio2}.c`.
## Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
## Copyright (c) 2018 Jorge Aparicio (Rust compiler-builtins/libm code).
## Developed at SunPro, a Sun Microsystems, Inc. business.
## Developed at SunSoft, a Sun Microsystems, Inc. business.
## Permission to use, copy, modify, and distribute this software is freely
## granted, provided that this notice is preserved.

import std/math/common
import std/math/power
import std/math/rounding

const
  PIO2_HI = 1.57079632679489655800e+00
  PIO2_LO = 6.12323399573676603587e-17
  PI_HI = 3.14159265358979311600e+00
  PI_LO = 1.22464679914735317720e-16
  TWO_OVER_PI = 0.63661977236758134308
  PIO2_1 = 1.57079632673412561417e+00
  PIO2_1T = 6.07710050650619224932e-11
  PIO2_2 = 6.07710050630396597660e-11
  PIO2_2T = 2.02226624879595063154e-21
  PIO2_3 = 2.02226624871116645580e-21
  PIO2_3T = 8.47842766036889956997e-32
  LIMB_MASK = 0xffffff'u64
  # 2/pi in base 2^24, from fdlibm's rem_pio2_large table. Fifty limbs
  # cover the full binary64 exponent range with over 170 fractional bits left
  # even for the largest finite input.
  IPIO2: array[50, uint64] = [
    0xA2F983'u64, 0x6E4E44'u64, 0x1529FC'u64, 0x2757D1'u64,
    0xF534DD'u64, 0xC0DB62'u64, 0x95993C'u64, 0x439041'u64,
    0xFE5163'u64, 0xABDEBB'u64, 0xC561B7'u64, 0x246E3A'u64,
    0x424DD2'u64, 0xE00649'u64, 0x2EEA09'u64, 0xD1921C'u64,
    0xFE1DEB'u64, 0x1CB129'u64, 0xA73EE8'u64, 0x8235F5'u64,
    0x2EBB44'u64, 0x84E99C'u64, 0x7026B4'u64, 0x5F7E41'u64,
    0x3991D6'u64, 0x398353'u64, 0x39F49C'u64, 0x845F8B'u64,
    0xBDF928'u64, 0x3B1FF8'u64, 0x97FFDE'u64, 0x05980F'u64,
    0xEF2F11'u64, 0x8B5A0A'u64, 0x6D1F6D'u64, 0x367ECF'u64,
    0x27CB09'u64, 0xB74F46'u64, 0x3F669E'u64, 0x5FEA2D'u64,
    0x7527BA'u64, 0xC7EBE5'u64, 0xF17B3D'u64, 0x0739F7'u64,
    0x8A5292'u64, 0xEA6BFB'u64, 0x5FB11F'u64, 0x8D5D08'u64,
    0x560330'u64, 0x46FC7B'u64]

func isNan64(x: float64): bool {.inline.} =
  let bits = cast[uint64](x)
  ((bits shr 52) and 0x7ff'u64) == 0x7ff'u64 and
    (bits and 0x000fffffffffffff'u64) != 0'u64

func isInf64(x: float64): bool {.inline.} =
  (cast[uint64](x) and 0x7fffffffffffffff'u64) == 0x7ff0000000000000'u64

func signBit64(x: float64): bool {.inline.} =
  (cast[uint64](x) shr 63) != 0'u64

# fdlibm kernels for arguments in [-pi/4, pi/4]; `y` is the tail of the
# reduced argument (x + y), `iy` = 0 when the tail is known to be zero.
func kernelSin(x, y: float64; iy: int): float64 =
  # FreeBSD k_sin.c
  const
    S1 = -1.66666666666666324348e-01
    S2 = 8.33333333332248946124e-03
    S3 = -1.98412698298579493134e-04
    S4 = 2.75573137070700676789e-06
    S5 = -2.50507602534068634195e-08
    S6 = 1.58969099521155010221e-10
  let z = x * x
  let w = z * z
  let r = S2 + z * (S3 + z * S4) + z * w * (S5 + z * S6)
  let v = z * x
  if iy == 0: x + v * (S1 + z * r)
  else: x - ((z * (0.5 * y - v * r) - y) - v * S1)

func kernelCos(x, y: float64): float64 =
  # FreeBSD k_cos.c
  const
    C1 = 4.16666666666666019037e-02
    C2 = -1.38888888888741095749e-03
    C3 = 2.48015872894767294178e-05
    C4 = -2.75573143513906633035e-07
    C5 = 2.08757232129817482790e-09
    C6 = -1.13596475577881948265e-11
  let z = x * x
  var w = z * z
  let r = z * (C1 + z * (C2 + z * C3)) + w * w * (C4 + z * (C5 + z * C6))
  let hz = 0.5 * z
  w = 1.0 - hz
  w + (((1.0 - w) - hz) + (z * r - x * y))

func withLowWordZero(x: float64): float64 {.inline.} =
  cast[float64](cast[uint64](x) and 0xffffffff00000000'u64)

func kernelTan(x0, y0: float64; iy: int): float64 =
  ## tan(x0 + y0) for iy = 1, -1/tan(x0 + y0) for iy = -1 (FreeBSD k_tan.c).
  const
    T: array[13, float64] = [
      3.33333333333334091986e-01, 1.33333333333201242699e-01,
      5.39682539762260521377e-02, 2.18694882948595424599e-02,
      8.86323982359930005737e-03, 3.59207910759131235356e-03,
      1.45620945432529025516e-03, 5.88041240820264096874e-04,
      2.46463134818469906812e-04, 7.81794442939557092300e-05,
      7.14072491382608190305e-05, -1.85586374855275456654e-05,
      2.59073051863633712884e-05]
    PIO4 = 7.85398163397448278999e-01
    PIO4LO = 3.06161699786838301793e-17
  var x = x0
  var y = y0
  let hx = cast[int32](cast[uint32](cast[uint64](x) shr 32))
  let ix = hx and 0x7fffffff'i32
  let big = ix >= 0x3FE59428'i32           # |x| >= 0.6744
  if big:
    if hx < 0'i32:
      x = -x
      y = -y
    x = (PIO4 - x) + (PIO4LO - y)
    y = 0.0
  var z = x * x
  var w = z * z
  var r = T[1] + w * (T[3] + w * (T[5] + w * (T[7] + w * (T[9] + w * T[11]))))
  var v = z * (T[2] + w * (T[4] + w * (T[6] + w * (T[8] + w * (T[10] + w * T[12])))))
  var s = z * x
  r = y + z * (s * (r + v) + y)
  r += T[0] * s
  w = x + r
  if big:
    v = float64(iy)
    let sgn = float64(1 - ((hx shr 30) and 2'i32))
    return sgn * (v - 2.0 * (x - (w * w / (w + v) - r)))
  if iy == 1: return w
  # -1/(x + r), computed accurately
  z = withLowWordZero(w)
  v = r - (z - x)                           # z + v = r + x
  let a = -1.0 / w
  let t = withLowWordZero(a)
  s = 1.0 + t * z
  t + a * (s + t * v)

func productBit(a: array[53, uint64], bit: int): uint64 {.inline.} =
  if bit < 0 or bit >= 53 * 24: return 0'u64
  (a[bit div 24] shr (bit mod 24)) and 1'u64

func fractionValue(a: array[53, uint64], bits: int): tuple[hi, lo: float64] =
  ## The fraction as a double-double of 106 SIGNIFICANT bits: leading zero
  ## bits are skipped first, because an argument near a multiple of pi/2
  ## leaves a remainder with ~60 of them, and a fixed window would keep only
  ## the rest. The omitted tail is below 2^-106 of the value.
  var lz = 0
  while lz < 1000 and productBit(a, bits - 1 - lz) == 0'u64: inc lz
  if lz >= 1000: return (hi: 0.0, lo: 0.0)
  var h = 0'u64
  var l = 0'u64
  for i in 0..<53:
    h = (h shl 1) or productBit(a, bits - 1 - lz - i)
  for i in 53..<106:
    l = (l shl 1) or productBit(a, bits - 1 - lz - i)
  # 2^-(53+lz) and 2^-(106+lz), exact: lz is far below the subnormal range
  let sh = cast[float64](uint64(1023 - 53 - lz) shl 52)
  let sl = cast[float64](uint64(1023 - 106 - lz) shl 52)
  (hi: float64(h) * sh, lo: float64(l) * sl)

func split27(a: float64): tuple[hi, lo: float64] {.inline.} =
  ## Veltkamp split: hi holds the top 26 bits, a = hi + lo exactly.
  let c = 134217729.0 * a                         # 2^27 + 1
  let hi = c - (c - a)
  (hi: hi, lo: a - hi)

func twoProd(a, b: float64): tuple[p, e: float64] {.inline.} =
  ## Dekker: a*b = p + e exactly (no FMA needed).
  let p = a * b
  let (ah, al) = split27(a)
  let (bh, bl) = split27(b)
  (p: p, e: ((ah * bh - p) + ah * bl + al * bh) + al * bl)

func largeReduce(x: float64): tuple[n: int, y0, y1: float64] =
  ## Multiply the significand by a fixed-point 2/pi expansion, then take the
  ## low quadrant bits and the signed fractional part. No huge floating-point
  ## quotient is formed or converted to an integer.
  let raw = cast[uint64](x) and 0x7fffffffffffffff'u64
  let exponent = int((raw shr 52) and 0x7ff'u64) - 1023
  let significand = (raw and 0x000fffffffffffff'u64) or (1'u64 shl 52)
  let m = [significand and LIMB_MASK,
           (significand shr 24) and LIMB_MASK,
           significand shr 48]
  var p = default(array[53, uint64])
  for i in 0..<50:
    for j in 0..<3:
      p[49 - i + j] += IPIO2[i] * m[j]
  for i in 0..<52:
    let carry = p[i] shr 24
    p[i] = p[i] and LIMB_MASK
    p[i + 1] += carry

  # x*(2/pi) = p / 2^fractionBits. All callers here have |x| >= 2^20.
  let fractionBits = 1200 + 52 - exponent
  var floorMod8 = 0
  for i in 0..<3:
    floorMod8 = floorMod8 or (int(productBit(p, fractionBits + i)) shl i)
  let roundUp = productBit(p, fractionBits - 1) != 0'u64
  var fracLimbs = p
  if roundUp:
    # Form 1-frac exactly before conversion, avoiding catastrophic cancellation
    # when x is exceptionally close to an integer multiple of pi/2.
    let whole = fractionBits div 24
    let rem = fractionBits mod 24
    for i in 0..<whole:
      fracLimbs[i] = LIMB_MASK - fracLimbs[i]
    let topMask = if rem == 0: 0'u64 else: (1'u64 shl rem) - 1'u64
    if rem > 0:
      fracLimbs[whole] = topMask - (fracLimbs[whole] and topMask)
    var carry = 1'u64
    for i in 0..<whole:
      let sum = fracLimbs[i] + carry
      fracLimbs[i] = sum and LIMB_MASK
      carry = sum shr 24
    if rem > 0:
      fracLimbs[whole] = (fracLimbs[whole] + carry) and topMask
    floorMod8 = (floorMod8 + 1) and 7
  let (fh, fl) = fractionValue(fracLimbs, fractionBits)
  let n = if signBit64(x): -floorMod8 else: floorMod8
  # r = frac * pi/2 as a double-double: the exact head product plus the
  # cross terms (fl*PIO2_LO is below 2^-160 and dropped).
  let (hp, he) = twoProd(fh, PIO2_HI)
  let t = he + (fh * PIO2_LO + fl * PIO2_HI)
  let h = hp + t
  let l = t - (h - hp)
  if signBit64(x) xor roundUp: (n: n, y0: -h, y1: -l)
  else: (n: n, y0: h, y1: l)

func reduceQuarter(x: float64): tuple[n: int, y0, y1: float64] =
  ## Cody-Waite for medium arguments and fixed-point Payne-Hanek reduction for
  ## the rest of finite binary64. The medium path's quotient is bounded.
  if (cast[uint64](x) and 0x7fffffffffffffff'u64) >= 0x413921fb00000000'u64:
    return largeReduce(x)
  let n = int(round(x * TWO_OVER_PI))
  let nf = float64(n)
  var r = x - nf * PIO2_1
  var w = nf * PIO2_1T
  var y = r - w
  let ix = cast[uint32]((cast[uint64](x) shr 32) and 0x7fffffff'u64)
  let ey = int((cast[uint64](y) shr 52) and 0x7ff'u64)
  let ex = int(ix shr 20)
  if ex - ey > 16:
    let t = r
    w = nf * PIO2_2
    r = t - w
    w = nf * PIO2_2T - ((t - r) - w)
    y = r - w
    let ey2 = int((cast[uint64](y) shr 52) and 0x7ff'u64)
    if ex - ey2 > 49:
      let t2 = r
      w = nf * PIO2_3
      r = t2 - nf * PIO2_3
      w = nf * PIO2_3T - ((t2 - r) - w)
      y = r - w
  (n: n, y0: y, y1: (r - y) - w)

func sin64(x: float64): float64 =
  if isNan64(x): return x
  if isInf64(x): return x - x
  if x == 0.0: return x
  let (n, y0, y1) = reduceQuarter(x)
  result = case n and 3
    of 0: kernelSin(y0, y1, 1)
    of 1: kernelCos(y0, y1)
    of 2: -kernelSin(y0, y1, 1)
    else: -kernelCos(y0, y1)

func cos64(x: float64): float64 =
  if isNan64(x): return x
  if isInf64(x): return x - x
  if x == 0.0: return 1.0
  let (n, y0, y1) = reduceQuarter(x)
  result = case n and 3
    of 0: kernelCos(y0, y1)
    of 1: -kernelSin(y0, y1, 1)
    of 2: -kernelCos(y0, y1)
    else: kernelSin(y0, y1, 1)

func tan64(x: float64): float64 =
  if isNan64(x): return x
  if isInf64(x): return x - x
  if x == 0.0: return x
  let (n, y0, y1) = reduceQuarter(x)
  kernelTan(y0, y1, 1 - ((n and 1) shl 1))     # 1 for even n, -1 for odd

# fdlibm rational approximation used by both inverse sine functions.
func asinR(z: float64): float64 =
  const
    P0 = 1.66666666666666657415e-01
    P1 = -3.25565818622400915405e-01
    P2 = 2.01212532134862925881e-01
    P3 = -4.00555345006794114027e-02
    P4 = 7.91534994289814532176e-04
    P5 = 3.47933107596021167570e-05
    Q1 = -2.40339491173441421878e+00
    Q2 = 2.02094576023350569471e+00
    Q3 = -6.88283971605453293030e-01
    Q4 = 7.70381505559019352791e-02
  let p = z * (P0 + z * (P1 + z * (P2 + z * (P3 + z * (P4 + z * P5)))))
  let q = 1.0 + z * (Q1 + z * (Q2 + z * (Q3 + z * Q4)))
  p / q

func asin64(x: float64): float64 =
  const PIO2 = 1.57079632679489655800
  let ax = if x < 0.0: -x else: x
  if isNan64(x): return x
  if ax > 1.0: return 0.0 / (x - x)
  if ax == 1.0: return (if signBit64(x): -PIO2 else: PIO2)
  if ax < 0.5:
    if ax < 1.0e-8: return x
    return x + x * asinR(x * x)
  let z = (1.0 - ax) * 0.5
  let s = sqrt(z)
  let r = asinR(z)
  var a: float64
  if ax > 0.975:
    a = PIO2 - 2.0 * (s + s * r)
  else:
    let f = cast[float64](cast[uint64](s) and 0xffffffff00000000'u64)
    let c = (z - f * f) / (s + f)
    a = 0.5 * PIO2 - (2.0 * s * r - (PIO2_LO - 2.0 * c) - (0.5 * PIO2 - 2.0 * f))
  if signBit64(x): -a else: a

func acos64(x: float64): float64 =
  const PIO2 = 1.57079632679489655800
  if isNan64(x): return x
  if x < -1.0 or x > 1.0: return 0.0 / (x - x)
  if x == 1.0: return 0.0
  if x == -1.0: return PI_HI
  if x < 0.5 and x > -0.5:
    return PIO2 - (x - (PIO2_LO - x * asinR(x * x)))
  if x < 0.0:
    let z = (1.0 + x) * 0.5
    let s = sqrt(z)
    let w = asinR(z) * s - PIO2_LO
    return 2.0 * (PIO2 - (s + w))
  let z = (1.0 - x) * 0.5
  let s = sqrt(z)
  let f = cast[float64](cast[uint64](s) and 0xffffffff00000000'u64)
  let c = (z - f * f) / (s + f)
  2.0 * (f + (asinR(z) * s + c))

func atan64(x: float64): float64 =
  const
    ATANHI: array[4, float64] = [4.63647609000806093515e-01, 7.85398163397448278999e-01,
      9.82793723247329054082e-01, 1.57079632679489655800e+00]
    ATANLO: array[4, float64] = [2.26987774529616870924e-17, 3.06161699786838301793e-17,
      1.39033110312309984516e-17, 6.12323399573676603587e-17]
    AT: array[11, float64] = [3.33333333333329318027e-01, -1.99999999998764832476e-01,
      1.42857142725034663711e-01, -1.11111104054623557880e-01, 9.09088713343650656196e-02,
      -7.69187620504482999495e-02, 6.66107313738753120669e-02, -5.83357013379057348645e-02,
      4.97687799461593236017e-02, -3.65315727442169155270e-02, 1.62858201153657823623e-02]
  if isNan64(x): return x
  if isInf64(x): return (if signBit64(x): -ATANHI[3] else: ATANHI[3])
  var a = x
  let ix = cast[uint32]((cast[uint64](x) shr 32) and 0x7fffffff'u64)
  let sign = signBit64(x)
  var id = -1
  if ix < 0x3fdc0000'u32:
    if ix < 0x3e400000'u32: return x
  else:
    if a < 0.0: a = -a
    if ix < 0x3ff30000'u32:
      if ix < 0x3fe60000'u32:
        a = (2.0 * a - 1.0) / (2.0 + a)
        id = 0
      else:
        a = (a - 1.0) / (a + 1.0)
        id = 1
    elif ix < 0x40038000'u32:
      a = (a - 1.5) / (1.0 + 1.5 * a)
      id = 2
    else:
      a = -1.0 / a
      id = 3
  let z = a * a
  let w = z * z
  let s1 = z * (AT[0] + w * (AT[2] + w * (AT[4] + w * (AT[6] + w * (AT[8] + w * AT[10])))))
  let s2 = w * (AT[1] + w * (AT[3] + w * (AT[5] + w * (AT[7] + w * AT[9]))))
  if id < 0: return a - a * (s1 + s2)
  let r = ATANHI[id] - (a * (s1 + s2) - ATANLO[id] - a)
  if sign: -r else: r

func atan2_64(y, x: float64): float64 =
  if isNan64(x) or isNan64(y): return x + y
  let xb = cast[uint64](x)
  let yb = cast[uint64](y)
  let ax = xb and 0x7fffffffffffffff'u64
  let ay = yb and 0x7fffffffffffffff'u64
  let sy = (yb shr 63) != 0'u64
  let sx = (xb shr 63) != 0'u64
  if ay == 0'u64:
    if sx: return (if sy: -PI_HI else: PI_HI)
    return y
  if ax == 0'u64: return (if sy: -PIO2_HI else: PIO2_HI)
  let xInf = ax == 0x7ff0000000000000'u64
  let yInf = ay == 0x7ff0000000000000'u64
  if xInf and yInf:
    if sx: return (if sy: -3.0 * PI_HI / 4.0 else: 3.0 * PI_HI / 4.0)
    return (if sy: -PI_HI / 4.0 else: PI_HI / 4.0)
  if yInf: return (if sy: -PIO2_HI else: PIO2_HI)
  if xInf:
    if sx: return (if sy: -PI_HI else: PI_HI)
    return (if sy: -0.0 else: 0.0)
  let a = atan64((if sy: -y else: y) / (if sx: -x else: x))
  if sx:
    if sy: a - PI_HI else: PI_HI - a
  else:
    if sy: -a else: a

func sin*(x: float32): float32 = float32(sin64(float64(x)))
func sin*(x: float64): float64 = sin64(x)
func cos*(x: float32): float32 = float32(cos64(float64(x)))
func cos*(x: float64): float64 = cos64(x)
func tan*(x: float32): float32 = float32(tan64(float64(x)))
func tan*(x: float64): float64 = tan64(x)
func arcsin*(x: float32): float32 = float32(asin64(float64(x)))
func arcsin*(x: float64): float64 = asin64(x)
func arccos*(x: float32): float32 = float32(acos64(float64(x)))
func arccos*(x: float64): float64 = acos64(x)
func arctan*(x: float32): float32 = float32(atan64(float64(x)))
func arctan*(x: float64): float64 = atan64(x)
func arctan2*(y, x: float32): float32 = float32(atan2_64(float64(y), float64(x)))
func arctan2*(y, x: float64): float64 = atan2_64(y, x)
