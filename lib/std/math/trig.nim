## Native (libc-free) implementations of `sin`, `cos`, `tan`, `arcsin`, `arccos`,
## `arctan` and `arctan2`. The polynomial kernels and inverse-trig approximations
## follow fdlibm as carried by Rust compiler-builtins/libm. Large-argument
## reduction is based on the Payne-Hanek algorithm in fdlibm's `k_rem_pio2.c`;
## the fixed-point Nim reducer is a new implementation, not a direct code port.
## Both reducers return the remainder as a head + tail pair, which the kernels
## consume (`k_sin.c`, `k_cos.c`, `k_tan.c`) — collapsing it to one double costs
## over a ulp.
##
## The medium reducer covers `|x| < pi/2 * 2^26` with a Cody-Waite split of
## `pi/2` into four pieces of at most 26 significant bits, so every
## `n * piece` product is exact for `n < 2^27` and the whole reduction is a
## straight-line sequence (no data-dependent branches). The nearest integer is
## obtained without a `round` call via the `1.5 * 2^52` magic-number trick,
## exact for these bounded quotients and valid with or without FMA contraction.
##
## The large reducer multiplies only the table limbs that can influence the
## extracted bits (quadrant + 106 fraction bits, anchored at the quadrant so
## the extraction uses fixed exponents): the dropped terms provably stay below
## 2^-140 of the fraction. Arguments whose dropped terms could alias a carry
## into the quadrant (an unbroken run of ones up to the quadrant bits),
## arguments within ~2^-47 of a multiple of `pi/2`, and `|x| >= 2^964` take
## the full-precision `largeReduceExact` path instead.
##
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

const
  PIO2_HI = 1.57079632679489655800e+00
  PIO2_LO = 6.12323399573676603587e-17
  PI_HI = 3.14159265358979311600e+00
  PI_LO = 1.22464679914735317720e-16
  TWO_OVER_PI = 0.63661977236758134308
  # pi/2 split into Cody-Waite pieces: A, B, C with at most 26 significant
  # bits (so every n*piece product is exact for |n| < 2^27, as 27+26-1 <= 53
  # significant bits) and a full-precision tail piece D. The residual after
  # D is below 2^-130, so the dropped part stays under n * 2^-130 <= 2^-103
  # even for the largest n that reaches this path; the rounding of the
  # n*D product itself is at most ulp(n*D)/2 <= 2^-103 as well. This keeps
  # the absolute error of the reduced argument far below the point where it
  # could matter even when the reduced argument is nearly zero (x close to
  # a multiple of pi/2, where a relative error would blow up).
  PIO2_A = 1.570796310901641845703125
  PIO2_B = 1.58932547122958567342720925807952880859375e-08
  PIO2_C = 6.12323393205359425102082826519733771419851109385490417480469e-17
  PIO2_D = 6.36831716351094990796199425288059588978250343317061887662036e-25
  # 1.5 * 2^52: adding the scaled quotient puts the nearest integer in the
  # low bits of the sum's mantissa, for quotients bounded well below 2^51.
  # Works with and without FMA contraction: both round the scaled quotient
  # to a nearest integer; exact ties (which cannot make the reduced argument
  # leave [-pi/4, pi/4]) may pick the even neighbour instead of away from
  # zero like `round`.
  ROUND_MAGIC = 6755399441055744.0
  # Largest |x| taking the medium path: pi/2 * 2^26 rounded as a bit pattern;
  # x below it keeps |n| <= 2^26 and n*piece products exact.
  MEDIUM_MAX_BITS = 0x419921fb54442d18'u64
  # Bit pattern of pi/4 rounded to double. |x| below it always yields n = 0
  # (x*(2/pi) stays safely under 0.5), so the whole Cody-Waite sequence in
  # `reduceQuarter` multiplies by zero and returns (0, x, 0) unchanged.
  PIO4_MAX_BITS = 0x3fe921fb54442d18'u64
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
  TWO_POW_M53 = 1.1102230246251565404236316680908203125e-16   # 2^-53
  TWO_POW_M106 = 1.2325951644078309452578161362459e-32        # 2^-106

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

func fracTimesPio2(fh, fl: float64): tuple[y0, y1: float64] {.inline.} =
  ## (fh + fl) * pi/2 as a double-double: exact head product plus the cross
  ## terms (fl*PIO2_LO is below 2^-160 and dropped).
  let (hp, he) = twoProd(fh, PIO2_HI)
  let t = he + (fh * PIO2_LO + fl * PIO2_HI)
  let h = hp + t
  let l = t - (h - hp)
  (y0: h, y1: l)

func largeReduceExact(x: float64): tuple[n: int, y0, y1: float64] =
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
  let (y0, y1) = fracTimesPio2(fh, fl)
  let n = if signBit64(x): -floorMod8 else: floorMod8
  if signBit64(x) xor roundUp: (n: n, y0: -y0, y1: -y1)
  else: (n: n, y0: y0, y1: y1)

func largeReduce(x: float64): tuple[n: int, y0, y1: float64] =
  ## Payne-Hanek reduction over only the limbs that can influence the
  ## quadrant bits and the top 106 fraction bits; `largeReduceExact` handles
  ## the cases where the short window would not be provably sufficient.
  ## Local limb k holds product limb qLimb - 7 + k; the window [0, 9]
  ## covers product limbs [qLimb-7, qLimb+2]. The dropped terms stay below
  ## 2^(24*qLimb-118), twelve bits under the extraction floor.
  let raw = cast[uint64](x) and 0x7fffffffffffffff'u64
  let exponent = int((raw shr 52) and 0x7ff'u64) - 1023
  let significand = (raw and 0x000fffffffffffff'u64) or (1'u64 shl 52)
  let m0 = significand and LIMB_MASK
  let m1 = (significand shr 24) and LIMB_MASK
  let m2 = significand shr 48
  # x*(2/pi) = p / 2^fractionBits with 24-bit limbs.
  let fractionBits = 1200 + 52 - exponent
  let qLimb = fractionBits div 24          # limb holding quadrant bit 0
  let qRem = fractionBits mod 24
  if qLimb < 12:
    # |x| >= 2^964: the fraction starts in the low table limbs; the exact
    # path costs the same there.
    return largeReduceExact(x)

  var p = default(array[12, uint64])
  # Terms whose product limb 49-i+j lands in [qLimb-7, qLimb+1]:
  #   i >= 48+j-qLimb and i <= 56+j-qLimb. Local index = 56+j-qLimb-i.
  block:
    var iLo = 48 - qLimb
    if iLo < 0: iLo = 0
    var iHi = 56 - qLimb
    if iHi > 49: iHi = 49
    for i in iLo .. iHi:
      p[56 - qLimb - i] += IPIO2[i] * m0
    iLo = 49 - qLimb
    if iLo < 0: iLo = 0
    iHi = 57 - qLimb
    if iHi > 49: iHi = 49
    for i in iLo .. iHi:
      p[57 - qLimb - i] += IPIO2[i] * m1
    iLo = 50 - qLimb
    if iLo < 0: iLo = 0
    iHi = 58 - qLimb
    if iHi > 49: iHi = 49
    # m2 has at most 5 bits: multiplying it by a packed pair of table limbs
    # stays below 2^53, halving the multiply count of this group. The more
    # significant table limb IPIO2[i] sits in the high half, so the low half
    # of the product lands on limb 50-i and the high half on limb 51-i.
    var i = iLo
    while i + 1 <= iHi:
      let pair = ((IPIO2[i] shl 24) or IPIO2[i+1]) * m2
      p[57 - qLimb - i] += pair and LIMB_MASK
      p[58 - qLimb - i] += pair shr 24
      inc i
      inc i
    if i == iHi:
      p[58 - qLimb - i] += IPIO2[i] * m2
  for k in 0 ..< 9:
    let carry = p[k] shr 24
    p[k] = p[k] and LIMB_MASK
    p[k + 1] += carry

  # Quadrant bits [fractionBits, fractionBits+3); they can straddle a limb.
  # p[7] = limb qLimb, p[8] = limb qLimb+1.
  let quad = int(((p[7] shr qRem) or (p[8] shl (24 - qRem))) and 7'u64)
  # Round-up bit: bit fractionBits-1 (the fraction's MSB). It is bit 23 of
  # the aligned limb (p[7] shl (24-qRem)) or (p[6] shr qRem) for every qRem:
  # p[7] bit qRem-1 maps to bit 23 for qRem > 0, p[6] bit 23 for qRem = 0
  # (bits of p[7] above qRem land above 23 and are masked off).
  let roundBit =
    (((p[7] shl (24 - qRem)) or (p[6] shr qRem)) shr 23) and 1'u64

  # Exactness guard: the dropped terms (total < 2^dBit, dBit = 24*qLimb-142)
  # are added to this partial sum; their carry can reach the quadrant only
  # through an unbroken run of ones from dBit up to fractionBits+2. Detect
  # that run and take the exact path when it occurs. The run starts at bit 2
  # of local limb 2 and ends at bit fractionBits+2 (local 7, spilling into
  # local 8 when qRem > 21).
  block exactness:
    if (p[2] shr 2) != 0x3fffff'u64: break exactness
    if p[3] != LIMB_MASK: break exactness
    if p[4] != LIMB_MASK: break exactness
    if p[5] != LIMB_MASK: break exactness
    if p[6] != LIMB_MASK: break exactness
    if qRem <= 21:
      let m = (1'u64 shl (qRem + 3)) - 1'u64
      if (p[7] and m) != m: break exactness
    else:
      if p[7] != LIMB_MASK: break exactness
      let m = (1'u64 shl (qRem - 21)) - 1'u64
      if (p[8] and m) != m: break exactness
    return largeReduceExact(x)

  # Keep only the fraction bits of limb qLimb (those below bit qRem).
  let lowMask = (1'u64 shl qRem) - 1'u64    # 0 when qRem == 0
  p[7] = p[7] and lowMask

  # Aligned fraction limbs: a_k holds F's bits [24k, 24k+24) most
  # significant first. The upper source limb p[7-k] contributes only its low
  # qRem bits (they are F's bits [24k, 24k+qRem); its high bits belong to
  # a_{k-1}), the lower source p[6-k] contributes its top 24-qRem bits.
  # p[7] itself is already masked to its low qRem bits above. h = F[0,53)
  # and l = F[53,106) then assemble with constant shifts only - no
  # qRem-dependent branch.
  let a0 = (p[7] shl (24 - qRem)) or (p[6] shr qRem)
  let a1 = ((p[6] and lowMask) shl (24 - qRem)) or (p[5] shr qRem)
  let a2 = ((p[5] and lowMask) shl (24 - qRem)) or (p[4] shr qRem)
  let a3 = ((p[4] and lowMask) shl (24 - qRem)) or (p[3] shr qRem)
  let a4 = ((p[3] and lowMask) shl (24 - qRem)) or (p[2] shr qRem)
  # When the fraction rounds up (roundBit), the reduced argument is built
  # from 1-f instead of f, and the quadrant gains one. Over the 106-bit
  # window, 1-f is the two's complement of (h, l): complementing both words
  # inside 53 bits via a mask is branch-free, and the possible missing +1
  # borrow from fraction bits below the window changes the result by one ulp
  # of l = 2^-106 of the fraction - far below the extraction floor, since
  # the fraction is at least 2^-47 whenever the h < 64 check below does not
  # divert to the exact path.
  let ruMask = (0'u64 - roundBit) and ((1'u64 shl 53) - 1'u64)
  let h = ((a0 shl 29) or (a1 shl 5) or (a2 shr 19)) xor ruMask
  let l = (((a2 and 0x7ffff'u64) shl 34) or (a3 shl 10) or (a4 shr 14)) xor ruMask
  let floorMod8 = (quad + int(roundBit)) and 7
  # h < 64 means the fraction's first 47 bits are zero: x is within ~2^-47
  # of a multiple of pi/2 and the fixed 106-bit window loses relative
  # accuracy. Exact path. (For rounded-up fractions this sees 1-f, exactly
  # like the limb complement it replaces.)
  if h < 64'u64:
    return largeReduceExact(x)
  let fh = float64(h) * TWO_POW_M53
  let fl = float64(l) * TWO_POW_M106
  let (y0, y1) = fracTimesPio2(fh, fl)
  let n = if signBit64(x): -floorMod8 else: floorMod8
  # Negate y exactly when x is negative xor the rounding went up: XOR with
  # the sign-bit mask is IEEE negation, down to +-0.0.
  let flip = ((cast[uint64](x) shr 63) xor roundBit) shl 63
  (n: n,
    y0: cast[float64](cast[uint64](y0) xor flip),
    y1: cast[float64](cast[uint64](y1) xor flip))

func reduceQuarter(x: float64): tuple[n: int, y0, y1: float64] {.inline.} =
  ## Four-piece Cody-Waite for |x| < pi/2*2^26 (every n*piece product exact),
  ## fixed-point Payne-Hanek reduction above that. The quotient is bounded
  ## (|n| <= 2^26), which the magic-number rounding relies on.
  let ax = cast[uint64](x) and 0x7fffffffffffffff'u64
  if ax >= MEDIUM_MAX_BITS:
    return largeReduce(x)
  if ax < PIO4_MAX_BITS:
    # |x| < pi/4: the quotient is 0, every product below is a multiplication
    # by zero and the sequence would return (0, x, 0) exactly. Skip it.
    return (n: 0, y0: x, y1: 0.0)
  # Nearest integer of x*(2/pi) without a call: the low 32 mantissa bits of
  # the biased sum hold n (two's complement), the unbiased sum holds it as a
  # float. Exact for |n| < 2^31.
  let t = x * TWO_OVER_PI + ROUND_MAGIC
  let nf = t - ROUND_MAGIC
  let n = int(cast[int32](cast[uint32](cast[uint64](t))))
  # r = x - nf*PIO2_A exactly (both products and the subtraction, since the
  # pieces carry <= 26 significant bits and the result stays within 2.1).
  var y = x - nf * PIO2_A
  var w = nf * PIO2_B
  var r = y - w
  # (y, y1) below tracks x - nf*(A+B+C+D) as a head + tail pair.
  var y1 = (y - r) - w
  y = r
  w = nf * PIO2_C
  r = y - w
  y1 = y1 + ((y - r) - w)
  y = r
  w = nf * PIO2_D
  r = y - w
  y1 = y1 + ((y - r) - w)
  (n: n, y0: r, y1: y1)

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
    # fdlibm's tables, extended with a leading identity entry so no region
    # needs a special case: table index i = id + 1, with id = -1 for
    # |x| < 7/16 (no argument transform). The Mobius tables implement
    # a' = (P*a + Q) / (R*a + S) on |x| and reproduce fdlibm's transforms
    # operation for operation (x - 1.0 is x + (-1.0), addition commutes),
    # so every result is bit-identical to the branch tree it replaces.
    ATANHI: array[5, float64] = [0.0, 4.63647609000806093515e-01,
      7.85398163397448278999e-01, 9.82793723247329054082e-01,
      1.57079632679489655800e+00]
    ATANLO: array[5, float64] = [0.0, 2.26987774529616870924e-17,
      3.06161699786838301793e-17, 1.39033110312309984516e-17,
      6.12323399573676603587e-17]
    MP: array[5, float64] = [1.0, 2.0, 1.0, 1.0, 0.0]
    MQ: array[5, float64] = [0.0, -1.0, -1.0, -1.5, -1.0]
    MR: array[5, float64] = [0.0, 1.0, 1.0, 1.5, 1.0]
    MS: array[5, float64] = [1.0, 2.0, 1.0, 1.0, 0.0]
    AT: array[11, float64] = [3.33333333333329318027e-01, -1.99999999998764832476e-01,
      1.42857142725034663711e-01, -1.11111104054623557880e-01, 9.09088713343650656196e-02,
      -7.69187620504482999495e-02, 6.66107313738753120669e-02, -5.83357013379057348645e-02,
      4.97687799461593236017e-02, -3.65315727442169155270e-02, 1.62858201153657823623e-02]
  if isNan64(x): return x
  if isInf64(x): return (if signBit64(x): -ATANHI[4] else: ATANHI[4])
  let ix = cast[uint32]((cast[uint64](x) shr 32) and 0x7fffffff'u64)
  if ix < 0x3e400000'u32: return x  # |x| < 2^-27 (covers +-0.0)
  let ax = cast[float64](cast[uint64](x) and 0x7fffffffffffffff'u64)
  # Region id, branch-free: the thresholds are ordered, so a sum of
  # comparisons selects -1 (|x| < 7/16), 0, 1, 2 or 3 without the
  # mispredictable branch tree of the original fdlibm structure.
  let id = int(ix >= 0x3fdc0000'u32) + int(ix >= 0x3fe60000'u32) +
    int(ix >= 0x3ff30000'u32) + int(ix >= 0x40038000'u32) - 1
  let i = id + 1
  let a = (MP[i] * ax + MQ[i]) / (MR[i] * ax + MS[i])
  let z = a * a
  let w = z * z
  let s1 = z * (AT[0] + w * (AT[2] + w * (AT[4] + w * (AT[6] + w * (AT[8] + w * AT[10])))))
  let s2 = w * (AT[1] + w * (AT[3] + w * (AT[5] + w * (AT[7] + w * (AT[9])))))
  let r = ATANHI[i] - (a * (s1 + s2) - ATANLO[i] - a)
  # copysign(r, x) without a branch; r is never zero here and +-0.0
  # returned early above.
  cast[float64](cast[uint64](r) or (cast[uint64](x) and 0x8000000000000000'u64))

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
