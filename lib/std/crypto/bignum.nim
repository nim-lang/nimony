#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Fixed-capacity unsigned big integers and Montgomery arithmetic, for
## SIGNATURE VERIFICATION.
##
## Everything a verifier computes is public — the signature, the message
## digest, the public key — so this module is deliberately *not* constant
## time: `modExp` skips zero bits and `sub` branches on a comparison. Do not
## use it with a secret.
##
## A number is at most `MaxBits` wide (RSA-4096 plus headroom for the
## Montgomery product), held in a fixed array of 32-bit limbs together with a
## `range`-typed limb count, which is what lets every index in here be proven
## rather than checked.

{.feature: "staticContracts".}

const
  MaxBits* = 4096
  MaxLimbs* = MaxBits div 32 + 2
    ## Two spare limbs: the Montgomery product keeps `n + 2` words of state.

type
  Limbs* = array[MaxLimbs, uint32]
  LimbCount* = range[1 .. MaxLimbs - 2]

  BigNum* = object
    ## Little-endian limbs; the ones at and past `n` are zero.
    limbs*: Limbs
    n*: LimbCount

  Mont* = object
    ## A Montgomery context for an ODD modulus `m`: `R = 2^(32*n)`.
    m*: BigNum
    m0inv: uint32       ## `-m^-1 mod 2^32`
    r2: Limbs           ## `R^2 mod m`

func bitLen*(a: BigNum): int =
  result = 0
  var i = a.n - 1
  while i >= 0:
    if a.limbs[i] != 0'u32:
      var x = a.limbs[i]
      var b = 0
      while x != 0'u32:
        x = x shr 1
        inc b
      return i * 32 + b
    dec i

func isZero*(a: BigNum): bool =
  result = true
  for i in 0 ..< a.n:
    if a.limbs[i] != 0'u32: return false

func fromBytesBE*(data: openArray[byte]; res: var BigNum): bool =
  ## Reads a big-endian unsigned number. Leading zero bytes are free; `false`
  ## when the number does not fit in `MaxBits`.
  res = BigNum(limbs: default(Limbs), n: 1)
  var start = 0
  while start < data.len and data[start] == 0'u8: inc start
  let significant = data.len - start
  if significant > MaxBits div 8: return false
  var limb = 0
  var shift = 0
  var i = data.len - 1
  while i >= start:
    if limb < MaxLimbs:
      res.limbs[limb] = res.limbs[limb] or (uint32(data[i]) shl shift)
    shift = shift + 8
    if shift == 32:
      shift = 0
      inc limb
    dec i
  var used = (significant + 3) div 4
  if used < 1: used = 1
  if used > MaxLimbs - 2: return false
  res.n = used
  result = true

func toBytesBE*(a: BigNum; outLen: int): seq[byte] {.requires: outLen >= 0.} =
  ## The low `outLen` bytes of `a`, big-endian, zero padded on the left.
  result = newSeq[byte](outLen)
  var j = result.len - 1         # output position, filled from the right
  var i = 0                      # byte index into `a`, little-endian
  while j >= 0:
    let limb = i div 4
    let shift = (i mod 4) * 8
    var v = 0'u8
    if limb >= 0 and limb < MaxLimbs:
      v = byte((a.limbs[limb] shr shift) and 0xFF'u32)
    if j < result.len: result[j] = v
    dec j
    inc i

func cmpLimbs(a, b: Limbs; n: LimbCount): int =
  ## Compares the low `n` limbs.
  var i = n - 1
  while i >= 0:
    if a[i] < b[i]: return -1
    if a[i] > b[i]: return 1
    dec i
  result = 0

func cmp*(a, b: BigNum): int =
  let n = max(a.n, b.n)
  result = cmpLimbs(a.limbs, b.limbs, n)

func subLimbs(a: var Limbs; b: Limbs; n: LimbCount): uint32 =
  ## `a -= b` over `n` limbs; returns the borrow.
  var borrow = 0'u64
  for i in 0 ..< n:
    let d = uint64(a[i]) - uint64(b[i]) - borrow
    a[i] = uint32(d and 0xFFFFFFFF'u64)
    borrow = (d shr 63) and 1'u64
  result = uint32(borrow)

func addLimbs(a: var Limbs; b: Limbs; n: LimbCount): uint32 =
  var carry = 0'u64
  for i in 0 ..< n:
    let s = uint64(a[i]) + uint64(b[i]) + carry
    a[i] = uint32(s and 0xFFFFFFFF'u64)
    carry = s shr 32
  result = uint32(carry)

# ------------------------------------------------------------- Montgomery ---

func initMont*(m: BigNum; res: var Mont): bool =
  ## `false` for an even or trivially small modulus.
  res = Mont(m: m, m0inv: 0'u32, r2: default(Limbs))
  if (m.limbs[0] and 1'u32) == 0'u32: return false
  if m.n == 1 and m.limbs[0] < 3'u32: return false
  # Newton: each step doubles the number of correct low bits of m^-1.
  var inv = 1'u32
  for i in 0 ..< 5:
    inv = inv * (2'u32 - m.limbs[0] * inv)
  res.m0inv = 0'u32 - inv
  # R^2 mod m by doubling 1 (2 * 32 * n) times, reducing as we go.
  var x = default(Limbs)
  x[0] = 1'u32
  let n = m.n
  for i in 0 ..< 64 * n:
    let y = x
    let carry = addLimbs(x, y, n)
    if carry != 0'u32 or cmpLimbs(x, m.limbs, n) >= 0:
      discard subLimbs(x, m.limbs, n)
  res.r2 = x
  result = true

func montMul*(ctx: Mont; a, b: Limbs): Limbs =
  ## `a * b * R^-1 mod m` (CIOS). Operands must be below `m`.
  let n = ctx.m.n
  var t = default(Limbs)
  for i in 0 ..< n:
    var c = 0'u64
    let bi = uint64(b[i])
    for j in 0 ..< n:
      let s = uint64(t[j]) + uint64(a[j]) * bi + c
      t[j] = uint32(s and 0xFFFFFFFF'u64)
      c = s shr 32
    var s = uint64(t[n]) + c
    t[n] = uint32(s and 0xFFFFFFFF'u64)
    t[n+1] = uint32(s shr 32)
    let q = uint64(t[0] * ctx.m0inv)
    s = uint64(t[0]) + q * uint64(ctx.m.limbs[0])
    c = s shr 32
    for j in 1 ..< n:
      s = uint64(t[j]) + q * uint64(ctx.m.limbs[j]) + c
      t[j-1] = uint32(s and 0xFFFFFFFF'u64)
      c = s shr 32
    s = uint64(t[n]) + c
    t[n-1] = uint32(s and 0xFFFFFFFF'u64)
    t[n] = t[n+1] + uint32(s shr 32)
    t[n+1] = 0'u32
  if t[n] != 0'u32 or cmpLimbs(t, ctx.m.limbs, n) >= 0:
    discard subLimbs(t, ctx.m.limbs, n)
  t[n] = 0'u32
  result = t

func toMont*(ctx: Mont; a: Limbs): Limbs {.inline.} = montMul(ctx, a, ctx.r2)

func fromMont*(ctx: Mont; a: Limbs): Limbs =
  var one = default(Limbs)
  one[0] = 1'u32
  result = montMul(ctx, a, one)

func reduce*(ctx: Mont; a: BigNum): Limbs =
  ## `a mod m` for any `a` of at most `m.n` limbs below `2 * m`... and more
  ## generally for any `a` that fits `m.n` limbs: subtract until below.
  result = a.limbs
  let n = ctx.m.n
  var i = n
  while i < MaxLimbs:
    result[i] = 0'u32
    inc i
  if a.n > n:
    # wider than the modulus: fold through Montgomery, `a * R^-1 * R^2 * R^-1`
    # would need a double-width product — reject instead (never needed here).
    for j in 0 ..< MaxLimbs: result[j] = 0'u32
    return
  while cmpLimbs(result, ctx.m.limbs, n) >= 0:
    discard subLimbs(result, ctx.m.limbs, n)

func modAdd*(ctx: Mont; a, b: Limbs): Limbs =
  result = a
  let carry = addLimbs(result, b, ctx.m.n)
  if carry != 0'u32 or cmpLimbs(result, ctx.m.limbs, ctx.m.n) >= 0:
    discard subLimbs(result, ctx.m.limbs, ctx.m.n)

func modSub*(ctx: Mont; a, b: Limbs): Limbs =
  result = a
  let borrow = subLimbs(result, b, ctx.m.n)
  if borrow != 0'u32:
    discard addLimbs(result, ctx.m.limbs, ctx.m.n)

func montPow*(ctx: Mont; base: Limbs; exp: BigNum): Limbs =
  ## `base^exp` with `base` and the result in Montgomery form. Left-to-right
  ## square-and-multiply: variable time, for public exponents only.
  var one = default(Limbs)
  one[0] = 1'u32
  result = toMont(ctx, one)
  var i = exp.bitLen - 1
  while i >= 0:
    result = montMul(ctx, result, result)
    let limb = i div 32
    if limb >= 0 and limb < MaxLimbs and ((exp.limbs[limb] shr (i mod 32)) and 1'u32) != 0'u32:
      result = montMul(ctx, result, base)
    dec i

func modExp*(base, exp: BigNum; ctx: Mont): BigNum =
  ## `base^exp mod m`.
  let b = toMont(ctx, reduce(ctx, base))
  result = BigNum(limbs: fromMont(ctx, montPow(ctx, b, exp)), n: ctx.m.n)

func montInverse*(ctx: Mont; a: Limbs): Limbs =
  ## `a^-1` for a PRIME modulus by Fermat, Montgomery form in and out.
  var e = ctx.m
  # m - 2; m is odd and >= 3, so the low limb does not underflow past 1
  var two = default(Limbs)
  two[0] = 2'u32
  discard subLimbs(e.limbs, two, e.n)
  result = montPow(ctx, a, e)

proc fromHex*(hex: string): BigNum =
  ## For curve and test constants written as hex. Panics on a malformed or
  ## oversized literal, which is a programming error, not input.
  var bytes = newSeq[byte]((hex.len + 1) div 2)
  var j = bytes.len - 1
  var i = hex.len - 1
  var lowNibble = true
  while i >= 0 and j >= 0:
    let ch = hex[i]
    var v = 0'u8
    if ch in {'0'..'9'}: v = byte(ord(ch) - ord('0'))
    elif ch in {'a'..'f'}: v = byte(ord(ch) - ord('a') + 10)
    elif ch in {'A'..'F'}: v = byte(ord(ch) - ord('A') + 10)
    else: panic "bignum.fromHex: not a hex digit"
    if j < bytes.len:
      if lowNibble:
        bytes[j] = v
      else:
        bytes[j] = bytes[j] or (v shl 4)
        dec j
    lowNibble = not lowNibble
    dec i
  result = default(BigNum)
  if not fromBytesBE(bytes, result): panic "bignum.fromHex: too wide"
