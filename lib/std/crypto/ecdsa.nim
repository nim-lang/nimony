#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## ECDSA signature VERIFICATION over NIST P-256 and P-384 (FIPS 186-5).
##
## These are the two curves certificates and TLS 1.3 `CertificateVerify`
## messages are signed with. Verification handles only public values, so the
## arithmetic is `std/crypto/bignum`'s variable-time Montgomery code.

{.feature: "staticContracts".}

import std/crypto/bignum

type
  Curve* = enum
    P256, P384

  CurveParams = object
    fp, fn: Mont           ## the field and the group order
    b, gx, gy: Limbs       ## Montgomery form over `fp`
    bytes: int             ## coordinate width

  Point = object
    ## Jacobian `(X/Z^2, Y/Z^3)`, Montgomery form; `Z == 0` is infinity.
    x, y, z: Limbs

proc params(c: Curve): CurveParams =
  var p, n, b, gx, gy: BigNum
  case c
  of P256:
    p = fromHex("ffffffff00000001000000000000000000000000ffffffffffffffffffffffff")
    n = fromHex("ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551")
    b = fromHex("5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b")
    gx = fromHex("6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296")
    gy = fromHex("4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5")
  of P384:
    p = fromHex("fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000ffffffff")
    n = fromHex("ffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf581a0db248b0a77aecec196accc52973")
    b = fromHex("b3312fa7e23ee7e4988e056be3f82d19181d9c6efe8141120314088f5013875ac656398d8a2ed19d2a85c8edd3ec2aef")
    gx = fromHex("aa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7")
    gy = fromHex("3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f")
  result = default(CurveParams)
  if not initMont(p, result.fp) or not initMont(n, result.fn):
    panic "ecdsa: bad curve constants"
  result.b = toMont(result.fp, b.limbs)
  result.gx = toMont(result.fp, gx.limbs)
  result.gy = toMont(result.fp, gy.limbs)
  result.bytes = (if c == P256: 32 else: 48)

func isZeroLimbs(a: Limbs): bool =
  result = true
  for i in 0 ..< MaxLimbs:
    if a[i] != 0'u32: return false

func sameLimbs(a, b: Limbs): bool =
  result = true
  for i in 0 ..< MaxLimbs:
    if a[i] != b[i]: return false

func dbl(f: Mont; p: Point): Point =
  ## dbl-2001-b, valid because both curves have `a = -3`.
  if isZeroLimbs(p.z): return p
  let delta = montMul(f, p.z, p.z)
  let gamma = montMul(f, p.y, p.y)
  let beta = montMul(f, p.x, gamma)
  let t = montMul(f, modSub(f, p.x, delta), modAdd(f, p.x, delta))
  let alpha = modAdd(f, modAdd(f, t, t), t)
  let beta2 = modAdd(f, beta, beta)
  let beta4 = modAdd(f, beta2, beta2)
  let beta8 = modAdd(f, beta4, beta4)
  let x3 = modSub(f, montMul(f, alpha, alpha), beta8)
  let yz = modAdd(f, p.y, p.z)
  let z3 = modSub(f, modSub(f, montMul(f, yz, yz), gamma), delta)
  let gg = montMul(f, gamma, gamma)
  let g2 = modAdd(f, gg, gg)
  let g4 = modAdd(f, g2, g2)
  let g8 = modAdd(f, g4, g4)
  let y3 = modSub(f, montMul(f, alpha, modSub(f, beta4, x3)), g8)
  result = Point(x: x3, y: y3, z: z3)

func add(f: Mont; p, q: Point): Point =
  ## add-2007-bl, with the cases it excludes handled first.
  if isZeroLimbs(p.z): return q
  if isZeroLimbs(q.z): return p
  let z1z1 = montMul(f, p.z, p.z)
  let z2z2 = montMul(f, q.z, q.z)
  let u1 = montMul(f, p.x, z2z2)
  let u2 = montMul(f, q.x, z1z1)
  let s1 = montMul(f, montMul(f, p.y, q.z), z2z2)
  let s2 = montMul(f, montMul(f, q.y, p.z), z1z1)
  let h = modSub(f, u2, u1)
  let sd = modSub(f, s2, s1)
  if isZeroLimbs(h):
    if isZeroLimbs(sd): return dbl(f, p)
    return Point(x: default(Limbs), y: default(Limbs), z: default(Limbs))
  let h2 = modAdd(f, h, h)
  let i = montMul(f, h2, h2)
  let j = montMul(f, h, i)
  let r = modAdd(f, sd, sd)
  let v = montMul(f, u1, i)
  let x3 = modSub(f, modSub(f, montMul(f, r, r), j), modAdd(f, v, v))
  let s1j = montMul(f, s1, j)
  let y3 = modSub(f, montMul(f, r, modSub(f, v, x3)), modAdd(f, s1j, s1j))
  let zz = modAdd(f, p.z, q.z)
  let z3 = montMul(f, modSub(f, modSub(f, montMul(f, zz, zz), z1z1), z2z2), h)
  result = Point(x: x3, y: y3, z: z3)

func bitAt(a: Limbs; i: int): bool =
  let limb = i div 32
  if limb >= 0 and limb < MaxLimbs:
    result = ((a[limb] shr (i mod 32)) and 1'u32) != 0'u32
  else:
    result = false

func shamir(f: Mont; u1: Limbs; g: Point; u2: Limbs; q: Point; bits: int): Point =
  ## `u1*G + u2*Q` by one shared doubling chain.
  let gq = add(f, g, q)
  result = Point(x: default(Limbs), y: default(Limbs), z: default(Limbs))
  var i = bits - 1
  while i >= 0:
    result = dbl(f, result)
    let a = bitAt(u1, i)
    let b = bitAt(u2, i)
    if a and b: result = add(f, result, gq)
    elif a: result = add(f, result, g)
    elif b: result = add(f, result, q)
    dec i

func onCurve(cp: CurveParams; x, y: Limbs): bool =
  ## `y^2 == x^3 - 3x + b`, Montgomery form.
  let f = cp.fp
  let y2 = montMul(f, y, y)
  let x3 = montMul(f, montMul(f, x, x), x)
  let x3a = modSub(f, modSub(f, modSub(f, x3, x), x), x)
  result = sameLimbs(y2, modAdd(f, x3a, cp.b))

func scalarBelowOrder(cp: CurveParams; data: openArray[byte]; res: var BigNum): bool =
  ## `1 <= v < n`, as SEC1 demands of `r` and `s`.
  result = fromBytesBE(data, res) and not res.isZero and cmp(res, cp.fn.m) < 0

proc ecdsaVerify*(curve: Curve; publicKey: openArray[byte]; digest: openArray[byte];
                  r, s: openArray[byte]): bool =
  ## Checks the signature `(r, s)` of `digest` under `publicKey`, an
  ## uncompressed SEC1 point (`04 || X || Y`). `r` and `s` are big-endian
  ## integers, leading zeros allowed. The digest is truncated to the order's
  ## width, so a SHA-384 digest may be checked against a P-256 key.
  let cp = params(curve)
  let w = cp.bytes
  if publicKey.len < 1 or publicKey[0] != 4'u8 or publicKey.len != 1 + 2 * w:
    return false
  var qx = default(BigNum)
  var qy = default(BigNum)
  if not fromBytesBE(publicKey.toOpenArray(1, w), qx): return false
  if not fromBytesBE(publicKey.toOpenArray(1 + w, 2 * w), qy): return false
  if cmp(qx, cp.fp.m) >= 0 or cmp(qy, cp.fp.m) >= 0: return false
  let qxm = toMont(cp.fp, qx.limbs)
  let qym = toMont(cp.fp, qy.limbs)
  if not onCurve(cp, qxm, qym): return false
  var rr = default(BigNum)
  var ss = default(BigNum)
  if not scalarBelowOrder(cp, r, rr) or not scalarBelowOrder(cp, s, ss): return false
  # e: the leftmost `w` bytes of the digest, then below n (one subtraction
  # suffices: n > 2^(8w - 1))
  var e = default(BigNum)
  let take = min(digest.len, w)
  if take > 0:
    if not fromBytesBE(digest.toOpenArray(0, take - 1), e): return false
  let fn = cp.fn
  let em = reduce(fn, e)
  let sInv = montInverse(fn, toMont(fn, ss.limbs))
  let u1 = fromMont(fn, montMul(fn, toMont(fn, em), sInv))
  let u2 = fromMont(fn, montMul(fn, toMont(fn, rr.limbs), sInv))
  var one = default(Limbs)
  one[0] = 1'u32
  let g = Point(x: cp.gx, y: cp.gy, z: toMont(cp.fp, one))
  let q = Point(x: qxm, y: qym, z: toMont(cp.fp, one))
  let x = shamir(cp.fp, u1, g, u2, q, w * 8)
  if isZeroLimbs(x.z): return false
  # affine x = X / Z^2, then mod n, compared to r
  let zInv = montInverse(cp.fp, x.z)
  let ax = fromMont(cp.fp, montMul(cp.fp, x.x, montMul(cp.fp, zInv, zInv)))
  let v = reduce(fn, BigNum(limbs: ax, n: cp.fp.m.n))
  result = sameLimbs(v, rr.limbs)

# ------------------------------------------------------------------- DER ----

func derInt(data: openArray[byte]; pos: var int; res: var seq[byte]): bool =
  ## One DER INTEGER at `pos`: tag 2, a short length, a positive value.
  result = false
  if pos < 0 or pos + 2 > data.len: return
  if data[pos] != 2'u8: return
  let L = int(data[pos + 1])
  if L == 0 or L >= 128: return
  let start = pos + 2
  if start + L > data.len: return
  res = @[]
  for i in start ..< start + L:
    if i < data.len: res.add data[i]
  pos = start + L
  result = true

proc ecdsaVerifyDer*(curve: Curve; publicKey, digest, signature: openArray[byte]): bool =
  ## `ecdsaVerify` with the signature in its X.509/TLS encoding,
  ## `SEQUENCE { INTEGER r, INTEGER s }`.
  result = false
  if signature.len < 8 or signature[0] != 0x30'u8: return
  var pos = 2
  var bodyLen = int(signature[1])
  if bodyLen == 0x81:
    bodyLen = int(signature[2])
    pos = 3
  elif bodyLen >= 0x80:
    return
  if pos + bodyLen != signature.len: return
  var r: seq[byte] = @[]
  var s: seq[byte] = @[]
  if not derInt(signature, pos, r): return
  if not derInt(signature, pos, s): return
  if pos != signature.len: return
  result = ecdsaVerify(curve, publicKey, digest, r, s)
