#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## RSA signature VERIFICATION (RFC 8017): PKCS #1 v1.5, which most
## certificate chains are signed with, and PSS, which TLS 1.3 demands for an
## RSA `CertificateVerify`.
##
## Only the public operation lives here, so `std/crypto/bignum`'s
## variable-time arithmetic is fine.

{.feature: "staticContracts".}

import std/crypto/[bignum, sha2, hmac]

type
  RsaHash* = enum
    rhSha256, rhSha384, rhSha512

  RsaPublicKey* = object
    n*, e*: BigNum
    modBytes*: int          ## `k`, the modulus length in bytes

func digestLen*(h: RsaHash): int =
  case h
  of rhSha256: 32
  of rhSha384: 48
  of rhSha512: 64

func hashOf(h: RsaHash; data: openArray[byte]): seq[byte] =
  result = @[]
  case h
  of rhSha256:
    for b in sha256(data): result.add b
  of rhSha384:
    for b in sha384(data): result.add b
  of rhSha512:
    for b in sha512(data): result.add b

func initRsaPublicKey*(modulus, exponent: openArray[byte]; key: var RsaPublicKey): bool =
  ## From the big-endian integers of an `RSAPublicKey`. Refuses moduli below
  ## 2048 bits — anything smaller has not been safe to trust for years.
  key = RsaPublicKey(n: default(BigNum), e: default(BigNum), modBytes: 0)
  if not fromBytesBE(modulus, key.n) or not fromBytesBE(exponent, key.e): return false
  let bits = key.n.bitLen
  if bits < 2048: return false
  if key.e.isZero or (key.e.limbs[0] and 1'u32) == 0'u32: return false
  key.modBytes = (bits + 7) div 8
  result = true

func publicOp(key: RsaPublicKey; sig: openArray[byte]; em: var seq[byte]): bool =
  ## `sig^e mod n` as a `k`-byte string; `false` for a signature of the wrong
  ## length or not below the modulus.
  result = false
  if sig.len != key.modBytes: return
  var s = default(BigNum)
  if not fromBytesBE(sig, s): return
  if cmp(s, key.n) >= 0: return
  var ctx = default(Mont)
  if not initMont(key.n, ctx): return
  if key.modBytes < 0: return
  em = toBytesBE(modExp(s, key.e, ctx), key.modBytes)
  result = true

const
  # DER of `DigestInfo` up to the digest itself (RFC 8017 §9.2, note 1)
  Sha256Prefix = [0x30'u8, 0x31, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01,
                  0x65, 0x03, 0x04, 0x02, 0x01, 0x05, 0x00, 0x04, 0x20]
  Sha384Prefix = [0x30'u8, 0x41, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01,
                  0x65, 0x03, 0x04, 0x02, 0x02, 0x05, 0x00, 0x04, 0x30]
  Sha512Prefix = [0x30'u8, 0x51, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01,
                  0x65, 0x03, 0x04, 0x02, 0x03, 0x05, 0x00, 0x04, 0x40]

func rsaVerifyPkcs1v15*(key: RsaPublicKey; h: RsaHash; message, signature: openArray[byte]): bool =
  ## RSASSA-PKCS1-v1_5. The expected encoding is BUILT and compared whole —
  ## parsing the decrypted block instead is how Bleichenbacher's 2006 forgery
  ## got through so many verifiers.
  result = false
  var em: seq[byte] = @[]
  if not publicOp(key, signature, em): return
  var t: seq[byte] = @[]
  case h
  of rhSha256:
    for b in Sha256Prefix: t.add b
  of rhSha384:
    for b in Sha384Prefix: t.add b
  of rhSha512:
    for b in Sha512Prefix: t.add b
  for b in hashOf(h, message): t.add b
  if em.len < t.len + 11: return
  var expected: seq[byte] = @[0'u8, 1'u8]
  for i in 0 ..< em.len - t.len - 3: expected.add 0xFF'u8
  expected.add 0'u8
  for b in t: expected.add b
  result = constantTimeEquals(em, expected)

func mgf1(h: RsaHash; seed: openArray[byte]; maskLen: int): seq[byte] =
  result = @[]
  var counter = 0'u32
  while result.len < maskLen:
    var input: seq[byte] = @[]
    for b in seed: input.add b
    input.add byte(counter shr 24)
    input.add byte((counter shr 16) and 0xFF'u32)
    input.add byte((counter shr 8) and 0xFF'u32)
    input.add byte(counter and 0xFF'u32)
    for b in hashOf(h, input):
      if result.len < maskLen: result.add b
    inc counter

func rsaVerifyPss*(key: RsaPublicKey; h: RsaHash; message, signature: openArray[byte];
                   saltLen: int): bool =
  ## RSASSA-PSS with MGF1 over the same hash. TLS 1.3 fixes `saltLen` to the
  ## digest length; certificates say it in their algorithm parameters.
  result = false
  if saltLen < 0: return
  var em: seq[byte] = @[]
  if not publicOp(key, signature, em): return
  let hLen = digestLen(h)
  let emBits = key.n.bitLen - 1
  let emLen = (emBits + 7) div 8
  # emLen is k or k - 1; in the latter case the leading byte of `em` is zero
  var off = em.len - emLen
  if off < 0 or off > 1: return
  if off == 1 and em.len > 0 and em[0] != 0'u8: return
  if emLen < hLen + saltLen + 2: return
  let last = off + emLen - 1
  if last < 0 or last >= em.len: return
  if em[last] != 0xBC'u8: return
  let dbLen = emLen - hLen - 1
  var maskedDb: seq[byte] = @[]
  for i in 0 ..< dbLen:
    let j = off + i
    if j >= 0 and j < em.len: maskedDb.add em[j]
  var hh: seq[byte] = @[]
  for i in 0 ..< hLen:
    let j = off + dbLen + i
    if j >= 0 and j < em.len: hh.add em[j]
  if maskedDb.len != dbLen or hh.len != hLen: return
  # the top `8*emLen - emBits` bits of maskedDB must be zero
  let topBits = 8 * emLen - emBits
  if topBits < 0 or topBits > 8: return
  let topMask = byte((0xFF00 shr topBits) and 0xFF)
  if maskedDb.len > 0 and (maskedDb[0] and topMask) != 0'u8: return
  let mask = mgf1(h, hh, dbLen)
  var db: seq[byte] = @[]
  for i in 0 ..< maskedDb.len:
    if i < mask.len: db.add(maskedDb[i] xor mask[i])
  if db.len != dbLen: return
  if db.len > 0: db[0] = db[0] and (not topMask)
  # DB = PS (zeros) || 0x01 || salt
  let psLen = emLen - hLen - saltLen - 2
  for i in 0 ..< psLen:
    if i < db.len and db[i] != 0'u8: return
  if psLen < 0 or psLen >= db.len or db[psLen] != 1'u8: return
  var mPrime: seq[byte] = @[0'u8, 0, 0, 0, 0, 0, 0, 0]
  for b in hashOf(h, message): mPrime.add b
  for i in db.len - saltLen ..< db.len:
    if i >= 0 and i < db.len: mPrime.add db[i]
  result = constantTimeEquals(hashOf(h, mPrime), hh)
