#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## RSA signing (RFC 8017): RSASSA-PKCS1-v1_5 with SHA-256/384/512, which is
## JWT's `RS256`/`RS384`/`RS512`, from an RSA private key.
##
## A key is read from PKCS #8 (`-----BEGIN PRIVATE KEY-----`, what Google
## service-account JSON files and `openssl genpkey` hold) or PKCS #1
## (`-----BEGIN RSA PRIVATE KEY-----`), as PEM or DER. Encrypted keys,
## multi-prime keys and moduli outside 2048..4096 bits are refused.
##
## Unlike `std/crypto/rsa`, which only does the public operation, this module
## holds secrets: the parser never branches on, or indexes a table with, a
## byte of the private key. Only the layout — lengths, tags, the modulus and
## the public exponent — steers it. Signing (`rsaSignPkcs1v15`) runs on
## `std/crypto/private/ctbignum`'s constant-time arithmetic (`{.assembler.}`
## code on the native x86-64 backend), blinded, and checks every signature
## against the public key before releasing it. A key is wiped when it is
## destroyed; temporaries the C compiler leaves on the stack are not.

{.feature: "staticContracts".}

import std/[strutils, volatile]
import std/crypto/[bignum, rsa, sysrand]
import std/crypto/private/[der, pem, ctbignum]

type
  RsaKeyError* = enum
    ## Why a key was refused.
    rkeNone = "ok"
    rkePem = "no PEM private key block"
    rkeEncrypted = "encrypted private keys are not supported"
    rkeBase64 = "invalid base64 in the PEM body"
    rkeDer = "malformed DER"
    rkeNotRsa = "not an RSA (rsaEncryption) key"
    rkeVersion = "unsupported key version (multi-prime keys are not supported)"
    rkeModulusSize = "modulus is not between 2048 and 4096 bits"
    rkeExponent = "public exponent is even, below 3, or not below the modulus"
    rkePrimes = "p or q is even or not half the width of the modulus"
    rkeCrtParams = "dP, dQ or qInv is wider than half the modulus"
    rkeMismatch = "p * q is not the modulus"

  RsaSignError* = enum
    ## Why `rsaSignPkcs1v15` produced no signature.
    rseNone = "ok"
    rseKey = "the key is empty (never parsed, or its parse failed)"
    rseRandom = "the operating system's random number generator failed"
    rseFault = "the signature failed its check: a corrupt key or a hardware fault"

  RsaPrivateKey* = object
    ## An RSA private key in CRT form, ready to sign. Its fields are private;
    ## it is zeroed when destroyed.
    n, e: BigNum            # public
    modBits: int            # public: the bit length of `n`; 0 for no key
    crt: RsaCrt             # p, q, dP, dQ, qInv and their Montgomery contexts

const
  MinModulusBits = 2048
  MaxModulusBits = MaxBits

proc `=destroy`*(key: RsaPrivateKey) =
  ## Zeroes the whole key, public half included.
  let p = cast[ptr UncheckedArray[byte]](addr key)
  for i in 0 ..< sizeof(RsaPrivateKey):
    volatileStore(addr p[i], 0'u8)

proc `=wasMoved`*(key: var RsaPrivateKey) {.nodestroy, inline.} =
  ## A moved-from key holds nothing worth wiping.
  key.modBits = 0

func primeBits(modBits: int): int {.inline.} =
  ## The most bits `p` or `q` may have: the larger half of the modulus.
  (modBits + 1) div 2

func primeLimbs(modBits: int): int {.inline.} =
  (primeBits(modBits) + 31) div 32

func publicKey*(key: RsaPrivateKey): RsaPublicKey =
  ## The public half, for `rsaVerifyPkcs1v15` or to publish.
  result = RsaPublicKey(n: key.n, e: key.e, modBytes: (key.modBits + 7) div 8)

func modulusBits*(key: RsaPrivateKey): int =
  ## The bit length of the modulus; 0 for a key that was never parsed.
  key.modBits

# -------------------------------------------------- branch-free integers ---

func isZeroMask(x: uint32): uint32 {.inline.} =
  ## All ones when `x == 0`, else zero.
  result = 0'u32 - (((x or (0'u32 - x)) shr 31) xor 1'u32)

func badEncoding(d: openArray[byte]; t: Tlv): uint32 =
  ## All ones when the INTEGER at `t` is negative or not minimal. Looks at
  ## its first two bytes through masks only. `t.len >= 1`.
  result = 0'u32
  if t.start < 0 or t.start >= d.len: return 0xFFFF_FFFF'u32
  let b0 = uint32(d[t.start])
  var b1 = 0x80'u32                      # no second byte: nothing to check
  let at = t.start + 1
  if t.len >= 2 and at < d.len: b1 = uint32(d[at])
  let negative = 0'u32 - (b0 shr 7)
  let redundantZero = isZeroMask(b0) and (0'u32 - ((b1 shr 7) xor 1'u32))
  result = negative or redundantZero

func loadInt(d: openArray[byte]; t: Tlv; limbs: int; x: var BigNum) =
  ## The magnitude of the INTEGER at `t` into `x`, `x.n = limbs`. Every byte
  ## goes to a position fixed by its offset from the end; a sign byte adds
  ## zero. `t.len` must be at most `4 * limbs + 1`, so nothing is dropped
  ## except that zero byte.
  x = BigNum(limbs: default(Limbs), n: max(1, min(limbs, MaxLimbs - 2)))
  for k in 0 ..< t.len:
    let i = t.next - 1 - k               # the k-th byte from the end
    let limb = k div 4
    if i >= 0 and i < d.len and limb >= 0 and limb < MaxLimbs:
      x.limbs[limb] = x.limbs[limb] or (uint32(d[i]) shl uint32((k mod 4) * 8))

func bitsFrom(x: BigNum; bits: int): uint32 =
  ## Nonzero when `x` has a set bit at position `bits` or above. The masks
  ## depend on `bits` and the limb index only.
  result = 0'u32
  for i in 0 ..< MaxLimbs:
    var m = 0'u32
    if i * 32 >= bits: m = 0xFFFF_FFFF'u32
    elif (i + 1) * 32 > bits: m = not ((1'u32 shl uint32(bits - i * 32)) - 1'u32)
    result = result or (x.limbs[i] and m)

proc wipe(x: var BigNum) =
  for i in 0 ..< MaxLimbs:
    volatileStore(addr x.limbs[i], 0'u32)

# ----------------------------------------------------------------- PKCS#1 ---

func versionIs(d: openArray[byte]; t: Tlv; v: byte): bool =
  result = false
  if t.tag == TagInt and t.len == 1 and t.start >= 0 and t.start < d.len:
    result = d[t.start] == v

func publicInt(d: openArray[byte]; t: Tlv; x: var BigNum): bool =
  ## A public INTEGER: positive, minimal, at most `MaxBits` wide.
  result = false
  if t.tag != TagInt or t.len < 1 or t.len > MaxBits div 8 + 1: return
  if badEncoding(d, t) != 0'u32: return
  result = fromBytesBE(bytesOf(d, t.start, t.len), x)

proc parsePkcs1(d: openArray[byte]; s: Tlv; key: var RsaPrivateKey): RsaKeyError =
  ## `RSAPrivateKey ::= SEQUENCE { version, n, e, d, p, q, dP, dQ, qInv }`
  ## spanning `s`, which the caller has checked is a SEQUENCE.
  var f = default(array[9, Tlv])
  var p = s.start
  var count = 0
  var t = default(Tlv)
  while p < s.next:
    if not readTlv(d, p, s.next, t): return rkeDer
    if count == 0 and not versionIs(d, t, 0'u8):
      return (if versionIs(d, t, 1'u8): rkeVersion else: rkeDer)
    if count >= f.len or t.tag != TagInt or t.len < 1: return rkeDer
    f[count] = t
    inc count
    p = t.next
  if count != f.len: return rkeDer
  # the public half
  var n = default(BigNum)
  var e = default(BigNum)
  if not publicInt(d, f[1], n):
    # a well-formed positive INTEGER fails only by being wider than
    # `MaxBits`: a modulus too large, not malformed DER
    return (if badEncoding(d, f[1]) == 0'u32: rkeModulusSize else: rkeDer)
  if not publicInt(d, f[2], e): return rkeDer
  let bits = n.bitLen
  if bits < MinModulusBits or bits > MaxModulusBits: return rkeModulusSize
  if (e.limbs[0] and 1'u32) == 0'u32 or e.bitLen < 2 or cmp(e, n) >= 0:
    return rkeExponent
  # the secret half: only its DER lengths may decide anything before the end
  let half = primeBits(bits)
  let limbs = primeLimbs(bits)
  if f[3].len > (bits + 7) div 8 + 1: return rkeDer
  for i in 4 .. 8:
    if f[i].len > 4 * limbs + 1:
      return (if i <= 5: rkePrimes else: rkeCrtParams)
  var encoding = 0'u32
  for i in 3 .. 8: encoding = encoding or badEncoding(d, f[i])
  var prime1, prime2, exponent1, exponent2, coefficient = default(BigNum)
  loadInt(d, f[4], limbs, prime1)
  loadInt(d, f[5], limbs, prime2)
  loadInt(d, f[6], limbs, exponent1)
  loadInt(d, f[7], limbs, exponent2)
  loadInt(d, f[8], limbs, coefficient)
  let primes = bitsFrom(prime1, half) or bitsFrom(prime2, half) or
               ((prime1.limbs[0] and prime2.limbs[0] and 1'u32) xor 1'u32)
  let crt = bitsFrom(exponent1, half) or bitsFrom(exponent2, half) or
            bitsFrom(coefficient, half)
  result = rkeNone
  if encoding != 0'u32: result = rkeDer
  elif primes != 0'u32: result = rkePrimes
  elif crt != 0'u32: result = rkeCrtParams
  else:
    key = RsaPrivateKey(n: n, e: e, modBits: bits)
    # `p * q == n` by ctbignum's constant-time product (`ctMul`); the public
    # shape `initRsaCrt` also checks holds here, so `false` means a mismatch
    if not initRsaCrt(n, e, prime1, prime2, exponent1, exponent2, coefficient,
                      key.crt):
      result = rkeMismatch
  wipe(prime1)
  wipe(prime2)
  wipe(exponent1)
  wipe(exponent2)
  wipe(coefficient)

# ----------------------------------------------------------------- PKCS#8 ---

type KeyFormat = enum kfAny, kfPkcs8, kfPkcs1

proc parsePkcs8(d: openArray[byte]; s: Tlv; key: var RsaPrivateKey): RsaKeyError =
  ## `PrivateKeyInfo ::= SEQUENCE { version 0, AlgorithmIdentifier,
  ## privateKey OCTET STRING, [0] attributes OPTIONAL }`.
  var ver, alg, oid, octets, inner = default(Tlv)
  if not readTlv(d, s.start, s.next, ver) or ver.tag != TagInt: return rkeDer
  if not versionIs(d, ver, 0'u8):
    return (if versionIs(d, ver, 1'u8): rkeVersion else: rkeDer)
  if not readTlv(d, ver.next, s.next, alg) or alg.tag != TagSeq: return rkeDer
  if not readTlv(d, alg.start, alg.next, oid) or oid.tag != TagOid: return rkeDer
  if not sameBytes(d, oid.start, oid.len, OidRsaEncryption): return rkeNotRsa
  if oid.next != alg.next:               # parameters: NULL or absent
    var params = default(Tlv)
    if not readTlv(d, oid.next, alg.next, params) or params.tag != TagNull or
       params.len != 0 or params.next != alg.next: return rkeDer
  if not readTlv(d, alg.next, s.next, octets) or octets.tag != TagOctetString:
    return rkeDer
  if octets.next != s.next:              # [0] attributes, then nothing
    var attrs = default(Tlv)
    if not readTlv(d, octets.next, s.next, attrs) or attrs.tag != 0xA0'u8 or
       attrs.next != s.next: return rkeDer
  if not readTlv(d, octets.start, octets.next, inner) or inner.tag != TagSeq or
     inner.next != octets.next: return rkeDer
  result = parsePkcs1(d, inner, key)

proc parseDer(der: openArray[byte]; format: KeyFormat;
              key: var RsaPrivateKey): RsaKeyError =
  key = default(RsaPrivateKey)
  var s, first, second = default(Tlv)
  if not readTlv(der, 0, der.len, s) or s.tag != TagSeq or s.next != der.len:
    return rkeDer
  if not readTlv(der, s.start, s.next, first) or
     not readTlv(der, first.next, s.next, second): return rkeDer
  # PKCS #8 has the AlgorithmIdentifier SEQUENCE second, PKCS #1 the modulus
  if second.tag == TagSeq and format != kfPkcs1: result = parsePkcs8(der, s, key)
  elif second.tag == TagInt and format != kfPkcs8: result = parsePkcs1(der, s, key)
  else: result = rkeDer
  if result != rkeNone: key = default(RsaPrivateKey)

proc parseRsaPrivateKeyDer*(der: openArray[byte]; key: var RsaPrivateKey): RsaKeyError =
  ## Reads a PKCS #8 `PrivateKeyInfo` holding an `rsaEncryption` key or a
  ## PKCS #1 `RSAPrivateKey`; which one it is, is detected. Returns `rkeNone`
  ## on success. On failure `key` is empty and the result says why.
  ##
  ## Checks: strict DER with nothing after the key, version 0 (two primes),
  ## a 2048..4096-bit modulus, an odd public exponent `3 <= e < n`, odd `p`
  ## and `q` and CRT values of at most `ceil(bits / 2)` bits each (the larger
  ## half of the modulus width), and `p * q == n`. With `p * q == n` that
  ## makes `p` and `q` exactly half the width for an even bit length; for an
  ## odd one (2049, 4095) they are the two halves rounded up and down, or
  ## both rounded up.
  ## `d` must be well formed but is not kept: signing uses the CRT values.
  result = parseDer(der, kfAny, key)

proc parseRsaPrivateKeyPem*(pem: string; key: var RsaPrivateKey): RsaKeyError =
  ## Reads the first private key block of `pem`: `BEGIN PRIVATE KEY`
  ## (PKCS #8) or `BEGIN RSA PRIVATE KEY` (PKCS #1). Text before it and
  ## other blocks before it (certificates) are skipped. An encrypted key
  ## (`BEGIN ENCRYPTED PRIVATE KEY`, or `Proc-Type: 4,ENCRYPTED` in a PKCS #1
  ## block) gives `rkeEncrypted`; another kind of key (`BEGIN EC PRIVATE
  ## KEY`) gives `rkeNotRsa`. See `parseRsaPrivateKeyDer` for the checks.
  ##
  ## The base64 body is decoded without table lookups, and the decoded DER
  ## is wiped before this returns.
  const
    Dashes = "-----"
    BeginTag = "-----BEGIN "
    Labels = ["PRIVATE KEY", "RSA PRIVATE KEY", "ENCRYPTED PRIVATE KEY",
              "EC PRIVATE KEY", "DSA PRIVATE KEY", "OPENSSH PRIVATE KEY"]
  key = default(RsaPrivateKey)
  result = rkePem
  # The text before the key block is public, so a plain search is fine for it.
  var pos = 0
  var label = -1
  var bodyStart = 0
  while label < 0:
    if pos < 0: return rkePem
    let at = find(pem, BeginTag, pos)
    if at < 0: return rkePem
    let l = at + BeginTag.len
    for i in 0 ..< Labels.len:
      if label < 0 and ctContinuesWith(pem, l, Labels[i] & Dashes):
        label = i
        bodyStart = l + Labels[i].len + Dashes.len
    pos = l
  if label < 0 or label >= Labels.len: return rkePem
  case label
  of 0, 1: discard
  of 2: return rkeEncrypted
  else: return rkeNotRsa
  # RFC 1421 headers follow the BEGIN line's LF or CRLF; PKCS #1 encryption
  # is announced there
  let lf = ctContinuesWith(pem, bodyStart + 1, "Proc-Type:")
  let crlf = ctContinuesWith(pem, bodyStart + 2, "Proc-Type:")
  if label == 1 and (lf or crlf): return rkeEncrypted
  let endLine = "-----END " & Labels[label] & Dashes
  # From here on every byte may be secret.
  let bodyEnd = ctFindByte(pem, bodyStart, '-')
  if not ctContinuesWith(pem, bodyEnd, endLine): return rkePem
  if bodyEnd < bodyStart or bodyEnd > pem.len: return rkePem
  var der: seq[byte] = @[]
  if not ctBase64Decode(pem.toOpenArray(bodyStart, bodyEnd - 1), der):
    return rkeBase64
  result = parseDer(der, (if label == 0: kfPkcs8 else: kfPkcs1), key)
  wipe(der)

# ---------------------------------------------------------------- signing ---

proc rsaSignPkcs1v15*(key: RsaPrivateKey; h: RsaHash; message: openArray[byte];
                      signature: var seq[byte]): RsaSignError =
  ## RSASSA-PKCS1-v1_5 (RFC 8017 §8.2.1): signs `message` with the hash `h`
  ## (`rhSha256` for JWT's `RS256`). The signature is `k` bytes, `k` the
  ## modulus length, and is the same for the same key and message every time:
  ## it equals `openssl dgst -sha256 -sign`'s. It verifies with
  ## `rsaVerifyPkcs1v15(publicKey(key), h, message, signature)`.
  ##
  ## Returns `rseNone` with the signature in `signature`, or the reason with
  ## `signature` empty: `rseKey` for a key that was never parsed, `rseRandom`
  ## when the OS random number generator cannot be read (there is no
  ## unblinded fallback), `rseFault` when the result fails its check.
  ##
  ## The private operation is ctbignum's constant-time CRT exponentiation
  ## with fresh base blinding per prime (from `std/crypto/sysrand`). Before
  ## a signature is released it is checked against the public key, `s^e mod
  ## n == EM`, also in constant time (the candidate is still secret until it
  ## passes), so a wrong `dP`, `dQ` or `qInv` in the key or a fault during
  ## the computation never leaks a signature that would give away a factor
  ## of `n` (Boneh, DeMillo and Lipton).
  ##
  ## A JWT signed with `RS256` (RFC 7515, 7518), for example with the
  ## `private_key` of a Google service-account file. `tests/nimony/crypto/
  ## trsasign.nim` runs this code verbatim and checks it against OpenSSL.
  ## (It is not a `runnableExamples` block: an `import` inside one breaks
  ## the build of modules that import this one, a Nimony bug.)
  ##
  ## ```nim
  ## import std/base64
  ## import std/crypto/[rsa, rsasign]
  ##
  ## proc base64Url(data: openArray[byte]): string =
  ##   ## base64url without padding, as JWS wants it.
  ##   result = encode(data, safe = true)
  ##   while result.len > 0 and result[result.len - 1] == '=':
  ##     result.setLen(result.len - 1)
  ##
  ## proc base64Url(s: string): string =
  ##   var b: seq[byte] = @[]
  ##   for c in s: b.add byte(c)
  ##   result = base64Url(b)
  ##
  ## proc signJwt(pem, claims: string; jwt: var string): bool =
  ##   ## `header.claims.signature`; `false` if the key is refused.
  ##   jwt = ""
  ##   var key = default(RsaPrivateKey)
  ##   if parseRsaPrivateKeyPem(pem, key) != rkeNone: return false
  ##   let input = base64Url("""{"alg":"RS256","typ":"JWT"}""") & "." &
  ##               base64Url(claims)
  ##   var bytes: seq[byte] = @[]
  ##   for c in input: bytes.add byte(c)
  ##   var sig: seq[byte] = @[]
  ##   if rsaSignPkcs1v15(key, rhSha256, bytes, sig) != rseNone: return false
  ##   jwt = input & "." & base64Url(sig)
  ##   result = true
  ## ```
  signature = @[]
  let k = (key.modBits + 7) div 8
  if k <= 0: return rseKey               # never parsed: `modBits` is 0
  var em: seq[byte] = @[]
  var c = default(BigNum)
  # neither can fail for a parsed key: `k` is 256..512 bytes
  if not emsaPkcs1v15Encode(h, message, k, em): return rseKey
  if not fromBytesBE(em, c): return rseKey
  let blindLen = rsaCrtBlindLen(key.crt)
  if blindLen <= 0: return rseKey
  var blindP = newSeq[byte](blindLen)
  var blindQ = newSeq[byte](blindLen)
  var s = default(BigNum)
  result = rseRandom
  if urandom(blindP) and urandom(blindQ):
    result = rseFault
    if rsaCrtPrivate(key.crt, c, blindP, blindQ, s):
      signature = toBytesBE(s, k)
      result = rseNone
  wipe(blindP)
  wipe(blindQ)
