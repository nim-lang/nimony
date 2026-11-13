#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# The portable path of `std/crypto/private/ctbignum` (included by it when
# the build is not the native backend on x86-64): the module's API on top of
# `ctbignumi31`, a line-by-line translation of BearSSL's i31 code. The RSA
# private operation follows `src/rsa/rsa_i31_priv.c` of BearSSL commit
# 7bea48e5e850ab4cafbe68d3765cdaba13a86d6f (MIT licence, Copyright (c) 2016
# Thomas Pornin; the full notice is in `ctbignumi31.nim`); the lines that
# come from it cite `rsa_i31_priv.c:<line>`. What BearSSL does not have, the
# per-prime base blinding, the fault check and the API's 32-bit limbs, is
# Nimony's original design, built here from the i31 routines and marked
# "not in BearSSL".
#
# Representation at the boundary: the API's numbers are `Limbs` (32-bit
# words, little-endian, the count public). They enter the i31 code as
# big-endian bytes through `br_i31_decode` / `br_i31_decode_reduce` and leave
# it through `br_i31_encode`, which is how BearSSL itself moves numbers in
# and out (rsa_i31_priv.c:93, 143, 196). On this path the "Montgomery form"
# of the `CtMont` routines is BearSSL's: `x * R mod m` with
# `R = 2^(31 * len)`, `len` the i31 word count of `m`, not `2^(32 * m.n)`.
# The values are still ordinary numbers below `m` in `Limbs`; only the
# factor differs from the native path's, and nothing outside this module
# sees a value in Montgomery form.
#
# Operands of the Montgomery routines (`ctMontMul`, `ctToMont`,
# `ctFromMont`, `ctModMul`, `ctModSub`, `ctMontPow`) must be below `m`, as
# BearSSL requires: `toI31` decodes them into `m`'s `len` words, and the
# bits at and above `31 * len` are not read. When `31 * len < 32 * m.n`
# (moduli of 513, 1025, 2049 or 3073 bits, for example) that drops bits an
# operand of `m.n` limbs can have, so an operand that is only below
# `2^(32 * m.n)` gives a wrong result. `ctMod` takes any width.
#
# What BearSSL leaks and this path therefore leaks too: the BIT length of
# each modulus. `br_i31_decode` computes it without a branch, but it is the
# i31 header (word 0), and the loops and the normalisation in
# `br_i31_muladd_small` (i31_muladd.c:38-41: "we accept to leak that
# length") run on it. For `p` and `q` that is the bit length of the primes;
# rsa_i31_priv.c:44-47 counts their lengths as public. The limb counts that
# the native path leaks are public here as well.

import std/crypto/private/ctbignumi31

type
  HalfLimbCount* = range[1 .. (MaxLimbs - 2) div 2]
    ## The limb count of an RSA prime: two of them fit a `BigNum`.

const
  BrMaxRsaSize = MaxBits                       # inner.h:56 BR_MAX_RSA_SIZE (4096)
  BrMaxRsaFactor = (BrMaxRsaSize + 64) shr 1   # inner.h:74 BR_MAX_RSA_FACTOR
  U = 2 + ((BrMaxRsaFactor + 30) div 31)       # rsa_i31_priv.c:27
  TLEN = 8 * U                                 # rsa_i31_priv.c:28
  NW = 3 + (MaxBits + 30) div 31
    ## i31 words for any number of `MaxBits` bits: header, value words, and
    ## the extra top word `br_i31_montymul` and `br_i31_mulacc` write.
  MaxBytes = 4 * MaxLimbs
  FactorBytes = 4 * ((MaxLimbs - 2) div 2)     # a prime: at most `h` limbs

type
  I31Buf = array[NW, uint32]

  CtMont* = object
    ## Montgomery context for a SECRET odd modulus `m`, in BearSSL's i31
    ## form (portable path).
    m*: BigNum          ## the limbs are secret; `m.n` is public
    m0i: uint32         ## `br_i31_ninv31(m[1])`: `-m^-1 mod 2^31`
    mw: I31Buf          ## `m` as i31 words, word 0 the encoded bit length

  RsaCrt* = object
    ## An RSA private key in CRT form (RFC 8017 §3.2, second representation),
    ## ready for `rsaCrtPrivate`. Built by `initRsaCrt`; cleared by `wipe`.
    ## On this path it holds what BearSSL's `br_rsa_private_key` holds, the
    ## factors and CRT values as big-endian bytes that every private
    ## operation decodes again (rsa_i31_priv.c:89-99), plus the public key.
    n, e: BigNum        # public
    n_bitlen: uint32    # public: `br_rsa_private_key.n_bitlen`
    nw: I31Buf          # public: `n` as i31 words, for the fault check
    n0i: uint32         # public: `br_i31_ninv31(nw[1])`
    eb: array[MaxBytes, byte]   # public: `e`, big-endian, `elen` bytes
    elen: int
    plen: int           # public: `4 * p.n`, the length of each secret below
    p, q, dp, dq, iq: array[FactorBytes, byte]   # secret, big-endian
    pm2, qm2: array[FactorBytes, byte]  # secret: `p - 2`, `q - 2` (blinding)

template i31(x: var I31Buf): I31 = cast[I31](addr x)
template i31(x: var array[1 + TLEN, uint32]): I31 = cast[I31](addr x)
template i31(x: var array[3 * U, uint32]): I31 = cast[I31](addr x)
template bytesOf(x: var array[MaxBytes, byte]): Bytes = cast[Bytes](addr x)
template bytesOf(x: var array[FactorBytes, byte]): Bytes = cast[Bytes](addr x)

# ------------------------------------------------- declassification ---

when defined(ctgrind):
  proc makeMemDefined(p: pointer; len: int) {.
    importc: "VALGRIND_MAKE_MEM_DEFINED", header: "<valgrind/memcheck.h>".}

proc publicBitLen(x: I31) {.inline.} =
  # Not in BearSSL, which simply uses it: the encoded bit length that
  # `br_i31_decode` just computed for a secret modulus (`p`, `q`, a `CtMont`
  # modulus) is public from here on, the leak described at the top of this
  # file. This is the one point where this path declares a value derived
  # from a secret public; the tests/ctgrind builds (`-d:ctgrind`) tell
  # valgrind's memcheck so, every other build compiles it to nothing.
  # Called only by `decodeModulus` (tests/ctgrind/setup.nim checks that).
  when defined(ctgrind):
    makeMemDefined(addr x[0], sizeof(uint32))
  else:
    discard x

proc decodeModulus(x: I31; src: Bytes; len: int) =
  # `br_i31_decode` of a secret MODULUS, whose header (its bit length) is
  # then public. Why that is sound: BearSSL makes the same assumption
  # (inner.h:1307-1312: "only the modulus length may leak";
  # rsa_i31_priv.c:44-47: the lengths of p and q "are not considered
  # secret (we cannot really hide them anyway in constant-time code)"), and
  # conformant key generators (FIPS 186-5 A.1.3) make |p| = |q| = |n|/2, so
  # the header carries no information beyond the public |n|; for an imported
  # key it pins p to within a factor of 2, far from what factoring with
  # partial knowledge needs. Use it only on the modulus of a Montgomery
  # context (`ctInitMont`) and on `p` and `q` (rsa_i31_priv.c:93, 99), never
  # on another secret: tests/ctgrind/setup.nim counts the call sites.
  br_i31_decode(x, src, len)
  publicBitLen(x)

# ------------------------------------------------------- the boundary ---
# Not in BearSSL: moving `Limbs` in and out of the byte strings the i31 code
# reads and writes. Data movement only: no branch, no index on a limb value.

proc limbsToBytes(a: Limbs; n: int; dst: Bytes; len: int) =
  # the low `n` limbs of `a` as `len >= 4 * n` big-endian bytes
  var i = 0
  while i < len:
    let k = len - 1 - i               # byte `i` from the right
    let limb = k div 4
    var b = 0'u8
    if limb >= 0 and limb < n and limb < MaxLimbs:
      b = byte((a[limb] shr (8 * (k mod 4))) and 0xFF'u32)
    dst[i] = b
    inc i

proc bytesToLimbs(src: Bytes; len: int): Limbs =
  # `len` big-endian bytes as limbs; limbs past `(len + 3) div 4` zero
  result = default(Limbs)
  var i = 0
  while i < len:
    let k = len - 1 - i
    let limb = k div 4
    if limb >= 0 and limb < MaxLimbs:
      result[limb] = result[limb] or (uint32(src[i]) shl (8 * (k mod 4)))
    inc i

proc toI31(ctx: CtMont; a: Limbs; x: I31) =
  # `a < m` as i31 words with `m`'s header: i31_decred.c:65-66, the
  # `br_i31_decode` then `x[0] = m_ebitlen` of `br_i31_decode_reduce`.
  # Bits of `a` at and above `31 * len` (`len` the word count of `m`) are
  # dropped, so the caller must pass `a < m` (see the top of this file).
  var buf = default(array[MaxBytes, byte])
  limbsToBytes(a, ctx.m.n, bytesOf(buf), 4 * ctx.m.n)
  br_i31_decode(x, bytesOf(buf), 4 * ctx.m.n)
  x[0] = ctx.mw[0]

proc fromI31(x: I31): Limbs =
  var buf = default(array[MaxBytes, byte])
  br_i31_encode(bytesOf(buf), 4 * (MaxLimbs - 2), x)
  result = bytesToLimbs(bytesOf(buf), 4 * (MaxLimbs - 2))

func encodedBitLen(bits: int): uint32 =
  # br_i31_bit_length's encoding (i31_bitlen.c:43) of a public bit length:
  # `(index of the top word) << 5` plus the bits used in that word
  let w = (bits + 30) div 31
  result = (uint32(w - 1) shl 5) + uint32(bits - 31 * (w - 1))

# ------------------------------------------------------ masks and limbs ---

func ctMaskNonZero*(x: uint32): uint32 {.inline.} =
  ## All ones when `x != 0`, else zero: `-NEQ(x, 0)` (inner.h:791-797),
  ## through `ctOpaque`.
  result = ctOpaque(0'u32 - NEQ(x, 0'u32))

func ctSelect*(mask, a, b: uint32): uint32 {.inline.} =
  ## `a` where `mask` is all ones, `b` where it is zero: `MUX` (inner.h:772)
  ## on the low bit of the mask.
  result = MUX(mask and 1'u32, a, b)

func ctAdd*(a: var Limbs; b: Limbs; n: LimbCount): uint32 =
  ## `a += b` over the low `n` limbs; returns the carry (0 or 1). BearSSL's
  ## 32-bit-word `br_i32_add` (i32_add.c:28-51) with `ctl = 1`; the word
  ## count is `n`, not a header word, so `a[u]` here is i32's `a[u + 1]`.
  var cc = 0'u32                                          # i32_add.c:34
  var u = 0                                               # i32_add.c:36
  while u < n:
    let aw = a[u]                                         # i32_add.c:39
    let bw = b[u]                                         # i32_add.c:40
    let naw = aw + bw + cc                                # i32_add.c:41
    cc = (cc and EQ(naw, aw)) or LT(naw, aw)              # i32_add.c:47
    a[u] = MUX(1'u32, naw, aw)                            # i32_add.c:48
    inc u
  result = cc                                             # i32_add.c:50

func ctSub*(a: var Limbs; b: Limbs; n: LimbCount): uint32 =
  ## `a -= b` over the low `n` limbs; returns the borrow (0 or 1).
  ## `br_i32_sub` (i32_sub.c:28-51) with `ctl = 1`, indexed as `ctAdd`.
  var cc = 0'u32                                          # i32_sub.c:34
  var u = 0                                               # i32_sub.c:36
  while u < n:
    let aw = a[u]                                         # i32_sub.c:39
    let bw = b[u]                                         # i32_sub.c:40
    let naw = aw - bw - cc                                # i32_sub.c:41
    cc = (cc and EQ(naw, aw)) or GT(naw, aw)              # i32_sub.c:47
    a[u] = MUX(1'u32, naw, aw)                            # i32_sub.c:48
    inc u
  result = cc                                             # i32_sub.c:50

func ctLessMask*(a, b: Limbs; n: LimbCount): uint32 =
  ## All ones when `a < b` over the low `n` limbs: the borrow of `a - b`,
  ## `br_i32_sub` with `ctl = 0` (the i31 code's comparison idiom,
  ## i31_montmul.c:126).
  var d = a
  var cc = 0'u32                                          # i32_sub.c:34
  var u = 0                                               # i32_sub.c:36
  while u < n:
    let aw = d[u]                                         # i32_sub.c:39
    let bw = b[u]                                         # i32_sub.c:40
    let naw = aw - bw - cc                                # i32_sub.c:41
    cc = (cc and EQ(naw, aw)) or GT(naw, aw)              # i32_sub.c:47
    d[u] = MUX(0'u32, naw, aw)                            # i32_sub.c:48
    inc u
  result = ctOpaque(0'u32 - cc)

func ctEqualMask*(a, b: Limbs; n: LimbCount): uint32 =
  ## All ones when the low `n` limbs of `a` and `b` agree: `br_i31_iszero`
  ## (i31_iszero.c:28-39) of `a xor b`.
  var z = 0'u32                                           # i31_iszero.c:34
  var u = n                                               # i31_iszero.c:35
  while u > 0:
    z = z or (a[u - 1] xor b[u - 1])                      # i31_iszero.c:36
    dec u
  result = ctOpaque(0'u32 - ((not (z or (0'u32 - z))) shr 31))  # i31_iszero.c:38

func ctMaskZero*(x: uint32): uint32 {.inline.} =
  ## All ones when `x == 0`, else zero.
  result = not ctMaskNonZero(x)

func ctMaskEq*(a, b: uint32): uint32 {.inline.} =
  ## All ones when `a == b`, else zero.
  result = ctMaskZero(a xor b)

# ------------------------------------------------------------ limb vectors ---

func ctFromBytesBE*(data: openArray[byte]; res: var Limbs): bool =
  ## Reads a big-endian number by byte POSITION, with no leading-zero skip
  ## (`bignum.fromBytesBE` branches on the bytes): for random blinding input.
  ## Limbs past `(data.len + 3) div 4` are zero. `false` when `data` is empty
  ## or longer than `(MaxLimbs - 2) * 4` bytes.
  res = default(Limbs)
  if data.len == 0 or data.len > (MaxLimbs - 2) * 4: return false
  var limb = 0
  var shift = 0
  var i = data.len - 1
  while i >= 0:
    if limb >= 0 and limb < MaxLimbs:
      res[limb] = res[limb] or (uint32(data[i]) shl shift)
    shift = shift + 8
    if shift == 32:
      shift = 0
      inc limb
    dec i
  result = true

func ctMul*(a, b: Limbs; h: HalfLimbCount): BigNum =
  ## The full product `a * b` of two `h`-limb numbers, as a `2h`-limb
  ## `BigNum` (its top limb may be zero): `br_i31_mulacc` into a zeroed
  ## `d`, as rsa_i31_priv.c:106-108 computes `p * q`. The operands' headers
  ## are the public width `32 h`, not their bit lengths, so the loop bounds
  ## of `br_i31_mulacc` do not depend on the values.
  {.cast(noSideEffect).}:
    var bb = default(array[MaxBytes, byte])
    var x = default(I31Buf)
    var y = default(I31Buf)
    var d = default(I31Buf)
    limbsToBytes(a, h, bytesOf(bb), 4 * h)
    br_i31_decode(i31(x), bytesOf(bb), 4 * h)
    x[0] = encodedBitLen(32 * h)
    limbsToBytes(b, h, bytesOf(bb), 4 * h)
    br_i31_decode(i31(y), bytesOf(bb), 4 * h)
    y[0] = encodedBitLen(32 * h)
    br_i31_zero(i31(d), x[0])                               # rsa_i31_priv.c:107
    br_i31_mulacc(i31(d), i31(x), i31(y))                   # rsa_i31_priv.c:108
    br_i31_encode(bytesOf(bb), 8 * h, i31(d))               # rsa_i31_priv.c:120
    result = BigNum(limbs: bytesToLimbs(bytesOf(bb), 8 * h), n: h + h)

# ------------------------------------------------------------- Montgomery ---

func ctInitMont*(m: BigNum): CtMont =
  ## The context for an ODD modulus `m >= 3`, which the caller must have
  ## checked: `m0i` is wrong for an even one. `m` is decoded with
  ## `br_i31_decode` and `m0i` is `br_i31_ninv31(m[1])` (rsa_i31_priv.c:93,
  ## 141). The bit length of `m` becomes public (see above).
  {.cast(noSideEffect).}:
    result = CtMont(m: m, m0i: 0'u32, mw: default(I31Buf))
    var bb = default(array[MaxBytes, byte])
    limbsToBytes(m.limbs, m.n, bytesOf(bb), 4 * m.n)
    decodeModulus(i31(result.mw), bytesOf(bb), 4 * m.n)     # rsa_i31_priv.c:93
    result.m0i = br_i31_ninv31(result.mw[1])                # rsa_i31_priv.c:141

func ctMontMul*(ctx: CtMont; a, b: Limbs): Limbs =
  ## `a * b * R^-1 mod m`, `R = 2^(31 * len)`, for `a, b < m`:
  ## `br_i31_montymul`.
  {.cast(noSideEffect).}:
    var c = ctx
    var x = default(I31Buf)
    var y = default(I31Buf)
    var d = default(I31Buf)
    toI31(ctx, a, i31(x))
    toI31(ctx, b, i31(y))
    br_i31_montymul(i31(d), i31(x), i31(y), i31(c.mw), ctx.m0i)
    result = fromI31(i31(d))

func ctToMont*(ctx: CtMont; a: Limbs): Limbs =
  ## `a * R mod m`, for `a < m`: `br_i31_to_monty`.
  {.cast(noSideEffect).}:
    var c = ctx
    var x = default(I31Buf)
    toI31(ctx, a, i31(x))
    br_i31_to_monty(i31(x), i31(c.mw))
    result = fromI31(i31(x))

func ctFromMont*(ctx: CtMont; a: Limbs): Limbs =
  ## `a * R^-1 mod m`, for `a < m`: `br_i31_from_monty`.
  {.cast(noSideEffect).}:
    var c = ctx
    var x = default(I31Buf)
    toI31(ctx, a, i31(x))
    br_i31_from_monty(i31(x), i31(c.mw), ctx.m0i)
    result = fromI31(i31(x))

func ctModMul*(ctx: CtMont; a, b: Limbs): Limbs =
  ## `a * b mod m` for `a, b < m`, in the ordinary domain:
  ## `br_i31_to_monty(a)` then `br_i31_montymul`, as rsa_i31_priv.c:173-175
  ## multiplies `s1` by `iq`.
  {.cast(noSideEffect).}:
    var c = ctx
    var x = default(I31Buf)
    var y = default(I31Buf)
    var d = default(I31Buf)
    toI31(ctx, a, i31(x))
    toI31(ctx, b, i31(y))
    br_i31_to_monty(i31(x), i31(c.mw))                      # rsa_i31_priv.c:173
    br_i31_montymul(i31(d), i31(x), i31(y), i31(c.mw), ctx.m0i)  # rsa_i31_priv.c:175
    result = fromI31(i31(d))

func ctModSub*(ctx: CtMont; a, b: Limbs): Limbs =
  ## `a - b mod m` for `a, b < m`: rsa_i31_priv.c:172,
  ## `br_i31_add(a, m, br_i31_sub(a, b, 1))`.
  {.cast(noSideEffect).}:
    var c = ctx
    var x = default(I31Buf)
    var y = default(I31Buf)
    toI31(ctx, a, i31(x))
    toI31(ctx, b, i31(y))
    discard br_i31_add(i31(x), i31(c.mw), br_i31_sub(i31(x), i31(y), 1'u32))  # rsa_i31_priv.c:172
    result = fromI31(i31(x))

func ctMod*(ctx: CtMont; x: Limbs; xn: LimbCount): Limbs =
  ## `x mod m` for any `xn`-limb `x`, secret or public:
  ## `br_i31_decode_reduce` of its `4 xn` bytes (rsa_i31_priv.c:143).
  {.cast(noSideEffect).}:
    var c = ctx
    var bb = default(array[MaxBytes, byte])
    var d = default(I31Buf)
    limbsToBytes(x, xn, bytesOf(bb), 4 * xn)
    br_i31_decode_reduce(i31(d), bytesOf(bb), 4 * xn, i31(c.mw))
    result = fromI31(i31(d))

func ctMontPow*(ctx: CtMont; base, exp: Limbs; expLimbs: LimbCount): Limbs =
  ## `base^exp` for `base < m` in Montgomery form, result in Montgomery form.
  ## `br_i31_modpow_opt` over the `4 * expLimbs` big-endian bytes of the
  ## exponent: the same squarings and multiplications for every exponent of
  ## that width. Not 1:1: `br_i31_modpow_opt` takes and returns the ordinary
  ## form, so `base` is taken out of Montgomery form before and the result
  ## put back after (one `br_i31_from_monty`, one `br_i31_to_monty`).
  {.cast(noSideEffect).}:
    var c = ctx
    var x = default(I31Buf)
    var eb = default(array[MaxBytes, byte])
    var tmp = default(array[8 * NW, uint32])
    toI31(ctx, base, i31(x))
    br_i31_from_monty(i31(x), i31(c.mw), ctx.m0i)
    limbsToBytes(exp, expLimbs, bytesOf(eb), 4 * expLimbs)
    discard br_i31_modpow_opt(i31(x), bytesOf(eb), 4 * expLimbs, i31(c.mw),
                              ctx.m0i, cast[I31](addr tmp), 8 * NW)
    br_i31_to_monty(i31(x), i31(c.mw))
    result = fromI31(i31(x))

# -------------------------------------------------------------- RSA (CRT) ---

func initRsaCrt*(n, e, p, q, dP, dQ, qInv: BigNum; key: var RsaCrt): bool =
  ## Builds the CRT form of a private key from its RFC 8017 components.
  ## Checks the public shape (`n` odd, `e` odd with `3 <= e < n`, `p` and
  ## `q` of the same limb count `h >= 2` with `2h` limbs holding `n`, `dP`,
  ## `dQ`, `qInv` no wider than `h` limbs) and then `p * q == n` in constant
  ## time, which also makes `p` and `q` odd. `false` when any fails; the
  ## secret check runs to the end either way and only its outcome is
  ## branched on, by the caller. It does NOT check `dP`, `dQ` or `qInv`
  ## against `p` and `q`: a wrong one is caught by the fault check of
  ## `rsaCrtPrivate`.
  {.cast(noSideEffect).}:
    key = default(RsaCrt)
    key.n = n
    key.e = e
    # the public shape: Nimony's checks, not in BearSSL
    if (n.limbs[0] and 1'u32) == 0'u32: return false
    if cmp(e, n) >= 0: return false
    if (e.limbs[0] and 1'u32) == 0'u32 or (e.n == 1 and e.limbs[0] < 3'u32): return false
    let h = p.n
    if q.n != h or dP.n > h or dQ.n > h or qInv.n > h: return false
    if h < 2 or h > (MaxLimbs - 2) div 2 or n.n > h + h: return false
    # what `br_rsa_private_key` holds: big-endian bytes, here all `4 h` long
    # (the public limb count), and `n_bitlen`
    key.n_bitlen = uint32(n.bitLen)
    key.plen = 4 * h
    limbsToBytes(p.limbs, h, bytesOf(key.p), key.plen)
    limbsToBytes(q.limbs, h, bytesOf(key.q), key.plen)
    limbsToBytes(dP.limbs, h, bytesOf(key.dp), key.plen)
    limbsToBytes(dQ.limbs, h, bytesOf(key.dq), key.plen)
    limbsToBytes(qInv.limbs, h, bytesOf(key.iq), key.plen)
    key.elen = 4 * e.n
    limbsToBytes(e.limbs, e.n, bytesOf(key.eb), key.elen)
    # not in BearSSL: `n` in i31 form for the fault check (public)
    var nb = default(array[MaxBytes, byte])
    limbsToBytes(n.limbs, n.n, bytesOf(nb), 4 * n.n)
    br_i31_decode(i31(key.nw), bytesOf(nb), 4 * n.n)
    key.n0i = br_i31_ninv31(key.nw[1])
    # rsa_i31_priv.c:62-87, with the public lengths of above in place of the
    # leading-zero skip of rsa_i31_priv.c:49-60 (a branch on secret bytes
    # that BearSSL accepts, Nimony's ctgrind does not)
    let plen = key.plen
    let qlen = key.plen
    var z = (if plen > qlen: plen else: qlen) shl 3         # rsa_i31_priv.c:65
    var fwlen = 1                                           # rsa_i31_priv.c:66
    while z > 0:                                            # rsa_i31_priv.c:67
      z = z - 31                                            # rsa_i31_priv.c:68
      inc fwlen                                             # rsa_i31_priv.c:69
    fwlen = fwlen + (fwlen and 1)                           # rsa_i31_priv.c:75
    if 6 * fwlen > TLEN:                                    # rsa_i31_priv.c:80
      return false                                          # rsa_i31_priv.c:81
    var tmp = default(array[1 + TLEN, uint32])
    let mq = i31(tmp)                                       # rsa_i31_priv.c:92
    decodeModulus(mq, bytesOf(key.q), qlen)                 # rsa_i31_priv.c:93
    let t1 = mq +! fwlen                                    # rsa_i31_priv.c:98
    decodeModulus(t1, bytesOf(key.p), plen)                 # rsa_i31_priv.c:99
    let t2 = mq +! (2 * fwlen)                              # rsa_i31_priv.c:106
    br_i31_zero(t2, mq[0])                                  # rsa_i31_priv.c:107
    br_i31_mulacc(t2, mq, t1)                               # rsa_i31_priv.c:108
    # rsa_i31_priv.c:119-130 compares `p * q` with the INPUT `x` (the borrow
    # of `x - p q`). Here the same loop compares it with `n` in both
    # directions, over `2 plen` bytes rather than `xlen`: `p q` is not yet
    # known to be `xlen` bytes long, and a truncated product must not match.
    let t3 = cast[Bytes](mq +! (4 * fwlen))                 # rsa_i31_priv.c:119
    let plen2 = plen + qlen
    br_i31_encode(t3, plen2, t2)                            # rsa_i31_priv.c:120
    limbsToBytes(n.limbs, n.n, bytesOf(nb), plen2)
    var u = plen2                                           # rsa_i31_priv.c:121
    var r = 0'u32                                           # rsa_i31_priv.c:122
    var r2 = 0'u32
    while u > 0:                                            # rsa_i31_priv.c:123
      dec u                                                 # rsa_i31_priv.c:126
      let wn = uint32(t3[u])                                # rsa_i31_priv.c:127
      let wx = uint32(bytesOf(nb)[u])                        # rsa_i31_priv.c:128
      r = ((wx - (wn + r)) shr 8) and 1'u32                 # rsa_i31_priv.c:129: n < p q
      r2 = ((wn - (wx + r2)) shr 8) and 1'u32               # the same, swapped: p q < n
    let same = NOT(r) and NOT(r2)
    # rsa_i31_priv.c:141, 150 and 202: `p0i & q0i` is 0 for an even factor
    let q0i = br_i31_ninv31(mq[1])                          # rsa_i31_priv.c:141
    let p0i = br_i31_ninv31(t1[1])                          # rsa_i31_priv.c:150
    let ok = p0i and q0i and same                           # rsa_i31_priv.c:202
    # not in BearSSL: `p - 2` and `q - 2`, the Fermat exponents of the
    # blinding, with `br_i31_sub`
    let two = mq +! (5 * fwlen)
    br_i31_zero(two, t1[0])
    two[1] = 2'u32
    discard br_i31_sub(t1, two, 1'u32)
    br_i31_encode(bytesOf(key.pm2), plen, t1)
    br_i31_zero(two, mq[0])
    two[1] = 2'u32
    discard br_i31_sub(mq, two, 1'u32)
    br_i31_encode(bytesOf(key.qm2), qlen, mq)
    result = ok == 1'u32

func rsaCrtModBytes*(key: RsaCrt): int =
  ## `k`, the length of the modulus in bytes.
  result = (key.n.bitLen + 7) div 8

func rsaCrtBlindLen*(key: RsaCrt): int =
  ## How many random bytes `rsaCrtPrivate` wants for EACH of `blindP` and
  ## `blindQ`: the prime's width plus 64 bits, so the reduction mod the prime
  ## leaves a negligible bias.
  result = key.plen + 8


proc blindedPow(s, m: I31; m0i: uint32; blind: Bytes; blen: int;
                exp: Bytes; explen: int; fermat: Bytes; fermatlen: int;
                e: Bytes; elen: int; tmp: I31; twlen: int;
                bt: I31; fwlen: int): uint32 =
  # Not in BearSSL: `s = s^exp mod m` with Nimony's base blinding, around
  # the `br_i31_modpow_opt` of rsa_i31_priv.c:144 / 153, whose return value
  # this returns. With `r` the blinding factor reduced mod `m`:
  #   s = s * r^e;  s = s^exp;  s = s * r^(m - 2)
  # which is `s^exp` because `e * exp = 1 (mod m - 1)` and `r^(m-2) = 1/r`
  # (and wrong when `r = 0 mod m`, which the fault check then refuses).
  # A product in the ordinary domain is `br_i31_to_monty` and then
  # `br_i31_montymul`, as rsa_i31_priv.c:173-175 multiplies `s1` by `iq`.
  let mwlen = int((m[0] + 63'u32) shr 5)    # i31_modpow2.c:42: words to copy
  let rr = bt                               # r
  let rx = bt +! fwlen                      # r^e, then r^(m - 2)
  let t = bt +! (2 * fwlen)                 # a product
  br_i31_decode_reduce(rr, blind, blen, m)
  copyMem(rx, rr, mwlen * sizeof(uint32))
  discard br_i31_modpow_opt(rx, e, elen, m, m0i, tmp, twlen)
  br_i31_to_monty(s, m)
  br_i31_montymul(t, s, rx, m, m0i)
  copyMem(s, t, mwlen * sizeof(uint32))
  result = br_i31_modpow_opt(s, exp, explen, m, m0i, tmp, twlen)  # rsa_i31_priv.c:144 / 153
  copyMem(rx, rr, mwlen * sizeof(uint32))
  discard br_i31_modpow_opt(rx, fermat, fermatlen, m, m0i, tmp, twlen)
  br_i31_to_monty(s, m)
  br_i31_montymul(t, s, rx, m, m0i)
  copyMem(s, t, mwlen * sizeof(uint32))

proc wipeBytes(x: var array[FactorBytes, byte]) =
  for i in 0 ..< FactorBytes:
    volatileStore(addr x[i], 0'u8)

func rsaCrtRaw*(key: RsaCrt; c: BigNum; blindP, blindQ: Limbs; blindLimbs: LimbCount): BigNum =
  ## `c^d mod n` by CRT with base blinding and NO fault check; `rsaCrtPrivate`
  ## is the entry point. `blindP` and `blindQ` are random `blindLimbs`-limb
  ## numbers (reduced mod `p` and `q` here). For `c < n`. This is
  ## `br_rsa_i31_private` (rsa_i31_priv.c:31-203) with the blinding of
  ## `blindedPow` around its two exponentiations; its status word
  ## (rsa_i31_priv.c:202), 0 for an even factor, a wiped key or `c >= p q`,
  ## zeroes the result by mask, which the fault check then refuses.
  {.cast(noSideEffect).}:
    # a local copy for the addresses of the key's byte strings (`key` is not
    # `var`); wiped below
    var k = key
    var xb = default(array[MaxBytes, byte])
    var bpb = default(array[MaxBytes, byte])
    var bqb = default(array[MaxBytes, byte])
    var bt = default(array[3 * U, uint32])
    let blen = 4 * blindLimbs
    limbsToBytes(blindP, blindLimbs, bytesOf(bpb), blen)
    limbsToBytes(blindQ, blindLimbs, bytesOf(bqb), blen)
    # rsa_i31_priv.c:49-60 skips the leading zero bytes of `p` and `q`; their
    # lengths here are the public `4 h` instead (the skip branches on secret
    # bytes)
    let p = bytesOf(k.p)                                    # rsa_i31_priv.c:49
    let plen = k.plen                                       # rsa_i31_priv.c:50
    let q = bytesOf(k.q)                                    # rsa_i31_priv.c:55
    let qlen = k.plen                                       # rsa_i31_priv.c:56
    var z = (if plen > qlen: plen else: qlen) shl 3         # rsa_i31_priv.c:65
    var fwlen = 1                                           # rsa_i31_priv.c:66
    while z > 0:                                            # rsa_i31_priv.c:67
      z = z - 31                                            # rsa_i31_priv.c:68
      inc fwlen                                             # rsa_i31_priv.c:69
    fwlen = fwlen + (fwlen and 1)                           # rsa_i31_priv.c:75
    if 6 * fwlen > TLEN:                                    # rsa_i31_priv.c:80
      return BigNum(limbs: default(Limbs), n: 1)            # rsa_i31_priv.c:81
    let xlen = int((k.n_bitlen + 7'u32) shr 3)              # rsa_i31_priv.c:87
    # BearSSL's `x` is the input and output buffer: `c` as `xlen` bytes
    let x = bytesOf(xb)
    limbsToBytes(c.limbs, c.n, x, xlen)
    var tmp = default(array[1 + TLEN, uint32])              # rsa_i31_priv.c:39
    let mq = i31(tmp)                                       # rsa_i31_priv.c:92
    decodeModulus(mq, q, qlen)                              # rsa_i31_priv.c:93
    var t1 = mq +! fwlen                                    # rsa_i31_priv.c:98
    decodeModulus(t1, p, plen)                              # rsa_i31_priv.c:99
    var t2 = mq +! (2 * fwlen)                              # rsa_i31_priv.c:106
    br_i31_zero(t2, mq[0])                                  # rsa_i31_priv.c:107
    br_i31_mulacc(t2, mq, t1)                               # rsa_i31_priv.c:108
    var t3 = mq +! (4 * fwlen)                              # rsa_i31_priv.c:119
    br_i31_encode(cast[Bytes](t3), xlen, t2)                # rsa_i31_priv.c:120
    var u = xlen                                            # rsa_i31_priv.c:121
    var r = 0'u32                                           # rsa_i31_priv.c:122
    while u > 0:                                            # rsa_i31_priv.c:123
      dec u                                                 # rsa_i31_priv.c:126
      let wn = uint32(cast[Bytes](t3)[u])                   # rsa_i31_priv.c:127
      let wx = uint32(x[u])                                 # rsa_i31_priv.c:128
      r = ((wx - (wn + r)) shr 8) and 1'u32                 # rsa_i31_priv.c:129
    let mp = mq +! (2 * fwlen)                              # rsa_i31_priv.c:135
    moveMem(mp, t1, fwlen * sizeof(uint32))                 # rsa_i31_priv.c:136
    let q0i = br_i31_ninv31(mq[1])                          # rsa_i31_priv.c:141
    let s2 = mq +! fwlen                                    # rsa_i31_priv.c:142
    br_i31_decode_reduce(s2, x, xlen, mq)                   # rsa_i31_priv.c:143
    # rsa_i31_priv.c:144-145 `r &= br_i31_modpow_opt(s2, sk->dq, ...)`, blinded
    r = r and blindedPow(s2, mq, q0i, bytesOf(bqb), blen, bytesOf(k.dq), k.plen,
                         bytesOf(k.qm2), k.plen, bytesOf(k.eb), k.elen,
                         mq +! (3 * fwlen), TLEN - 3 * fwlen, i31(bt), fwlen)
    let p0i = br_i31_ninv31(mp[1])                          # rsa_i31_priv.c:150
    let s1 = mq +! (3 * fwlen)                              # rsa_i31_priv.c:151
    br_i31_decode_reduce(s1, x, xlen, mp)                   # rsa_i31_priv.c:152
    # rsa_i31_priv.c:153-154 `r &= br_i31_modpow_opt(s1, sk->dp, ...)`, blinded
    r = r and blindedPow(s1, mp, p0i, bytesOf(bpb), blen, bytesOf(k.dp), k.plen,
                         bytesOf(k.pm2), k.plen, bytesOf(k.eb), k.elen,
                         mq +! (4 * fwlen), TLEN - 4 * fwlen, i31(bt), fwlen)
    t1 = mq +! (4 * fwlen)                                  # rsa_i31_priv.c:169
    t2 = mq +! (5 * fwlen)                                  # rsa_i31_priv.c:170
    br_i31_reduce(t2, s2, mp)                               # rsa_i31_priv.c:171
    discard br_i31_add(s1, mp, br_i31_sub(s1, t2, 1'u32))   # rsa_i31_priv.c:172
    br_i31_to_monty(s1, mp)                                 # rsa_i31_priv.c:173
    br_i31_decode_reduce(t1, bytesOf(k.iq), k.plen, mp)     # rsa_i31_priv.c:174
    br_i31_montymul(t2, s1, t1, mp, p0i)                    # rsa_i31_priv.c:175
    t3 = s2                                                 # rsa_i31_priv.c:189
    br_i31_mulacc(t3, mq, t2)                               # rsa_i31_priv.c:190
    br_i31_encode(x, xlen, t3)                              # rsa_i31_priv.c:196
    let status = p0i and q0i and r                          # rsa_i31_priv.c:202
    # not in BearSSL: the API has no status, so a failed one zeroes the
    # output by mask (and `rsaCrtCheckMask` refuses it)
    let mask = ctOpaque(0'u32 - (status and 1'u32))
    for i in 0 ..< xlen:
      x[i] = byte(uint32(x[i]) and mask)
    let h = k.plen div 4
    if h + h <= MaxLimbs - 2:
      result = BigNum(limbs: bytesToLimbs(x, xlen), n: h + h)
    else:
      result = BigNum(limbs: default(Limbs), n: 1)
    {.cast(noSideEffect).}:
      wipeBytes(k.p)
      wipeBytes(k.q)
      wipeBytes(k.dp)
      wipeBytes(k.dq)
      wipeBytes(k.iq)
      wipeBytes(k.pm2)
      wipeBytes(k.qm2)

func rsaCrtCheckMask*(key: RsaCrt; s, c: BigNum): uint32 {.noinline.} =
  ## The fault check (Boneh-DeMillo-Lipton) as a mask: all ones when `s < n`
  ## and `s^e mod n == c`, else zero. `s` may be as wide as `p.n + q.n`
  ## limbs; the limbs past `n.n` must be zero. Constant time in `s`: the
  ## candidate is not yet known to be a valid signature. Not in BearSSL; built
  ## from its pieces: the byte-wise borrow of rsa_i31_priv.c:121-130 for
  ## `s < n`, `br_i31_decode_reduce` and `br_i31_modpow_opt` for `s^e`.
  ## (`noinline` so that tests/ctgrind can count its instructions by name.)
  {.cast(noSideEffect).}:
    const W = 4 * (MaxLimbs - 2)
    var k = key
    var sb = default(array[MaxBytes, byte])
    var nb = default(array[MaxBytes, byte])
    var cb = default(array[MaxBytes, byte])
    var vb = default(array[MaxBytes, byte])
    limbsToBytes(s.limbs, MaxLimbs - 2, bytesOf(sb), W)
    limbsToBytes(k.n.limbs, k.n.n, bytesOf(nb), W)
    limbsToBytes(c.limbs, MaxLimbs - 2, bytesOf(cb), W)
    # rsa_i31_priv.c:121-130 over every byte: `r = 1` exactly when `s < n`
    var u = W                                               # rsa_i31_priv.c:121
    var r = 0'u32                                           # rsa_i31_priv.c:122
    while u > 0:                                            # rsa_i31_priv.c:123
      dec u                                                 # rsa_i31_priv.c:126
      let wn = uint32(nb[u])                                # rsa_i31_priv.c:127
      let wx = uint32(sb[u])                                # rsa_i31_priv.c:128
      r = ((wx - (wn + r)) shr 8) and 1'u32                 # rsa_i31_priv.c:129
    # `s^e mod n` from the low `xlen` bytes of `s` (all of it when `r = 1`)
    let xlen = int((k.n_bitlen + 7'u32) shr 3)
    var x = default(I31Buf)
    var tmp = default(array[1 + TLEN, uint32])
    br_i31_decode_reduce(i31(x), bytesOf(sb) +! (W - xlen), xlen, i31(k.nw))
    discard br_i31_modpow_opt(i31(x), bytesOf(k.eb), k.elen, i31(k.nw), k.n0i,
                              i31(tmp), TLEN)
    br_i31_encode(bytesOf(vb), W, i31(x))
    # `br_i31_iszero` (i31_iszero.c:34-38) of `v xor c`, over every byte, so
    # a `c >= n` cannot match
    var zz = 0'u32
    for i in 0 ..< W:
      zz = zz or (uint32(vb[i]) xor uint32(cb[i]))
    let same = (not (zz or (0'u32 - zz))) shr 31
    result = ctOpaque(0'u32 - (r and same))
    # the copy (taken for the addresses of the public `n` and `e`) holds the
    # secret byte strings too
    wipeBytes(k.p)
    wipeBytes(k.q)
    wipeBytes(k.dp)
    wipeBytes(k.dq)
    wipeBytes(k.iq)
    wipeBytes(k.pm2)
    wipeBytes(k.qm2)

func rsaCrtCheck*(key: RsaCrt; s, c: BigNum): bool =
  ## `rsaCrtCheckMask` as a `bool`, the one place its verdict is branched on.
  result = rsaCrtCheckMask(key, s, c) == 0xFFFFFFFF'u32

func rsaCrtPrivate*(key: RsaCrt; c: BigNum; blindP, blindQ: openArray[byte];
                    s: var BigNum): bool =
  ## `s = c^d mod n` for `c < n`, by CRT with per-prime base blinding,
  ## followed by the fault check. `blindP` and `blindQ` must be fresh random
  ## bytes of the same length, `rsaCrtBlindLen(key)` each (at most
  ## `(MaxLimbs - 2) * 4`).
  ## `false`, with `s` zero, for `c >= n`, a blinding input of the wrong size
  ## or a failed fault check (a wrong `dP`, `dQ` or `qInv`, a hardware
  ## fault, or a blinding factor divisible by `p` or `q`).
  s = BigNum(limbs: default(Limbs), n: 1)
  if cmp(c, key.n) >= 0: return false
  if blindP.len != blindQ.len: return false
  var bP = default(Limbs)
  var bQ = default(Limbs)
  if not ctFromBytesBE(blindP, bP) or not ctFromBytesBE(blindQ, bQ): return false
  let bl = (blindP.len + 3) div 4
  if bl < 1 or bl > MaxLimbs - 2: return false
  let r = rsaCrtRaw(key, c, bP, bQ, bl)
  if rsaCrtCheck(key, r, c):
    s = r
    s.n = key.n.n
    result = true
  else:
    result = false

proc wipe*(key: var RsaCrt) =
  ## Overwrites the secret bytes of `key` with stores the C compiler may not
  ## drop. The public parts stay.
  wipeBytes(key.p)
  wipeBytes(key.q)
  wipeBytes(key.dp)
  wipeBytes(key.dq)
  wipeBytes(key.iq)
  wipeBytes(key.pm2)
  wipeBytes(key.qm2)
