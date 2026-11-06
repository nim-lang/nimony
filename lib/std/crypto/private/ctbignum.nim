#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Constant-time big-number arithmetic for the RSA PRIVATE-key operation.
##
## `std/crypto/bignum` is for verification and branches on its operands; this
## module is its counterpart for secrets: a prime factor, a CRT exponent, a
## blinding factor and every intermediate derived from them. Every routine
## that touches a secret follows four rules:
##
## 1. No `if`, `while`, `case`, short-circuit `and`/`or`, `min`/`max` or
##    early `return` on a secret. A condition becomes an all-ones/zero
##    `uint32` mask built by arithmetic (`ctMaskZero`, the borrow of a
##    subtraction) and is applied with `and`/`or` (`ctSelect`).
## 2. No array index computed from a secret: a window lookup reads every
##    table entry and keeps one by mask.
## 3. No `div`/`mod` on a secret; every loop bound is a public limb count
##    (`BigNum.n` of the modulus), never the bit length of a secret.
## 4. Numbers enter and leave as `bignum`'s 32-bit limbs. Inside, the
##    native path works on 64-bit words with x86-64's `mul` (constant time),
##    the portable path on 31-bit words with 32x32->64-bit products. A
##    64-bit multiply is constant time on x86-64 and AArch64; on some 32-bit
##    cores (Cortex-M3, old PowerPC) it is not, and nothing here can fix
##    that.
##
## Public: the RSA modulus `n`, the exponent `e`, the input `c`, the output
## once the fault check has passed, and every limb count (`p.n` is half of
## `n.n`, fixed by the key size). The limb counts come from `fromBytesBE`,
## whose leading-zero skip therefore reveals only lengths the DER encoding
## already made public.
##
## Where the instructions come from:
##
## - On the native backend for x86-64 (`nimony n`) every operation on secret
##   limbs goes through the primitives of the first branch below, the
##   `{.assembler.}` procs of `ctbignumx64`, where each statement is the
##   instruction that runs. There the Montgomery multiplication and
##   `ctMontPow` are a line-by-line translation of OpenSSL's `bn_mul_mont`
##   (`x86_64-mont.pl`) and `bn_mul_mont_gather5`/`bn_power5` family
##   (`x86_64-mont5.pl`), plain `mul`/`adc` code without MULX/ADX, and of the
##   window-5 driver of `bn_mod_exp_mont_consttime` (`bn_exp.c`), over 64-bit
##   words with `R = 2^(64 top)`, OpenSSL commit
##   5dbd6501ee6c07c8a397df5c979743c05d775603 (Apache-2.0; see
##   `ctbignumx64`). The rules above describe that path.
## - Everywhere else (`nimony c`, other CPUs) the module is `ctbignumport`,
##   the same API on a line-by-line translation of BearSSL's constant-time
##   i31 code (`ctbignumi31`, and `rsa_i31_priv.c` for the CRT flow), BearSSL
##   commit 7bea48e5e850ab4cafbe68d3765cdaba13a86d6f (MIT; the notice is in
##   `ctbignumi31`): 31-bit words with an encoded bit length, BearSSL's
##   MUX/EQ/GT masks (passed through a volatile load so the C compiler cannot
##   turn them into branches), and BearSSL's one deliberate leak, the bit
##   length of each modulus, `p` and `q` included, which bounds its loops
##   (see `ctbignumport`). It is constant time only as long as the C compiler
##   keeps the masks as masks; the ctgrind run (valgrind memcheck with the
##   secrets marked undefined, see `tests/ctgrind/`) checks that for the
##   builds it is run on.
##
## The Montgomery routines (`ctMontMul`, `ctToMont`, `ctModMul`, `ctModSub`,
## `ctMontPow`) take operands below the modulus `m`, on both paths. `R`
## differs between the paths (and so does a value in Montgomery form);
## nothing outside this module sees one.
##
## Not covered: copies of secret limbs left on the stack by the
## value-returning routines below. `wipe` clears an `RsaCrt`, not every
## temporary. `ctFromBytesBE` places secret bytes with ordinary shifts (data
## movement, no condition).

{.feature: "staticContracts".}

import std/volatile
import std/crypto/bignum

when defined(nimNativeBackend) and defined(amd64):
  # The native path: OpenSSL's x86-64 Montgomery code, ported in
  # `ctbignumx64`.

  type
    HalfLimbCount* = range[1 .. (MaxLimbs - 2) div 2]
      ## The limb count of an RSA prime: two of them fit a `BigNum`.

    CtMont* = object
      ## Montgomery context for a SECRET odd modulus `m`, `R = 2^(64 top)`
      ## with `top = montTop(m.n)` 64-bit words.
      m*: BigNum          ## the limbs are secret; `m.n` is public
      m0inv: uint32       ## `-m^-1 mod 2^32`
      r2: Limbs           ## `R^2 mod m`
      one: Limbs          ## `R mod m`, the Montgomery form of 1

    RsaCrt* = object
      ## An RSA private key in CRT form (RFC 8017 §3.2, second representation),
      ## ready for `rsaCrtPrivate`. Built by `initRsaCrt`; cleared by `wipe`.
      n, e: BigNum        # public
      nm: CtMont          # Montgomery context for `n`, for the fault check
      p, q: CtMont
      dP, dQ: Limbs       # CRT exponents, `p.m.n` and `q.m.n` limbs wide
      pm2, qm2: Limbs     # `p - 2` and `q - 2`, the Fermat exponents
      qInvM: Limbs        # `qInv mod p` in Montgomery form

  # ------------------------------------------------------------- primitives ---
  #
  # Everything that computes on a secret limb goes through the routines of this
  # section: the `{.assembler.}` procs of `ctbignumx64`, whose instructions are
  # the ones written there.

  import std/crypto/private/ctbignumx64

  proc w32(x: var Limbs): W32 {.inline.} = cast[W32](addr x)

  func ctMaskNonZero*(x: uint32): uint32 =
    ## All ones when `x != 0`, else zero.
    {.cast(noSideEffect).}:
      result = uint32(ctxMaskNonZero(uint64(x)) and 0xFFFFFFFF'u64)

  func ctSelect*(mask, a, b: uint32): uint32 =
    ## `a` where `mask` is all ones, `b` where it is zero.
    {.cast(noSideEffect).}:
      result = uint32(ctxSelect(uint64(mask), uint64(a), uint64(b)) and 0xFFFFFFFF'u64)

  func ctAdd*(a: var Limbs; b: Limbs; n: LimbCount): uint32 =
    ## `a += b` over the low `n` limbs; returns the carry (0 or 1).
    var y = b
    {.cast(noSideEffect).}:
      result = uint32(ctxAdd(w32(a), w32(y), n) and 1'u64)

  func ctSub*(a: var Limbs; b: Limbs; n: LimbCount): uint32 =
    ## `a -= b` over the low `n` limbs; returns the borrow (0 or 1).
    var y = b
    {.cast(noSideEffect).}:
      result = uint32(ctxSub(w32(a), w32(y), n) and 1'u64)

  func ctLessMask*(a, b: Limbs; n: LimbCount): uint32 =
    ## All ones when `a < b` over the low `n` limbs: the borrow of `a - b`.
    var x = a
    var y = b
    {.cast(noSideEffect).}:
      result = uint32(ctxLess(w32(x), w32(y), n) and 0xFFFFFFFF'u64)

  func ctEqualMask*(a, b: Limbs; n: LimbCount): uint32 =
    ## All ones when the low `n` limbs of `a` and `b` agree.
    var x = a
    var y = b
    {.cast(noSideEffect).}:
      result = uint32(ctxEqual(w32(x), w32(y), n) and 0xFFFFFFFF'u64)

  func condSub(x: var Limbs; m: Limbs; n: LimbCount; top: uint32) =
    # for `x + top * 2^(32n) < 2m`: `x -= m` unless that would go negative
    var y = m
    {.cast(noSideEffect).}:
      ctxCondSub(w32(x), w32(y), n, uint64(top))

  func maskedAdd(a: var Limbs; m: Limbs; n: LimbCount; bit: uint32) =
    # `a += m` when `bit != 0`, carry dropped
    var y = m
    {.cast(noSideEffect).}:
      ctxMaskedAdd(w32(a), w32(y), n, uint64(bit))

  func highZeroMask(a: Limbs; start: LimbCount): uint32 =
    # all ones when the limbs from `start` on are zero
    var x = a
    var z = default(Limbs)
    {.cast(noSideEffect).}:
      result = uint32(ctxEqual(cast[W32](addr x[start]), w32(z), MaxLimbs - start) and
                      0xFFFFFFFF'u64)

  func andMask(a: var Limbs; mask: uint32) =
    {.cast(noSideEffect).}:
      ctxAndMask(w32(a), MaxLimbs, uint64(mask))

  func shiftIn(r: var Limbs; n: LimbCount; word: uint32; shift: range[0..31]): uint32 =
    # `r = 2r + bit shift of word`; returns the bit shifted out (0 or 1)
    {.cast(noSideEffect).}:
      result = uint32(ctxShiftIn(w32(r), n, uint64(word), shift) and 1'u64)

  func mulRaw(r: var Limbs; a, b: Limbs; h: HalfLimbCount) =
    # `r = a * b`, `r` zero on entry
    var x = a
    var y = b
    {.cast(noSideEffect).}:
      ctxMul(w32(r), w32(x), w32(y), h)

  func negInv(m0: uint32): uint32 =
    # `-m0^-1 mod 2^32` for odd `m0`
    {.cast(noSideEffect).}:
      result = uint32(ctxNegInv(uint64(m0)) and 0xFFFFFFFF'u64)

  # The Montgomery arithmetic is the OpenSSL port of `ctbignumx64` over
  # 64-bit words. A `Limbs` (32-bit limbs, little-endian) IS a little-endian
  # array of 64-bit words in memory, so the boundary is a cast: `top` words
  # cover `n` limbs, with at least the 2 words `bn_mul_mont` wants
  # (`bn_mont.c` calls it only for `num > 1`); the limbs between `n` and
  # `2 top` are zero. `R = 2^(64 top)`, which `ctInitMont` below uses.

  const MaxTop = (MaxLimbs - 2 + 1) div 2   # words of the widest modulus

  type
    TpBuf = array[2 * MaxTop + 8, uint64]   # tp[-2 ..]: `num + 2` words for
                                            # bn_mul_mont, `2 num` for bn_power5

  proc w64(x: var Limbs): W64 {.inline.} = cast[W64](addr x)

  func montTop(n: int): int {.inline.} =
    result = (n + 1) div 2
    if result < 2: result = 2

  func montN0(m: Limbs): uint64 =
    # `n0 = -m^-1 mod 2^64`, from the low word of `m`
    {.cast(noSideEffect).}:
      result = bnNegInv64(uint64(m[0]) or (uint64(m[1]) shl 32))

  func montLimbs(n: LimbCount): LimbCount =
    # `2 * montTop(n)`: the 32-bit limbs that `R` covers
    if n <= 2: result = 4
    elif (n and 1) == 0: result = n
    elif n < MaxLimbs - 2: result = n + 1
    else: result = n                        # not reached: MaxLimbs - 2 is even

  proc initFrame(tp: var TpBuf; mask: var array[32, uint64];
                 n0: uint64; top: int): MontFrame {.inline.} =
    # the reference's stack frame: tp = &buf[2], so tp[-2] and tp[-1] exist
    var t2n = 2
    if top >= 1 and top <= MaxTop: t2n = 2 + 2 * top
    result = MontFrame(n0: n0, power: 0, tp: cast[W64](addr tp[2]),
                       tpm2: cast[W64](addr tp[0]), mask: cast[W64](addr mask),
                       t2n: cast[W64](addr tp[t2n]), tm1: cast[int](addr tp[1]))

  func montMulRaw(m: Limbs; m0inv: uint32; n: LimbCount; a, b: Limbs): Limbs =
    # `a * b * R^-1 mod m` by `bn_mul_mont`; `m0inv` (32 bits) is not used,
    # `n0` (64 bits) is computed from `m`
    var x = a
    var y = b
    var mm = m
    var tp = default(TpBuf)
    var mask = default(array[32, uint64])
    result = default(Limbs)
    {.cast(noSideEffect).}:
      var fr = initFrame(tp, mask, montN0(m), montTop(n))
      discard bnMulMont(w64(result), w64(x), w64(y), w64(mm), addr fr, montTop(n))

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
    ## `BigNum` (its top limb may be zero). Schoolbook, no operand-dependent
    ## branch.
    var r = default(Limbs)
    mulRaw(r, a, b, h)
    result = BigNum(limbs: r, n: h + h)

  # ------------------------------------------------------------- Montgomery ---

  func ctInitMont*(m: BigNum): CtMont =
    ## The context for an ODD modulus `m >= 3`, which the caller must have
    ## checked. Constant time in the limbs of `m`. On the native backend
    ## `R = 2^(64 top)` (`top = montTop(m.n)` words, as `bn_mul_mont` counts
    ## them): `R^2 mod m` is `1` doubled `128 top` times with a masked
    ## reduction after each step, and `R mod m` is the value half way.
    let n = montLimbs(m.n)            # 32-bit limbs covered by R
    var x = default(Limbs)
    x[0] = 1'u32
    var one = default(Limbs)
    for i in 0 ..< 64 * n:
      let y = x
      let carry = ctAdd(x, y, n)
      condSub(x, m.limbs, n, carry)
      if i == 32 * n - 1: one = x     # public step count: R mod m
    result = CtMont(m: m, m0inv: negInv(m.limbs[0]), r2: x, one: one)

  func ctMontMul*(ctx: CtMont; a, b: Limbs): Limbs =
    ## `a * b * R^-1 mod m` by `bn_mul_mont`, for `a, b < m`. The final
    ## reduction always computes `t - m` and keeps it by mask. Limbs past
    ## `m.n` are zero.
    result = montMulRaw(ctx.m.limbs, ctx.m0inv, ctx.m.n, a, b)

  func ctToMont*(ctx: CtMont; a: Limbs): Limbs {.inline.} =
    ## `a * R mod m`, for `a < m`.
    result = ctMontMul(ctx, a, ctx.r2)

  func ctFromMont*(ctx: CtMont; a: Limbs): Limbs =
    ## `a * R^-1 mod m`.
    var one = default(Limbs)
    one[0] = 1'u32
    result = ctMontMul(ctx, a, one)

  func ctModMul*(ctx: CtMont; a, b: Limbs): Limbs =
    ## `a * b mod m` for `a, b < m`, in the ordinary domain.
    result = ctMontMul(ctx, ctMontMul(ctx, a, b), ctx.r2)

  func ctModSub*(ctx: CtMont; a, b: Limbs): Limbs =
    ## `a - b mod m` for `a, b < m`: `m` is added back by mask on the borrow.
    let n = ctx.m.n
    result = a
    let borrow = ctSub(result, b, n)
    maskedAdd(result, ctx.m.limbs, n, borrow)

  func ctMod*(ctx: CtMont; x: Limbs; xn: LimbCount): Limbs =
    ## `x mod m` for any `xn`-limb `x`, secret or public: `x` is shifted in one
    ## bit at a time, `32 * xn` steps of an `m.n`-limb masked reduction.
    let n = ctx.m.n
    var r = default(Limbs)
    var li = xn - 1
    while li >= 0:
      let word = x[li]
      var b = 31
      while b >= 0:
        # r = 2r + bit; the value is below 2m, so one masked subtraction suffices
        let top = shiftIn(r, n, word, b)
        condSub(r, ctx.m.limbs, n, top)
        dec b
      dec li
    result = r

  func bnGetBits(p: Limbs; ptop: int; bitpos: int): uint64 =
    # bn_exp.c:487-504 `bn_get_bits`, over the 64-bit words of `p` (`ptop`
    # of them): the word starting at bit `bitpos`. Branches only on the
    # public position.
    {.cast(noSideEffect).}:
      var pp = p
      let d = w64(pp)
      result = 0'u64                              # bn_exp.c:489
      var wordpos = bitpos div 64                 # bn_exp.c:492
      let bp = bitpos mod 64                      # bn_exp.c:493
      if wordpos >= 0 and wordpos < ptop:         # bn_exp.c:494
        result = d[wordpos]                       # bn_exp.c:495
        if bp != 0:                               # bn_exp.c:496
          result = result shr bp                  # bn_exp.c:497
          inc wordpos
          if wordpos < ptop:                      # bn_exp.c:498
            result = result or (d[wordpos] shl (64 - bp))  # bn_exp.c:499

  proc wipeWords(x: var openArray[uint64]) =
    for i in 0 ..< x.len:
      volatileStore(addr x[i], 0'u64)

  proc mulMontGather5(rp, ap, table, np: W64; fr: ptr MontFrame; num: int) =
    # x86_64-mont5.pl:106-118: `bn_mul_mont_gather5` takes `.Lmul_enter`
    # unless `num` is divisible by 8, then `bn_mul4x_mont_gather5`
    if (num and 7) != 0:
      discard bnMulMontGather5(rp, ap, table, np, fr, num)
    else:
      discard bnMul4xMontGather5(rp, ap, table, np, fr, num)

  proc power5(rp, ap, table, np: W64; fr: ptr MontFrame; num: int) =
    # `bn_power5` (x86_64-mont5.pl:1113-1243) for `num` divisible by 8: the
    # five squarings, then `mul4x_internal` with `$rptr = $aptr`
    # (x86_64-mont5.pl:1219-1225; after `__bn_post4x_internal` `$aptr` is
    # `$rptr`), here through `bn_mul4x_mont_gather5`
    bnPower5Sqr5(rp, ap, table, np, fr, num)
    discard bnMul4xMontGather5(rp, rp, table, np, fr, num)

  func ctMontPow*(ctx: CtMont; base, exp: Limbs; expLimbs: LimbCount): Limbs =
    ## `base^exp` for `base < m` in Montgomery form, result in Montgomery
    ## form (below `m`). The exponent is read as `expLimbs` full 32-bit
    ## limbs. On the native backend this is the window-5 path of OpenSSL's
    ## `bn_mod_exp_mont_consttime` (`crypto/bn/bn_exp.c`, the
    ## `OPENSSL_BN_ASM_MONT5` branch, bn_exp.c:927-1040) driving the
    ## `ctbignumx64` port of `bn_mul_mont`, `bn_mul_mont_gather5`,
    ## `bn_power5`, `bn_scatter5`, `bn_gather5` and `bn_get_bits5`:
    ## `32 * expLimbs` squarings, one multiplication per 5 exponent bits by a
    ## table entry gathered with all 32 entries read.
    ##
    ## Differences at the boundary, each a public decision:
    ## - `bits = 32 * expLimbs`, the API's exponent width, where bn_exp.c:637
    ##   takes `p->top * 64`; the window is 5 for every width (bn_exp.c:703
    ##   and :710 pick it for widths above 306 bits; below, OpenSSL runs a
    ##   C fallback with a smaller window, not translated).
    ## - `a^0` is `ctx.one` (bn_exp.c:760, `bn_to_mont_fixed_top(1)`), not
    ##   the `2^(64 top) - m` of bn_exp.c:751-757, which branches on the top
    ##   bit of the secret `m`; both are `R mod m`.
    ## - `am` is `base`, already in Montgomery form (bn_exp.c:763-765).
    ## - The almost-reduced result (bn_exp.c:1025-1037) is converted out of
    ##   and back into Montgomery form (`bn_from_mont_fixed_top`, then
    ##   `ctToMont`), which leaves it fully reduced as this API promises.
    let top = montTop(ctx.m.n)                    # bn_exp.c:628 top = m->top
    var e = default(Limbs)                        # the exponent, zero above
    for i in 0 ..< expLimbs: e[i] = exp[i]
    let ptop = (expLimbs + 1) div 2               # p->top
    var bits = 32 * expLimbs                      # bn_exp.c:637
    var np = ctx.m.limbs                          # bn_exp.c:953 copy of N.d
    var am = base                                 # bn_exp.c:763
    var tmp = ctx.one                             # bn_exp.c:760
    var powerbuf = default(array[32 * MaxTop, uint64])  # bn_exp.c:722-735
    var tp = default(TpBuf)
    var mask = default(array[32, uint64])
    let pb = cast[W64](addr powerbuf)
    {.cast(noSideEffect).}:
      var fr = initFrame(tp, mask, montN0(ctx.m.limbs), top)
      bnScatter5(w64(tmp), top, pb, 0)                          # bn_exp.c:956
      bnScatter5(w64(am), top, pb, 1)                           # bn_exp.c:957
      discard bnMulMont(w64(tmp), w64(am), w64(am), w64(np), addr fr, top)  # :958
      bnScatter5(w64(tmp), top, pb, 2)                          # bn_exp.c:959
      var i = 4
      while i < 32:                                             # bn_exp.c:968
        discard bnMulMont(w64(tmp), w64(tmp), w64(tmp), w64(np), addr fr, top)  # :969
        bnScatter5(w64(tmp), top, pb, i)                        # bn_exp.c:970
        i = i * 2
      i = 3
      while i < 8:                                              # bn_exp.c:972
        fr.power = i - 1
        mulMontGather5(w64(tmp), w64(am), pb, w64(np), addr fr, top)  # :974
        bnScatter5(w64(tmp), top, pb, i)                        # bn_exp.c:975
        var j = 2 * i
        while j < 32:                                           # bn_exp.c:976
          discard bnMulMont(w64(tmp), w64(tmp), w64(tmp), w64(np), addr fr, top)  # :977
          bnScatter5(w64(tmp), top, pb, j)                      # bn_exp.c:978
          j = j * 2
        i = i + 2
      while i < 16:                                             # bn_exp.c:981
        fr.power = i - 1
        mulMontGather5(w64(tmp), w64(am), pb, w64(np), addr fr, top)  # :982
        bnScatter5(w64(tmp), top, pb, i)                        # bn_exp.c:983
        discard bnMulMont(w64(tmp), w64(tmp), w64(tmp), w64(np), addr fr, top)  # :984
        bnScatter5(w64(tmp), top, pb, 2 * i)                    # bn_exp.c:985
        i = i + 2
      while i < 32:                                             # bn_exp.c:987
        fr.power = i - 1
        mulMontGather5(w64(tmp), w64(am), pb, w64(np), addr fr, top)  # :988
        bnScatter5(w64(tmp), top, pb, i)                        # bn_exp.c:989
        i = i + 2
      let window0 = (bits - 1) mod 5 + 1                        # bn_exp.c:998
      let wmask = (1'u64 shl window0) - 1'u64                   # bn_exp.c:999
      bits = bits - window0                                     # bn_exp.c:1000
      let wvalue = bnGetBits(e, ptop, bits) and wmask           # bn_exp.c:1001
      bnGather5(w64(tmp), top, pb, int(wvalue), fr.mask)        # bn_exp.c:1002
      if (top and 7) != 0:                                      # bn_exp.c:1008
        while bits > 0:                                         # bn_exp.c:1009
          for k in 0 ..< 5:                                     # bn_exp.c:1010-1014
            discard bnMulMont(w64(tmp), w64(tmp), w64(tmp), w64(np), addr fr, top)
          bits = bits - 5                                       # bn_exp.c:1016
          fr.power = bnGetBits5(w64(e), bits)
          mulMontGather5(w64(tmp), w64(tmp), pb, w64(np), addr fr, top)  # :1015
      else:
        while bits > 0:                                         # bn_exp.c:1019
          bits = bits - 5                                       # bn_exp.c:1021
          fr.power = bnGetBits5(w64(e), bits)
          power5(w64(tmp), w64(tmp), pb, w64(np), addr fr, top) # bn_exp.c:1020
      wipeWords(powerbuf)                                       # bn_exp.c:1081
      wipeWords(tp)
      wipeWords(mask)          # not in bn_exp.c: the masks of the last gather
      fr.power = 0
    result = ctToMont(ctx, ctFromMont(ctx, tmp))                # bn_exp.c:1060

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
    key = RsaCrt(n: n, e: e, nm: default(CtMont), p: default(CtMont),
                 q: default(CtMont), dP: dP.limbs, dQ: dQ.limbs,
                 pm2: default(Limbs), qm2: default(Limbs), qInvM: default(Limbs))
    if (n.limbs[0] and 1'u32) == 0'u32: return false
    if cmp(e, n) >= 0: return false
    if (e.limbs[0] and 1'u32) == 0'u32 or (e.n == 1 and e.limbs[0] < 3'u32): return false
    let h = p.n
    if q.n != h or dP.n > h or dQ.n > h or qInv.n > h: return false
    if h < 2 or h > (MaxLimbs - 2) div 2 or n.n > h + h: return false
    key.nm = ctInitMont(n)                # odd, and above e >= 3
    let prod = ctMul(p.limbs, q.limbs, h)
    let same = ctEqualMask(prod.limbs, n.limbs, prod.n)
    key.p = ctInitMont(p)
    key.q = ctInitMont(q)
    var two = default(Limbs)
    two[0] = 2'u32
    key.pm2 = p.limbs
    discard ctSub(key.pm2, two, h)
    key.qm2 = q.limbs
    discard ctSub(key.qm2, two, h)
    key.qInvM = ctToMont(key.p, ctMod(key.p, qInv.limbs, h))
    result = same == 0xFFFFFFFF'u32

  func rsaCrtModBytes*(key: RsaCrt): int =
    ## `k`, the length of the modulus in bytes.
    result = (key.n.bitLen + 7) div 8

  func rsaCrtBlindLen*(key: RsaCrt): int =
    ## How many random bytes `rsaCrtPrivate` wants for EACH of `blindP` and
    ## `blindQ`: the prime's width plus 64 bits, so the reduction mod the prime
    ## leaves a negligible bias.
    result = 4 * key.p.m.n + 8

  func rsaCrtRaw*(key: RsaCrt; c: BigNum; blindP, blindQ: Limbs; blindLimbs: LimbCount): BigNum =
    ## `c^d mod n` by CRT with base blinding and NO fault check; `rsaCrtPrivate`
    ## is the entry point. `blindP` and `blindQ` are random `blindLimbs`-limb
    ## numbers (reduced mod `p` and `q` here). For `c < n`.
    let h = key.p.m.n
    # per prime: cp = c * r^e (mod p); mp = cp^dP * r^-1 = c^dP (mod p)
    let rP = ctToMont(key.p, ctMod(key.p, blindP, blindLimbs))
    let cP = ctMontMul(key.p, ctToMont(key.p, ctMod(key.p, c.limbs, c.n)),
                       ctMontPow(key.p, rP, key.e.limbs, key.e.n))
    var mP = ctMontMul(key.p, ctMontPow(key.p, cP, key.dP, h),
                       ctMontPow(key.p, rP, key.pm2, h))
    mP = ctFromMont(key.p, mP)
    let rQ = ctToMont(key.q, ctMod(key.q, blindQ, blindLimbs))
    let cQ = ctMontMul(key.q, ctToMont(key.q, ctMod(key.q, c.limbs, c.n)),
                       ctMontPow(key.q, rQ, key.e.limbs, key.e.n))
    var mQ = ctMontMul(key.q, ctMontPow(key.q, cQ, key.dQ, h),
                       ctMontPow(key.q, rQ, key.qm2, h))
    mQ = ctFromMont(key.q, mQ)
    # Garner: s = mq + q * (qInv * (mp - mq) mod p)
    let diff = ctModSub(key.p, mP, ctMod(key.p, mQ, h))
    let hh = ctMontMul(key.p, diff, key.qInvM)
    if h <= (MaxLimbs - 2) div 2:
      result = ctMul(hh, key.q.m.limbs, h)
      discard ctAdd(result.limbs, mQ, result.n)   # below n: no carry out
    else:
      result = BigNum(limbs: default(Limbs), n: 1)

  func rsaCrtCheckMask*(key: RsaCrt; s, c: BigNum): uint32 {.noinline.} =
    ## The fault check (Boneh-DeMillo-Lipton) as a mask: all ones when `s < n`
    ## and `s^e mod n == c`, else zero. `s` may be as wide as `p.n + q.n`
    ## limbs; the limbs past `n.n` must be zero. Constant time in `s`: the
    ## candidate is not yet known to be a valid signature, so `s^e` is
    ## `ctMontPow` over `e`'s `e.n` limbs, not `bignum.modExp`.
    ## (`noinline` so that tests/ctgrind can count its instructions by name.)
    let nn = key.n.n
    let below = ctLessMask(s.limbs, key.n.limbs, nn) and highZeroMask(s.limbs, nn)
    var sn = s.limbs
    # a value at or past `n` is zeroed: `ctMontMul` wants its inputs below `n`
    andMask(sn, below)
    let v = ctFromMont(key.nm, ctMontPow(key.nm, ctToMont(key.nm, sn),
                                         key.e.limbs, key.e.n))
    # over every limb: `v` is zero past `n.n`, so a `c >= n` cannot match
    let same = ctEqualMask(v, c.limbs, MaxLimbs - 2)
    result = below and same

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

  proc wipeLimbs(x: var Limbs) =
    for i in 0 ..< MaxLimbs:
      volatileStore(addr x[i], 0'u32)

  proc wipe*(key: var RsaCrt) =
    ## Overwrites the secret limbs of `key` with stores the C compiler may not
    ## drop. The public parts stay.
    wipeLimbs(key.p.m.limbs)
    wipeLimbs(key.p.r2)
    wipeLimbs(key.p.one)
    key.p.m0inv = 0'u32
    wipeLimbs(key.q.m.limbs)
    wipeLimbs(key.q.r2)
    wipeLimbs(key.q.one)
    key.q.m0inv = 0'u32
    wipeLimbs(key.dP)
    wipeLimbs(key.dQ)
    wipeLimbs(key.pm2)
    wipeLimbs(key.qm2)
    wipeLimbs(key.qInvM)

else:
  # The portable path: BearSSL's i31 code, see `ctbignumport`.
  include std/crypto/private/ctbignumport
