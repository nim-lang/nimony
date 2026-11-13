#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The secret-dependent limb arithmetic of `std/crypto/private/ctbignum` for
## x86-64, in `{.assembler.}`: what the RSA private-key operation runs on the
## native backend.
##
## Two parts:
##
## 1. The Montgomery multiplication and the constant-time exponentiation
##    kernels are a line-by-line translation of OpenSSL's plain x86-64
##    `mul`/`adc` code paths:
##
##    - `bnMulMont`: `bn_mul_mont`, `.Lmul_enter` (`crypto/bn/asm/x86_64-mont.pl`)
##    - `bnMulMontGather5`: `bn_mul_mont_gather5`, `.Lmul_enter`
##    - `bnMul4xMontGather5`: `bn_mul4x_mont_gather5` + `mul4x_internal`
##    - `bnPower5Sqr5`: `bn_power5`'s five `__bn_sqr8x_internal` +
##      `__bn_sqr8x_reduction` + `__bn_post4x_internal`
##    - `bnScatter5`, `bnGather5`, `bnGetBits5`: `bn_scatter5`, `bn_gather5`,
##      `bn_get_bits5`
##
##    (all but the first from `crypto/bn/asm/x86_64-mont5.pl`) of OpenSSL
##    commit 5dbd6501ee6c07c8a397df5c979743c05d775603
##    (https://github.com/openssl/openssl), Copyright The OpenSSL Project
##    Authors, the CRYPTOGAMS code of Andy Polyakov, licensed under the
##    Apache License 2.0 (https://www.openssl.org/source/license.html). The
##    driver, `bn_mod_exp_mont_consttime` of `crypto/bn/bn_exp.c`, is
##    translated in `ctbignum.nim`. Every statement (or small group) cites
##    the reference line it translates, as `x86_64-mont.pl:212`. Not
##    translated: the mulx/adx variants, and `bn_mul_mont`'s dispatch to
##    `bn_mul4x_mont`/`bn_sqr8x_mont` (x86_64-mont.pl:120-131), so
##    `bnMulMont` always runs `.Lmul_enter`; `bnNegInv64` is not from the
##    reference (OpenSSL computes `n0` in C).
##
##    Where `{.assembler.}` cannot say what the reference says, the closest
##    equivalent is used and the spot says why. The recurring cases:
##
##    - `%rsp` and `%rbp` belong to the frame. The reference's temporary
##      vectors (`tp`, `t[]`, the gather masks, the saved `n0*a[i]` words)
##      live on its own stack; here the caller passes them in a
##      `MontFrame`. `%rbp`'s role goes to another register, and values that
##      no longer fit in registers live in `{.stack.}` slots (`num`, `n0`,
##      `rp`, and in place of `%xmm1..%xmm4`).
##    - An address has a base and a literal displacement OR a base and a
##      scaled index, never both, and the scale is the element size. So
##      `-16(%rsp,$j,8)` is `tpm2[j]` through a second pointer
##      `tpm2 = tp - 2 words`; byte-offset indices with displacements
##      (`-8($ap,$j)` .. `16($ap,$j)`) become running pointers or a running
##      word index, as each proc's doc explains.
##    - There is no `lea`. `lea k($r),$r` is an `add` (on an `int` name of
##      the register when it holds a pointer, see `advRcx`), `lea 1($j),$j`
##      an `inc`. Both write flags, so where the reference puts its `lea`
##      between an instruction that sets CF and the one that reads it, the
##      `lea` moves after the reader; the spot says so.
##    - The reference's loops enter in the middle (`jmp .L1st_enter`) and
##      close with `jne`; here they are `while true` loops rotated to start
##      at the entry label, closed by `if zf(): break` at the `jne`. The
##      instructions run in the same order.
##    - The flags are unknown at the head of every `while true` (arkham's
##      rule), so a borrow that the reference carries in CF around a loop
##      (`.Lsub`) is saved with `sbb c,c` before the back edge and restored
##      with `neg c` at the head.
##    - SSE2 (`pcmpeqd`/`pand`/`por` over `%xmm`) is not in the vocabulary.
##      The masked gather of a table entry is done with the same masks in
##      64-bit registers: every one of the 32 entries is loaded, and-ed with
##      its mask and or-ed in (`buildMasks`, `gatherEntry`).
##    - No `call` and no `jmp` between routines: `bn_power5`'s internal
##      subroutines are inlined in one loop, and the jump of `mul4x_internal`
##      into `__bn_post4x_internal` is the shared template `sqr4xSub`.
##
## 2. Small helpers that are not Montgomery arithmetic (`ctxAdd`, `ctxSub`,
##    `ctxCondSub`, `ctxMul`, ...): Nimony's own design, over 32-bit limbs.
##    Numbers are little-endian 32-bit limbs, as in `std/crypto/bignum`;
##    each limb is loaded zero-extended into a 64-bit register (`mov r32,
##    [mem]`), so a 32x32-bit product plus two 32-bit addends fits one 64-bit
##    register and no carry flag has to survive between limbs. A register
##    therefore has two names below: a `uint32` one for the load and the
##    store, a `uint64` one for the arithmetic.
##
## Why assembler: the operands are an RSA prime, a CRT exponent, a blinding
## factor and everything derived from them, and constant time is a property
## of the INSTRUCTIONS executed. An `{.assembler.}` body is one instruction
## per statement, so what is written here is what runs. Every loop counts a
## public limb count, every memory index is a loop counter or a public
## offset, and every condition on a secret is a mask, never a jump.
##
## Every count `n` is at least 1.

type
  W32* = ptr UncheckedArray[uint32]
  W64* = ptr UncheckedArray[uint64]
  W16 = ptr UncheckedArray[uint16]
  MontFrame* = object
    ## What the reference keeps on its own stack frame, passed by the caller:
    ## `n0[0]`, the 7th argument (`power`), the temporary vector `tp`
    ## (`num + 2` words, for `bn_mul_mont`'s and `bn_mul_mont_gather5`'s
    ## `alloca`), `tpm2 = tp - 2 words` (see the module doc) and the 32-word
    ## gather mask table (the reference's 256 bytes of masks).
    n0*: uint64
    power*: int
    tp*: W64
    tpm2*: W64
    mask*: W64
    t2n*: W64     ## `&t[2 num]` for `bn_power5` (48+8(%rsp,$num,2))
    tm1*: int     ## the address of `t[-1]`, where `bn_power5`'s reduction
                  ## stops its walk over the saved `n0*a[i]` words
  MontFramePtr* = ptr MontFrame

proc addq(d: var uint64; s: uint64) {.instruction: "add".}
proc subq(d: var uint64; s: uint64) {.instruction: "sub".}
proc sbbq(d: var uint64; s: uint64) {.instruction: "sbb".}
proc imulq(d: var uint64; s: uint64) {.instruction: "imul".}
proc andq(d: var uint64; s: uint64) {.instruction: "and".}
proc orq(d: var uint64; s: uint64) {.instruction: "or".}
proc xorq(d: var uint64; s: uint64) {.instruction: "xor".}
proc negq(d: var uint64) {.instruction: "neg".}
proc shlq(d: var uint64; c: int) {.instruction: "shl".}
proc shrq(d: var uint64; c: int) {.instruction: "shr".}
proc inci(d: var int) {.instruction: "inc".}
proc addi(d: var int; s: int) {.instruction: "add".}
proc cmpi(a, b: int) {.instruction: "cmp".}
proc incq(d: var uint64) {.instruction: "inc".}
proc cmpq(a, b: uint64) {.instruction: "cmp".}
proc zf(): bool {.instruction: "zf".}
proc nc(): bool {.instruction: "nc".}
proc nz(): bool {.instruction: "nz".}
proc cf(): bool {.instruction: "cf".}
proc mulq(lo: var uint64; hi: var uint64; s: uint64) {.instruction: "mul".}
proc adcq(d: var uint64; s: uint64) {.instruction: "adc".}
proc notq(d: var uint64) {.instruction: "not".}
proc xori(d: var int; s: int) {.instruction: "xor".}
proc subi(d: var int; s: int) {.instruction: "sub".}
proc deci(d: var int) {.instruction: "dec".}
proc negi(d: var int) {.instruction: "neg".}
proc sbbi(d: var int; s: int) {.instruction: "sbb".}
proc noti(d: var int) {.instruction: "not".}
proc shli(d: var int; c: int) {.instruction: "shl".}
proc shri(d: var int; c: int) {.instruction: "shr".}
proc sari(d: var int; c: int) {.instruction: "sar".}
proc decq(d: var uint64) {.instruction: "dec".}
proc andi(d: var int; s: int) {.instruction: "and".}
proc cmovai(d: var int; s: int) {.instruction: "cmova".}
proc shr32(d: var uint32; c: int) {.instruction: "shr".}
proc and32(d: var uint32; s: uint32) {.instruction: "and".}
proc xor32(d: var uint32; s: uint32) {.instruction: "xor".}

proc ctxAdd*(a {.register: "rdi".}: W32; b {.register: "rsi".}: W32;
             n {.register: "rdx".}: int): uint64 {.assembler.} =
  ## `a += b` over `n` limbs; returns the carry (0 or 1).
  var i {.register: "rcx".}: int
  var c {.register: "r8".}: uint64
  i = 0
  c = 0
  while true:
    var x32 {.register: "rax".}: uint32
    var y32 {.register: "r9".}: uint32
    x32 = a[i]
    y32 = b[i]
    var x {.register: "rax".}: uint64
    var y {.register: "r9".}: uint64
    addq(x, y)
    addq(x, c)
    var lo {.register: "rax".}: uint32
    a[i] = lo
    var s {.register: "rax".}: uint64
    shrq(s, 32)
    c = s
    inci(i)
    cmpi(i, n)
    if zf(): break
  result = c

proc ctxSub*(a {.register: "rdi".}: W32; b {.register: "rsi".}: W32;
             n {.register: "rdx".}: int): uint64 {.assembler.} =
  ## `a -= b` over `n` limbs; returns the borrow (0 or 1). A limb difference
  ## that goes negative wraps the 64-bit register, so bit 63 is the borrow.
  var i {.register: "rcx".}: int
  var br {.register: "r8".}: uint64
  i = 0
  br = 0
  while true:
    var x32 {.register: "rax".}: uint32
    var y32 {.register: "r9".}: uint32
    x32 = a[i]
    y32 = b[i]
    var x {.register: "rax".}: uint64
    var y {.register: "r9".}: uint64
    subq(x, y)
    subq(x, br)
    var lo {.register: "rax".}: uint32
    a[i] = lo
    var d {.register: "rax".}: uint64
    shrq(d, 63)
    br = d
    inci(i)
    cmpi(i, n)
    if zf(): break
  result = br

proc ctxLess*(a {.register: "rdi".}: W32; b {.register: "rsi".}: W32;
              n {.register: "rdx".}: int): uint64 {.assembler.} =
  ## All ones when `a < b` over `n` limbs: the borrow of `a - b`, nothing
  ## stored.
  var i {.register: "rcx".}: int
  var br {.register: "r8".}: uint64
  i = 0
  br = 0
  while true:
    var x32 {.register: "rax".}: uint32
    var y32 {.register: "r9".}: uint32
    x32 = a[i]
    y32 = b[i]
    var x {.register: "rax".}: uint64
    var y {.register: "r9".}: uint64
    subq(x, y)
    subq(x, br)
    shrq(x, 63)
    br = x
    inci(i)
    cmpi(i, n)
    if zf(): break
  negq(br)                     # 1 -> all ones
  result = br

proc ctxEqual*(a {.register: "rdi".}: W32; b {.register: "rsi".}: W32;
               n {.register: "rdx".}: int): uint64 {.assembler.} =
  ## All ones when the `n` limbs of `a` and `b` agree: the differences are
  ## or-ed together, and `acc - 1` borrows exactly when `acc` is zero.
  var i {.register: "rcx".}: int
  var acc {.register: "r8".}: uint64
  var m {.register: "r10".}: uint64
  i = 0
  acc = 0
  while true:
    var x32 {.register: "rax".}: uint32
    var y32 {.register: "r9".}: uint32
    x32 = a[i]
    y32 = b[i]
    var x {.register: "rax".}: uint64
    var y {.register: "r9".}: uint64
    xorq(x, y)
    orq(acc, x)
    inci(i)
    cmpi(i, n)
    if zf(): break
  m = 0
  subq(acc, 1)                 # CF iff acc was 0
  sbbq(m, m)                   # m = -CF
  result = m

proc ctxMaskNonZero*(x {.register: "rdi".}: uint64): uint64 {.assembler.} =
  ## All ones when `x != 0`: `neg` sets CF exactly then.
  var m {.register: "rax".}: uint64
  var v {.register: "rdi".}: uint64
  negq(v)
  m = 0
  sbbq(m, m)
  result = m

proc ctxSelect*(mask {.register: "rdi".}: uint64; a {.register: "rsi".}: uint64;
                b {.register: "rdx".}: uint64): uint64 {.assembler.} =
  ## `b xor ((a xor b) and mask)`: `a` under an all-ones mask, `b` under zero.
  var r {.register: "rax".}: uint64
  r = a
  xorq(r, b)
  andq(r, mask)
  xorq(r, b)
  result = r

proc ctxAndMask*(a {.register: "rdi".}: W32; n {.register: "rsi".}: int;
                 mask {.register: "rdx".}: uint64) {.assembler.} =
  ## `a[i] = a[i] and mask` over `n` limbs.
  var i {.register: "rcx".}: int
  i = 0
  while true:
    var x32 {.register: "rax".}: uint32
    x32 = a[i]
    var x {.register: "rax".}: uint64
    andq(x, mask)
    var lo {.register: "rax".}: uint32
    a[i] = lo
    inci(i)
    cmpi(i, n)
    if zf(): break

proc ctxCondSub*(x {.register: "rdi".}: W32; m {.register: "rsi".}: W32;
                 n {.register: "rdx".}: int; top {.register: "rcx".}: uint64) {.assembler.} =
  ## For `x + top * 2^(32n) < 2m`: `x -= m` unless that would go negative.
  ## One pass finds the borrow of `x - m`; the second subtracts `m` and a
  ## mask, all ones when `top != 0` or there was no borrow.
  var i {.register: "r8".}: int
  var br {.register: "r9".}: uint64
  var mask {.register: "r10".}: uint64
  var t {.register: "r11".}: uint64
  i = 0
  br = 0
  while true:
    var x32 {.register: "rax".}: uint32
    var y32 {.register: "r11".}: uint32
    x32 = x[i]
    y32 = m[i]
    var v {.register: "rax".}: uint64
    var y {.register: "r11".}: uint64
    subq(v, y)
    subq(v, br)
    shrq(v, 63)
    br = v
    inci(i)
    cmpi(i, n)
    if zf(): break
  mask = br
  subq(mask, 1)                # all ones iff no borrow
  t = top
  negq(t)                      # CF iff top != 0
  sbbq(t, t)
  orq(mask, t)
  i = 0
  br = 0
  while true:
    var x32 {.register: "rax".}: uint32
    var y32 {.register: "r11".}: uint32
    x32 = x[i]
    y32 = m[i]
    var v {.register: "rax".}: uint64
    var y {.register: "r11".}: uint64
    andq(y, mask)
    subq(v, y)
    subq(v, br)
    var lo {.register: "rax".}: uint32
    x[i] = lo
    var d {.register: "rax".}: uint64
    shrq(d, 63)
    br = d
    inci(i)
    cmpi(i, n)
    if zf(): break

proc ctxMaskedAdd*(a {.register: "rdi".}: W32; m {.register: "rsi".}: W32;
                   n {.register: "rdx".}: int; bit {.register: "rcx".}: uint64) {.assembler.} =
  ## `a += m` over `n` limbs when `bit != 0`, `a += 0` otherwise; the carry
  ## out is dropped.
  var i {.register: "r8".}: int
  var c {.register: "r9".}: uint64
  var mask {.register: "r10".}: uint64
  mask = bit
  negq(mask)
  sbbq(mask, mask)
  i = 0
  c = 0
  while true:
    var x32 {.register: "rax".}: uint32
    var y32 {.register: "r11".}: uint32
    x32 = a[i]
    y32 = m[i]
    var x {.register: "rax".}: uint64
    var y {.register: "r11".}: uint64
    andq(y, mask)
    addq(x, y)
    addq(x, c)
    var lo {.register: "rax".}: uint32
    a[i] = lo
    var s {.register: "rax".}: uint64
    shrq(s, 32)
    c = s
    inci(i)
    cmpi(i, n)
    if zf(): break

proc ctxShiftIn*(r {.register: "rdi".}: W32; n {.register: "rsi".}: int;
                 word {.register: "rdx".}: uint64; shift {.register: "rcx".}: int): uint64 {.assembler.} =
  ## `r = 2r + bit` over `n` limbs, `bit` being bit `shift` of `word`;
  ## returns the bit shifted out of the top (0 or 1).
  var carry {.register: "r8".}: uint64
  var i {.register: "r9".}: int
  carry = word
  shrq(carry, shift)
  andq(carry, 1)
  i = 0
  while true:
    var x32 {.register: "rax".}: uint32
    x32 = r[i]
    var x {.register: "rax".}: uint64
    shlq(x, 1)
    orq(x, carry)
    var lo {.register: "rax".}: uint32
    r[i] = lo
    var s {.register: "rax".}: uint64
    shrq(s, 32)
    carry = s
    inci(i)
    cmpi(i, n)
    if zf(): break
  result = carry

proc ctxMul*(r {.register: "rdi".}: W32; a {.register: "rsi".}: W32;
             b {.register: "rdx".}: W32; h {.register: "rcx".}: int) {.assembler.} =
  ## `r = a * b` for two `h`-limb numbers, schoolbook; `r` (2h limbs) must
  ## be zero on entry.
  var i {.register: "r8".}: int
  var c {.register: "r10".}: uint64
  var j {.register: "r11".}: int
  var k {.register: "rbx".}: int
  i = 0
  while true:
    var a32 {.register: "r9".}: uint32
    a32 = a[i]
    var ai {.register: "r9".}: uint64
    c = 0
    j = 0
    k = i
    while true:
      var x32 {.register: "rax".}: uint32
      var y32 {.register: "r12".}: uint32
      x32 = b[j]
      y32 = r[k]
      var x {.register: "rax".}: uint64
      var y {.register: "r12".}: uint64
      imulq(x, ai)
      addq(x, y)
      addq(x, c)
      var lo {.register: "rax".}: uint32
      r[k] = lo
      var s {.register: "rax".}: uint64
      shrq(s, 32)
      c = s
      inci(j)
      inci(k)
      cmpi(j, h)
      if zf(): break
    var c32 {.register: "r10".}: uint32
    r[k] = c32
    inci(i)
    cmpi(i, h)
    if zf(): break

proc ctxNegInv*(m0 {.register: "rdi".}: uint64): uint64 {.assembler.} =
  ## `-m0^-1 mod 2^32` for odd `m0` by Newton's iteration `inv = inv *
  ## (2 - m0 * inv)`, five steps from `inv = 1` (1, 2, 4, 8, 16, 32 bits).
  ## The upper halves of the products are garbage that the final shifts drop.
  var inv {.register: "rax".}: uint64
  var t {.register: "rcx".}: uint64
  var y {.register: "rdx".}: uint64
  inv = 1
  t = m0
  imulq(t, inv)
  y = 2
  subq(y, t)
  imulq(inv, y)
  t = m0
  imulq(t, inv)
  y = 2
  subq(y, t)
  imulq(inv, y)
  t = m0
  imulq(t, inv)
  y = 2
  subq(y, t)
  imulq(inv, y)
  t = m0
  imulq(t, inv)
  y = 2
  subq(y, t)
  imulq(inv, y)
  t = m0
  imulq(t, inv)
  y = 2
  subq(y, t)
  imulq(inv, y)
  negq(inv)
  shlq(inv, 32)
  shrq(inv, 32)
  result = inv


# ---------------------------------------------------------------------------
# OpenSSL port: x86_64-mont.pl and x86_64-mont5.pl (plain mul/adc paths).
# Numbers are little-endian 64-bit words, `num >= 2` of them.
# ---------------------------------------------------------------------------

proc bnNegInv64*(m0 {.register: "rdi".}: uint64): uint64 {.assembler.} =
  ## `n0 = -m0^-1 mod 2^64` for odd `m0`. NOT from the assembler reference:
  ## OpenSSL computes `n0` in C (`BN_MONT_CTX_set` in `bn_mont.c`, through
  ## `BN_mod_inverse`); here it is Newton's iteration `inv = inv * (2 - m0 *
  ## inv)`, six steps from `inv = 1` (1, 2, 4, 8, 16, 32, 64 bits), no branch.
  var inv {.register: "rax".}: uint64
  var t {.register: "rcx".}: uint64
  var y {.register: "rdx".}: uint64
  inv = 1
  t = m0
  imulq(t, inv)
  y = 2
  subq(y, t)
  imulq(inv, y)
  t = m0
  imulq(t, inv)
  y = 2
  subq(y, t)
  imulq(inv, y)
  t = m0
  imulq(t, inv)
  y = 2
  subq(y, t)
  imulq(inv, y)
  t = m0
  imulq(t, inv)
  y = 2
  subq(y, t)
  imulq(inv, y)
  t = m0
  imulq(t, inv)
  y = 2
  subq(y, t)
  imulq(inv, y)
  t = m0
  imulq(t, inv)
  y = 2
  subq(y, t)
  imulq(inv, y)
  negq(inv)
  result = inv

proc bnMulMont*(rp {.register: "rdi".}: W64; ap {.register: "rsi".}: W64;
                bp0 {.register: "rdx".}: W64; np {.register: "rcx".}: W64;
                fr {.register: "r8".}: MontFramePtr;
                num {.register: "r9".}: int): int {.assembler.} =
  ## `bn_mul_mont`, the `.Lmul_enter` path (x86_64-mont.pl:131-371):
  ## `rp = ap * bp * 2^(-64 num) mod np`, the result below `np` when the
  ## product is below `np * 2^(64 num)`; `rp` may alias `ap` or `bp`. `n0`
  ## and `tp` come in `fr`. The dispatch of x86_64-mont.pl:120-131 to
  ## `bn_mul4x_mont`/`bn_sqr8x_mont` is not taken: those are unrolled
  ## variants of this algorithm with the same result.
  # register map: $rp %rdi -> rpS (stack), $ap %rsi, $bp %rdx -> %r12
  # (x86_64-mont.pl:181), $np %rcx, $n0 %r8 -> n0S (stack), $num %r9 -> numS
  # (stack), $lo0 %r10, $hi0 %r11, $hi1 %r13, $i %r14, $j %r15, $m0 %rbx,
  # $m1 %rbp -> %r8, tp (%rsp) -> %rdi, -16(%rsp) -> tpm2 %r9
  var numS {.stack.}: int
  var rpS {.stack.}: W64
  var n0S {.stack.}: uint64
  var bp {.register: "r12".}: W64
  var lo0 {.register: "r10".}: uint64
  var hi0 {.register: "r11".}: uint64
  var hi1 {.register: "r13".}: uint64
  var i {.register: "r14".}: int
  var j {.register: "r15".}: int
  var m0 {.register: "rbx".}: uint64
  numS = num                   # x86_64-mont.pl:117 (mov ${num}d,${num}d)
  rpS = rp
  bp = bp0                     # x86_64-mont.pl:181 (mov $bp,%r12)
  var lo {.register: "rax".}: uint64
  var hi {.register: "rdx".}: uint64
  lo = fr.n0                   # x86_64-mont.pl:187 (mov ($n0),$n0)
  n0S = lo
  var tp {.register: "rdi".}: W64
  var tpm2 {.register: "r9".}: W64
  tp = fr.tp                   # x86_64-mont.pl:150-178 (the alloca)
  tpm2 = fr.tpm2
  var m1 {.register: "r8".}: uint64
  m0 = bp[0]                   # x86_64-mont.pl:188
  lo = ap[0]                   # x86_64-mont.pl:189
  xori(i, i)                   # x86_64-mont.pl:191
  xori(j, j)                   # x86_64-mont.pl:192
  m1 = n0S                     # x86_64-mont.pl:194
  mulq(lo, hi, m0)             # x86_64-mont.pl:195 ap[0]*bp[0]
  lo0 = lo                     # x86_64-mont.pl:196
  lo = np[0]                   # x86_64-mont.pl:197
  imulq(m1, lo0)               # x86_64-mont.pl:199 "tp[0]"*n0
  hi0 = hi                     # x86_64-mont.pl:200
  mulq(lo, hi, m1)             # x86_64-mont.pl:202 np[0]*m1
  addq(lo0, lo)                # x86_64-mont.pl:203 discarded
  lo = ap[1]                   # x86_64-mont.pl:204
  adcq(hi, 0)                  # x86_64-mont.pl:205
  hi1 = hi                     # x86_64-mont.pl:206
  inci(j)                      # x86_64-mont.pl:208 j++
  # x86_64-mont.pl:209 jmp .L1st_enter: the loop is rotated to start there
  while true:
    # .L1st_enter
    mulq(lo, hi, m0)           # x86_64-mont.pl:223 ap[j]*bp[0]
    addq(hi0, lo)              # x86_64-mont.pl:224
    lo = np[j]                 # x86_64-mont.pl:225
    adcq(hi, 0)                # x86_64-mont.pl:226
    inci(j)                    # x86_64-mont.pl:227 j++
    lo0 = hi                   # x86_64-mont.pl:228
    mulq(lo, hi, m1)           # x86_64-mont.pl:230 np[j]*m1
    cmpi(j, numS)              # x86_64-mont.pl:231
    if zf(): break             # x86_64-mont.pl:232 jne .L1st
    # .L1st
    addq(hi1, lo)              # x86_64-mont.pl:213
    lo = ap[j]                 # x86_64-mont.pl:214
    adcq(hi, 0)                # x86_64-mont.pl:215
    addq(hi1, hi0)             # x86_64-mont.pl:216 np[j]*m1+ap[j]*bp[0]
    hi0 = lo0                  # x86_64-mont.pl:217
    adcq(hi, 0)                # x86_64-mont.pl:218
    tpm2[j] = hi1              # x86_64-mont.pl:219 tp[j-1]
    hi1 = hi                   # x86_64-mont.pl:220
  addq(hi1, lo)                # x86_64-mont.pl:234
  lo = ap[0]                   # x86_64-mont.pl:235 ap[0]
  adcq(hi, 0)                  # x86_64-mont.pl:236
  addq(hi1, hi0)               # x86_64-mont.pl:237 np[j]*m1+ap[j]*bp[0]
  adcq(hi, 0)                  # x86_64-mont.pl:238
  tpm2[j] = hi1                # x86_64-mont.pl:239 tp[j-1]
  hi1 = hi                     # x86_64-mont.pl:240
  hi0 = lo0                    # x86_64-mont.pl:241
  xorq(hi, hi)                 # x86_64-mont.pl:243
  addq(hi1, hi0)               # x86_64-mont.pl:244
  adcq(hi, 0)                  # x86_64-mont.pl:245
  # x86_64-mont.pl:246-247 store to -8(%rsp,$num,8) and (%rsp,$num,8): here
  # $j == $num, and the two `inc` step tpm2[j] onto tp[num-1] and tp[num]
  # ($j is cleared at .Louter before its next use)
  inci(j)
  tpm2[j] = hi1                # x86_64-mont.pl:246
  inci(j)
  tpm2[j] = hi                 # x86_64-mont.pl:247 store upmost overflow bit
  inci(i)                      # x86_64-mont.pl:249 i++
  # x86_64-mont.pl:250 jmp .Louter
  while true:
    # .Louter
    m0 = bp[i]                 # x86_64-mont.pl:253 m0=bp[i]
    xori(j, j)                 # x86_64-mont.pl:254 j=0
    m1 = n0S                   # x86_64-mont.pl:255
    lo0 = tp[0]                # x86_64-mont.pl:256
    mulq(lo, hi, m0)           # x86_64-mont.pl:257 ap[0]*bp[i]
    addq(lo0, lo)              # x86_64-mont.pl:258 ap[0]*bp[i]+tp[0]
    lo = np[0]                 # x86_64-mont.pl:259
    adcq(hi, 0)                # x86_64-mont.pl:260
    imulq(m1, lo0)             # x86_64-mont.pl:262 tp[0]*n0
    hi0 = hi                   # x86_64-mont.pl:263
    mulq(lo, hi, m1)           # x86_64-mont.pl:265 np[0]*m1
    addq(lo0, lo)              # x86_64-mont.pl:266 discarded
    lo = ap[1]                 # x86_64-mont.pl:267
    adcq(hi, 0)                # x86_64-mont.pl:268
    lo0 = tp[1]                # x86_64-mont.pl:269 tp[1]
    hi1 = hi                   # x86_64-mont.pl:270
    inci(j)                    # x86_64-mont.pl:272 j++
    # x86_64-mont.pl:273 jmp .Linner_enter: rotated loop
    while true:
      # .Linner_enter
      mulq(lo, hi, m0)         # x86_64-mont.pl:287 ap[j]*bp[i]
      addq(hi0, lo)            # x86_64-mont.pl:288
      lo = np[j]               # x86_64-mont.pl:289
      adcq(hi, 0)              # x86_64-mont.pl:290
      addq(lo0, hi0)           # x86_64-mont.pl:291 ap[j]*bp[i]+tp[j]
      hi0 = hi                 # x86_64-mont.pl:292
      adcq(hi0, 0)             # x86_64-mont.pl:293
      inci(j)                  # x86_64-mont.pl:294 j++
      mulq(lo, hi, m1)         # x86_64-mont.pl:296 np[j]*m1
      cmpi(j, numS)            # x86_64-mont.pl:297
      if zf(): break           # x86_64-mont.pl:298 jne .Linner
      # .Linner
      addq(hi1, lo)            # x86_64-mont.pl:277
      lo = ap[j]               # x86_64-mont.pl:278
      adcq(hi, 0)              # x86_64-mont.pl:279
      addq(hi1, lo0)           # x86_64-mont.pl:280 np[j]*m1+ap[j]*bp[i]+tp[j]
      lo0 = tp[j]              # x86_64-mont.pl:281
      adcq(hi, 0)              # x86_64-mont.pl:282
      tpm2[j] = hi1            # x86_64-mont.pl:283 tp[j-1]
      hi1 = hi                 # x86_64-mont.pl:284
    addq(hi1, lo)              # x86_64-mont.pl:300
    lo = ap[0]                 # x86_64-mont.pl:301 ap[0]
    adcq(hi, 0)                # x86_64-mont.pl:302
    addq(hi1, lo0)             # x86_64-mont.pl:303 np[j]*m1+ap[j]*bp[i]+tp[j]
    lo0 = tp[j]                # x86_64-mont.pl:304
    adcq(hi, 0)                # x86_64-mont.pl:305
    tpm2[j] = hi1              # x86_64-mont.pl:306 tp[j-1]
    hi1 = hi                   # x86_64-mont.pl:307
    xorq(hi, hi)               # x86_64-mont.pl:309
    addq(hi1, hi0)             # x86_64-mont.pl:310
    adcq(hi, 0)                # x86_64-mont.pl:311
    addq(hi1, lo0)             # x86_64-mont.pl:312 pull upmost overflow bit
    adcq(hi, 0)                # x86_64-mont.pl:313
    # x86_64-mont.pl:314-315: -8(%rsp,$num,8), (%rsp,$num,8) as at :246
    inci(j)
    tpm2[j] = hi1              # x86_64-mont.pl:314
    inci(j)
    tpm2[j] = hi               # x86_64-mont.pl:315 store upmost overflow bit
    inci(i)                    # x86_64-mont.pl:317 i++
    cmpi(i, numS)              # x86_64-mont.pl:318
    if nc(): break             # x86_64-mont.pl:319 jb .Louter
  var c {.register: "r11".}: uint64     # the CF saved across .Lsub's back edge
  var r {.register: "r12".}: W64        # $rp, back from its stack slot
  r = rpS
  xori(i, i)                   # x86_64-mont.pl:321 i=0 and clear CF!
  lo = tp[0]                   # x86_64-mont.pl:322 tp[0]
  j = numS                     # x86_64-mont.pl:323 j=num
  c = 0                        # CF = 0 as left by x86_64-mont.pl:321
  while true:
    # .Lsub
    negq(c)                    # CF from the previous iteration
    sbbq(lo, np[i])            # x86_64-mont.pl:326
    r[i] = lo                  # x86_64-mont.pl:327 rp[i]=tp[i]-np[i]
    inci(i)                    # x86_64-mont.pl:329 i++ (inc keeps CF)
    lo = tp[i]                 # x86_64-mont.pl:328 tp[i+1], after the i++
    sbbq(c, c)                 # save CF for the back edge and the exit
    deci(j)                    # x86_64-mont.pl:330 doesn't affect CF!
    if zf(): break             # x86_64-mont.pl:331 jnz .Lsub
  var mbx {.register: "rbx".}: uint64
  negq(c)                      # CF of the last x86_64-mont.pl:326
  sbbq(lo, 0)                  # x86_64-mont.pl:333 handle upmost overflow bit
  mbx = 0xFFFF_FFFF_FFFF_FFFF'u64  # x86_64-mont.pl:334
  xorq(mbx, lo)                # x86_64-mont.pl:335 not %rax
  xori(i, i)                   # x86_64-mont.pl:336
  j = numS                     # x86_64-mont.pl:337 j=num
  var nreg {.register: "r8".}: int      # $num, for the store of :344
  nreg = numS
  while true:
    # .Lcopy: conditional copy
    var x {.register: "rcx".}: uint64
    var y {.register: "rdx".}: uint64
    x = r[i]                   # x86_64-mont.pl:340
    y = tp[i]                  # x86_64-mont.pl:341
    andq(x, mbx)               # x86_64-mont.pl:342
    andq(y, lo)                # x86_64-mont.pl:343
    var tpi {.register: "rdi".}: ptr UncheckedArray[int]
    tpi[i] = nreg              # x86_64-mont.pl:344 zap temporary vector
    orq(y, x)                  # x86_64-mont.pl:345
    r[i] = y                   # x86_64-mont.pl:346 rp[i]=tp[i]
    inci(i)                    # x86_64-mont.pl:347
    subi(j, 1)                 # x86_64-mont.pl:348
    if zf(): break             # x86_64-mont.pl:349 jnz .Lcopy
  result = 1                   # x86_64-mont.pl:353 mov $1,%rax

template gatherEntry(acc, x: var uint64; k: var int; tbl, mask: W64) =
  ## One masked gather of a 64-bit word from the 32 interleaved powers at
  ## `tbl` (x86_64-mont5.pl:238-259, :328-354, :3551-3580): the reference
  ## and-s and or-s all 32 entries two at a time in `%xmm` registers; here
  ## one at a time in 64-bit registers, the same loads and the same masks.
  xorq(acc, acc)               # pxor %xmm4,%xmm4; pxor %xmm5,%xmm5
  xori(k, k)
  while true:
    x = tbl[k]                 # movdqa 16*$k-128($bp),%xmm0
    andq(x, mask[k])           # pand 16*$k-128(%rdx),%xmm0
    orq(acc, x)                # por %xmm0,%xmm4
    inci(k)
    cmpi(k, 32)
    if zf(): break

template buildMasks(mask: W64; idx: int; k, mi: var int; mq: var uint64) =
  ## The 32 masks `mask[k] = (k == idx) ? -1 : 0` (x86_64-mont5.pl:176-237,
  ## :3516-3548): the reference compares the broadcast index with the
  ## counters 0..31 by `pcmpeqd`; here `neg` sets CF exactly when `k xor
  ## idx` is non-zero and `sbb`/`not` turn it into the mask.
  xori(k, k)
  while true:
    mi = k
    xori(mi, idx)
    negi(mi)                   # CF iff k != idx
    sbbi(mi, mi)               # -1 iff k != idx
    noti(mi)                   # -1 iff k == idx  (pcmpeqd)
    mask[k] = mq               # movdqa %xmm0,16*$k+112(%r10)
    inci(k)
    cmpi(k, 32)
    if zf(): break

proc bnMulMontGather5*(rp {.register: "rdi".}: W64; ap {.register: "rsi".}: W64;
                       table {.register: "rdx".}: W64; np {.register: "rcx".}: W64;
                       fr {.register: "r8".}: MontFramePtr;
                       num {.register: "r9".}: int): int {.assembler.} =
  ## `bn_mul_mont_gather5`, the `.Lmul_enter` path (x86_64-mont5.pl:121-476):
  ## `rp = ap * table[power] * 2^(-64 num) mod np`, `table` holding the 32
  ## powers interleaved by `bnScatter5`, `power = fr.power` the 7th argument.
  ## The `test $7,num` dispatch to `bn_mul4x_mont_gather5`
  ## (x86_64-mont5.pl:110-118) is the caller's.
  # register map as in `bnMulMont`; the mask table (on the reference's
  # stack at -112+24(%rsp,$num,8)) is `fr.mask`, its pointer kept in maskS
  var numS {.stack.}: int
  var rpS {.stack.}: W64
  var n0S {.stack.}: uint64
  var maskS {.stack.}: W64
  var bp {.register: "r12".}: W64
  var i {.register: "r14".}: int
  var j {.register: "r15".}: int
  var m0 {.register: "rbx".}: uint64
  numS = num                   # x86_64-mont5.pl:107 (mov ${num}d,${num}d)
  rpS = rp
  bp = table                   # x86_64-mont5.pl:170 lea 128($bp),%r12
  var lo {.register: "rax".}: uint64
  var hi {.register: "rdx".}: uint64
  var mk {.register: "r13".}: W64
  var idx {.register: "r11".}: int
  var mi {.register: "r10".}: int
  var mq {.register: "r10".}: uint64
  mk = fr.mask               # x86_64-mont5.pl:178-179 (place the mask)
  maskS = mk
  idx = fr.power             # x86_64-mont5.pl:122 movd 8(%rsp),%xmm5
  buildMasks(mk, idx, j, mi, mq)   # x86_64-mont5.pl:176-237
  var x {.register: "r10".}: uint64
  gatherEntry(m0, x, j, bp, mk)    # x86_64-mont5.pl:238-257, :259 m0=bp[0]
  var bpi {.register: "r12".}: int
  addi(bpi, 256)             # x86_64-mont5.pl:258 lea $STRIDE($bp),$bp
  # %r10, %r11, %r13 held the masks' pointer, index and counter: re-declared
  var lo0 {.register: "r10".}: uint64
  var hi0 {.register: "r11".}: uint64
  var hi1 {.register: "r13".}: uint64
  lo = fr.n0                   # x86_64-mont5.pl:261 (mov ($n0),$n0)
  n0S = lo
  var tp {.register: "rdi".}: W64
  var tpm2 {.register: "r9".}: W64
  tp = fr.tp                   # x86_64-mont5.pl:137-165 (the alloca)
  tpm2 = fr.tpm2
  var m1 {.register: "r8".}: uint64
  var bq {.register: "r12".}: W64     # $bp after the `lea`
  lo = ap[0]                   # x86_64-mont5.pl:262
  xori(i, i)                   # x86_64-mont5.pl:264
  xori(j, j)                   # x86_64-mont5.pl:265
  m1 = n0S                     # x86_64-mont5.pl:267
  mulq(lo, hi, m0)             # x86_64-mont5.pl:268 ap[0]*bp[0]
  lo0 = lo                     # x86_64-mont5.pl:269
  lo = np[0]                   # x86_64-mont5.pl:270
  imulq(m1, lo0)               # x86_64-mont5.pl:272 "tp[0]"*n0
  hi0 = hi                     # x86_64-mont5.pl:273
  mulq(lo, hi, m1)             # x86_64-mont5.pl:275 np[0]*m1
  addq(lo0, lo)                # x86_64-mont5.pl:276 discarded
  lo = ap[1]                   # x86_64-mont5.pl:277
  adcq(hi, 0)                  # x86_64-mont5.pl:278
  hi1 = hi                     # x86_64-mont5.pl:279
  inci(j)                      # x86_64-mont5.pl:281 j++
  # x86_64-mont5.pl:282 jmp .L1st_enter: the loop is rotated to start there
  while true:
    # .L1st_enter
    mulq(lo, hi, m0)           # x86_64-mont5.pl:296 ap[j]*bp[0]
    addq(hi0, lo)              # x86_64-mont5.pl:297
    lo = np[j]                 # x86_64-mont5.pl:298
    adcq(hi, 0)                # x86_64-mont5.pl:299
    inci(j)                    # x86_64-mont5.pl:300 j++
    lo0 = hi                   # x86_64-mont5.pl:301
    mulq(lo, hi, m1)           # x86_64-mont5.pl:303 np[j]*m1
    cmpi(j, numS)              # x86_64-mont5.pl:304
    if zf(): break             # x86_64-mont5.pl:305 jne .L1st
    # .L1st
    addq(hi1, lo)              # x86_64-mont5.pl:286
    lo = ap[j]                 # x86_64-mont5.pl:287
    adcq(hi, 0)                # x86_64-mont5.pl:288
    addq(hi1, hi0)             # x86_64-mont5.pl:289 np[j]*m1+ap[j]*bp[0]
    hi0 = lo0                  # x86_64-mont5.pl:290
    adcq(hi, 0)                # x86_64-mont5.pl:291
    tpm2[j] = hi1              # x86_64-mont5.pl:292 tp[j-1]
    hi1 = hi                   # x86_64-mont5.pl:293
  addq(hi1, lo)                # x86_64-mont5.pl:308
  adcq(hi, 0)                  # x86_64-mont5.pl:309
  addq(hi1, hi0)               # x86_64-mont5.pl:310 np[j]*m1+ap[j]*bp[0]
  adcq(hi, 0)                  # x86_64-mont5.pl:311
  tpm2[j] = hi1                # x86_64-mont5.pl:312 tp[num-1]
  hi1 = hi                     # x86_64-mont5.pl:313
  hi0 = lo0                    # x86_64-mont5.pl:314
  xorq(hi, hi)                 # x86_64-mont5.pl:316
  addq(hi1, hi0)               # x86_64-mont5.pl:317
  adcq(hi, 0)                  # x86_64-mont5.pl:318
  # x86_64-mont5.pl:319-320 -8(%rsp,$num,8), (%rsp,$num,8): $j == $num here;
  # two `inc` step tpm2[j] onto them ($j is cleared before its next use)
  inci(j)
  tpm2[j] = hi1                # x86_64-mont5.pl:319
  inci(j)
  tpm2[j] = hi                 # x86_64-mont5.pl:320 store upmost overflow bit
  inci(i)                      # x86_64-mont5.pl:322 i++
  # x86_64-mont5.pl:323 jmp .Louter
  while true:
    # .Louter
    var mk {.register: "rdx".}: W64
    mk = maskS               # x86_64-mont5.pl:326-327 where the mask is
    gatherEntry(m0, lo, j, bq, mk)   # x86_64-mont5.pl:328-350, :354 m0=bp[i]
    var bpi {.register: "r12".}: int
    addi(bpi, 256)           # x86_64-mont5.pl:351 lea $STRIDE($bp),$bp
    var hh {.register: "rdx".}: uint64   # %rdx held the mask pointer
    lo = ap[0]                 # x86_64-mont5.pl:353 ap[0]
    xori(j, j)                 # x86_64-mont5.pl:356 j=0
    m1 = n0S                   # x86_64-mont5.pl:357
    lo0 = tp[0]                # x86_64-mont5.pl:358
    mulq(lo, hh, m0)           # x86_64-mont5.pl:360 ap[0]*bp[i]
    addq(lo0, lo)              # x86_64-mont5.pl:361 ap[0]*bp[i]+tp[0]
    lo = np[0]                 # x86_64-mont5.pl:362
    adcq(hh, 0)                # x86_64-mont5.pl:363
    imulq(m1, lo0)             # x86_64-mont5.pl:365 tp[0]*n0
    hi0 = hh                   # x86_64-mont5.pl:366
    mulq(lo, hh, m1)           # x86_64-mont5.pl:368 np[0]*m1
    addq(lo0, lo)              # x86_64-mont5.pl:369 discarded
    lo = ap[1]                 # x86_64-mont5.pl:370
    adcq(hh, 0)                # x86_64-mont5.pl:371
    lo0 = tp[1]                # x86_64-mont5.pl:372 tp[1]
    hi1 = hh                   # x86_64-mont5.pl:373
    inci(j)                    # x86_64-mont5.pl:375 j++
    # x86_64-mont5.pl:376 jmp .Linner_enter: rotated loop
    while true:
      # .Linner_enter
      mulq(lo, hh, m0)         # x86_64-mont5.pl:390 ap[j]*bp[i]
      addq(hi0, lo)            # x86_64-mont5.pl:391
      lo = np[j]               # x86_64-mont5.pl:392
      adcq(hh, 0)              # x86_64-mont5.pl:393
      addq(lo0, hi0)           # x86_64-mont5.pl:394 ap[j]*bp[i]+tp[j]
      hi0 = hh                 # x86_64-mont5.pl:395
      adcq(hi0, 0)             # x86_64-mont5.pl:396
      inci(j)                  # x86_64-mont5.pl:397 j++
      mulq(lo, hh, m1)         # x86_64-mont5.pl:399 np[j]*m1
      cmpi(j, numS)            # x86_64-mont5.pl:400
      if zf(): break           # x86_64-mont5.pl:401 jne .Linner
      # .Linner
      addq(hi1, lo)            # x86_64-mont5.pl:380
      lo = ap[j]               # x86_64-mont5.pl:381
      adcq(hh, 0)              # x86_64-mont5.pl:382
      addq(hi1, lo0)           # x86_64-mont5.pl:383 np[j]*m1+ap[j]*bp[i]+tp[j]
      lo0 = tp[j]              # x86_64-mont5.pl:384
      adcq(hh, 0)              # x86_64-mont5.pl:385
      tpm2[j] = hi1            # x86_64-mont5.pl:386 tp[j-1]
      hi1 = hh                 # x86_64-mont5.pl:387
    addq(hi1, lo)              # x86_64-mont5.pl:403
    adcq(hh, 0)                # x86_64-mont5.pl:404
    addq(hi1, lo0)             # x86_64-mont5.pl:405 np[j]*m1+ap[j]*bp[i]+tp[j]
    lo0 = tp[j]                # x86_64-mont5.pl:406 (%rsp,$num,8)
    adcq(hh, 0)                # x86_64-mont5.pl:407
    tpm2[j] = hi1              # x86_64-mont5.pl:408 tp[num-1]
    hi1 = hh                   # x86_64-mont5.pl:409
    xorq(hh, hh)               # x86_64-mont5.pl:411
    addq(hi1, hi0)             # x86_64-mont5.pl:412
    adcq(hh, 0)                # x86_64-mont5.pl:413
    addq(hi1, lo0)             # x86_64-mont5.pl:414 pull upmost overflow bit
    adcq(hh, 0)                # x86_64-mont5.pl:415
    # x86_64-mont5.pl:416-417 as at :319-320
    inci(j)
    tpm2[j] = hi1              # x86_64-mont5.pl:416
    inci(j)
    tpm2[j] = hh               # x86_64-mont5.pl:417 store upmost overflow bit
    inci(i)                    # x86_64-mont5.pl:419 i++
    cmpi(i, numS)              # x86_64-mont5.pl:420
    if nc(): break             # x86_64-mont5.pl:421 jb .Louter
  var c {.register: "r11".}: uint64     # the CF saved across .Lsub's back edge
  var r {.register: "r12".}: W64        # $rp, back from its stack slot
  r = rpS
  xori(i, i)                   # x86_64-mont5.pl:423 i=0 and clear CF!
  lo = tp[0]                   # x86_64-mont5.pl:424 tp[0]
  # x86_64-mont5.pl:425 lea (%rsp),$ap: tp[i+1] is read as tp[i] after i++
  j = numS                     # x86_64-mont5.pl:426 j=num
  c = 0                        # CF = 0 as left by x86_64-mont5.pl:423
  while true:
    # .Lsub
    negq(c)                    # CF from the previous iteration
    sbbq(lo, np[i])            # x86_64-mont5.pl:429
    r[i] = lo                  # x86_64-mont5.pl:430 rp[i]=tp[i]-np[i]
    inci(i)                    # x86_64-mont5.pl:432 i++ (inc keeps CF)
    lo = tp[i]                 # x86_64-mont5.pl:431 tp[i+1], after the i++
    sbbq(c, c)                 # save CF for the back edge and the exit
    deci(j)                    # x86_64-mont5.pl:433 doesn't affect CF!
    if zf(): break             # x86_64-mont5.pl:434 jnz .Lsub
  var mbx {.register: "rbx".}: uint64
  negq(c)                      # CF of the last x86_64-mont5.pl:429
  sbbq(lo, 0)                  # x86_64-mont5.pl:436 handle upmost overflow bit
  mbx = 0xFFFF_FFFF_FFFF_FFFF'u64  # x86_64-mont5.pl:437
  xorq(mbx, lo)                # x86_64-mont5.pl:438
  xori(i, i)                   # x86_64-mont5.pl:439
  j = numS                     # x86_64-mont5.pl:440 j=num
  while true:
    # .Lcopy: conditional copy
    var x {.register: "rcx".}: uint64
    var y {.register: "rdx".}: uint64
    x = r[i]                   # x86_64-mont5.pl:443
    y = tp[i]                  # x86_64-mont5.pl:444
    andq(x, mbx)               # x86_64-mont5.pl:445
    andq(y, lo)                # x86_64-mont5.pl:446
    var tpi {.register: "rdi".}: ptr UncheckedArray[int]
    tpi[i] = i                 # x86_64-mont5.pl:447 zap temporary vector
    orq(y, x)                  # x86_64-mont5.pl:448
    r[i] = y                   # x86_64-mont5.pl:449 rp[i]=tp[i]
    inci(i)                    # x86_64-mont5.pl:450
    subi(j, 1)                 # x86_64-mont5.pl:451
    if zf(): break             # x86_64-mont5.pl:452 jnz .Lcopy
  result = 1                   # x86_64-mont5.pl:456 mov $1,%rax

proc bnScatter5*(inp {.register: "rdi".}: W64; num {.register: "rsi".}: int;
                 tbl {.register: "rdx".}: W64; idx {.register: "rcx".}: int) {.assembler.} =
  ## `bn_scatter5` (x86_64-mont5.pl:3474-3492): word `k` of `inp` to
  ## `tbl[32 k + idx]`. `idx` is public (the precomputation order).
  var n {.register: "rsi".}: int
  var ti {.register: "rdx".}: int
  var s {.register: "rcx".}: int
  var x {.register: "rax".}: uint64
  cmpi(n, 0)                   # x86_64-mont5.pl:3479
  if nz():                     # x86_64-mont5.pl:3480 jz .Lscatter_epilogue
    shli(s, 3)                 # x86_64-mont5.pl:3481 lea ($tbl,$idx,8),$tbl
    addi(ti, s)
    while true:
      # .Lscatter
      var ip {.register: "rdi".}: W64
      var tp {.register: "rdx".}: W64
      x = ip[0]                # x86_64-mont5.pl:3483
      var ii {.register: "rdi".}: int
      addi(ii, 8)              # x86_64-mont5.pl:3484 lea 8($inp),$inp
      tp[0] = x                # x86_64-mont5.pl:3485
      var tq {.register: "rdx".}: int
      addi(tq, 256)            # x86_64-mont5.pl:3486 lea 32*8($tbl),$tbl
      subi(n, 1)               # x86_64-mont5.pl:3487
      if zf(): break           # x86_64-mont5.pl:3488 jnz .Lscatter

proc bnGather5*(outp {.register: "rdi".}: W64; num {.register: "rsi".}: int;
                tbl {.register: "rdx".}: W64; idx {.register: "rcx".}: int;
                mask {.register: "r8".}: W64) {.assembler.} =
  ## `bn_gather5` (x86_64-mont5.pl:3494-3590): `outp[k] = tbl[32 k + idx]`
  ## for `k < num`, every entry read. The reference builds its masks on its
  ## own stack (`sub $0x108,%rsp`); here in `mask`, 32 words from the caller.
  var n {.register: "rsi".}: int
  var k {.register: "r9".}: int
  var mi {.register: "r10".}: int
  var mq {.register: "r10".}: uint64
  var acc {.register: "rax".}: uint64
  var x {.register: "r10".}: uint64
  var t {.register: "r11".}: W64
  t = tbl                      # x86_64-mont5.pl:3511 lea 128($tbl),%r11
  buildMasks(mask, idx, k, mi, mq)   # x86_64-mont5.pl:3509-3548
  while true:
    # .Lgather
    gatherEntry(acc, x, k, t, mask)  # x86_64-mont5.pl:3551-3576
    var ti {.register: "r11".}: int
    addi(ti, 256)              # x86_64-mont5.pl:3577 lea $STRIDE(%r11),%r11
    var op {.register: "rdi".}: W64
    op[0] = acc                # x86_64-mont5.pl:3580 m0=bp[0]
    var oi {.register: "rdi".}: int
    addi(oi, 8)                # x86_64-mont5.pl:3581 lea 8($out),$out
    subi(n, 1)                 # x86_64-mont5.pl:3582
    if zf(): break             # x86_64-mont5.pl:3583 jnz .Lgather

proc bnGetBits5*(inp {.register: "rdi".}: W64; off {.register: "rsi".}: int): int {.assembler.} =
  ## `bn_get_bits5` (x86_64-mont5.pl:3453-3471): the 5 bits of `inp` from
  ## bit `off` on, by a 16-bit load at a byte offset chosen with `cmova`.
  var r10 {.register: "r10".}: int
  var r11 {.register: "r11".}: int
  var c {.register: "rcx".}: int
  var n {.register: "rsi".}: int
  var a {.register: "rax".}: int
  var ii {.register: "rdi".}: int
  r10 = ii                     # x86_64-mont5.pl:3459 lea 0($inp),%r10
  r11 = ii                     # x86_64-mont5.pl:3460 lea 1($inp),%r11
  addi(r11, 1)
  c = n                        # x86_64-mont5.pl:3461
  shri(n, 4)                   # x86_64-mont5.pl:3462
  andi(c, 15)                  # x86_64-mont5.pl:3463
  a = c                        # x86_64-mont5.pl:3464 lea -8(%ecx),%eax
  subi(a, 8)
  cmpi(c, 11)                  # x86_64-mont5.pl:3465
  cmovai(r10, r11)             # x86_64-mont5.pl:3466
  cmovai(c, a)                 # x86_64-mont5.pl:3467
  # x86_64-mont5.pl:3468 movzw (%r10,$num,2),%eax: no zero-extending load;
  # %eax is cleared first and the 16-bit load fills %ax
  var a32 {.register: "rax".}: uint32
  var p {.register: "r10".}: W16
  xor32(a32, a32)
  var a16 {.register: "rax".}: uint16
  a16 = p[n]
  var b32 {.register: "rax".}: uint32
  shr32(a32, c)                # x86_64-mont5.pl:3469 shrl %cl,%eax
  and32(a32, 31)               # x86_64-mont5.pl:3470
  var res {.register: "rax".}: int
  result = res

# Pointer arithmetic: the reference's `lea k($p),$p` on a pointer register.
# An `{.assembler.}` pointer local takes no `add`, so the register is renamed
# to an `int` for the `add` and back to a pointer after it (no instruction).
template advRcx(n: untyped) =
  var ri {.register: "rcx".}: int
  addi(ri, n)
  var rp {.register: "rcx".}: W64
template advRbx(n: untyped) =
  var ri {.register: "rbx".}: int
  addi(ri, n)
  var rp {.register: "rbx".}: W64
template advRsi(n: untyped) =
  var ri {.register: "rsi".}: int
  addi(ri, n)
  var rp {.register: "rsi".}: W64
template advRdi(n: untyped) =
  var ri {.register: "rdi".}: int
  addi(ri, n)
  var rp {.register: "rdi".}: W64
template advR8(n: untyped) =
  var ri {.register: "r8".}: int
  addi(ri, n)
  var rp {.register: "r8".}: W64
template advR11(n: untyped) =
  var ri {.register: "r11".}: int
  addi(ri, n)
  var rp {.register: "r11".}: W64
template advR12(n: untyped) =
  var ri {.register: "r12".}: int
  addi(ri, n)
  var rp {.register: "r12".}: W64
template advR14(n: untyped) =
  var ri {.register: "r14".}: int
  addi(ri, n)
  var rp {.register: "r14".}: W64

template sqr4xSub(s12, s13, s14, s15, smask, scy, stp, snp, srp, scnt: untyped) =
  ## x86_64-mont5.pl:2076-2106, `.Lsqr4x_sub` from `.Lsqr4x_sub_entry`:
  ## `rp = t + (n and mask)` over `num` words where `%r12..%r15` were
  ## preloaded with `n[0]-1, n[1..3]` (so that `not` gives `-n`), `%rax`
  ## is the mask, `%r10` the carry, `%rcx` minus the count of 4-word groups.
  ## Register map: $tptr %rbx, $nptr %rbp -> %r11, $rptr %rdi. The loop is
  ## rotated to start at the entry label.
  while true:
    # .Lsqr4x_sub_entry
    advR11(32)                 # x86_64-mont5.pl:2082 lea 8*4($nptr),$nptr
    notq(s12)                  # x86_64-mont5.pl:2083
    notq(s13)                  # x86_64-mont5.pl:2084
    notq(s14)                  # x86_64-mont5.pl:2085
    notq(s15)                  # x86_64-mont5.pl:2086
    andq(s12, smask)           # x86_64-mont5.pl:2087
    andq(s13, smask)           # x86_64-mont5.pl:2088
    andq(s14, smask)           # x86_64-mont5.pl:2089
    andq(s15, smask)           # x86_64-mont5.pl:2090
    negq(scy)                  # x86_64-mont5.pl:2092 mov %r10,%cf
    adcq(s12, stp[0])          # x86_64-mont5.pl:2093
    adcq(s13, stp[1])          # x86_64-mont5.pl:2094
    adcq(s14, stp[2])          # x86_64-mont5.pl:2095
    adcq(s15, stp[3])          # x86_64-mont5.pl:2096
    srp[0] = s12               # x86_64-mont5.pl:2097
    srp[1] = s13               # x86_64-mont5.pl:2099
    sbbq(scy, scy)             # x86_64-mont5.pl:2100 mov %cf,%r10
    advRbx(32)                 # x86_64-mont5.pl:2098 lea 8*4($tptr),$tptr,
                               # after the sbb that reads the adc's CF
    srp[2] = s14               # x86_64-mont5.pl:2101
    srp[3] = s15               # x86_64-mont5.pl:2102
    advRdi(32)                 # x86_64-mont5.pl:2103 lea 8*4($rptr),$rptr
    inci(scnt)                 # x86_64-mont5.pl:2105 pass %cf
    if zf(): break             # x86_64-mont5.pl:2106 jnz .Lsqr4x_sub
    # .Lsqr4x_sub
    s12 = snp[0]               # x86_64-mont5.pl:2077
    s13 = snp[1]               # x86_64-mont5.pl:2078
    s14 = snp[2]               # x86_64-mont5.pl:2079
    s15 = snp[3]               # x86_64-mont5.pl:2080

proc bnMul4xMontGather5*(rp {.register: "rdi".}: W64; ap {.register: "rsi".}: W64;
                         table {.register: "rdx".}: W64; np {.register: "rcx".}: W64;
                         fr {.register: "r8".}: MontFramePtr;
                         num {.register: "r9".}: int): int {.assembler.} =
  ## `bn_mul4x_mont_gather5` (x86_64-mont5.pl:483-589) with
  ## `mul4x_internal` (x86_64-mont5.pl:593-1049) and the final subtraction
  ## of `__bn_post4x_internal` it jumps into (x86_64-mont5.pl:2076-2106):
  ## `rp = ap * table[power] * 2^(-64 num) mod np` for `num` divisible by 8,
  ## almost reduced (below `2^(64 num)`). `tp` (`num + 1` words) and the
  ## mask table come in `fr`.
  ##
  ## Register map: $rp %rdi -> rpS (56+8(%rsp)), $ap %rsi, $bp %r12, $np
  ## %rcx, $n0 %r8 -> n0S, $m0 %rbx, $m1 %rbp -> %r8, @A %r10,%r11, @N
  ## %r13,%rdi, $tp %r14, $j %r15 -> k, $num %r9.
  ## `$num` is a byte count in the reference; here it is a word count, as
  ## a scaled index (`($ap,$num)` is `ap[num]`), and `numBS` holds it in
  ## bytes for the pointer rewinds (`lea ($np,$num),$np`). `($ap,$j)` with
  ## the displacements -8, 0, 8, 16 in one iteration has no form here: `k`
  ## is the index of the next of those four words, stepped by `inc` (which
  ## keeps CF) after each load, so `k = j/8 - 1` at the loop test, and
  ## `add $32,$j; jnz` becomes `cmp k,-1; jnz`. A `lea` that the reference
  ## places between an `add` and its `adc` is moved after the `adc`: it is
  ## an `add` here and would overwrite CF.
  var rpS {.stack.}: W64
  var n0S {.stack.}: uint64
  var bEndS {.stack.}: int
  var numBS {.stack.}: int
  var maskS {.stack.}: W64
  rpS = rp                     # x86_64-mont5.pl:693 save $rp
  # x86_64-mont5.pl:511-562 (the alloca) is the caller's `fr.tp`
  # mul4x_internal
  var t13 {.register: "r13".}: int
  var tbli {.register: "rdx".}: int
  var nn {.register: "r9".}: int
  t13 = nn                     # x86_64-mont5.pl:595 shl \$5,$num
  shli(t13, 8)
  addi(t13, tbli)              # x86_64-mont5.pl:598 lea 128(%rdx,$num),%r13
  bEndS = t13                  # x86_64-mont5.pl:692 save end of b[num]
  var bp {.register: "r12".}: W64
  var tb {.register: "rdx".}: W64
  bp = tb                      # x86_64-mont5.pl:609 lea 128(%rdx),$bp
  var mk {.register: "r10".}: W64
  var idx {.register: "r11".}: int
  var kk {.register: "r15".}: int
  var mi {.register: "r13".}: int
  var mq {.register: "r13".}: uint64
  mk = fr.mask                 # x86_64-mont5.pl:608 place the mask
  maskS = mk
  idx = fr.power               # x86_64-mont5.pl:596 load 7th argument
  buildMasks(mk, idx, kk, mi, mq)  # x86_64-mont5.pl:606-659
  var m0 {.register: "rbx".}: uint64
  var gx {.register: "r13".}: uint64
  gatherEntry(m0, gx, kk, bp, mk)  # x86_64-mont5.pl:660-688, :690 m0=bp[0]
  advR12(256)                  # x86_64-mont5.pl:689 lea $STRIDE($bp),$bp
  var lo {.register: "rax".}: uint64
  var hi {.register: "rdx".}: uint64
  lo = fr.n0                   # x86_64-mont5.pl:695 pull n0[0] value
  n0S = lo
  var tp {.register: "r14".}: W64
  tp = fr.tp                   # (x86_64-mont5.pl:706 lea 64+8(%rsp),$tp,
                               # loaded here while `fr` is in %r8)
  var m1 {.register: "r8".}: uint64
  var a0 {.register: "r10".}: uint64
  var a1 {.register: "r11".}: uint64
  var n0r {.register: "r13".}: uint64
  var n1 {.register: "rdi".}: uint64
  var k {.register: "r15".}: int
  lo = ap[0]                   # x86_64-mont5.pl:696
  t13 = nn                     # x86_64-mont5.pl:697 lea ($ap,$num),$ap
  shli(t13, 3)
  advRsi(t13)
  negi(t13)
  numBS = t13                  # -$num in bytes, for the rewinds
  negi(nn)                     # x86_64-mont5.pl:698 neg $num
  m1 = n0S                     # x86_64-mont5.pl:700
  mulq(lo, hi, m0)             # x86_64-mont5.pl:701 ap[0]*bp[0]
  a0 = lo                      # x86_64-mont5.pl:702
  lo = np[0]                   # x86_64-mont5.pl:703
  imulq(m1, a0)                # x86_64-mont5.pl:705 "tp[0]"*n0
  a1 = hi                      # x86_64-mont5.pl:707
  mulq(lo, hi, m1)             # x86_64-mont5.pl:709 np[0]*m1
  addq(a0, lo)                 # x86_64-mont5.pl:710 discarded
  k = nn                       # x86_64-mont5.pl:711 8($ap,$num): k = num+1
  inci(k)
  lo = ap[k]                   # x86_64-mont5.pl:711
  adcq(hi, 0)                  # x86_64-mont5.pl:712
  n1 = hi                      # x86_64-mont5.pl:713
  mulq(lo, hi, m0)             # x86_64-mont5.pl:715
  addq(a1, lo)                 # x86_64-mont5.pl:716
  lo = np[1]                   # x86_64-mont5.pl:717
  adcq(hi, 0)                  # x86_64-mont5.pl:718
  a0 = hi                      # x86_64-mont5.pl:719
  mulq(lo, hi, m1)             # x86_64-mont5.pl:721
  addq(n1, lo)                 # x86_64-mont5.pl:722
  inci(k)                      # x86_64-mont5.pl:723 16($ap,$num): k = num+2
  lo = ap[k]                   # x86_64-mont5.pl:723
  adcq(hi, 0)                  # x86_64-mont5.pl:724
  addq(n1, a1)                 # x86_64-mont5.pl:725
  inci(k)                      # x86_64-mont5.pl:726 lea 4*8($num),$j (k = j-1)
  adcq(hi, 0)                  # x86_64-mont5.pl:728
  advRcx(32)                   # x86_64-mont5.pl:727 lea 8*4($np),$np
  tp[0] = n1                   # x86_64-mont5.pl:729
  n0r = hi                     # x86_64-mont5.pl:730
  # x86_64-mont5.pl:731 jmp .L1st4x
  while true:
    # .L1st4x
    mulq(lo, hi, m0)           # x86_64-mont5.pl:735 ap[j]*bp[0]
    addq(a0, lo)               # x86_64-mont5.pl:736
    lo = np[-2]                # x86_64-mont5.pl:737
    adcq(hi, 0)                # x86_64-mont5.pl:739
    advR14(32)                 # x86_64-mont5.pl:738 lea 32($tp),$tp
    a1 = hi                    # x86_64-mont5.pl:740
    mulq(lo, hi, m1)           # x86_64-mont5.pl:742 np[j]*m1
    addq(n0r, lo)              # x86_64-mont5.pl:743
    lo = ap[k]                 # x86_64-mont5.pl:744 -8($ap,$j)
    inci(k)
    adcq(hi, 0)                # x86_64-mont5.pl:745
    addq(n0r, a0)              # x86_64-mont5.pl:746 np[j]*m1+ap[j]*bp[0]
    adcq(hi, 0)                # x86_64-mont5.pl:747
    tp[-3] = n0r               # x86_64-mont5.pl:748 tp[j-1]
    n1 = hi                    # x86_64-mont5.pl:749
    mulq(lo, hi, m0)           # x86_64-mont5.pl:751 ap[j]*bp[0]
    addq(a1, lo)               # x86_64-mont5.pl:752
    lo = np[-1]                # x86_64-mont5.pl:753
    adcq(hi, 0)                # x86_64-mont5.pl:754
    a0 = hi                    # x86_64-mont5.pl:755
    mulq(lo, hi, m1)           # x86_64-mont5.pl:757 np[j]*m1
    addq(n1, lo)               # x86_64-mont5.pl:758
    lo = ap[k]                 # x86_64-mont5.pl:759 ($ap,$j)
    inci(k)
    adcq(hi, 0)                # x86_64-mont5.pl:760
    addq(n1, a1)               # x86_64-mont5.pl:761 np[j]*m1+ap[j]*bp[0]
    adcq(hi, 0)                # x86_64-mont5.pl:762
    tp[-2] = n1                # x86_64-mont5.pl:763 tp[j-1]
    n0r = hi                   # x86_64-mont5.pl:764
    mulq(lo, hi, m0)           # x86_64-mont5.pl:766 ap[j]*bp[0]
    addq(a0, lo)               # x86_64-mont5.pl:767
    lo = np[0]                 # x86_64-mont5.pl:768
    adcq(hi, 0)                # x86_64-mont5.pl:769
    a1 = hi                    # x86_64-mont5.pl:770
    mulq(lo, hi, m1)           # x86_64-mont5.pl:772 np[j]*m1
    addq(n0r, lo)              # x86_64-mont5.pl:773
    lo = ap[k]                 # x86_64-mont5.pl:774 8($ap,$j)
    inci(k)
    adcq(hi, 0)                # x86_64-mont5.pl:775
    addq(n0r, a0)              # x86_64-mont5.pl:776 np[j]*m1+ap[j]*bp[0]
    adcq(hi, 0)                # x86_64-mont5.pl:777
    tp[-1] = n0r               # x86_64-mont5.pl:778 tp[j-1]
    n1 = hi                    # x86_64-mont5.pl:779
    mulq(lo, hi, m0)           # x86_64-mont5.pl:781 ap[j]*bp[0]
    addq(a1, lo)               # x86_64-mont5.pl:782
    lo = np[1]                 # x86_64-mont5.pl:783
    adcq(hi, 0)                # x86_64-mont5.pl:784
    a0 = hi                    # x86_64-mont5.pl:785
    mulq(lo, hi, m1)           # x86_64-mont5.pl:787 np[j]*m1
    addq(n1, lo)               # x86_64-mont5.pl:788
    lo = ap[k]                 # x86_64-mont5.pl:789 16($ap,$j)
    inci(k)
    adcq(hi, 0)                # x86_64-mont5.pl:790
    addq(n1, a1)               # x86_64-mont5.pl:791 np[j]*m1+ap[j]*bp[0]
    adcq(hi, 0)                # x86_64-mont5.pl:793
    advRcx(32)                 # x86_64-mont5.pl:792 lea 8*4($np),$np
    tp[0] = n1                 # x86_64-mont5.pl:794 tp[j-1]
    n0r = hi                   # x86_64-mont5.pl:795
    cmpi(k, -1)                # x86_64-mont5.pl:797 add \$32,$j (j == 0 iff k == -1)
    if zf(): break             # x86_64-mont5.pl:798 jnz .L1st4x
  mulq(lo, hi, m0)             # x86_64-mont5.pl:800 ap[j]*bp[0]
  addq(a0, lo)                 # x86_64-mont5.pl:801
  lo = np[-2]                  # x86_64-mont5.pl:802
  adcq(hi, 0)                  # x86_64-mont5.pl:804
  advR14(32)                   # x86_64-mont5.pl:803 lea 32($tp),$tp
  a1 = hi                      # x86_64-mont5.pl:805
  mulq(lo, hi, m1)             # x86_64-mont5.pl:807 np[j]*m1
  addq(n0r, lo)                # x86_64-mont5.pl:808
  lo = ap[-1]                  # x86_64-mont5.pl:809
  adcq(hi, 0)                  # x86_64-mont5.pl:810
  addq(n0r, a0)                # x86_64-mont5.pl:811 np[j]*m1+ap[j]*bp[0]
  adcq(hi, 0)                  # x86_64-mont5.pl:812
  tp[-3] = n0r                 # x86_64-mont5.pl:813 tp[j-1]
  n1 = hi                      # x86_64-mont5.pl:814
  mulq(lo, hi, m0)             # x86_64-mont5.pl:816 ap[j]*bp[0]
  addq(a1, lo)                 # x86_64-mont5.pl:817
  lo = np[-1]                  # x86_64-mont5.pl:818
  adcq(hi, 0)                  # x86_64-mont5.pl:819
  a0 = hi                      # x86_64-mont5.pl:820
  mulq(lo, hi, m1)             # x86_64-mont5.pl:822 np[j]*m1
  addq(n1, lo)                 # x86_64-mont5.pl:823
  lo = ap[nn]                  # x86_64-mont5.pl:824 ap[0]
  adcq(hi, 0)                  # x86_64-mont5.pl:825
  addq(n1, a1)                 # x86_64-mont5.pl:826 np[j]*m1+ap[j]*bp[0]
  adcq(hi, 0)                  # x86_64-mont5.pl:827
  tp[-2] = n1                  # x86_64-mont5.pl:828 tp[j-1]
  n0r = hi                     # x86_64-mont5.pl:829
  advRcx(numBS)                # x86_64-mont5.pl:831 rewind $np
  xorq(n1, n1)                 # x86_64-mont5.pl:833
  addq(n0r, a0)                # x86_64-mont5.pl:834
  adcq(n1, 0)                  # x86_64-mont5.pl:835
  tp[-1] = n0r                 # x86_64-mont5.pl:836
  # x86_64-mont5.pl:838 jmp .Louter4x
  while true:
    # .Louter4x
    var mk2 {.register: "rdx".}: W64
    mk2 = maskS                # x86_64-mont5.pl:842 where the mask is ...
    var gy {.register: "r13".}: uint64
    # ... but %r13 ($N[0]) is dead here and %rax is ap[0]: the gather
    # temporary is %r13, the counter %r15 ($j, set below)
    gatherEntry(m0, gy, k, bp, mk2)  # x86_64-mont5.pl:843-865, :867 m0=bp[i]
    advR12(256)                # x86_64-mont5.pl:866 lea $STRIDE($bp),$bp
    var hh {.register: "rdx".}: uint64
    a0 = tp[nn]                # x86_64-mont5.pl:869
    m1 = n0S                   # x86_64-mont5.pl:870
    mulq(lo, hh, m0)           # x86_64-mont5.pl:871 ap[0]*bp[i]
    addq(a0, lo)               # x86_64-mont5.pl:872 ap[0]*bp[i]+tp[0]
    lo = np[0]                 # x86_64-mont5.pl:873
    adcq(hh, 0)                # x86_64-mont5.pl:874
    imulq(m1, a0)              # x86_64-mont5.pl:876 tp[0]*n0
    a1 = hh                    # x86_64-mont5.pl:877
    tp[0] = n1                 # x86_64-mont5.pl:878 store upmost overflow bit
    advR14(numBS)              # x86_64-mont5.pl:880 rewind $tp
    mulq(lo, hh, m1)           # x86_64-mont5.pl:882 np[0]*m1
    addq(a0, lo)               # x86_64-mont5.pl:883 "$N[0]", discarded
    k = nn                     # x86_64-mont5.pl:884 8($ap,$num): k = num+1
    inci(k)
    lo = ap[k]                 # x86_64-mont5.pl:884
    adcq(hh, 0)                # x86_64-mont5.pl:885
    n1 = hh                    # x86_64-mont5.pl:886
    mulq(lo, hh, m0)           # x86_64-mont5.pl:888 ap[j]*bp[i]
    addq(a1, lo)               # x86_64-mont5.pl:889
    lo = np[1]                 # x86_64-mont5.pl:890
    adcq(hh, 0)                # x86_64-mont5.pl:891
    addq(a1, tp[1])            # x86_64-mont5.pl:892 +tp[1]
    adcq(hh, 0)                # x86_64-mont5.pl:893
    a0 = hh                    # x86_64-mont5.pl:894
    mulq(lo, hh, m1)           # x86_64-mont5.pl:896 np[j]*m1
    addq(n1, lo)               # x86_64-mont5.pl:897
    inci(k)                    # x86_64-mont5.pl:898 16($ap,$num): k = num+2
    lo = ap[k]                 # x86_64-mont5.pl:898
    adcq(hh, 0)                # x86_64-mont5.pl:899
    addq(n1, a1)               # x86_64-mont5.pl:900 np[j]*m1+ap[j]*bp[i]+tp[j]
    inci(k)                    # x86_64-mont5.pl:901 lea 4*8($num),$j (k = j-1)
    adcq(hh, 0)                # x86_64-mont5.pl:903
    advRcx(32)                 # x86_64-mont5.pl:902 lea 8*4($np),$np
    n0r = hh                   # x86_64-mont5.pl:904
    # x86_64-mont5.pl:905 jmp .Linner4x
    while true:
      # .Linner4x
      mulq(lo, hh, m0)         # x86_64-mont5.pl:909 ap[j]*bp[i]
      addq(a0, lo)             # x86_64-mont5.pl:910
      lo = np[-2]              # x86_64-mont5.pl:911
      adcq(hh, 0)              # x86_64-mont5.pl:912
      addq(a0, tp[2])          # x86_64-mont5.pl:913 ap[j]*bp[i]+tp[j]
      adcq(hh, 0)              # x86_64-mont5.pl:915
      advR14(32)               # x86_64-mont5.pl:914 lea 32($tp),$tp
      a1 = hh                  # x86_64-mont5.pl:916
      mulq(lo, hh, m1)         # x86_64-mont5.pl:918 np[j]*m1
      addq(n0r, lo)            # x86_64-mont5.pl:919
      lo = ap[k]               # x86_64-mont5.pl:920 -8($ap,$j)
      inci(k)
      adcq(hh, 0)              # x86_64-mont5.pl:921
      addq(n0r, a0)            # x86_64-mont5.pl:922
      adcq(hh, 0)              # x86_64-mont5.pl:923
      tp[-4] = n1              # x86_64-mont5.pl:924 tp[j-1]
      n1 = hh                  # x86_64-mont5.pl:925
      mulq(lo, hh, m0)         # x86_64-mont5.pl:927 ap[j]*bp[i]
      addq(a1, lo)             # x86_64-mont5.pl:928
      lo = np[-1]              # x86_64-mont5.pl:929
      adcq(hh, 0)              # x86_64-mont5.pl:930
      addq(a1, tp[-1])         # x86_64-mont5.pl:931
      adcq(hh, 0)              # x86_64-mont5.pl:932
      a0 = hh                  # x86_64-mont5.pl:933
      mulq(lo, hh, m1)         # x86_64-mont5.pl:935 np[j]*m1
      addq(n1, lo)             # x86_64-mont5.pl:936
      lo = ap[k]               # x86_64-mont5.pl:937 ($ap,$j)
      inci(k)
      adcq(hh, 0)              # x86_64-mont5.pl:938
      addq(n1, a1)             # x86_64-mont5.pl:939
      adcq(hh, 0)              # x86_64-mont5.pl:940
      tp[-3] = n0r             # x86_64-mont5.pl:941 tp[j-1]
      n0r = hh                 # x86_64-mont5.pl:942
      mulq(lo, hh, m0)         # x86_64-mont5.pl:944 ap[j]*bp[i]
      addq(a0, lo)             # x86_64-mont5.pl:945
      lo = np[0]               # x86_64-mont5.pl:946
      adcq(hh, 0)              # x86_64-mont5.pl:947
      addq(a0, tp[0])          # x86_64-mont5.pl:948 ap[j]*bp[i]+tp[j]
      adcq(hh, 0)              # x86_64-mont5.pl:949
      a1 = hh                  # x86_64-mont5.pl:950
      mulq(lo, hh, m1)         # x86_64-mont5.pl:952 np[j]*m1
      addq(n0r, lo)            # x86_64-mont5.pl:953
      lo = ap[k]               # x86_64-mont5.pl:954 8($ap,$j)
      inci(k)
      adcq(hh, 0)              # x86_64-mont5.pl:955
      addq(n0r, a0)            # x86_64-mont5.pl:956
      adcq(hh, 0)              # x86_64-mont5.pl:957
      tp[-2] = n1              # x86_64-mont5.pl:958 tp[j-1]
      n1 = hh                  # x86_64-mont5.pl:959
      mulq(lo, hh, m0)         # x86_64-mont5.pl:961 ap[j]*bp[i]
      addq(a1, lo)             # x86_64-mont5.pl:962
      lo = np[1]               # x86_64-mont5.pl:963
      adcq(hh, 0)              # x86_64-mont5.pl:964
      addq(a1, tp[1])          # x86_64-mont5.pl:965
      adcq(hh, 0)              # x86_64-mont5.pl:966
      a0 = hh                  # x86_64-mont5.pl:967
      mulq(lo, hh, m1)         # x86_64-mont5.pl:969 np[j]*m1
      addq(n1, lo)             # x86_64-mont5.pl:970
      lo = ap[k]               # x86_64-mont5.pl:971 16($ap,$j)
      inci(k)
      adcq(hh, 0)              # x86_64-mont5.pl:972
      addq(n1, a1)             # x86_64-mont5.pl:973
      adcq(hh, 0)              # x86_64-mont5.pl:975
      advRcx(32)               # x86_64-mont5.pl:974 lea 8*4($np),$np
      tp[-1] = n0r             # x86_64-mont5.pl:976 tp[j-1]
      n0r = hh                 # x86_64-mont5.pl:977
      cmpi(k, -1)              # x86_64-mont5.pl:979 add \$32,$j (j == 0 iff k == -1)
      if zf(): break           # x86_64-mont5.pl:980 jnz .Linner4x
    mulq(lo, hh, m0)           # x86_64-mont5.pl:982 ap[j]*bp[i]
    addq(a0, lo)               # x86_64-mont5.pl:983
    lo = np[-2]                # x86_64-mont5.pl:984
    adcq(hh, 0)                # x86_64-mont5.pl:985
    addq(a0, tp[2])            # x86_64-mont5.pl:986 ap[j]*bp[i]+tp[j]
    adcq(hh, 0)                # x86_64-mont5.pl:988
    advR14(32)                 # x86_64-mont5.pl:987 lea 32($tp),$tp
    a1 = hh                    # x86_64-mont5.pl:989
    mulq(lo, hh, m1)           # x86_64-mont5.pl:991 np[j]*m1
    addq(n0r, lo)              # x86_64-mont5.pl:992
    lo = ap[-1]                # x86_64-mont5.pl:993
    adcq(hh, 0)                # x86_64-mont5.pl:994
    addq(n0r, a0)              # x86_64-mont5.pl:995
    adcq(hh, 0)                # x86_64-mont5.pl:996
    tp[-4] = n1                # x86_64-mont5.pl:997 tp[j-1]
    n1 = hh                    # x86_64-mont5.pl:998
    mulq(lo, hh, m0)           # x86_64-mont5.pl:1000 ap[j]*bp[i]
    addq(a1, lo)               # x86_64-mont5.pl:1001
    lo = m1                    # x86_64-mont5.pl:1002
    m1 = np[-1]                # x86_64-mont5.pl:1003
    adcq(hh, 0)                # x86_64-mont5.pl:1004
    addq(a1, tp[-1])           # x86_64-mont5.pl:1005
    adcq(hh, 0)                # x86_64-mont5.pl:1006
    a0 = hh                    # x86_64-mont5.pl:1007
    mulq(lo, hh, m1)           # x86_64-mont5.pl:1009 np[j]*m1
    addq(n1, lo)               # x86_64-mont5.pl:1010
    lo = ap[nn]                # x86_64-mont5.pl:1011 ap[0]
    adcq(hh, 0)                # x86_64-mont5.pl:1012
    addq(n1, a1)               # x86_64-mont5.pl:1013
    adcq(hh, 0)                # x86_64-mont5.pl:1014
    tp[-3] = n0r               # x86_64-mont5.pl:1015 tp[j-1]
    n0r = hh                   # x86_64-mont5.pl:1016
    tp[-2] = n1                # x86_64-mont5.pl:1018 tp[j-1]
    advRcx(numBS)              # x86_64-mont5.pl:1019 rewind $np
    xorq(n1, n1)               # x86_64-mont5.pl:1021
    addq(n0r, a0)              # x86_64-mont5.pl:1022
    adcq(n1, 0)                # x86_64-mont5.pl:1023
    addq(n0r, tp[0])           # x86_64-mont5.pl:1024 pull upmost overflow bit
    adcq(n1, 0)                # x86_64-mont5.pl:1025 upmost overflow bit
    tp[-1] = n0r               # x86_64-mont5.pl:1026
    var bpi {.register: "r12".}: int
    cmpi(bpi, bEndS)           # x86_64-mont5.pl:1028
    var bp2 {.register: "r12".}: W64
    if nc(): break             # x86_64-mont5.pl:1029 jb .Louter4x
  # x86_64-mont5.pl:1033-1049: set up for .Lsqr4x_sub_entry
  var smask {.register: "rax".}: uint64
  xorq(smask, smask)           # x86_64-mont5.pl:1033
  xori(k, k)                   # $j is zero at :1035 in the reference
                               # (here k = -1): cleared before the sub
  subq(m1, n0r)                # x86_64-mont5.pl:1034 compare top-most words
  var kq {.register: "r15".}: uint64
  adcq(kq, kq)                 # x86_64-mont5.pl:1035 $j is zero
  orq(n1, kq)                  # x86_64-mont5.pl:1036
  subq(smask, n1)              # x86_64-mont5.pl:1037 %rax=-$N[1]
  var stp {.register: "rbx".}: W64
  stp = tp                     # x86_64-mont5.pl:1038 lea ($tp,$num),%rbx
  advRbx(numBS)
  var s12 {.register: "r12".}: uint64
  s12 = np[0]                  # x86_64-mont5.pl:1039
  var snp {.register: "r11".}: W64
  snp = np                     # x86_64-mont5.pl:1040 lea ($np),%rbp
  var scnt {.register: "rcx".}: int
  scnt = nn                    # x86_64-mont5.pl:1041
  sari(scnt, 2)                # x86_64-mont5.pl:1042 sar \$3+2,%rcx ($num
                               # in words here)
  var srp {.register: "rdi".}: W64
  srp = rpS                    # x86_64-mont5.pl:1043 rptr in .sqr4x_sub
  decq(s12)                    # x86_64-mont5.pl:1044 so that after 'not' we get -n[0]
  var scy {.register: "r10".}: uint64
  xorq(scy, scy)               # x86_64-mont5.pl:1045
  var s13 {.register: "r13".}: uint64
  var s14 {.register: "r14".}: uint64
  var s15 {.register: "r15".}: uint64
  s13 = snp[1]                 # x86_64-mont5.pl:1046
  s14 = snp[2]                 # x86_64-mont5.pl:1047
  s15 = snp[3]                 # x86_64-mont5.pl:1048
  sqr4xSub(s12, s13, s14, s15, smask, scy, stp, snp, srp, scnt)  # x86_64-mont5.pl:1049 jmp .Lsqr4x_sub_entry
  result = 1                   # x86_64-mont5.pl:570 mov \$1,%rax

template cmpRcxRsi() =
  ## `cmp $0,$j` of `.Lsqr4x_1st`/`.Lsqr4x_inner`: with `$j` replaced by
  ## the running pointer `%rcx = $aptr + $j`, `$j == 0` is `%rcx == $aptr`
  var ci {.register: "rcx".}: int
  var si {.register: "rsi".}: int
  cmpi(ci, si)
  var cp {.register: "rcx".}: W64
  var sp {.register: "rsi".}: W64

template lea2(d, x, s: untyped) =
  ## `lea ($s,$x,2),$d`: `d = 2 x + s`. There is no `lea`; the `add`s write
  ## the flags, so every use below sits where no flag is live (the
  ## reference's order is changed where it was not; each spot says so).
  d = x
  addq(d, d)
  addq(d, s)

template noTail(r8, r9, r10, r11, r12, r13, r14, r15, rtp2, lo, nptr, nptrS,
                carryS: untyped) =
  ## x86_64-mont5.pl:2018-2042, `.L8x_no_tail` and what follows it to the
  ## loop test. The reference reaches it by falling through from
  ## `.L8x_tail_done` (CF from `neg $carry`) or by `jae .L8x_no_tail` (CF
  ## from the `cmp`); here it is written out in both arms of the `if`
  ## (tail duplication), because arkham drops a CF that two arms set by
  ## different instructions (nimony-bugs/asm-if-merge-drops-carry.md).
  adcq(r8, rtp2[0])        # x86_64-mont5.pl:2019
  adcq(r9, rtp2[1])        # x86_64-mont5.pl:2020
  adcq(r10, rtp2[2])       # x86_64-mont5.pl:2021
  adcq(r11, rtp2[3])       # x86_64-mont5.pl:2022
  adcq(r12, rtp2[4])       # x86_64-mont5.pl:2023
  adcq(r13, rtp2[5])       # x86_64-mont5.pl:2024
  adcq(r14, rtp2[6])       # x86_64-mont5.pl:2025
  adcq(r15, rtp2[7])       # x86_64-mont5.pl:2026
  adcq(lo, 0)              # x86_64-mont5.pl:2027 top-most carry
  var nl {.register: "rcx".}: uint64
  nl = nptr[-1]            # x86_64-mont5.pl:2028 np[num-1]
  carryS = 0               # x86_64-mont5.pl:2029 xor $carry,$carry
  nptr = nptrS             # x86_64-mont5.pl:2031 restore $nptr
  rtp2[0] = r8             # x86_64-mont5.pl:2033 store top 512 bits
  rtp2[1] = r9             # x86_64-mont5.pl:2034
  # x86_64-mont5.pl:2035 movq %xmm3,$num: -$num is read from numNegS
  rtp2[2] = r10            # x86_64-mont5.pl:2036
  rtp2[3] = r11            # x86_64-mont5.pl:2037
  rtp2[4] = r12            # x86_64-mont5.pl:2038
  rtp2[5] = r13            # x86_64-mont5.pl:2039
  rtp2[6] = r14            # x86_64-mont5.pl:2040
  rtp2[7] = r15            # x86_64-mont5.pl:2041
  advRsi(64)               # x86_64-mont5.pl:2042

proc bnPower5Sqr5*(rp {.register: "rdi".}: W64; ap {.register: "rsi".}: W64;
                   table {.register: "rdx".}: W64; np {.register: "rcx".}: W64;
                   fr {.register: "r8".}: MontFramePtr;
                   num {.register: "r9".}: int) {.assembler.} =
  ## The five squarings of `bn_power5` (x86_64-mont5.pl:1113-1217):
  ## `rp = ap^(2^5) * 2^(-5*64 num) mod np`, almost reduced, for `num`
  ## divisible by 8, each squaring being `__bn_sqr8x_internal` (the
  ## squaring x86_64-mont5.pl:1329-1775 and `__bn_sqr8x_reduction`
  ## x86_64-mont5.pl:1787-2045) and `__bn_post4x_internal`
  ## (x86_64-mont5.pl:2059-2110). The five `call` pairs are one loop here;
  ## the closing `mul4x_internal` of x86_64-mont5.pl:1219-1225 is
  ## `bnMul4xMontGather5`, called next by the wrapper with the same
  ## arguments. `t` (`2 num` words, with a word before it) and `n0` come in
  ## `fr`. `$num` stays in bytes, as in the reference, and lives in stack
  ## slots (`numS`, `numNegS` for %xmm3); %xmm1/%xmm2/%xmm4 are `rptrS`,
  ## `nptrS`, `bptrS`.
  var numS {.stack.}: int        # $num, bytes
  var numNegS {.stack.}: int     # -$num, bytes (%xmm3)
  var n0S {.stack.}: uint64      # 32(%rsp)
  var rptrS {.stack.}: W64       # %xmm1
  var nptrS {.stack.}: W64       # %xmm2
  var bptrS {.stack.}: W64       # %xmm4
  var tEndS {.stack.}: W64       # 48+8(%rsp,$num,2), also 8+8(%rsp)
  var tBaseS {.stack.}: W64      # 48+8(%rsp)
  var tm1S {.stack.}: int        # where the walk over n0*a[i] stops
  var nEndS {.stack.}: int       # 0+8(%rsp), end of n[]
  var tptrS {.stack.}: W64       # $tptr while %rsi holds $carry
  var carryS {.stack.}: uint64   # $carry while %rsi holds $tptr
  var cnt5S {.stack.}: int       # the five calls
  var nn {.register: "r9".}: int
  var x {.register: "rax".}: uint64
  shli(nn, 3)                  # x86_64-mont5.pl:1139 convert $num to bytes
  # x86_64-mont5.pl:1140 lea ($num,$num,2),%r10d: for the frame placement
  # (x86_64-mont5.pl:1144-1185), which is the caller's `fr`
  numS = nn
  negi(nn)                     # x86_64-mont5.pl:1141
  x = fr.n0                    # x86_64-mont5.pl:1142 *n0
  n0S = x                      # x86_64-mont5.pl:1199
  var xp {.register: "rax".}: W64
  xp = fr.t2n
  tEndS = xp
  xp = fr.tp
  tBaseS = xp
  var xi {.register: "rax".}: int
  xi = fr.tm1
  tm1S = xi
  var r10 {.register: "r10".}: int
  r10 = nn                     # x86_64-mont5.pl:1187
  negi(nn)                     # x86_64-mont5.pl:1188
  rptrS = rp                   # x86_64-mont5.pl:1203 save $rptr
  nptrS = np                   # x86_64-mont5.pl:1204 save $nptr
  numNegS = r10                # x86_64-mont5.pl:1205 -$num
  bptrS = table                # x86_64-mont5.pl:1206
  cnt5S = 5                    # x86_64-mont5.pl:1208-1217 five call pairs
  while true:
    # ---- __bn_sqr8x_internal (x86_64-mont5.pl:1253) ----
    # register map: $i %rbp -> %r9 (bytes), $j %rcx -> %rcx = $aptr+$j and
    # %r8 = $tptr+$j (running pointers: ($aptr,$j) and ($tptr,$j) with a
    # displacement), $tptr %rdi, $aptr %rsi, @A0 %r10,%r11, @A1 %r12,%r13,
    # $a0 %r14, $a1 %r15, $ai %rbx; $num (%r9) is numS
    var i {.register: "r9".}: int
    var aptr {.register: "rsi".}: W64
    var tptr {.register: "rdi".}: W64
    var apj {.register: "rcx".}: W64
    var tpj {.register: "r8".}: W64
    var lo {.register: "rax".}: uint64
    var hi {.register: "rdx".}: uint64
    var a00 {.register: "r10".}: uint64
    var a01 {.register: "r11".}: uint64
    var a10 {.register: "r12".}: uint64
    var a11 {.register: "r13".}: uint64
    var a0 {.register: "r14".}: uint64
    var a1 {.register: "r15".}: uint64
    var ai {.register: "rbx".}: uint64
    var r10i {.register: "r10".}: int
    i = r10i                   # x86_64-mont5.pl:1329 lea 32(%r10),$i
    addi(i, 32)
    advRsi(numS)               # x86_64-mont5.pl:1330 end of a[] buffer
    var jj {.register: "rcx".}: int
    jj = numS                  # x86_64-mont5.pl:1332 $j=$num
    var apj0 {.register: "rcx".}: W64
    apj = aptr                 # ($aptr,$i): %rcx = $aptr+$i (as at :1362)
    advRcx(i)
    a0 = apj[-4]               # x86_64-mont5.pl:1335 a[0]
    tptr = tEndS               # x86_64-mont5.pl:1336 end of tp[] buffer
    lo = apj[-3]               # x86_64-mont5.pl:1337 a[1]
    advRdi(i)                  # x86_64-mont5.pl:1338 lea -32($tptr,$i),$tptr
    advRdi(-32)
    tpj = tptr                 # ($tptr,$i): %r8 = $tptr+$i (as at :1362)
    advR8(i)
    ai = apj[-2]               # x86_64-mont5.pl:1339 a[2]
    a1 = lo                    # x86_64-mont5.pl:1340
    mulq(lo, hi, a0)           # x86_64-mont5.pl:1342 a[1]*a[0]
    a00 = lo                   # x86_64-mont5.pl:1343
    lo = ai                    # x86_64-mont5.pl:1344 a[2]
    a01 = hi                   # x86_64-mont5.pl:1345
    tpj[-3] = a00              # x86_64-mont5.pl:1346 t[1]
    mulq(lo, hi, a0)           # x86_64-mont5.pl:1348 a[2]*a[0]
    addq(a01, lo)              # x86_64-mont5.pl:1349
    lo = ai                    # x86_64-mont5.pl:1350
    adcq(hi, 0)                # x86_64-mont5.pl:1351
    tpj[-2] = a01              # x86_64-mont5.pl:1352 t[2]
    a00 = hi                   # x86_64-mont5.pl:1353
    ai = apj[-1]               # x86_64-mont5.pl:1356 a[3]
    mulq(lo, hi, a1)           # x86_64-mont5.pl:1357 a[2]*a[1]
    a10 = lo                   # x86_64-mont5.pl:1358
    lo = ai                    # x86_64-mont5.pl:1359
    a11 = hi                   # x86_64-mont5.pl:1360
    # x86_64-mont5.pl:1362 lea ($i),$j: %rcx and %r8 already are $aptr+$i
    # and $tptr+$i
    mulq(lo, hi, a0)           # x86_64-mont5.pl:1363 a[3]*a[0]
    addq(a00, lo)              # x86_64-mont5.pl:1364
    lo = ai                    # x86_64-mont5.pl:1365
    a01 = hi                   # x86_64-mont5.pl:1366
    adcq(a01, 0)               # x86_64-mont5.pl:1367
    addq(a00, a10)             # x86_64-mont5.pl:1368
    adcq(a01, 0)               # x86_64-mont5.pl:1369
    tpj[-1] = a00              # x86_64-mont5.pl:1370 t[3]
    # x86_64-mont5.pl:1371 jmp .Lsqr4x_1st
    while true:
      # .Lsqr4x_1st
      ai = apj[0]              # x86_64-mont5.pl:1375 a[4]
      mulq(lo, hi, a1)         # x86_64-mont5.pl:1376 a[3]*a[1]
      addq(a11, lo)            # x86_64-mont5.pl:1377
      lo = ai                  # x86_64-mont5.pl:1378
      a10 = hi                 # x86_64-mont5.pl:1379
      adcq(a10, 0)             # x86_64-mont5.pl:1380
      mulq(lo, hi, a0)         # x86_64-mont5.pl:1382 a[4]*a[0]
      addq(a01, lo)            # x86_64-mont5.pl:1383
      lo = ai                  # x86_64-mont5.pl:1384
      ai = apj[1]              # x86_64-mont5.pl:1385 a[5]
      a00 = hi                 # x86_64-mont5.pl:1386
      adcq(a00, 0)             # x86_64-mont5.pl:1387
      addq(a01, a11)           # x86_64-mont5.pl:1388
      adcq(a00, 0)             # x86_64-mont5.pl:1389
      mulq(lo, hi, a1)         # x86_64-mont5.pl:1392 a[4]*a[3]
      addq(a10, lo)            # x86_64-mont5.pl:1393
      lo = ai                  # x86_64-mont5.pl:1394
      tpj[0] = a01             # x86_64-mont5.pl:1395 t[4]
      a11 = hi                 # x86_64-mont5.pl:1396
      adcq(a11, 0)             # x86_64-mont5.pl:1397
      mulq(lo, hi, a0)         # x86_64-mont5.pl:1399 a[5]*a[2]
      addq(a00, lo)            # x86_64-mont5.pl:1400
      lo = ai                  # x86_64-mont5.pl:1401
      ai = apj[2]              # x86_64-mont5.pl:1402 a[6]
      a01 = hi                 # x86_64-mont5.pl:1403
      adcq(a01, 0)             # x86_64-mont5.pl:1404
      addq(a00, a10)           # x86_64-mont5.pl:1405
      adcq(a01, 0)             # x86_64-mont5.pl:1406
      mulq(lo, hi, a1)         # x86_64-mont5.pl:1408 a[5]*a[3]
      addq(a11, lo)            # x86_64-mont5.pl:1409
      lo = ai                  # x86_64-mont5.pl:1410
      tpj[1] = a00             # x86_64-mont5.pl:1411 t[5]
      a10 = hi                 # x86_64-mont5.pl:1412
      adcq(a10, 0)             # x86_64-mont5.pl:1413
      mulq(lo, hi, a0)         # x86_64-mont5.pl:1415 a[6]*a[2]
      addq(a01, lo)            # x86_64-mont5.pl:1416
      lo = ai                  # x86_64-mont5.pl:1417
      ai = apj[3]              # x86_64-mont5.pl:1418 a[7]
      a00 = hi                 # x86_64-mont5.pl:1419
      adcq(a00, 0)             # x86_64-mont5.pl:1420
      addq(a01, a11)           # x86_64-mont5.pl:1421
      adcq(a00, 0)             # x86_64-mont5.pl:1422
      mulq(lo, hi, a1)         # x86_64-mont5.pl:1425 a[6]*a[5]
      addq(a10, lo)            # x86_64-mont5.pl:1426
      lo = ai                  # x86_64-mont5.pl:1427
      tpj[2] = a01             # x86_64-mont5.pl:1428 t[6]
      a11 = hi                 # x86_64-mont5.pl:1429
      adcq(a11, 0)             # x86_64-mont5.pl:1430
      advRcx(32)               # x86_64-mont5.pl:1431 lea 32($j),$j
      advR8(32)
      mulq(lo, hi, a0)         # x86_64-mont5.pl:1433 a[7]*a[4]
      addq(a00, lo)            # x86_64-mont5.pl:1434
      lo = ai                  # x86_64-mont5.pl:1435
      a01 = hi                 # x86_64-mont5.pl:1436
      adcq(a01, 0)             # x86_64-mont5.pl:1437
      addq(a00, a10)           # x86_64-mont5.pl:1438
      adcq(a01, 0)             # x86_64-mont5.pl:1439
      tpj[-1] = a00            # x86_64-mont5.pl:1440 t[7]
      cmpRcxRsi()              # x86_64-mont5.pl:1442 cmp \$0,$j
      if zf(): break           # x86_64-mont5.pl:1443 jne .Lsqr4x_1st
    mulq(lo, hi, a1)           # x86_64-mont5.pl:1445 a[7]*a[5]
    addq(a11, lo)              # x86_64-mont5.pl:1446
    adcq(hi, 0)                # x86_64-mont5.pl:1448
    addi(i, 16)                # x86_64-mont5.pl:1447 lea 16($i),$i, after
                               # the adc that reads the add's CF
    addq(a11, a01)             # x86_64-mont5.pl:1449
    adcq(hi, 0)                # x86_64-mont5.pl:1450
    tptr[0] = a11              # x86_64-mont5.pl:1452 t[8]
    a10 = hi                   # x86_64-mont5.pl:1453
    tptr[1] = hi               # x86_64-mont5.pl:1454 t[9]
    # x86_64-mont5.pl:1455 jmp .Lsqr4x_outer
    while true:
      # .Lsqr4x_outer
      apj = aptr               # ($aptr,$i): %rcx = $aptr+$i (as at :1503)
      advRcx(i)
      a0 = apj[-4]             # x86_64-mont5.pl:1459 a[0]
      tptr = tEndS             # x86_64-mont5.pl:1460 end of tp[] buffer
      lo = apj[-3]             # x86_64-mont5.pl:1461 a[1]
      advRdi(i)                # x86_64-mont5.pl:1462 lea -32($tptr,$i),$tptr
      advRdi(-32)
      tpj = tptr               # ($tptr,$i): %r8 = $tptr+$i (as at :1503)
      advR8(i)
      ai = apj[-2]             # x86_64-mont5.pl:1463 a[2]
      a1 = lo                  # x86_64-mont5.pl:1464
      mulq(lo, hi, a0)         # x86_64-mont5.pl:1466 a[1]*a[0]
      a00 = tpj[-3]            # x86_64-mont5.pl:1467 t[1]
      addq(a00, lo)            # x86_64-mont5.pl:1468 a[1]*a[0]+t[1]
      lo = ai                  # x86_64-mont5.pl:1469 a[2]
      adcq(hi, 0)              # x86_64-mont5.pl:1470
      tpj[-3] = a00            # x86_64-mont5.pl:1471 t[1]
      a01 = hi                 # x86_64-mont5.pl:1472
      mulq(lo, hi, a0)         # x86_64-mont5.pl:1474 a[2]*a[0]
      addq(a01, lo)            # x86_64-mont5.pl:1475
      lo = ai                  # x86_64-mont5.pl:1476
      adcq(hi, 0)              # x86_64-mont5.pl:1477
      addq(a01, tpj[-2])       # x86_64-mont5.pl:1478 a[2]*a[0]+t[2]
      a00 = hi                 # x86_64-mont5.pl:1479
      adcq(a00, 0)             # x86_64-mont5.pl:1480
      tpj[-2] = a01            # x86_64-mont5.pl:1481 t[2]
      xorq(a10, a10)           # x86_64-mont5.pl:1483
      ai = apj[-1]             # x86_64-mont5.pl:1485 a[3]
      mulq(lo, hi, a1)         # x86_64-mont5.pl:1486 a[2]*a[1]
      addq(a10, lo)            # x86_64-mont5.pl:1487 a[2]*a[1]+t[3]
      lo = ai                  # x86_64-mont5.pl:1488
      adcq(hi, 0)              # x86_64-mont5.pl:1489
      addq(a10, tpj[-1])       # x86_64-mont5.pl:1490
      a11 = hi                 # x86_64-mont5.pl:1491
      adcq(a11, 0)             # x86_64-mont5.pl:1492
      mulq(lo, hi, a0)         # x86_64-mont5.pl:1494 a[3]*a[0]
      addq(a00, lo)            # x86_64-mont5.pl:1495 a[3]*a[0]+a[2]*a[1]+t[3]
      lo = ai                  # x86_64-mont5.pl:1496
      adcq(hi, 0)              # x86_64-mont5.pl:1497
      addq(a00, a10)           # x86_64-mont5.pl:1498
      a01 = hi                 # x86_64-mont5.pl:1499
      adcq(a01, 0)             # x86_64-mont5.pl:1500
      tpj[-1] = a00            # x86_64-mont5.pl:1501 t[3]
      # x86_64-mont5.pl:1503 lea ($i),$j: %rcx/%r8 already set above
      # x86_64-mont5.pl:1504 jmp .Lsqr4x_inner
      while true:
        # .Lsqr4x_inner
        ai = apj[0]            # x86_64-mont5.pl:1508 a[4]
        mulq(lo, hi, a1)       # x86_64-mont5.pl:1509 a[3]*a[1]
        addq(a11, lo)          # x86_64-mont5.pl:1510 a[3]*a[1]+t[4]
        lo = ai                # x86_64-mont5.pl:1511
        a10 = hi               # x86_64-mont5.pl:1512
        adcq(a10, 0)           # x86_64-mont5.pl:1513
        addq(a11, tpj[0])      # x86_64-mont5.pl:1514
        adcq(a10, 0)           # x86_64-mont5.pl:1515
        mulq(lo, hi, a0)       # x86_64-mont5.pl:1518 a[4]*a[0]
        addq(a01, lo)          # x86_64-mont5.pl:1519 a[4]*a[0]+a[3]*a[1]+t[4]
        lo = ai                # x86_64-mont5.pl:1520 a[3]
        ai = apj[1]            # x86_64-mont5.pl:1521 a[5]
        a00 = hi               # x86_64-mont5.pl:1522
        adcq(a00, 0)           # x86_64-mont5.pl:1523
        addq(a01, a11)         # x86_64-mont5.pl:1524
        adcq(a00, 0)           # x86_64-mont5.pl:1525
        mulq(lo, hi, a1)       # x86_64-mont5.pl:1527 a[4]*a[3]
        addq(a10, lo)          # x86_64-mont5.pl:1528 a[4]*a[3]+t[5]
        tpj[0] = a01           # x86_64-mont5.pl:1529 t[4]
        lo = ai                # x86_64-mont5.pl:1530
        a11 = hi               # x86_64-mont5.pl:1531
        adcq(a11, 0)           # x86_64-mont5.pl:1532
        addq(a10, tpj[1])      # x86_64-mont5.pl:1533
        adcq(a11, 0)           # x86_64-mont5.pl:1535
        advRcx(16)             # x86_64-mont5.pl:1534 lea 16($j),$j, after
        advR8(16)              # the adc that reads the add's CF
        mulq(lo, hi, a0)       # x86_64-mont5.pl:1537 a[5]*a[2]
        addq(a00, lo)          # x86_64-mont5.pl:1538 a[5]*a[2]+a[4]*a[3]+t[5]
        lo = ai                # x86_64-mont5.pl:1539
        adcq(hi, 0)            # x86_64-mont5.pl:1540
        addq(a00, a10)         # x86_64-mont5.pl:1541
        a01 = hi               # x86_64-mont5.pl:1542
        adcq(a01, 0)           # x86_64-mont5.pl:1543
        tpj[-1] = a00          # x86_64-mont5.pl:1544 t[5], "preloaded t[1]" below
        cmpRcxRsi()            # x86_64-mont5.pl:1546 cmp \$0,$j
        if zf(): break         # x86_64-mont5.pl:1547 jne .Lsqr4x_inner
      mulq(lo, hi, a1)         # x86_64-mont5.pl:1550 a[5]*a[3]
      addq(a11, lo)            # x86_64-mont5.pl:1551
      adcq(hi, 0)              # x86_64-mont5.pl:1552
      addq(a11, a01)           # x86_64-mont5.pl:1553
      adcq(hi, 0)              # x86_64-mont5.pl:1554
      tptr[0] = a11            # x86_64-mont5.pl:1556 t[6], "preloaded t[2]" below
      a10 = hi                 # x86_64-mont5.pl:1557
      tptr[1] = hi             # x86_64-mont5.pl:1558 t[7], "preloaded t[3]" below
      addi(i, 16)              # x86_64-mont5.pl:1560
      if zf(): break           # x86_64-mont5.pl:1561 jnz .Lsqr4x_outer
    # comments apply to $num==4 case
    a0 = aptr[-4]              # x86_64-mont5.pl:1564 a[0]
    tptr = tEndS               # x86_64-mont5.pl:1565 end of tp[] buffer
    lo = aptr[-3]              # x86_64-mont5.pl:1566 a[1]
    advRdi(i)                  # x86_64-mont5.pl:1567 lea -32($tptr,$i),$tptr
    advRdi(-32)
    ai = aptr[-2]              # x86_64-mont5.pl:1568 a[2]
    a1 = lo                    # x86_64-mont5.pl:1569
    mulq(lo, hi, a0)           # x86_64-mont5.pl:1571 a[1]*a[0]
    addq(a00, lo)              # x86_64-mont5.pl:1572 a[1]*a[0]+t[1], preloaded t[1]
    lo = ai                    # x86_64-mont5.pl:1573 a[2]
    a01 = hi                   # x86_64-mont5.pl:1574
    adcq(a01, 0)               # x86_64-mont5.pl:1575
    mulq(lo, hi, a0)           # x86_64-mont5.pl:1577 a[2]*a[0]
    addq(a01, lo)              # x86_64-mont5.pl:1578
    lo = ai                    # x86_64-mont5.pl:1579
    tptr[-3] = a00             # x86_64-mont5.pl:1580 t[1]
    a00 = hi                   # x86_64-mont5.pl:1581
    adcq(a00, 0)               # x86_64-mont5.pl:1582
    addq(a01, a11)             # x86_64-mont5.pl:1583 a[2]*a[0]+t[2], preloaded t[2]
    ai = aptr[-1]              # x86_64-mont5.pl:1584 a[3]
    adcq(a00, 0)               # x86_64-mont5.pl:1585
    mulq(lo, hi, a1)           # x86_64-mont5.pl:1587 a[2]*a[1]
    addq(a10, lo)              # x86_64-mont5.pl:1588 a[2]*a[1]+t[3], preloaded t[3]
    lo = ai                    # x86_64-mont5.pl:1589
    tptr[-2] = a01             # x86_64-mont5.pl:1590 t[2]
    a11 = hi                   # x86_64-mont5.pl:1591
    adcq(a11, 0)               # x86_64-mont5.pl:1592
    mulq(lo, hi, a0)           # x86_64-mont5.pl:1594 a[3]*a[0]
    addq(a00, lo)              # x86_64-mont5.pl:1595 a[3]*a[0]+a[2]*a[1]+t[3]
    lo = ai                    # x86_64-mont5.pl:1596
    a01 = hi                   # x86_64-mont5.pl:1597
    adcq(a01, 0)               # x86_64-mont5.pl:1598
    addq(a00, a10)             # x86_64-mont5.pl:1599
    adcq(a01, 0)               # x86_64-mont5.pl:1600
    tptr[-1] = a00             # x86_64-mont5.pl:1601 t[3]
    mulq(lo, hi, a1)           # x86_64-mont5.pl:1603 a[3]*a[1]
    addq(a11, lo)              # x86_64-mont5.pl:1604
    lo = aptr[-2]              # x86_64-mont5.pl:1605 a[2]
    adcq(hi, 0)                # x86_64-mont5.pl:1606
    addq(a11, a01)             # x86_64-mont5.pl:1607
    adcq(hi, 0)                # x86_64-mont5.pl:1608
    tptr[0] = a11              # x86_64-mont5.pl:1610 t[4]
    a10 = hi                   # x86_64-mont5.pl:1611
    tptr[1] = hi               # x86_64-mont5.pl:1612 t[5]
    mulq(lo, hi, ai)           # x86_64-mont5.pl:1614 a[2]*a[3]
    # shift-n-add: $shift %r14 ($a0), $carry %r15 ($a1), @S %r12,%r13,%rbx,%r8
    var shift {.register: "r14".}: uint64
    var carry {.register: "r15".}: uint64
    var s0 {.register: "r12".}: uint64
    var s1 {.register: "r13".}: uint64
    var s2 {.register: "rbx".}: uint64
    var s3 {.register: "r8".}: uint64
    addi(i, 16)                # x86_64-mont5.pl:1620
    xorq(shift, shift)         # x86_64-mont5.pl:1621
    subi(i, numS)              # x86_64-mont5.pl:1622 $i=16-$num
    xorq(carry, carry)         # x86_64-mont5.pl:1623
    addq(lo, a10)              # x86_64-mont5.pl:1625 t[5]
    adcq(hi, 0)                # x86_64-mont5.pl:1626
    tptr[1] = lo               # x86_64-mont5.pl:1627 t[5]
    tptr[2] = hi               # x86_64-mont5.pl:1628 t[6]
    tptr[3] = carry            # x86_64-mont5.pl:1629 t[7]
    apj = aptr                 # ($aptr,$i): %rcx = $aptr+$i, stepped with $i
    advRcx(i)
    lo = apj[-2]               # x86_64-mont5.pl:1631 a[0]
    tptr = tBaseS              # x86_64-mont5.pl:1632 lea 48+8(%rsp),$tptr
    xorq(a00, a00)             # x86_64-mont5.pl:1633 t[0]
    a01 = tptr[1]              # x86_64-mont5.pl:1634 t[1]
    lea2(s0, a00, shift)       # x86_64-mont5.pl:1636 t[2*i]<<1 | shift
    shrq(a00, 63)              # x86_64-mont5.pl:1637
    s1 = a01                   # x86_64-mont5.pl:1638 lea ($j,$A0[1],2),$S[1]:
    addq(s1, s1)               # $j is 0 here, so S[1] = 2*A0[1]
    shrq(a01, 63)              # x86_64-mont5.pl:1639
    orq(s1, a00)               # x86_64-mont5.pl:1640 | t[2*i]>>63
    a00 = tptr[2]              # x86_64-mont5.pl:1641 t[2*i+2] # prefetch
    shift = a01                # x86_64-mont5.pl:1642 shift=t[2*i+1]>>63
    mulq(lo, hi, lo)           # x86_64-mont5.pl:1643 a[i]*a[i]
    negq(carry)                # x86_64-mont5.pl:1644 mov $carry,cf
    a01 = tptr[3]              # x86_64-mont5.pl:1645 t[2*i+2+1] # prefetch
    adcq(s0, lo)               # x86_64-mont5.pl:1646
    lo = apj[-1]               # x86_64-mont5.pl:1647 a[i+1] # prefetch
    tptr[0] = s0               # x86_64-mont5.pl:1648
    adcq(s1, hi)               # x86_64-mont5.pl:1649
    tptr[1] = s1               # x86_64-mont5.pl:1652
    sbbq(carry, carry)         # x86_64-mont5.pl:1653 mov cf,$carry
    lea2(s2, a00, shift)       # x86_64-mont5.pl:1651, after the sbb (CF)
    shrq(a00, 63)              # x86_64-mont5.pl:1654
    s3 = a01                   # x86_64-mont5.pl:1655 lea ($j,$A0[1],2),$S[3]
    addq(s3, s3)
    shrq(a01, 63)              # x86_64-mont5.pl:1656
    orq(s3, a00)               # x86_64-mont5.pl:1657 | t[2*i]>>63
    a00 = tptr[4]              # x86_64-mont5.pl:1658 t[2*i+2] # prefetch
    shift = a01                # x86_64-mont5.pl:1659 shift=t[2*i+1]>>63
    mulq(lo, hi, lo)           # x86_64-mont5.pl:1660 a[i]*a[i]
    negq(carry)                # x86_64-mont5.pl:1661 mov $carry,cf
    a01 = tptr[5]              # x86_64-mont5.pl:1662 t[2*i+2+1] # prefetch
    adcq(s2, lo)               # x86_64-mont5.pl:1663
    lo = apj[0]                # x86_64-mont5.pl:1664 a[i+1] # prefetch
    tptr[2] = s2               # x86_64-mont5.pl:1665
    adcq(s3, hi)               # x86_64-mont5.pl:1666
    tptr[3] = s3               # x86_64-mont5.pl:1668
    sbbq(carry, carry)         # x86_64-mont5.pl:1669 mov cf,$carry
    addi(i, 16)                # x86_64-mont5.pl:1667 lea 16($i),$i, after the sbb
    advRcx(16)
    advRdi(64)                 # x86_64-mont5.pl:1670 lea 64($tptr),$tptr
    # x86_64-mont5.pl:1671 jmp .Lsqr4x_shift_n_add
    while true:
      # .Lsqr4x_shift_n_add
      lea2(s0, a00, shift)     # x86_64-mont5.pl:1675 t[2*i]<<1 | shift
      shrq(a00, 63)            # x86_64-mont5.pl:1676
      s1 = a01                 # x86_64-mont5.pl:1677 lea ($j,$A0[1],2),$S[1]
      addq(s1, s1)
      shrq(a01, 63)            # x86_64-mont5.pl:1678
      orq(s1, a00)             # x86_64-mont5.pl:1679 | t[2*i]>>63
      a00 = tptr[-2]           # x86_64-mont5.pl:1680 t[2*i+2] # prefetch
      shift = a01              # x86_64-mont5.pl:1681 shift=t[2*i+1]>>63
      mulq(lo, hi, lo)         # x86_64-mont5.pl:1682 a[i]*a[i]
      negq(carry)              # x86_64-mont5.pl:1683 mov $carry,cf
      a01 = tptr[-1]           # x86_64-mont5.pl:1684 t[2*i+2+1] # prefetch
      adcq(s0, lo)             # x86_64-mont5.pl:1685
      lo = apj[-1]             # x86_64-mont5.pl:1686 a[i+1] # prefetch
      tptr[-4] = s0            # x86_64-mont5.pl:1687
      adcq(s1, hi)             # x86_64-mont5.pl:1688
      tptr[-3] = s1            # x86_64-mont5.pl:1691
      sbbq(carry, carry)       # x86_64-mont5.pl:1692 mov cf,$carry
      lea2(s2, a00, shift)     # x86_64-mont5.pl:1690, after the sbb (CF)
      shrq(a00, 63)            # x86_64-mont5.pl:1693
      s3 = a01                 # x86_64-mont5.pl:1694 lea ($j,$A0[1],2),$S[3]
      addq(s3, s3)
      shrq(a01, 63)            # x86_64-mont5.pl:1695
      orq(s3, a00)             # x86_64-mont5.pl:1696 | t[2*i]>>63
      a00 = tptr[0]            # x86_64-mont5.pl:1697 t[2*i+2] # prefetch
      shift = a01              # x86_64-mont5.pl:1698 shift=t[2*i+1]>>63
      mulq(lo, hi, lo)         # x86_64-mont5.pl:1699 a[i]*a[i]
      negq(carry)              # x86_64-mont5.pl:1700 mov $carry,cf
      a01 = tptr[1]            # x86_64-mont5.pl:1701 t[2*i+2+1] # prefetch
      adcq(s2, lo)             # x86_64-mont5.pl:1702
      lo = apj[0]              # x86_64-mont5.pl:1703 a[i+1] # prefetch
      tptr[-2] = s2            # x86_64-mont5.pl:1704
      adcq(s3, hi)             # x86_64-mont5.pl:1705
      tptr[-1] = s3            # x86_64-mont5.pl:1708
      sbbq(carry, carry)       # x86_64-mont5.pl:1709 mov cf,$carry
      lea2(s0, a00, shift)     # x86_64-mont5.pl:1707, after the sbb (CF)
      shrq(a00, 63)            # x86_64-mont5.pl:1710
      s1 = a01                 # x86_64-mont5.pl:1711 lea ($j,$A0[1],2),$S[1]
      addq(s1, s1)
      shrq(a01, 63)            # x86_64-mont5.pl:1712
      orq(s1, a00)             # x86_64-mont5.pl:1713 | t[2*i]>>63
      a00 = tptr[2]            # x86_64-mont5.pl:1714 t[2*i+2] # prefetch
      shift = a01              # x86_64-mont5.pl:1715 shift=t[2*i+1]>>63
      mulq(lo, hi, lo)         # x86_64-mont5.pl:1716 a[i]*a[i]
      negq(carry)              # x86_64-mont5.pl:1717 mov $carry,cf
      a01 = tptr[3]            # x86_64-mont5.pl:1718 t[2*i+2+1] # prefetch
      adcq(s0, lo)             # x86_64-mont5.pl:1719
      lo = apj[1]              # x86_64-mont5.pl:1720 a[i+1] # prefetch
      tptr[0] = s0             # x86_64-mont5.pl:1721
      adcq(s1, hi)             # x86_64-mont5.pl:1722
      tptr[1] = s1             # x86_64-mont5.pl:1725
      sbbq(carry, carry)       # x86_64-mont5.pl:1726 mov cf,$carry
      lea2(s2, a00, shift)     # x86_64-mont5.pl:1724, after the sbb (CF)
      shrq(a00, 63)            # x86_64-mont5.pl:1727
      s3 = a01                 # x86_64-mont5.pl:1728 lea ($j,$A0[1],2),$S[3]
      addq(s3, s3)
      shrq(a01, 63)            # x86_64-mont5.pl:1729
      orq(s3, a00)             # x86_64-mont5.pl:1730 | t[2*i]>>63
      a00 = tptr[4]            # x86_64-mont5.pl:1731 t[2*i+2] # prefetch
      shift = a01              # x86_64-mont5.pl:1732 shift=t[2*i+1]>>63
      mulq(lo, hi, lo)         # x86_64-mont5.pl:1733 a[i]*a[i]
      negq(carry)              # x86_64-mont5.pl:1734 mov $carry,cf
      a01 = tptr[5]            # x86_64-mont5.pl:1735 t[2*i+2+1] # prefetch
      adcq(s2, lo)             # x86_64-mont5.pl:1736
      lo = apj[2]              # x86_64-mont5.pl:1737 a[i+1] # prefetch
      tptr[2] = s2             # x86_64-mont5.pl:1738
      adcq(s3, hi)             # x86_64-mont5.pl:1739
      tptr[3] = s3             # x86_64-mont5.pl:1740
      sbbq(carry, carry)       # x86_64-mont5.pl:1741 mov cf,$carry
      advRdi(64)               # x86_64-mont5.pl:1742 lea 64($tptr),$tptr
      advRcx(32)               # (%rcx = $aptr+$i follows $i)
      addi(i, 32)              # x86_64-mont5.pl:1743
      if zf(): break           # x86_64-mont5.pl:1744 jnz .Lsqr4x_shift_n_add
    lea2(s0, a00, shift)       # x86_64-mont5.pl:1746 t[2*i]<<1 | shift
    shrq(a00, 63)              # x86_64-mont5.pl:1748
    s1 = a01                   # x86_64-mont5.pl:1749 lea ($j,$A0[1],2),$S[1]
    addq(s1, s1)
    shrq(a01, 63)              # x86_64-mont5.pl:1750
    orq(s1, a00)               # x86_64-mont5.pl:1751 | t[2*i]>>63
    a00 = tptr[-2]             # x86_64-mont5.pl:1752 t[2*i+2] # prefetch
    shift = a01                # x86_64-mont5.pl:1753 shift=t[2*i+1]>>63
    mulq(lo, hi, lo)           # x86_64-mont5.pl:1754 a[i]*a[i]
    negq(carry)                # x86_64-mont5.pl:1755 mov $carry,cf
    a01 = tptr[-1]             # x86_64-mont5.pl:1756 t[2*i+2+1] # prefetch
    adcq(s0, lo)               # x86_64-mont5.pl:1757
    lo = aptr[-1]              # x86_64-mont5.pl:1758 a[i+1] # prefetch
    tptr[-4] = s0              # x86_64-mont5.pl:1759
    adcq(s1, hi)               # x86_64-mont5.pl:1760
    tptr[-3] = s1              # x86_64-mont5.pl:1763
    sbbq(carry, carry)         # x86_64-mont5.pl:1764 mov cf,$carry
    lea2(s2, a00, shift)       # x86_64-mont5.pl:1762, after the sbb (CF)
    shrq(a00, 63)              # x86_64-mont5.pl:1765
    s3 = a01                   # x86_64-mont5.pl:1766 lea ($j,$A0[1],2),$S[3]
    addq(s3, s3)
    shrq(a01, 63)              # x86_64-mont5.pl:1767
    orq(s3, a00)               # x86_64-mont5.pl:1768 | t[2*i]>>63
    mulq(lo, hi, lo)           # x86_64-mont5.pl:1769 a[i]*a[i]
    negq(carry)                # x86_64-mont5.pl:1770 mov $carry,cf
    adcq(s2, lo)               # x86_64-mont5.pl:1771
    adcq(s3, hi)               # x86_64-mont5.pl:1772
    tptr[-2] = s2              # x86_64-mont5.pl:1773
    tptr[-1] = s3              # x86_64-mont5.pl:1774
    # ---- __bn_sqr8x_reduction (x86_64-mont5.pl:1787-2045) ----
    # register map: $nptr %rbp -> %rdi, $m0 %rbx, %rcx (the counter 8..1,
    # also the index of the saved n0*a[i] words at 48-8+8(%rsp,%rcx,8),
    # i.e. t[%rcx-1]) -> a pointer to that word, stepped by -8, the loop
    # ending when it reaches t[-1] (tm1S); $tptr %rdi and $carry %rsi share
    # %rsi: $tptr is in tptrS while .L8x_reduce borrows $carry, and $carry
    # is in carryS while %rsi is $tptr (the tail), handled in a free
    # register (%rax, %rdx or %rcx) where the reference touches it
    var nptr {.register: "rdi".}: W64
    var rtp {.register: "rsi".}: W64
    var m0 {.register: "rbx".}: uint64
    var spw {.register: "rcx".}: W64
    var r8 {.register: "r8".}: uint64
    var r9 {.register: "r9".}: uint64
    var r10 {.register: "r10".}: uint64
    var r11 {.register: "r11".}: uint64
    var r12 {.register: "r12".}: uint64
    var r13 {.register: "r13".}: uint64
    var r14 {.register: "r14".}: uint64
    var r15 {.register: "r15".}: uint64
    nptr = nptrS               # x86_64-mont5.pl:1787 movq %xmm2,$nptr
    xorq(lo, lo)               # x86_64-mont5.pl:1789
    spw = nptr                 # x86_64-mont5.pl:1790 lea ($nptr,$num),%rcx
    advRcx(numS)
    var nei {.register: "rcx".}: int
    nEndS = nei                # x86_64-mont5.pl:1792 mov %rcx,0+8(%rsp)
    var tend {.register: "rdx".}: W64
    tend = tEndS               # x86_64-mont5.pl:1791 end of t[] buffer
    # x86_64-mont5.pl:1794 mov %rdx,8+8(%rsp): that is tEndS
    rtp = tBaseS               # x86_64-mont5.pl:1793 lea 48+8(%rsp,$num),$tptr
    advRsi(numS)
    # x86_64-mont5.pl:1795 neg $num: -$num is numNegS
    # x86_64-mont5.pl:1796 jmp .L8x_reduction_loop
    while true:
      # .L8x_reduction_loop
      advRsi(numNegS)          # x86_64-mont5.pl:1800 start of current t[] window
      m0 = rtp[0]              # x86_64-mont5.pl:1802
      r9 = rtp[1]              # x86_64-mont5.pl:1803
      r10 = rtp[2]             # x86_64-mont5.pl:1804
      r11 = rtp[3]             # x86_64-mont5.pl:1805
      r12 = rtp[4]             # x86_64-mont5.pl:1806
      r13 = rtp[5]             # x86_64-mont5.pl:1807
      r14 = rtp[6]             # x86_64-mont5.pl:1808
      r15 = rtp[7]             # x86_64-mont5.pl:1809
      tend[0] = lo             # x86_64-mont5.pl:1810 store top-most carry bit
      advRsi(64)               # x86_64-mont5.pl:1811
      r8 = m0                  # x86_64-mont5.pl:1814
      imulq(m0, n0S)           # x86_64-mont5.pl:1815 n0*a[0]
      lo = nptr[0]             # x86_64-mont5.pl:1816 n[0]
      var spw1 {.register: "rcx".}: W64
      spw = tBaseS             # x86_64-mont5.pl:1817 mov \$8,%ecx: t[8-1]
      advRcx(56)
      tptrS = rtp              # $tptr aside: .L8x_reduce borrows $carry
      var cy {.register: "rsi".}: uint64
      var hr {.register: "rdx".}: uint64
      # x86_64-mont5.pl:1818 jmp .L8x_reduce
      while true:
        # .L8x_reduce
        mulq(lo, hi, m0)       # x86_64-mont5.pl:1822
        lo = nptr[1]           # x86_64-mont5.pl:1823 n[1]
        negq(r8)               # x86_64-mont5.pl:1824
        r8 = hi                # x86_64-mont5.pl:1825
        adcq(r8, 0)            # x86_64-mont5.pl:1826
        mulq(lo, hi, m0)       # x86_64-mont5.pl:1828
        addq(r9, lo)           # x86_64-mont5.pl:1829
        lo = nptr[2]           # x86_64-mont5.pl:1830
        adcq(hi, 0)            # x86_64-mont5.pl:1831
        addq(r8, r9)           # x86_64-mont5.pl:1832
        spw[0] = m0            # x86_64-mont5.pl:1833 put aside n0*a[i]
        r9 = hi                # x86_64-mont5.pl:1834
        adcq(r9, 0)            # x86_64-mont5.pl:1835
        mulq(lo, hi, m0)       # x86_64-mont5.pl:1837
        addq(r10, lo)          # x86_64-mont5.pl:1838
        lo = nptr[3]           # x86_64-mont5.pl:1839
        adcq(hi, 0)            # x86_64-mont5.pl:1840
        addq(r9, r10)          # x86_64-mont5.pl:1841
        cy = n0S               # x86_64-mont5.pl:1842 pull n0, borrow $carry
        r10 = hi               # x86_64-mont5.pl:1843
        adcq(r10, 0)           # x86_64-mont5.pl:1844
        mulq(lo, hi, m0)       # x86_64-mont5.pl:1846
        addq(r11, lo)          # x86_64-mont5.pl:1847
        lo = nptr[4]           # x86_64-mont5.pl:1848
        adcq(hi, 0)            # x86_64-mont5.pl:1849
        imulq(cy, r8)          # x86_64-mont5.pl:1850 modulo-scheduled
        addq(r10, r11)         # x86_64-mont5.pl:1851
        r11 = hi               # x86_64-mont5.pl:1852
        adcq(r11, 0)           # x86_64-mont5.pl:1853
        mulq(lo, hi, m0)       # x86_64-mont5.pl:1855
        addq(r12, lo)          # x86_64-mont5.pl:1856
        lo = nptr[5]           # x86_64-mont5.pl:1857
        adcq(hi, 0)            # x86_64-mont5.pl:1858
        addq(r11, r12)         # x86_64-mont5.pl:1859
        r12 = hi               # x86_64-mont5.pl:1860
        adcq(r12, 0)           # x86_64-mont5.pl:1861
        mulq(lo, hi, m0)       # x86_64-mont5.pl:1863
        addq(r13, lo)          # x86_64-mont5.pl:1864
        lo = nptr[6]           # x86_64-mont5.pl:1865
        adcq(hi, 0)            # x86_64-mont5.pl:1866
        addq(r12, r13)         # x86_64-mont5.pl:1867
        r13 = hi               # x86_64-mont5.pl:1868
        adcq(r13, 0)           # x86_64-mont5.pl:1869
        mulq(lo, hi, m0)       # x86_64-mont5.pl:1871
        addq(r14, lo)          # x86_64-mont5.pl:1872
        lo = nptr[7]           # x86_64-mont5.pl:1873
        adcq(hi, 0)            # x86_64-mont5.pl:1874
        addq(r13, r14)         # x86_64-mont5.pl:1875
        r14 = hi               # x86_64-mont5.pl:1876
        adcq(r14, 0)           # x86_64-mont5.pl:1877
        mulq(lo, hi, m0)       # x86_64-mont5.pl:1879
        m0 = cy                # x86_64-mont5.pl:1880 n0*a[i]
        addq(r15, lo)          # x86_64-mont5.pl:1881
        lo = nptr[0]           # x86_64-mont5.pl:1882 n[0]
        adcq(hi, 0)            # x86_64-mont5.pl:1883
        addq(r14, r15)         # x86_64-mont5.pl:1884
        r15 = hi               # x86_64-mont5.pl:1885
        adcq(r15, 0)           # x86_64-mont5.pl:1886
        advRcx(-8)             # x86_64-mont5.pl:1888 dec %ecx
        var ce {.register: "rcx".}: int
        cmpi(ce, tm1S)
        var cp {.register: "rcx".}: W64
        if zf(): break         # x86_64-mont5.pl:1889 jnz .L8x_reduce
      advRdi(64)               # x86_64-mont5.pl:1891 lea 8*8($nptr),$nptr
      var rtp2 {.register: "rsi".}: W64
      rtp2 = tptrS             # $tptr back in %rsi
      xorq(lo, lo)             # x86_64-mont5.pl:1892
      var tendB {.register: "rdx".}: W64
      tend = tEndS             # x86_64-mont5.pl:1893 pull end of t[]
      var npi {.register: "rdi".}: int
      cmpi(npi, nEndS)         # x86_64-mont5.pl:1894 end of n[]?
      var npp {.register: "rdi".}: W64
      if cf():                 # x86_64-mont5.pl:1895 jae .L8x_no_tail
        addq(r8, rtp2[0])      # x86_64-mont5.pl:1898
        adcq(r9, rtp2[1])      # x86_64-mont5.pl:1899
        adcq(r10, rtp2[2])     # x86_64-mont5.pl:1900
        adcq(r11, rtp2[3])     # x86_64-mont5.pl:1901
        adcq(r12, rtp2[4])     # x86_64-mont5.pl:1902
        adcq(r13, rtp2[5])     # x86_64-mont5.pl:1903
        adcq(r14, rtp2[6])     # x86_64-mont5.pl:1904
        adcq(r15, rtp2[7])     # x86_64-mont5.pl:1905
        sbbq(lo, lo)           # x86_64-mont5.pl:1906 top carry ($carry in %rax,
        carryS = lo            # then aside: %rsi is $tptr)
        spw = tBaseS           # x86_64-mont5.pl:1908 pull n0*a[0] (t[7])
        m0 = spw[7]
        advRcx(56)             # x86_64-mont5.pl:1909 mov \$8,%ecx: t[8-1]
        lo = nptr[0]           # x86_64-mont5.pl:1910
        var hr2 {.register: "rdx".}: uint64
        # x86_64-mont5.pl:1911 jmp .L8x_tail
        while true:
          while true:
            # .L8x_tail
            mulq(lo, hi, m0)   # x86_64-mont5.pl:1915
            addq(r8, lo)       # x86_64-mont5.pl:1916
            lo = nptr[1]       # x86_64-mont5.pl:1917
            rtp2[0] = r8       # x86_64-mont5.pl:1918 save result
            r8 = hi            # x86_64-mont5.pl:1919
            adcq(r8, 0)        # x86_64-mont5.pl:1920
            mulq(lo, hi, m0)   # x86_64-mont5.pl:1922
            addq(r9, lo)       # x86_64-mont5.pl:1923
            lo = nptr[2]       # x86_64-mont5.pl:1924
            adcq(hi, 0)        # x86_64-mont5.pl:1925
            addq(r8, r9)       # x86_64-mont5.pl:1926
            r9 = hi            # x86_64-mont5.pl:1928
            adcq(r9, 0)        # x86_64-mont5.pl:1929
            advRsi(8)          # x86_64-mont5.pl:1927 $tptr++, after the adc
            mulq(lo, hi, m0)   # x86_64-mont5.pl:1931
            addq(r10, lo)      # x86_64-mont5.pl:1932
            lo = nptr[3]       # x86_64-mont5.pl:1933
            adcq(hi, 0)        # x86_64-mont5.pl:1934
            addq(r9, r10)      # x86_64-mont5.pl:1935
            r10 = hi           # x86_64-mont5.pl:1936
            adcq(r10, 0)       # x86_64-mont5.pl:1937
            mulq(lo, hi, m0)   # x86_64-mont5.pl:1939
            addq(r11, lo)      # x86_64-mont5.pl:1940
            lo = nptr[4]       # x86_64-mont5.pl:1941
            adcq(hi, 0)        # x86_64-mont5.pl:1942
            addq(r10, r11)     # x86_64-mont5.pl:1943
            r11 = hi           # x86_64-mont5.pl:1944
            adcq(r11, 0)       # x86_64-mont5.pl:1945
            mulq(lo, hi, m0)   # x86_64-mont5.pl:1947
            addq(r12, lo)      # x86_64-mont5.pl:1948
            lo = nptr[5]       # x86_64-mont5.pl:1949
            adcq(hi, 0)        # x86_64-mont5.pl:1950
            addq(r11, r12)     # x86_64-mont5.pl:1951
            r12 = hi           # x86_64-mont5.pl:1952
            adcq(r12, 0)       # x86_64-mont5.pl:1953
            mulq(lo, hi, m0)   # x86_64-mont5.pl:1955
            addq(r13, lo)      # x86_64-mont5.pl:1956
            lo = nptr[6]       # x86_64-mont5.pl:1957
            adcq(hi, 0)        # x86_64-mont5.pl:1958
            addq(r12, r13)     # x86_64-mont5.pl:1959
            r13 = hi           # x86_64-mont5.pl:1960
            adcq(r13, 0)       # x86_64-mont5.pl:1961
            mulq(lo, hi, m0)   # x86_64-mont5.pl:1963
            addq(r14, lo)      # x86_64-mont5.pl:1964
            lo = nptr[7]       # x86_64-mont5.pl:1965
            adcq(hi, 0)        # x86_64-mont5.pl:1966
            addq(r13, r14)     # x86_64-mont5.pl:1967
            r14 = hi           # x86_64-mont5.pl:1968
            adcq(r14, 0)       # x86_64-mont5.pl:1969
            mulq(lo, hi, m0)   # x86_64-mont5.pl:1971
            m0 = spw[-1]       # x86_64-mont5.pl:1972 pull n0*a[i]
            addq(r15, lo)      # x86_64-mont5.pl:1973
            adcq(hi, 0)        # x86_64-mont5.pl:1974
            addq(r14, r15)     # x86_64-mont5.pl:1975
            lo = nptr[0]       # x86_64-mont5.pl:1976 pull n[0]
            r15 = hi           # x86_64-mont5.pl:1977
            adcq(r15, 0)       # x86_64-mont5.pl:1978
            advRcx(-8)         # x86_64-mont5.pl:1980 dec %ecx
            var ce2 {.register: "rcx".}: int
            cmpi(ce2, tm1S)
            var cp2 {.register: "rcx".}: W64
            if zf(): break     # x86_64-mont5.pl:1981 jnz .L8x_tail
          advRdi(64)           # x86_64-mont5.pl:1983 lea 8*8($nptr),$nptr
          var tendC {.register: "rdx".}: W64
          tend = tEndS         # x86_64-mont5.pl:1984 pull end of t[]
          var npi2 {.register: "rdi".}: int
          cmpi(npi2, nEndS)    # x86_64-mont5.pl:1985 end of n[]?
          var npp2 {.register: "rdi".}: W64
          if nc(): break       # x86_64-mont5.pl:1986 jae .L8x_tail_done
          spw = tBaseS         # x86_64-mont5.pl:1988 pull n0*a[0] (t[7])
          m0 = spw[7]
          var cyd {.register: "rdx".}: uint64   # $carry, in %rdx (free here)
          cyd = carryS
          negq(cyd)            # x86_64-mont5.pl:1989
          lo = nptr[0]         # x86_64-mont5.pl:1990 pull n[0]
          adcq(r8, rtp2[0])    # x86_64-mont5.pl:1991
          adcq(r9, rtp2[1])    # x86_64-mont5.pl:1992
          adcq(r10, rtp2[2])   # x86_64-mont5.pl:1993
          adcq(r11, rtp2[3])   # x86_64-mont5.pl:1994
          adcq(r12, rtp2[4])   # x86_64-mont5.pl:1995
          adcq(r13, rtp2[5])   # x86_64-mont5.pl:1996
          adcq(r14, rtp2[6])   # x86_64-mont5.pl:1997
          adcq(r15, rtp2[7])   # x86_64-mont5.pl:1998
          sbbq(cyd, cyd)       # x86_64-mont5.pl:1999 top carry
          carryS = cyd
          advRcx(56)           # x86_64-mont5.pl:2001 mov \$8,%ecx: t[8-1]
          var hi2 {.register: "rdx".}: uint64
          # x86_64-mont5.pl:2002 jmp .L8x_tail
        # .L8x_tail_done
        var tend2 {.register: "rdx".}: W64
        xorq(lo, lo)           # x86_64-mont5.pl:2006
        addq(r8, tend2[0])     # x86_64-mont5.pl:2007 can this overflow?
        adcq(r9, 0)            # x86_64-mont5.pl:2008
        adcq(r10, 0)           # x86_64-mont5.pl:2009
        adcq(r11, 0)           # x86_64-mont5.pl:2010
        adcq(r12, 0)           # x86_64-mont5.pl:2011
        adcq(r13, 0)           # x86_64-mont5.pl:2012
        adcq(r14, 0)           # x86_64-mont5.pl:2013
        adcq(r15, 0)           # x86_64-mont5.pl:2014
        adcq(lo, 0)            # x86_64-mont5.pl:2015
        var cyc {.register: "rcx".}: uint64    # $carry, in %rcx (free here)
        cyc = carryS
        negq(cyc)              # x86_64-mont5.pl:2017
        # .L8x_no_tail, reached by falling through
        noTail(r8, r9, r10, r11, r12, r13, r14, r15, rtp2, lo, nptr, nptrS, carryS)
      else:
        # .L8x_no_tail, reached by jae (x86_64-mont5.pl:1895); %rsi/%rdi
        # are named again: the other arm's names for them ended with it
        var rtpE {.register: "rsi".}: W64
        var nptrE {.register: "rdi".}: W64
        noTail(r8, r9, r10, r11, r12, r13, r14, r15, rtp2, lo, nptr, nptrS, carryS)
      var tpi {.register: "rsi".}: int
      var tei {.register: "rdx".}: int
      cmpi(tpi, tei)           # x86_64-mont5.pl:2044 end of t[]?
      var tpp {.register: "rsi".}: W64
      var tep {.register: "rdx".}: W64
      if nc(): break           # x86_64-mont5.pl:2045 jb .L8x_reduction_loop
    # ---- __bn_post4x_internal (x86_64-mont5.pl:2059-2110) ----
    # register map: $tptr %rbx, $nptr %rbp -> %r11 (as in `sqr4xSub`);
    # %rax is the top-most carry from the reduction, %rsi its $tptr
    var nptrP {.register: "rdi".}: W64   # named again after the loop
    var snp {.register: "r11".}: W64
    snp = nptr                 # $nptr into %r11
    var s12 {.register: "r12".}: uint64
    s12 = snp[0]               # x86_64-mont5.pl:2061
    var rtp3 {.register: "rsi".}: W64
    var stp {.register: "rbx".}: W64
    stp = rtp3                 # x86_64-mont5.pl:2062 lea (%rdi,$num),$tptr
    advRbx(numNegS)
    var scnt {.register: "rcx".}: int
    scnt = numNegS             # x86_64-mont5.pl:2063
    var srp {.register: "rdi".}: W64
    srp = rptrS                # x86_64-mont5.pl:2064 restore $rptr
    var smask {.register: "rax".}: uint64
    negq(smask)                # x86_64-mont5.pl:2065
    var nap {.register: "rsi".}: W64
    nap = rptrS                # x86_64-mont5.pl:2066 prepare for back-to-back call
    sari(scnt, 5)              # x86_64-mont5.pl:2067 sar \$3+2,%rcx
    decq(s12)                  # x86_64-mont5.pl:2068 so that after 'not' we get -n[0]
    var scy {.register: "r10".}: uint64
    xorq(scy, scy)             # x86_64-mont5.pl:2069
    var s13 {.register: "r13".}: uint64
    var s14 {.register: "r14".}: uint64
    var s15 {.register: "r15".}: uint64
    s13 = snp[1]               # x86_64-mont5.pl:2070
    s14 = snp[2]               # x86_64-mont5.pl:2071
    s15 = snp[3]               # x86_64-mont5.pl:2072
    sqr4xSub(s12, s13, s14, s15, smask, scy, stp, snp, srp, scnt)  # x86_64-mont5.pl:2073-2106
    var r10n {.register: "r10".}: int
    r10n = numNegS             # x86_64-mont5.pl:2108 prepare for back-to-back call
    # x86_64-mont5.pl:2109 neg $num: $num is numS
    var c5 {.register: "rcx".}: int
    c5 = cnt5S                 # the next of the five call pairs
    deci(c5)
    cnt5S = c5
    if zf(): break
