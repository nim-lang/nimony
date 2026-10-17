#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## GF(2^255 - 19) for x86-64, in `{.assembler.}`: the field arithmetic
## `std/crypto/x25519` runs on the native backend.
##
## Why assembler: X25519's scalar is the secret, and constant time is a
## property of the INSTRUCTIONS executed — no branch and no memory index may
## depend on it. A compiler is free to turn `mask and (a xor b)` into a branch
## if it thinks that faster; an `{.assembler.}` body is transliterated one
## statement to one instruction, so what is written here is what runs. Every
## procedure below executes the same instruction sequence for every input.
##
## An element is four little-endian 64-bit limbs holding a value below 2^256
## that is congruent to the field element; only `feFreeze` produces the
## canonical representative below p. Reduction uses 2^256 = 38 (mod p), and
## every conditional correction is a mask made by `sbb`, never a jump.

type
  Fe64* = array[4, uint64]
  FePtr* = ptr UncheckedArray[uint64]

proc mulq(lo: var uint64; hi: var uint64; s: uint64) {.instruction: "mul".}
proc imulq(d: var uint64; s: uint64) {.instruction: "imul".}
proc addq(d: var uint64; s: uint64) {.instruction: "add".}
proc adcq(d: var uint64; s: uint64) {.instruction: "adc".}
proc subq(d: var uint64; s: uint64) {.instruction: "sub".}
proc sbbq(d: var uint64; s: uint64) {.instruction: "sbb".}
proc andq(d: var uint64; s: uint64) {.instruction: "and".}
proc xorq(d: var uint64; s: uint64) {.instruction: "xor".}
proc negq(d: var uint64) {.instruction: "neg".}
proc shlq(d: var uint64; c: int) {.instruction: "shl".}
proc shrq(d: var uint64; c: int) {.instruction: "shr".}
proc sarq(d: var uint64; c: int) {.instruction: "sar".}

proc feMul*(r {.register: "rdi".}: FePtr; a {.register: "rsi".}: FePtr;
            b {.register: "rdx".}: FePtr) {.assembler.} =
  ## `r = a * b`. Operand-scanning schoolbook into eight limbs, then the top
  ## four folded down as `38 * hi`. `r` may alias `a` or `b`: nothing is stored
  ## before everything is read.
  var bp {.register: "rcx".}: FePtr
  bp = b                       # rdx is the high half of every product below
  var lo {.register: "rax".}: uint64
  var hi {.register: "rdx".}: uint64
  var t0 {.register: "r8".}: uint64
  var t1 {.register: "r9".}: uint64
  var t2 {.register: "r10".}: uint64
  var t3 {.register: "r11".}: uint64
  var t4 {.register: "rbx".}: uint64
  var t5 {.register: "r12".}: uint64
  var t6 {.register: "r13".}: uint64
  var t7 {.register: "r14".}: uint64
  var c {.register: "r15".}: uint64
  # t0..t4 = a0 * b
  lo = bp[0]
  mulq(lo, hi, a[0])
  t0 = lo
  t1 = hi
  lo = bp[1]
  mulq(lo, hi, a[0])
  addq(t1, lo)
  adcq(hi, 0)
  t2 = hi
  lo = bp[2]
  mulq(lo, hi, a[0])
  addq(t2, lo)
  adcq(hi, 0)
  t3 = hi
  lo = bp[3]
  mulq(lo, hi, a[0])
  addq(t3, lo)
  adcq(hi, 0)
  t4 = hi
  # t1..t5 += a1 * b
  lo = bp[0]
  mulq(lo, hi, a[1])
  addq(t1, lo)
  adcq(hi, 0)
  c = hi
  lo = bp[1]
  mulq(lo, hi, a[1])
  addq(lo, c)
  adcq(hi, 0)
  addq(t2, lo)
  adcq(hi, 0)
  c = hi
  lo = bp[2]
  mulq(lo, hi, a[1])
  addq(lo, c)
  adcq(hi, 0)
  addq(t3, lo)
  adcq(hi, 0)
  c = hi
  lo = bp[3]
  mulq(lo, hi, a[1])
  addq(lo, c)
  adcq(hi, 0)
  addq(t4, lo)
  adcq(hi, 0)
  t5 = hi
  # t2..t6 += a2 * b
  lo = bp[0]
  mulq(lo, hi, a[2])
  addq(t2, lo)
  adcq(hi, 0)
  c = hi
  lo = bp[1]
  mulq(lo, hi, a[2])
  addq(lo, c)
  adcq(hi, 0)
  addq(t3, lo)
  adcq(hi, 0)
  c = hi
  lo = bp[2]
  mulq(lo, hi, a[2])
  addq(lo, c)
  adcq(hi, 0)
  addq(t4, lo)
  adcq(hi, 0)
  c = hi
  lo = bp[3]
  mulq(lo, hi, a[2])
  addq(lo, c)
  adcq(hi, 0)
  addq(t5, lo)
  adcq(hi, 0)
  t6 = hi
  # t3..t7 += a3 * b
  lo = bp[0]
  mulq(lo, hi, a[3])
  addq(t3, lo)
  adcq(hi, 0)
  c = hi
  lo = bp[1]
  mulq(lo, hi, a[3])
  addq(lo, c)
  adcq(hi, 0)
  addq(t4, lo)
  adcq(hi, 0)
  c = hi
  lo = bp[2]
  mulq(lo, hi, a[3])
  addq(lo, c)
  adcq(hi, 0)
  addq(t5, lo)
  adcq(hi, 0)
  c = hi
  lo = bp[3]
  mulq(lo, hi, a[3])
  addq(lo, c)
  adcq(hi, 0)
  addq(t6, lo)
  adcq(hi, 0)
  t7 = hi
  # t0..t3 += 38 * t4..t7, since 2^256 = 38 (mod p)
  lo = 38
  mulq(lo, hi, t4)
  addq(t0, lo)
  adcq(hi, 0)
  c = hi
  lo = 38
  mulq(lo, hi, t5)
  addq(lo, c)
  adcq(hi, 0)
  addq(t1, lo)
  adcq(hi, 0)
  c = hi
  lo = 38
  mulq(lo, hi, t6)
  addq(lo, c)
  adcq(hi, 0)
  addq(t2, lo)
  adcq(hi, 0)
  c = hi
  lo = 38
  mulq(lo, hi, t7)
  addq(lo, c)
  adcq(hi, 0)
  addq(t3, lo)
  adcq(hi, 0)
  c = hi
  # the carry word is at most 38: fold it again
  lo = 38
  imulq(lo, c)
  addq(t0, lo)
  adcq(t1, 0)
  adcq(t2, 0)
  adcq(t3, 0)
  # a carry out of that leaves t below 38*39, so one more 38 cannot carry
  sbbq(c, c)
  andq(c, 38)
  addq(t0, c)
  r[0] = t0
  r[1] = t1
  r[2] = t2
  r[3] = t3

proc feAdd*(r {.register: "rdi".}: FePtr; a {.register: "rsi".}: FePtr;
            b {.register: "rdx".}: FePtr) {.assembler.} =
  ## `r = a + b`; a carry out of 2^256 is worth 38.
  var t0 {.register: "r8".}: uint64
  var t1 {.register: "r9".}: uint64
  var t2 {.register: "r10".}: uint64
  var t3 {.register: "r11".}: uint64
  var m {.register: "rax".}: uint64
  t0 = a[0]
  t1 = a[1]
  t2 = a[2]
  t3 = a[3]
  addq(t0, b[0])
  adcq(t1, b[1])
  adcq(t2, b[2])
  adcq(t3, b[3])
  sbbq(m, m)                   # all ones iff the sum carried
  andq(m, 38)
  addq(t0, m)
  adcq(t1, 0)
  adcq(t2, 0)
  adcq(t3, 0)
  sbbq(m, m)
  andq(m, 38)
  addq(t0, m)
  r[0] = t0
  r[1] = t1
  r[2] = t2
  r[3] = t3

proc feSub*(r {.register: "rdi".}: FePtr; a {.register: "rsi".}: FePtr;
            b {.register: "rdx".}: FePtr) {.assembler.} =
  ## `r = a - b`; a borrow out of 2^256 is worth -38.
  var t0 {.register: "r8".}: uint64
  var t1 {.register: "r9".}: uint64
  var t2 {.register: "r10".}: uint64
  var t3 {.register: "r11".}: uint64
  var m {.register: "rax".}: uint64
  t0 = a[0]
  t1 = a[1]
  t2 = a[2]
  t3 = a[3]
  subq(t0, b[0])
  sbbq(t1, b[1])
  sbbq(t2, b[2])
  sbbq(t3, b[3])
  sbbq(m, m)                   # all ones iff the difference borrowed
  andq(m, 38)
  subq(t0, m)
  sbbq(t1, 0)
  sbbq(t2, 0)
  sbbq(t3, 0)
  sbbq(m, m)
  andq(m, 38)
  subq(t0, m)
  r[0] = t0
  r[1] = t1
  r[2] = t2
  r[3] = t3

proc feMulSmall*(r {.register: "rdi".}: FePtr; a {.register: "rsi".}: FePtr;
                 k {.register: "rdx".}: uint64) {.assembler.} =
  ## `r = a * k` for a public `k` below 2^32 (the curve's 121665).
  var kk {.register: "rcx".}: uint64
  kk = k
  var lo {.register: "rax".}: uint64
  var hi {.register: "rdx".}: uint64
  var t0 {.register: "r8".}: uint64
  var t1 {.register: "r9".}: uint64
  var t2 {.register: "r10".}: uint64
  var t3 {.register: "r11".}: uint64
  lo = a[0]
  mulq(lo, hi, kk)
  t0 = lo
  t1 = hi
  lo = a[1]
  mulq(lo, hi, kk)
  addq(t1, lo)
  adcq(hi, 0)
  t2 = hi
  lo = a[2]
  mulq(lo, hi, kk)
  addq(t2, lo)
  adcq(hi, 0)
  t3 = hi
  lo = a[3]
  mulq(lo, hi, kk)
  addq(t3, lo)
  adcq(hi, 0)
  # the `a` pointer is spent: rsi takes the carry word from here on (declared
  # only now, since declaring it ends the pointer's binding)
  var c {.register: "rsi".}: uint64
  c = hi
  lo = 38
  imulq(lo, c)
  addq(t0, lo)
  adcq(t1, 0)
  adcq(t2, 0)
  adcq(t3, 0)
  sbbq(c, c)
  andq(c, 38)
  addq(t0, c)
  r[0] = t0
  r[1] = t1
  r[2] = t2
  r[3] = t3

proc feCswap*(a {.register: "rdi".}: FePtr; b {.register: "rsi".}: FePtr;
              bit {.register: "rdx".}: uint64) {.assembler.} =
  ## Swaps `a` and `b` iff `bit == 1`: the same loads, xors and stores either
  ## way, the choice carried only by the mask.
  var m {.register: "rdx".}: uint64
  var x {.register: "rax".}: uint64
  var y {.register: "rcx".}: uint64
  var t {.register: "r8".}: uint64
  m = bit
  negq(m)                      # 0 -> 0, 1 -> all ones
  x = a[0]
  y = b[0]
  t = x
  xorq(t, y)
  andq(t, m)
  xorq(x, t)
  xorq(y, t)
  a[0] = x
  b[0] = y
  x = a[1]
  y = b[1]
  t = x
  xorq(t, y)
  andq(t, m)
  xorq(x, t)
  xorq(y, t)
  a[1] = x
  b[1] = y
  x = a[2]
  y = b[2]
  t = x
  xorq(t, y)
  andq(t, m)
  xorq(x, t)
  xorq(y, t)
  a[2] = x
  b[2] = y
  x = a[3]
  y = b[3]
  t = x
  xorq(t, y)
  andq(t, m)
  xorq(x, t)
  xorq(y, t)
  a[3] = x
  b[3] = y

proc feFreeze*(r {.register: "rdi".}: FePtr; a {.register: "rsi".}: FePtr) {.assembler.} =
  ## The canonical representative below p = 2^255 - 19.
  var t0 {.register: "r8".}: uint64
  var t1 {.register: "r9".}: uint64
  var t2 {.register: "r10".}: uint64
  var t3 {.register: "r11".}: uint64
  var q {.register: "rax".}: uint64
  var n19 {.register: "rcx".}: uint64
  t0 = a[0]
  t1 = a[1]
  t2 = a[2]
  t3 = a[3]
  # bit 255 is worth 19
  q = t3
  shrq(q, 63)
  shlq(t3, 1)
  shrq(t3, 1)
  n19 = 19
  imulq(q, n19)
  addq(t0, q)
  adcq(t1, 0)
  adcq(t2, 0)
  adcq(t3, 0)
  # now t < 2^255 + 19; t >= p iff t + 19 reaches bit 255
  var u0 {.register: "rax".}: uint64
  var u1 {.register: "rcx".}: uint64
  var u2 {.register: "rdx".}: uint64
  var u3 {.register: "rsi".}: uint64
  var m {.register: "r12".}: uint64
  u0 = t0
  u1 = t1
  u2 = t2
  u3 = t3
  addq(u0, 19)
  adcq(u1, 0)
  adcq(u2, 0)
  adcq(u3, 0)
  m = u3
  sarq(m, 63)                  # all ones iff t >= p
  shlq(u3, 1)
  shrq(u3, 1)                  # u - 2^255 = t - p
  # t = m ? u : t
  xorq(u0, t0)
  andq(u0, m)
  xorq(t0, u0)
  xorq(u1, t1)
  andq(u1, m)
  xorq(t1, u1)
  xorq(u2, t2)
  andq(u2, m)
  xorq(t2, u2)
  xorq(u3, t3)
  andq(u3, m)
  xorq(t3, u3)
  r[0] = t0
  r[1] = t1
  r[2] = t2
  r[3] = t3
