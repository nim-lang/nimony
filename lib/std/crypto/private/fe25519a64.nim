#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## GF(2^255 - 19) for AArch64, in `{.assembler.}`: the field arithmetic
## `std/crypto/x25519` runs on the native backend. The twin of `fe25519x64`,
## with the same procs and the same element layout; see that module for why
## this is assembler at all.
##
## An element is four little-endian 64-bit limbs holding a value below 2^256
## that is congruent to the field element; only `feFreeze` produces the
## canonical representative below p. Reduction uses 2^256 = 38 (mod p).
##
## What differs from x86-64 is what AArch64 makes cheap. `mul`/`umulh` are
## three-address and leave NZCV alone, so a carry chain may run straight
## through a block of products; the flag-setting `adds`/`adcs`/`subs`/`sbcs`
## are instructions of their own, so nothing else touches the carry; and a
## conditional correction is a `csel`, which selects without branching. Note
## Arm's carry after a SUBTRACT: C is set when nothing was borrowed, so a
## borrow is the `lo` condition.

type
  Fe64* = array[4, uint64]
  FePtr* = ptr UncheckedArray[uint64]

proc adds(a, b: uint64): uint64 {.instruction: "adds3".}
proc adcs(a, b: uint64): uint64 {.instruction: "adcs3".}
proc subs(a, b: uint64): uint64 {.instruction: "subs3".}
proc sbcs(a, b: uint64): uint64 {.instruction: "sbcs3".}
proc mulLo(a, b: uint64): uint64 {.instruction: "mul3".}
proc mulHi(a, b: uint64): uint64 {.instruction: "umulh".}
proc cselhs(a, b: uint64): uint64 {.instruction: "cselhs".}
proc csello(a, b: uint64): uint64 {.instruction: "csello".}
proc cselne(a, b: uint64): uint64 {.instruction: "cselne".}
proc cmpq(a, b: uint64) {.instruction: "cmp".}
proc addq(d: var uint64; s: uint64) {.instruction: "add".}
proc subq(d: var uint64; s: uint64) {.instruction: "sub".}
proc andq(d: var uint64; s: uint64) {.instruction: "and".}
proc shrq(d: var uint64; c: int) {.instruction: "shr".}

template mulRow(ai, b0, b1, b2, b3, s0, s1, s2, s3, t0, t1, t2, t3, t4: untyped) =
  ## `t0..t4 += ai * b`, where `t4` is not yet live: the low halves are added
  ## at t0..t3 with the carry landing in t4, then the high halves one limb up.
  ## The partial product has one limb more than it had, so the second chain
  ## cannot carry out of t4.
  s0 = mulLo(ai, b0)
  s1 = mulLo(ai, b1)
  s2 = mulLo(ai, b2)
  s3 = mulLo(ai, b3)
  t0 = adds(t0, s0)
  t1 = adcs(t1, s1)
  t2 = adcs(t2, s2)
  t3 = adcs(t3, s3)
  t4 = adcs(0, 0)
  s0 = mulHi(ai, b0)
  s1 = mulHi(ai, b1)
  s2 = mulHi(ai, b2)
  s3 = mulHi(ai, b3)
  t1 = adds(t1, s0)
  t2 = adcs(t2, s1)
  t3 = adcs(t3, s2)
  t4 = adcs(t4, s3)

proc feMul*(r {.register: "x0".}: FePtr; a {.register: "x1".}: FePtr;
            b {.register: "x2".}: FePtr) {.assembler.} =
  ## `r = a * b`: schoolbook into eight limbs, every operand in a register, then
  ## the top four folded down as `38 * hi`. `r` may alias `a` or `b`: nothing
  ## is stored before everything is read.
  var a0 {.register: "x3".}: uint64
  var a1 {.register: "x4".}: uint64
  var a2 {.register: "x5".}: uint64
  var a3 {.register: "x6".}: uint64
  var b0 {.register: "x7".}: uint64
  var b1 {.register: "x9".}: uint64
  var b2 {.register: "x10".}: uint64
  var b3 {.register: "x11".}: uint64
  var t0 {.register: "x12".}: uint64
  var t1 {.register: "x13".}: uint64
  var t2 {.register: "x14".}: uint64
  var t3 {.register: "x15".}: uint64
  var t4 {.register: "x19".}: uint64
  var t5 {.register: "x20".}: uint64
  var t6 {.register: "x21".}: uint64
  var t7 {.register: "x22".}: uint64
  var s0 {.register: "x23".}: uint64
  var s1 {.register: "x24".}: uint64
  var s2 {.register: "x25".}: uint64
  var s3 {.register: "x26".}: uint64
  var c38 {.register: "x27".}: uint64
  a0 = a[0]
  a1 = a[1]
  a2 = a[2]
  a3 = a[3]
  b0 = b[0]
  b1 = b[1]
  b2 = b[2]
  b3 = b[3]
  # t0..t4 = a0 * b
  t0 = mulLo(a0, b0)
  t1 = mulLo(a0, b1)
  t2 = mulLo(a0, b2)
  t3 = mulLo(a0, b3)
  s0 = mulHi(a0, b0)
  s1 = mulHi(a0, b1)
  s2 = mulHi(a0, b2)
  s3 = mulHi(a0, b3)
  t1 = adds(t1, s0)
  t2 = adcs(t2, s1)
  t3 = adcs(t3, s2)
  t4 = adcs(s3, 0)
  mulRow(a1, b0, b1, b2, b3, s0, s1, s2, s3, t1, t2, t3, t4, t5)
  mulRow(a2, b0, b1, b2, b3, s0, s1, s2, s3, t2, t3, t4, t5, t6)
  mulRow(a3, b0, b1, b2, b3, s0, s1, s2, s3, t3, t4, t5, t6, t7)
  # t0..t3 += 38 * t4..t7, since 2^256 = 38 (mod p); a and b are spent, so
  # their registers take the high halves
  var h0 {.register: "x3".}: uint64
  var h1 {.register: "x4".}: uint64
  var h2 {.register: "x5".}: uint64
  var h3 {.register: "x6".}: uint64
  var top {.register: "x7".}: uint64
  c38 = 38
  s0 = mulLo(t4, c38)
  s1 = mulLo(t5, c38)
  s2 = mulLo(t6, c38)
  s3 = mulLo(t7, c38)
  h0 = mulHi(t4, c38)
  h1 = mulHi(t5, c38)
  h2 = mulHi(t6, c38)
  h3 = mulHi(t7, c38)
  t0 = adds(t0, s0)
  t1 = adcs(t1, s1)
  t2 = adcs(t2, s2)
  t3 = adcs(t3, s3)
  top = adcs(0, 0)
  t1 = adds(t1, h0)
  t2 = adcs(t2, h1)
  t3 = adcs(t3, h2)
  top = adcs(top, h3)
  # the carry word is at most 38: fold it again
  s0 = mulLo(top, c38)
  t0 = adds(t0, s0)
  t1 = adcs(t1, 0)
  t2 = adcs(t2, 0)
  t3 = adcs(t3, 0)
  # a carry out of that leaves t below 38*39, so one more 38 cannot carry
  s0 = cselhs(c38, 0)
  addq(t0, s0)
  r[0] = t0
  r[1] = t1
  r[2] = t2
  r[3] = t3

proc feAdd*(r {.register: "x0".}: FePtr; a {.register: "x1".}: FePtr;
            b {.register: "x2".}: FePtr) {.assembler.} =
  ## `r = a + b`; a carry out of 2^256 is worth 38.
  var t0 {.register: "x3".}: uint64
  var t1 {.register: "x4".}: uint64
  var t2 {.register: "x5".}: uint64
  var t3 {.register: "x6".}: uint64
  var u0 {.register: "x7".}: uint64
  var u1 {.register: "x9".}: uint64
  var u2 {.register: "x10".}: uint64
  var u3 {.register: "x11".}: uint64
  var m {.register: "x12".}: uint64
  var c38 {.register: "x13".}: uint64
  t0 = a[0]
  t1 = a[1]
  t2 = a[2]
  t3 = a[3]
  u0 = b[0]
  u1 = b[1]
  u2 = b[2]
  u3 = b[3]
  c38 = 38
  t0 = adds(t0, u0)
  t1 = adcs(t1, u1)
  t2 = adcs(t2, u2)
  t3 = adcs(t3, u3)
  m = cselhs(c38, 0)           # 38 iff the sum carried
  t0 = adds(t0, m)
  t1 = adcs(t1, 0)
  t2 = adcs(t2, 0)
  t3 = adcs(t3, 0)
  m = cselhs(c38, 0)
  addq(t0, m)
  r[0] = t0
  r[1] = t1
  r[2] = t2
  r[3] = t3

proc feSub*(r {.register: "x0".}: FePtr; a {.register: "x1".}: FePtr;
            b {.register: "x2".}: FePtr) {.assembler.} =
  ## `r = a - b`; a borrow out of 2^256 is worth -38.
  var t0 {.register: "x3".}: uint64
  var t1 {.register: "x4".}: uint64
  var t2 {.register: "x5".}: uint64
  var t3 {.register: "x6".}: uint64
  var u0 {.register: "x7".}: uint64
  var u1 {.register: "x9".}: uint64
  var u2 {.register: "x10".}: uint64
  var u3 {.register: "x11".}: uint64
  var m {.register: "x12".}: uint64
  var c38 {.register: "x13".}: uint64
  t0 = a[0]
  t1 = a[1]
  t2 = a[2]
  t3 = a[3]
  u0 = b[0]
  u1 = b[1]
  u2 = b[2]
  u3 = b[3]
  c38 = 38
  t0 = subs(t0, u0)
  t1 = sbcs(t1, u1)
  t2 = sbcs(t2, u2)
  t3 = sbcs(t3, u3)
  m = csello(c38, 0)           # 38 iff the difference borrowed (C clear)
  t0 = subs(t0, m)
  t1 = sbcs(t1, 0)
  t2 = sbcs(t2, 0)
  t3 = sbcs(t3, 0)
  m = csello(c38, 0)
  subq(t0, m)
  r[0] = t0
  r[1] = t1
  r[2] = t2
  r[3] = t3

proc feMulSmall*(r {.register: "x0".}: FePtr; a {.register: "x1".}: FePtr;
                 k {.register: "x2".}: uint64) {.assembler.} =
  ## `r = a * k` for a public `k` below 2^32 (the curve's 121665).
  var t0 {.register: "x3".}: uint64
  var t1 {.register: "x4".}: uint64
  var t2 {.register: "x5".}: uint64
  var t3 {.register: "x6".}: uint64
  var h0 {.register: "x7".}: uint64
  var h1 {.register: "x9".}: uint64
  var h2 {.register: "x10".}: uint64
  var h3 {.register: "x11".}: uint64
  var c38 {.register: "x12".}: uint64
  var top {.register: "x13".}: uint64
  t0 = a[0]
  t1 = a[1]
  t2 = a[2]
  t3 = a[3]
  h0 = mulHi(t0, k)
  h1 = mulHi(t1, k)
  h2 = mulHi(t2, k)
  h3 = mulHi(t3, k)
  t0 = mulLo(t0, k)
  t1 = mulLo(t1, k)
  t2 = mulLo(t2, k)
  t3 = mulLo(t3, k)
  t1 = adds(t1, h0)
  t2 = adcs(t2, h1)
  t3 = adcs(t3, h2)
  top = adcs(h3, 0)
  c38 = 38
  top = mulLo(top, c38)
  t0 = adds(t0, top)
  t1 = adcs(t1, 0)
  t2 = adcs(t2, 0)
  t3 = adcs(t3, 0)
  top = cselhs(c38, 0)
  addq(t0, top)
  r[0] = t0
  r[1] = t1
  r[2] = t2
  r[3] = t3

proc feCswap*(a {.register: "x0".}: FePtr; b {.register: "x1".}: FePtr;
              bit {.register: "x2".}: uint64) {.assembler.} =
  ## Swaps `a` and `b` iff `bit == 1`: the same loads, selects and stores
  ## either way. `csel` picks by the flags without branching on them.
  var x {.register: "x3".}: uint64
  var y {.register: "x4".}: uint64
  var nx {.register: "x5".}: uint64
  var ny {.register: "x6".}: uint64
  cmpq(bit, 0)
  x = a[0]
  y = b[0]
  nx = cselne(y, x)
  ny = cselne(x, y)
  a[0] = nx
  b[0] = ny
  x = a[1]
  y = b[1]
  nx = cselne(y, x)
  ny = cselne(x, y)
  a[1] = nx
  b[1] = ny
  x = a[2]
  y = b[2]
  nx = cselne(y, x)
  ny = cselne(x, y)
  a[2] = nx
  b[2] = ny
  x = a[3]
  y = b[3]
  nx = cselne(y, x)
  ny = cselne(x, y)
  a[3] = nx
  b[3] = ny

proc feFreeze*(r {.register: "x0".}: FePtr; a {.register: "x1".}: FePtr) {.assembler.} =
  ## The canonical representative below p = 2^255 - 19.
  var t0 {.register: "x2".}: uint64
  var t1 {.register: "x3".}: uint64
  var t2 {.register: "x4".}: uint64
  var t3 {.register: "x5".}: uint64
  var u0 {.register: "x6".}: uint64
  var u1 {.register: "x7".}: uint64
  var u2 {.register: "x9".}: uint64
  var u3 {.register: "x10".}: uint64
  var q {.register: "x11".}: uint64
  var n19 {.register: "x12".}: uint64
  t0 = a[0]
  t1 = a[1]
  t2 = a[2]
  t3 = a[3]
  # bit 255 is worth 19
  q = t3
  shrq(q, 63)
  andq(t3, 0x7FFF_FFFF_FFFF_FFFF'u64)
  n19 = 19
  q = mulLo(q, n19)
  t0 = adds(t0, q)
  t1 = adcs(t1, 0)
  t2 = adcs(t2, 0)
  t3 = adcs(t3, 0)
  # now t < 2^255 + 19; t >= p iff t + 19 reaches bit 255
  u0 = adds(t0, 19)
  u1 = adcs(t1, 0)
  u2 = adcs(t2, 0)
  u3 = adcs(t3, 0)
  q = u3
  shrq(q, 63)
  andq(u3, 0x7FFF_FFFF_FFFF_FFFF'u64)   # u - 2^255 = t - p
  cmpq(q, 0)
  t0 = cselne(u0, t0)
  t1 = cselne(u1, t1)
  t2 = cselne(u2, t2)
  t3 = cselne(u3, t3)
  r[0] = t0
  r[1] = t1
  r[2] = t2
  r[3] = t3
