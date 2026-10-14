#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Poly1305 for AArch64, in `{.assembler.}`: the twin of `poly1305x64`, with
## the same procs and the same state layout.
##
## The accumulator `h` is 130 bits in three words, `r` two words, and the
## product is reduced with `2^130 = 5 (mod p)` folded in through
## `s1 = r1 + r1 shr 2`. This is the scalar algorithm of OpenSSL's
## `poly1305-armv8`: `r0`, `r1` and `s1` stay in registers for the whole run,
## the products are `mul`/`umulh` pairs, and the carry chain runs through them
## because AArch64's multiplies leave the flags alone.

type
  Words* = ptr UncheckedArray[uint64]
    ## The state is `[h0, h1, h2, r0, r1, s1]`.

proc adds(a, b: uint64): uint64 {.instruction: "adds3".}
proc adcs(a, b: uint64): uint64 {.instruction: "adcs3".}
proc mulLo(a, b: uint64): uint64 {.instruction: "mul3".}
proc mulHi(a, b: uint64): uint64 {.instruction: "umulh".}
proc cselne(a, b: uint64): uint64 {.instruction: "cselne".}
proc cmpq(a, b: uint64) {.instruction: "cmp".}
proc addq(d: var uint64; s: uint64) {.instruction: "add".}
proc andq(d: var uint64; s: uint64) {.instruction: "and".}
proc shrq(d: var uint64; c: int) {.instruction: "shr".}
proc addi(d: var int; s: int) {.instruction: "add".}
proc subsi(a, b: int): int {.instruction: "subs3".}
proc zf(): bool {.instruction: "zf".}

proc polyBlocks*(st {.register: "x0".}: Words; m {.register: "x1".}: Words;
                 n {.register: "x2".}: int; hibit {.register: "x3".}: uint64) {.assembler.} =
  ## Absorbs `n >= 1` sixteen-byte blocks at `m`: `h = (h + block) * r`. A
  ## block is a 129-bit number when `hibit` is 1 (every full block) and 128
  ## bits when it is 0 (a final block the caller padded).
  var h0 {.register: "x4".}: uint64
  var h1 {.register: "x5".}: uint64
  var h2 {.register: "x6".}: uint64
  var r0 {.register: "x7".}: uint64
  var r1 {.register: "x9".}: uint64
  var s1 {.register: "x10".}: uint64
  var d0 {.register: "x11".}: uint64
  var d1 {.register: "x12".}: uint64
  var d2 {.register: "x13".}: uint64
  var t0 {.register: "x14".}: uint64
  var t1 {.register: "x15".}: uint64
  var i {.register: "x19".}: int
  var cnt {.register: "x20".}: int
  h0 = st[0]
  h1 = st[1]
  h2 = st[2]
  r0 = st[3]
  r1 = st[4]
  s1 = st[5]
  cnt = n
  i = 0
  while true:
    # h += the block (and 2^128 for a full one)
    t0 = m[i]
    addi(i, 1)
    t1 = m[i]
    addi(i, 1)
    h0 = adds(h0, t0)
    h1 = adcs(h1, t1)
    h2 = adcs(h2, hibit)
    # d = h * r, with h1 * r1 and h2 * r1 folded in as h * s1
    d0 = mulLo(h0, r0)
    d1 = mulHi(h0, r0)
    t0 = mulLo(h1, s1)
    t1 = mulHi(h1, s1)
    d0 = adds(d0, t0)
    t0 = mulLo(h0, r1)
    d1 = adcs(d1, t1)
    d2 = mulHi(h0, r1)
    d1 = adds(d1, t0)
    t0 = mulLo(h1, r0)
    d2 = adcs(d2, 0)
    t1 = mulHi(h1, r0)
    d1 = adds(d1, t0)
    t0 = mulLo(h2, s1)         # h2 is small: no high half
    d2 = adcs(d2, t1)
    t1 = mulLo(h2, r0)
    d1 = adds(d1, t0)
    d2 = adcs(d2, t1)
    # h2 = d2 & 3; h = d0..d1 + (d2 & ~3) + (d2 >> 2), i.e. 5 * (d2 >> 2)
    t0 = d2
    andq(t0, 0xFFFF_FFFF_FFFF_FFFC'u64)
    h2 = d2
    andq(h2, 3)
    shrq(d2, 2)
    addq(t0, d2)
    h0 = adds(d0, t0)
    h1 = adcs(d1, 0)
    h2 = adcs(h2, 0)
    cnt = subsi(cnt, 1)
    if zf(): break
  st[0] = h0
  st[1] = h1
  st[2] = h2

proc polyFinish*(st {.register: "x0".}: Words; tag {.register: "x1".}: Words;
                 s {.register: "x2".}: Words) {.assembler.} =
  ## `tag = (h mod p + s) mod 2^128`. Whether `h >= p` is decided by the
  ## carry into bit 130 of `h + 5`, and acted on with `csel`.
  var h0 {.register: "x3".}: uint64
  var h1 {.register: "x4".}: uint64
  var h2 {.register: "x5".}: uint64
  var g0 {.register: "x6".}: uint64
  var g1 {.register: "x7".}: uint64
  var s0 {.register: "x9".}: uint64
  var s1 {.register: "x10".}: uint64
  h0 = st[0]
  h1 = st[1]
  h2 = st[2]
  s0 = s[0]
  s1 = s[1]
  g0 = adds(h0, 5)
  g1 = adcs(h1, 0)
  h2 = adcs(h2, 0)
  shrq(h2, 2)                  # non-zero iff h + 5 reached 2^130, i.e. h >= p
  cmpq(h2, 0)
  h0 = cselne(g0, h0)
  h1 = cselne(g1, h1)
  h0 = adds(h0, s0)
  h1 = adcs(h1, s1)
  tag[0] = h0
  tag[1] = h1
