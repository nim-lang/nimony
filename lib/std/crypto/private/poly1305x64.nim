#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Poly1305 for x86-64, in `{.assembler.}`: the block function and the final
## reduction `std/crypto/chacha20poly1305` runs on the native backend.
##
## The accumulator `h` is 130 bits in three words, `r` two words, and the
## product is reduced with `2^130 = 5 (mod p)` folded in through
## `s1 = r1 + r1 shr 2` (r1 is a multiple of 4 after clamping). This is the
## scalar algorithm of OpenSSL's `poly1305-x86_64`, which is constant time as
## long as nothing in it branches — and here nothing can.

type
  Words* = ptr UncheckedArray[uint64]
    ## The state is `[h0, h1, h2, r0, r1, s1]`.

proc mulq(lo: var uint64; hi: var uint64; s: uint64) {.instruction: "mul".}
proc imulq(d: var uint64; s: uint64) {.instruction: "imul".}
proc addq(d: var uint64; s: uint64) {.instruction: "add".}
proc adcq(d: var uint64; s: uint64) {.instruction: "adc".}
proc andq(d: var uint64; s: uint64) {.instruction: "and".}
proc shrq(d: var uint64; c: int) {.instruction: "shr".}
proc cmovnzq(d: var uint64; s: uint64) {.instruction: "cmovnz".}
proc addi(d: var int; s: int) {.instruction: "add".}
proc deci(d: var int) {.instruction: "dec".}
proc zf(): bool {.instruction: "zf".}

proc polyBlocks*(st {.register: "rdi".}: Words; m {.register: "rsi".}: Words;
                 n {.register: "rdx".}: int; hibit {.register: "rcx".}: uint64) {.assembler.} =
  ## Absorbs `n >= 1` sixteen-byte blocks at `m`: `h = (h + block) * r`. A
  ## block is a 129-bit number when `hibit` is 1 (every full block) and 128
  ## bits when it is 0 (a final block the caller padded).
  var cnt {.register: "r8".}: int
  cnt = n
  var lo {.register: "rax".}: uint64
  var hi {.register: "rdx".}: uint64
  var i {.register: "r9".}: int
  var j {.register: "r10".}: int
  var h0 {.register: "r11".}: uint64
  var h1 {.register: "rbx".}: uint64
  var h2 {.register: "r12".}: uint64
  var d1 {.register: "r13".}: uint64
  var d2 {.register: "r14".}: uint64
  var d3 {.register: "r15".}: uint64
  i = 0
  j = 1
  h0 = st[0]
  h1 = st[1]
  h2 = st[2]
  while true:
    addq(h0, m[i])
    adcq(h1, m[j])
    adcq(h2, hibit)
    lo = st[4]
    mulq(lo, hi, h0)           # h0 * r1
    d2 = lo
    d3 = hi
    lo = st[3]
    mulq(lo, hi, h0)           # h0 * r0
    h0 = lo
    d1 = hi
    lo = st[3]
    mulq(lo, hi, h1)           # h1 * r0
    addq(d2, lo)
    adcq(d3, hi)
    lo = st[5]
    mulq(lo, hi, h1)           # h1 * s1
    h1 = h2
    addq(h0, lo)
    adcq(d1, hi)
    imulq(h1, st[5])           # h2 * s1 (h2 is small)
    addq(d2, h1)
    h1 = d1
    adcq(d3, 0)
    imulq(h2, st[3])           # h2 * r0
    addq(h1, d2)
    lo = 0xFFFF_FFFF_FFFF_FFFC'u64
    adcq(d3, h2)
    # h2 = d3 & 3; h += (d3 & ~3) + (d3 >> 2), i.e. 5 * (d3 >> 2)
    andq(lo, d3)
    h2 = d3
    shrq(d3, 2)
    andq(h2, 3)
    addq(lo, d3)
    addq(h0, lo)
    adcq(h1, 0)
    adcq(h2, 0)
    addi(i, 2)
    addi(j, 2)
    deci(cnt)
    if zf(): break
  st[0] = h0
  st[1] = h1
  st[2] = h2

proc polyFinish*(st {.register: "rdi".}: Words; tag {.register: "rsi".}: Words;
                 s {.register: "rdx".}: Words) {.assembler.} =
  ## `tag = (h mod p + s) mod 2^128`. Whether `h >= p` is decided by the
  ## carry into bit 130 of `h + 5`, and acted on with `cmovnz`.
  var h0 {.register: "r8".}: uint64
  var h1 {.register: "r9".}: uint64
  var h2 {.register: "r10".}: uint64
  var g0 {.register: "rax".}: uint64
  var g1 {.register: "rcx".}: uint64
  h0 = st[0]
  h1 = st[1]
  h2 = st[2]
  g0 = h0
  g1 = h1
  addq(g0, 5)
  adcq(g1, 0)
  adcq(h2, 0)
  shrq(h2, 2)                  # non-zero iff h + 5 reached 2^130, i.e. h >= p
  cmovnzq(h0, g0)
  cmovnzq(h1, g1)
  addq(h0, s[0])
  adcq(h1, s[1])
  tag[0] = h0
  tag[1] = h1
