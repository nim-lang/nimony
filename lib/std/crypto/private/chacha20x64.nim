#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The ChaCha20 block function for x86-64, in `{.assembler.}`.
##
## ChaCha20 is constant time by design — additions, xors and fixed rotations,
## nothing indexed by data — but "by design" is a statement about an algorithm,
## and what runs is whatever a compiler made of it. Here it is the instructions
## written below, one each.

type
  Words32* = ptr UncheckedArray[uint32]

proc add32(d: var uint32; s: uint32) {.instruction: "add".}
proc xor32(d: var uint32; s: uint32) {.instruction: "xor".}
proc rol32(x: uint32; n: int): uint32 {.instruction: "rol".}
proc deci(d: var int) {.instruction: "dec".}
proc zf(): bool {.instruction: "zf".}

template quarterRound(o: Words32; a, b, c, d: var uint32; ia, ib, ic, id: int) =
  a = o[ia]
  b = o[ib]
  c = o[ic]
  d = o[id]
  add32(a, b)
  xor32(d, a)
  d = rol32(d, 16)
  add32(c, d)
  xor32(b, c)
  b = rol32(b, 12)
  add32(a, b)
  xor32(d, a)
  d = rol32(d, 8)
  add32(c, d)
  xor32(b, c)
  b = rol32(b, 7)
  o[ia] = a
  o[ib] = b
  o[ic] = c
  o[id] = d

proc chachaCore*(o {.register: "rdi".}: Words32; s {.register: "rsi".}: Words32) {.assembler.} =
  ## `o = s + 20 rounds of s`: one 64-byte keystream block from the 16-word
  ## input state. The rounds run in place on `o`.
  var a {.register: "rax".}: uint32
  var b {.register: "rcx".}: uint32
  var c {.register: "rdx".}: uint32
  var d {.register: "r8".}: uint32
  var n {.register: "r9".}: int
  # o = s
  a = s[0]
  o[0] = a
  a = s[1]
  o[1] = a
  a = s[2]
  o[2] = a
  a = s[3]
  o[3] = a
  a = s[4]
  o[4] = a
  a = s[5]
  o[5] = a
  a = s[6]
  o[6] = a
  a = s[7]
  o[7] = a
  a = s[8]
  o[8] = a
  a = s[9]
  o[9] = a
  a = s[10]
  o[10] = a
  a = s[11]
  o[11] = a
  a = s[12]
  o[12] = a
  a = s[13]
  o[13] = a
  a = s[14]
  o[14] = a
  a = s[15]
  o[15] = a
  n = 10
  while true:
    quarterRound(o, a, b, c, d, 0, 4, 8, 12)
    quarterRound(o, a, b, c, d, 1, 5, 9, 13)
    quarterRound(o, a, b, c, d, 2, 6, 10, 14)
    quarterRound(o, a, b, c, d, 3, 7, 11, 15)
    quarterRound(o, a, b, c, d, 0, 5, 10, 15)
    quarterRound(o, a, b, c, d, 1, 6, 11, 12)
    quarterRound(o, a, b, c, d, 2, 7, 8, 13)
    quarterRound(o, a, b, c, d, 3, 4, 9, 14)
    deci(n)
    if zf(): break
  # o += s
  a = o[0]
  add32(a, s[0])
  o[0] = a
  a = o[1]
  add32(a, s[1])
  o[1] = a
  a = o[2]
  add32(a, s[2])
  o[2] = a
  a = o[3]
  add32(a, s[3])
  o[3] = a
  a = o[4]
  add32(a, s[4])
  o[4] = a
  a = o[5]
  add32(a, s[5])
  o[5] = a
  a = o[6]
  add32(a, s[6])
  o[6] = a
  a = o[7]
  add32(a, s[7])
  o[7] = a
  a = o[8]
  add32(a, s[8])
  o[8] = a
  a = o[9]
  add32(a, s[9])
  o[9] = a
  a = o[10]
  add32(a, s[10])
  o[10] = a
  a = o[11]
  add32(a, s[11])
  o[11] = a
  a = o[12]
  add32(a, s[12])
  o[12] = a
  a = o[13]
  add32(a, s[13])
  o[13] = a
  a = o[14]
  add32(a, s[14])
  o[14] = a
  a = o[15]
  add32(a, s[15])
  o[15] = a
