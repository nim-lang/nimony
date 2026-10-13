#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The ChaCha20 block function for AArch64, in `{.assembler.}`: the twin of
## `chacha20x64`, with the same proc.
##
## AArch64 has registers enough for the whole 16-word state, so unlike the
## x86-64 version the rounds never touch memory: the input is loaded once, the
## twenty rounds run in registers, and the output is stored once. A `uint32`
## local is a `w` register, so `add` is the 32-bit `add` and the rotates are
## 32-bit `ror`s (left by 16, 12, 8, 7 is right by 16, 20, 24, 25).

type
  Words32* = ptr UncheckedArray[uint32]

proc add32(d: var uint32; s: uint32) {.instruction: "add".}
proc xor32(d: var uint32; s: uint32) {.instruction: "xor".}
proc ror32(x: uint32; n: int): uint32 {.instruction: "ror".}
proc subsi(a, b: int): int {.instruction: "subs3".}
proc zf(): bool {.instruction: "zf".}

template quarterRound(a, b, c, d: untyped) =
  add32(a, b)
  xor32(d, a)
  d = ror32(d, 16)
  add32(c, d)
  xor32(b, c)
  b = ror32(b, 20)
  add32(a, b)
  xor32(d, a)
  d = ror32(d, 24)
  add32(c, d)
  xor32(b, c)
  b = ror32(b, 25)

template finish(o, s, v, t: untyped; i: int) =
  t = s[i]
  add32(v, t)
  o[i] = v

proc chachaCore*(o {.register: "x0".}: Words32; s {.register: "x1".}: Words32) {.assembler.} =
  ## `o = s + 20 rounds of s`: one 64-byte keystream block from the 16-word
  ## input state.
  var v0 {.register: "x2".}: uint32
  var v1 {.register: "x3".}: uint32
  var v2 {.register: "x4".}: uint32
  var v3 {.register: "x5".}: uint32
  var v4 {.register: "x6".}: uint32
  var v5 {.register: "x7".}: uint32
  var v6 {.register: "x9".}: uint32
  var v7 {.register: "x10".}: uint32
  var v8 {.register: "x11".}: uint32
  var v9 {.register: "x12".}: uint32
  var v10 {.register: "x13".}: uint32
  var v11 {.register: "x14".}: uint32
  var v12 {.register: "x15".}: uint32
  var v13 {.register: "x19".}: uint32
  var v14 {.register: "x20".}: uint32
  var v15 {.register: "x21".}: uint32
  var t {.register: "x22".}: uint32
  var n {.register: "x23".}: int
  v0 = s[0]
  v1 = s[1]
  v2 = s[2]
  v3 = s[3]
  v4 = s[4]
  v5 = s[5]
  v6 = s[6]
  v7 = s[7]
  v8 = s[8]
  v9 = s[9]
  v10 = s[10]
  v11 = s[11]
  v12 = s[12]
  v13 = s[13]
  v14 = s[14]
  v15 = s[15]
  n = 10
  while true:
    quarterRound(v0, v4, v8, v12)
    quarterRound(v1, v5, v9, v13)
    quarterRound(v2, v6, v10, v14)
    quarterRound(v3, v7, v11, v15)
    quarterRound(v0, v5, v10, v15)
    quarterRound(v1, v6, v11, v12)
    quarterRound(v2, v7, v8, v13)
    quarterRound(v3, v4, v9, v14)
    n = subsi(n, 1)
    if zf(): break
  # o = the rounds + s
  finish(o, s, v0, t, 0)
  finish(o, s, v1, t, 1)
  finish(o, s, v2, t, 2)
  finish(o, s, v3, t, 3)
  finish(o, s, v4, t, 4)
  finish(o, s, v5, t, 5)
  finish(o, s, v6, t, 6)
  finish(o, s, v7, t, 7)
  finish(o, s, v8, t, 8)
  finish(o, s, v9, t, 9)
  finish(o, s, v10, t, 10)
  finish(o, s, v11, t, 11)
  finish(o, s, v12, t, 12)
  finish(o, s, v13, t, 13)
  finish(o, s, v14, t, 14)
  finish(o, s, v15, t, 15)
