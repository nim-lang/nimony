# 32-bit locals in an AArch64 `{.assembler.}` body: a `uint32` local is a `w`
# register, so `add` wraps at 2^32 and `ror` rotates within 32 bits. Checked
# against the portable `rotr` on the same inputs.
import std/syncio
import std/bitops

type Words32 = ptr UncheckedArray[uint32]

proc add32(d: var uint32; s: uint32) {.instruction: "add".}
proc xor32(d: var uint32; s: uint32) {.instruction: "xor".}
proc ror32(x: uint32; n: int): uint32 {.instruction: "ror".}
proc ror64(x: uint64; n: int): uint64 {.instruction: "ror".}

proc mix(p {.register: "x0".}: Words32) {.assembler.} =
  var a {.register: "x1".}: uint32
  var b {.register: "x2".}: uint32
  a = p[0]
  b = p[1]
  add32(a, b)
  xor32(b, a)
  b = ror32(b, 25)
  p[0] = a
  p[1] = b

proc rot64(x {.register: "x0".}: uint64; n {.register: "x1".}: int): uint64 {.assembler.} =
  result = ror64(x, n)

proc main =
  var w = [0xFFFF_FFF0'u32, 0x0000_0020'u32]
  mix(cast[Words32](addr w))
  let a = 0xFFFF_FFF0'u32 + 0x20'u32
  let b = rotateRightBits(0x20'u32 xor a, 25)
  echo w[0] == a, " ", w[1] == b, " ", w[0], " ", w[1]
  echo rot64(0x8000_0000_0000_0001'u64, 1), " ", rot64(1'u64, 63)
main()
