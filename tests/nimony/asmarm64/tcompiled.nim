# The AArch64 rows that are legal in COMPILED code, where the allocator places
# the operands: `ror` (literal and register counts, 32 and 64 bits), `mul3`
# and `umulh`. Checked against portable arithmetic.
import std/syncio
import std/bitops

proc ror32(x: uint32; n: int): uint32 {.instruction: "ror".}
proc ror64(x: uint64; n: int): uint64 {.instruction: "ror".}
proc mulLo(a, b: uint64): uint64 {.instruction: "mul3".}
proc mulHi(a, b: uint64): uint64 {.instruction: "umulh".}

proc hiPortable(a, b: uint64): uint64 =
  let al = a and 0xFFFF_FFFF'u64
  let ah = a shr 32
  let bl = b and 0xFFFF_FFFF'u64
  let bh = b shr 32
  let ll = al * bl
  let lh = al * bh
  let hl = ah * bl
  let hh = ah * bh
  let mid = (ll shr 32) + (lh and 0xFFFF_FFFF'u64) + (hl and 0xFFFF_FFFF'u64)
  result = hh + (lh shr 32) + (hl shr 32) + (mid shr 32)

proc main =
  var bad = 0
  var x = 0x9E37_79B9'u32
  var y = 0x9E37_79B9_7F4A_7C15'u64
  for n in 0 ..< 32:
    if ror32(x, n) != rotateRightBits(x, n): inc bad
    if ror32(x, 7) != rotateRightBits(x, 7): inc bad
  for n in 0 ..< 64:
    if ror64(y, n) != rotateRightBits(y, n): inc bad
  var a = 0xFFFF_FFFF_FFFF_FFFF'u64
  for i in 0 ..< 20:
    let b = y xor (uint64(i) * 0x1234_5678_9ABC_DEF1'u64)
    if mulLo(a, b) != a * b: inc bad
    if mulHi(a, b) != hiPortable(a, b): inc bad
    a = a - b
  echo "mismatches: ", bad
main()
