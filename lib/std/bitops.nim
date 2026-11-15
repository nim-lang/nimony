#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements a series of low-level bit manipulation procedures
## over the fixed-width unsigned integers `uint8`, `uint16`, `uint32` and
## `uint64` -- or any type that satifies the `BitInteger` concept. 
##
## The procedures are generic and constrained by the `BitInteger` concept
## below, which lists exactly the bitwise and shift operators the algorithms
## rely on. The bodies are written against those operations only (plus a
## runtime `sizeof` to recover the bit width), so they lower cleanly to C,
## LLVM and JS alike, and any type providing the operator set is accepted.
##
## Bit indices are 0-based and, for the rotate procedures, the shift amount is
## taken modulo the bit width so any amount is well defined.

type
  BitInteger* = concept ## Fixed-width unsigned integers usable with the bit
                        ## manipulation procedures below. Satisfied by `uint8`,
                        ## `uint16`, `uint32` and `uint64`.
    func `and`(x, y: Self): Self
    func `or`(x, y: Self): Self
    func `xor`(x, y: Self): Self
    func `not`(x: Self): Self
    func `shl`(x: Self, y: int): Self
    func `shr`(x: Self, y: int): Self
    func `-`(x, y: Self): Self
    func `==`(x, y: Self): bool

template bitWidth[T: BitInteger](x: T): int = sizeof(x) * 8

# --- countSetBits / popcount ---

func countSetBits*[T: BitInteger](x: T): int {.inline.} =
  ## Counts the set bits in `x` (the Hamming weight / population count).
  # Kernighan's algorithm: clears the lowest set bit each iteration, so it
  # loops exactly `popcount` times and needs no width-specific table.
  var v = x
  result = 0
  while v != T(0):
    v = v and (v - T(1))
    inc result

func popcount*[T: BitInteger](x: T): int {.inline.} =
  ## Alias for `countSetBits`.
  countSetBits(x)

# --- parityBits ---

func parityBits*[T: BitInteger](x: T): int {.inline.} =
  ## Returns `1` when `x` has an odd number of set bits, otherwise `0`.
  countSetBits(x) and 1

# --- firstSetBit ---

func firstSetBit*[T: BitInteger](x: T): int {.inline.} =
  ## Returns the 1-based index of the least-significant set bit of `x`.
  ## Returns `0` when `x` is `0`.
  if x == T(0): result = 0
  else:
    var v = x
    result = 1
    while (v and T(1)) == T(0):
      v = v shr 1
      inc result

# --- trailingZeroBits ---

func trailingZeroBits*[T: BitInteger](x: T): int {.inline.} =
  ## Returns the number of trailing zero bits of `x`, or the bit width when
  ## `x` is `0`.
  if x == T(0): result = bitWidth(x)
  else:
    var v = x
    result = 0
    while (v and T(1)) == T(0):
      v = v shr 1
      inc result

# --- leadingZeroBits ---

func leadingZeroBits*[T: BitInteger](x: T): int {.inline.} =
  ## Returns the number of leading (most-significant) zero bits of `x`, or the
  ## bit width when `x` is `0`.
  if x == T(0): result = bitWidth(x)
  else:
    let topBit = T(1) shl (bitWidth(x) - 1)
    var v = x
    result = 0
    while (v and topBit) == T(0):
      v = v shl 1
      inc result

# --- bitand / bitor / bitxor / bitnot ---

func bitand*[T: BitInteger](x, y: T): T {.inline.} =
  ## Computes the `and` of `x` and `y`.
  x and y

func bitor*[T: BitInteger](x, y: T): T {.inline.} =
  ## Computes the `or` of `x` and `y`.
  x or y

func bitxor*[T: BitInteger](x, y: T): T {.inline.} =
  ## Computes the `xor` of `x` and `y`.
  x xor y

func bitnot*[T: BitInteger](x: T): T {.inline.} =
  ## Computes the bitwise complement of `x`.
  not x

# --- rotateLeftBits / rotateRightBits ---

func rotateLeftBits*[T: BitInteger](v: T, amount: int): T {.inline.} =
  ## Rotates the bits of `v` left by `amount` positions (taken modulo the
  ## bit width).
  let w = bitWidth(v)
  let a = amount and (w - 1)
  result = (v shl a) or (v shr ((w - a) and (w - 1)))

func rotateRightBits*[T: BitInteger](v: T, amount: int): T {.inline.} =
  ## Rotates the bits of `v` right by `amount` positions (taken modulo the
  ## bit width).
  let w = bitWidth(v)
  let a = amount and (w - 1)
  result = (v shr a) or (v shl ((w - a) and (w - 1)))

# --- setBit / clearBit / flipBit / testBit ---

func setBit*[T: BitInteger](v: var T, bit: int) {.inline.} =
  ## Sets the bit at position `bit` of `v` to `1`.
  v = v or (T(1) shl bit)

func clearBit*[T: BitInteger](v: var T, bit: int) {.inline.} =
  ## Clears the bit at position `bit` of `v` to `0`.
  v = v and not (T(1) shl bit)

func flipBit*[T: BitInteger](v: var T, bit: int) {.inline.} =
  ## Toggles the bit at position `bit` of `v`.
  v = v xor (T(1) shl bit)

func testBit*[T: BitInteger](v: T, bit: int): bool {.inline.} =
  ## Returns `true` when the bit at position `bit` of `v` is set.
  (v and (T(1) shl bit)) != T(0)

# --- byteSwap ---

const HardwareBswap = not defined(wasm32) and
                      (defined(amd64) or defined(arm64) or not defined(nimNativeBackend))
  ## Where the `Bswap` intrinsic has a lowering: every C/LLVM target (a GCC
  ## builtin) and the native targets its row lists (`bswap`, `rev`). The web
  ## back end and the 32-bit native targets swap with shifts instead.

when HardwareBswap:
  func bswapIns(x: uint16): uint16 {.intrinsic: "Bswap".}
  func bswapIns(x: uint32): uint32 {.intrinsic: "Bswap".}
  func bswapIns(x: uint64): uint64 {.intrinsic: "Bswap".}
else:
  func bswapIns(x: uint16): uint16 {.inline.} = (x shl 8) or (x shr 8)
  func bswapIns(x: uint32): uint32 {.inline.} =
    let y = ((x and 0x00FF00FF'u32) shl 8) or ((x shr 8) and 0x00FF00FF'u32)
    result = (y shl 16) or (y shr 16)
  func bswapIns(x: uint64): uint64 {.inline.} =
    let lo = bswapIns(uint32(x and 0xFFFF_FFFF'u64))
    let hi = bswapIns(uint32(x shr 32))
    result = (uint64(lo) shl 32) or uint64(hi)

func byteSwap*(x: uint8): uint8 {.inline.} =
  ## Reverses the byte order of `x`; a single byte is its own swap.
  x

func byteSwap*(x: uint16): uint16 {.inline.} =
  ## Reverses the byte order of `x` (`0x1122` becomes `0x2211`). One
  ## instruction where the target has it (`bswap`/`rev`), shifts elsewhere.
  bswapIns(x)

func byteSwap*(x: uint32): uint32 {.inline.} =
  ## Reverses the byte order of `x` (`0x11223344` becomes `0x44332211`).
  bswapIns(x)

func byteSwap*(x: uint64): uint64 {.inline.} =
  ## Reverses the byte order of `x`.
  bswapIns(x)

func byteSwap*(x: int16): int16 {.inline.} = cast[int16](bswapIns(cast[uint16](x)))
  ## Reverses the byte order of `x`.
func byteSwap*(x: int32): int32 {.inline.} = cast[int32](bswapIns(cast[uint32](x)))
  ## Reverses the byte order of `x`.
func byteSwap*(x: int64): int64 {.inline.} = cast[int64](bswapIns(cast[uint64](x)))
  ## Reverses the byte order of `x`.
