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
## `uint64`.
##
## This is the Nimony port. Unlike Nim 2's `std/bitops`, the procedures are
## **not** generic over `SomeInteger`: Nimony resolves the bitwise operators
## (`shl`, `shr`, `and`, ...) against concrete integer types, so a body written
## against an abstract `T` cannot be checked, and `when sizeof(x)` on a generic
## type is not usable either. The API is therefore provided as an overload set
## per unsigned width, which keeps every operation total and backend-neutral
## (the loop-based scans below lower cleanly to C, LLVM and JS alike). Signed
## and `int`/`uint` overloads can be layered on top later.
##
## Bit indices are 0-based and, for the rotate procedures, the shift amount is
## taken modulo the bit width so any amount is well defined.

# `countBits32` / `countBits64` come from system (`include "system/countbits_impl"`).

# --- internal scan cores (callers guarantee `x != 0`) ---

func trailingZeros32(x: uint32): int {.inline.} =
  var v = x
  result = 0
  while (v and 1'u32) == 0'u32:
    v = v shr 1
    inc result

func trailingZeros64(x: uint64): int {.inline.} =
  var v = x
  result = 0
  while (v and 1'u64) == 0'u64:
    v = v shr 1
    inc result

func leadingZeros32(x: uint32): int {.inline.} =
  var v = x
  result = 0
  while (v and 0x8000_0000'u32) == 0'u32:
    v = v shl 1
    inc result

func leadingZeros64(x: uint64): int {.inline.} =
  var v = x
  result = 0
  while (v and 0x8000_0000_0000_0000'u64) == 0'u64:
    v = v shl 1
    inc result

# --- countSetBits / popcount ---

func countSetBits*(x: uint8): int {.inline.} =
  ## Counts the set bits in `x` (the Hamming weight / population count).
  countBits32(x.uint32)
func countSetBits*(x: uint16): int {.inline.} =
  countBits32(x.uint32)
func countSetBits*(x: uint32): int {.inline.} =
  countBits32(x)
func countSetBits*(x: uint64): int {.inline.} =
  countBits64(x)

func popcount*(x: uint8): int {.inline.} =
  ## Alias for `countSetBits`.
  countSetBits(x)
func popcount*(x: uint16): int {.inline.} = countSetBits(x)
func popcount*(x: uint32): int {.inline.} = countSetBits(x)
func popcount*(x: uint64): int {.inline.} = countSetBits(x)

# --- parityBits ---

func parityBits*(x: uint8): int {.inline.} =
  ## Returns `1` when `x` has an odd number of set bits, otherwise `0`.
  countSetBits(x) and 1
func parityBits*(x: uint16): int {.inline.} = countSetBits(x) and 1
func parityBits*(x: uint32): int {.inline.} = countSetBits(x) and 1
func parityBits*(x: uint64): int {.inline.} = countSetBits(x) and 1

# --- firstSetBit ---

func firstSetBit*(x: uint8): int {.inline.} =
  ## Returns the 1-based index of the least-significant set bit of `x`.
  ## Returns `0` when `x` is `0`.
  if x == 0'u8: result = 0
  else: result = trailingZeros32(x.uint32) + 1
func firstSetBit*(x: uint16): int {.inline.} =
  if x == 0'u16: result = 0
  else: result = trailingZeros32(x.uint32) + 1
func firstSetBit*(x: uint32): int {.inline.} =
  if x == 0'u32: result = 0
  else: result = trailingZeros32(x) + 1
func firstSetBit*(x: uint64): int {.inline.} =
  if x == 0'u64: result = 0
  else: result = trailingZeros64(x) + 1

# --- trailingZeroBits ---

func trailingZeroBits*(x: uint8): int {.inline.} =
  ## Returns the number of trailing zero bits of `x`, or the bit width (`8`)
  ## when `x` is `0`.
  if x == 0'u8: result = 8
  else: result = trailingZeros32(x.uint32)
func trailingZeroBits*(x: uint16): int {.inline.} =
  if x == 0'u16: result = 16
  else: result = trailingZeros32(x.uint32)
func trailingZeroBits*(x: uint32): int {.inline.} =
  if x == 0'u32: result = 32
  else: result = trailingZeros32(x)
func trailingZeroBits*(x: uint64): int {.inline.} =
  if x == 0'u64: result = 64
  else: result = trailingZeros64(x)

# --- leadingZeroBits ---

func leadingZeroBits*(x: uint8): int {.inline.} =
  ## Returns the number of leading (most-significant) zero bits of `x`, or the
  ## bit width (`8`) when `x` is `0`.
  if x == 0'u8: result = 8
  else: result = leadingZeros32(x.uint32) - 24
func leadingZeroBits*(x: uint16): int {.inline.} =
  if x == 0'u16: result = 16
  else: result = leadingZeros32(x.uint32) - 16
func leadingZeroBits*(x: uint32): int {.inline.} =
  if x == 0'u32: result = 32
  else: result = leadingZeros32(x)
func leadingZeroBits*(x: uint64): int {.inline.} =
  if x == 0'u64: result = 64
  else: result = leadingZeros64(x)

# --- bitand / bitor / bitxor / bitnot ---

func bitand*(x, y: uint8): uint8 {.inline.} =
  ## Computes the `and` of `x` and `y`.
  x and y
func bitand*(x, y: uint16): uint16 {.inline.} = x and y
func bitand*(x, y: uint32): uint32 {.inline.} = x and y
func bitand*(x, y: uint64): uint64 {.inline.} = x and y

func bitor*(x, y: uint8): uint8 {.inline.} =
  ## Computes the `or` of `x` and `y`.
  x or y
func bitor*(x, y: uint16): uint16 {.inline.} = x or y
func bitor*(x, y: uint32): uint32 {.inline.} = x or y
func bitor*(x, y: uint64): uint64 {.inline.} = x or y

func bitxor*(x, y: uint8): uint8 {.inline.} =
  ## Computes the `xor` of `x` and `y`.
  x xor y
func bitxor*(x, y: uint16): uint16 {.inline.} = x xor y
func bitxor*(x, y: uint32): uint32 {.inline.} = x xor y
func bitxor*(x, y: uint64): uint64 {.inline.} = x xor y

func bitnot*(x: uint8): uint8 {.inline.} =
  ## Computes the bitwise complement of `x`.
  not x
func bitnot*(x: uint16): uint16 {.inline.} = not x
func bitnot*(x: uint32): uint32 {.inline.} = not x
func bitnot*(x: uint64): uint64 {.inline.} = not x

# --- rotateLeftBits / rotateRightBits ---

func rotateLeftBits*(v: uint8, amount: int): uint8 {.inline.} =
  ## Rotates the bits of `v` left by `amount` positions (taken modulo `8`).
  let a = amount and 7
  result = (v shl a) or (v shr ((8 - a) and 7))
func rotateLeftBits*(v: uint16, amount: int): uint16 {.inline.} =
  let a = amount and 15
  result = (v shl a) or (v shr ((16 - a) and 15))
func rotateLeftBits*(v: uint32, amount: int): uint32 {.inline.} =
  let a = amount and 31
  result = (v shl a) or (v shr ((32 - a) and 31))
func rotateLeftBits*(v: uint64, amount: int): uint64 {.inline.} =
  let a = amount and 63
  result = (v shl a) or (v shr ((64 - a) and 63))

func rotateRightBits*(v: uint8, amount: int): uint8 {.inline.} =
  ## Rotates the bits of `v` right by `amount` positions (taken modulo `8`).
  let a = amount and 7
  result = (v shr a) or (v shl ((8 - a) and 7))
func rotateRightBits*(v: uint16, amount: int): uint16 {.inline.} =
  let a = amount and 15
  result = (v shr a) or (v shl ((16 - a) and 15))
func rotateRightBits*(v: uint32, amount: int): uint32 {.inline.} =
  let a = amount and 31
  result = (v shr a) or (v shl ((32 - a) and 31))
func rotateRightBits*(v: uint64, amount: int): uint64 {.inline.} =
  let a = amount and 63
  result = (v shr a) or (v shl ((64 - a) and 63))

# --- setBit / clearBit / flipBit / testBit ---

func setBit*(v: var uint8, bit: int) {.inline.} =
  ## Sets the bit at position `bit` of `v` to `1`.
  v = v or (1'u8 shl bit)
func setBit*(v: var uint16, bit: int) {.inline.} = v = v or (1'u16 shl bit)
func setBit*(v: var uint32, bit: int) {.inline.} = v = v or (1'u32 shl bit)
func setBit*(v: var uint64, bit: int) {.inline.} = v = v or (1'u64 shl bit)

func clearBit*(v: var uint8, bit: int) {.inline.} =
  ## Clears the bit at position `bit` of `v` to `0`.
  v = v and not (1'u8 shl bit)
func clearBit*(v: var uint16, bit: int) {.inline.} = v = v and not (1'u16 shl bit)
func clearBit*(v: var uint32, bit: int) {.inline.} = v = v and not (1'u32 shl bit)
func clearBit*(v: var uint64, bit: int) {.inline.} = v = v and not (1'u64 shl bit)

func flipBit*(v: var uint8, bit: int) {.inline.} =
  ## Toggles the bit at position `bit` of `v`.
  v = v xor (1'u8 shl bit)
func flipBit*(v: var uint16, bit: int) {.inline.} = v = v xor (1'u16 shl bit)
func flipBit*(v: var uint32, bit: int) {.inline.} = v = v xor (1'u32 shl bit)
func flipBit*(v: var uint64, bit: int) {.inline.} = v = v xor (1'u64 shl bit)

func testBit*(v: uint8, bit: int): bool {.inline.} =
  ## Returns `true` when the bit at position `bit` of `v` is set.
  (v and (1'u8 shl bit)) != 0'u8
func testBit*(v: uint16, bit: int): bool {.inline.} = (v and (1'u16 shl bit)) != 0'u16
func testBit*(v: uint32, bit: int): bool {.inline.} = (v and (1'u32 shl bit)) != 0'u32
func testBit*(v: uint64, bit: int): bool {.inline.} = (v and (1'u64 shl bit)) != 0'u64
