# `div` / `mod` by a constant that is NOT a power of two.
#
# Native-relevant by nature: arkham turns such a division into a multiplication
# by the divisor's reciprocal (`core/divmagic.nim` in nativenif), and the ways
# to get that wrong are quiet — a magic constant one off, a missing "add"
# fixup for the divisors whose magic needs 65 bits (7, 641, …), the signed high
# half derived from an unsigned `mul`, the round toward zero. The C backend
# leaves all of it to the C compiler, so under `nimony c` this is the oracle.
#
# The last group compares two quotients in ONE expression: arkham used to leave
# a quotient in rax as a temp, and the second division reloaded rax under it —
# `(a div 10) != (b div x)` compiled to `cmp rax, rax`.

import std / syncio

var acc {.threadvar.}: uint64

proc note(v: int64) =
  acc = acc * 1000003'u64 + cast[uint64](v)

proc noteU(v: uint64) =
  acc = acc * 1000003'u64 + v

proc report(name: string) =
  echo name, " ", acc
  acc = 0'u64

proc opaque[T](x: T): T {.noinline.} = x

proc chkSigned(a: int64) =
  note a div 3; note a mod 3
  note a div 7; note a mod 7
  note a div 10; note a mod 10
  note a div -10; note a mod -10
  note a div -7; note a mod -7
  note a div 641; note a mod 641
  note a div 1000; note a mod 1000
  note a div 6700417; note a mod -6700417
  note a div 0x1_0000_0001; note a mod 0x1_0000_0001
  note a div 0x7FFF_FFFF_FFFF_FFFF; note a mod 0x7FFF_FFFF_FFFF_FFFF

proc chkUnsigned(a: uint64) =
  noteU a div 3'u64; noteU a mod 3'u64
  noteU a div 7'u64; noteU a mod 7'u64
  noteU a div 10'u64; noteU a mod 10'u64
  noteU a div 641'u64; noteU a mod 641'u64
  noteU a div 1000'u64; noteU a mod 1000'u64
  noteU a div 0x1_0000_0001'u64; noteU a mod 0x1_0000_0001'u64
  noteU a div 0xFFFF_FFFF_FFFF_FFFB'u64; noteU a mod 0xFFFF_FFFF_FFFF_FFFB'u64

proc chkSameExpr(a: int64; b: uint64) =
  let d10 = opaque(10'i64)
  let ud10 = opaque(10'u64)
  note int64(a div 10 == a div d10)
  note int64(a mod 10 == a mod d10)
  note int64(b div 10'u64 == b div ud10)
  note int64(b mod 10'u64 == b mod ud10)
  note int64(a div 7 + a mod 7)

proc next(seed: var uint64): uint64 =
  seed = seed xor (seed shl 13)
  seed = seed xor (seed shr 7)
  seed = seed xor (seed shl 17)
  seed

var vals = @[0'u64, 1, 2, 3, 6, 7, 9, 10, 11, 99, 100, 101, 640, 641, 642,
             high(uint64), high(uint64) - 1,
             0x7FFF_FFFF_FFFF_FFFF'u64, 0x8000_0000_0000_0001'u64]
var seed = 0x9E3779B97F4A7C15'u64
for i in 0 ..< 200: vals.add next(seed)

for v in vals:
  let s = cast[int64](v)
  if s != low(int64): chkSigned(opaque(s))
  if s != low(int64): chkSigned(opaque(-s))
report "signed"
for v in vals: chkUnsigned(opaque(v))
report "unsigned"
for v in vals:
  let s = cast[int64](v)
  if s != low(int64): chkSameExpr(opaque(s), opaque(v))
report "sameexpr"
