# `div` / `mod` by a constant power of two.
#
# Native-relevant by nature: under `nimony c` the C compiler owns strength
# reduction, so only a backend that does its own can get this wrong — and the
# ways to get it wrong are quiet. `asr` alone rounds toward MINUS INFINITY, so a
# signed `-1 div 2` comes out -1 instead of 0; a mask gives `-3 mod 2 == 1`
# instead of -1; and a divisor spelled `sizeof(T)` rather than `4` must reduce
# too or the lowering never fires where it matters.
#
# Every value below is folded into a per-group checksum instead of printed: the
# point is that the two backends agree on several hundred results, and a golden
# file of several hundred lines is not a better way to say so. A checksum that
# is wrong names the group, which is as far as any of these would need bisecting.

import std / syncio

var acc = 0'u64

proc note(v: int64) =
  acc = acc * 1000003'u64 + cast[uint64](v)

proc report(name: string) =
  echo name, " ", acc
  acc = 0'u64

proc chk64s(a: int64) =
  for k in [1, 2, 3, 8, 31, 32, 33, 62]:
    let d = 1'i64 shl k
    note a div d
    note a mod d
    note a div (d + 1)                 # the non-power-of-two neighbour is untouched
    note a mod (d + 1)

proc chk64u(a: uint64) =
  for k in [1, 2, 3, 8, 31, 32, 33, 62]:
    let d = 1'u64 shl k
    note cast[int64](a div d)
    note cast[int64](a mod d)
    note cast[int64](a div (d + 1'u64))
    note cast[int64](a mod (d + 1'u64))

proc chk32s(a: int32) =
  for k in [1, 4, 30]:
    let d = 1'i32 shl k
    note int64(a div d)
    note int64(a mod d)

proc chk32u(a: uint32) =
  for k in [1, 4, 30]:
    let d = 1'u32 shl k
    note int64(a div d)
    note int64(a mod d)

proc chk8s(a: int8) =
  for k in [1, 2, 6]:
    let d = 1'i8 shl k
    note int64(a div d)
    note int64(a mod d)

proc chk8u(a: uint8) =
  for k in [1, 2, 6]:
    let d = 1'u8 shl k
    note int64(a div d)
    note int64(a mod d)

type Tok = distinct uint32

proc byteOffset(hi, lo: uint): int =
  ## The shape the lowering was written for: `nifcore.leaveScope` divides a byte
  ## distance by `sizeof(NifToken)`, and the divisor is a `sizeof` expression, not
  ## a literal — it only reduces if the constant folder is consulted.
  int((hi - lo) div sizeof(Tok).uint)

proc main =
  for a in [0'i64, 1'i64, -1'i64, 2'i64, -2'i64, 3'i64, -3'i64, 7'i64, -7'i64,
            8'i64, -8'i64, 9'i64, -9'i64, 255'i64, -255'i64,
            65536'i64, -65536'i64, 1234567891011'i64, -1234567891011'i64,
            high(int64), low(int64) + 1'i64, high(int64) - 1'i64]:
    chk64s a
  report "s64"
  for a in [0'u64, 1'u64, 2'u64, 3'u64, 7'u64, 8'u64, 9'u64, 255'u64,
            65536'u64, 1234567891011'u64,
            high(uint64), high(uint64) - 1'u64, 1'u64 shl 63]:
    chk64u a
  report "u64"
  for a in [0'i32, 1'i32, -1'i32, 5'i32, -5'i32, 1024'i32, -1024'i32,
            high(int32), low(int32) + 1'i32]:
    chk32s a
  report "s32"
  for a in [0'u32, 1'u32, 5'u32, 1024'u32, high(uint32), high(uint32) - 1'u32]:
    chk32u a
  report "u32"
  for a in [0'i8, 1'i8, -1'i8, 5'i8, -5'i8, 127'i8, -127'i8]:
    chk8s a
  report "s8"
  for a in [0'u8, 1'u8, 5'u8, 127'u8, 255'u8]:
    chk8u a
  report "u8"
  # A divisor that only a variable can carry: never reducible, so the ordinary
  # `sdiv`/`udiv` path stays covered by the same test.
  var v = 8'i64
  note (-9'i64) div v
  note (-9'i64) mod v
  v = 1
  note (-9'i64) div v
  note (-9'i64) mod v
  report "var"
  note int64(byteOffset(4096'u, 4064'u))
  note int64(byteOffset(4096'u, 4096'u))
  report "sizeof"

main()
