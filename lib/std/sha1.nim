## SHA-1 (Secure Hash Algorithm 1), 160-bit message digest.
## Minimal port from Nim's `checksums/sha1` — only the bits used by
## `nifchecksums` / `nifindexes` are provided. Byte-swap helpers are
## inlined since Nimony doesn't have `std/endians`.

import std/syncio

const Sha1DigestSize = 20

type
  Sha1Digest* = array[0 .. Sha1DigestSize - 1, uint8]
  SecureHash* = distinct Sha1Digest

  Sha1State* = object
    count: int
    state: array[5, uint32]
    buf:   array[64, byte]

proc newSha1State*(): Sha1State =
  result = default(Sha1State)
  result.state[0] = 0x67452301'u32
  result.state[1] = 0xEFCDAB89'u32
  result.state[2] = 0x98BADCFE'u32
  result.state[3] = 0x10325476'u32
  result.state[4] = 0xC3D2E1F0'u32

template ror27(val: uint32): uint32 = (val shr 27) or (val shl  5)
template ror2 (val: uint32): uint32 = (val shr  2) or (val shl 30)
template ror31(val: uint32): uint32 = (val shr 31) or (val shl  1)

proc loadBE32(p: ptr byte): uint32 {.inline.} =
  let q = cast[ptr UncheckedArray[byte]](p)
  (uint32(q[0]) shl 24) or (uint32(q[1]) shl 16) or
    (uint32(q[2]) shl 8) or uint32(q[3])

proc storeBE32(p: ptr byte; v: uint32) {.inline.} =
  let q = cast[ptr UncheckedArray[byte]](p)
  q[0] = byte(v shr 24)
  q[1] = byte(v shr 16)
  q[2] = byte(v shr  8)
  q[3] = byte(v)

proc storeBE64(p: ptr byte; v: uint64) {.inline.} =
  let q = cast[ptr UncheckedArray[byte]](p)
  q[0] = byte(v shr 56)
  q[1] = byte(v shr 48)
  q[2] = byte(v shr 40)
  q[3] = byte(v shr 32)
  q[4] = byte(v shr 24)
  q[5] = byte(v shr 16)
  q[6] = byte(v shr  8)
  q[7] = byte(v)

proc transform(ctx: var Sha1State) =
  var w: array[80, uint32] = default(array[80, uint32])
  var a = ctx.state[0]
  var b = ctx.state[1]
  var c = ctx.state[2]
  var d = ctx.state[3]
  var e = ctx.state[4]
  var t = 0

  while t < 16:
    w[t] = loadBE32(addr ctx.buf[t * 4])
    let ne = ror27(a) + e + w[t] + (d xor (b and (c xor d))) + 0x5A827999'u32
    e = d; d = c; c = ror2(b); b = a; a = ne
    inc t

  while t < 20:
    w[t] = ror31(w[t-3] xor w[t-8] xor w[t-14] xor w[t-16])
    let ne = ror27(a) + e + w[t] + (d xor (b and (c xor d))) + 0x5A827999'u32
    e = d; d = c; c = ror2(b); b = a; a = ne
    inc t

  while t < 40:
    w[t] = ror31(w[t-3] xor w[t-8] xor w[t-14] xor w[t-16])
    let ne = ror27(a) + e + w[t] + (b xor c xor d) + 0x6ED9EBA1'u32
    e = d; d = c; c = ror2(b); b = a; a = ne
    inc t

  while t < 60:
    w[t] = ror31(w[t-3] xor w[t-8] xor w[t-14] xor w[t-16])
    let ne = ror27(a) + e + w[t] + ((b and c) or (d and (b or c))) + 0x8F1BBCDC'u32
    e = d; d = c; c = ror2(b); b = a; a = ne
    inc t

  while t < 80:
    w[t] = ror31(w[t-3] xor w[t-8] xor w[t-14] xor w[t-16])
    let ne = ror27(a) + e + w[t] + (b xor c xor d) + 0xCA62C1D6'u32
    e = d; d = c; c = ror2(b); b = a; a = ne
    inc t

  ctx.state[0] += a
  ctx.state[1] += b
  ctx.state[2] += c
  ctx.state[3] += d
  ctx.state[4] += e

proc update*(ctx: var Sha1State; data: openArray[char]) =
  var i = ctx.count mod 64
  var j = 0
  var len = data.len
  if len > 64 - i:
    for k in 0 ..< (64 - i):
      ctx.buf[i + k] = byte(data[j + k])
    len -= 64 - i
    j += 64 - i
    transform(ctx)
    i = 0
  while len >= 64:
    for k in 0 ..< 64:
      ctx.buf[k] = byte(data[j + k])
    len -= 64
    j += 64
    transform(ctx)
  while len > 0:
    dec len
    ctx.buf[i] = byte(data[j])
    inc i
    inc j
    if i == 64:
      transform(ctx)
      i = 0
  ctx.count += data.len

proc finalize*(ctx: var Sha1State): Sha1Digest =
  result = default(Sha1Digest)
  let cnt = uint64(ctx.count * 8)
  update(ctx, "\x80")
  while (ctx.count mod 64) != (64 - 8):
    update(ctx, "\x00")
  var tail: array[8, byte] = default(array[8, byte])
  storeBE64(addr tail[0], cnt)
  var tailChars: array[8, char] = default(array[8, char])
  for k in 0 ..< 8: tailChars[k] = char(tail[k])
  update(ctx, tailChars)
  for k in 0 ..< 5:
    storeBE32(addr result[k * 4], ctx.state[k])

const HexChars = "0123456789ABCDEF"

proc `$`*(self: SecureHash): string =
  result = newString(Sha1DigestSize * 2)
  var digest = Sha1Digest(self)
  for i in 0 ..< Sha1DigestSize:
    result[i * 2]     = HexChars[int(digest[i] shr 4)]
    result[i * 2 + 1] = HexChars[int(digest[i] and 0x0F)]
