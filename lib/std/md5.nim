#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the `MD5 <https://en.wikipedia.org/wiki/MD5>`_ message
## digest algorithm (RFC 1321).
##
## .. warning:: MD5 is considered broken for cryptographic use (collisions are
##   feasible). Use it only for checksums / non-security purposes.
##
## This is the Nimony port. It exposes the one-shot API (`toMD5`, `getMD5`, and
## `$` on a digest); the incremental `MD5Context` (`md5Init`/`md5Update`/
## `md5Final`) of Nim 2's `std/md5` is left for a follow-up.

type
  MD5Digest* = array[16, uint8]
    ## Represents an MD5 digest (128 bits / 16 bytes).

const
  Shift: array[64, int] = [
    7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
    5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
    4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
    6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21]

  K: array[64, uint32] = [
    0xd76aa478'u32, 0xe8c7b756'u32, 0x242070db'u32, 0xc1bdceee'u32,
    0xf57c0faf'u32, 0x4787c62a'u32, 0xa8304613'u32, 0xfd469501'u32,
    0x698098d8'u32, 0x8b44f7af'u32, 0xffff5bb1'u32, 0x895cd7be'u32,
    0x6b901122'u32, 0xfd987193'u32, 0xa679438e'u32, 0x49b40821'u32,
    0xf61e2562'u32, 0xc040b340'u32, 0x265e5a51'u32, 0xe9b6c7aa'u32,
    0xd62f105d'u32, 0x02441453'u32, 0xd8a1e681'u32, 0xe7d3fbc8'u32,
    0x21e1cde6'u32, 0xc33707d6'u32, 0xf4d50d87'u32, 0x455a14ed'u32,
    0xa9e3e905'u32, 0xfcefa3f8'u32, 0x676f02d9'u32, 0x8d2a4c8a'u32,
    0xfffa3942'u32, 0x8771f681'u32, 0x6d9d6122'u32, 0xfde5380c'u32,
    0xa4beea44'u32, 0x4bdecfa9'u32, 0xf6bb4b60'u32, 0xbebfbc70'u32,
    0x289b7ec6'u32, 0xeaa127fa'u32, 0xd4ef3085'u32, 0x04881d05'u32,
    0xd9d4d039'u32, 0xe6db99e5'u32, 0x1fa27cf8'u32, 0xc4ac5665'u32,
    0xf4292244'u32, 0x432aff97'u32, 0xab9423a7'u32, 0xfc93a039'u32,
    0x655b59c3'u32, 0x8f0ccc92'u32, 0xffeff47d'u32, 0x85845dd1'u32,
    0x6fa87e4f'u32, 0xfe2ce6e0'u32, 0xa3014314'u32, 0x4e0811a1'u32,
    0xf7537e82'u32, 0xbd3af235'u32, 0x2ad7d2bb'u32, 0xeb86d391'u32]

func rotl(x: uint32, c: int): uint32 {.inline.} =
  (x shl c) or (x shr (32 - c))

func toMD5*(msg: string): MD5Digest =
  ## Computes the MD5 digest of `msg`.
  var a0 = 0x67452301'u32
  var b0 = 0xefcdab89'u32
  var c0 = 0x98badcfe'u32
  var d0 = 0x10325476'u32

  # Build the padded message: original bytes, then 0x80, then zeros until the
  # length is 56 mod 64, then the original bit length as little-endian uint64.
  let origLen = msg.len
  var data = newSeq[uint8](0)
  for i in 0 ..< origLen:
    data.add cast[uint8](msg[i])
  data.add 0x80'u8
  while data.len mod 64 != 56:
    data.add 0'u8
  let bitLen = uint64(origLen) * 8'u64
  for i in 0 ..< 8:
    data.add uint8((bitLen shr (8'u64 * uint64(i))) and 0xFF'u64)

  let nblocks = data.len div 64
  for blk in 0 ..< nblocks:
    let base = blk * 64
    var M = default(array[16, uint32])
    for j in 0 ..< 16:
      let o = base + j * 4
      M[j] = uint32(data[o]) or (uint32(data[o+1]) shl 8) or
             (uint32(data[o+2]) shl 16) or (uint32(data[o+3]) shl 24)

    var A = a0
    var B = b0
    var C = c0
    var D = d0
    for i in 0 ..< 64:
      var f = 0'u32
      var g = 0
      if i < 16:
        f = (B and C) or ((not B) and D)
        g = i
      elif i < 32:
        f = (D and B) or ((not D) and C)
        g = (5 * i + 1) mod 16
      elif i < 48:
        f = B xor C xor D
        g = (3 * i + 5) mod 16
      else:
        f = C xor (B or (not D))
        g = (7 * i) mod 16
      f = f + A + K[i] + M[g]
      A = D
      D = C
      C = B
      B = B + rotl(f, Shift[i])
    a0 = a0 + A
    b0 = b0 + B
    c0 = c0 + C
    d0 = d0 + D

  result = default(MD5Digest)
  for i in 0 ..< 4:
    let sh = 8'u32 * uint32(i)
    result[i]      = uint8((a0 shr sh) and 0xFF'u32)
    result[i + 4]  = uint8((b0 shr sh) and 0xFF'u32)
    result[i + 8]  = uint8((c0 shr sh) and 0xFF'u32)
    result[i + 12] = uint8((d0 shr sh) and 0xFF'u32)

func `$`*(d: MD5Digest): string =
  ## Converts an `MD5Digest` to its lowercase hexadecimal string.
  const hex = "0123456789abcdef"
  result = ""
  for b in d:
    result.add hex[int(b shr 4)]
    result.add hex[int(b and 0x0F'u8)]

func getMD5*(s: string): string =
  ## Computes the MD5 digest of `s` and returns it as a lowercase hex string.
  runnableExamples:
    doAssert getMD5("abc") == "900150983cd24fb0d6963f7d28e17f72"
  result = $toMD5(s)
