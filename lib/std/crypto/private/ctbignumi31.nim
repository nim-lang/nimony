#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## BearSSL's constant-time "i31" big integers, translated line by line for
## the portable (non-`{.assembler.}`) path of `std/crypto/private/ctbignum`.
##
## Translated from BearSSL (https://www.bearssl.org/), commit
## 7bea48e5e850ab4cafbe68d3765cdaba13a86d6f (2026-04-06), files
## `src/inner.h` (NOT, MUX, EQ, NEQ, GT, GE, LT, BIT_LENGTH, MUL31,
## MUL31_lo, br_enc32be, br_i31_zero, br_rem, br_div), `src/codec/ccopy.c`,
## `src/int/i32_div32.c` and `src/int/i31_{add,sub,bitlen,decode,decred,
## encode,fmont,tmont,montmul,mulacc,muladd,ninv31,reduce,rshift,modpow2}.c`.
## Every statement carries the file and line it comes from; where Nimony
## cannot say the same thing the same way, the comment there says why.
##
## BearSSL is
##   Copyright (c) 2016 Thomas Pornin <pornin@bolet.org>
## and distributed under the MIT licence:
##
##   Permission is hereby granted, free of charge, to any person obtaining
##   a copy of this software and associated documentation files (the
##   "Software"), to deal in the Software without restriction, including
##   without limitation the rights to use, copy, modify, merge, publish,
##   distribute, sublicense, and/or sell copies of the Software, and to
##   permit persons to whom the Software is furnished to do so, subject to
##   the following conditions:
##
##   The above copyright notice and this permission notice shall be
##   included in all copies or substantial portions of the Software.
##
##   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
##   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
##   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
##   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
##   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
##   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
##   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
##   SOFTWARE.
##
## The representation is BearSSL's: a number is an array of `uint32` whose
## word 0 is the "encoded bit length" and whose words 1.. hold 31 bits each,
## little-endian. BearSSL's `uint32_t *` and `unsigned char *` parameters
## become `I31` and `Bytes`, unchecked pointers, because the C code passes
## overlapping windows of one stack buffer (`mq + 2 * fwlen` and the like)
## that a Nimony `openArray` cannot express without tripping the aliasing
## checks; `+!` is C's pointer addition.
##
## Not 1:1, everywhere: every mask that BearSSL builds from a 0/1 control
## (`-ctl` in MUX, `-EQ(u, bits)` in the window lookup) is passed through
## `ctOpaque` first, so that the C compiler behind `nimony c` cannot see it
## is all ones or zero and turn the masked select back into a branch: clang
## -O3 did exactly that to Nimony's earlier masked code (tests/ctgrind caught
## it). BearSSL relies on the compiler not doing so.

import std/volatile

type
  I31* = ptr UncheckedArray[uint32]
    ## BearSSL's `uint32_t *`: word 0 is the encoded bit length.
  Bytes* = ptr UncheckedArray[byte]
    ## BearSSL's `unsigned char *`.

func `+!`*(p: I31; k: int): I31 {.inline.} =
  ## C's `p + k` on a `uint32_t *`.
  result = cast[I31](addr p[k])

func `+!`*(p: Bytes; k: int): Bytes {.inline.} =
  ## C's `p + k` on an `unsigned char *`.
  result = cast[Bytes](addr p[k])

func ctOpaque*(x: uint32): uint32 {.inline.} =
  ## `x`, passed through a volatile store and load so that the C compiler
  ## cannot know its value. Not in BearSSL: a compiler that can see a mask is
  ## all ones or zero may turn `y xor (mask and (x xor y))` back into a
  ## branch, and clang -O3 did, in Nimony's earlier masked code (caught by
  ## tests/ctgrind).
  var v = 0'u32
  {.cast(noSideEffect).}:
    volatileStore(addr v, x)
    result = volatileLoad(addr v)

# ------------------------------------------------------- inner.h helpers ---

func NOT*(ctl: uint32): uint32 {.inline.} =
  ## Negate a boolean (inner.h:757-764).
  result = ctl xor 1'u32                                  # inner.h:763

func MUX*(ctl, x, y: uint32): uint32 {.inline.} =
  ## Multiplexer: `x` if `ctl == 1`, `y` if `ctl == 0` (inner.h:766-773).
  # inner.h:772 `y ^ (-ctl & (x ^ y))`; `-ctl` through ctOpaque (see the
  # module doc) and written `0 - ctl`, Nimony's unsigned negation
  result = y xor (ctOpaque(0'u32 - ctl) and (x xor y))

func EQ*(x, y: uint32): uint32 {.inline.} =
  ## 1 if `x == y`, 0 otherwise (inner.h:775-785).
  let q = x xor y                                         # inner.h:783
  result = NOT((q or (0'u32 - q)) shr 31)                 # inner.h:784

func NEQ*(x, y: uint32): uint32 {.inline.} =
  ## 1 if `x != y`, 0 otherwise (inner.h:787-797).
  let q = x xor y                                         # inner.h:795
  result = (q or (0'u32 - q)) shr 31                      # inner.h:796

func GT*(x, y: uint32): uint32 {.inline.} =
  ## 1 if `x > y`, 0 otherwise (inner.h:799-821).
  let z = y - x                                           # inner.h:819
  result = (z xor ((x xor y) and (x xor z))) shr 31       # inner.h:820

func GE*(x, y: uint32): uint32 {.inline.} =
  ## inner.h:826 `#define GE(x, y) NOT(GT(y, x))`
  result = NOT(GT(y, x))

func LT*(x, y: uint32): uint32 {.inline.} =
  ## inner.h:827 `#define LT(x, y) GT(y, x)`
  result = GT(y, x)

func BIT_LENGTH*(x: uint32): uint32 {.inline.} =
  ## The bit length of `x`, 0 to 32 (inner.h:910-926).
  var x = x
  var k = NEQ(x, 0'u32)                                   # inner.h:919
  var c = GT(x, 0xFFFF'u32)                               # inner.h:920
  x = MUX(c, x shr 16, x)
  k = k + (c shl 4)
  c = GT(x, 0x00FF'u32)                                   # inner.h:921
  x = MUX(c, x shr 8, x)
  k = k + (c shl 3)
  c = GT(x, 0x000F'u32)                                   # inner.h:922
  x = MUX(c, x shr 4, x)
  k = k + (c shl 2)
  c = GT(x, 0x0003'u32)                                   # inner.h:923
  x = MUX(c, x shr 2, x)
  k = k + (c shl 1)
  k = k + GT(x, 0x0001'u32)                               # inner.h:924
  result = k                                              # inner.h:925

func MUL31*(x, y: uint32): uint64 {.inline.} =
  ## inner.h:993 `((uint64_t)(x) * (uint64_t)(y))` (BR_CT_MUL31 unset)
  result = uint64(x) * uint64(y)

func MUL31_lo*(x, y: uint32): uint32 {.inline.} =
  ## inner.h:994 `(((uint32_t)(x) * (uint32_t)(y)) & 0x7FFFFFFF)`
  result = (x * y) and 0x7FFFFFFF'u32

proc br_enc32be*(dst: Bytes; x: uint32) {.inline.} =
  ## inner.h:557-571, the portable (not BR_BE_UNALIGNED) branch.
  let buf = dst                                           # inner.h:565
  buf[0] = byte((x shr 24) and 0xFF'u32)                  # inner.h:566
  buf[1] = byte((x shr 16) and 0xFF'u32)                  # inner.h:567
  buf[2] = byte((x shr 8) and 0xFF'u32)                   # inner.h:568
  buf[3] = byte(x and 0xFF'u32)                           # inner.h:569

proc br_i31_zero*(x: I31; bit_len: uint32) {.inline.} =
  ## inner.h:1382-1387.
  x[0] = bit_len                                          # inner.h:1385 `*x ++ = bit_len`
  # inner.h:1386 `memset(x, 0, ((bit_len + 31) >> 5) * sizeof *x)`, as a
  # loop over the words after word 0 (no memset in Nimony's stdlib)
  let n = int((bit_len + 31'u32) shr 5)
  var i = 1
  while i <= n:
    x[i] = 0'u32
    inc i

# ------------------------------------------------------------ i32_div32.c ---

func br_divrem*(hi, lo, d: uint32; r: var uint32): uint32 =
  ## `(hi:lo) / d` and its remainder, constant time (i32_div32.c:27-56).
  var hi = hi
  var lo = lo
  var q = 0'u32                                           # i32_div32.c:36
  let ch = EQ(hi, d)                                      # i32_div32.c:37
  hi = MUX(ch, 0'u32, hi)                                 # i32_div32.c:38
  var k = 31                                              # i32_div32.c:39
  while k > 0:
    let j = 32 - k                                        # i32_div32.c:43
    let w = (hi shl j) or (lo shr k)                      # i32_div32.c:44
    let ctl = GE(w, d) or (hi shr k)                      # i32_div32.c:45
    let hi2 = (w - d) shr j                               # i32_div32.c:46
    let lo2 = lo - (d shl k)                              # i32_div32.c:47
    hi = MUX(ctl, hi2, hi)                                # i32_div32.c:48
    lo = MUX(ctl, lo2, lo)                                # i32_div32.c:49
    q = q or (ctl shl k)                                  # i32_div32.c:50
    dec k
  let cf = GE(lo, d) or hi                                # i32_div32.c:52
  q = q or cf                                             # i32_div32.c:53
  r = MUX(cf, lo - d, lo)                                 # i32_div32.c:54
  result = q                                              # i32_div32.c:55

func br_rem*(hi, lo, d: uint32): uint32 {.inline.} =
  ## inner.h:1045-1055: the remainder of `br_divrem`.
  var r = 0'u32
  discard br_divrem(hi, lo, d, r)                         # inner.h:1054
  result = r                                              # inner.h:1055

func br_div*(hi, lo, d: uint32): uint32 {.inline.} =
  ## inner.h:1058-1068: the quotient of `br_divrem`.
  var r = 0'u32
  result = br_divrem(hi, lo, d, r)                        # inner.h:1067

# ---------------------------------------------------------------- ccopy.c ---

proc br_ccopy*(ctl: uint32; dst, src: pointer; len: int) =
  ## Copies `len` bytes from `src` to `dst` iff `ctl == 1` (ccopy.c:27-44).
  var d = cast[Bytes](dst)                                # ccopy.c:34
  var s = cast[Bytes](src)                                # ccopy.c:35
  var len = len
  while len > 0:                                          # ccopy.c:36 `while (len -- > 0)`
    dec len
    let x = uint32(s[0])                                  # ccopy.c:39 `x = *s ++`
    s = s +! 1
    let y = uint32(d[0])                                  # ccopy.c:40
    d[0] = byte(MUX(ctl, x, y))                           # ccopy.c:41
    d = d +! 1                                            # ccopy.c:42

# ------------------------------------------------------------- i31 words ---

func br_i31_bit_length*(x: I31; xlen: int): uint32 =
  ## i31_bitlen.c:27-44. `x` points at the first value word.
  var tw = 0'u32                                          # i31_bitlen.c:33
  var twk = 0'u32                                         # i31_bitlen.c:34
  var xlen = xlen
  while xlen > 0:                                         # i31_bitlen.c:35 `while (xlen -- > 0)`
    dec xlen
    let c = EQ(tw, 0'u32)                                 # i31_bitlen.c:38
    let w = x[xlen]                                       # i31_bitlen.c:39
    tw = MUX(c, w, tw)                                    # i31_bitlen.c:40
    twk = MUX(c, uint32(xlen), twk)                       # i31_bitlen.c:41
  result = (twk shl 5) + BIT_LENGTH(tw)                   # i31_bitlen.c:43

proc br_i31_decode*(x: I31; src: Bytes; len: int) =
  ## Big-endian bytes to i31 (i31_decode.c:27-57).
  let buf = src                                           # i31_decode.c:36
  var u = len                                             # i31_decode.c:37
  var v = 1                                               # i31_decode.c:38
  var acc = 0'u32                                         # i31_decode.c:39
  var acc_len = 0                                         # i31_decode.c:40
  while u > 0:                                            # i31_decode.c:41 `while (u -- > 0)`
    dec u
    let b = uint32(buf[u])                                # i31_decode.c:44
    acc = acc or (b shl acc_len)                          # i31_decode.c:45
    acc_len = acc_len + 8                                 # i31_decode.c:46
    if acc_len >= 31:                                     # i31_decode.c:47
      x[v] = acc and 0x7FFFFFFF'u32                       # i31_decode.c:48 `x[v ++] = ...`
      inc v
      acc_len = acc_len - 31                              # i31_decode.c:49
      acc = b shr (8 - acc_len)                           # i31_decode.c:50
  if acc_len != 0:                                        # i31_decode.c:53
    x[v] = acc                                            # i31_decode.c:54 `x[v ++] = acc`
    inc v
  x[0] = br_i31_bit_length(x +! 1, v - 1)                 # i31_decode.c:56

proc br_i31_add*(a, b: I31; ctl: uint32): uint32 =
  ## `a += b` iff `ctl == 1`; returns the carry (i31_add.c:27-46).
  var cc = 0'u32                                          # i31_add.c:34
  let m = int((a[0] + 63'u32) shr 5)                      # i31_add.c:35
  var u = 1                                               # i31_add.c:36
  while u < m:
    let aw = a[u]                                         # i31_add.c:39
    let bw = b[u]                                         # i31_add.c:40
    let naw = aw + bw + cc                                # i31_add.c:41
    cc = naw shr 31                                       # i31_add.c:42
    a[u] = MUX(ctl, naw and 0x7FFFFFFF'u32, aw)           # i31_add.c:43
    inc u
  result = cc                                             # i31_add.c:45

proc br_i31_sub*(a, b: I31; ctl: uint32): uint32 =
  ## `a -= b` iff `ctl == 1`; returns the borrow (i31_sub.c:27-46).
  var cc = 0'u32                                          # i31_sub.c:34
  let m = int((a[0] + 63'u32) shr 5)                      # i31_sub.c:35
  var u = 1                                               # i31_sub.c:36
  while u < m:
    let aw = a[u]                                         # i31_sub.c:39
    let bw = b[u]                                         # i31_sub.c:40
    let naw = aw - bw - cc                                # i31_sub.c:41
    cc = naw shr 31                                       # i31_sub.c:42
    a[u] = MUX(ctl, naw and 0x7FFFFFFF'u32, aw)           # i31_sub.c:43
    inc u
  result = cc                                             # i31_sub.c:45

proc br_i31_rshift*(x: I31; count: int) =
  ## `x >>= count`, `count < 31` (i31_rshift.c:27-47).
  let len = int((x[0] + 31'u32) shr 5)                    # i31_rshift.c:34
  if len == 0:                                            # i31_rshift.c:35
    return                                                # i31_rshift.c:36
  var r = x[1] shr count                                  # i31_rshift.c:38
  var u = 2                                               # i31_rshift.c:39
  while u <= len:
    let w = x[u]                                          # i31_rshift.c:42
    x[u - 1] = ((w shl (31 - count)) or r) and 0x7FFFFFFF'u32  # i31_rshift.c:43
    r = w shr count                                       # i31_rshift.c:44
    inc u
  x[len] = r                                              # i31_rshift.c:46

proc br_i31_muladd_small*(x: I31; z: uint32; m: I31) =
  ## `x = (x * 2^31 + z) mod m` (i31_muladd.c:27-157).
  # i31_muladd.c:42-45: the modulus bit length is not secret to BearSSL
  let m_bitlen = m[0]
  if m_bitlen == 0'u32:
    return
  if m_bitlen <= 31'u32:                                  # i31_muladd.c:46
    let hi = x[1] shr 1                                   # i31_muladd.c:49
    let lo = (x[1] shl 31) or z                           # i31_muladd.c:50
    x[1] = br_rem(hi, lo, m[1])                           # i31_muladd.c:51
    return                                                # i31_muladd.c:52
  let mlen = int((m_bitlen + 31'u32) shr 5)               # i31_muladd.c:54
  let mblr = int(m_bitlen and 31'u32)                     # i31_muladd.c:55
  let hi = x[mlen]                                        # i31_muladd.c:83
  var a0, a1, b0: uint32
  if mblr == 0:                                           # i31_muladd.c:84
    a0 = x[mlen]                                          # i31_muladd.c:85
    moveMem(addr x[2], addr x[1], (mlen - 1) * sizeof(uint32))  # i31_muladd.c:86
    x[1] = z                                              # i31_muladd.c:87
    a1 = x[mlen]                                          # i31_muladd.c:88
    b0 = m[mlen]                                          # i31_muladd.c:89
  else:
    a0 = ((x[mlen] shl (31 - mblr)) or (x[mlen - 1] shr mblr)) and
         0x7FFFFFFF'u32                                   # i31_muladd.c:91-92
    moveMem(addr x[2], addr x[1], (mlen - 1) * sizeof(uint32))  # i31_muladd.c:93
    x[1] = z                                              # i31_muladd.c:94
    a1 = ((x[mlen] shl (31 - mblr)) or (x[mlen - 1] shr mblr)) and
         0x7FFFFFFF'u32                                   # i31_muladd.c:95-96
    b0 = ((m[mlen] shl (31 - mblr)) or (m[mlen - 1] shr mblr)) and
         0x7FFFFFFF'u32                                   # i31_muladd.c:97-98
  let g = br_div(a0 shr 1, a1 or (a0 shl 31), b0)         # i31_muladd.c:114
  let q = MUX(EQ(a0, b0), 0x7FFFFFFF'u32,
              MUX(EQ(g, 0'u32), 0'u32, g - 1'u32))        # i31_muladd.c:115
  var cc = 0'u32                                          # i31_muladd.c:126
  var tb = 1'u32                                          # i31_muladd.c:127
  var u = 1                                               # i31_muladd.c:128
  while u <= mlen:
    let mw = m[u]                                         # i31_muladd.c:132
    let zl = MUL31(mw, q) + uint64(cc)                    # i31_muladd.c:133
    cc = uint32((zl shr 31) and 0xFFFFFFFF'u64)           # i31_muladd.c:134
    let zw = uint32(zl and 0xFFFFFFFF'u64) and 0x7FFFFFFF'u32  # i31_muladd.c:135
    let xw = x[u]                                         # i31_muladd.c:136
    var nxw = xw - zw                                     # i31_muladd.c:137
    cc = cc + (nxw shr 31)                                # i31_muladd.c:138
    nxw = nxw and 0x7FFFFFFF'u32                          # i31_muladd.c:139
    x[u] = nxw                                            # i31_muladd.c:140
    tb = MUX(EQ(nxw, mw), tb, GT(nxw, mw))                # i31_muladd.c:141
    inc u
  let over = GT(cc, hi)                                   # i31_muladd.c:153
  let under = (not over) and (tb or LT(cc, hi))           # i31_muladd.c:154
  discard br_i31_add(x, m, over)                          # i31_muladd.c:155
  discard br_i31_sub(x, m, under)                         # i31_muladd.c:156

proc br_i31_decode_reduce*(x: I31; src: Bytes; len: int; m: I31) =
  ## Big-endian bytes, reduced modulo `m` (i31_decred.c:27-103).
  let m_ebitlen = m[0]                                    # i31_decred.c:41
  if m_ebitlen == 0'u32:                                  # i31_decred.c:46
    x[0] = 0'u32                                          # i31_decred.c:47
    return                                                # i31_decred.c:48
  br_i31_zero(x, m_ebitlen)                               # i31_decred.c:54
  var m_rbitlen = m_ebitlen shr 5                         # i31_decred.c:60
  m_rbitlen = (m_ebitlen and 31'u32) + (m_rbitlen shl 5) - m_rbitlen  # i31_decred.c:61
  let mblen = int((m_rbitlen + 7'u32) shr 3)              # i31_decred.c:62
  var k = mblen - 1                                       # i31_decred.c:63
  if k >= len:                                            # i31_decred.c:64
    br_i31_decode(x, src, len)                            # i31_decred.c:65
    x[0] = m_ebitlen                                      # i31_decred.c:66
    return                                                # i31_decred.c:67
  let buf = src                                           # i31_decred.c:69
  br_i31_decode(x, buf, k)                                # i31_decred.c:70
  x[0] = m_ebitlen                                        # i31_decred.c:71
  var acc = 0'u32                                         # i31_decred.c:76
  var acc_len = 0                                         # i31_decred.c:77
  while k < len:                                          # i31_decred.c:78
    let v = uint32(buf[k])                                # i31_decred.c:81 `v = buf[k ++]`
    inc k
    if acc_len >= 23:                                     # i31_decred.c:82
      acc_len = acc_len - 23                              # i31_decred.c:83
      acc = acc shl (8 - acc_len)                         # i31_decred.c:84
      acc = acc or (v shr acc_len)                        # i31_decred.c:85
      br_i31_muladd_small(x, acc, m)                      # i31_decred.c:86
      acc = v and (0xFF'u32 shr (8 - acc_len))            # i31_decred.c:87
    else:
      acc = (acc shl 8) or v                              # i31_decred.c:89
      acc_len = acc_len + 8                               # i31_decred.c:90
  if acc_len != 0:                                        # i31_decred.c:98
    acc = (acc or (x[1] shl acc_len)) and 0x7FFFFFFF'u32  # i31_decred.c:99
    br_i31_rshift(x, 31 - acc_len)                        # i31_decred.c:100
    br_i31_muladd_small(x, acc, m)                        # i31_decred.c:101

proc br_i31_encode*(dst: Bytes; len: int; x: I31) =
  ## i31 to `len` big-endian bytes, truncated or zero-padded
  ## (i31_encode.c:27-79).
  let xlen = int((x[0] + 31'u32) shr 5)                   # i31_encode.c:36
  if xlen == 0:                                           # i31_encode.c:37
    # i31_encode.c:38 `memset(dst, 0, len)`, as a loop
    var i = 0
    while i < len:
      dst[i] = 0'u8
      inc i
    return                                                # i31_encode.c:39
  # i31_encode.c:41 `buf = dst + len`: `buf` is kept as an offset into `dst`
  # because `buf[-3]` below indexes before it
  var buf = len
  var len = len
  var k = 1                                               # i31_encode.c:42
  var acc = 0'u32                                         # i31_encode.c:43
  var acc_len = 0                                         # i31_encode.c:44
  while len != 0:                                         # i31_encode.c:45
    let w = if k <= xlen: x[k] else: 0'u32                # i31_encode.c:48
    inc k                                                 # i31_encode.c:49
    if acc_len == 0:                                      # i31_encode.c:50
      acc = w                                             # i31_encode.c:51
      acc_len = 31                                        # i31_encode.c:52
    else:
      let z = acc or (w shl acc_len)                      # i31_encode.c:56
      dec acc_len                                         # i31_encode.c:57
      acc = w shr (31 - acc_len)                          # i31_encode.c:58
      if len >= 4:                                        # i31_encode.c:59
        buf = buf - 4                                     # i31_encode.c:60
        len = len - 4                                     # i31_encode.c:61
        br_enc32be(dst +! buf, z)                         # i31_encode.c:62
      else:
        # i31_encode.c:64-74: the fall-through `switch (len)` as three `if`s
        if len == 3:                                      # i31_encode.c:65
          dst[buf - 3] = byte((z shr 16) and 0xFF'u32)    # i31_encode.c:66
        if len >= 2:                                      # i31_encode.c:68
          dst[buf - 2] = byte((z shr 8) and 0xFF'u32)     # i31_encode.c:69
        if len >= 1:                                      # i31_encode.c:71
          dst[buf - 1] = byte(z and 0xFF'u32)             # i31_encode.c:72
        return                                            # i31_encode.c:75

func br_i31_ninv31*(x: uint32): uint32 =
  ## `-1/x mod 2^31` for odd `x`, 0 for even `x` (i31_ninv31.c:27-39).
  var y = 2'u32 - x                                       # i31_ninv31.c:33
  y = y * (2'u32 - y * x)                                 # i31_ninv31.c:34
  y = y * (2'u32 - y * x)                                 # i31_ninv31.c:35
  y = y * (2'u32 - y * x)                                 # i31_ninv31.c:36
  y = y * (2'u32 - y * x)                                 # i31_ninv31.c:37
  result = MUX(x and 1'u32, 0'u32 - y, 0'u32) and 0x7FFFFFFF'u32  # i31_ninv31.c:38

proc br_i31_montymul*(d, x, y, m: I31; m0i: uint32) =
  ## `d = x * y / 2^(31 * len) mod m` (i31_montmul.c:27-127). `d` must not
  ## overlap `x`, `y` or `m`.
  let len = int((m[0] + 31'u32) shr 5)                    # i31_montmul.c:46
  let len4 = len and not 3                                # i31_montmul.c:47
  br_i31_zero(d, m[0])                                    # i31_montmul.c:48
  var dh = 0'u32                                          # i31_montmul.c:49
  var u = 0                                               # i31_montmul.c:50
  while u < len:
    # i31_montmul.c:66-70: BR_64 is set for the 64-bit targets Nimony
    # builds, so the carry `r` is the `uint64_t` variant
    let xu = x[u + 1]                                     # i31_montmul.c:72
    let f = MUL31_lo((d[1] + MUL31_lo(x[u + 1], y[1])), m0i)  # i31_montmul.c:73
    var r = 0'u64                                         # i31_montmul.c:75
    var v = 0                                             # i31_montmul.c:76
    while v < len4:
      var z = uint64(d[v + 1]) + MUL31(xu, y[v + 1]) +
              MUL31(f, m[v + 1]) + r                      # i31_montmul.c:79-80
      r = z shr 31                                        # i31_montmul.c:81
      d[v + 0] = uint32(z and 0x7FFFFFFF'u64)             # i31_montmul.c:82
      z = uint64(d[v + 2]) + MUL31(xu, y[v + 2]) +
          MUL31(f, m[v + 2]) + r                          # i31_montmul.c:83-84
      r = z shr 31                                        # i31_montmul.c:85
      d[v + 1] = uint32(z and 0x7FFFFFFF'u64)             # i31_montmul.c:86
      z = uint64(d[v + 3]) + MUL31(xu, y[v + 3]) +
          MUL31(f, m[v + 3]) + r                          # i31_montmul.c:87-88
      r = z shr 31                                        # i31_montmul.c:89
      d[v + 2] = uint32(z and 0x7FFFFFFF'u64)             # i31_montmul.c:90
      z = uint64(d[v + 4]) + MUL31(xu, y[v + 4]) +
          MUL31(f, m[v + 4]) + r                          # i31_montmul.c:91-92
      r = z shr 31                                        # i31_montmul.c:93
      d[v + 3] = uint32(z and 0x7FFFFFFF'u64)             # i31_montmul.c:94
      v = v + 4                                           # i31_montmul.c:76 `v += 4`
    while v < len:                                        # i31_montmul.c:96
      let z = uint64(d[v + 1]) + MUL31(xu, y[v + 1]) +
              MUL31(f, m[v + 1]) + r                      # i31_montmul.c:99-100
      r = z shr 31                                        # i31_montmul.c:101
      d[v] = uint32(z and 0x7FFFFFFF'u64)                 # i31_montmul.c:102
      inc v
    # i31_montmul.c:110 `dh += r`: `r` fits 32 bits here (i31_montmul.c:57)
    dh = dh + uint32(r and 0xFFFFFFFF'u64)
    d[len] = dh and 0x7FFFFFFF'u32                        # i31_montmul.c:111
    dh = dh shr 31                                        # i31_montmul.c:112
    inc u
  d[0] = m[0]                                             # i31_montmul.c:120
  discard br_i31_sub(d, m, NEQ(dh, 0'u32) or NOT(br_i31_sub(d, m, 0'u32)))  # i31_montmul.c:126

proc br_i31_to_monty*(x, m: I31) =
  ## `x = x * 2^(31 * len) mod m` (i31_tmont.c:27-36).
  var k = (m[0] + 31'u32) shr 5                           # i31_tmont.c:33
  while k > 0'u32:
    br_i31_muladd_small(x, 0'u32, m)                      # i31_tmont.c:34
    dec k

proc br_i31_from_monty*(x, m: I31; m0i: uint32) =
  ## `x = x / 2^(31 * len) mod m` (i31_fmont.c:27-60).
  let len = int((m[0] + 31'u32) shr 5)                    # i31_fmont.c:33
  var u = 0                                               # i31_fmont.c:34
  while u < len:
    let f = MUL31_lo(x[1], m0i)                           # i31_fmont.c:38
    var cc = 0'u64                                        # i31_fmont.c:39
    var v = 0                                             # i31_fmont.c:40
    while v < len:
      let z = uint64(x[v + 1]) + MUL31(f, m[v + 1]) + cc  # i31_fmont.c:43
      cc = z shr 31                                       # i31_fmont.c:44
      if v != 0:                                          # i31_fmont.c:45
        x[v] = uint32(z and 0xFFFFFFFF'u64) and 0x7FFFFFFF'u32  # i31_fmont.c:46
      inc v
    x[len] = uint32(cc and 0xFFFFFFFF'u64)                # i31_fmont.c:49
    inc u
  discard br_i31_sub(x, m, NOT(br_i31_sub(x, m, 0'u32)))  # i31_fmont.c:59

proc br_i31_reduce*(x, a, m: I31) =
  ## `x = a mod m` (i31_reduce.c:27-66).
  let m_bitlen = m[0]                                     # i31_reduce.c:34
  let mlen = int((m_bitlen + 31'u32) shr 5)               # i31_reduce.c:35
  x[0] = m_bitlen                                         # i31_reduce.c:37
  if m_bitlen == 0'u32:                                   # i31_reduce.c:38
    return                                                # i31_reduce.c:39
  let a_bitlen = a[0]                                     # i31_reduce.c:46
  let alen = int((a_bitlen + 31'u32) shr 5)               # i31_reduce.c:47
  if a_bitlen < m_bitlen:                                 # i31_reduce.c:48
    copyMem(addr x[1], addr a[1], alen * sizeof(uint32))  # i31_reduce.c:49
    var u = alen                                          # i31_reduce.c:50
    while u < mlen:
      x[u + 1] = 0'u32                                    # i31_reduce.c:51
      inc u
    return                                                # i31_reduce.c:53
  copyMem(addr x[1], addr a[2 + (alen - mlen)], (mlen - 1) * sizeof(uint32))  # i31_reduce.c:61
  x[mlen] = 0'u32                                         # i31_reduce.c:62
  var u = 1 + alen - mlen                                 # i31_reduce.c:63
  while u > 0:
    br_i31_muladd_small(x, a[u], m)                       # i31_reduce.c:64
    dec u

proc br_i31_mulacc*(d, a, b: I31) =
  ## `d += a * b`, not modular (i31_mulacc.c:27-74). `d` must not overlap
  ## `a` or `b`.
  let alen = int((a[0] + 31'u32) shr 5)                   # i31_mulacc.c:34
  let blen = int((b[0] + 31'u32) shr 5)                   # i31_mulacc.c:35
  let dl = (a[0] and 31'u32) + (b[0] and 31'u32)          # i31_mulacc.c:41
  let dh = (a[0] shr 5) + (b[0] shr 5)                    # i31_mulacc.c:42
  d[0] = (dh shl 5) + dl + ((not (dl - 31'u32)) shr 31)   # i31_mulacc.c:43
  var u = 0                                               # i31_mulacc.c:45
  while u < blen:
    # i31_mulacc.c:57-61: BR_64, so the carry is the `uint64_t` variant
    let f = b[1 + u]                                      # i31_mulacc.c:63
    var cc = 0'u64                                        # i31_mulacc.c:64
    var v = 0                                             # i31_mulacc.c:65
    while v < alen:
      let z = uint64(d[1 + u + v]) + MUL31(f, a[1 + v]) + cc  # i31_mulacc.c:68
      cc = z shr 31                                       # i31_mulacc.c:69
      d[1 + u + v] = uint32(z and 0x7FFFFFFF'u64)         # i31_mulacc.c:70
      inc v
    d[1 + u + alen] = uint32(cc and 0xFFFFFFFF'u64)       # i31_mulacc.c:72
    inc u

proc br_i31_modpow_opt*(x: I31; e: Bytes; elen: int; m: I31; m0i: uint32;
                        tmp: I31; twlen: int): uint32 =
  ## `x = x^e mod m`, `e` big-endian over `elen` bytes; returns 0 when `tmp`
  ## (`twlen` words) is too small, 1 otherwise (i31_modpow2.c:27-160).
  var elen = elen
  var ei = 0                                              # `e ++` below advances this index
  var mwlen = int((m[0] + 63'u32) shr 5)                  # i31_modpow2.c:42
  let mlen = mwlen * sizeof(uint32)                       # i31_modpow2.c:43
  mwlen = mwlen + (mwlen and 1)                           # i31_modpow2.c:44
  let t1 = tmp                                            # i31_modpow2.c:45
  let t2 = tmp +! mwlen                                   # i31_modpow2.c:46
  if twlen < (mwlen shl 1):                               # i31_modpow2.c:54
    return 0'u32                                          # i31_modpow2.c:55
  var win_len = 5                                         # i31_modpow2.c:57
  while win_len > 1:
    if ((1 shl win_len) + 1) * mwlen <= twlen:            # i31_modpow2.c:58
      break                                               # i31_modpow2.c:59
    dec win_len
  br_i31_to_monty(x, m)                                   # i31_modpow2.c:66
  if win_len == 1:                                        # i31_modpow2.c:73
    copyMem(t2, x, mlen)                                  # i31_modpow2.c:74
  else:
    copyMem(t2 +! mwlen, x, mlen)                         # i31_modpow2.c:76
    var base = t2 +! mwlen                                # i31_modpow2.c:77
    var u = 2                                             # i31_modpow2.c:78
    while u < (1 shl win_len):
      br_i31_montymul(base +! mwlen, base, x, m, m0i)     # i31_modpow2.c:79
      base = base +! mwlen                                # i31_modpow2.c:80
      inc u
  br_i31_zero(x, m[0])                                    # i31_modpow2.c:89
  x[int((m[0] + 31'u32) shr 5)] = 1'u32                   # i31_modpow2.c:90
  br_i31_muladd_small(x, 0'u32, m)                        # i31_modpow2.c:91
  var acc = 0'u32                                         # i31_modpow2.c:97
  var acc_len = 0                                         # i31_modpow2.c:98
  while acc_len > 0 or elen > 0:                          # i31_modpow2.c:99
    var k = win_len                                       # i31_modpow2.c:106
    if acc_len < win_len:                                 # i31_modpow2.c:107
      if elen > 0:                                        # i31_modpow2.c:108
        acc = (acc shl 8) or uint32(e[ei])                # i31_modpow2.c:109 `*e ++`
        inc ei
        dec elen                                          # i31_modpow2.c:110
        acc_len = acc_len + 8                             # i31_modpow2.c:111
      else:
        k = acc_len                                       # i31_modpow2.c:113
    let bits = (acc shr (acc_len - k)) and ((1'u32 shl k) - 1'u32)  # i31_modpow2.c:116
    acc_len = acc_len - k                                 # i31_modpow2.c:117
    var i = 0                                             # i31_modpow2.c:122
    while i < k:
      br_i31_montymul(t1, x, x, m, m0i)                   # i31_modpow2.c:123
      copyMem(x, t1, mlen)                                # i31_modpow2.c:124
      inc i
    if win_len > 1:                                       # i31_modpow2.c:133
      br_i31_zero(t2, m[0])                               # i31_modpow2.c:134
      var base = t2 +! mwlen                              # i31_modpow2.c:135
      var u = 1                                           # i31_modpow2.c:136
      while u < (1 shl k):
        # i31_modpow2.c:139 `mask = -EQ(u, bits)`, through ctOpaque
        let mask = ctOpaque(0'u32 - EQ(uint32(u), bits))
        var v = 1                                         # i31_modpow2.c:140
        while v < mwlen:
          t2[v] = t2[v] or (mask and base[v])             # i31_modpow2.c:141
          inc v
        base = base +! mwlen                              # i31_modpow2.c:143
        inc u
    br_i31_montymul(t1, x, t2, m, m0i)                    # i31_modpow2.c:151
    br_ccopy(NEQ(bits, 0'u32), x, t1, mlen)               # i31_modpow2.c:152 CCOPY
  br_i31_from_monty(x, m, m0i)                            # i31_modpow2.c:158
  result = 1'u32                                          # i31_modpow2.c:159
