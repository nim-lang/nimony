#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## ChaCha20, Poly1305 and their AEAD construction (RFC 8439).
##
## Constant time by construction: ChaCha20 is additions, rotations and xors
## only, and Poly1305 is done in 26-bit limbs so that every product fits a
## `uint64` and the final reduction is a mask, not a branch.

{.feature: "staticContracts".}

import std/crypto/hmac

type
  ChaChaKey* = array[32, byte]
  ChaChaNonce* = array[12, byte]
  Poly1305Tag* = array[16, byte]

func rotl(x: uint32; n: int): uint32 {.inline.} =
  (x shl n) or (x shr (32 - n))

func load32le(b: openArray[byte]; o: int): uint32 {.inline,
    requires: o >= 0 and o + 3 < b.len.} =
  uint32(b[o]) or (uint32(b[o+1]) shl 8) or (uint32(b[o+2]) shl 16) or
    (uint32(b[o+3]) shl 24)

# --------------------------------------------------------------- ChaCha20 ---

func chachaState(key: ChaChaKey; counter: uint32; nonce: ChaChaNonce): array[16, uint32] =
  result = default(array[16, uint32])
  result[0] = 0x61707865'u32
  result[1] = 0x3320646e'u32
  result[2] = 0x79622d32'u32
  result[3] = 0x6b206574'u32
  for i in 0 ..< 8: result[4 + i] = load32le(key, i * 4)
  result[12] = counter
  for i in 0 ..< 3: result[13 + i] = load32le(nonce, i * 4)

when defined(nimNativeBackend) and defined(amd64):
  import std/crypto/private/chacha20x64

  func chachaRounds(s: array[16, uint32]): array[16, uint32] =
    ## `s` plus its 20 rounds, by the hand-written block function.
    var input = s
    result = default(array[16, uint32])
    {.cast(noSideEffect).}:
      chachaCore(cast[Words32](addr result), cast[Words32](addr input))
else:
  template quarterRound(x: var array[16, uint32]; a, b, c, d: int) =
    x[a] = x[a] + x[b]; x[d] = rotl(x[d] xor x[a], 16)
    x[c] = x[c] + x[d]; x[b] = rotl(x[b] xor x[c], 12)
    x[a] = x[a] + x[b]; x[d] = rotl(x[d] xor x[a], 8)
    x[c] = x[c] + x[d]; x[b] = rotl(x[b] xor x[c], 7)

  func chachaRounds(s: array[16, uint32]): array[16, uint32] =
    var x = s
    for i in 0 ..< 10:
      quarterRound(x, 0, 4, 8, 12)
      quarterRound(x, 1, 5, 9, 13)
      quarterRound(x, 2, 6, 10, 14)
      quarterRound(x, 3, 7, 11, 15)
      quarterRound(x, 0, 5, 10, 15)
      quarterRound(x, 1, 6, 11, 12)
      quarterRound(x, 2, 7, 8, 13)
      quarterRound(x, 3, 4, 9, 14)
    result = default(array[16, uint32])
    for i in 0 ..< 16: result[i] = x[i] + s[i]

func chachaBlock*(key: ChaChaKey; counter: uint32; nonce: ChaChaNonce;
                  output: var array[64, byte]) =
  ## One 64-byte keystream block.
  let x = chachaRounds(chachaState(key, counter, nonce))
  for i in 0 ..< 16:
    let v = x[i]
    let o = i * 4
    output[o] = byte(v and 0xFF'u32)
    output[o+1] = byte((v shr 8) and 0xFF'u32)
    output[o+2] = byte((v shr 16) and 0xFF'u32)
    output[o+3] = byte(v shr 24)

func chacha20Xor*(key: ChaChaKey; counter: uint32; nonce: ChaChaNonce;
                  data: var openArray[byte]) =
  ## Encrypts (or, identically, decrypts) `data` in place with the keystream
  ## starting at block `counter`.
  var ks = default(array[64, byte])
  var ctr = counter
  var pos = 0
  while pos < data.len:
    chachaBlock(key, ctr, nonce, ks)
    var j = 0
    while j < 64 and pos < data.len:
      data[pos] = data[pos] xor ks[j]
      inc pos
      inc j
    ctr = ctr + 1'u32

# --------------------------------------------------------------- Poly1305 ---

when defined(nimNativeBackend) and defined(amd64):
  import std/crypto/private/poly1305x64

  # The block function and the final reduction are hand-written assembler
  # (see the module): the accumulator depends on the key, and only assembler
  # promises that the instructions which run do not depend on it.
  type
    Poly1305* = object
      st: array[6, uint64]       ## h0, h1, h2, r0, r1, s1
      s: array[2, uint64]
      buf: array[16, byte]
      bufLen: range[0..15]

  func le64at(b: openArray[byte]; o: int): uint64 {.requires: o >= 0 and o + 7 < b.len.} =
    result = uint64(b[o]) or (uint64(b[o+1]) shl 8) or (uint64(b[o+2]) shl 16) or
      (uint64(b[o+3]) shl 24) or (uint64(b[o+4]) shl 32) or (uint64(b[o+5]) shl 40) or
      (uint64(b[o+6]) shl 48) or (uint64(b[o+7]) shl 56)

  func initPoly1305*(key: array[32, byte]): Poly1305 =
    result = Poly1305(st: default(array[6, uint64]), s: default(array[2, uint64]),
                      buf: default(array[16, byte]), bufLen: 0)
    let r0 = le64at(key, 0) and 0x0FFFFFFC0FFFFFFF'u64
    let r1 = le64at(key, 8) and 0x0FFFFFFC0FFFFFFC'u64
    result.st[3] = r0
    result.st[4] = r1
    result.st[5] = r1 + (r1 shr 2)
    result.s[0] = le64at(key, 16)
    result.s[1] = le64at(key, 24)

  proc words(x: var array[6, uint64]): Words {.inline.} = cast[Words](addr x)

  func absorb(p: var Poly1305; hibit: uint64) =
    var blk = p.buf
    {.cast(noSideEffect).}:
      polyBlocks(words(p.st), cast[Words](addr blk), 1, hibit)

  func update*(p: var Poly1305; data: openArray[byte]) =
    for i in 0 ..< data.len:
      let at = p.bufLen
      p.buf[at] = data[i]
      if at == 15:
        absorb(p, 1'u64)
        p.bufLen = 0
      else:
        p.bufLen = at + 1

  func padTo16(p: var Poly1305) =
    if p.bufLen > 0:
      var n: int = p.bufLen
      while n < 16:
        p.buf[n] = 0'u8
        inc n
      absorb(p, 1'u64)
      p.bufLen = 0

  func finish*(p: Poly1305): Poly1305Tag =
    var st = p
    if st.bufLen > 0:
      var n: int = st.bufLen
      st.buf[n] = 1'u8
      inc n
      while n < 16:
        st.buf[n] = 0'u8
        inc n
      absorb(st, 0'u64)
    var tag = default(array[2, uint64])
    var s = st.s
    {.cast(noSideEffect).}:
      polyFinish(words(st.st), cast[Words](addr tag), cast[Words](addr s))
    result = default(Poly1305Tag)
    for i in 0 ..< 2:
      for k in 0 ..< 8:
        result[8*i + k] = byte((tag[i] shr (8 * k)) and 0xFF'u64)

else:
  type
    Poly1305* = object
      r, h: array[5, uint32]     ## 26-bit limbs
      pad: array[4, uint32]
      buf: array[16, byte]
      bufLen: range[0..15]

  func initPoly1305*(key: array[32, byte]): Poly1305 =
    result = Poly1305(r: default(array[5, uint32]), h: default(array[5, uint32]),
                      pad: default(array[4, uint32]), buf: default(array[16, byte]),
                      bufLen: 0)
    # r &= 0xffffffc0ffffffc0ffffffc0fffffff
    result.r[0] = load32le(key, 0) and 0x3ffffff'u32
    result.r[1] = (load32le(key, 3) shr 2) and 0x3ffff03'u32
    result.r[2] = (load32le(key, 6) shr 4) and 0x3ffc0ff'u32
    result.r[3] = (load32le(key, 9) shr 6) and 0x3f03fff'u32
    result.r[4] = (load32le(key, 12) shr 8) and 0x00fffff'u32
    for i in 0 ..< 4: result.pad[i] = load32le(key, 16 + i * 4)

  func polyBlock(h: var array[5, uint32]; r: array[5, uint32]; m: array[16, byte];
                 hibit: uint32) =
    ## `h = (h + m) * r mod 2^130 - 5`, with `m` read as a 129-bit number when
    ## `hibit` is set (a full block) and as 128 bits otherwise.
    let r0 = uint64(r[0])
    let r1 = uint64(r[1])
    let r2 = uint64(r[2])
    let r3 = uint64(r[3])
    let r4 = uint64(r[4])
    let s1 = r1 * 5
    let s2 = r2 * 5
    let s3 = r3 * 5
    let s4 = r4 * 5
    var h0 = h[0] + (load32le(m, 0) and 0x3ffffff'u32)
    var h1 = h[1] + ((load32le(m, 3) shr 2) and 0x3ffffff'u32)
    var h2 = h[2] + ((load32le(m, 6) shr 4) and 0x3ffffff'u32)
    var h3 = h[3] + ((load32le(m, 9) shr 6) and 0x3ffffff'u32)
    var h4 = h[4] + ((load32le(m, 12) shr 8) or hibit)
    let d0 = uint64(h0)*r0 + uint64(h1)*s4 + uint64(h2)*s3 + uint64(h3)*s2 + uint64(h4)*s1
    var d1 = uint64(h0)*r1 + uint64(h1)*r0 + uint64(h2)*s4 + uint64(h3)*s3 + uint64(h4)*s2
    var d2 = uint64(h0)*r2 + uint64(h1)*r1 + uint64(h2)*r0 + uint64(h3)*s4 + uint64(h4)*s3
    var d3 = uint64(h0)*r3 + uint64(h1)*r2 + uint64(h2)*r1 + uint64(h3)*r0 + uint64(h4)*s4
    var d4 = uint64(h0)*r4 + uint64(h1)*r3 + uint64(h2)*r2 + uint64(h3)*r1 + uint64(h4)*r0
    var c = d0 shr 26
    h0 = uint32(d0 and 0x3ffffff'u64)
    d1 = d1 + c; c = d1 shr 26; h1 = uint32(d1 and 0x3ffffff'u64)
    d2 = d2 + c; c = d2 shr 26; h2 = uint32(d2 and 0x3ffffff'u64)
    d3 = d3 + c; c = d3 shr 26; h3 = uint32(d3 and 0x3ffffff'u64)
    d4 = d4 + c; c = d4 shr 26; h4 = uint32(d4 and 0x3ffffff'u64)
    h0 = h0 + uint32(c * 5)
    let c2 = h0 shr 26
    h0 = h0 and 0x3ffffff'u32
    h1 = h1 + c2
    h[0] = h0
    h[1] = h1
    h[2] = h2
    h[3] = h3
    h[4] = h4

  func update*(p: var Poly1305; data: openArray[byte]) =
    for i in 0 ..< data.len:
      let at = p.bufLen
      p.buf[at] = data[i]
      if at == 15:
        polyBlock(p.h, p.r, p.buf, 1'u32 shl 24)
        p.bufLen = 0
      else:
        p.bufLen = at + 1

  func padTo16(p: var Poly1305) =
    ## The AEAD pads each part with zeros to a whole block. Flushing the partial
    ## block as a FULL block (hibit set, zeros after) is exactly that.
    if p.bufLen > 0:
      var n: int = p.bufLen
      while n < 16:
        p.buf[n] = 0'u8
        inc n
      polyBlock(p.h, p.r, p.buf, 1'u32 shl 24)
      p.bufLen = 0

  func finish*(p: Poly1305): Poly1305Tag =
    var st = p
    if st.bufLen > 0:
      # A short final block: append 1, pad with zeros, and no high bit.
      var n: int = st.bufLen
      st.buf[n] = 1'u8
      inc n
      while n < 16:
        st.buf[n] = 0'u8
        inc n
      polyBlock(st.h, st.r, st.buf, 0'u32)
    var h0 = st.h[0]
    var h1 = st.h[1]
    var h2 = st.h[2]
    var h3 = st.h[3]
    var h4 = st.h[4]
    var c = h1 shr 26; h1 = h1 and 0x3ffffff'u32
    h2 = h2 + c; c = h2 shr 26; h2 = h2 and 0x3ffffff'u32
    h3 = h3 + c; c = h3 shr 26; h3 = h3 and 0x3ffffff'u32
    h4 = h4 + c; c = h4 shr 26; h4 = h4 and 0x3ffffff'u32
    h0 = h0 + c * 5; c = h0 shr 26; h0 = h0 and 0x3ffffff'u32
    h1 = h1 + c
    # g = h + -p, selected without a branch
    var g0 = h0 + 5; c = g0 shr 26; g0 = g0 and 0x3ffffff'u32
    var g1 = h1 + c; c = g1 shr 26; g1 = g1 and 0x3ffffff'u32
    var g2 = h2 + c; c = g2 shr 26; g2 = g2 and 0x3ffffff'u32
    var g3 = h3 + c; c = g3 shr 26; g3 = g3 and 0x3ffffff'u32
    let g4 = h4 + c - (1'u32 shl 26)
    var mask = (g4 shr 31) - 1'u32        # all ones iff h >= p
    g0 = g0 and mask
    g1 = g1 and mask
    g2 = g2 and mask
    g3 = g3 and mask
    let g4m = g4 and mask
    mask = not mask
    h0 = (h0 and mask) or g0
    h1 = (h1 and mask) or g1
    h2 = (h2 and mask) or g2
    h3 = (h3 and mask) or g3
    h4 = (h4 and mask) or g4m
    # h = h % 2^128, as four 32-bit words
    let w0 = h0 or (h1 shl 26)
    let w1 = (h1 shr 6) or (h2 shl 20)
    let w2 = (h2 shr 12) or (h3 shl 14)
    let w3 = (h3 shr 18) or (h4 shl 8)
    # mac = (h + pad) % 2^128
    var f = uint64(w0) + uint64(st.pad[0])
    let o0 = uint32(f and 0xffffffff'u64)
    f = uint64(w1) + uint64(st.pad[1]) + (f shr 32)
    let o1 = uint32(f and 0xffffffff'u64)
    f = uint64(w2) + uint64(st.pad[2]) + (f shr 32)
    let o2 = uint32(f and 0xffffffff'u64)
    f = uint64(w3) + uint64(st.pad[3]) + (f shr 32)
    let o3 = uint32(f and 0xffffffff'u64)
    result = default(Poly1305Tag)
    let words = [o0, o1, o2, o3]
    for i in 0 ..< 4:
      let o = i * 4
      result[o] = byte(words[i] and 0xFF'u32)
      result[o+1] = byte((words[i] shr 8) and 0xFF'u32)
      result[o+2] = byte((words[i] shr 16) and 0xFF'u32)
      result[o+3] = byte(words[i] shr 24)

func poly1305*(key: array[32, byte]; msg: openArray[byte]): Poly1305Tag =
  var p = initPoly1305(key)
  p.update(msg)
  result = p.finish()

# ------------------------------------------------------------------- AEAD ---

func le64(x: uint64): array[8, byte] =
  result = default(array[8, byte])
  for i in 0 ..< 8: result[i] = byte((x shr (8 * i)) and 0xFF'u64)

func aeadTag(key: ChaChaKey; nonce: ChaChaNonce;
             aad, ciphertext: openArray[byte]): Poly1305Tag =
  var block0 = default(array[64, byte])
  chachaBlock(key, 0'u32, nonce, block0)
  var otk = default(array[32, byte])
  for i in 0 ..< 32: otk[i] = block0[i]
  var p = initPoly1305(otk)
  p.update(aad)
  p.padTo16()
  p.update(ciphertext)
  p.padTo16()
  p.update(le64(uint64(aad.len)))
  p.update(le64(uint64(ciphertext.len)))
  result = p.finish()

func chacha20Poly1305Seal*(key: ChaChaKey; nonce: ChaChaNonce;
                           aad: openArray[byte]; data: var openArray[byte]): Poly1305Tag =
  ## Encrypts `data` in place and returns the tag that authenticates it
  ## together with `aad`.
  chacha20Xor(key, 1'u32, nonce, data)
  result = aeadTag(key, nonce, aad, data)

func chacha20Poly1305Open*(key: ChaChaKey; nonce: ChaChaNonce;
                           aad: openArray[byte]; data: var openArray[byte];
                           tag: openArray[byte]): bool =
  ## Checks `tag` and, only if it is right, decrypts `data` in place. On
  ## `false` the ciphertext is left untouched: nothing derived from an
  ## unauthenticated message ever reaches the caller.
  let expected = aeadTag(key, nonce, aad, data)
  if not constantTimeEquals(expected, tag): return false
  chacha20Xor(key, 1'u32, nonce, data)
  result = true
