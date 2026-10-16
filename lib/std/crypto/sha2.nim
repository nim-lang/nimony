#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## SHA-2 (FIPS 180-4): SHA-256, SHA-384 and SHA-512.
##
## SHA-256 is the transcript hash and the HKDF hash of the TLS 1.3 suites
## `std/tls` speaks; SHA-384 and SHA-512 are what certificate signatures are
## computed over. None of them branches on or indexes by the data, so the
## straightforward implementation is already constant time.

{.feature: "staticContracts".}

runnableExamples:
  doAssert toHex(sha256("abc")) ==
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

type
  Sha256Digest* = array[32, byte]
  Sha384Digest* = array[48, byte]
  Sha512Digest* = array[64, byte]

  Sha256* = object
    ## Incremental SHA-256. Copyable: a copy is a snapshot of the hash so far,
    ## which is how a TLS transcript hash is read in the middle of a handshake.
    h: array[8, uint32]
    buf: array[64, byte]
    bufLen: range[0..63]
    total: uint64             ## bytes fed so far

  Sha512* = object
    ## Incremental SHA-512; also SHA-384, which differs only in its initial
    ## state and in how much of the final state it outputs.
    h: array[8, uint64]
    buf: array[128, byte]
    bufLen: range[0..127]
    total: uint64             ## bytes fed so far; 2^64 bytes is out of reach

const
  K256: array[64, uint32] = [
    0x428a2f98'u32, 0x71374491'u32, 0xb5c0fbcf'u32, 0xe9b5dba5'u32,
    0x3956c25b'u32, 0x59f111f1'u32, 0x923f82a4'u32, 0xab1c5ed5'u32,
    0xd807aa98'u32, 0x12835b01'u32, 0x243185be'u32, 0x550c7dc3'u32,
    0x72be5d74'u32, 0x80deb1fe'u32, 0x9bdc06a7'u32, 0xc19bf174'u32,
    0xe49b69c1'u32, 0xefbe4786'u32, 0x0fc19dc6'u32, 0x240ca1cc'u32,
    0x2de92c6f'u32, 0x4a7484aa'u32, 0x5cb0a9dc'u32, 0x76f988da'u32,
    0x983e5152'u32, 0xa831c66d'u32, 0xb00327c8'u32, 0xbf597fc7'u32,
    0xc6e00bf3'u32, 0xd5a79147'u32, 0x06ca6351'u32, 0x14292967'u32,
    0x27b70a85'u32, 0x2e1b2138'u32, 0x4d2c6dfc'u32, 0x53380d13'u32,
    0x650a7354'u32, 0x766a0abb'u32, 0x81c2c92e'u32, 0x92722c85'u32,
    0xa2bfe8a1'u32, 0xa81a664b'u32, 0xc24b8b70'u32, 0xc76c51a3'u32,
    0xd192e819'u32, 0xd6990624'u32, 0xf40e3585'u32, 0x106aa070'u32,
    0x19a4c116'u32, 0x1e376c08'u32, 0x2748774c'u32, 0x34b0bcb5'u32,
    0x391c0cb3'u32, 0x4ed8aa4a'u32, 0x5b9cca4f'u32, 0x682e6ff3'u32,
    0x748f82ee'u32, 0x78a5636f'u32, 0x84c87814'u32, 0x8cc70208'u32,
    0x90befffa'u32, 0xa4506ceb'u32, 0xbef9a3f7'u32, 0xc67178f2'u32]

  K512: array[80, uint64] = [
    0x428a2f98d728ae22'u64, 0x7137449123ef65cd'u64, 0xb5c0fbcfec4d3b2f'u64,
    0xe9b5dba58189dbbc'u64, 0x3956c25bf348b538'u64, 0x59f111f1b605d019'u64,
    0x923f82a4af194f9b'u64, 0xab1c5ed5da6d8118'u64, 0xd807aa98a3030242'u64,
    0x12835b0145706fbe'u64, 0x243185be4ee4b28c'u64, 0x550c7dc3d5ffb4e2'u64,
    0x72be5d74f27b896f'u64, 0x80deb1fe3b1696b1'u64, 0x9bdc06a725c71235'u64,
    0xc19bf174cf692694'u64, 0xe49b69c19ef14ad2'u64, 0xefbe4786384f25e3'u64,
    0x0fc19dc68b8cd5b5'u64, 0x240ca1cc77ac9c65'u64, 0x2de92c6f592b0275'u64,
    0x4a7484aa6ea6e483'u64, 0x5cb0a9dcbd41fbd4'u64, 0x76f988da831153b5'u64,
    0x983e5152ee66dfab'u64, 0xa831c66d2db43210'u64, 0xb00327c898fb213f'u64,
    0xbf597fc7beef0ee4'u64, 0xc6e00bf33da88fc2'u64, 0xd5a79147930aa725'u64,
    0x06ca6351e003826f'u64, 0x142929670a0e6e70'u64, 0x27b70a8546d22ffc'u64,
    0x2e1b21385c26c926'u64, 0x4d2c6dfc5ac42aed'u64, 0x53380d139d95b3df'u64,
    0x650a73548baf63de'u64, 0x766a0abb3c77b2a8'u64, 0x81c2c92e47edaee6'u64,
    0x92722c851482353b'u64, 0xa2bfe8a14cf10364'u64, 0xa81a664bbc423001'u64,
    0xc24b8b70d0f89791'u64, 0xc76c51a30654be30'u64, 0xd192e819d6ef5218'u64,
    0xd69906245565a910'u64, 0xf40e35855771202a'u64, 0x106aa07032bbd1b8'u64,
    0x19a4c116b8d2d0c8'u64, 0x1e376c085141ab53'u64, 0x2748774cdf8eeb99'u64,
    0x34b0bcb5e19b48a8'u64, 0x391c0cb3c5c95a63'u64, 0x4ed8aa4ae3418acb'u64,
    0x5b9cca4f7763e373'u64, 0x682e6ff3d6b2b8a3'u64, 0x748f82ee5defb2fc'u64,
    0x78a5636f43172f60'u64, 0x84c87814a1f0ab72'u64, 0x8cc702081a6439ec'u64,
    0x90befffa23631e28'u64, 0xa4506cebde82bde9'u64, 0xbef9a3f7b2c67915'u64,
    0xc67178f2e372532b'u64, 0xca273eceea26619c'u64, 0xd186b8c721c0c207'u64,
    0xeada7dd6cde0eb1e'u64, 0xf57d4f7fee6ed178'u64, 0x06f067aa72176fba'u64,
    0x0a637dc5a2c898a6'u64, 0x113f9804bef90dae'u64, 0x1b710b35131c471b'u64,
    0x28db77f523047d84'u64, 0x32caab7b40c72493'u64, 0x3c9ebe0a15c9bebc'u64,
    0x431d67c49c100d4c'u64, 0x4cc5d4becb3e42b6'u64, 0x597f299cfc657e2a'u64,
    0x5fcb6fab3ad6faec'u64, 0x6c44198c4a475817'u64]

func rotr32(x: uint32; n: int): uint32 {.inline.} =
  (x shr n) or (x shl (32 - n))

func rotr64(x: uint64; n: int): uint64 {.inline.} =
  (x shr n) or (x shl (64 - n))

# ---------------------------------------------------------------- SHA-256 ---

func compress256(h: var array[8, uint32]; data: array[64, byte]) =
  var w = default(array[64, uint32])
  for j in 0 ..< 16:
    let o = j * 4
    w[j] = (uint32(data[o]) shl 24) or (uint32(data[o+1]) shl 16) or
           (uint32(data[o+2]) shl 8) or uint32(data[o+3])
  for j in 16 ..< 64:
    let s0 = rotr32(w[j-15], 7) xor rotr32(w[j-15], 18) xor (w[j-15] shr 3)
    let s1 = rotr32(w[j-2], 17) xor rotr32(w[j-2], 19) xor (w[j-2] shr 10)
    w[j] = w[j-16] + s0 + w[j-7] + s1
  var a = h[0]
  var b = h[1]
  var c = h[2]
  var d = h[3]
  var e = h[4]
  var f = h[5]
  var g = h[6]
  var hh = h[7]
  for j in 0 ..< 64:
    let S1 = rotr32(e, 6) xor rotr32(e, 11) xor rotr32(e, 25)
    let ch = (e and f) xor ((not e) and g)
    let t1 = hh + S1 + ch + K256[j] + w[j]
    let S0 = rotr32(a, 2) xor rotr32(a, 13) xor rotr32(a, 22)
    let maj = (a and b) xor (a and c) xor (b and c)
    let t2 = S0 + maj
    hh = g
    g = f
    f = e
    e = d + t1
    d = c
    c = b
    b = a
    a = t1 + t2
  h[0] = h[0] + a
  h[1] = h[1] + b
  h[2] = h[2] + c
  h[3] = h[3] + d
  h[4] = h[4] + e
  h[5] = h[5] + f
  h[6] = h[6] + g
  h[7] = h[7] + hh

func initSha256*(): Sha256 =
  result = Sha256(h: [0x6a09e667'u32, 0xbb67ae85'u32, 0x3c6ef372'u32, 0xa54ff53a'u32,
                      0x510e527f'u32, 0x9b05688c'u32, 0x1f83d9ab'u32, 0x5be0cd19'u32],
                  buf: default(array[64, byte]), bufLen: 0, total: 0'u64)

func update*(s: var Sha256; data: openArray[byte]) =
  s.total = s.total + uint64(data.len)
  for i in 0 ..< data.len:
    let at = s.bufLen
    s.buf[at] = data[i]
    if at == 63:
      compress256(s.h, s.buf)
      s.bufLen = 0
    else:
      s.bufLen = at + 1

func update*(s: var Sha256; data: string) =
  s.total = s.total + uint64(data.len)
  for i in 0 ..< data.len:
    let at = s.bufLen
    s.buf[at] = byte(data[i])
    if at == 63:
      compress256(s.h, s.buf)
      s.bufLen = 0
    else:
      s.bufLen = at + 1

func finish*(s: Sha256): Sha256Digest =
  ## The digest of everything fed so far. `s` is not consumed — that is what
  ## lets a TLS transcript hash be read and then extended.
  var st = s
  let bitLen = st.total * 8'u64
  var n: int = st.bufLen
  st.buf[n] = 0x80'u8
  inc n
  if n > 56:
    while n < 64:
      st.buf[n] = 0'u8
      inc n
    compress256(st.h, st.buf)
    n = 0
  while n < 56:
    st.buf[n] = 0'u8
    inc n
  for i in 0 ..< 8:
    st.buf[56 + i] = byte((bitLen shr (8 * (7 - i))) and 0xFF'u64)
  compress256(st.h, st.buf)
  result = default(Sha256Digest)
  for j in 0 ..< 8:
    let o = j * 4
    result[o] = byte(st.h[j] shr 24)
    result[o+1] = byte((st.h[j] shr 16) and 0xFF'u32)
    result[o+2] = byte((st.h[j] shr 8) and 0xFF'u32)
    result[o+3] = byte(st.h[j] and 0xFF'u32)

func sha256*(data: openArray[byte]): Sha256Digest =
  var s = initSha256()
  s.update(data)
  result = s.finish()

func sha256*(data: string): Sha256Digest =
  var s = initSha256()
  s.update(data)
  result = s.finish()

# ---------------------------------------------------------------- SHA-512 ---

func compress512(h: var array[8, uint64]; data: array[128, byte]) =
  var w = default(array[80, uint64])
  for j in 0 ..< 16:
    let o = j * 8
    var x = 0'u64
    for k in 0 ..< 8:
      x = (x shl 8) or uint64(data[o + k])
    w[j] = x
  for j in 16 ..< 80:
    let s0 = rotr64(w[j-15], 1) xor rotr64(w[j-15], 8) xor (w[j-15] shr 7)
    let s1 = rotr64(w[j-2], 19) xor rotr64(w[j-2], 61) xor (w[j-2] shr 6)
    w[j] = w[j-16] + s0 + w[j-7] + s1
  var a = h[0]
  var b = h[1]
  var c = h[2]
  var d = h[3]
  var e = h[4]
  var f = h[5]
  var g = h[6]
  var hh = h[7]
  for j in 0 ..< 80:
    let S1 = rotr64(e, 14) xor rotr64(e, 18) xor rotr64(e, 41)
    let ch = (e and f) xor ((not e) and g)
    let t1 = hh + S1 + ch + K512[j] + w[j]
    let S0 = rotr64(a, 28) xor rotr64(a, 34) xor rotr64(a, 39)
    let maj = (a and b) xor (a and c) xor (b and c)
    let t2 = S0 + maj
    hh = g
    g = f
    f = e
    e = d + t1
    d = c
    c = b
    b = a
    a = t1 + t2
  h[0] = h[0] + a
  h[1] = h[1] + b
  h[2] = h[2] + c
  h[3] = h[3] + d
  h[4] = h[4] + e
  h[5] = h[5] + f
  h[6] = h[6] + g
  h[7] = h[7] + hh

func initSha512*(): Sha512 =
  result = Sha512(h: [0x6a09e667f3bcc908'u64, 0xbb67ae8584caa73b'u64,
                      0x3c6ef372fe94f82b'u64, 0xa54ff53a5f1d36f1'u64,
                      0x510e527fade682d1'u64, 0x9b05688c2b3e6c1f'u64,
                      0x1f83d9abfb41bd6b'u64, 0x5be0cd19137e2179'u64],
                  buf: default(array[128, byte]), bufLen: 0, total: 0'u64)

func initSha384*(): Sha512 =
  ## SHA-384 is SHA-512 from another starting point, truncated; feed it with
  ## `update` and read it with `finish384`.
  result = Sha512(h: [0xcbbb9d5dc1059ed8'u64, 0x629a292a367cd507'u64,
                      0x9159015a3070dd17'u64, 0x152fecd8f70e5939'u64,
                      0x67332667ffc00b31'u64, 0x8eb44a8768581511'u64,
                      0xdb0c2e0d64f98fa7'u64, 0x47b5481dbefa4fa4'u64],
                  buf: default(array[128, byte]), bufLen: 0, total: 0'u64)

func update*(s: var Sha512; data: openArray[byte]) =
  s.total = s.total + uint64(data.len)
  for i in 0 ..< data.len:
    let at = s.bufLen
    s.buf[at] = data[i]
    if at == 127:
      compress512(s.h, s.buf)
      s.bufLen = 0
    else:
      s.bufLen = at + 1

func update*(s: var Sha512; data: string) =
  s.total = s.total + uint64(data.len)
  for i in 0 ..< data.len:
    let at = s.bufLen
    s.buf[at] = byte(data[i])
    if at == 127:
      compress512(s.h, s.buf)
      s.bufLen = 0
    else:
      s.bufLen = at + 1

func finalState(s: Sha512): array[8, uint64] =
  var st = s
  let bitLen = st.total * 8'u64
  var n: int = st.bufLen
  st.buf[n] = 0x80'u8
  inc n
  if n > 112:
    while n < 128:
      st.buf[n] = 0'u8
      inc n
    compress512(st.h, st.buf)
    n = 0
  # The length field is 128 bits; its upper half is zero for any input that
  # fits in memory.
  while n < 120:
    st.buf[n] = 0'u8
    inc n
  for i in 0 ..< 8:
    st.buf[120 + i] = byte((bitLen shr (8 * (7 - i))) and 0xFF'u64)
  compress512(st.h, st.buf)
  result = st.h

func finish*(s: Sha512): Sha512Digest =
  let h = finalState(s)
  result = default(Sha512Digest)
  for j in 0 ..< 8:
    for k in 0 ..< 8:
      result[j * 8 + k] = byte((h[j] shr (8 * (7 - k))) and 0xFF'u64)

func finish384*(s: Sha512): Sha384Digest =
  let h = finalState(s)
  result = default(Sha384Digest)
  for j in 0 ..< 6:
    for k in 0 ..< 8:
      result[j * 8 + k] = byte((h[j] shr (8 * (7 - k))) and 0xFF'u64)

func sha512*(data: openArray[byte]): Sha512Digest =
  var s = initSha512()
  s.update(data)
  result = s.finish()

func sha384*(data: openArray[byte]): Sha384Digest =
  var s = initSha384()
  s.update(data)
  result = s.finish384()

func sha512*(data: string): Sha512Digest =
  var s = initSha512()
  s.update(data)
  result = s.finish()

func sha384*(data: string): Sha384Digest =
  var s = initSha384()
  s.update(data)
  result = s.finish384()

# ------------------------------------------------------------------ hex -----

func toHex*(d: openArray[byte]): string =
  ## Lower-case hex, the way digests are written in test vectors.
  const HexChars = "0123456789abcdef"
  result = newStringOfCap(d.len * 2)
  for i in 0 ..< d.len:
    result.add HexChars[int(d[i] shr 4)]
    result.add HexChars[int(d[i] and 0x0F'u8)]
