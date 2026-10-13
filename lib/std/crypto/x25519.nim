#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## X25519 Diffie-Hellman (RFC 7748).
##
## The field is GF(2^255 - 19) in sixteen 16-bit limbs held in `int64`s, after
## TweetNaCl: every product fits, carries are arithmetic shifts, and the
## Montgomery ladder swaps with a mask. Nothing branches on or indexes by a
## secret, so the scalar does not leak through timing.

{.feature: "staticContracts".}

type
  X25519Key* = array[32, byte]

const
  BasePoint*: X25519Key = [9'u8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

when defined(nimNativeBackend) and defined(amd64):
  import std/crypto/private/fe25519x64

  # The field arithmetic is hand-written assembler (see the module), which is
  # what makes the ladder's timing independent of the scalar on this backend.
  type
    Fe = Fe64

  proc fp(x: var Fe): FePtr {.inline.} = cast[FePtr](addr x)

  func add(a, b: Fe): Fe =
    var x = a
    var y = b
    result = default(Fe)
    {.cast(noSideEffect).}:
      feAdd(fp(result), fp(x), fp(y))

  func sub(a, b: Fe): Fe =
    var x = a
    var y = b
    result = default(Fe)
    {.cast(noSideEffect).}:
      feSub(fp(result), fp(x), fp(y))

  func mul(a, b: Fe): Fe =
    var x = a
    var y = b
    result = default(Fe)
    {.cast(noSideEffect).}:
      feMul(fp(result), fp(x), fp(y))

  func sqr(a: Fe): Fe {.inline.} = mul(a, a)

  func mul121665(a: Fe): Fe =
    var x = a
    result = default(Fe)
    {.cast(noSideEffect).}:
      feMulSmall(fp(result), fp(x), 121665'u64)

  func cswap(p, q: var Fe; b: int64) =
    {.cast(noSideEffect).}:
      feCswap(fp(p), fp(q), uint64(b))

  func unpack(o: var Fe; n: X25519Key) =
    for i in 0 ..< 4:
      var w = 0'u64
      for k in 0 ..< 8:
        w = w or (uint64(n[8*i + k]) shl (8 * k))
      o[i] = w
    o[3] = o[3] and 0x7FFF_FFFF_FFFF_FFFF'u64

  func pack(o: var X25519Key; n: Fe) =
    var x = n
    var t = default(Fe)
    {.cast(noSideEffect).}:
      feFreeze(fp(t), fp(x))
    for i in 0 ..< 4:
      for k in 0 ..< 8:
        o[8*i + k] = byte((t[i] shr (8 * k)) and 0xFF'u64)

  func inv(i: Fe): Fe =
    ## i^(p-2), by the fixed square-and-multiply chain of p - 2 = 2^255 - 21.
    result = i
    var a = 253
    while a >= 0:
      result = sqr(result)
      if a != 2 and a != 4:
        result = mul(result, i)
      dec a

else:
  type
    Fe = array[16, int64]

  const
    Fe121665: Fe = [0xDB41'i64, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

  func carry(o: var Fe) =
    for i in 0 ..< 16:
      o[i] = o[i] + (1'i64 shl 16)
      let c = ashr(o[i], 16)
      if i < 15:
        o[i+1] = o[i+1] + c - 1
      else:
        o[0] = o[0] + 38 * (c - 1)
      o[i] = o[i] - (c shl 16)

  func cswap(p, q: var Fe; b: int64) =
    ## Swaps `p` and `q` iff `b == 1`, with the same memory traffic either way.
    let mask = not (b - 1)
    for i in 0 ..< 16:
      let t = mask and (p[i] xor q[i])
      p[i] = p[i] xor t
      q[i] = q[i] xor t

  func pack(o: var X25519Key; n: Fe) =
    var t = n
    carry(t)
    carry(t)
    carry(t)
    var m = default(Fe)
    for j in 0 ..< 2:
      m[0] = t[0] - 0xffed
      for i in 1 ..< 15:
        m[i] = t[i] - 0xffff - (ashr(m[i-1], 16) and 1)
        m[i-1] = m[i-1] and 0xffff
      m[15] = t[15] - 0x7fff - (ashr(m[14], 16) and 1)
      let b = ashr(m[15], 16) and 1
      m[14] = m[14] and 0xffff
      cswap(t, m, 1 - b)
    for i in 0 ..< 16:
      o[2*i] = byte(t[i] and 0xff)
      o[2*i+1] = byte(ashr(t[i], 8) and 0xff)

  func unpack(o: var Fe; n: X25519Key) =
    for i in 0 ..< 16:
      o[i] = int64(n[2*i]) + (int64(n[2*i+1]) shl 8)
    o[15] = o[15] and 0x7fff

  func add(a, b: Fe): Fe =
    result = default(Fe)
    for i in 0 ..< 16: result[i] = a[i] + b[i]

  func sub(a, b: Fe): Fe =
    result = default(Fe)
    for i in 0 ..< 16: result[i] = a[i] - b[i]

  func mul(a, b: Fe): Fe =
    var t = default(array[31, int64])
    for i in 0 ..< 16:
      for j in 0 ..< 16:
        t[i+j] = t[i+j] + a[i] * b[j]
    for i in 0 ..< 15:
      t[i] = t[i] + 38 * t[i+16]
    result = default(Fe)
    for i in 0 ..< 16: result[i] = t[i]
    carry(result)
    carry(result)

  func sqr(a: Fe): Fe {.inline.} = mul(a, a)

  func inv(i: Fe): Fe =
    ## i^(p-2), by the fixed square-and-multiply chain of p - 2 = 2^255 - 21.
    result = i
    var a = 253
    while a >= 0:
      result = sqr(result)
      if a != 2 and a != 4:
        result = mul(result, i)
      dec a

  func mul121665(a: Fe): Fe {.inline.} = mul(a, Fe121665)


func x25519*(scalar, point: X25519Key): X25519Key =
  ## `scalar * point` on Curve25519: the shared secret when `point` is the
  ## peer's public key, the public key when it is `BasePoint`.
  var z = scalar
  z[31] = (z[31] and 127'u8) or 64'u8
  z[0] = z[0] and 248'u8
  var x = default(Fe)
  unpack(x, point)
  var a = default(Fe)
  var b = x
  var c = default(Fe)
  var d = default(Fe)
  var e = default(Fe)
  var f = default(Fe)
  a[0] = 1
  d[0] = 1
  var i = 254
  while i >= 0:
    let r = int64((z[i shr 3] shr (i and 7)) and 1'u8)
    cswap(a, b, r)
    cswap(c, d, r)
    e = add(a, c)
    a = sub(a, c)
    c = add(b, d)
    b = sub(b, d)
    d = sqr(e)
    f = sqr(a)
    a = mul(c, a)
    c = mul(b, e)
    e = add(a, c)
    a = sub(a, c)
    b = sqr(a)
    c = sub(d, f)
    a = mul121665(c)
    a = add(a, d)
    c = mul(c, a)
    a = mul(d, f)
    d = mul(b, x)
    b = sqr(e)
    cswap(a, b, r)
    cswap(c, d, r)
    dec i
  let r = mul(a, inv(c))
  result = default(X25519Key)
  pack(result, r)

func x25519Public*(secret: X25519Key): X25519Key {.inline.} =
  x25519(secret, BasePoint)

func isAllZero*(k: X25519Key): bool =
  ## RFC 7748 §6.1: a peer that sends a small-order point makes the shared
  ## secret all zeros. Checked without an early exit.
  var acc = 0'u8
  for i in 0 ..< 32: acc = acc or k[i]
  result = acc == 0'u8
