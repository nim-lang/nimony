#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## HMAC (RFC 2104) and HKDF (RFC 5869) over SHA-256 and SHA-384.
##
## HKDF is the whole of TLS 1.3's key schedule; `std/tls` builds its
## `HKDF-Expand-Label` on `hkdfExpand` here.

{.feature: "staticContracts".}

import std/crypto/sha2

type
  HmacSha256* = object
    ## Incremental HMAC-SHA-256: `initHmacSha256`, any number of `update`s,
    ## `finish`.
    inner, outer: Sha256

  HmacSha384* = object
    inner, outer: Sha512

const
  Sha256Block = 64
  Sha384Block = 128

func initHmacSha256*(key: openArray[byte]): HmacSha256 =
  var k = default(array[Sha256Block, byte])
  if key.len > Sha256Block:
    let d = sha256(key)
    for i in 0 ..< d.len: k[i] = d[i]
  else:
    for i in 0 ..< key.len: k[i] = key[i]
  var ipad = default(array[Sha256Block, byte])
  var opad = default(array[Sha256Block, byte])
  for i in 0 ..< Sha256Block:
    ipad[i] = k[i] xor 0x36'u8
    opad[i] = k[i] xor 0x5c'u8
  result = HmacSha256(inner: initSha256(), outer: initSha256())
  result.inner.update(ipad)
  result.outer.update(opad)

func update*(h: var HmacSha256; data: openArray[byte]) {.inline.} =
  h.inner.update(data)

func finish*(h: HmacSha256): Sha256Digest =
  var o = h.outer
  o.update(h.inner.finish())
  result = o.finish()

func hmacSha256*(key, data: openArray[byte]): Sha256Digest =
  var h = initHmacSha256(key)
  h.update(data)
  result = h.finish()

func initHmacSha384*(key: openArray[byte]): HmacSha384 =
  var k = default(array[Sha384Block, byte])
  if key.len > Sha384Block:
    let d = sha384(key)
    for i in 0 ..< d.len: k[i] = d[i]
  else:
    for i in 0 ..< key.len: k[i] = key[i]
  var ipad = default(array[Sha384Block, byte])
  var opad = default(array[Sha384Block, byte])
  for i in 0 ..< Sha384Block:
    ipad[i] = k[i] xor 0x36'u8
    opad[i] = k[i] xor 0x5c'u8
  result = HmacSha384(inner: initSha384(), outer: initSha384())
  result.inner.update(ipad)
  result.outer.update(opad)

func update*(h: var HmacSha384; data: openArray[byte]) {.inline.} =
  h.inner.update(data)

func finish*(h: HmacSha384): Sha384Digest =
  var o = h.outer
  o.update(h.inner.finish384())
  result = o.finish384()

func hmacSha384*(key, data: openArray[byte]): Sha384Digest =
  var h = initHmacSha384(key)
  h.update(data)
  result = h.finish()

# ------------------------------------------------------------------ HKDF ----

func hkdfExtractSha256*(salt, ikm: openArray[byte]): Sha256Digest =
  ## `HKDF-Extract(salt, IKM)`. An empty salt means a string of zeros the
  ## length of the hash, which HMAC's key padding makes the same thing.
  result = hmacSha256(salt, ikm)

func hkdfExpandSha256*(prk, info: openArray[byte]; outLen: int): seq[byte] {.
    requires: outLen >= 0 and outLen <= 255 * 32.} =
  ## `HKDF-Expand(PRK, info, L)`.
  result = newSeq[byte](outLen)
  var t = default(Sha256Digest)
  var tLen = 0
  var counter = 1'u8
  var pos = 0
  while pos < result.len:
    var h = initHmacSha256(prk)
    if tLen > 0: h.update(t)
    h.update(info)
    h.update([counter])
    t = h.finish()
    tLen = t.len
    var j = 0
    while j < t.len and pos < result.len:
      result[pos] = t[j]
      inc pos
      inc j
    if counter < 255'u8: inc counter

func hkdfExtractSha384*(salt, ikm: openArray[byte]): Sha384Digest =
  result = hmacSha384(salt, ikm)

func hkdfExpandSha384*(prk, info: openArray[byte]; outLen: int): seq[byte] {.
    requires: outLen >= 0 and outLen <= 255 * 48.} =
  result = newSeq[byte](outLen)
  var t = default(Sha384Digest)
  var tLen = 0
  var counter = 1'u8
  var pos = 0
  while pos < result.len:
    var h = initHmacSha384(prk)
    if tLen > 0: h.update(t)
    h.update(info)
    h.update([counter])
    t = h.finish()
    tLen = t.len
    var j = 0
    while j < t.len and pos < result.len:
      result[pos] = t[j]
      inc pos
      inc j
    if counter < 255'u8: inc counter

# ------------------------------------------------------- constant time ------

func constantTimeEquals*(a, b: openArray[byte]): bool =
  ## Whether `a` and `b` hold the same bytes, in time that depends only on
  ## their lengths. A MAC check must use this: an early `return false` tells a
  ## forger how many leading bytes of a guess were right.
  if a.len != b.len: return false
  var acc = 0'u8
  for i in 0 ..< a.len:
    acc = acc or (a[i] xor b[i])
  result = acc == 0'u8
