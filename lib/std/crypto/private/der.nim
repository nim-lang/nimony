#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## A strict reader for DER (X.690) elements, shared by `std/crypto/x509` and
## `std/crypto/rsasign`. It reads one tag-length-value element at a time and
## never copies: a `Tlv` is a pair of offsets into the caller's buffer.
##
## Only the definite length forms DER allows are accepted, and lengths below
## 2^24. Lengths and offsets are public in every use here; the content bytes
## are not looked at, so this module is safe on a buffer holding a secret.

{.feature: "staticContracts".}

type
  Tlv* = object
    tag*: byte
    start*: int               ## first content byte
    len*: int                 ## content length
    next*: int                ## first byte after the element

const
  TagBool* = 0x01'u8
  TagInt* = 0x02'u8
  TagBitString* = 0x03'u8
  TagOctetString* = 0x04'u8
  TagNull* = 0x05'u8
  TagOid* = 0x06'u8
  TagUtcTime* = 0x17'u8
  TagGenTime* = 0x18'u8
  TagSeq* = 0x30'u8
  TagSet* = 0x31'u8

  OidRsaEncryption* = [0x2a'u8, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x01]
    ## `rsaEncryption` (1.2.840.113549.1.1.1), the key OID of RSA keys.

func readTlv*(d: openArray[byte]; pos, limit: int; t: var Tlv): bool =
  ## One DER element at `pos`, which must end at or before `limit`. Only the
  ## definite length forms DER allows, and lengths below 2^24.
  result = false
  if limit > d.len or limit < 2 or pos < 0 or pos > limit - 2: return
  let tag = d[pos]
  let b = int(d[pos + 1])
  var p = pos + 2
  var L = 0
  if b < 0x80:
    L = b
  else:
    let n = b - 0x80
    if n < 1 or n > 3: return
    # a leading zero length octet is not minimal: DER forbids it
    if p >= limit or d[p] == 0'u8: return
    for k in 0 ..< n:
      if p < 0 or p >= limit: return
      L = L * 256 + int(d[p])
      inc p
    if L < 0x80: return            # not minimal: DER forbids it
  if L < 0 or L > limit - p: return
  t = Tlv(tag: tag, start: p, len: L, next: p + L)
  result = true

func bytesOf*(d: openArray[byte]; start, len: int): seq[byte] =
  ## A copy of `d[start ..< start + len]`; positions outside `d` are skipped.
  result = @[]
  for i in start ..< start + len:
    if i >= 0 and i < d.len: result.add d[i]

func sameBytes*(d: openArray[byte]; start, len: int; lit: openArray[byte]): bool =
  ## Whether `d[start ..< start + len]` equals `lit`. Variable time: for
  ## public data such as OIDs only.
  if len != lit.len: return false
  result = true
  for i in 0 ..< lit.len:
    let j = start + i
    if j < 0 or j >= d.len or d[j] != lit[i]: return false
