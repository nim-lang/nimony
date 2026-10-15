#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## X.509 certificates (RFC 5280), as much of them as a TLS client needs to
## decide whether to believe a server: DER parsing, signature checks, chain
## building to a trust anchor, validity periods, and host name matching
## (RFC 6125).
##
## The parser is strict and small. Anything it does not understand that is
## marked *critical* makes the certificate unusable, as RFC 5280 requires;
## anything else is skipped.

{.feature: "staticContracts".}

import std/[strutils, base64]
import std/crypto/[sha2, rsa, ecdsa]

type
  KeyKind* = enum
    kkNone, kkRsa, kkEcP256, kkEcP384

  PublicKey* = object
    kind*: KeyKind
    rsa*: RsaPublicKey
    ecPoint*: seq[byte]       ## uncompressed SEC1 point

  SigAlg* = enum
    saUnknown,
    saRsaSha256, saRsaSha384, saRsaSha512,
    saEcdsaSha256, saEcdsaSha384, saEcdsaSha512

  Certificate* = object
    der*: seq[byte]           ## the whole certificate
    tbsStart, tbsLen: int     ## the signed part, as a slice of `der`
    sigAlg*: SigAlg
    signature*: seq[byte]
    issuer*, subject*: seq[byte]   ## raw DER of the two Names
    notBefore*, notAfter*: int64   ## seconds since the Unix epoch
    publicKey*: PublicKey
    isCa*: bool
    pathLen*: int             ## -1: unlimited
    hasKeyUsage*: bool
    canSignCerts*: bool       ## keyUsage keyCertSign
    hasEku*: bool
    serverAuth*: bool         ## extKeyUsage serverAuth or anyExtendedKeyUsage
    dnsNames*: seq[string]
    ipAddresses*: seq[seq[byte]]
    commonName*: string       ## for messages only; never matched against

  CertError* = enum
    ceNone = "ok"
    ceParse = "malformed certificate"
    ceExpired = "certificate expired or not yet valid"
    ceUntrusted = "no chain to a trusted root"
    ceBadSignature = "certificate signature does not verify"
    ceHostMismatch = "certificate is not valid for this host name"
    ceNotCa = "issuer is not a certificate authority"
    ceUsage = "certificate may not be used for TLS server authentication"
    ceChainTooLong = "certificate chain too long"

  TrustStore* = object
    roots*: seq[Certificate]
    system*: bool             ## the OS's own roots (`std/crypto/certstore`),
                              ## so the OS's chain engine may judge instead

# ------------------------------------------------------------- DER reader ---

type
  Tlv = object
    tag: byte
    start: int                ## first content byte
    len: int                  ## content length
    next: int                 ## first byte after the element

func readTlv(d: openArray[byte]; pos, limit: int; t: var Tlv): bool =
  ## One DER element at `pos`, which must end at or before `limit`. Only the
  ## definite length forms DER allows, and lengths below 2^24.
  result = false
  if limit > d.len or pos < 0 or pos + 2 > limit: return
  let tag = d[pos]
  let b = int(d[pos + 1])
  var p = pos + 2
  var L = 0
  if b < 0x80:
    L = b
  else:
    let n = b - 0x80
    if n < 1 or n > 3: return
    for k in 0 ..< n:
      if p < 0 or p >= limit: return
      L = L * 256 + int(d[p])
      inc p
    if L < 0x80: return            # not minimal: DER forbids it
  if L < 0 or L > limit - p: return
  t = Tlv(tag: tag, start: p, len: L, next: p + L)
  result = true

func bytesOf(d: openArray[byte]; start, len: int): seq[byte] =
  result = @[]
  for i in start ..< start + len:
    if i >= 0 and i < d.len: result.add d[i]

func sameBytes(d: openArray[byte]; start, len: int; lit: openArray[byte]): bool =
  if len != lit.len: return false
  result = true
  for i in 0 ..< lit.len:
    let j = start + i
    if j < 0 or j >= d.len or d[j] != lit[i]: return false

const
  OidRsaEncryption = [0x2a'u8, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x01]
  OidSha256Rsa = [0x2a'u8, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0b]
  OidSha384Rsa = [0x2a'u8, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0c]
  OidSha512Rsa = [0x2a'u8, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0d]
  OidEcPublicKey = [0x2a'u8, 0x86, 0x48, 0xce, 0x3d, 0x02, 0x01]
  OidP256 = [0x2a'u8, 0x86, 0x48, 0xce, 0x3d, 0x03, 0x01, 0x07]
  OidP384 = [0x2b'u8, 0x81, 0x04, 0x00, 0x22]
  OidEcdsaSha256 = [0x2a'u8, 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x02]
  OidEcdsaSha384 = [0x2a'u8, 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x03]
  OidEcdsaSha512 = [0x2a'u8, 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x04]
  OidBasicConstraints = [0x55'u8, 0x1d, 0x13]
  OidKeyUsage = [0x55'u8, 0x1d, 0x0f]
  OidSubjectAltName = [0x55'u8, 0x1d, 0x11]
  OidExtKeyUsage = [0x55'u8, 0x1d, 0x25]
  OidServerAuth = [0x2b'u8, 0x06, 0x01, 0x05, 0x05, 0x07, 0x03, 0x01]
  OidAnyEku = [0x55'u8, 0x1d, 0x25, 0x00]
  OidCommonName = [0x55'u8, 0x04, 0x03]

  TagInt = 0x02'u8
  TagBitString = 0x03'u8
  TagOctetString = 0x04'u8
  TagNull = 0x05'u8
  TagOid = 0x06'u8
  TagUtcTime = 0x17'u8
  TagGenTime = 0x18'u8
  TagSeq = 0x30'u8
  TagSet = 0x31'u8
  TagBool = 0x01'u8

func sigAlgOf(d: openArray[byte]; algId: Tlv): SigAlg =
  ## `AlgorithmIdentifier ::= SEQUENCE { OID, params }`.
  result = saUnknown
  var oid = default(Tlv)
  if algId.tag != TagSeq or not readTlv(d, algId.start, algId.next, oid): return
  if oid.tag != TagOid: return
  if sameBytes(d, oid.start, oid.len, OidSha256Rsa): result = saRsaSha256
  elif sameBytes(d, oid.start, oid.len, OidSha384Rsa): result = saRsaSha384
  elif sameBytes(d, oid.start, oid.len, OidSha512Rsa): result = saRsaSha512
  elif sameBytes(d, oid.start, oid.len, OidEcdsaSha256): result = saEcdsaSha256
  elif sameBytes(d, oid.start, oid.len, OidEcdsaSha384): result = saEcdsaSha384
  elif sameBytes(d, oid.start, oid.len, OidEcdsaSha512): result = saEcdsaSha512

func bitStringBytes(d: openArray[byte]; t: Tlv; res: var seq[byte]): bool =
  ## The content of a BIT STRING with no unused bits.
  result = false
  if t.tag != TagBitString or t.len < 1: return
  if t.start < 0 or t.start >= d.len or d[t.start] != 0'u8: return
  res = bytesOf(d, t.start + 1, t.len - 1)
  result = true

func intBytes(d: openArray[byte]; t: Tlv): seq[byte] =
  ## A positive INTEGER's magnitude (its sign byte dropped).
  result = bytesOf(d, t.start, t.len)
  while result.len > 1 and result[0] == 0'u8:
    result.delete(0)

func parsePublicKey(d: openArray[byte]; spki: Tlv; pk: var PublicKey): bool =
  ## `SubjectPublicKeyInfo ::= SEQUENCE { AlgorithmIdentifier, BIT STRING }`.
  result = false
  pk = PublicKey(kind: kkNone, rsa: default(RsaPublicKey), ecPoint: @[])
  if spki.tag != TagSeq: return
  var alg = default(Tlv)
  var keyBits = default(Tlv)
  if not readTlv(d, spki.start, spki.next, alg) or alg.tag != TagSeq: return
  if not readTlv(d, alg.next, spki.next, keyBits): return
  var oid = default(Tlv)
  if not readTlv(d, alg.start, alg.next, oid) or oid.tag != TagOid: return
  var key: seq[byte] = @[]
  if not bitStringBytes(d, keyBits, key): return
  if sameBytes(d, oid.start, oid.len, OidRsaEncryption):
    # RSAPublicKey ::= SEQUENCE { modulus INTEGER, publicExponent INTEGER }
    var s = default(Tlv)
    var n = default(Tlv)
    var e = default(Tlv)
    if not readTlv(key, 0, key.len, s) or s.tag != TagSeq: return
    if not readTlv(key, s.start, s.next, n) or n.tag != TagInt: return
    if not readTlv(key, n.next, s.next, e) or e.tag != TagInt: return
    var rk = default(RsaPublicKey)
    if not initRsaPublicKey(intBytes(key, n), intBytes(key, e), rk): return
    pk = PublicKey(kind: kkRsa, rsa: rk, ecPoint: @[])
    result = true
  elif sameBytes(d, oid.start, oid.len, OidEcPublicKey):
    var curve = default(Tlv)
    if not readTlv(d, oid.next, alg.next, curve) or curve.tag != TagOid: return
    if sameBytes(d, curve.start, curve.len, OidP256):
      pk = PublicKey(kind: kkEcP256, rsa: default(RsaPublicKey), ecPoint: key)
    elif sameBytes(d, curve.start, curve.len, OidP384):
      pk = PublicKey(kind: kkEcP384, rsa: default(RsaPublicKey), ecPoint: key)
    else:
      return
    result = true

func digits(d: openArray[byte]; at, n: int; v: var int): bool =
  result = false
  v = 0
  for i in at ..< at + n:
    if i < 0 or i >= d.len: return
    let c = d[i]
    if c < byte('0') or c > byte('9'): return
    v = v * 10 + int(c - byte('0'))
  result = true

func daysFromCivil(y, m, d: int): int64 =
  ## Days since 1970-01-01 (Howard Hinnant's algorithm).
  let yy = if m <= 2: y - 1 else: y
  let era = (if yy >= 0: yy else: yy - 399) div 400
  let yoe = yy - era * 400
  let mp = (m + 9) mod 12
  let doy = (153 * mp + 2) div 5 + d - 1
  let doe = yoe * 365 + yoe div 4 - yoe div 100 + doy
  result = int64(era) * 146097 + int64(doe) - 719468

func parseTime(d: openArray[byte]; t: Tlv; res: var int64): bool =
  ## UTCTime `YYMMDDHHMMSSZ` or GeneralizedTime `YYYYMMDDHHMMSSZ`, which is
  ## all RFC 5280 lets a certificate use.
  result = false
  var y = 0
  var p = t.start
  if t.tag == TagUtcTime and t.len == 13:
    if not digits(d, p, 2, y): return
    y = if y >= 50: 1900 + y else: 2000 + y
    p = p + 2
  elif t.tag == TagGenTime and t.len == 15:
    if not digits(d, p, 4, y): return
    p = p + 4
  else:
    return
  var mo = 0
  var dd = 0
  var hh = 0
  var mi = 0
  var ss = 0
  if not digits(d, p, 2, mo) or not digits(d, p + 2, 2, dd) or
     not digits(d, p + 4, 2, hh) or not digits(d, p + 6, 2, mi) or
     not digits(d, p + 8, 2, ss): return
  let z = p + 10
  if z < 0 or z >= d.len or d[z] != byte('Z'): return
  if mo < 1 or mo > 12 or dd < 1 or dd > 31 or hh > 23 or mi > 59 or ss > 60: return
  res = daysFromCivil(y, mo, dd) * 86400 + int64(hh * 3600 + mi * 60 + ss)
  result = true

func findCommonName(d: openArray[byte]; name: Tlv): string =
  ## `Name ::= SEQUENCE OF SET OF SEQUENCE { OID, value }`; the first CN.
  result = ""
  var rdn = default(Tlv)
  var p = name.start
  while readTlv(d, p, name.next, rdn):
    p = rdn.next
    var atv = default(Tlv)
    if rdn.tag == TagSet and readTlv(d, rdn.start, rdn.next, atv) and atv.tag == TagSeq:
      var oid = default(Tlv)
      var val = default(Tlv)
      if readTlv(d, atv.start, atv.next, oid) and oid.tag == TagOid and
         sameBytes(d, oid.start, oid.len, OidCommonName) and
         readTlv(d, oid.next, atv.next, val):
        for b in bytesOf(d, val.start, val.len): result.add char(b)
        return

func boolValue(d: openArray[byte]; t: Tlv): bool =
  result = false
  if t.len == 1 and t.start >= 0 and t.start < d.len:
    result = d[t.start] != 0'u8

func parseExtension(d: openArray[byte]; ext: Tlv; c: var Certificate): bool =
  ## `Extension ::= SEQUENCE { extnID OID, critical BOOLEAN DEFAULT FALSE,
  ## extnValue OCTET STRING }`. `false` only for a malformed or an
  ## unsupported critical extension.
  result = false
  var oid = default(Tlv)
  if ext.tag != TagSeq or not readTlv(d, ext.start, ext.next, oid) or oid.tag != TagOid:
    return
  var t = default(Tlv)
  if not readTlv(d, oid.next, ext.next, t): return
  var critical = false
  if t.tag == TagBool:
    critical = boolValue(d, t)
    let after = t.next
    if not readTlv(d, after, ext.next, t): return
  if t.tag != TagOctetString: return
  let vStart = t.start
  let vEnd = t.next
  var v = default(Tlv)
  if sameBytes(d, oid.start, oid.len, OidBasicConstraints):
    if not readTlv(d, vStart, vEnd, v) or v.tag != TagSeq: return
    var f = default(Tlv)
    var p = v.start
    if readTlv(d, p, v.next, f) and f.tag == TagBool:
      c.isCa = boolValue(d, f)
      p = f.next
    if readTlv(d, p, v.next, f) and f.tag == TagInt:
      if f.len == 1 and f.start >= 0 and f.start < d.len: c.pathLen = int(d[f.start])
    result = true
  elif sameBytes(d, oid.start, oid.len, OidKeyUsage):
    if not readTlv(d, vStart, vEnd, v) or v.tag != TagBitString or v.len < 2: return
    c.hasKeyUsage = true
    # bit 5 (keyCertSign) of the first content byte after the unused-bits count
    let at = v.start + 1
    if at >= 0 and at < d.len:
      c.canSignCerts = (d[at] and 0x04'u8) != 0'u8
    result = true
  elif sameBytes(d, oid.start, oid.len, OidExtKeyUsage):
    if not readTlv(d, vStart, vEnd, v) or v.tag != TagSeq: return
    c.hasEku = true
    var p = v.start
    var e = default(Tlv)
    while readTlv(d, p, v.next, e):
      p = e.next
      if e.tag == TagOid and (sameBytes(d, e.start, e.len, OidServerAuth) or
                             sameBytes(d, e.start, e.len, OidAnyEku)):
        c.serverAuth = true
    result = true
  elif sameBytes(d, oid.start, oid.len, OidSubjectAltName):
    if not readTlv(d, vStart, vEnd, v) or v.tag != TagSeq: return
    var p = v.start
    var g = default(Tlv)
    while readTlv(d, p, v.next, g):
      p = g.next
      if g.tag == 0x82'u8:                 # [2] dNSName, IA5String
        var s = ""
        for b in bytesOf(d, g.start, g.len): s.add char(b)
        c.dnsNames.add s
      elif g.tag == 0x87'u8:               # [7] iPAddress
        c.ipAddresses.add bytesOf(d, g.start, g.len)
    result = true
  else:
    result = not critical

func parseCertificate*(der: openArray[byte]; c: var Certificate): bool =
  ## Parses one DER certificate. Checks structure only — nothing here says
  ## whether to TRUST it; that is `verifyChain`.
  result = false
  c = Certificate(der: @[], tbsStart: 0, tbsLen: 0, sigAlg: saUnknown,
                  signature: @[], issuer: @[], subject: @[], notBefore: 0,
                  notAfter: 0, publicKey: default(PublicKey), isCa: false,
                  pathLen: -1, hasKeyUsage: false, canSignCerts: false,
                  hasEku: false, serverAuth: false, dnsNames: @[],
                  ipAddresses: @[], commonName: "")
  for b in der: c.der.add b
  let d = c.der
  var cert, tbs, alg, sig = default(Tlv)
  if not readTlv(d, 0, d.len, cert) or cert.tag != TagSeq or cert.next != d.len: return
  if not readTlv(d, cert.start, cert.next, tbs) or tbs.tag != TagSeq: return
  if not readTlv(d, tbs.next, cert.next, alg): return
  if not readTlv(d, alg.next, cert.next, sig) or sig.next != cert.next: return
  c.tbsStart = cert.start
  c.tbsLen = tbs.next - cert.start
  c.sigAlg = sigAlgOf(d, alg)
  if not bitStringBytes(d, sig, c.signature): return
  # TBSCertificate
  var t = default(Tlv)
  var p = tbs.start
  if not readTlv(d, p, tbs.next, t): return
  if t.tag == 0xA0'u8:                     # [0] version
    p = t.next
    if not readTlv(d, p, tbs.next, t): return
  if t.tag != TagInt: return               # serialNumber
  p = t.next
  var innerAlg = default(Tlv)
  if not readTlv(d, p, tbs.next, innerAlg): return
  # The signature algorithm is stated twice; a mismatch is a forgery attempt.
  if innerAlg.len != alg.len or
     bytesOf(d, innerAlg.start, innerAlg.len) != bytesOf(d, alg.start, alg.len): return
  var issuer, validity, subject, spki = default(Tlv)
  if not readTlv(d, innerAlg.next, tbs.next, issuer) or issuer.tag != TagSeq: return
  if not readTlv(d, issuer.next, tbs.next, validity) or validity.tag != TagSeq: return
  if not readTlv(d, validity.next, tbs.next, subject) or subject.tag != TagSeq: return
  if not readTlv(d, subject.next, tbs.next, spki): return
  c.issuer = bytesOf(d, issuer.start, issuer.len)
  c.subject = bytesOf(d, subject.start, subject.len)
  c.commonName = findCommonName(d, subject)
  var nb, na = default(Tlv)
  if not readTlv(d, validity.start, validity.next, nb): return
  if not readTlv(d, nb.next, validity.next, na): return
  if not parseTime(d, nb, c.notBefore) or not parseTime(d, na, c.notAfter): return
  if not parsePublicKey(d, spki, c.publicKey): return
  p = spki.next
  while readTlv(d, p, tbs.next, t):
    p = t.next
    if t.tag == 0xA3'u8:                   # [3] extensions
      var exts = default(Tlv)
      if not readTlv(d, t.start, t.next, exts) or exts.tag != TagSeq: return
      var q = exts.start
      var e = default(Tlv)
      while readTlv(d, q, exts.next, e):
        q = e.next
        if not parseExtension(d, e, c): return
  result = true

# ------------------------------------------------------------- signatures ---

func tbs(c: Certificate): seq[byte] = bytesOf(c.der, c.tbsStart, c.tbsLen)

proc verifyWith*(key: PublicKey; alg: SigAlg; message, signature: openArray[byte]): bool =
  ## Checks an X.509 signature (PKCS #1 v1.5 for RSA).
  result = false
  case alg
  of saRsaSha256, saRsaSha384, saRsaSha512:
    if key.kind != kkRsa: return
    let h = case alg
            of saRsaSha256: rhSha256
            of saRsaSha384: rhSha384
            else: rhSha512
    result = rsaVerifyPkcs1v15(key.rsa, h, message, signature)
  of saEcdsaSha256, saEcdsaSha384, saEcdsaSha512:
    var digest: seq[byte] = @[]
    case alg
    of saEcdsaSha256:
      for b in sha256(message): digest.add b
    of saEcdsaSha384:
      for b in sha384(message): digest.add b
    else:
      for b in sha512(message): digest.add b
    case key.kind
    of kkEcP256: result = ecdsaVerifyDer(P256, key.ecPoint, digest, signature)
    of kkEcP384: result = ecdsaVerifyDer(P384, key.ecPoint, digest, signature)
    else: discard
  of saUnknown: discard

proc isSignedBy*(c, issuer: Certificate): bool =
  result = verifyWith(issuer.publicKey, c.sigAlg, tbs(c), c.signature)

# -------------------------------------------------------------- host names --

func lowerAscii(s: string): string =
  result = s
  for i in 0 ..< result.len:
    if result[i] in {'A'..'Z'}: result[i] = char(ord(result[i]) + 32)

func parseIp4(host: string; ip: var array[4, byte]): bool =
  var octet = 0
  var val = 0
  var digitsSeen = 0
  result = false
  for ch in host:
    if ch in {'0'..'9'}:
      val = val * 10 + (ord(ch) - ord('0'))
      inc digitsSeen
      if val > 255 or digitsSeen > 3: return
    elif ch == '.' and digitsSeen > 0 and octet < 3:
      ip[octet] = byte(val)
      inc octet
      val = 0
      digitsSeen = 0
    else:
      return
  if octet != 3 or digitsSeen == 0: return
  ip[3] = byte(val)
  result = true

func matchesName(pattern, host: string): bool =
  ## RFC 6125 §6.4: exact, case-insensitive; a wildcard only as the whole
  ## leftmost label, matching exactly one label, and never in a public suffix
  ## position like `*.com` (we require at least two labels after it).
  let p = lowerAscii(pattern)
  let h = lowerAscii(host)
  if p.len == 0 or h.len == 0: return false
  if not p.startsWith("*."):
    return p == h
  let suffix = p.substr(1)                  # ".example.com"
  var dots = 0
  for ch in suffix:
    if ch == '.': inc dots
  if dots < 2: return false
  let firstDot = h.find('.')
  if firstDot <= 0: return false
  result = h.substr(firstDot) == suffix

func matchesHost*(c: Certificate; host: string): bool =
  ## Whether the certificate names `host`. Only the subjectAltName counts:
  ## the subject's CN has not been a host name for clients since RFC 6125.
  var ip = default(array[4, byte])
  if parseIp4(host, ip):
    for a in c.ipAddresses:
      if a.len == 4 and a[0] == ip[0] and a[1] == ip[1] and a[2] == ip[2] and a[3] == ip[3]:
        return true
    return false
  for n in c.dnsNames:
    if matchesName(n, host): return true
  result = false

# ------------------------------------------------------------- trust store --

proc addPem*(store: var TrustStore; pem: string): int =
  ## Adds every certificate in a PEM bundle; returns how many parsed. Ones
  ## that do not parse are skipped: a system bundle may well carry a root
  ## with an algorithm this module does not speak.
  const
    Begin = "-----BEGIN CERTIFICATE-----"
    End = "-----END CERTIFICATE-----"
  result = 0
  var pos = 0
  while true:
    let a = pem.find(Begin, pos)
    if a < 0: break
    let b = pem.find(End, a + Begin.len)
    if b < 0: break
    let body = pem.substr(a + Begin.len, b - 1)
    pos = b + End.len
    let raw = decode(body)
    var bytes: seq[byte] = @[]
    for ch in raw: bytes.add byte(ch)
    var c = default(Certificate)
    if parseCertificate(bytes, c):
      store.roots.add c
      inc result

# The operating system's roots: `std/crypto/certstore`.

# ------------------------------------------------------------------- chain --

const MaxChainDepth = 8

proc validAt(c: Certificate; now: int64): bool {.inline.} =
  now >= c.notBefore and now <= c.notAfter

proc verifyChain*(store: TrustStore; chain: openArray[Certificate]; host: string;
                  now: int64): CertError =
  ## `chain[0]` is the server's certificate, the rest what it sent along to
  ## help (in any order, possibly with extras). Builds a path to a root of
  ## `store` and checks validity, CA flags, key usage and the host name.
  if chain.len == 0: return ceParse
  let leaf = chain[0]
  if not validAt(leaf, now): return ceExpired
  if not matchesHost(leaf, host): return ceHostMismatch
  if leaf.hasEku and not leaf.serverAuth: return ceUsage
  var current = leaf
  var depth = 0
  while depth < MaxChainDepth:
    # a trust anchor that issued `current`?
    for r in store.roots:
      if r.subject == current.issuer and isSignedBy(current, r):
        # anchors are trusted as configured; an expired one is still refused
        if not validAt(r, now): return ceExpired
        return ceNone
    # otherwise an intermediate the server sent
    var found = false
    var next = default(Certificate)
    for i in 1 ..< chain.len:
      let cand = chain[i]
      if cand.subject == current.issuer and isSignedBy(current, cand):
        next = cand
        found = true
        break
    if not found:
      return ceUntrusted
    if not next.isCa: return ceNotCa
    if next.hasKeyUsage and not next.canSignCerts: return ceNotCa
    if next.pathLen >= 0 and depth > next.pathLen: return ceNotCa
    if not validAt(next, now): return ceExpired
    current = next
    inc depth
  result = ceChainTooLong
