#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## A TLS 1.3 client (RFC 8446), written against `std/socket` and the
## primitives of `std/crypto`.
##
## What it speaks, deliberately few:
##
## * TLS 1.3 only. Every server worth trusting has spoken it for years, and
##   1.2's surface is most of what has gone wrong with TLS.
## * Key exchange X25519; cipher suite `TLS_CHACHA20_POLY1305_SHA256`.
## * Server authentication by `std/crypto/x509` against a `TrustStore`:
##   ECDSA P-256/P-384 and RSA-PSS `CertificateVerify`, a chain to a root, the
##   host name. There is no "insecure" switch; a caller who must talk to a
##   self-signed server puts that certificate into the store it passes.
##
## No session resumption, no 0-RTT, no client certificates (a request for one
## is answered with an empty certificate, which the server may refuse).
##
## .. code-block:: nim
##   var c = TlsConn()
##   tlsConnect(c, "example.com", 443, loadSystemTrustStore())
##   c.write("GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n")
##   var buf = newString(4096)
##   let n = c.read(buf)

{.feature: "staticContracts".}

import std/[socket, dns, times]
import std/crypto/[sha2, hmac, chacha20poly1305, x25519, x509, ecdsa, rsa, sysrand]

type
  TlsFailure* = enum
    tfNone = "no failure"
    tfRandom = "the system random number generator could not be read"
    tfConnect = "could not connect"
    tfProtocol = "the peer violated the TLS protocol"
    tfAlert = "the peer sent a fatal alert"
    tfUnsupported = "the server chose parameters this client does not speak"
    tfCertificate = "the server's certificate is not trusted"
    tfSignature = "the server's handshake signature does not verify"
    tfFinished = "the handshake transcript does not match"
    tfDecrypt = "a record failed authentication"
    tfClosed = "the connection is closed"

  Direction = object
    ## One direction of the record layer.
    secret: seq[byte]          ## the current traffic secret
    key: ChaChaKey
    iv: ChaChaNonce
    seqNo: uint64

  TlsConn* = object
    sock: Socket
    failure*: TlsFailure       ## what went wrong, once something did
    certError*: CertError      ## the detail when `failure == tfCertificate`
    alert*: int                ## the alert description, when `failure == tfAlert`
    rd, wr: Direction
    encrypted: bool            ## records are protected (after ServerHello)
    hs: seq[byte]              ## handshake bytes not yet parsed
    app: seq[byte]             ## decrypted application data not yet read
    appPos: int
    eof: bool                  ## close_notify received
    alpn*: string              ## the protocol the server picked, if any

const
  ContentChangeCipherSpec = 20'u8
  ContentAlert = 21'u8
  ContentHandshake = 22'u8
  ContentApplicationData = 23'u8

  HsClientHello = 1'u8
  HsServerHello = 2'u8
  HsNewSessionTicket = 4'u8
  HsEncryptedExtensions = 8'u8
  HsCertificate = 11'u8
  HsCertificateRequest = 13'u8
  HsCertificateVerify = 15'u8
  HsFinished = 20'u8
  HsKeyUpdate = 24'u8

  SuiteChaCha20Poly1305 = 0x1303
  GroupX25519 = 0x001d
  Tls13 = 0x0304

  ExtServerName = 0x0000
  ExtSupportedGroups = 0x000a
  ExtSignatureAlgorithms = 0x000d
  ExtAlpn = 0x0010
  ExtSupportedVersions = 0x002b
  ExtKeyShare = 0x0033

  SchemeEcdsaP256Sha256 = 0x0403
  SchemeEcdsaP384Sha384 = 0x0503
  SchemeRsaPssSha256 = 0x0804
  SchemeRsaPssSha384 = 0x0805
  SchemeRsaPssSha512 = 0x0806
  # offered only so a server knows which CERTIFICATE signatures we can check;
  # TLS 1.3 forbids them for CertificateVerify itself
  SchemeRsaPkcs1Sha256 = 0x0401
  SchemeRsaPkcs1Sha384 = 0x0501
  SchemeRsaPkcs1Sha512 = 0x0601

  MaxPlaintext = 16384
  MaxCiphertext = MaxPlaintext + 256
  MaxHandshake = 1 shl 18     ## a certificate chain bigger than this is abuse

  HelloRetryRandom = [0xCF'u8, 0x21, 0xAD, 0x74, 0xE5, 0x9A, 0x61, 0x11, 0xBE, 0x1D,
                      0x8C, 0x02, 0x1E, 0x65, 0xB8, 0x91, 0xC2, 0xA2, 0x11, 0x16,
                      0x7A, 0xBB, 0x8C, 0x5E, 0x07, 0x9E, 0x09, 0xE2, 0xC8, 0xA8,
                      0x33, 0x9C]

# ------------------------------------------------------------- byte building

proc put8(s: var seq[byte]; v: int) {.inline.} = s.add byte(v and 0xFF)
proc put16(s: var seq[byte]; v: int) =
  s.add byte((v shr 8) and 0xFF)
  s.add byte(v and 0xFF)
proc put24(s: var seq[byte]; v: int) =
  s.add byte((v shr 16) and 0xFF)
  s.add byte((v shr 8) and 0xFF)
  s.add byte(v and 0xFF)
proc putBytes(s: var seq[byte]; b: openArray[byte]) =
  for x in b: s.add x
proc putStr(s: var seq[byte]; b: string) =
  for x in b: s.add byte(x)

proc patch16(s: var seq[byte]; at, v: int) =
  ## Back-fills a length that was reserved with `put16(0)`.
  if at >= 0 and at + 1 < s.len:
    s[at] = byte((v shr 8) and 0xFF)
    s[at + 1] = byte(v and 0xFF)

# ------------------------------------------------------------- byte reading

proc get8(d: openArray[byte]; pos: var int; limit: int; v: var int): bool =
  result = false
  if pos < 0 or pos >= limit or limit > d.len: return
  v = int(d[pos])
  pos = pos + 1
  result = true

proc get16(d: openArray[byte]; pos: var int; limit: int; v: var int): bool =
  result = false
  if pos < 0 or pos + 2 > limit or limit > d.len: return
  v = int(d[pos]) shl 8 or int(d[pos + 1])
  pos = pos + 2
  result = true

proc get24(d: openArray[byte]; pos: var int; limit: int; v: var int): bool =
  result = false
  if pos < 0 or pos + 3 > limit or limit > d.len: return
  v = int(d[pos]) shl 16 or int(d[pos + 1]) shl 8 or int(d[pos + 2])
  pos = pos + 3
  result = true

proc getBytes(d: openArray[byte]; pos: var int; limit, n: int; res: var seq[byte]): bool =
  result = false
  if n < 0 or pos < 0 or pos + n > limit or limit > d.len: return
  res = @[]
  for i in pos ..< pos + n:
    if i < d.len: res.add d[i]
  pos = pos + n
  result = true

# ------------------------------------------------------------ key schedule --

proc hkdfExpandLabel(secret: openArray[byte]; label: string; context: openArray[byte];
                     length: int): seq[byte] {.requires: length >= 0 and length <= 255.} =
  var info: seq[byte] = @[]
  info.put16 length
  info.put8 6 + label.len
  info.putStr "tls13 "
  info.putStr label
  info.put8 context.len
  info.putBytes context
  result = hkdfExpandSha256(secret, info, length)

proc deriveSecret(secret: openArray[byte]; label: string; transcript: Sha256): seq[byte] =
  let h = transcript.finish()
  result = hkdfExpandLabel(secret, label, h, 32)

proc setKeys(d: var Direction; secret: seq[byte]) =
  d.secret = secret
  let k = hkdfExpandLabel(secret, "key", [], 32)
  let iv = hkdfExpandLabel(secret, "iv", [], 12)
  for i in 0 ..< 32:
    if i < k.len: d.key[i] = k[i]
  for i in 0 ..< 12:
    if i < iv.len: d.iv[i] = iv[i]
  d.seqNo = 0'u64

proc nonceFor(d: Direction): ChaChaNonce =
  ## The per-record nonce: the IV xored with the record number, left-padded.
  result = d.iv
  for i in 0 ..< 8:
    result[4 + i] = result[4 + i] xor byte((d.seqNo shr (8 * (7 - i))) and 0xFF'u64)

proc finishedMac(baseSecret: openArray[byte]; transcript: Sha256): Sha256Digest =
  let fk = hkdfExpandLabel(baseSecret, "finished", [], 32)
  result = hmacSha256(fk, transcript.finish())

# ------------------------------------------------------------ record layer --

template fail(c: var TlsConn; f: TlsFailure; e: ErrorCode) =
  ## Records why and raises. A template, so that both the prover and the
  ## reader see the `raise` at the call site: nothing after it runs.
  if c.failure == tfNone: c.failure = f
  raise e

template fail(c: var TlsConn; f: TlsFailure) = fail(c, f, ProtocolError)

proc sendRecord(c: var TlsConn; contentType: byte; payload: openArray[byte];
                dl: Deadline) {.passive, raises.} =
  ## One record. Once keys are in place it is protected: the real type moves
  ## inside and the outer header always says application data.
  var rec: seq[byte] = @[]
  if not c.encrypted:
    rec.put8 int(contentType)
    rec.put16 0x0301                 # legacy_record_version, as RFC 8446 asks
    rec.put16 payload.len
    rec.putBytes payload
  else:
    var inner: seq[byte] = @[]
    inner.putBytes payload
    inner.add contentType
    let ctLen = inner.len + 16
    rec.put8 int(ContentApplicationData)
    rec.put16 0x0303
    rec.put16 ctLen
    var aad: seq[byte] = @[]
    for b in rec: aad.add b
    let tag = chacha20Poly1305Seal(c.wr.key, nonceFor(c.wr), aad, inner)
    c.wr.seqNo = c.wr.seqNo + 1'u64
    rec.putBytes inner
    rec.putBytes tag
  var chars = newString(rec.len)
  for i in 0 ..< rec.len:
    if i < chars.len: chars[i] = char(rec[i])
  write(c.sock, chars, dl)

proc readRecord(c: var TlsConn; contentType: var byte; payload: var seq[byte];
                dl: Deadline) {.passive, raises.} =
  ## The next record, decrypted when keys are in place. ChangeCipherSpec
  ## records — middlebox-compatibility noise that TLS 1.3 says to ignore — are
  ## skipped here.
  while true:
    while c.sock.buffered < 5:
      if fill(c.sock, dl) == 0: c.fail(tfClosed, EndOfStreamError)
    var hdr = default(array[5, byte])
    block:
      let view = c.sock.peek
      if view.len < 5: c.fail(tfProtocol)
      for i in 0 ..< 5:
        if i < view.len: hdr[i] = byte(view[i])
    let typ = hdr[0]
    let len = int(hdr[3]) shl 8 or int(hdr[4])
    if len > MaxCiphertext: c.fail(tfProtocol)
    while c.sock.buffered < 5 + len:
      if fill(c.sock, dl) == 0: c.fail(tfClosed, EndOfStreamError)
    var body: seq[byte] = @[]
    block:
      let view = c.sock.peek
      for i in 5 ..< 5 + len:
        if i < view.len: body.add byte(view[i])
    c.sock.consume(5 + len)
    if body.len != len: c.fail(tfProtocol)
    if typ == ContentChangeCipherSpec:
      continue
    if not c.encrypted or typ != ContentApplicationData:
      if c.encrypted and typ != ContentAlert: c.fail(tfProtocol)
      contentType = typ
      payload = body
      return
    if body.len < 17: c.fail(tfDecrypt)
    let ctLen = body.len - 16
    var ct: seq[byte] = @[]
    var tag: seq[byte] = @[]
    for i in 0 ..< body.len:
      if i < ctLen: ct.add body[i]
      else: tag.add body[i]
    if not chacha20Poly1305Open(c.rd.key, nonceFor(c.rd), hdr, ct, tag):
      c.fail(tfDecrypt)
    c.rd.seqNo = c.rd.seqNo + 1'u64
    # strip the zero padding; the last non-zero byte is the real type
    var i = ct.len - 1
    while i >= 0 and ct[i] == 0'u8: dec i
    if i < 0: c.fail(tfProtocol)
    contentType = ct[i]
    payload = @[]
    for j in 0 ..< i:
      if j < ct.len: payload.add ct[j]
    return

proc onAlert(c: var TlsConn; payload: seq[byte]) {.raises.} =
  ## `close_notify` ends the stream; every other alert in TLS 1.3 is fatal.
  if payload.len >= 2 and payload[1] == 0'u8:
    c.eof = true
    return
  if payload.len >= 2: c.alert = int(payload[1])
  c.fail(tfAlert)

proc nextHandshake(c: var TlsConn; msgType: var byte; msg: var seq[byte];
                   dl: Deadline) {.passive, raises.} =
  ## One whole handshake message (header included, which is what the
  ## transcript hashes), reassembled across records as needed.
  while true:
    if c.hs.len >= 4:
      let bodyLen = int(c.hs[1]) shl 16 or int(c.hs[2]) shl 8 or int(c.hs[3])
      if bodyLen > MaxHandshake: c.fail(tfProtocol)
      if c.hs.len >= 4 + bodyLen:
        msgType = c.hs[0]
        msg = @[]
        var rest: seq[byte] = @[]
        for i in 0 ..< c.hs.len:
          if i < 4 + bodyLen: msg.add c.hs[i]
          else: rest.add c.hs[i]
        c.hs = rest
        return
    var typ = 0'u8
    var payload: seq[byte] = @[]
    readRecord(c, typ, payload, dl)
    if typ == ContentAlert:
      onAlert(c, payload)
      c.fail(tfClosed, EndOfStreamError)
    if typ != ContentHandshake: c.fail(tfProtocol)
    if c.hs.len + payload.len > MaxHandshake: c.fail(tfProtocol)
    c.hs.putBytes payload

# --------------------------------------------------------------- handshake --

proc isIpLiteral(host: string): bool =
  result = host.len > 0
  for ch in host:
    if ch notin {'0'..'9', '.', ':'}: return false

proc clientHello(host: string; random: array[32, byte]; keyShare: X25519Key): seq[byte] =
  var body: seq[byte] = @[]
  body.put16 0x0303                          # legacy_version
  body.putBytes random
  body.put8 0                                # legacy_session_id: empty
  body.put16 2
  body.put16 SuiteChaCha20Poly1305
  body.put8 1                                # legacy_compression_methods
  body.put8 0
  var ext: seq[byte] = @[]
  if not isIpLiteral(host):
    ext.put16 ExtServerName
    ext.put16 host.len + 5
    ext.put16 host.len + 3
    ext.put8 0                               # host_name
    ext.put16 host.len
    ext.putStr host
  ext.put16 ExtSupportedGroups
  ext.put16 4
  ext.put16 2
  ext.put16 GroupX25519
  ext.put16 ExtSignatureAlgorithms
  const schemes = [SchemeEcdsaP256Sha256, SchemeEcdsaP384Sha384, SchemeRsaPssSha256,
                   SchemeRsaPssSha384, SchemeRsaPssSha512, SchemeRsaPkcs1Sha256,
                   SchemeRsaPkcs1Sha384, SchemeRsaPkcs1Sha512]
  ext.put16 2 + 2 * schemes.len
  ext.put16 2 * schemes.len
  for s in schemes: ext.put16 s
  ext.put16 ExtAlpn
  ext.put16 11
  ext.put16 9
  ext.put8 8
  ext.putStr "http/1.1"
  ext.put16 ExtSupportedVersions
  ext.put16 3
  ext.put8 2
  ext.put16 Tls13
  ext.put16 ExtKeyShare
  ext.put16 2 + 4 + 32
  ext.put16 4 + 32
  ext.put16 GroupX25519
  ext.put16 32
  ext.putBytes keyShare
  body.put16 ext.len
  body.putBytes ext
  result = @[]
  result.put8 int(HsClientHello)
  result.put24 body.len
  result.putBytes body

proc parseServerHello(c: var TlsConn; msg: seq[byte]; serverShare: var X25519Key) {.raises.} =
  var pos = 4
  let limit = msg.len
  var v = 0
  var random: seq[byte] = @[]
  if not get16(msg, pos, limit, v): c.fail(tfProtocol)
  if not getBytes(msg, pos, limit, 32, random): c.fail(tfProtocol)
  if random == @HelloRetryRandom:
    # We offered our one group; a server asking for another wants something
    # this client does not have.
    c.fail(tfUnsupported)
  var sidLen = 0
  var sid: seq[byte] = @[]
  if not get8(msg, pos, limit, sidLen) or not getBytes(msg, pos, limit, sidLen, sid):
    c.fail(tfProtocol)
  if sid.len != 0: c.fail(tfProtocol)       # must echo our empty one
  var suite = 0
  if not get16(msg, pos, limit, suite): c.fail(tfProtocol)
  if suite != SuiteChaCha20Poly1305: c.fail(tfUnsupported)
  if not get8(msg, pos, limit, v) or v != 0: c.fail(tfProtocol)
  var extLen = 0
  if not get16(msg, pos, limit, extLen): c.fail(tfProtocol)
  let extEnd = pos + extLen
  if extEnd != limit: c.fail(tfProtocol)
  var version = 0
  var haveShare = false
  while pos < extEnd:
    var typ = 0
    var len = 0
    if not get16(msg, pos, extEnd, typ) or not get16(msg, pos, extEnd, len): c.fail(tfProtocol)
    let e = pos + len
    if e > extEnd: c.fail(tfProtocol)
    case typ
    of ExtSupportedVersions:
      if not get16(msg, pos, e, version): c.fail(tfProtocol)
    of ExtKeyShare:
      var group = 0
      var klen = 0
      var key: seq[byte] = @[]
      if not get16(msg, pos, e, group) or not get16(msg, pos, e, klen) or
         not getBytes(msg, pos, e, klen, key): c.fail(tfProtocol)
      if group != GroupX25519 or key.len != 32: c.fail(tfUnsupported)
      for i in 0 ..< 32:
        if i < key.len: serverShare[i] = key[i]
      haveShare = true
    else:
      discard
    pos = e
  if version != Tls13: c.fail(tfUnsupported)
  if not haveShare: c.fail(tfProtocol)

proc parseCertificates(c: var TlsConn; msg: seq[byte]; chain: var seq[Certificate]) {.raises.} =
  var pos = 4
  let limit = msg.len
  var ctxLen = 0
  var ctx: seq[byte] = @[]
  if not get8(msg, pos, limit, ctxLen) or not getBytes(msg, pos, limit, ctxLen, ctx):
    c.fail(tfProtocol)
  var listLen = 0
  if not get24(msg, pos, limit, listLen): c.fail(tfProtocol)
  if pos + listLen != limit: c.fail(tfProtocol)
  chain = @[]
  while pos < limit:
    var certLen = 0
    var der: seq[byte] = @[]
    if not get24(msg, pos, limit, certLen) or not getBytes(msg, pos, limit, certLen, der):
      c.fail(tfProtocol)
    var extLen = 0
    var exts: seq[byte] = @[]
    if not get16(msg, pos, limit, extLen) or not getBytes(msg, pos, limit, extLen, exts):
      c.fail(tfProtocol)
    var cert = default(Certificate)
    if not parseCertificate(der, cert):
      if chain.len == 0:
        c.certError = ceParse
        c.fail(tfCertificate)
      # an extra certificate we cannot read is harmless; the leaf is not
      continue
    chain.add cert
  if chain.len == 0:
    c.certError = ceParse
    c.fail(tfCertificate)

proc verifyCertificateVerify(c: var TlsConn; msg: seq[byte]; leaf: Certificate;
                             transcript: Sha256) {.raises.} =
  var pos = 4
  let limit = msg.len
  var scheme = 0
  var sigLen = 0
  var sig: seq[byte] = @[]
  if not get16(msg, pos, limit, scheme) or not get16(msg, pos, limit, sigLen) or
     not getBytes(msg, pos, limit, sigLen, sig): c.fail(tfProtocol)
  var content: seq[byte] = @[]
  for i in 0 ..< 64: content.add 0x20'u8
  content.putStr "TLS 1.3, server CertificateVerify"
  content.add 0'u8
  content.putBytes transcript.finish()
  var ok = false
  let key = leaf.publicKey
  case scheme
  of SchemeEcdsaP256Sha256:
    if key.kind == kkEcP256:
      ok = ecdsaVerifyDer(P256, key.ecPoint, sha256(content), sig)
  of SchemeEcdsaP384Sha384:
    if key.kind == kkEcP384:
      ok = ecdsaVerifyDer(P384, key.ecPoint, sha384(content), sig)
  of SchemeRsaPssSha256:
    if key.kind == kkRsa: ok = rsaVerifyPss(key.rsa, rhSha256, content, sig, 32)
  of SchemeRsaPssSha384:
    if key.kind == kkRsa: ok = rsaVerifyPss(key.rsa, rhSha384, content, sig, 48)
  of SchemeRsaPssSha512:
    if key.kind == kkRsa: ok = rsaVerifyPss(key.rsa, rhSha512, content, sig, 64)
  else:
    discard
  if not ok: c.fail(tfSignature)

proc parseEncryptedExtensions(c: var TlsConn; msg: seq[byte]) {.raises.} =
  var pos = 4
  let limit = msg.len
  var extLen = 0
  if not get16(msg, pos, limit, extLen) or pos + extLen != limit: c.fail(tfProtocol)
  while pos < limit:
    var typ = 0
    var len = 0
    if not get16(msg, pos, limit, typ) or not get16(msg, pos, limit, len): c.fail(tfProtocol)
    let e = pos + len
    if e > limit: c.fail(tfProtocol)
    if typ == ExtAlpn:
      var listLen = 0
      var nameLen = 0
      var name: seq[byte] = @[]
      if get16(msg, pos, e, listLen) and get8(msg, pos, e, nameLen) and
         getBytes(msg, pos, e, nameLen, name):
        c.alpn = ""
        for b in name: c.alpn.add char(b)
    pos = e

proc tlsConnect*(c: var TlsConn; host: string; port: uint16; store: TrustStore;
                 dl = never) {.passive, raises.} =
  ## Connects to `host:port` and runs the handshake. Raises on any failure,
  ## with `c.failure` (and `c.certError`/`c.alert`) saying which; `c` is then
  ## unusable. `host` is what the certificate must name.
  c = TlsConn(sock: initSocket(-1, never), failure: tfNone, certError: ceNone, alert: 0,
              rd: default(Direction), wr: default(Direction), encrypted: false,
              hs: @[], app: @[], appPos: 0, eof: false, alpn: "")
  var ip = host
  if not isIpLiteral(host):
    try:
      ip = resolve(host, dl)
    except ErrorCode as e:
      c.fail(tfConnect, e)
  try:
    c.sock = connect(ip, port, dl)
  except ErrorCode as e:
    c.fail(tfConnect, e)

  var secretKey = default(X25519Key)
  var random = default(array[32, byte])
  if not urandom(secretKey) or not urandom(random): c.fail(tfRandom, IOError)
  var transcript = initSha256()
  let hello = clientHello(host, random, x25519Public(secretKey))
  transcript.update(hello)
  sendRecord(c, ContentHandshake, hello, dl)

  # ServerHello, in the clear
  var typ = 0'u8
  var msg: seq[byte] = @[]
  nextHandshake(c, typ, msg, dl)
  if typ != HsServerHello: c.fail(tfProtocol)
  var serverShare = default(X25519Key)
  parseServerHello(c, msg, serverShare)
  transcript.update(msg)
  let shared = x25519(secretKey, serverShare)
  if isAllZero(shared): c.fail(tfProtocol)
  # the rest of the handshake is protected; a fragment of it sharing the
  # ServerHello's record would have been sent in the clear, which is illegal
  if c.hs.len != 0: c.fail(tfProtocol)

  # key schedule up to the handshake secrets
  let zeros = default(array[32, byte])
  let early = hkdfExtractSha256([], zeros)
  let derived = deriveSecret(early, "derived", initSha256())
  let hsSecret = hkdfExtractSha256(derived, shared)
  let cHs = deriveSecret(hsSecret, "c hs traffic", transcript)
  let sHs = deriveSecret(hsSecret, "s hs traffic", transcript)
  c.rd.setKeys(sHs)
  c.wr.setKeys(cHs)
  c.encrypted = true

  nextHandshake(c, typ, msg, dl)
  if typ != HsEncryptedExtensions: c.fail(tfProtocol)
  parseEncryptedExtensions(c, msg)
  transcript.update(msg)

  nextHandshake(c, typ, msg, dl)
  var certRequested = false
  if typ == HsCertificateRequest:
    certRequested = true
    transcript.update(msg)
    nextHandshake(c, typ, msg, dl)
  if typ != HsCertificate: c.fail(tfProtocol)
  var chain: seq[Certificate] = @[]
  parseCertificates(c, msg, chain)
  transcript.update(msg)
  let verdict = verifyChain(store, chain, host, getTime().toUnix)
  if verdict != ceNone:
    c.certError = verdict
    c.fail(tfCertificate)

  nextHandshake(c, typ, msg, dl)
  if typ != HsCertificateVerify: c.fail(tfProtocol)
  if chain.len > 0:
    verifyCertificateVerify(c, msg, chain[0], transcript)
  transcript.update(msg)

  nextHandshake(c, typ, msg, dl)
  if typ != HsFinished: c.fail(tfProtocol)
  let expected = finishedMac(sHs, transcript)
  var got: seq[byte] = @[]
  var p = 4
  if not getBytes(msg, p, msg.len, 32, got) or p != msg.len: c.fail(tfProtocol)
  if not constantTimeEquals(expected, got): c.fail(tfFinished)
  transcript.update(msg)

  # application secrets are derived over the transcript up to server Finished
  let derived2 = deriveSecret(hsSecret, "derived", initSha256())
  let master = hkdfExtractSha256(derived2, zeros)
  let cAp = deriveSecret(master, "c ap traffic", transcript)
  let sAp = deriveSecret(master, "s ap traffic", transcript)

  if certRequested:
    # an empty Certificate: we have none to offer
    var empty: seq[byte] = @[]
    empty.put8 int(HsCertificate)
    empty.put24 4
    empty.put8 0
    empty.put24 0
    transcript.update(empty)
    sendRecord(c, ContentHandshake, empty, dl)
  var fin: seq[byte] = @[]
  fin.put8 int(HsFinished)
  fin.put24 32
  fin.putBytes finishedMac(cHs, transcript)
  sendRecord(c, ContentHandshake, fin, dl)

  c.rd.setKeys(sAp)
  c.wr.setKeys(cAp)

# ------------------------------------------------------- application data --

proc onPostHandshake(c: var TlsConn; dl: Deadline) {.passive, raises.} =
  ## Session tickets are dropped (no resumption); a KeyUpdate rolls the read
  ## key and, if asked, ours too.
  var typ = 0'u8
  var msg: seq[byte] = @[]
  while c.hs.len >= 4:
    let bodyLen = int(c.hs[1]) shl 16 or int(c.hs[2]) shl 8 or int(c.hs[3])
    if c.hs.len < 4 + bodyLen: return
    nextHandshake(c, typ, msg, dl)
    case typ
    of HsNewSessionTicket:
      discard
    of HsKeyUpdate:
      if msg.len != 5: c.fail(tfProtocol)
      let requested = msg[4] == 1'u8
      c.rd.setKeys(hkdfExpandLabel(c.rd.secret, "traffic upd", [], 32))
      if requested:
        var ku: seq[byte] = @[]
        ku.put8 int(HsKeyUpdate)
        ku.put24 1
        ku.put8 0
        sendRecord(c, ContentHandshake, ku, dl)
        c.wr.setKeys(hkdfExpandLabel(c.wr.secret, "traffic upd", [], 32))
    else:
      c.fail(tfProtocol)

proc read*(c: var TlsConn; dest: var openArray[char]; dl = never): int {.passive, raises.} =
  ## Up to `dest.len` bytes of application data; `0` once the server has
  ## closed the connection with `close_notify`. A connection that ends
  ## WITHOUT one raises `EndOfStreamError`: that is a truncation an attacker
  ## can cause, and the bytes before it may not be the whole message.
  result = 0
  if c.failure != tfNone: raise ProtocolError
  while c.appPos >= c.app.len and not c.eof:
    var typ = 0'u8
    var payload: seq[byte] = @[]
    readRecord(c, typ, payload, dl)
    case typ
    of ContentApplicationData:
      c.app = payload
      c.appPos = 0
    of ContentHandshake:
      c.hs.putBytes payload
      onPostHandshake(c, dl)
    of ContentAlert:
      onAlert(c, payload)
    else:
      c.fail(tfProtocol)
  while result < dest.len and c.appPos < c.app.len:
    if c.appPos >= 0:
      dest[result] = char(c.app[c.appPos])
    inc result
    inc c.appPos

proc write*(c: var TlsConn; data: openArray[char]; dl = never) {.passive, raises.} =
  ## Sends `data` as application records of at most 16 KiB each.
  if c.failure != tfNone: raise ProtocolError
  var pos = 0
  while pos < data.len:
    var chunk: seq[byte] = @[]
    while pos < data.len and chunk.len < MaxPlaintext:
      chunk.add byte(data[pos])
      inc pos
    sendRecord(c, ContentApplicationData, chunk, dl)

proc write*(c: var TlsConn; data: string; dl = never) {.passive, raises.} =
  write(c, toOpenArray(data, 0, data.len - 1), dl)

proc close*(c: var TlsConn; dl = never) {.passive.} =
  ## Sends `close_notify` (best effort) and closes the socket.
  if c.failure == tfNone and c.encrypted:
    try:
      sendRecord(c, ContentAlert, [1'u8, 0'u8], dl)
    except ErrorCode:
      discard
  close(c.sock)
  c.failure = tfClosed
