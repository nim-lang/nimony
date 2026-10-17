#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## A TLS 1.3 client (RFC 8446) as a `streamio.TlsEngine`: a state machine that
## takes records in and gives records out and does no I/O of its own. The
## stream core moves the bytes (`connect(address, tls = newTlsEngine())`).
##
## What it speaks, deliberately few:
##
## * TLS 1.3 only. Every server worth trusting has spoken it for years, and
##   1.2's surface is most of what has gone wrong with TLS.
## * Key exchange X25519; cipher suite `TLS_CHACHA20_POLY1305_SHA256`.
## * Server authentication by `std/crypto/x509` against a `TrustStore`:
##   ECDSA P-256/P-384 and RSA-PSS `CertificateVerify`, a chain to a root, the
##   host name. There is no "insecure" switch; a caller who must talk to a
##   self-signed server puts that certificate into the store.
##
## No session resumption, no 0-RTT, no client certificates (a request for one
## is answered with an empty certificate, which the server may refuse).
##
## .. code-block:: nim
##   let e = newTlsEngine()
##   let (r, w) = connect("example.com:443", "GET / HTTP/1.1\r\n...", tls = e)
##   # on a failure, e.failure / e.certError / e.alert say what went wrong

{.feature: "staticContracts".}

import std/[times, streamio]
import std/crypto/[sha2, hmac, chacha20poly1305, x25519, x509, ecdsa, rsa, sysrand,
                  certstore]

export streamio.TlsEngine

type
  TlsFailure* = enum
    tfNone = "no failure"
    tfRandom = "the system random number generator could not be read"
    tfProtocol = "the peer violated the TLS protocol"
    tfAlert = "the peer sent a fatal alert"
    tfUnsupported = "the server chose parameters this client does not speak"
    tfCertificate = "the server's certificate is not trusted"
    tfSignature = "the server's handshake signature does not verify"
    tfFinished = "the handshake transcript does not match"
    tfDecrypt = "a record failed authentication"
    tfState = "the engine was used out of order"

  Direction = object
    ## One direction of the record layer.
    secret: seq[byte]          ## the current traffic secret
    key: ChaChaKey
    iv: ChaChaNonce
    seqNo: uint64

  HsState = enum
    hsIdle, hsWaitServerHello, hsWaitEncExt, hsWaitCert, hsWaitCertVerify,
    hsWaitFinished, hsConnected, hsFailed

  NimonyTlsEngine* = ref object of TlsEngine
    ## The default engine. After a failure, `failure` (and `certError`,
    ## `alert`) say what went wrong.
    failure*: TlsFailure
    certError*: CertError      ## the detail when `failure == tfCertificate`
    alert*: int                ## the alert description, when `failure == tfAlert`
    alpn*: string              ## the protocol the server picked, if any
    store: TrustStore
    host: string
    state: HsState
    secretKey: X25519Key
    transcript: Sha256
    hsSecret, cHs, sHs: seq[byte]
    certRequested: bool
    leaf: Certificate
    rd, wr: Direction
    encrypted: bool            ## records are protected (after ServerHello)
    inBuf: seq[byte]           ## network bytes not yet a whole record
    hs: seq[byte]              ## handshake bytes not yet a whole message
    outBuf: string             ## records waiting for `recordsOut`
    app: string                ## plaintext waiting for `appOut`
    peerClosed: bool
    closeSent: bool

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

template fail(e: NimonyTlsEngine; f: TlsFailure; err: ErrorCode) =
  ## Records why and raises. A template, so that both the prover and the
  ## reader see the `raise` at the call site: nothing after it runs.
  if e.failure == tfNone: e.failure = f
  e.state = hsFailed
  raise err

template fail(e: NimonyTlsEngine; f: TlsFailure) = fail(e, f, ProtocolError)

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

proc parseServerHello(e: NimonyTlsEngine; msg: seq[byte]; serverShare: var X25519Key) {.raises.} =
  var pos = 4
  let limit = msg.len
  var v = 0
  var random: seq[byte] = @[]
  if not get16(msg, pos, limit, v): e.fail(tfProtocol)
  if not getBytes(msg, pos, limit, 32, random): e.fail(tfProtocol)
  if random == @HelloRetryRandom:
    # We offered our one group; a server asking for another wants something
    # this client does not have.
    e.fail(tfUnsupported)
  var sidLen = 0
  var sid: seq[byte] = @[]
  if not get8(msg, pos, limit, sidLen) or not getBytes(msg, pos, limit, sidLen, sid):
    e.fail(tfProtocol)
  if sid.len != 0: e.fail(tfProtocol)       # must echo our empty one
  var suite = 0
  if not get16(msg, pos, limit, suite): e.fail(tfProtocol)
  if suite != SuiteChaCha20Poly1305: e.fail(tfUnsupported)
  if not get8(msg, pos, limit, v) or v != 0: e.fail(tfProtocol)
  var extLen = 0
  if not get16(msg, pos, limit, extLen): e.fail(tfProtocol)
  let extEnd = pos + extLen
  if extEnd != limit: e.fail(tfProtocol)
  var version = 0
  var haveShare = false
  while pos < extEnd:
    var typ = 0
    var len = 0
    if not get16(msg, pos, extEnd, typ) or not get16(msg, pos, extEnd, len): e.fail(tfProtocol)
    let stop = pos + len
    if stop > extEnd: e.fail(tfProtocol)
    case typ
    of ExtSupportedVersions:
      if not get16(msg, pos, stop, version): e.fail(tfProtocol)
    of ExtKeyShare:
      var group = 0
      var klen = 0
      var key: seq[byte] = @[]
      if not get16(msg, pos, stop, group) or not get16(msg, pos, stop, klen) or
         not getBytes(msg, pos, stop, klen, key): e.fail(tfProtocol)
      if group != GroupX25519 or key.len != 32: e.fail(tfUnsupported)
      for i in 0 ..< 32:
        if i < key.len: serverShare[i] = key[i]
      haveShare = true
    else:
      discard
    pos = stop
  if version != Tls13: e.fail(tfUnsupported)
  if not haveShare: e.fail(tfProtocol)

proc parseCertificates(e: NimonyTlsEngine; msg: seq[byte]; chain: var seq[Certificate]) {.raises.} =
  var pos = 4
  let limit = msg.len
  var ctxLen = 0
  var ctx: seq[byte] = @[]
  if not get8(msg, pos, limit, ctxLen) or not getBytes(msg, pos, limit, ctxLen, ctx):
    e.fail(tfProtocol)
  var listLen = 0
  if not get24(msg, pos, limit, listLen): e.fail(tfProtocol)
  if pos + listLen != limit: e.fail(tfProtocol)
  chain = @[]
  while pos < limit:
    var certLen = 0
    var der: seq[byte] = @[]
    if not get24(msg, pos, limit, certLen) or not getBytes(msg, pos, limit, certLen, der):
      e.fail(tfProtocol)
    var extLen = 0
    var exts: seq[byte] = @[]
    if not get16(msg, pos, limit, extLen) or not getBytes(msg, pos, limit, extLen, exts):
      e.fail(tfProtocol)
    var cert = default(Certificate)
    if not parseCertificate(der, cert):
      if chain.len == 0:
        e.certError = ceParse
        e.fail(tfCertificate)
      # an extra certificate we cannot read is harmless; the leaf is not
      continue
    chain.add cert
  if chain.len == 0:
    e.certError = ceParse
    e.fail(tfCertificate)

proc verifyCertificateVerify(e: NimonyTlsEngine; msg: seq[byte]; leaf: Certificate;
                             transcript: Sha256) {.raises.} =
  var pos = 4
  let limit = msg.len
  var scheme = 0
  var sigLen = 0
  var sig: seq[byte] = @[]
  if not get16(msg, pos, limit, scheme) or not get16(msg, pos, limit, sigLen) or
     not getBytes(msg, pos, limit, sigLen, sig): e.fail(tfProtocol)
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
  if not ok: e.fail(tfSignature)

proc parseEncryptedExtensions(e: NimonyTlsEngine; msg: seq[byte]) {.raises.} =
  var pos = 4
  let limit = msg.len
  var extLen = 0
  if not get16(msg, pos, limit, extLen) or pos + extLen != limit: e.fail(tfProtocol)
  while pos < limit:
    var typ = 0
    var len = 0
    if not get16(msg, pos, limit, typ) or not get16(msg, pos, limit, len): e.fail(tfProtocol)
    let stop = pos + len
    if stop > limit: e.fail(tfProtocol)
    if typ == ExtAlpn:
      var listLen = 0
      var nameLen = 0
      var name: seq[byte] = @[]
      if get16(msg, pos, stop, listLen) and get8(msg, pos, stop, nameLen) and
         getBytes(msg, pos, stop, nameLen, name):
        e.alpn = ""
        for b in name: e.alpn.add char(b)
    pos = stop


# ----------------------------------------------------------------- records --

proc seal(e: NimonyTlsEngine; contentType: byte; payload: openArray[byte]) =
  ## One record into `outBuf`, protected once keys are in place: the real type
  ## moves inside and the outer header always says application data.
  var rec: seq[byte] = @[]
  if not e.encrypted:
    rec.put8 int(contentType)
    rec.put16 0x0301                 # legacy_record_version, as RFC 8446 asks
    rec.put16 payload.len
    rec.putBytes payload
  else:
    var inner: seq[byte] = @[]
    inner.putBytes payload
    inner.add contentType
    rec.put8 int(ContentApplicationData)
    rec.put16 0x0303
    rec.put16 inner.len + 16
    var aad: seq[byte] = @[]
    for b in rec: aad.add b
    let tag = chacha20Poly1305Seal(e.wr.key, nonceFor(e.wr), aad, inner)
    e.wr.seqNo = e.wr.seqNo + 1'u64
    rec.putBytes inner
    rec.putBytes tag
  for b in rec: e.outBuf.add char(b)

proc onAlert(e: NimonyTlsEngine; payload: seq[byte]) {.raises.} =
  ## `close_notify` ends the stream; every other alert in TLS 1.3 is fatal.
  if payload.len >= 2 and payload[1] == 0'u8:
    e.peerClosed = true
    return
  if payload.len >= 2: e.alert = int(payload[1])
  e.fail(tfAlert)

# --------------------------------------------------------------- handshake --

proc onServerHello(e: NimonyTlsEngine; msg: seq[byte]) {.raises.} =
  var serverShare = default(X25519Key)
  parseServerHello(e, msg, serverShare)
  e.transcript.update(msg)
  let shared = x25519(e.secretKey, serverShare)
  if isAllZero(shared): e.fail(tfProtocol)
  let zeros = default(array[32, byte])
  let early = hkdfExtractSha256([], zeros)
  let derived = deriveSecret(early, "derived", initSha256())
  var hsSecret: seq[byte] = @[]
  for b in hkdfExtractSha256(derived, shared): hsSecret.add b
  e.hsSecret = hsSecret
  e.cHs = deriveSecret(hsSecret, "c hs traffic", e.transcript)
  e.sHs = deriveSecret(hsSecret, "s hs traffic", e.transcript)
  e.rd.setKeys(e.sHs)
  e.wr.setKeys(e.cHs)
  e.encrypted = true
  e.state = hsWaitEncExt

proc onServerFinished(e: NimonyTlsEngine; msg: seq[byte]) {.raises.} =
  let expected = finishedMac(e.sHs, e.transcript)
  var got: seq[byte] = @[]
  var p = 4
  if not getBytes(msg, p, msg.len, 32, got) or p != msg.len: e.fail(tfProtocol)
  if not constantTimeEquals(expected, got): e.fail(tfFinished)
  e.transcript.update(msg)
  # application secrets are derived over the transcript up to server Finished
  let zeros = default(array[32, byte])
  let derived2 = deriveSecret(e.hsSecret, "derived", initSha256())
  let master = hkdfExtractSha256(derived2, zeros)
  let cAp = deriveSecret(master, "c ap traffic", e.transcript)
  let sAp = deriveSecret(master, "s ap traffic", e.transcript)
  if e.certRequested:
    # an empty Certificate: we have none to offer
    var empty: seq[byte] = @[]
    empty.put8 int(HsCertificate)
    empty.put24 4
    empty.put8 0
    empty.put24 0
    e.transcript.update(empty)
    seal(e, ContentHandshake, empty)
  var fin: seq[byte] = @[]
  fin.put8 int(HsFinished)
  fin.put24 32
  fin.putBytes finishedMac(e.cHs, e.transcript)
  seal(e, ContentHandshake, fin)
  e.rd.setKeys(sAp)
  e.wr.setKeys(cAp)
  e.state = hsConnected

proc onKeyUpdate(e: NimonyTlsEngine; msg: seq[byte]) {.raises.} =
  if msg.len != 5: e.fail(tfProtocol)
  let requested = msg[4] == 1'u8
  e.rd.setKeys(hkdfExpandLabel(e.rd.secret, "traffic upd", [], 32))
  if requested:
    var ku: seq[byte] = @[]
    ku.put8 int(HsKeyUpdate)
    ku.put24 1
    ku.put8 0
    seal(e, ContentHandshake, ku)
    e.wr.setKeys(hkdfExpandLabel(e.wr.secret, "traffic upd", [], 32))

proc onHandshakeMessage(e: NimonyTlsEngine; typ: byte; msg: seq[byte]) {.raises.} =
  case e.state
  of hsWaitServerHello:
    if typ != HsServerHello: e.fail(tfProtocol)
    onServerHello(e, msg)
    # the rest of the handshake is protected; a fragment of it sharing the
    # ServerHello's record would have been sent in the clear, which is illegal
    if e.hs.len != 0: e.fail(tfProtocol)
  of hsWaitEncExt:
    if typ != HsEncryptedExtensions: e.fail(tfProtocol)
    parseEncryptedExtensions(e, msg)
    e.transcript.update(msg)
    e.state = hsWaitCert
  of hsWaitCert:
    if typ == HsCertificateRequest and not e.certRequested:
      e.certRequested = true
      e.transcript.update(msg)
    elif typ == HsCertificate:
      var chain: seq[Certificate] = @[]
      parseCertificates(e, msg, chain)
      e.transcript.update(msg)
      let verdict = verifyServerChain(e.store, chain, e.host, getTime().toUnix)
      if verdict != ceNone:
        e.certError = verdict
        e.fail(tfCertificate)
      if chain.len > 0: e.leaf = chain[0]
      e.state = hsWaitCertVerify
    else:
      e.fail(tfProtocol)
  of hsWaitCertVerify:
    if typ != HsCertificateVerify: e.fail(tfProtocol)
    verifyCertificateVerify(e, msg, e.leaf, e.transcript)
    e.transcript.update(msg)
    e.state = hsWaitFinished
  of hsWaitFinished:
    if typ != HsFinished: e.fail(tfProtocol)
    onServerFinished(e, msg)
  of hsConnected:
    # session tickets are dropped (no resumption); a KeyUpdate rolls our keys
    if typ == HsKeyUpdate: onKeyUpdate(e, msg)
    elif typ != HsNewSessionTicket: e.fail(tfProtocol)
  of hsIdle, hsFailed:
    e.fail(tfState)

proc processHandshake(e: NimonyTlsEngine) {.raises.} =
  ## Every whole handshake message in `hs`, in order.
  while true:
    let hs = e.hs
    if hs.len < 4: return
    let typ = hs[0]
    let bodyLen = int(hs[1]) shl 16 or int(hs[2]) shl 8 or int(hs[3])
    if bodyLen > MaxHandshake: e.fail(tfProtocol)
    if hs.len < 4 + bodyLen: return
    var msg: seq[byte] = @[]
    var rest: seq[byte] = @[]
    for i in 0 ..< hs.len:
      if i < 4 + bodyLen: msg.add hs[i]
      else: rest.add hs[i]
    e.hs = rest
    onHandshakeMessage(e, typ, msg)

proc processRecord(e: NimonyTlsEngine; hdr: array[5, byte]; body: seq[byte]) {.raises.} =
  let typ = hdr[0]
  if typ == ContentChangeCipherSpec:
    return                       # middlebox-compatibility noise; TLS 1.3 ignores it
  var contentType = typ
  var payload: seq[byte] = @[]
  if not e.encrypted or typ != ContentApplicationData:
    if e.encrypted and typ != ContentAlert: e.fail(tfProtocol)
    payload = body
  else:
    if body.len < 17: e.fail(tfDecrypt)
    let ctLen = body.len - 16
    var ct: seq[byte] = @[]
    var tag: seq[byte] = @[]
    for i in 0 ..< body.len:
      if i < ctLen: ct.add body[i]
      else: tag.add body[i]
    if not chacha20Poly1305Open(e.rd.key, nonceFor(e.rd), hdr, ct, tag):
      e.fail(tfDecrypt)
    e.rd.seqNo = e.rd.seqNo + 1'u64
    # strip the zero padding; the last non-zero byte is the real type
    var i = ct.len - 1
    while i >= 0 and ct[i] == 0'u8: dec i
    if i < 0: e.fail(tfProtocol)
    contentType = ct[i]
    for j in 0 ..< i:
      if j < ct.len: payload.add ct[j]
  case contentType
  of ContentHandshake:
    if e.hs.len + payload.len > MaxHandshake: e.fail(tfProtocol)
    e.hs.putBytes payload
    processHandshake(e)
  of ContentApplicationData:
    if e.state != hsConnected: e.fail(tfProtocol)
    for b in payload: e.app.add char(b)
  of ContentAlert:
    onAlert(e, payload)
  else:
    e.fail(tfProtocol)

# ------------------------------------------------------------- the engine --

proc newTlsEngine*(store: sink TrustStore): NimonyTlsEngine =
  ## An engine that trusts `store`'s roots.
  NimonyTlsEngine(failure: tfNone, certError: ceNone, alert: 0, alpn: "",
                  store: store, host: "", state: hsIdle,
                  secretKey: default(X25519Key), transcript: initSha256(),
                  hsSecret: @[], cHs: @[], sHs: @[], certRequested: false,
                  leaf: default(Certificate), rd: default(Direction),
                  wr: default(Direction), encrypted: false, inBuf: @[], hs: @[],
                  outBuf: "", app: "", peerClosed: false, closeSent: false)

proc newTlsEngine*(): NimonyTlsEngine =
  ## The default engine, verifying against the system's root certificates.
  newTlsEngine(loadSystemTrustStore())

method start*(e: NimonyTlsEngine; host: string) {.raises.} =
  if e.state != hsIdle: e.fail(tfState, BadOperation)
  e.host = host
  var secretKey = default(X25519Key)
  var random = default(array[32, byte])
  if not urandom(secretKey) or not urandom(random): e.fail(tfRandom, IOError)
  e.secretKey = secretKey
  let hello = clientHello(host, random, x25519Public(secretKey))
  e.transcript.update(hello)
  seal(e, ContentHandshake, hello)
  e.state = hsWaitServerHello

method recordsIn*(e: NimonyTlsEngine; src: openArray[char]; used: var int) {.raises.} =
  used = src.len
  if e.state in {hsIdle, hsFailed}: e.fail(tfState, BadOperation)
  for ch in src: e.inBuf.add byte(ch)
  while true:
    let buf = e.inBuf
    if buf.len < 5: return
    var hdr = default(array[5, byte])
    for i in 0 ..< 5: hdr[i] = buf[i]
    let len = int(hdr[3]) shl 8 or int(hdr[4])
    if len > MaxCiphertext: e.fail(tfProtocol)
    if buf.len < 5 + len: return
    var body: seq[byte] = @[]
    var rest: seq[byte] = @[]
    for i in 5 ..< buf.len:
      if i < 5 + len: body.add buf[i]
      else: rest.add buf[i]
    e.inBuf = rest
    processRecord(e, hdr, body)

method recordsOut*(e: NimonyTlsEngine; dest: var string) {.raises.} =
  dest.add e.outBuf
  e.outBuf = ""

method appIn*(e: NimonyTlsEngine; src: openArray[char]; used: var int) {.raises.} =
  ## Sealed as application records of at most 16 KiB each.
  if e.state != hsConnected or e.closeSent: e.fail(tfState, BadOperation)
  var pos = 0
  while pos < src.len:
    var chunk: seq[byte] = @[]
    while pos < src.len and chunk.len < MaxPlaintext:
      chunk.add byte(src[pos])
      inc pos
    seal(e, ContentApplicationData, chunk)
  used = src.len

method appOut*(e: NimonyTlsEngine; dest: var string) {.raises.} =
  dest.add e.app
  e.app = ""

method close*(e: NimonyTlsEngine) {.raises.} =
  if e.state == hsConnected and not e.closeSent:
    seal(e, ContentAlert, [1'u8, 0'u8])
    e.closeSent = true

method isConnected*(e: NimonyTlsEngine): bool = e.state == hsConnected

method isPeerClosed*(e: NimonyTlsEngine): bool = e.peerClosed
