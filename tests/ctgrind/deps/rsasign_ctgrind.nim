# ctgrind check of RSA signing through the public API of `std/crypto/rsasign`,
# from the PEM text to the signature. Run by `tests/ctgrind/setup.nim`, which
# reads memcheck's report against the list of intended declassification
# points there; see that file for the commands.
#
# Three stages per key, each with its secret marked for memcheck:
#
# A. PEM armour: every character of the base64 body is secret, and
#    `ctBase64Decode` (std/crypto/private/pem) decodes it. Memcheck cannot
#    tell a character's class (letter, space, `=`) from its value, so the
#    body's layout (its length, line breaks, padding) is reported where the
#    decoder counts it; the setup's allow-list names those places.
# B. Key parsing: the DER is decoded here with `std/base64`, and the content
#    bytes of the six secret INTEGERs (d, p, q, dP, dQ, qInv) are marked;
#    tags and lengths stay public, as `rsasign` documents.
#    `parseRsaPrivateKeyDer` must then branch only on the key's validity.
# C. Signing: `rsaSignPkcs1v15` with the key from B. The fault check's
#    verdict is the only place allowed to look at a derived value.
#
# The C backend marks with memcheck's client requests and declassifies each
# stage's results before checking them. The native backend has no C header,
# so a secret is TAINTED there instead (an undefined zero is added to it, see
# `undefinedZero`); nothing can be declassified that way, so the tainted pass
# only computes, and a second, untainted pass checks the results.
#
# The blinding factors come from the OS inside `rsaSignPkcs1v15` and cannot
# be marked from here: they are checked only where they meet the key.

import std/[syncio, strutils, base64]
import std/crypto/[rsa, rsasign]
import std/crypto/private/[der, pem]

when defined(nimNativeBackend) and defined(amd64):
  proc subq(d: var uint64; s: uint64) {.instruction: "sub".}

  proc undefinedZero(): uint64 {.assembler.} =
    ## Zero, with every bit undefined to memcheck: the same never-written
    ## stack word read through two slots and subtracted (through memory, so
    ## VEX cannot see that the operands are equal).
    var u {.stack.}: uint64
    var u2 {.stack.}: uint64
    var d {.register: "rax".}: uint64
    var e {.register: "rcx".}: uint64
    d = u
    u2 = d
    e = u2
    subq(d, e)
    result = d

  const Tainting = true

  proc secret(p: ptr UncheckedArray[byte]; len: int) =
    let z = byte(undefinedZero() and 0xFF'u64)
    for i in 0 ..< len: p[i] = p[i] + z

  proc public(p: pointer; len: int) = discard
else:
  proc makeMemUndefined(p: pointer; len: int) {.
    importc: "VALGRIND_MAKE_MEM_UNDEFINED", header: "<valgrind/memcheck.h>".}
  proc makeMemDefined(p: pointer; len: int) {.
    importc: "VALGRIND_MAKE_MEM_DEFINED", header: "<valgrind/memcheck.h>".}

  const Tainting = false

  proc secret(p: ptr UncheckedArray[byte]; len: int) =
    if len > 0: makeMemUndefined(p, len)

  proc public(p: pointer; len: int) =
    if len > 0: makeMemDefined(p, len)

const
  OpensslTxt = slurp("../../nimony/crypto/rsavectors/openssl.txt")
  Keys = [
    ("test2048", slurp("../../nimony/crypto/rsakeys/test2048.pkcs8.pem")),
    ("test2048", slurp("../../nimony/crypto/rsakeys/test2048.pkcs1.pem")),
    ("test2048b", slurp("../../nimony/crypto/rsakeys/test2048b.pkcs8.pem")),
    ("test3072", slurp("../../nimony/crypto/rsakeys/test3072.pkcs8.pem")),
    ("test4096", slurp("../../nimony/crypto/rsakeys/test4096.pkcs1.pem"))]
  Message = "hello nimony"

proc bytesOf(s: string): seq[byte] =
  result = @[]
  for c in s: result.add byte(c)

proc expected(name: string): string =
  ## OpenSSL's signature of `Message` with SHA-256, as hex.
  result = ""
  for line in OpensslTxt.splitLines:
    let f = line.splitWhitespace
    if f.len == 5 and f[0] == "s" and f[1] == name and f[2] == "256" and
       f[3] == "hello":
      result = f[4]

proc bodyOf(pem: string): string =
  ## The base64 body of the private key block, line breaks included.
  let b = pem.find("-----BEGIN ")
  if b < 0: quit "ctgrind: no PEM block"
  let s = pem.find('\n', b) + 1
  if s <= 0: quit "ctgrind: no PEM body"
  let e = pem.find("-----END ", s)
  result = pem.substr(s, e - 1)

proc derOf(body: string): seq[byte] =
  var flat = ""
  for c in body:
    if c notin {'\n', '\r', ' ', '\t'}: flat.add c
  result = bytesOf(decode(flat))

proc markSecretInts(d: var seq[byte]) =
  ## Marks the content bytes of d, p, q, dP, dQ and qInv: the INTEGERs 3..8
  ## of the RSAPrivateKey, found inside a PKCS #8 wrapper when there is one.
  var s, t = default(Tlv)
  if not readTlv(d, 0, d.len, s): quit "ctgrind: DER"
  var first, second = default(Tlv)
  discard readTlv(d, s.start, s.next, first)
  discard readTlv(d, first.next, s.next, second)
  if second.tag == TagSeq:                    # PKCS #8: OCTET STRING { SEQ }
    var octets = default(Tlv)
    discard readTlv(d, second.next, s.next, octets)
    discard readTlv(d, octets.start, octets.next, s)
  var p = s.start
  var i = 0
  while p < s.next and readTlv(d, p, s.next, t):
    if i >= 3 and t.len > 0:
      secret(cast[ptr UncheckedArray[byte]](addr d[t.start]), t.len)
    inc i
    p = t.next
  if i != 9: quit "ctgrind: not a two-prime key"

# One proc per stage, never inlined, so that `tests/ctgrind/setup.nim` can
# tell from a report's stack which stage it belongs to.

proc stageArmour(pemText: string; mark: bool; der: var seq[byte]): bool {.noinline.} =
  ## A: `ctBase64Decode` of the body, every character secret.
  var body: seq[char] = @[]       # not a string: an SSO string has no stable `addr`
  for c in bodyOf(pemText): body.add c
  if mark and body.len > 0:
    secret(cast[ptr UncheckedArray[byte]](addr body[0]), body.len)
  var decoded: seq[byte] = @[]
  var ok = ctBase64Decode(body.toOpenArray(0, body.len - 1), decoded)
  if mark:
    public(addr ok, sizeof(ok))
    public(addr decoded, sizeof(decoded))   # its length counts the letters
    if decoded.len > 0: public(addr decoded[0], decoded.len)
    if body.len > 0: public(addr body[0], body.len)
  result = false
  if not (mark and Tainting):
    der = decoded
    result = ok

proc stageScan(pemText: string; mark: bool): int {.noinline.} =
  ## A': the scans `parseRsaPrivateKeyPem` makes over the secret body, to
  ## find the END line and a `Proc-Type:` header. Only the latter's verdict
  ## may be reported.
  var body: seq[char] = @[]
  for c in bodyOf(pemText): body.add c
  if mark and body.len > 0:
    secret(cast[ptr UncheckedArray[byte]](addr body[0]), body.len)
  var at = ctFindByte(body.toOpenArray(0, body.len - 1), 0, '-')
  var header = ctContinuesWith(body.toOpenArray(0, body.len - 1), 1, "Proc-Type:")
  if mark:
    public(addr at, sizeof(at))
    public(addr header, sizeof(header))
    if body.len > 0: public(addr body[0], body.len)
  result = 0
  if not (mark and Tainting):
    result = at + ord(header)

proc stageKey(der: var seq[byte]; mark: bool; key: var RsaPrivateKey): RsaKeyError {.noinline.} =
  ## B: `parseRsaPrivateKeyDer` with the secret INTEGERs marked.
  if mark: markSecretInts(der)
  result = parseRsaPrivateKeyDer(der, key)
  when defined(ctgrindSelfTest):
    # the last byte of the DER belongs to qInv: a table index and a branch
    # on it are leaks the setup's checker must refuse
    if mark and der.len > 0:
      var table: seq[byte] = @[]
      for i in 0 ..< 256: table.add byte((i * 7 + 3) and 0xFF)
      if table[der[der.len - 1]] == 7'u8: echo "selftest"
  if mark:
    public(addr result, sizeof(result))
    if der.len > 0: public(addr der[0], der.len)  # mimalloc's `free` reads it

proc stageSign(key: RsaPrivateKey; mark: bool; sig: var seq[byte]): RsaSignError {.noinline.} =
  ## C: `rsaSignPkcs1v15` with the key of B.
  result = rsaSignPkcs1v15(key, rhSha256, bytesOf(Message), sig)
  if mark:
    public(addr result, sizeof(result))
    if sig.len > 0: public(addr sig[0], sig.len)

proc run(mark: bool): int =
  ## Parses and signs with every key; with `mark`, from secrets marked for
  ## memcheck. Returns the number of wrong results (0 for a tainted pass,
  ## whose results cannot be looked at).
  result = 0
  let check = not (mark and Tainting)
  for (name, pemText) in Keys:
    var decoded: seq[byte] = @[]
    let ok = stageArmour(pemText, mark, decoded)
    var der = derOf(bodyOf(pemText))
    if check and (not ok or decoded != der): inc result
    let scan = stageScan(pemText, mark)
    if check and scan != bodyOf(pemText).len: inc result
    var key = default(RsaPrivateKey)
    let err = stageKey(der, mark, key)
    if check and err != rkeNone: quit "ctgrind: key refused: " & name
    var sig: seq[byte] = @[]
    let serr = stageSign(key, mark, sig)
    if check:
      if serr != rseNone: inc result
      else:
        var hex = ""
        for b in sig: hex.add toHex(int(b), 2)
        if toLowerAscii(hex) != expected(name): inc result
        if not rsaVerifyPkcs1v15(publicKey(key), rhSha256, bytesOf(Message), sig):
          inc result

proc main() =
  var failures = run(true)
  if Tainting: failures = run(false)
  if failures == 0: echo "ctgrind ok"
  else: echo "ctgrind: ", failures, " wrong results"

main()
