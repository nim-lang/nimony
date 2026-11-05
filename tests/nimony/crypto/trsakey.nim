import std/[syncio, base64]
import std/crypto/[rsa, rsasign]
import std/crypto/private/pem

# Every key under rsakeys/ is a throwaway made with `openssl genpkey` for this
# test; the *.hello.sig files are `openssl dgst -sha256 -sign` of Hello.
# rsakeys/bad/ holds keys crafted from test2048 to break one rule each
# (`gen/badkeys.py` writes the DER ones; see gen/README.md).
const
  Hello = "hello nimony"
  P8 = slurp("rsakeys/test2048.pkcs8.pem")
  P1 = slurp("rsakeys/test2048.pkcs1.pem")
  Cert = slurp("certs/example.com.pem")
  Sig2048 = slurp("rsakeys/test2048.hello.sig")
  Sig2048b = slurp("rsakeys/test2048b.hello.sig")
  Sig3072 = slurp("rsakeys/test3072.hello.sig")
  Sig4096 = slurp("rsakeys/test4096.hello.sig")
  P8b = slurp("rsakeys/test2048b.pkcs8.pem")
  P8x3072 = slurp("rsakeys/test3072.pkcs8.pem")
  P1x3072 = slurp("rsakeys/test3072.pkcs1.pem")
  P8x4096 = slurp("rsakeys/test4096.pkcs8.pem")
  P1x4096 = slurp("rsakeys/test4096.pkcs1.pem")
  Der8 = slurp("rsakeys/test2048.pkcs8.der")
  Der1 = slurp("rsakeys/test2048.pkcs1.der")
  Refused = [
    ("1024 bits", slurp("rsakeys/test1024.pkcs8.pem")),
    ("1024 bits PKCS#1", slurp("rsakeys/test1024.pkcs1.pem")),
    ("encrypted PKCS#8", slurp("rsakeys/test2048.encrypted.pem")),
    ("encrypted PKCS#1", slurp("rsakeys/test2048.pkcs1-encrypted.pem")),
    ("EC key", slurp("rsakeys/ecp256.pkcs8.pem")),
    ("RSA-PSS key", slurp("rsakeys/testpss2048.pkcs8.pem")),
    ("3 primes, PKCS#8", slurp("rsakeys/bad/multiprime.pkcs8.pem")),
    ("3 primes, PKCS#1", slurp("rsakeys/bad/multiprime.pkcs1.pem"))]
  BadDer = [
    ("version1", slurp("rsakeys/bad/version1.der")),
    ("even-e", slurp("rsakeys/bad/even-e.der")),
    ("e-one", slurp("rsakeys/bad/e-one.der")),
    ("e-too-big", slurp("rsakeys/bad/e-too-big.der")),
    ("modulus-small", slurp("rsakeys/bad/modulus-small.der")),
    ("modulus-large", slurp("rsakeys/bad/modulus-large.der")),
    ("p-even", slurp("rsakeys/bad/p-even.der")),
    ("p-wide", slurp("rsakeys/bad/p-wide.der")),
    ("q-narrow", slurp("rsakeys/bad/q-narrow.der")),
    ("pq-not-n", slurp("rsakeys/bad/pq-not-n.der")),
    ("dp-wide", slurp("rsakeys/bad/dp-wide.der")),
    ("qi-wide", slurp("rsakeys/bad/qi-wide.der")),
    ("negative-dq", slurp("rsakeys/bad/negative-dq.der")),
    ("nonminimal-e", slurp("rsakeys/bad/nonminimal-e.der")),
    ("missing-qi", slurp("rsakeys/bad/missing-qi.der")),
    ("extra-field", slurp("rsakeys/bad/extra-field.der")),
    ("trailing-byte", slurp("rsakeys/bad/trailing-byte.der")),
    ("long-length", slurp("rsakeys/bad/long-length.der")),
    ("nonminimal-length", slurp("rsakeys/bad/nonminimal-length.der")),
    ("dp-corrupt", slurp("rsakeys/bad/dp-corrupt.der"))]

proc bytesOf(s: string): seq[byte] =
  result = @[]
  for c in s: result.add byte(c)

proc pemErr(s: string): RsaKeyError =
  var k = default(RsaPrivateKey)
  result = parseRsaPrivateKeyPem(s, k)

proc derErr(d: openArray[byte]): RsaKeyError =
  var k = default(RsaPrivateKey)
  result = parseRsaPrivateKeyDer(d, k)

proc replaced(s, a, b: string): string =
  ## `s` with the first `a` replaced by `b`.
  result = s
  for i in 0 .. s.len - a.len:
    if s.substr(i, i + a.len - 1) == a:
      return s.substr(0, i - 1) & b & s.substr(i + a.len)

proc good(name, pemText, sig: string) =
  var k = default(RsaPrivateKey)
  let r = parseRsaPrivateKeyPem(pemText, k)
  let pk = publicKey(k)
  echo name, ": ", r, ", ", k.modulusBits, " bits, openssl signature verifies: ",
       rsaVerifyPkcs1v15(pk, rhSha256, bytesOf(Hello), bytesOf(sig))

proc goodKeys =
  echo "-- good keys"
  good("2048 PKCS#8", P8, Sig2048)
  good("2048 PKCS#1", P1, Sig2048)
  good("2048b PKCS#8", P8b, Sig2048b)
  good("3072 PKCS#8", P8x3072, Sig3072)
  good("3072 PKCS#1", P1x3072, Sig3072)
  good("4096 PKCS#8", P8x4096, Sig4096)
  good("4096 PKCS#1", P1x4096, Sig4096)
  # another key's signature must not verify under this key's public half
  good("2048 vs 2048b signature", P8, Sig2048b)
  for (name, der) in [("PKCS#8 DER", Der8), ("PKCS#1 DER", Der1)]:
    var k = default(RsaPrivateKey)
    let r = parseRsaPrivateKeyDer(bytesOf(der), k)
    echo name, ": ", r, ", verifies: ", rsaVerifyPkcs1v15(publicKey(k), rhSha256,
         bytesOf(Hello), bytesOf(Sig2048))
  # both encodings carry the same public key
  var a = default(RsaPrivateKey)
  var b = default(RsaPrivateKey)
  discard parseRsaPrivateKeyPem(P8, a)
  discard parseRsaPrivateKeyPem(P1, b)
  echo "same modulus: ", publicKey(a).n.limbs == publicKey(b).n.limbs,
       ", same exponent: ", publicKey(a).e.limbs == publicKey(b).e.limbs
  # a failed parse leaves the key empty, even one that held a key before
  echo "reuse after failure: ", parseRsaPrivateKeyPem("junk", a), ", ",
       a.modulusBits, " bits"

proc refusedKeys =
  echo "-- refused keys"
  for (name, text) in Refused:
    echo name, ": ", pemErr(text)
  for (name, der) in BadDer:
    echo name, ": ", derErr(bytesOf(der))

proc pemArmour =
  echo "-- PEM armour"
  var crlf = ""
  for c in P8:
    if c == '\n': crlf.add '\r'
    crlf.add c
  echo "CRLF: ", pemErr(crlf)
  echo "certificate first: ", pemErr(Cert & P1)
  echo "no line breaks: ", pemErr(replaced(replaced(P8, "-----\n", "-----"), "\n-----END", "-----END"))
  echo "empty: ", pemErr("")
  echo "certificate only: ", pemErr(Cert)
  echo "no END: ", pemErr(replaced(P8, "-----END PRIVATE KEY-----", ""))
  echo "wrong END: ", pemErr(replaced(P8, "-----END PRIVATE KEY-----",
                                      "-----END RSA PRIVATE KEY-----"))
  echo "PKCS#8 body, PKCS#1 label: ", pemErr(replaced(replaced(P8,
      "BEGIN PRIVATE KEY", "BEGIN RSA PRIVATE KEY"), "END PRIVATE KEY",
      "END RSA PRIVATE KEY"))
  echo "PKCS#1 body, PKCS#8 label: ", pemErr(replaced(replaced(P1,
      "BEGIN RSA PRIVATE KEY", "BEGIN PRIVATE KEY"), "END RSA PRIVATE KEY",
      "END PRIVATE KEY"))
  echo "bad letter: ", pemErr(replaced(P8, "MIIE", "MI*E"))
  echo "letter after padding: ", pemErr(replaced(P8, "\n-----END", "=A\n-----END"))
  var der = bytesOf(Der8)
  der.setLen(der.len - 1)
  echo "truncated DER in PEM: ", pemErr("-----BEGIN PRIVATE KEY-----\n" &
    encode(der) & "\n-----END PRIVATE KEY-----\n")
  echo "header in PKCS#8: ", pemErr(replaced(P8, "KEY-----\n", "KEY-----\nX: y\n\n"))

proc derMutations =
  echo "-- DER mutations"
  let der = bytesOf(Der8)
  # every proper prefix is refused
  var accepted = 0
  for n in 0 ..< der.len:
    if derErr(der.toOpenArray(0, n - 1)) == rkeNone: inc accepted
  echo "prefixes accepted: ", accepted, " of ", der.len
  # flip the low bit of each byte and count the outcomes. Flips inside `d`,
  # dP, dQ and qInv still parse: nothing short of a signature can tell (the
  # signer's fault check does).
  var counts = default(array[RsaKeyError, int])
  for i in 0 ..< der.len:
    var m = der
    m[i] = m[i] xor 1'u8
    inc counts[derErr(m)]
  for e in low(RsaKeyError) .. high(RsaKeyError):
    if counts[e] > 0: echo "  ", e, ": ", counts[e]

proc lcg(state: var uint32): byte =
  state = state * 1664525'u32 + 1013904223'u32
  result = byte(state shr 24)

proc wrapped(s: string; width: int): string =
  result = ""
  for i, c in s:
    if i > 0 and i mod width == 0: result.add '\n'
    result.add c

proc base64Decoder =
  echo "-- branch-free base64"
  var state = 7'u32
  var mismatches = 0
  for n in 0 .. 300:
    var data: seq[byte] = @[]
    for i in 0 ..< n: data.add lcg(state)
    var got: seq[byte] = @[]
    let ok = ctBase64Decode(wrapped(encode(data), 64), got)
    if not ok or got != data or bytesOf(decode(encode(data))) != data:
      inc mismatches
  echo "round trips against std/base64, 0..300 bytes: ", mismatches, " mismatches"
  for s in ["", "QQ==", "QUI=", "QUJD", " Q U\tJ D\r\n", "QQ", "QQ=", "QUI",
            "Q===", "QR==", "QUJ=", "QQ==QQ==", "QQ==\n", "Q", "QU*D", "QU-D",
            "QU_D", "=QUJ"]:
    var got: seq[byte] = @[]
    let ok = ctBase64Decode(s, got)
    var text = ""
    for b in got: text.add char(b)
    echo "  \"", s.replaced("\t", "\\t").replaced("\r", "\\r").replaced("\n", "\\n"),
         "\": ", ok, " \"", text, "\""

goodKeys()
refusedKeys()
pemArmour()
derMutations()
base64Decoder()
