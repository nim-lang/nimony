# RSASSA-PKCS1-v1_5 signing (`std/crypto/rsasign`, SHA-256) with the
# throwaway test keys of tests/nimony/crypto/rsakeys, 2048, 3072 and 4096
# bits:
#
#   nimony c -d:release bench/rsasign.nim     (portable ctbignum, C at -O3)
#   nimony n -d:release bench/rsasign.nim     (native: ctbignumx64 assembler)
#   <exe> [2048|3072|4096]                    one size only, for hyperfine
#
# Each signature is checked against the public key, so a fast wrong answer
# cannot pass for a speed-up. The full run prints the mean time per
# signature; the smoke run (`-d:benchSmoke`) signs once per size and prints
# a checksum of the signatures, which are deterministic (PKCS #1 v1.5).

import std / [syncio, cmdline, monotimes, times, strutils]
import std / crypto / [rsa, rsasign]

const
  smoke = defined(benchSmoke)
  Iterations = when smoke: 1 else: 20
  Keys = [(2048, slurp("../tests/nimony/crypto/rsakeys/test2048.pkcs8.pem")),
          (3072, slurp("../tests/nimony/crypto/rsakeys/test3072.pkcs8.pem")),
          (4096, slurp("../tests/nimony/crypto/rsakeys/test4096.pkcs8.pem"))]
  Message = """eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJiZW5jaCJ9"""

proc main() =
  var only = 0
  if paramCount() >= 1:
    for (bits, _) in Keys:
      if paramStr(1) == $bits: only = bits
    if only == 0: quit "usage: rsasign [2048|3072|4096]"
  var msg: seq[byte] = @[]
  for c in Message: msg.add byte(c)
  var checksum = 0'u64
  for (bits, pemText) in Keys:
    if only != 0 and only != bits: continue
    var key = default(RsaPrivateKey)
    if parseRsaPrivateKeyPem(pemText, key) != rkeNone: quit "key refused"
    let pub = publicKey(key)
    var sig: seq[byte] = @[]
    let t0 = getMonoTime()
    for i in 0 ..< Iterations:
      if rsaSignPkcs1v15(key, rhSha256, msg, sig) != rseNone: quit "signing failed"
    let t1 = getMonoTime()
    if not rsaVerifyPkcs1v15(pub, rhSha256, msg, sig): quit "wrong signature"
    for b in sig: checksum = checksum * 31'u64 + uint64(b)
    when not smoke:
      let ns = inNanoseconds(t1 - t0)
      echo bits, " bits: ", formatFloat(float(ns) / float(Iterations) / 1e6,
                                        ffDecimal, 2), " ms per signature"
  echo "checksum ", checksum

main()
