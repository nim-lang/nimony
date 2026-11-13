# libFuzzer target for the RSA private key parser and its base64 decoder.
# Not a suite test (hastur does not descend into this directory).
#
# Build and run (from the repository root; clang with libFuzzer):
#
#   bin/nimony c --cc:clang -d:release -d:libFuzzer tests/nimony/crypto/fuzz/fuzzrsakey.nim
#   (the program lands in nimcache/c/<module>/fuzzrsakey)
#   mkdir -p /tmp/rsakey-corpus && cp tests/nimony/crypto/rsakeys/*.pem \
#     tests/nimony/crypto/rsakeys/*.der tests/nimony/crypto/rsakeys/bad/* /tmp/rsakey-corpus/
#   nimcache/c/*/fuzzrsakey -max_len=8192 -timeout=10 /tmp/rsakey-corpus
#
# Without `-d:libFuzzer` the program replays the files named on its command
# line (a crash reproducer). Index errors and failed checks terminate the
# process, which libFuzzer reports as a finding; the properties checked:
#
# * no input crashes either parser or runs out of bounds;
# * a key that parses gives the same answer when parsed again, and a
#   `rkeNone` key has a 2048..4096-bit modulus;
# * whatever `ctBase64Decode` accepts is canonical base64: `std/base64`
#   encodes its output back to the input with whitespace removed, and
#   `std/base64` decodes the input to the same bytes.

import std/[syncio, base64]
import std/crypto/rsasign
import std/crypto/private/pem

proc check(cond: bool; what: string) =
  if not cond: quit "fuzzrsakey: property violated: " & what

proc exercise(data: openArray[byte]) =
  var text = ""
  for b in data: text.add char(b)
  var k = default(RsaPrivateKey)
  let r = parseRsaPrivateKeyDer(data, k)
  if r == rkeNone:
    check(k.modulusBits >= 2048 and k.modulusBits <= 4096, "modulus size")
  var k2 = default(RsaPrivateKey)
  check(parseRsaPrivateKeyDer(data, k2) == r, "DER parse is deterministic")
  let rp = parseRsaPrivateKeyPem(text, k)
  if rp == rkeNone:
    check(k.modulusBits >= 2048 and k.modulusBits <= 4096, "PEM modulus size")
  var decoded: seq[byte] = @[]
  if ctBase64Decode(text, decoded):
    var compact = ""
    for c in text:
      if c notin {' ', '\t', '\r', '\n'}: compact.add c
    check(encode(decoded) == compact, "base64 is canonical")
    var viaStd: seq[byte] = @[]
    for c in decode(compact): viaStd.add byte(c)
    check(viaStd == decoded, "base64 agrees with std/base64")
  else:
    check(decoded.len == 0, "base64 output empty on failure")

when defined(libFuzzer):
  # libFuzzer's own `main` would clash with the program's: link the driver
  # without it and start it from here.
  {.passC: "-fsanitize=fuzzer-no-link,address,undefined".}
  {.passL: "-fsanitize=fuzzer-no-link,address,undefined".}
  {.passL: "-l:libclang_rt.fuzzer_no_main.a".}

  type FuzzCallback = proc (data: ptr UncheckedArray[byte]; len: csize_t): cint {.cdecl.}

  proc runDriver(argc: ptr cint; argv: ptr pointer; cb: FuzzCallback): cint {.
    importc: "LLVMFuzzerRunDriver", cdecl.}

  var
    cmdCount {.importc: "cmdCount", nodecl.}: cint
    cmdLine {.importc: "cmdLine", nodecl.}: pointer

  proc testOneInput(data: ptr UncheckedArray[byte]; len: csize_t): cint {.cdecl.} =
    result = 0
    var buf = newSeq[byte](int(len))
    for i in 0 ..< buf.len: buf[i] = data[i]
    exercise(buf)

  quit int(runDriver(addr cmdCount, cast[ptr pointer](addr cmdLine), testOneInput))
else:
  import std/cmdline
  var n = 0
  for i in 1 .. paramCount():
    var s = ""
    try:
      s = readFile(paramStr(i))
    except:
      quit "fuzzrsakey: cannot read " & paramStr(i)
    var buf: seq[byte] = @[]
    for c in s: buf.add byte(c)
    exercise(buf)
    inc n
  echo "fuzzrsakey: ", n, " inputs, no property violated"
