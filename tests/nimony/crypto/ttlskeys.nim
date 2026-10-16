# The TLS 1.3 key schedule against RFC 8448 §3 ("Simple 1-RTT Handshake"):
# the same ClientHello/ServerHello bytes and ECDHE secret must give the same
# handshake traffic secrets. The trace's cipher suite is AES-128-GCM, so only
# the IV (whose length and label the suites share) is compared beyond them.
import std/syncio
include std/tls

proc nibble(ch: char): int =
  if ch in {'0'..'9'}: ord(ch) - ord('0') else: ord(ch) - ord('a') + 10

proc unhex(s: string): seq[byte] =
  result = @[]
  var i = 0
  while i + 1 < s.len:
    result.add byte(nibble(s[i]) * 16 + nibble(s[i + 1]))
    i += 2

proc main =
  let ch = unhex("010000c00303cb34ecb1e78163ba1c38c6dacb196a6dffa21a8d9912ec18a2ef6283024dece7000006130113031302010000910000000b0009000006736572766572ff01000100000a00140012001d0017001800190100010101020103010400230000003300260024001d002099381de560e4bd43d23d8e435a7dbafeb3c06e51c13cae4d5413691e529aaf2c002b0003020304000d0020001e040305030603020308040805080604010501060102010402050206020202002d00020101001c00024001")
  let sh = unhex("020000560303a6af06a4121860dc5e6e60249cd34c95930c8ac5cb1434dac155772ed3e2692800130100002e00330024001d0020c9828876112095fe66762bdbf7c672e156d6cc253b833df1dd69b1b04e751f0f002b00020304")
  let shared = unhex("8bd4054fb55b9d63fdfbacf9f04b9f0d35e6d63f537563efd46272900f89492d")
  var transcript = initSha256()
  transcript.update(ch)
  transcript.update(sh)
  let zeros = default(array[32, byte])
  let early = hkdfExtractSha256([], zeros)
  echo "early   ", toHex(early)
  let derived = deriveSecret(early, "derived", initSha256())
  echo "derived ", toHex(derived)
  let hsSecret = hkdfExtractSha256(derived, shared)
  echo "hs      ", toHex(hsSecret)
  echo "c hs    ", toHex(deriveSecret(hsSecret, "c hs traffic", transcript))
  let sHs = deriveSecret(hsSecret, "s hs traffic", transcript)
  echo "s hs    ", toHex(sHs)
  echo "s iv    ", toHex(hkdfExpandLabel(sHs, "iv", [], 12))

main()
