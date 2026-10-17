import std/syncio
import std/crypto/[sha2, hmac, chacha20poly1305, x25519]

proc hexVal(c: char): int =
  if c in {'0'..'9'}: ord(c) - ord('0')
  elif c in {'a'..'f'}: ord(c) - ord('a') + 10
  else: ord(c) - ord('A') + 10

proc unhex(s: string): seq[byte] =
  result = @[]
  var i = 0
  while i + 1 < s.len:
    result.add byte(hexVal(s[i]) * 16 + hexVal(s[i+1]))
    i += 2

proc bytesOf(s: string): seq[byte] =
  result = @[]
  for c in s: result.add byte(c)

proc arr32(s: string): array[32, byte] =
  result = default(array[32, byte])
  let b = unhex(s)
  for i in 0 ..< 32: result[i] = b[i]

proc arr12(s: string): array[12, byte] =
  result = default(array[12, byte])
  let b = unhex(s)
  for i in 0 ..< 12: result[i] = b[i]

# RFC 4231 test case 2
echo toHex(hmacSha256(bytesOf("Jefe"), bytesOf("what do ya want for nothing?")))
echo toHex(hmacSha384(bytesOf("Jefe"), bytesOf("what do ya want for nothing?")))

# RFC 5869 test case 1
let prk = hkdfExtractSha256(unhex("000102030405060708090a0b0c"),
                            unhex("0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"))
echo toHex(prk)
echo toHex(hkdfExpandSha256(prk, unhex("f0f1f2f3f4f5f6f7f8f9"), 42))

# RFC 8439 2.3.2
var blk = default(array[64, byte])
chachaBlock(arr32("000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"),
            1'u32, arr12("000000090000004a00000000"), blk)
echo toHex(blk)

# RFC 8439 2.5.2
echo toHex(poly1305(arr32("85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"),
                    bytesOf("Cryptographic Forum Research Group")))

# RFC 8439 2.8.2
let key = arr32("808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f")
let nonce = arr12("070000004041424344454647")
let aad = unhex("50515253c0c1c2c3c4c5c6c7")
var msg = bytesOf("Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it.")
let tag = chacha20Poly1305Seal(key, nonce, aad, msg)
echo toHex(msg)
echo toHex(tag)
echo chacha20Poly1305Open(key, nonce, aad, msg, tag)
var plain = ""
for b in msg: plain.add char(b)
echo plain
var bad = tag
bad[0] = bad[0] xor 1'u8
echo chacha20Poly1305Open(key, nonce, aad, msg, bad)

# RFC 7748 5.2
echo toHex(x25519(arr32("a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"),
                  arr32("e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c")))
# RFC 7748 6.1
let aliceSk = arr32("77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a")
let bobSk = arr32("5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
echo toHex(x25519Public(aliceSk))
echo toHex(x25519Public(bobSk))
echo toHex(x25519(aliceSk, x25519Public(bobSk)))
