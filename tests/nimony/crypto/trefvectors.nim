# Poly1305 and X25519 against a Python reference (`int` arithmetic straight
# from RFC 8439 and RFC 7748), on inputs chosen to reach the reductions: keys
# and messages of all ones, every interesting length, scalars and u-coordinates
# at and around p. The native backend runs hand-written assembler here and the
# C backend the portable code, so the same golden output checks both.
import std/syncio
import std/crypto/[chacha20poly1305, x25519, sha2]

proc nibble(c: char): int =
  if c in {'0'..'9'}: ord(c) - ord('0') else: ord(c) - ord('a') + 10

proc unhex(s: string): seq[byte] =
  result = @[]
  var i = 0
  while i + 1 < s.len:
    result.add byte(nibble(s[i]) * 16 + nibble(s[i+1]))
    i += 2

proc arr32(s: string): array[32, byte] =
  result = default(array[32, byte])
  let b = unhex(s)
  for i in 0 ..< 32: result[i] = b[i]

proc polyCase(bad: var int; key, msg, want: string) =
  let got = toHex(poly1305(arr32(key), unhex(msg)))
  if got != want:
    inc bad
    echo "poly1305 MISMATCH key=", key, " msg=", msg, " got ", got, " want ", want

proc xCase(bad: var int; k, u, want: string) =
  let got = toHex(x25519(arr32(k), arr32(u)))
  if got != want:
    inc bad
    echo "x25519 MISMATCH k=", k, " u=", u, " got ", got, " want ", want

proc main =
  var bad = 0
  polyCase(bad, "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "", "ffffffffffffffffffffffffffffffff")
  polyCase(bad, "00000000000000000000000000000000ffffffffffffffffffffffffffffffff", "ff", "ffffffffffffffffffffffffffffffff")
  polyCase(bad, "ffffffffffffffffffffffffffffffff00000000000000000000000000000000", "ffffffffffffffffffffffffffffff", "fcff27e6030028e6030028e6030028ee")
  polyCase(bad, "b97f69f75edf35c71fdad37066eae91d14f6ea01456b341770b835e942f549f1", "ffffffffffffffffffffffffffffffff", "a735090d4f3a05229a7f73ead8d42806")
  polyCase(bad, "d1ee73f0bdb597ced9dd0c95d4ebf0089295ebab5c2d6eb1c6f97c15ed2f3219", "ffffffffffffffffffffffffffffffffff", "180028342487281252c1179826e09506")
  polyCase(bad, "361838c42866c95ac9909f3c222a7604606de307a79997fc4802edc9301e0e04", "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "95da181353ed63daf4aa679aee52a9bd")
  polyCase(bad, "534999580a0af8a5fb5f8f9f4d8af77bfd18a69d7514ab765fad072641995e17", "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "38f24a98bb243a9642ba4f8837d9ed5c")
  polyCase(bad, "4307df2eb00ba2b6122b4a1dcfc268d8660ebe03b717ca80aebf9b7c718266e1", "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "01b12c043090e3e6c3debdc6e5f4fc97")
  polyCase(bad, "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "900fe32bc15fa8d7bca8efe4c7e37eb1")
  polyCase(bad, "00000000000000000000000000000000ffffffffffffffffffffffffffffffff", "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "ffffffffffffffffffffffffffffffff")
  polyCase(bad, "ffffffffffffffffffffffffffffffff00000000000000000000000000000000", "", "00000000000000000000000000000000")
  polyCase(bad, "b97f69f75edf35c71fdad37066eae91d14f6ea01456b341770b835e942f549f1", "ffc903f7bbf43f", "77c317841767e11ae827b09cec62db01")
  polyCase(bad, "d1ee73f0bdb597ced9dd0c95d4ebf0089295ebab5c2d6eb1c6f97c15ed2f3219", "562787106842d1aa5cc1848a8de9", "da726453b2275506312034488d476dbf")
  polyCase(bad, "361838c42866c95ac9909f3c222a7604606de307a79997fc4802edc9301e0e04", "d72045770ec18d113c82f999661ed5ac330ac41683", "a20c234ebf2fc9f26a04126a745cb7fd")
  polyCase(bad, "534999580a0af8a5fb5f8f9f4d8af77bfd18a69d7514ab765fad072641995e17", "208539941b62c40b228e03454bad0ad3575d728ee5db2510c5bf06a5", "bab8bc0ae9dadab94fcad23d983acf7b")
  polyCase(bad, "4307df2eb00ba2b6122b4a1dcfc268d8660ebe03b717ca80aebf9b7c718266e1", "8ffd6b4d1f33257b0d4aa5d9e0f3f05a62583644e66ec83fc2a6cda723dffaee4f3533", "35c1054b04da9df102da263b9b3d9007")
  polyCase(bad, "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "b03c1174ebe365e018a5b887516958aaf2d0f004a68a04f1c6f71282ca13121251d07a01baea6055ce24", "26b437ff8d88f3da929592523a2db3bd")
  polyCase(bad, "00000000000000000000000000000000ffffffffffffffffffffffffffffffff", "c16ec3b57c2f0b79616248a895d79960dae1c72a086dde0deb118413aa446896fef91f7f9d1dfc2961484813c831ad8e37", "ffffffffffffffffffffffffffffffff")
  polyCase(bad, "ffffffffffffffffffffffffffffffff00000000000000000000000000000000", "b149ed2471e7f106615bf3efd495a25886f15ec53a37631ca891f9f4ff519987aa802724ca01a6ab61372e4e24a14274a88b220a26698f6a", "5ede260b5be13cd1a0976b5d336cd9a2")
  polyCase(bad, "b97f69f75edf35c71fdad37066eae91d14f6ea01456b341770b835e942f549f1", "c44bdb46f42692a9939a77ce41db4a0a7b5a662ddb3c8c45f8ee91272eadde4c017168f455a3fbd82cae90274bb577c7c9c479d5fd9ae9768817830ab92790", "574f5173217ad1b17c7fc2f8ad918df2")
  xCase(bad, "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f", "0000000000000000000000000000000000000000000000000000000000000000")
  xCase(bad, "0000000000000000000000000000000000000000000000000000000000000000", "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f", "0000000000000000000000000000000000000000000000000000000000000000")
  xCase(bad, "66ecdc62c471a6c38be17e59e077f07e6e297a57685d36951fb903d130a8d677", "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f", "0000000000000000000000000000000000000000000000000000000000000000")
  xCase(bad, "dbded67966b39a0b7f7f6bc85a6ad6ee7afa887846acc4def3743dd1f86d4c98", "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "f3d3b815c38ab6bc511f4b1125a5aadc859e48856ef4c0d416054b4557419c55")
  xCase(bad, "006aaee1283e832bbb638970e0a6c9bba0c8f3c7731eeb847fe092d0c0611870", "0900000000000000000000000000000000000000000000000000000000000000", "266079d193e9030521346e52f7fe628f1cb6f50458ce9b2a63e99fc31056f973")
  xCase(bad, "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "029db14b5f166946b461b61f274f15c70fea842f7d42946a469da16f8945e1c4", "ef14644f9f1ecd4c3693b8948c3b915f49d01872b51f018a0291f594bd82835a")
  xCase(bad, "0000000000000000000000000000000000000000000000000000000000000000", "7d6a951ee19a68f0bfeaa087924d641ae8f154d4cd9e2fd79124367c86417760", "fb5bcb59253f90761824f41d1e5833de0c017d86a76ed67d15ff28892021ea27")
  xCase(bad, "66ecdc62c471a6c38be17e59e077f07e6e297a57685d36951fb903d130a8d677", "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f", "0000000000000000000000000000000000000000000000000000000000000000")
  echo 20, " poly1305 cases, ", 8, " x25519 cases, ", bad, " mismatches"

main()
