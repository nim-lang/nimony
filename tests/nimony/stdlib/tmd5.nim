import std/assertions
import std/md5

# RFC 1321, Appendix A.5 test suite
assert getMD5("") == "d41d8cd98f00b204e9800998ecf8427e"
assert getMD5("a") == "0cc175b9c0f1b6a831c399e269772661"
assert getMD5("abc") == "900150983cd24fb0d6963f7d28e17f72"
assert getMD5("message digest") == "f96b697d7cb7938d525a2f31aaf161d0"
assert getMD5("abcdefghijklmnopqrstuvwxyz") == "c3fcd3d76192e4007dfb496cca67e13b"
# multi-block inputs (> 55 bytes => padding spills into a second 64-byte block)
assert getMD5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") ==
  "d174ab98d277d9f5a5611c2c9f419d9f"
assert getMD5("12345678901234567890123456789012345678901234567890123456789012345678901234567890") ==
  "57edf4a22be3c955ac49da2e2107b67a"

# exactly 56 bytes: forces a full extra padding block
assert getMD5("12345678901234567890123456789012345678901234567890123456") ==
  "49f193adce178490e34d1b3a4ec0064c"

# toMD5 returns the raw 16-byte digest; $ renders it
let d = toMD5("abc")
assert d[0] == 0x90'u8
assert d[15] == 0x72'u8
assert $d == "900150983cd24fb0d6963f7d28e17f72"
