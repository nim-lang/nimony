import std/syncio
import std/crypto/sha2

echo toHex(sha256(""))
echo toHex(sha256("abc"))
echo toHex(sha256("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
var s = initSha256()
for i in 0 ..< 1000: s.update("a")
echo toHex(s.finish())
echo toHex(sha384("abc"))
echo toHex(sha512("abc"))
echo toHex(sha512("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"))
