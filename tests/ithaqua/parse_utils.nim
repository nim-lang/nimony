import std/[syncio, parseutils]

# consumed-count parsing API: no exceptions, results via var out-params
var n: BiggestInt = 0
echo parseBiggestInt("12345rest", n), " ", n     # 5 12345
echo parseBiggestInt("-99", n), " ", n           # 3 -99
echo parseBiggestInt("x12", n)                   # 0 (no parse)

var u: BiggestUInt = 0
echo parseBiggestUInt("4000000000", u), " ", u   # 10 4000000000

var h32: uint32 = 0
echo parseHex("0xDEAD", h32), " ", h32           # 6 57005
echo parseHex("ff", h32), " ", h32               # 2 255

# (parseBiggestFloat breaks the NATIVE backend's codegen — see
# nativebugs/parse_float.nim)

# skip family
echo skipUntil("hello world", ' ')               # 5
echo skipUntil("hello", 'z')                     # 5 (ran off the end)
echo skipUntil("abcdef", {'d', 'e'})             # 3
echo skipIgnoreCase("HeLLo world", "hello")      # 5
echo skipIgnoreCase("nope", "hello")             # 0
