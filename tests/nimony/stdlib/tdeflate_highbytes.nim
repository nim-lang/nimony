## Regression: the fixed Huffman table has 288 literal/length symbols. Symbols
## 286/287 are unused but still consume two 8-bit codes; leaving them out of
## the canonical encoder shifted every 9-bit literal (144..255) down by four.

import std / [syncio, assertions, compress/deflate]

proc main {.raises.} =
  for c in 0 .. 255:
    var s = newString(10)
    for i in 0 ..< s.len: s[i] = char(c)
    let z = deflate(toOpenArray(s, 0, s.len - 1), 6)
    var dec = ""
    assert inflate(toOpenArray(z, 0, z.len - 1), dec, 1_000_000) == infDone
    assert dec == s, "byte " & $c & " round-trip"
  echo "tdeflate_highbytes: ok"

try:
  main()
except ErrorCode as e:
  echo "tdeflate_highbytes: FAIL ", e
  quit(1)
