# native: runeLen/toUpper/editDistance produce wrong values, then it dies mid-run
# (buffered stdout lost). wasm: full correct output — see git history of tests/ithaqua/uni_text.nim.
import std/[syncio, unicode, editdistance, wordwrap]

# runes: multi-byte UTF-8 handling
let s = "caf\xc3\xa9 na\xc3\xafve"    # "café naïve" spelled out in UTF-8 bytes
echo s.len                             # byte length: 12
echo runeLen(s)                        # rune length: 10
echo toUpper(s)
echo toLower("HEISSE W\xc3\x9cRDE")
echo reversed("ab\xc3\xa9")            # rune-aware reverse

var runes = ""
for r in s.runes:
  runes.add $int32(r.ord)
  runes.add " "
echo runes

echo validateUtf8("good utf8")         # -1 (valid)
echo validateUtf8("bad \xff byte")     # index of the bad byte

# edit distance in runes
echo editDistance("kitten", "sitting")     # 3
echo editDistance("caf\xc3\xa9", "cafe")   # 1

# word wrapping is pure string layout
echo wrapWords("the quick brown fox jumps over the lazy dog", 10)
echo "---"
echo wrapWords("supercalifragilistic", 5, splitLongWords = true)
