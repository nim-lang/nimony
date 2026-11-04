# toNFD, toNFKD, isNonspacingMark and isCombining on Greek, Cyrillic, Arabic,
# Latin and Japanese text. Every expected value was produced by Python 3.14's
# unicodedata (Unicode 16.0.0): normalize("NFD"/"NFKD", s) and category(c).
import std/[assertions, syncio, unicode]

proc stripMarks(s: string): string =
  ## Drops the nonspacing marks: `"".join(c for c in s if category(c) != "Mn")`.
  result = ""
  for r in runes(s):
    if not isNonspacingMark(r): result.add r

proc fold(s: string): string =
  ## Lower case, compatibility decomposition, no accents.
  result = stripMarks(toNFKD(toLower(s)))

block ascii:
  assert toNFD("") == ""
  assert toNFKD("") == ""
  assert toNFD("plain ASCII 123") == "plain ASCII 123"
  assert toNFKD("plain ASCII 123") == "plain ASCII 123"

block greek:
  assert toNFD("ά") == "ά"
  assert toNFD("Ά") == "Ά"
  assert toNFD("ΐ") == "ΐ"
  assert toNFD("ΰ") == "ΰ"
  assert toNFD("ῷ") == "ῷ"
  assert toNFD("ς") == "ς"
  assert toNFKD("ς") == "ς"
  assert fold("Ἀθῆναι") == "αθηναι"
  assert fold("σύκα κύμης") == "συκα κυμης"
  assert fold("ΣΎΚΑ ΚΎΜΗΣ") == "συκα κυμησ" # simple lower case: no final sigma

block cyrillic:
  assert toNFD("й") == "й"
  assert toNFD("Й") == "Й"
  assert toNFD("ё") == "ё"
  assert toNFD("Ё") == "Ё"
  assert toNFD("ї") == "ї"
  assert fold("Йод ёлка") == "иод елка"

block arabic:
  assert toNFD("أ") == "أ"
  assert toNFD("إ") == "إ"
  assert toNFD("آ") == "آ"
  assert toNFD("ؤ") == "ؤ"
  assert toNFD("ئ") == "ئ"
  assert toNFD("ۀ") == "ۀ"
  # Harakat are not decomposed, only marked.
  assert toNFD("مُحَمَّد") == "مُحَمَّد"
  assert stripMarks("مُحَمَّد") == "محمد"
  assert stripMarks(toNFD("أإآؤئ")) == "اااوي"
  assert toNFD("ﻷ") == "ﻷ"
  assert toNFKD("ﻷ") == "لأ" # ligature lam with alef with hamza
  assert toNFKD("ﷺ") == "صلى الله عليه وسلم"
  for c in [0x064B, 0x064C, 0x064D, 0x064E, 0x064F, 0x0650, 0x0651, 0x0652,
            0x0653, 0x0654, 0x0655, 0x0670]:
    assert isNonspacingMark(Rune(c))

block latin:
  assert toNFD("é") == "é"
  assert toNFD("Å") == "Å"
  assert toNFD("Å") == "Å" # ANGSTROM SIGN
  assert toNFD("ệ") == "ệ"
  assert toNFD("q̣̇") == "q̣̇" # canonical order
  assert fold("Naïve Café") == "naive cafe"

block compatibility:
  assert toNFD("ﬁ") == "ﬁ"
  assert toNFKD("ﬁ") == "fi"
  assert toNFKD("x²") == "x2"
  assert toNFKD("①") == "1"
  assert toNFKD("ǅ") == "Dž"
  assert toNFD("‑") == "‑"
  assert toNFKD("‑") == "‐" # NON-BREAKING HYPHEN
  assert toNFD("が") == "が"
  assert toNFD("ｶﾞ") == "ｶﾞ"
  assert toNFKD("ｶﾞ") == "ガ" # halfwidth katakana

block hangul:
  assert toNFD("가") == "가"
  assert toNFD("각") == "각"

block marks:
  for c in [0x0301, 0x0308, 0x0342, 0x0345, 0x3099, 0x05B0]:
    assert isNonspacingMark(Rune(c))
    assert isCombining(Rune(c))
  # Mc and Me are combining marks but not nonspacing.
  for c in [0x0903, 0x20DD, 0x0488]:
    assert isCombining(Rune(c))
    assert not isNonspacingMark(Rune(c))
  for c in [0x0041, 0x03B1, 0x0627, 0x2010, 0x02B0]:
    assert not isCombining(Rune(c))
    assert not isNonspacingMark(Rune(c))

block graphemes:
  # A spacing mark (Mc) stays with its letter.
  assert graphemeLen("नः", 0) == 6
  assert reversed("aनःb") == "bनःa"

echo "unicode normalization ok"
