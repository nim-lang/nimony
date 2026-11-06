# Simple case mappings and letter tests at Unicode 16.0.0. Expected values
# come from UnicodeData.txt 16.0.0: the simple case mappings (fields 12-14)
# and the general category (field 2, L for letters).
import std/[assertions, syncio, unicode]

block symbolsWithCaseMappings:
  # Circled letters (So) and Roman numerals (Nl) have simple case mappings.
  assert toLower(Rune(0x24C4)) == Rune(0x24DE) # Ⓞ -> ⓞ
  assert toUpper(Rune(0x24DE)) == Rune(0x24C4)
  assert toLower(Rune(0x2169)) == Rune(0x2179) # Ⅹ -> ⅹ
  assert toUpper(Rune(0x2179)) == Rune(0x2169)
  assert toLower("ⓄⅩ Ⅻ") == "ⓞⅹ ⅻ"
  assert toUpper("ⓞⅹ ⅻ") == "ⓄⅩ Ⅻ"
  # COMBINING GREEK YPOGEGRAMMENI (Mn) upper-cases to IOTA.
  assert toUpper(Rune(0x0345)) == Rune(0x0399)
  # They have case, but they are not letters.
  assert isUpper(Rune(0x24C4)) and isLower(Rune(0x24DE))
  assert not isAlpha(Rune(0x24C4))
  assert not isAlpha(Rune(0x2169))
  assert not isAlpha(Rune(0x0345))
  assert not isAlpha("ⓄⅩ")

block lettersSinceUnicode12:
  assert toUpper(Rune(0x0264)) == Rune(0xA7CB) # ɤ -> Ɤ (Unicode 16)
  assert toLower(Rune(0xA7CB)) == Rune(0x0264)
  assert toLower(Rune(0x1C89)) == Rune(0x1C8A) # Ᲊ -> ᲊ (Unicode 14)
  assert isAlpha(Rune(0x0870)) # ARABIC LETTER ALEF WITH ATTACHED FATHA
  assert isAlpha(Rune(0x1E030)) # MODIFIER LETTER CYRILLIC SMALL A
  assert isAlpha(Rune(0x31350)) # CJK UNIFIED IDEOGRAPH-31350

block titleCase:
  assert toTitle(Rune('a')) == Rune('A')
  assert toTitle(Rune('A')) == Rune('A')
  assert toTitle(Rune('1')) == Rune('1')
  # Digraphs: ǅ is the title case of Ǆ, ǅ and ǆ.
  assert toTitle(Rune(0x01C4)) == Rune(0x01C5)
  assert toTitle(Rune(0x01C5)) == Rune(0x01C5)
  assert toTitle(Rune(0x01C6)) == Rune(0x01C5)
  assert toUpper(Rune(0x01C6)) == Rune(0x01C4)
  assert isTitle(Rune(0x01C5))
  assert not isTitle(Rune(0x01C4))
  # Georgian Mkhedruli has an upper case (Mtavruli) but is its own title case.
  assert toUpper(Rune(0x10D0)) == Rune(0x1C90)
  assert toTitle(Rune(0x10D0)) == Rune(0x10D0)
  assert toTitle(Rune(0x24DE)) == Rune(0x24C4)

block simpleMappingsOnly:
  # No SpecialCasing: ß has no simple upper case, İ lowercases to i.
  assert toUpper("ß") == "ß"
  assert toLower(Rune(0x0130)) == Rune('i')
  # No final sigma.
  assert toLower("ΣΎΚΑ") == "σύκα"
  assert toLower("ΚΎΜΗΣ") == "κύμησ"

echo "unicode case ok"
