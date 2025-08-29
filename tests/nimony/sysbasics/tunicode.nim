import std/unicode
import std/assertions

proc asRune(s: string): Rune =
  ## Compile-time conversion proc for converting string literals to a Rune
  ## value. Returns the first Rune of the specified string.
  ##
  ## Shortcuts code like ``"å".runeAt(0)`` to ``"å".asRune`` and returns a
  ## compile-time constant.
  if s.len == 0: Rune(0)
  else: s.runeAt(0)

let
  someString = "öÑ"
  someRunes = toRunes(someString)
  compared = (someString == $someRunes)
assert compared == true

proc testReplacements(word: string): string =
  case word
  of "two":
    return "2"
  of "foo":
    return "BAR"
  of "βeta":
    return "beta"
  of "alpha":
    return "αlpha"
  else:
    return "12345"

assert translate("two not alpha foo βeta", testReplacements) == "2 12345 αlpha BAR beta"
assert translate("  two not foo βeta  ", testReplacements) == "  2 12345 BAR beta  "

assert title("foo bar") == "Foo Bar"
assert title("αlpha βeta γamma") == "Αlpha Βeta Γamma"
assert title("") == ""

assert capitalize("βeta") == "Βeta"
assert capitalize("foo") == "Foo"
assert capitalize("") == ""

assert swapCase("FooBar") == "fOObAR"
assert swapCase(" ") == " "
assert swapCase("Αlpha Βeta Γamma") == "αLPHA βETA γAMMA"
assert swapCase("a✓B") == "A✓b"
assert swapCase("Јамогујестистаклоитоминештети") == "јАМОГУЈЕСТИСТАКЛОИТОМИНЕШТЕТИ"
assert swapCase("ὕαλονϕαγεῖνδύναμαιτοῦτοοὔμεβλάπτει") == "ὝΑΛΟΝΦΑΓΕῖΝΔΎΝΑΜΑΙΤΟῦΤΟΟὔΜΕΒΛΆΠΤΕΙ"
assert swapCase("Կրնամապակիուտեևինծիանհանգիստչըներ") == "կՐՆԱՄԱՊԱԿԻՈՒՏԵևԻՆԾԻԱՆՀԱՆԳԻՍՏՉԸՆԵՐ"
assert swapCase("") == ""

assert isAlpha("r")
assert isAlpha("α")
assert isAlpha("ϙ")
assert isAlpha("ஶ")
assert isAlpha("网")
assert(not isAlpha("$"))
assert(not isAlpha(""))

assert isAlpha("Βeta")
assert isAlpha("Args")
assert isAlpha("𐌼𐌰𐌲𐌲𐌻𐌴𐍃𐍄𐌰𐌽")
assert isAlpha("ὕαλονϕαγεῖνδύναμαιτοῦτοοὔμεβλάπτει")
assert isAlpha("Јамогујестистаклоитоминештети")
assert isAlpha("Կրնամապակիուտեևինծիանհանգիստչըներ")
assert isAlpha("编程语言")
assert(not isAlpha("$Foo✓"))
assert(not isAlpha("⠙⠕⠑⠎⠝⠞"))

assert isSpace("\t")
assert isSpace("\l")
assert(not isSpace("Β"))
assert(not isSpace("Βeta"))

assert isSpace("\t\l \v\r\f")
assert isSpace("       ")
assert(not isSpace(""))
assert(not isSpace("ΑΓc   \td"))

assert(not isLower(' '.Rune))

assert(not isUpper(' '.Rune))

assert toUpper("Γ") == "Γ"
assert toUpper("b") == "B"
assert toUpper("α") == "Α"
assert toUpper("✓") == "✓"
assert toUpper("ϙ") == "Ϙ"
assert toUpper("") == ""

assert toUpper("ΑΒΓ") == "ΑΒΓ"
assert toUpper("AAccβ") == "AACCΒ"
assert toUpper("A✓$β") == "A✓$Β"

assert toLower("a") == "a"
assert toLower("γ") == "γ"
assert toLower("Γ") == "γ"
assert toLower("4") == "4"
assert toLower("Ϙ") == "ϙ"
assert toLower("") == ""

assert toLower("abcdγ") == "abcdγ"
assert toLower("abCDΓ") == "abcdγ"
assert toLower("33aaΓ") == "33aaγ"

assert reversed("Reverse this!") == "!siht esreveR"
assert reversed("先秦兩漢") == "漢兩秦先"
assert reversed("as⃝df̅") == "f̅ds⃝a"
assert reversed("a⃞b⃞c⃞") == "c⃞b⃞a⃞"
assert reversed("ὕαλονϕαγεῖνδύναμαιτοῦτοοὔμεβλάπτει") == "ιετπάλβεμὔοοτῦοτιαμανύδνῖεγαϕνολαὕ"
assert reversed("Јамогујестистаклоитоминештети") == "итетшенимотиолкатситсејугомаЈ"
assert reversed("Կրնամապակիուտեևինծիանհանգիստչըներ") == "րենըչտսիգնահնաիծնիևետւոիկապամանրԿ"
assert len(toRunes("as⃝df̅")) == runeLen("as⃝df̅")
const test = "as⃝"
assert lastRune(test, test.len-1)[1] == 3
assert graphemeLen("è", 0) == 2

# test for rune positioning and runeSubStr()
let s = "Hänsel  ««: 10,00€"

var t = ""
for c in s.utf8:
  t.add c

assert(s == t)

when false:
  assert(runeReverseOffset(s, 1) == (20, 18))
  assert(runeReverseOffset(s, 19) == (-1, 18))

assert(runeStrAtPos(s, 0) == "H")
assert(runeSubStr(s, 0, 1) == "H")
assert(runeStrAtPos(s, 10) == ":")
assert(runeSubStr(s, 10, 1) == ":")
assert(runeStrAtPos(s, 9) == "«")
assert(runeSubStr(s, 9, 1) == "«")
assert(runeStrAtPos(s, 17) == "€")
assert(runeSubStr(s, 17, 1) == "€")
# echo runeStrAtPos(s, 18) # index error

assert(runeSubStr(s, 0) == "Hänsel  ««: 10,00€")
assert(runeSubStr(s, -18) == "Hänsel  ««: 10,00€")
assert(runeSubStr(s, 10) == ": 10,00€")
assert(runeSubStr(s, 18) == "")
assert(runeSubStr(s, 0, 10) == "Hänsel  ««")

assert(runeSubStr(s, 12) == "10,00€")
assert(runeSubStr(s, -6) == "10,00€")

assert(runeSubStr(s, 12, 5) == "10,00")
assert(runeSubStr(s, 12, -1) == "10,00")
assert(runeSubStr(s, -6, 5) == "10,00")
assert(runeSubStr(s, -6, -1) == "10,00")

assert(runeSubStr(s, 0, 100) == "Hänsel  ««: 10,00€")
assert(runeSubStr(s, -100, 100) == "Hänsel  ««: 10,00€")
assert(runeSubStr(s, 0, -100) == "")
assert(runeSubStr(s, 100, -100) == "")

block splitTests:
  let s = " this is an example  "
  let s2 = ":this;is;an:example;;"
  let s3 = ":this×is×an:example××"
  assert s.split() == @["", "this", "is", "an", "example", "", ""]
  # assert s2.split(seps = [':'.Rune, ';'.Rune]) == @["", "this", "is", "an",
  #     "example", "", ""]
  # assert s3.split(seps = [':'.Rune, "×".asRune]) == @["", "this", "is",
  #     "an", "example", "", ""]
  assert s.split(maxsplit = 4) == @["", "this", "is", "an", "example  "]
  # assert s.split(' '.Rune, maxsplit = 1) == @["", "this is an example  "]
  # assert s3.split("×".runeAt(0)) == @[":this", "is", "an:example", "", ""]

block stripTests:
  assert(strip("") == "")
  assert(strip(" ") == "")
  assert(strip("y") == "y")
  assert(strip("  foofoofoo  ") == "foofoofoo")
  assert(strip("sfoofoofoos", runes = toOpenArray(['s'.Rune], 0, 0)) == "foofoofoo")

  block:
    let stripTestRunes = ['b'.Rune, 'a'.Rune, 'r'.Rune]
    assert(strip("barfoofoofoobar", runes = toOpenArray(stripTestRunes, 0, stripTestRunes.len-1)) == "foofoofoo")
    assert(strip("sfoofoofoos", leading = false, runes = toOpenArray(['s'.Rune], 0, 1)) == "sfoofoofoo")
    assert(strip("sfoofoofoos", trailing = false, runes = toOpenArray(['s'.Rune], 0, 1)) == "foofoofoos")

  block:
    let stripTestRunes = ["«".asRune, "»".asRune]
    assert(strip("«TEXT»", runes = toOpenArray(stripTestRunes, 0, len(stripTestRunes)-1)) == "TEXT")
    assert(strip("copyright©", leading = false, runes = toOpenArray(["©".asRune], 0, 1)) == "copyright")
    assert(strip("¿Question?", trailing = false, runes = toOpenArray(["¿".asRune], 0, 1)) == "Question?")
    assert(strip("×text×", leading = false, runes = toOpenArray(["×".asRune], 0, 1)) == "×text")
    assert(strip("×text×", trailing = false, runes = toOpenArray(["×".asRune], 0, 1)) == "text×")

block repeatTests:
  assert repeat('c'.Rune, 5) == "ccccc"
  assert repeat("×".asRune, 5) == "×××××"

block alignTests:
  assert align("abc", 4) == " abc"
  assert align("a", 0) == "a"
  assert align("1232", 6) == "  1232"
  assert align("1232", 6, '#'.Rune) == "##1232"
  assert align("1232", 6, "×".asRune) == "××1232"
  assert alignLeft("abc", 4) == "abc "
  assert alignLeft("a", 0) == "a"
  assert alignLeft("1232", 6) == "1232  "
  assert alignLeft("1232", 6, '#'.Rune) == "1232##"
  assert alignLeft("1232", 6, "×".asRune) == "1232××"

block differentSizes:
  # upper and lower variants have different number of bytes
  assert toLower("AẞC") == "aßc"
  assert toLower("ȺẞCD") == "ⱥßcd"
  assert toUpper("ⱥbc") == "ȺBC"
  assert toUpper("rsⱦuv") == "RSȾUV"
  assert swapCase("ⱥbCd") == "ȺBcD"
  assert swapCase("XyꟆaB") == "xYᶎAb"
  assert swapCase("aᵹcᲈd") == "AꝽCꙊD"

block: # bug #17768
  let s1 = "abcdef"
  let s2 = "abcdéf"

  assert s1.runeSubStr(0, -1) == "abcde"
  assert s2.runeSubStr(0, -1) == "abcdé"
