import std/[assertions, strutils]

block: # isAlphaAscii
  assert isAlphaAscii('r')
  assert isAlphaAscii('A')
  assert not isAlphaAscii('$')

block: # isAlphaNumeric
  assert isAlphaNumeric('3')
  assert isAlphaNumeric('R')
  assert not isAlphaNumeric('!')

block: # isDigit
  assert isDigit('3')
  assert not isDigit('a')
  assert not isDigit('%')

block: # isSpaceAscii
  assert isSpaceAscii('\t')
  assert isSpaceAscii('\l')
  assert not isSpaceAscii('A')

block: # isLowerAscii
  assert isLowerAscii('a')
  assert isLowerAscii('z')
  assert not isLowerAscii('A')
  assert not isLowerAscii('5')
  assert not isLowerAscii('&')
  assert not isLowerAscii(' ')

block: # isUpperAscii
  assert isUpperAscii('A')
  assert not isUpperAscii('b')
  assert not isUpperAscii('5')
  assert not isUpperAscii('%')

block: # allCharsInSet
  assert allCharsInSet("abc", {'a'..'z'})
  assert not allCharsInSet("123", {'a'..'z'})
  assert not allCharsInSet("abc123", {'a'..'z'})
  assert allCharsInSet("123abc", {'a'..'z', '0'..'9'})
  assert not allCharsInSet("ABC _", {'a'..'z', '0'..'9'})
  assert allCharsInSet("", {})

block: # isEmptyOrWhitespace
  assert isEmptyOrWhitespace("")
  assert isEmptyOrWhitespace("       ")
  assert isEmptyOrWhitespace("\t\l \v\r\f")
  assert not isEmptyOrWhitespace("ABc   \td")
  assert not isEmptyOrWhitespace("a")

block: # toLowerAscii
  assert toLowerAscii('A') == 'a'
  assert toLowerAscii('B') == 'b'
  assert toLowerAscii('Y') == 'y'
  assert toLowerAscii('Z') == 'z'
  assert toLowerAscii('a') == 'a'
  assert toLowerAscii('b') == 'b'
  assert toLowerAscii(' ') == ' '
  assert toLowerAscii('1') == '1'
  assert toLowerAscii('(') == '('

block: # toLowerAscii*(s: string): string
  assert toLowerAscii("FOO") == "foo"
  assert toLowerAscii("fOO") == "foo"
  assert toLowerAscii("foo") == "foo"
  assert toLowerAscii(" bAr123_") == " bar123_"
  assert toLowerAscii(" bar123_") == " bar123_"

block: # toUpperAscii
  assert toUpperAscii('a') == 'A'
  assert toUpperAscii('b') == 'B'
  assert toUpperAscii('y') == 'Y'
  assert toUpperAscii('z') == 'Z'
  assert toUpperAscii('A') == 'A'
  assert toUpperAscii('B') == 'B'
  assert toUpperAscii(' ') == ' '
  assert toUpperAscii('1') == '1'
  assert toUpperAscii('(') == '('

block: # toUpperAscii*(s: string): string
  assert toUpperAscii("foo") == "FOO"
  assert toUpperAscii("Foo") == "FOO"
  assert toUpperAscii("FOO") == "FOO"
  assert toUpperAscii(" BaR123_") == " BAR123_"
  assert toUpperAscii(" BAR123_") == " BAR123_"

block: # capitalizeAscii
  assert capitalizeAscii("foo") == "Foo"
  assert capitalizeAscii("1bar") == "1bar"

block: # cmpIgnoreCase
  assert cmpIgnoreCase("", "") == 0
  assert cmpIgnoreCase("", "a") < 0
  assert cmpIgnoreCase("a", "") > 0
  assert cmpIgnoreCase("a", "a") == 0
  assert cmpIgnoreCase("a", "A") == 0
  assert cmpIgnoreCase("A", "a") == 0
  assert cmpIgnoreCase("A", "A") == 0
  assert cmpIgnoreCase("a", "b") < 0
  assert cmpIgnoreCase("b", "a") > 0
  assert cmpIgnoreCase("a", "aa") < 0
  assert cmpIgnoreCase("aa", "a") > 0
  assert cmpIgnoreCase("ab", "ac") < 0
  assert cmpIgnoreCase("ab", "aC") < 0
  assert cmpIgnoreCase("aB", "ac") < 0
  assert cmpIgnoreCase("aB", "aC") < 0
  assert cmpIgnoreCase("aac", "aab") > 0
  assert cmpIgnoreCase("aaC", "aab") > 0
  assert cmpIgnoreCase("aac", "aaB") > 0
  assert cmpIgnoreCase("aaC", "aaB") > 0
  assert cmpIgnoreCase("foobar", "foobar") == 0
  assert cmpIgnoreCase("foobar", "Foobar") == 0
  assert cmpIgnoreCase("fooBar", "foobar") == 0
  assert cmpIgnoreCase("FOOBAR", "foobar") == 0
  assert cmpIgnoreCase("FOOBAR", "foobar1") < 0
  assert cmpIgnoreCase("foobar", "FOOBAR1") < 0

block: # cmpIgnoreStyle
  assert cmpIgnoreStyle("", "") == 0
  assert cmpIgnoreStyle("", "_") == 0
  assert cmpIgnoreStyle("_", "") == 0
  assert cmpIgnoreStyle("_", "_") == 0
  assert cmpIgnoreStyle("_", "__") == 0
  assert cmpIgnoreStyle("__", "_") == 0
  assert cmpIgnoreStyle("__", "__") == 0
  assert cmpIgnoreStyle("", "a") < 0
  assert cmpIgnoreStyle("a", "") > 0
  assert cmpIgnoreStyle("a", "a") == 0
  assert cmpIgnoreStyle("", "_a") < 0
  assert cmpIgnoreStyle("_a", "") > 0
  assert cmpIgnoreStyle("_", "_a") < 0
  assert cmpIgnoreStyle("_a", "_") > 0
  assert cmpIgnoreStyle("a", "b") < 0
  assert cmpIgnoreStyle("b", "a") > 0
  assert cmpIgnoreStyle("A", "b") < 0
  assert cmpIgnoreStyle("B", "a") > 0
  assert cmpIgnoreStyle("B", "a") > 0
  assert cmpIgnoreStyle("aa", "aa") == 0
  assert cmpIgnoreStyle("aa", "aaa") < 0
  assert cmpIgnoreStyle("aaa", "aa") > 0
  assert cmpIgnoreStyle("aA", "aaa") < 0
  assert cmpIgnoreStyle("aAa", "aA") > 0
  assert cmpIgnoreStyle("a", "_a") == 0
  assert cmpIgnoreStyle("_a", "a") == 0
  assert cmpIgnoreStyle("_a", "_a") == 0
  assert cmpIgnoreStyle("a", "_aa") < 0
  assert cmpIgnoreStyle("_a", "aa") < 0
  assert cmpIgnoreStyle("_a", "_aa") < 0
  assert cmpIgnoreStyle("a", "_a__a__") < 0
  assert cmpIgnoreStyle("a_b", "_ab_") == 0
  assert cmpIgnoreStyle("a_b_c", "_ab__c") == 0
  assert cmpIgnoreStyle("abc", "_a__b__c_") == 0
  assert cmpIgnoreStyle("a_B", "_ab_") == 0
  assert cmpIgnoreStyle("a_b_C", "_ab__c") == 0
  assert cmpIgnoreStyle("ABC", "_a__b__c_") == 0
