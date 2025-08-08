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

block: # continuesWith
  assert continuesWith("", "", 0)
  assert continuesWith("", "", 1)
  assert not continuesWith("", "a", 0)
  assert not continuesWith("", "a", 1)
  assert continuesWith("a", "", 0)
  assert continuesWith("a", "", 1)
  assert continuesWith("a", "a", 0)
  assert not continuesWith("a", "aa", 0)
  assert continuesWith("aa", "a", 0)
  assert continuesWith("aa", "a", 1)
  assert not continuesWith("aa", "a", 2)
  assert not continuesWith("aa", "a", 3)
  assert continuesWith("ab", "a", 0)
  assert not continuesWith("ab", "a", 1)
  assert not continuesWith("ba", "a", 0)
  assert continuesWith("ba", "a", 1)
  assert continuesWith("bab", "a", 1)
  assert not continuesWith("bab", "a", 2)
  assert not continuesWith("bab", "a", 3)
  assert not continuesWith("a", "ab", 0)
  assert continuesWith("ab", "ab", 0)
  assert not continuesWith("AB", "ab", 0)
  assert not continuesWith("ab", "ab", 1)
  assert not continuesWith("ab", "ab", 2)
  assert continuesWith("abc", "ab", 0)
  assert not continuesWith("abc", "ab", 1)
  assert not continuesWith("abc", "bc", 0)
  assert continuesWith("abc", "bc", 1)
  assert not continuesWith("abc", "bc", 2)

block: # startsWith
  assert startsWith("", "")
  assert not startsWith("", "a")
  assert startsWith("a", "")
  assert startsWith("a", "a")
  assert not startsWith("a", "A")
  assert startsWith("aa", "a")
  assert startsWith("ab", "a")
  assert not startsWith("ba", "a")
  assert startsWith("ab", "ab")
  assert startsWith("abc", "ab")
  assert not startsWith("abc", "bc")
  assert startsWith("abc", "abc")
  assert not startsWith("bc", "abc")
  assert startsWith("abcd", "abc")

block: # endsWith
  assert endsWith("", "")
  assert not endsWith("", "a")
  assert endsWith("a", "")
  assert endsWith("a", "a")
  assert not endsWith("a", "A")
  assert endsWith("aa", "a")
  assert not endsWith("ab", "a")
  assert endsWith("ba", "a")
  assert endsWith("ab", "ab")
  assert not endsWith("abc", "ab")
  assert endsWith("abc", "bc")
  assert endsWith("abc", "abc")
  assert not endsWith("bc", "abc")
  assert not endsWith("abcd", "abc")
  assert endsWith("abcd", "bcd")

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

block: # escape
  assert escape("") == "\"\""
  assert escape("a") == "\"a\""
  assert escape("aA") == "\"aA\""
  assert escape(" /09:@AZ[`az{~") == "\" /09:@AZ[`az{~\""
  assert escape("\\") == "\"\\\\\""
  assert escape("\\\'\"") == "\"\\\\\\\'\\\"\""
  assert escape("\x00") == "\"\\x00\""
  assert escape("\x00\x01\x02\x03\x0A\x0B") == "\"\\x00\\x01\\x02\\x03\\x0A\\x0B\""
  assert escape("\x0E\x0F\x10\x11\x1A\x1B\x1E\x1F") == "\"\\x0E\\x0F\\x10\\x11\\x1A\\x1B\\x1E\\x1F\""
  assert escape("}~\x7F\x80\x81\xFE\xFF") == "\"}~\\x7F\\x80\\x81\\xFE\\xFF\""

block: # unescape
  try:
    assert unescape("\"\"") == ""
    assert unescape("\"a\"") == "a"
    assert unescape("\"aA\"") == "aA"
    assert unescape("\" /09:@AZ[`az{~\"") == " /09:@AZ[`az{~"
    assert unescape("\"\\\\\"") == "\\"
    assert unescape("\"\\\\\\\'\\\"\"") == "\\\'\""
    assert unescape("\"\\x00\\x01\\x02\\x03\\x0A\\x0B\"") == "\x00\x01\x02\x03\x0A\x0B"
    assert unescape("\"\\x0E\\x0F\\x10\\x11\\x1A\\x1B\\x1E\\x1F\"") == "\x0E\x0F\x10\x11\x1A\x1B\x1E\x1F"
    assert unescape("\"}~\\x7F\\x80\\x81\\xFE\\xFF\"") == "}~\x7F\x80\x81\xFE\xFF"
    assert unescape(r"\x013", "", "") == "\x013"
  except ErrorCode as e:
    assert false

block: # formatBiggestFloat
  assert formatBiggestFloat(0.0, ffDecimal, 0) == "0."
  assert formatBiggestFloat(0.0, ffDecimal, 1) == "0.0"
  assert formatBiggestFloat(-1.0, ffDecimal, 1) == "-1.0"
  assert formatBiggestFloat(1.0, ffDecimal, 1) == "1.0"
  assert formatBiggestFloat(-0.12, ffDecimal, 1) == "-0.1"
  assert formatBiggestFloat(0.12, ffDecimal, 1) == "0.1"
  assert formatBiggestFloat(-0.12, ffDecimal, 2) == "-0.12"
  assert formatBiggestFloat(0.12, ffDecimal, 2) == "0.12"
  assert formatBiggestFloat(1234.567, ffDecimal, -1) == "1234.567000"
  assert formatBiggestFloat(1234.567, ffDecimal, 0) == "1235."
  assert formatBiggestFloat(1234.567, ffDecimal, 1) == "1234.6"
  assert formatBiggestFloat(0.00000000001, ffDecimal, 11) == "0.00000000001"
  assert formatBiggestFloat(0.00000000001, ffScientific, 1, ',') in ["1,0e-11", "1,0e-011"]
  assert formatBiggestFloat(0.0, ffScientific, 3) == "0.000e+00"
  assert formatBiggestFloat(0.01, ffScientific, 3) == "1.000e-02"
  assert formatBiggestFloat(0.0123, ffScientific, 3) == "1.230e-02"
  assert formatBiggestFloat(123.0, ffScientific, 3) == "1.230e+02"
