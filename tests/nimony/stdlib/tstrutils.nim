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

block: # delete
  try:
    block:
      var s = "a"
      delete s, 0..0
      assert s == ""
    block:
      var s = "ab"
      delete s, 0..0
      assert s == "b"
      s = "ab"
      delete s, 1..1
      assert s == "a"
      s = "ab"
      delete s, 0..1
      assert s == ""
    block:
      var s = "abc"
      delete s, 0..0
      assert s == "bc"
      s = "abc"
      delete s, 1..1
      assert s == "ac"
      s = "abc"
      delete s, 2..2
      assert s == "ab"
      s = "abc"
      delete s, 0..1
      assert s == "c"
      s = "abc"
      delete s, 1..2
      assert s == "a"
      s = "abc"
      delete s, 0..2
      assert s == ""
    block:
      var s = "abcd"
      delete s, 0..0
      assert s == "bcd"
      s = "abcd"
      delete s, 1..1
      assert s == "acd"
      s = "abcd"
      delete s, 2..2
      assert s == "abd"
      s = "abcd"
      delete s, 3..3
      assert s == "abc"
      s = "abcd"
      delete s, 0..1
      assert s == "cd"
      s = "abcd"
      delete s, 1..2
      assert s == "ad"
      s = "abcd"
      delete s, 2..3
      assert s == "ab"
      s = "abcd"
      delete s, 0..2
      assert s == "d"
      s = "abcd"
      delete s, 1..3
      assert s == "a"
      s = "abcd"
      delete s, 0..3
      assert s == ""
    block:
      let s0 = "0123456789ABCDEFGH"
      var s = s0
      delete s, 0..0
      assert s == "123456789ABCDEFGH"
      s = s0
      delete s, 17..17
      assert s == "0123456789ABCDEFG"
      s = s0
      delete s, 0..9
      assert s == "ABCDEFGH"
      s = s0
      delete s, 10..17
      assert s == "0123456789"
      s = s0
      delete s, 1..17
      assert s == "0"
      s = s0
      delete s, 0..16
      assert s == "H"
      s = s0
      delete s, 0..17
      assert s == ""
  except ErrorCode as e:
    assert false

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

block: # find
  assert find("", 'A') == -1
  const haystack: string = "0123456789ABCDEFGH"
  assert haystack.find('A') == 10
  assert haystack.find('A', 5) == 10
  assert haystack.find('A', 5, 10) == 10
  assert haystack.find('A', 5, 9) == -1
  assert haystack.find('A', 0, 0) == -1 # search limited to the first char
  assert haystack.find('A', 5, 0) == -1 # last < start
  assert haystack.find('A', 5, 4) == -1 # last < start
  assert "".find({}) == -1
  assert "".find({'A'..'C'}) == -1
  assert "1".find({'A'..'C'}) == -1
  assert "abc".find({'A'..'C'}) == -1
  assert "A".find({'A'..'C'}) == 0
  assert "C".find({'A'..'C'}) == 0
  assert haystack.find({'A'..'C'}) == 10
  assert haystack.find({'A'..'C'}, 5) == 10
  assert haystack.find({'A'..'C'}, 5, 10) == 10
  assert haystack.find({'A'..'C'}, 5, 9) == -1

  assert "".find("") == 0
  assert "".find("a") == -1
  assert "".find("aa") == -1
  assert "".find("ab") == -1
  assert "a".find("") == 0
  assert "a".find("a") == 0
  assert "a".find("aa") == -1
  assert "a".find("ab") == -1
  assert "a".find("aaa") == -1
  assert "aa".find("") == 0
  assert "aa".find("a") == 0
  assert "aa".find("aa") == 0
  assert "aa".find("ab") == -1
  assert "aa".find("aaa") == -1
  assert "ab".find("") == 0
  assert "ab".find("a") == 0
  assert "ab".find("aa") == -1
  assert "ab".find("ab") == 0
  assert "ab".find("abc") == -1
  assert "abc".find("") == 0
  assert "abc".find("a") == 0
  assert "abc".find("aa") == -1
  assert "abc".find("ab") == 0
  assert "abc".find("abc") == 0
  assert "abc".find("bc") == 1
  assert "abc".find("c") == 2

  assert "abc".find("", start = 1) == 1
  assert "abc".find("", start = 2) == 2
  assert "abc".find("", start = 3) == 3
  assert "abc".find("", start = 4) == -1
  assert "abc".find("", start = 400) == -1
  assert "abc".find("", start = 1, last=3) == 1
  assert "abc".find("", start = 1, last=2) == 1
  assert "abc".find("", start = 1, last=1) == 1
  assert "abc".find("", start = 1, last=0) == 1
  assert "abc".find("", start = 1, last = -1) == 1

  assert "abc".find("a", start = 1) == -1
  assert "abc".find("a", start = 2) == -1
  assert "abc".find("a", start = 3) == -1
  assert "abc".find("a", start = 4) == -1
  assert "abc".find("a", start = 400) == -1
  assert "abc".find("a", start = 1, last=2) == -1
  assert "abc".find("a", start = 1, last=1) == -1
  assert "abc".find("a", start = 1, last=0) == -1
  assert "abc".find("a", start = 1, last = -1) == -1

  assert "abc".find("bc", start = 1) == 1
  assert "abc".find("bc", start = 2) == -1
  assert "abc".find("bc", start = 3) == -1
  assert "abc".find("bc", start = 4) == -1
  assert "abc".find("bc", start = 400) == -1
  assert "abc".find("bc", start = 1, last=2) == 1
  assert "abc".find("bc", start = 1, last=1) == -1
  assert "abc".find("bc", start = 1, last=0) == -1
  assert "abc".find("bc", start = 1, last = -1) == 1

  block:
    const haystack: string = "ABCABABABABCAB"
    assert haystack.len == 14

    # only last argument
    assert haystack.find("ABC") == 0
    assert haystack.find("ABC", last=12) == 0 # after the second ABC
    assert haystack.find("ABC", last=5) == 0 # before the second ABC

    # only start argument
    assert haystack.find("ABC", start=0) == 0
    assert haystack.find("ABC", start=1) == 9
    assert haystack.find("ABC", start=9) == 9
    assert haystack.find("ABC", start=10) == -1

    # both start and last arguments
    assert haystack.find("ABC", start=0, last=14) == 0
    assert haystack.find("ABC", start=0, last=13) == 0
    assert haystack.find("ABC", start=0, last=12) == 0
    assert haystack.find("ABC", start=1, last=13) == 9
    assert haystack.find("ABC", start=1, last=12) == 9
    assert haystack.find("ABC", start=1, last=11) == 9
    assert haystack.find("ABC", start=1, last=10) == -1

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

block: # formatFloat
  assert formatFloat(1234.567, ffDecimal, 1) == "1234.6"

block: # `%`
  try:
    assert "" % ["a"] == ""
    assert "1" % ["a"] == "1"
    assert "$$" % ["a"] == "$"
    assert "$1" % ["a"] == "a"
    assert "$$$1" % ["a"] == "$a"
    assert "$1$$" % ["a"] == "a$"
    assert "$2$1" % ["a", "b"] == "ba"
    assert "$# $3 $# $#" % ["a", "b", "c"] == "a c b c"
    assert "${1}12 ${-1}$2" % ["a", "b"] == "a12 bb"
    assert "$animal eats $food." % ["animal", "The cat", "food", "fish"] ==
             "The cat eats fish."
    assert "$x $y" % ["x", "a", "y", "b"] == "a b"
    assert "$x $y" % ["x", "a", "y", "b", "x"] == "a b"
  except:
    assert false

block: # strip
  assert strip("") == ""
  assert strip("", leading = false) == ""
  assert strip("", trailing = false) == ""
  assert strip(" ") == ""
  assert strip("\t \n") == ""
  assert strip("a") == "a"
  assert strip("a", leading = false) == "a"
  assert strip("a", trailing = false) == "a"
  assert strip(" a", leading = false) == " a"
  assert strip(" a", trailing = false) == "a"
  assert strip("a ", leading = false) == "a"
  assert strip("a ", trailing = false) == "a "
  assert strip("ab") == "ab"
  assert strip(" ab") == "ab"
  assert strip(" a b") == "a b"
  assert strip(" a b ") == "a b"
  assert strip("a  b ") == "a  b"
  assert strip("  ha  ") == "ha"
  assert strip("  foofoofoo  ") == "foofoofoo"
  assert strip("sfoofoofoos", chars = {'s'}) == "foofoofoo"
  assert strip("sfoosfoos", chars = {'s'}) == "foosfoo"
  assert strip("barfoofoofoobar", chars = {'b', 'a', 'r'}) == "foofoofoo"
  assert strip("bar", chars = {'b', 'a', 'r'}) == ""
  assert strip("rab", chars = {'b', 'a', 'r'}) == ""
  assert strip("bbbb", chars = {'b', 'a', 'r'}) == ""
  assert strip("BAR", chars = {'b', 'a', 'r'}) == "BAR"
  assert strip("bar", chars = {'B', 'A', 'R'}) == "bar"
  assert strip("bar-bar-bar", chars = {'b', 'a', 'r'}) == "-bar-"
  assert strip("stripme but don't strip this stripme",
               chars = {'s', 't', 'r', 'i', 'p', 'm', 'e'}) ==
               " but don't strip this "
  assert strip("sfoofoos", leading = false, chars = {'s'}) == "sfoofoo"
  assert strip("sfoofoos", trailing = false, chars = {'s'}) == "foofoos"
  assert "xxxxxx".strip(chars={'x'}) == ""
  assert "".strip(chars={'x'}).len == 0
  assert "         ".strip(chars={'x'}) == "         "
  assert "xxx xxx".strip(chars={'x'}) == " "
  assert "xxx  wind".strip(chars={'x'}) == "  wind"
  assert "xxx  iii".strip(chars={'i'}) == "xxx  "
  assert "x".strip(leading = false, chars={'x'}).len == 0
  assert "x".strip(trailing = false, chars={'x'}).len == 0
  assert "x".strip(leading = false, trailing = false, chars={'x'}) == "x"
  assert "xyz".strip(leading = false, trailing = false, chars={'x', 'y', 'z'}) == "xyz"

block: # trimZeros
  var x = ""
  x.trimZeros
  assert x == ""
  x = "0"
  x.trimZeros
  assert x == "0"
  x = "+0"
  x.trimZeros
  assert x == "+0"
  x = "-0"
  x.trimZeros
  assert x == "-0"
  x = "0."
  x.trimZeros()
  assert x == "0"
  x = "0.0"
  x.trimZeros()
  assert x == "0"
  x = "1"
  x.trimZeros
  assert x == "1"
  x = "+1"
  x.trimZeros
  assert x == "+1"
  x = "-1"
  x.trimZeros
  assert x == "-1"
  x = "1200"
  x.trimZeros()
  assert x == "1200"
  x = "120.0"
  x.trimZeros()
  assert x == "120"
  x = "120.00"
  x.trimZeros()
  assert x == "120"
  x = "120.000"
  x.trimZeros()
  assert x == "120"
  x = "0.01"
  x.trimZeros()
  assert x == "0.01"
  x = "0.010"
  x.trimZeros()
  assert x == "0.01"
  x = "0.0100"
  x.trimZeros()
  assert x == "0.01"
  x = "-0.010"
  x.trimZeros()
  assert x == "-0.01"
  x = "1.01"
  x.trimZeros()
  assert x == "1.01"
  x = "1.010"
  x.trimZeros()
  assert x == "1.01"
  x = "1.0100"
  x.trimZeros()
  assert x == "1.01"
  x = "1.001"
  x.trimZeros()
  assert x == "1.001"
  x = "1.0010"
  x.trimZeros()
  assert x == "1.001"
  x = "1.1001"
  x.trimZeros()
  assert x == "1.1001"
  x = "1.10010"
  x.trimZeros()
  assert x == "1.1001"
  x = "1.1001000"
  x.trimZeros()
  assert x == "1.1001"
  x = "1e2"
  x.trimZeros()
  assert x == "1e2"
  x = "1.0e2"
  x.trimZeros()
  assert x == "1e2"
  x = "-1.0e2"
  x.trimZeros()
  assert x == "-1e2"
  x = "1.00e2"
  x.trimZeros()
  assert x == "1e2"
  x = "1.000e2"
  x.trimZeros()
  assert x == "1e2"
  x = "9.9e2"
  x.trimZeros()
  assert x == "9.9e2"
  x = "9.90e2"
  x.trimZeros()
  assert x == "9.9e2"
  x = "9.900e2"
  x.trimZeros()
  assert x == "9.9e2"
  x = "9.09e2"
  x.trimZeros()
  assert x == "9.09e2"
  x = "9.090e2"
  x.trimZeros()
  assert x == "9.09e2"
  x = "1e20"
  x.trimZeros()
  assert x == "1e20"
  x = "1.0e20"
  x.trimZeros()
  assert x == "1e20"
  x = "1.00e200"
  x.trimZeros()
  assert x == "1e200"
  x = "1e-2"
  x.trimZeros()
  assert x == "1e-2"
  x = "1.00e-20"
  x.trimZeros()
  assert x == "1e-20"
  x = "1.01e-20"
  x.trimZeros()
  assert x == "1.01e-20"
  x = "1.010e-20"
  x.trimZeros()
  assert x == "1.01e-20"

  x = "1,0"
  x.trimZeros(',')
  assert x == "1"
