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

block: # split iterator
  proc testSplit(s: string; seps: set[char] = Whitespace; maxsplit: int = -1): seq[string] =
    for s in split(s, seps, maxsplit):
      result.add s

  assert testSplit("") == @[""]
  assert testSplit("", default(set[char])) == @[""]
  assert testSplit("", default(set[char]), 0) == @[""]
  assert testSplit("", {' '}) == @[""]
  assert testSplit("", {' '}, 0) == @[""]
  assert testSplit("", {' '}, 1) == @[""]
  assert testSplit("a") == @["a"]
  assert testSplit("a", default(set[char])) == @["a"]
  assert testSplit("a", default(set[char]), 0) == @["a"]
  assert testSplit("a", {' '}) == @["a"]
  assert testSplit("a", {' '}, 0) == @["a"]
  assert testSplit("a", {' '}, 1) == @["a"]
  assert testSplit("a", {'a'}) == @["", ""]
  assert testSplit("a", {'a'}, 0) == @["a"]
  assert testSplit("a", {'a'}, 1) == @["", ""]
  assert testSplit("aa") == @["aa"]
  assert testSplit("aa", default(set[char])) == @["aa"]
  assert testSplit("aa", default(set[char]), 0) == @["aa"]
  assert testSplit("aa", {' '}) == @["aa"]
  assert testSplit("aa", {' '}, 0) == @["aa"]
  assert testSplit("aa", {' '}, 1) == @["aa"]
  assert testSplit("aa", {'a'}) == @["", "", ""]
  assert testSplit("aa", {'a'}, 0) == @["aa"]
  assert testSplit("aa", {'a'}, 1) == @["", "a"]
  assert testSplit("aa", {'a'}, 2) == @["", "", ""]
  assert testSplit("ab") == @["ab"]
  assert testSplit("ab", default(set[char])) == @["ab"]
  assert testSplit("ab", default(set[char]), 0) == @["ab"]
  assert testSplit("ab", {' '}) == @["ab"]
  assert testSplit("ab", {' '}, 0) == @["ab"]
  assert testSplit("ab", {' '}, 1) == @["ab"]
  assert testSplit("ab", {'a'}) == @["", "b"]
  assert testSplit("ab", {'a'}, 0) == @["ab"]
  assert testSplit("ab", {'a'}, 1) == @["", "b"]
  assert testSplit("ab", {'a'}, 2) == @["", "b"]
  assert testSplit("ab", {'a', 'b'}) == @["", "", ""]
  assert testSplit("ab", {'a', 'b'}, 0) == @["ab"]
  assert testSplit("ab", {'a', 'b'}, 1) == @["", "b"]
  assert testSplit("ab", {'a', 'b'}, 2) == @["", "", ""]
  assert testSplit("a b\tc\vd\re\lf\fg") == @["a", "b", "c", "d", "e", "f", "g"]

  let s = " foo bar  baz  "
  assert testSplit(s) == @["", "foo", "bar", "", "baz", "", ""]
  assert testSplit(s, Whitespace, 0) == @[s]
  assert testSplit(s, Whitespace, 1) == @["", "foo bar  baz  "]
  assert testSplit(s, Whitespace, 2) == @["", "foo", "bar  baz  "]
  assert testSplit(s, Whitespace, 3) == @["", "foo", "bar", " baz  "]
  assert testSplit(s, Whitespace, 4) == @["", "foo", "bar", "", "baz  "]

  let s1 = "foo1bar23baz456"
  assert testSplit(s1, Digits) == @["foo", "bar", "", "baz", "", "", ""]
  assert testSplit(s1, Digits, 0) == @[s1]
  assert testSplit(s1, Digits, 1) == @["foo", "bar23baz456"]
  assert testSplit(s1, Digits, 2) == @["foo", "bar", "3baz456"]
  assert testSplit(s1, Digits, 3) == @["foo", "bar", "", "baz456"]
  assert testSplit(s1, Digits, 4) == @["foo", "bar", "", "baz", "56"]

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

block: # normalize
  assert normalize("") == ""
  assert normalize("a") == "a"
  assert normalize("1") == "1"
  assert normalize("ab") == "ab"
  assert normalize("A") == "a"
  assert normalize("AB") == "ab"
  assert normalize("a_") == "a"
  assert normalize("_a") == "a"
  assert normalize("_a_") == "a"
  assert normalize("ab_") == "ab"
  assert normalize("a_b") == "ab"
  assert normalize("_ab") == "ab"
  assert normalize("fOO_b_A_R") == "foobar"
  assert normalize("fOO_b__A___R0123456789") == "foobar0123456789"

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

block: # replace
  assert replace("", 'a', 'x') == ""
  assert replace("1", 'a', 'x') == "1"
  assert replace("a", 'a', 'x') == "x"
  assert replace("abac", 'a', 'x') == "xbxc"

  assert replace("", "", "") == ""
  assert replace("", "", "x") == ""
  assert replace("", "", "xy") == ""
  assert replace("", "a", "") == ""
  assert replace("", "a", "x") == ""
  assert replace("", "a", "xy") == ""
  assert replace("", "ab", "") == ""
  assert replace("", "ab", "x") == ""
  assert replace("", "ab", "xy") == ""
  assert replace("a", "", "") == "a"
  assert replace("a", "", "x") == "a"
  assert replace("a", "", "xy") == "a"
  assert replace("a", "a", "") == ""
  assert replace("a", "a", "x") == "x"
  assert replace("a", "a", "xy") == "xy"
  assert replace("a", "ab", "") == "a"
  assert replace("a", "ab", "x") == "a"
  assert replace("a", "ab", "xy") == "a"
  assert replace("ab", "", "") == "ab"
  assert replace("ab", "", "x") == "ab"
  assert replace("ab", "", "xy") == "ab"
  assert replace("ab", "a", "") == "b"
  assert replace("ab", "a", "x") == "xb"
  assert replace("ab", "a", "xy") == "xyb"
  assert replace("ab", "ab", "") == ""
  assert replace("ab", "ab", "x") == "x"
  assert replace("ab", "ab", "xy") == "xy"
  assert replace("abab", "", "") == "abab"
  assert replace("abab", "", "x") == "abab"
  assert replace("abab", "", "xy") == "abab"
  assert replace("abab", "a", "") == "bb"
  assert replace("abab", "a", "x") == "xbxb"
  assert replace("abab", "a", "xy") == "xybxyb"
  assert replace("abab", "ab", "") == ""
  assert replace("abab", "ab", "x") == "xx"
  assert replace("abab", "ab", "xy") == "xyxy"
  assert replace("1ab2ab3", "", "") == "1ab2ab3"
  assert replace("1ab2ab3", "", "x") == "1ab2ab3"
  assert replace("1ab2ab3", "", "xy") == "1ab2ab3"
  assert replace("1ab2ab3", "a", "") == "1b2b3"
  assert replace("1ab2ab3", "a", "x") == "1xb2xb3"
  assert replace("1ab2ab3", "a", "xy") == "1xyb2xyb3"
  assert replace("1ab2ab3", "ab", "") == "123"
  assert replace("1ab2ab3", "ab", "x") == "1x2x3"
  assert replace("1ab2ab3", "ab", "xy") == "1xy2xy3"

block: # replaceWord
  assert replaceWord("", "", "") == ""
  assert replaceWord("", "", "x") == ""
  assert replaceWord("", "", "xy") == ""
  assert replaceWord("", "a", "") == ""
  assert replaceWord("", "a", "x") == ""
  assert replaceWord("", "a", "xy") == ""
  assert replaceWord("", "ab", "") == ""
  assert replaceWord("", "ab", "x") == ""
  assert replaceWord("", "ab", "xy") == ""
  assert replaceWord("a", "", "") == "a"
  assert replaceWord("a", "", "x") == "a"
  assert replaceWord("a", "", "xy") == "a"
  assert replaceWord("a", "a", "") == ""
  assert replaceWord("a", "a", "x") == "x"
  assert replaceWord("a", "a", "xy") == "xy"
  assert replaceWord("a", "ab", "") == "a"
  assert replaceWord("a", "ab", "x") == "a"
  assert replaceWord("a", "ab", "xy") == "a"
  assert replaceWord("ab", "", "") == "ab"
  assert replaceWord("ab", "", "x") == "ab"
  assert replaceWord("ab", "", "xy") == "ab"
  assert replaceWord("ab", "a", "") == "ab"
  assert replaceWord("ab", "a", "x") == "ab"
  assert replaceWord("ab", "a", "xy") == "ab"
  assert replaceWord("ab", "ab", "") == ""
  assert replaceWord("ab", "ab", "x") == "x"
  assert replaceWord("ab", "ab", "xy") == "xy"
  assert replaceWord("abab", "", "") == "abab"
  assert replaceWord("abab", "a", "") == "abab"
  assert replaceWord("abab", "ab", "") == "abab"
  assert replaceWord("abab", "ba", "") == "abab"
  assert replaceWord("abab", "abab", "") == ""
  assert replaceWord("abab", "abab", "x") == "x"
  assert replaceWord("ab, ab", "ab", "x") == "x, x"
  assert replaceWord("ab, ab", "ab", "xyz") == "xyz, xyz"
  assert replaceWord("ab ab!ab\"ab#ab$ab%ab&ab'(ab)", "ab", "x") == "x x!x\"x#x$x%x&x'(x)"
  assert "-ld a-ldz -ld".replaceWord("-ld") == " a-ldz "
  assert "-lda-ldz -ld abc".replaceWord("-ld") == "-lda-ldz  abc"
  assert "-lda-ldz -ld abc".replaceWord("") == "-lda-ldz -ld abc"

block: # multiReplace
  assert multiReplace("", [("", "")]) == ""
  assert multiReplace("", [("", "x")]) == ""
  assert multiReplace("", [("", "xy")]) == ""
  assert multiReplace("", [("a", "")]) == ""
  assert multiReplace("", [("a", "x")]) == ""
  assert multiReplace("", [("a", "xy")]) == ""
  assert multiReplace("", [("ab", "")]) == ""
  assert multiReplace("", [("ab", "x")]) == ""
  assert multiReplace("", [("ab", "xy")]) == ""
  assert multiReplace("a", [("", "")]) == "a"
  assert multiReplace("a", [("", "x")]) == "a"
  assert multiReplace("a", [("", "xy")]) == "a"
  assert multiReplace("a", [("a", "")]) == ""
  assert multiReplace("a", [("a", "x")]) == "x"
  assert multiReplace("a", [("a", "xy")]) == "xy"
  assert multiReplace("a", [("ab", "")]) == "a"
  assert multiReplace("a", [("ab", "x")]) == "a"
  assert multiReplace("a", [("ab", "xy")]) == "a"
  assert multiReplace("ab", [("", "")]) == "ab"
  assert multiReplace("ab", [("", "x")]) == "ab"
  assert multiReplace("ab", [("", "xy")]) == "ab"
  assert multiReplace("ab", [("a", "")]) == "b"
  assert multiReplace("ab", [("a", "x")]) == "xb"
  assert multiReplace("ab", [("a", "xy")]) == "xyb"
  assert multiReplace("ab", [("ab", "")]) == ""
  assert multiReplace("ab", [("ab", "x")]) == "x"
  assert multiReplace("ab", [("ab", "xy")]) == "xy"
  assert multiReplace("abab", [("", "")]) == "abab"
  assert multiReplace("abab", [("a", "")]) == "bb"
  assert multiReplace("abab", [("ab", "")]) == ""
  assert multiReplace("abab", [("ba", "")]) == "ab"
  assert multiReplace("abab", [("abab", "")]) == ""
  assert "abba".multiReplace([("a", "b"), ("b", "a")]) == "baab"
  assert "Hello World.".multiReplace([("ello", "ELLO"), ("World.",
      "PEOPLE!")]) == "HELLO PEOPLE!"
  assert "aaaa".multiReplace([("a", "aa"), ("aa", "bb")]) == "aaaaaaaa"
  assert multiReplace("foobarbaz", [("foo", "bar"), ("bar", "baz"), ("baz", "foo")]) == "barbazfoo"

block: # multiReplace characters
  assert multiReplace("", [(default(set[char]), 'x')]) == ""
  assert multiReplace("", [({'a'}, 'x')]) == ""
  assert multiReplace("", [({'a', 'b'}, 'x')]) == ""
  assert multiReplace("", [({'a'}, 'x'), ({'b'}, 'y')]) == ""
  assert multiReplace("1", [(default(set[char]), 'x')]) == "1"
  assert multiReplace("1", [({'a'}, 'x')]) == "1"
  assert multiReplace("1", [({'a', 'b'}, 'x')]) == "1"
  assert multiReplace("1", [({'a'}, 'x'), ({'b'}, 'y')]) == "1"
  assert multiReplace("a", [(default(set[char]), 'x')]) == "a"
  assert multiReplace("a", [({'a'}, 'x')]) == "x"
  assert multiReplace("a", [({'a', 'b'}, 'x')]) == "x"
  assert multiReplace("a", [({'a'}, 'x'), ({'b'}, 'y')]) == "x"
  assert multiReplace("1ab", [(default(set[char]), 'x')]) == "1ab"
  assert multiReplace("1ab", [({'a'}, 'x')]) == "1xb"
  assert multiReplace("1ab", [({'a', 'b'}, 'x')]) == "1xx"
  assert multiReplace("1ab", [({'a'}, 'x'), ({'b'}, 'y')]) == "1xy"
  assert multiReplace("12ab", [({'a', 'b'}, 'x'), ({'1', '2'}, 'y')]) == "yyxx"
  # https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file#naming-conventions
  const SanitationRules = [
      ({'\0'..'\31'}, ' '),
      ({'"'}, '\''),
      ({'/', '\\', ':', '|'}, '-'),
      ({'*', '?', '<', '>'}, '_'),
    ]
  # Basic character set replacements
  assert multiReplace("abba", SanitationRules) == "abba"
  assert multiReplace("a/b\\c:d", SanitationRules) == "a-b-c-d"
  assert multiReplace("a*b?c", SanitationRules) == "a_b_c"
  assert multiReplace("\0\3test", SanitationRules) == "  test"
  assert multiReplace("testquote\"", SanitationRules) == "testquote'"
  assert multiReplace("", SanitationRules) == ""
  assert multiReplace("/\\:*?\"\0<>", [({'\0'..'\255'}, '.')]) == "........."

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

block: # formatSize
  assert formatSize(1024'i64 * 1024 * 1024 * 2 - 1) == "1.999GiB"
  assert formatSize(1024'i64 * 1024 * 1024 * 2) == "2GiB"
  assert formatSize((1'i64 shl 31) + (300'i64 shl 20)) == "2.293GiB" # <=== bug #8231
  assert formatSize(int64.high) == "7.999EiB"
  assert formatSize(int64.high div 2 + 1) == "4EiB"
  assert formatSize(int64.high div 2) == "3.999EiB"
  assert formatSize(int64.high div 4 + 1) == "2EiB"
  assert formatSize(int64.high div 4) == "1.999EiB"
  assert formatSize(int64.high div 8 + 1) == "1EiB"
  assert formatSize(int64.high div 8) == "1023.999PiB"
  assert formatSize(int64.high div 16 + 1) == "512PiB"
  assert formatSize(int64.high div 16) == "511.999PiB"
  assert formatSize(0) == "0B"
  assert formatSize(0, includeSpace = true) == "0 B"
  assert formatSize(1) == "1B"
  assert formatSize(2) == "2B"
  assert formatSize(1022) == "1022B"
  assert formatSize(1023) == "1023B"
  assert formatSize(1024) == "1KiB"
  assert formatSize(1025) == "1.001KiB"
  assert formatSize(1026) == "1.002KiB"
  assert formatSize(1024 * 2 - 2) == "1.998KiB"
  assert formatSize(1024 * 2 - 1) == "1.999KiB"
  assert formatSize(1024 * 2) == "2KiB"
  assert formatSize(1024 * 2 + 1) == "2.001KiB"
  assert formatSize(1024 * 2 + 2) == "2.002KiB"
  assert formatSize(4096 - 1) == "3.999KiB"
  assert formatSize(4096) == "4KiB"
  assert formatSize(4096 + 1) == "4.001KiB"
  assert formatSize(1024 * 512 - 1) == "511.999KiB"
  assert formatSize(1024 * 512) == "512KiB"
  assert formatSize(1024 * 512 + 1) == "512.001KiB"
  assert formatSize(1024 * 1024 - 2) == "1023.998KiB"
  assert formatSize(1024 * 1024 - 1) == "1023.999KiB"
  assert formatSize(1024 * 1024) == "1MiB"
  assert formatSize(1024 * 1024 + 1) == "1MiB"
  assert formatSize(1024 * 1024 + 1023) == "1MiB"
  assert formatSize(1024 * 1024 + 1024) == "1.001MiB"
  assert formatSize(1024 * 1024 + 1024 * 2) == "1.002MiB"
  assert formatSize(1024 * 1024 * 2 - 1) == "1.999MiB"
  assert formatSize(1024 * 1024 * 2) == "2MiB"
  assert formatSize(1024 * 1024 * 2 + 1) == "2MiB"
  assert formatSize(1024 * 1024 * 2 + 1024) == "2.001MiB"
  assert formatSize(1024 * 1024 * 2 + 1024 * 2) == "2.002MiB"
  assert formatSize(1024 * 1024 * 4 - 1) == "3.999MiB"
  assert formatSize(1024 * 1024 * 4) == "4MiB"
  assert formatSize(1024 * (1024 * 4 + 1)) == "4.001MiB"
  assert formatSize(1024 * 1024 * 512 - 1025) == "511.998MiB"
  assert formatSize(1024 * 1024 * 512 - 1) == "511.999MiB"
  assert formatSize(1024 * 1024 * 512) == "512MiB"
  assert formatSize(1024 * 1024 * 512 + 1) == "512MiB"
  assert formatSize(1024 * 1024 * 512 + 1024) == "512.001MiB"
  assert formatSize(1024 * 1024 * 512 + 1024 * 2) == "512.002MiB"
  assert formatSize(1024 * 1024 * 1024 - 1) == "1023.999MiB"
  assert formatSize(1024 * 1024 * 1024) == "1GiB"
  assert formatSize(1024 * 1024 * 1024 + 1) == "1GiB"
  assert formatSize(1024 * 1024 * 1025) == "1.001GiB"
  assert formatSize(1024 * 1024 * 1026) == "1.002GiB"
  # != 2.234MiB as (2.234 * 1024 * 1024).int.float / (1024 * 1024) = 2.23399...
  # and formatSize round down the value
  assert formatSize((2.234*1024*1024).int) == "2.233MiB"
  assert formatSize(4096, prefix = bpColloquial, includeSpace = true) == "4 kB"
  assert formatSize(4096, includeSpace = true) == "4 KiB"
  # (5378934).float / (1024 * 1024) = 5.12975...
  assert formatSize(5_378_934, prefix = bpColloquial, decimalSep = ',') == "5,129MB"
