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
