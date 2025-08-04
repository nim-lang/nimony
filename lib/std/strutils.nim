const
  Whitespace* = {' ', '\t', '\v', '\r', '\l', '\f'}
    ## All the characters that count as whitespace (space, tab, vertical tab,
    ## carriage return, new line, form feed).

const
  Whitespace* = {' ', '\t', '\v', '\r', '\l', '\f'}
    ## All the characters that count as whitespace (space, tab, vertical tab,
    ## carriage return, new line, form feed).

  Letters* = {'A'..'Z', 'a'..'z'}
    ## The set of letters.

  UppercaseLetters* = {'A'..'Z'}
    ## The set of uppercase ASCII letters.

  LowercaseLetters* = {'a'..'z'}
    ## The set of lowercase ASCII letters.

  PunctuationChars* = {'!'..'/', ':'..'@', '['..'`', '{'..'~'}
    ## The set of all ASCII punctuation characters.

  Digits* = {'0'..'9'}
    ## The set of digits.

  HexDigits* = {'0'..'9', 'A'..'F', 'a'..'f'}
    ## The set of hexadecimal digits.

  IdentChars* = {'a'..'z', 'A'..'Z', '0'..'9', '_'}
    ## The set of characters an identifier can consist of.

  IdentStartChars* = {'a'..'z', 'A'..'Z', '_'}
    ## The set of characters an identifier can start with.

  Newlines* = {'\13', '\10'}
    ## The set of characters a newline terminator can start with (carriage
    ## return, line feed).

  PrintableChars* = Letters + Digits + PunctuationChars + Whitespace
    ## The set of all printable ASCII characters (letters, digits, whitespace, and punctuation characters).

  AllChars* = {'\x00'..'\xFF'}
    ## A set with all the possible characters.
    ##
    ## Not very useful by its own, you can use it to create *inverted* sets to
    ## make the `find func<#find,string,set[char],Natural,int>`_
    ## find **invalid** characters in strings. Example:
    ##   ```nim
    ##   let invalid = AllChars - Digits
    ##   doAssert "01234".find(invalid) == -1
    ##   doAssert "01A34".find(invalid) == 2
    ##   ```

func isAlphaAscii*(c: char): bool {.inline.} =
  ## Checks whether or not character `c` is alphabetical.
  ##
  ## This checks a-z, A-Z ASCII characters only.
  ## Use `Unicode module<unicode.html>`_ for UTF-8 support.
  runnableExamples:
    assert isAlphaAscii('e') == true
    assert isAlphaAscii('E') == true
    assert isAlphaAscii('8') == false
  c in Letters

func isAlphaNumeric*(c: char): bool {.inline.} =
  ## Checks whether or not `c` is alphanumeric.
  ##
  ## This checks a-z, A-Z, 0-9 ASCII characters only.
  runnableExamples:
    assert isAlphaNumeric('n') == true
    assert isAlphaNumeric('8') == true
    assert isAlphaNumeric(' ') == false
  c in Letters+Digits

func isDigit*(c: char): bool {.inline.} =
  ## Checks whether or not `c` is a number.
  ##
  ## This checks 0-9 ASCII characters only.
  runnableExamples:
    assert isDigit('n') == false
    assert isDigit('8') == true
  c in Digits

func isSpaceAscii*(c: char): bool {.inline.} =
  ## Checks whether or not `c` is a whitespace character.
  runnableExamples:
    assert isSpaceAscii('n') == false
    assert isSpaceAscii(' ') == true
    assert isSpaceAscii('\t') == true
  c in Whitespace

func isLowerAscii*(c: char): bool {.inline.} =
  ## Checks whether or not `c` is a lower case character.
  ##
  ## This checks ASCII characters only.
  ## Use `Unicode module<unicode.html>`_ for UTF-8 support.
  ##
  ## See also:
  ## * `toLowerAscii func<#toLowerAscii,char>`_
  runnableExamples:
    assert isLowerAscii('e') == true
    assert isLowerAscii('E') == false
    assert isLowerAscii('7') == false
  c in LowercaseLetters

func isUpperAscii*(c: char): bool {.inline.} =
  ## Checks whether or not `c` is an upper case character.
  ##
  ## This checks ASCII characters only.
  ## Use `Unicode module<unicode.html>`_ for UTF-8 support.
  ##
  ## See also:
  ## * `toUpperAscii func<#toUpperAscii,char>`_
  runnableExamples:
    assert isUpperAscii('e') == false
    assert isUpperAscii('E') == true
    assert isUpperAscii('7') == false
  c in UppercaseLetters

func allCharsInSet*(s: string; theSet: set[char]): bool =
  ## Returns true if every character of `s` is in the set `theSet`.
  runnableExamples:
    assert allCharsInSet("aeea", {'a', 'e'}) == true
    assert allCharsInSet("", {'a', 'e'}) == true

  for c in items(s):
    if c notin theSet: return false
  return true

func isEmptyOrWhitespace*(s: string): bool {.inline.} =
  ## Checks if `s` is empty or consists entirely of whitespace characters.
  result = s.allCharsInSet(Whitespace)

proc endsWith*(s: string; c: char): bool {.inline.} =
  if s.len > 0: s[s.len-1] == c else: false

proc strlen*(x: cstring): int {.importc: "strlen", header: "<string.h>".}

proc `$`*(x: cstring): string =
  let L = int strlen(x)
  result = newString(L)
  for i in 0..<result.len:
    result[i] = x[i]

proc `$`*(x: char): string =
  result = newString(1)
  result[0] = x

func continuesWith*(s, prefix: string; start: int): bool =
  if prefix.len > s.len-start:
    return false
  for i in 0 ..< prefix.len:
    if s[i+start] != prefix[i]:
      return false
  return true

func startsWith*(s, prefix: string): bool =
  continuesWith s, prefix, 0

proc toLowerAscii*(c: char): char {.inline.} =
  ## Returns the lower case version of character `c`.
  ##
  ## This works only for the letters `A-Z`. See `unicode.toLower
  ## <unicode.html#toLower,Rune>`_ for a version that works for any Unicode
  ## character.
  ##
  ## See also:
  ## * `isLowerAscii func<#isLowerAscii,char>`_
  ## * `toLowerAscii func<#toLowerAscii,string>`_ for converting a string
  runnableExamples:
    assert toLowerAscii('A') == 'a'
    assert toLowerAscii('e') == 'e'
  if c >= 'A' and c <= 'Z': char(int(c) - int('A') + int('a'))
  else: c

func toLowerAscii*(s: string): string =
  ## Converts string `s` into lower case.
  ##
  ## This works only for the letters `A-Z`. See `unicode.toLower
  ## <unicode.html#toLower,string>`_ for a version that works for any Unicode
  ## character.
  ##
  ## See also:
  ## * `normalize func<#normalize,string>`_
  runnableExamples:
    assert toLowerAscii("FooBar!") == "foobar!"
  result = newString(s.len)
  for i in 0 ..< s.len:
    result[i] = toLowerAscii(s[i])

func toUpperAscii*(c: char): char {.inline.} =
  ## Converts character `c` into upper case.
  ##
  ## This works only for the letters `A-Z`.  See `unicode.toUpper
  ## <unicode.html#toUpper,Rune>`_ for a version that works for any Unicode
  ## character.
  ##
  ## See also:
  ## * `isUpperAscii func<#isUpperAscii,char>`_
  ## * `toUpperAscii func<#toUpperAscii,string>`_ for converting a string
  ## * `capitalizeAscii func<#capitalizeAscii,string>`_
  runnableExamples:
    assert toUpperAscii('a') == 'A'
    assert toUpperAscii('E') == 'E'
  if c >= 'a' and c <= 'z': char(int(c) - int('a') + int('A'))
  else: c

func toUpperAscii*(s: string): string =
  ## Converts string `s` into upper case.
  ##
  ## This works only for the letters `A-Z`.  See `unicode.toUpper
  ## <unicode.html#toUpper,string>`_ for a version that works for any Unicode
  ## character.
  ##
  ## See also:
  ## * `capitalizeAscii func<#capitalizeAscii,string>`_
  runnableExamples:
    assert toUpperAscii("FooBar!") == "FOOBAR!"
  result = newString(s.len)
  for i in 0 ..< s.len:
    result[i] = toUpperAscii(s[i])

func capitalizeAscii*(s: string): string {.inline.} =
  ## Converts the first character of string `s` into upper case.
  ##
  ## This works only for the letters `A-Z`.
  ## Use `Unicode module<unicode.html>`_ for UTF-8 support.
  ##
  ## See also:
  ## * `toUpperAscii func<#toUpperAscii,char>`_
  runnableExamples:
    assert capitalizeAscii("foo") == "Foo"
    assert capitalizeAscii("-bar") == "-bar"
  if s.len == 0: result = ""
  else:
    result = s
    result[0] = toUpperAscii(result[0])

func cmpIgnoreCase*(a, b: string): int =
  ## Compares two strings in a case insensitive manner. Returns:
  ##
  ## | `0` if a == b
  ## | `< 0` if a < b
  ## | `> 0` if a > b
  runnableExamples:
    assert cmpIgnoreCase("FooBar", "foobar") == 0
    assert cmpIgnoreCase("bar", "Foo") < 0
    assert cmpIgnoreCase("Foo5", "foo4") > 0

  let aLen = a.len
  let bLen = b.len
  let minLen = min(aLen, bLen)
  for i in 0 ..< minLen:
    result = a[i].toLowerAscii.ord - b[i].toLowerAscii.ord
    if result != 0: return
  result = aLen - bLen

func cmpIgnoreStyle*(a, b: string): int =
  ## Semantically the same as `cmp(normalize(a), normalize(b))`. It
  ## is just optimized to not allocate temporary strings. This should
  ## NOT be used to compare Nim identifier names.
  ## Use `macros.eqIdent<macros.html#eqIdent,string,string>`_ for that.
  ##
  ## Returns:
  ##
  ## | `0` if a == b
  ## | `< 0` if a < b
  ## | `> 0` if a > b
  runnableExamples:
    assert cmpIgnoreStyle("foo_bar", "FooBar") == 0
    assert cmpIgnoreStyle("foo_bar_5", "FooBar4") > 0
  let aLen = a.len
  let bLen = b.len
  var i = 0
  var j = 0
  while true:
    while i < aLen and a[i] == '_': inc i
    while j < bLen and b[j] == '_': inc j
    if i == aLen:
      if j == bLen:
        # both cursors at the end:
        return 0
      else:
        # not yet at the end of 'b':
        return -1
    elif j == bLen:
      return 1
    let aa = toLowerAscii(a[i])
    let bb = toLowerAscii(b[j])
    result = ord(aa) - ord(bb)
    if result != 0: return result
    # the characters are identical:
    inc i
    inc j

func replace*(s: string; sub, by: char): string =
  result = newString(s.len)
  var i = 0
  while i < s.len:
    if s[i] == sub: result[i] = by
    else: result[i] = s[i]
    inc i
