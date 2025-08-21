import std/parseutils

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
  ## Returns true if `s` continues with `prefix` at position `start`.
  ##
  ## If `prefix == ""` true is returned.
  ##
  ## See also:
  ## * `startsWith func<#startsWith,string,string>`_
  ## * `endsWith func<#endsWith,string,string>`_
  if prefix.len == 0:
    return true
  if prefix.len > s.len-start:
    return false
  for i in 0 ..< prefix.len:
    if s[i+start] != prefix[i]:
      return false
  return true

func startsWith*(s, prefix: string): bool =
  ## Returns true if `s` starts with string `prefix`.
  ##
  ## If `prefix == ""` true is returned.
  ##
  ## See also:
  ## * `endsWith func<#endsWith,string,string>`_
  ## * `continuesWith func<#continuesWith,string,string,Natural>`_
  ## * `removePrefix func<#removePrefix,string,string>`_
  runnableExamples:
    let a = "abracadabra"
    assert a.startsWith("abra") == true
    assert a.startsWith("bra") == false
  continuesWith s, prefix, 0

func endsWith*(s, suffix: string): bool =
  ## Returns true if `s` ends with `suffix`.
  ##
  ## If `suffix == ""` true is returned.
  ##
  ## See also:
  ## * `startsWith func<#startsWith,string,string>`_
  ## * `continuesWith func<#continuesWith,string,string,Natural>`_
  ## * `removeSuffix func<#removeSuffix,string,string>`_
  runnableExamples:
    let a = "abracadabra"
    assert a.endsWith("abra") == true
    assert a.endsWith("dab") == false
  if suffix.len > s.len:
    return false
  continuesWith s, suffix, s.len - suffix.len

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

func find*(s: string; sub: char; start: Natural = 0; last = -1): int =
  ## Searches for `sub` in `s` inside range `start..last` (both ends included).
  ## If `last` is unspecified or negative, it defaults to `s.high` (the last element).
  ##
  ## Searching is case-sensitive. If `sub` is not in `s`, -1 is returned.
  ## Otherwise the index returned is relative to `s[0]`, not `start`.
  ## Subtract `start` from the result for a `start`-origin index.
  ##
  ## See also:
  ## * `replace func<#replace,string,char,char>`_
  result = -1
  let last = if last < 0: s.high else: last

  for i in int(start)..last:
    if s[i] == sub:
      return i

func replace*(s: string; sub, by: char): string =
  result = newString(s.len)
  var i = 0
  while i < s.len:
    if s[i] == sub: result[i] = by
    else: result[i] = s[i]
    inc i

const HexChars = "0123456789ABCDEF"

func escape*(s: string, prefix = "\"", suffix = "\""): string =
  ## Escapes a string `s`.
  ##
  ## .. note:: The escaping scheme is different from
  ##    `system.addEscapedChar`.
  ##
  ## * replaces `'\0'..'\31'` and `'\127'..'\255'` by `\xHH` where `HH` is its hexadecimal value
  ## * replaces ``\`` by `\\`
  ## * replaces `'` by `\'`
  ## * replaces `"` by `\"`
  ##
  ## The resulting string is prefixed with `prefix` and suffixed with `suffix`.
  ## Both may be empty strings.
  ##
  ## See also:
  ## * `addEscapedChar proc<system.html#addEscapedChar,string,char>`_
  ## * `unescape func<#unescape,string,string,string>`_ for the opposite
  ##   operation
  result = newStringOfCap(s.len + s.len shr 2)
  result.add(prefix)
  for c in items(s):
    case c
    of '\0'..'\31', '\127'..'\255':
      add(result, "\\x")
      let n = ord(c)
      add(result, HexChars[int((n and 0xF0) shr 4)])
      add(result, HexChars[int(n and 0xF)])
    of '\\': add(result, "\\\\")
    of '\'': add(result, "\\'")
    of '\"': add(result, "\\\"")
    else: add(result, c)
  add(result, suffix)

func unescape*(s: string, prefix = "\"", suffix = "\""): string {.raises.} =
  ## Unescapes a string `s`.
  ##
  ## This complements `escape func<#escape,string,string,string>`_
  ## as it performs the opposite operations.
  ##
  ## If `s` does not begin with `prefix` and end with `suffix` a
  ## ValueError exception will be raised.
  result = newStringOfCap(s.len)
  var i = prefix.len
  if not s.startsWith(prefix):
    raise ValueError
  while true:
    if i >= s.len-suffix.len: break
    if s[i] == '\\':
      if i+1 >= s.len:
        result.add('\\')
        break
      case s[i+1]:
      of 'x':
        inc i, 2
        var c = 0
        i += parseutils.parseHex(s, c, i, maxLen = 2)
        result.add(chr(c))
        dec i, 2
      of '\\':
        result.add('\\')
      of '\'':
        result.add('\'')
      of '\"':
        result.add('\"')
      else:
        result.add('\\')
        result.add(s[i+1])
      inc(i, 2)
    else:
      result.add(s[i])
      inc(i)
  if not s.endsWith(suffix):
    raise ValueError

func c_snprintf(buf: cstring, n: csize_t, frmt: cstring): cint {.
  header: "<stdio.h>", importc: "snprintf", varargs.}

type
  FloatFormatMode* = enum
    ## The different modes of floating point formatting.
    ffDefault,   ## use the shorter floating point notation
    ffDecimal,   ## use decimal floating point notation
    ffScientific ## use scientific notation (using `e` character)

func formatBiggestFloat*(f: BiggestFloat, format: FloatFormatMode = ffDefault,
                         precision: range[-1..32] = 16;
                         decimalSep = '.'): string =
  ## Converts a floating point value `f` to a string.
  ##
  ## If `format == ffDecimal` then precision is the number of digits to
  ## be printed after the decimal point.
  ## If `format == ffScientific` then precision is the maximum number
  ## of significant digits to be printed.
  ## `precision`'s default value is the maximum number of meaningful digits
  ## after the decimal point for Nim's `biggestFloat` type.
  ##
  ## If `precision == -1`, it tries to format it nicely.
  runnableExamples:
    let x = 123.456
    assert x.formatBiggestFloat() == "123.4560000000000"
    assert x.formatBiggestFloat(ffDecimal, 4) == "123.4560"
    assert x.formatBiggestFloat(ffScientific, 2) == "1.23e+02"
  const floatFormatToChar: array[FloatFormatMode, char] = ['g', 'f', 'e']
  var
    frmtstr {.noinit.}: array[0..5, char]
    buf {.noinit.}: array[0..2500, char]
    L: cint
  frmtstr[0] = '%'
  if precision.int >= 0:
    frmtstr[1] = '#'
    frmtstr[2] = '.'
    frmtstr[3] = '*'
    frmtstr[4] = floatFormatToChar[format]
    frmtstr[5] = '\0'
    L = c_snprintf(cast[cstring](addr buf), csize_t(2501), cast[cstring](addr frmtstr), precision, f)
  else:
    frmtstr[1] = floatFormatToChar[format]
    frmtstr[2] = '\0'
    L = c_snprintf(cast[cstring](addr buf), csize_t(2501), cast[cstring](addr frmtstr), f)
  result = newString(L)
  for i in 0 ..< L:
    # Depending on the locale either dot or comma is produced,
    # but nothing else is possible:
    if buf[i] in {'.', ','}: result[i] = decimalSep
    else: result[i] = buf[i]

func formatFloat*(f: float, format: FloatFormatMode = ffDefault,
                  precision: range[-1..32] = 16; decimalSep = '.'): string =
  ## Converts a floating point value `f` to a string.
  ##
  ## If `format == ffDecimal` then precision is the number of digits to
  ## be printed after the decimal point.
  ## If `format == ffScientific` then precision is the maximum number
  ## of significant digits to be printed.
  ## `precision`'s default value is the maximum number of meaningful digits
  ## after the decimal point for Nim's `float` type.
  ##
  ## If `precision == -1`, it tries to format it nicely.
  runnableExamples:
    let x = 123.456
    assert x.formatFloat() == "123.4560000000000"
    assert x.formatFloat(ffDecimal, 4) == "123.4560"
    assert x.formatFloat(ffScientific, 2) == "1.23e+02"

  result = formatBiggestFloat(f, format, precision.int, decimalSep)
