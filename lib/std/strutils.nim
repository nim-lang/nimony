import std/[assertions, parseutils]

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
  if x == nil:
    result = ""
  else:
    let L = int strlen(x)
    result = newString(L)
    for i in 0..<result.len:
      result[i] = x[i]

proc `$`*(x: char): string =
  result = newString(1)
  result[0] = x

template stringHasSep(s: string, index: int, seps: set[char]): bool =
  s[index] in seps

template splitCommon(s, sep, maxsplit, sepLen) {.untyped.} =
  ## Common code for split procs
  var last = 0
  var splits = maxsplit

  while last <= len(s):
    var first = last
    while last < len(s) and not stringHasSep(s, last, sep):
      inc(last)
    if splits == 0: last = len(s)
    yield substr(s, first, last-1)
    if splits == 0: break
    dec(splits)
    inc(last, sepLen)

iterator split*(s: string; seps: set[char] = Whitespace;
                maxsplit: int = -1): string =
  ## Splits the string `s` into substrings using a group of separators.
  ##
  ## Substrings are separated by a substring containing only `seps`.
  ##
  ##   ```nim
  ##   for word in split("this\lis an\texample"):
  ##     writeLine(stdout, word)
  ##   ```
  ##
  ## ...generates this output:
  ##
  ##   ```
  ##   "this"
  ##   "is"
  ##   "an"
  ##   "example"
  ##   ```
  ##
  ## And the following code:
  ##
  ##   ```nim
  ##   for word in split("this:is;an$example", {';', ':', '$'}):
  ##     writeLine(stdout, word)
  ##   ```
  ##
  ## ...produces the same output as the first example. The code:
  ##
  ##   ```nim
  ##   let date = "2012-11-20T22:08:08.398990"
  ##   let separators = {' ', '-', ':', 'T'}
  ##   for number in split(date, separators):
  ##     writeLine(stdout, number)
  ##   ```
  ##
  ## ...results in:
  ##
  ##   ```
  ##   "2012"
  ##   "11"
  ##   "20"
  ##   "22"
  ##   "08"
  ##   "08.398990"
  ##   ```
  ##
  ##  .. note:: Empty separator set results in returning an original string,
  ##   following the interpretation "split by no element".
  splitCommon(s, seps, maxsplit, 1)

func delete*(s: var string, slice: Slice[int]) =
  ## Deletes the items `s[slice]`.
  ##
  ## This operation moves all elements after `s[slice]` in linear time, and
  ## is the string analog to `sequtils.delete`.
  runnableExamples:
    var a = "abcde"
    assert a == "abcde"
    a.delete(4..4)
    assert a == "abcd"
    a.delete(1..2)
    assert a == "ad"
    a.delete(1..0) # empty slice
    assert a == "ad"
  #when compileOption("boundChecks"):
  assert slice.a < s.len and slice.a >= 0 and slice.b < s.len
  if slice.b >= slice.a:
    var i = slice.a
    var j = slice.b + 1
    var newLen = s.len - j + i
    # if j < s.len: moveMem(addr s[i], addr s[j], s.len - j) # pending benchmark
    while i < newLen:
      s[i] = s[j]
      inc(i)
      inc(j)
    shrink(s, newLen)

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

func normalize*(s: string): string =
  ## Normalizes the string `s`.
  ##
  ## That means to convert it to lower case and remove any '_'. This
  ## should NOT be used to normalize Nim identifier names.
  ##
  ## See also:
  ## * `toLowerAscii func<#toLowerAscii,string>`_
  runnableExamples:
    assert normalize("Foo_bar") == "foobar"
    assert normalize("Foo Bar") == "foo bar"
  result = newString(s.len)
  var j = 0
  for i in 0 ..< len(s):
    if s[i] in UppercaseLetters:
      result[j] = chr(ord(s[i]) + (ord('a') - ord('A')))
      inc j
    elif s[i] != '_':
      result[j] = s[i]
      inc j
  if j != s.len: shrink(result, j)

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

func find*(s: string; chars: set[char]; start: Natural = 0; last = -1): int =
  ## Searches for `chars` in `s` inside range `start..last` (both ends included).
  ## If `last` is unspecified or negative, it defaults to `s.high` (the last element).
  ##
  ## If `s` contains none of the characters in `chars`, -1 is returned.
  ## Otherwise the index returned is relative to `s[0]`, not `start`.
  ## Subtract `start` from the result for a `start`-origin index.
  ##
  ## See also:
  ## * `multiReplace func<#multiReplace,string,varargs[]>`_
  result = -1
  let last = if last < 0: s.high else: last
  for i in int(start)..last:
    if s[i] in chars:
      return i

type
  SkipTable* = array[char, int] ## Character table for efficient substring search.

func initSkipTable*(a: var SkipTable; sub: string) =
  ## Initializes table `a` for efficient search of substring `sub`.
  ##
  ## See also:
  ## * `initSkipTable func<#initSkipTable,string>`_
  ## * `find func<#find,SkipTable,string,string,Natural,int>`_
  # TODO: this should be the `default()` initializer for the type.
  let m = len(sub)
  for i in a.low.int .. a.high.int:
    a[i] = m

  for i in 0 ..< m - 1:
    a[sub[i]] = m - 1 - i

func initSkipTable*(sub: string): SkipTable {.noinit.} =
  ## Returns a new table initialized for `sub`.
  ##
  ## See also:
  ## * `initSkipTable func<#initSkipTable,SkipTable,string>`_
  ## * `find func<#find,SkipTable,string,string,Natural,int>`_
  initSkipTable(result, sub)

func find*(a: SkipTable; s, sub: string; start: Natural = 0; last = -1): int =
  ## Searches for `sub` in `s` inside range `start..last` using preprocessed
  ## table `a`. If `last` is unspecified, it defaults to `s.high` (the last
  ## element).
  ##
  ## Searching is case-sensitive. If `sub` is not in `s`, -1 is returned.
  ##
  ## See also:
  ## * `initSkipTable func<#initSkipTable,string>`_
  ## * `initSkipTable func<#initSkipTable,SkipTable,string>`_
  let
    last = if last < 0: s.high else: last
    subLast = sub.len - 1

  if subLast == -1:
    # this was an empty needle string,
    # we count this as match in the first possible position:
    return start

  # This is an implementation of the Boyer-Moore Horspool algorithms
  # https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm
  result = -1
  var skip = start

  while last - skip >= subLast:
    var i = subLast
    while s[skip + i] == sub[i]:
      if i == 0:
        return skip
      dec i
    inc skip, a[s[skip + subLast]]

func find*(s, sub: string; start: Natural = 0; last = -1): int =
  ## Searches for `sub` in `s` inside range `start..last` (both ends included).
  ## If `last` is unspecified or negative, it defaults to `s.high` (the last element).
  ##
  ## Searching is case-sensitive. If `sub` is not in `s`, -1 is returned.
  ## Otherwise the index returned is relative to `s[0]`, not `start`.
  ## Subtract `start` from the result for a `start`-origin index.
  ##
  ## See also:
  ## * `replace func<#replace,string,string,string>`_
  if sub.len > s.len - start: return -1
  if sub.len == 1: return find(s, sub[0], start, last)

  # TODO: use `memmem` C function like Nim 2.
  result = find(initSkipTable(sub), s, sub, start, last)

func replace*(s: string; sub, by: char): string =
  result = newString(s.len)
  var i = 0
  while i < s.len:
    if s[i] == sub: result[i] = by
    else: result[i] = s[i]
    inc i

func replace*(s, sub: string; by = ""): string =
  ## Replaces every occurrence of the string `sub` in `s` with the string `by`.
  ##
  ## See also:
  ## * `find func<#find,string,string,Natural,int>`_
  ## * `replace func<#replace,string,char,char>`_ for replacing
  ##   single characters
  ## * `replaceWord func<#replaceWord,string,string,string>`_
  ## * `multiReplace func<#multiReplace,string,varargs[]>`_ for substrings
  ## * `multiReplace func<#multiReplace,openArray[char],varargs[]>`_ for single characters
  result = ""
  let subLen = sub.len
  if subLen == 0:
    result = s
  elif subLen == 1:
    # when the pattern is a single char, we use a faster
    # char-based search that doesn't need a skip table:
    let c = sub[0]
    let last = s.high
    var i = 0
    while true:
      let j = find(s, c, i, last)
      if j < 0: break
      add result, substr(s, i, j - 1)
      add result, by
      i = j + subLen
    # copy the rest:
    add result, substr(s, i)
  else:
    var a = initSkipTable(sub)
    let last = s.high
    var i = 0
    while true:
      let j = find(a, s, sub, i, last)
      if j < 0: break
      add result, substr(s, i, j - 1)
      add result, by
      i = j + subLen
    # copy the rest:
    add result, substr(s, i)

func replaceWord*(s, sub: string, by = ""): string =
  ## Replaces every occurrence of the string `sub` in `s` with the string `by`.
  ##
  ## Each occurrence of `sub` has to be surrounded by word boundaries
  ## (comparable to `\b` in regular expressions), otherwise it is not
  ## replaced.
  if sub.len == 0: return s
  const wordChars = {'a'..'z', 'A'..'Z', '0'..'9', '_', '\128'..'\255'}
  result = ""
  var a = initSkipTable(sub)
  var i = 0
  let last = s.high
  let sublen = sub.len
  if sublen > 0:
    while true:
      var j = find(a, s, sub, i, last)
      if j < 0: break
      # word boundary?
      if (j == 0 or s[j-1] notin wordChars) and
          (j+sub.len >= s.len or s[j+sub.len] notin wordChars):
        add result, substr(s, i, j - 1)
        add result, by
        i = j + sublen
      else:
        add result, substr(s, i, j)
        i = j + 1
    # copy the rest:
    add result, substr(s, i)

func multiReplace*(s: string; replacements: openArray[(string, string)]): string =
  ## Same as `replace<#replace,string,string,string>`_, but specialized for
  ## doing multiple replacements in a single pass through the input string.
  ##
  ## `multiReplace` scans the input string from left to right and replaces the
  ## matching substrings in the same order as passed in the argument list.
  ##
  ## The implications of the order of scanning the string and matching the
  ## replacements:
  ##   - In case of multiple matches at a given position, the earliest
  ##     replacement is applied.
  ##   - Overlaps are not handled. After performing a replacement, the scan
  ##     continues from the character after the matched substring. If the
  ##     resulting string then contains a possible match starting in a newly
  ##     placed substring, the additional replacement is not performed.
  ##
  ## If the resulting string is not longer than the original input string,
  ## only a single memory allocation is required.
  ##
  runnableExamples:
    # Swapping occurrences of 'a' and 'b':
    assert multireplace("abba", [("a", "b"), ("b", "a")]) == "baab"

    # The second replacement ("ab") is matched and performed first, the scan then
    # continues from 'c', so the "bc" replacement is never matched and thus skipped.
    assert multireplace("abc", [("bc", "x"), ("ab", "_b")]) == "_bc"
  result = newStringOfCap(s.len)
  var i = 0
  var fastChk: set[char] = {}
  # workaround https://github.com/nim-lang/nimony/issues/1461
  # and https://github.com/nim-lang/nimony/issues/1451
  for repl in replacements.items:
    if repl[0].len > 0:
      # Include first character of all replacements
      fastChk.incl repl[0][0]
  while i < s.len:
    block sIteration:
      # Assume most chars in s are not candidates for any replacement operation
      if s[i] in fastChk:
        for repl in replacements.items:
          if repl[0].len > 0 and s.continuesWith(repl[0], i):
            add result, repl[1]
            inc(i, repl[0].len)
            break sIteration
      # No matching replacement found
      # copy current character from s
      add result, s[i]
      inc(i)

func multiReplace*(s: openArray[char]; replacements: openArray[(set[char], char)]): string =
  ## Performs multiple character replacements in a single pass through the input.
  ##
  ## `multiReplace` scans the input `s` from left to right and replaces
  ## characters based on character sets, applying the first matching replacement
  ## at each position. Useful for sanitizing or transforming strings with
  ## predefined character mappings.
  ##
  ## The order of the `replacements` matters:
  ##   - First matching replacement is applied
  ##   - Subsequent replacements are not considered for the same character
  ##
  ## See also:
  ## - `multiReplace(s: string; replacements: varargs[(string, string)]) <#multiReplace,string,varargs[]>`_,
  runnableExamples:
    const WinSanitationRules = [
      ({'\0'..'\31'}, ' '),
      ({'"'}, '\''),
      ({'/', '\\', ':', '|'}, '-'),
      ({'*', '?', '<', '>'}, '_'),
    ]
    # Sanitize a filename with Windows-incompatible characters
    const file = "a/file:with?invalid*chars.txt"
    assert file.multiReplace(WinSanitationRules) == "a-file-with_invalid_chars.txt"
  result = newString(s.len)
  for i in 0..<s.len:
    var nextChar = s[i]
    # Workaround https://github.com/nim-lang/nimony/issues/1451
    for repl in replacements.items:
      if nextChar in repl[0]:
        nextChar = repl[1]
        break
    result[i] = nextChar

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

func findNormalized(x: string, inArray: openArray[string]): int =
  var i = 0
  while i < inArray.len - 1:
    if cmpIgnoreStyle(x, inArray[i]) == 0: return i
    inc(i, 2) # incrementing by 1 would probably lead to a
              # security hole...
  return -1

proc invalidFormatString(formatstr: string) {.noinline, raises.} =
  # TODO: Uncomment when exceptions are implemented.
  #raise newException(SyntaxError, "invalid format string: " & formatstr)
  raise SyntaxError

func `%`*(formatstr: string; a: openArray[string]): string {.raises.} =
  ## Interpolates a format string with the values from `a`.
  ##
  ## The `substitution`:idx: operator performs string substitutions in
  ## `formatstr` and returns a modified `formatstr`. This is often called
  ## `string interpolation`:idx:.
  ##
  ## This is best explained by an example:
  ##
  ##   ```nim
  ##   "$1 eats $2." % ["The cat", "fish"]
  ##   ```
  ##
  ## Results in:
  ##
  ##   ```nim
  ##   "The cat eats fish."
  ##   ```
  ##
  ## The substitution variables (the thing after the `$`) are enumerated
  ## from 1 to `a.len`.
  ## To produce a verbatim `$`, use `$$`.
  ## The notation `$#` can be used to refer to the next substitution
  ## variable:
  ##
  ##   ```nim
  ##   "$# eats $#." % ["The cat", "fish"]
  ##   ```
  ##
  ## Substitution variables can also be words (that is
  ## `[A-Za-z_]+[A-Za-z0-9_]*`) in which case the arguments in `a` with even
  ## indices are keys and with odd indices are the corresponding values.
  ## An example:
  ##
  ##   ```nim
  ##   "$animal eats $food." % ["animal", "The cat", "food", "fish"]
  ##   ```
  ##
  ## Results in:
  ##
  ##   ```nim
  ##   "The cat eats fish."
  ##   ```
  ##
  ## The variables are compared with `cmpIgnoreStyle`. `ValueError` is
  ## raised if an ill-formed format string has been passed to the `%` operator.
  result = newStringOfCap(formatstr.len + a.len shl 4)
  const PatternChars = {'a'..'z', 'A'..'Z', '0'..'9', '\128'..'\255', '_'}
  var i = 0
  var num = 0
  while i < len(formatstr):
    if formatstr[i] == '$' and i+1 < len(formatstr):
      case formatstr[i+1]
      of '#':
        if num >= a.len: invalidFormatString(formatstr)
        add result, a[num]
        inc i, 2
        inc num
      of '$':
        add result, '$'
        inc(i, 2)
      of '1'..'9', '-':
        var j = 0
        inc(i) # skip $
        var negative = formatstr[i] == '-'
        if negative: inc i
        while i < formatstr.len and formatstr[i] in Digits:
          j = j * 10 + ord(formatstr[i]) - ord('0')
          inc(i)
        let idx = if not negative: j-1 else: a.len-j
        if idx < 0 or idx >= a.len: invalidFormatString(formatstr)
        add result, a[idx]
      of '{':
        var j = i+2
        var k = 0
        var negative = formatstr[j] == '-'
        if negative: inc j
        var isNumber = 0
        while j < formatstr.len and formatstr[j] notin {'\0', '}'}:
          if formatstr[j] in Digits:
            k = k * 10 + ord(formatstr[j]) - ord('0')
            if isNumber == 0: isNumber = 1
          else:
            isNumber = -1
          inc(j)
        if isNumber == 1:
          let idx = if not negative: k-1 else: a.len-k
          if idx < 0 or idx >= a.len: invalidFormatString(formatstr)
          add result, a[idx]
        else:
          var x = findNormalized(substr(formatstr, i+2, j-1), a)
          if x >= 0 and x < a.len - 1: add result, a[x+1]
          else: invalidFormatString(formatstr)
        i = j+1
      of 'a'..'z', 'A'..'Z', '\128'..'\255', '_':
        var j = i+1
        while j < formatstr.len and formatstr[j] in PatternChars: inc(j)
        var x = findNormalized(substr(formatstr, i+1, j-1), a)
        if x >= 0 and x < a.len - 1: add result, a[x+1]
        else: invalidFormatString(formatstr)
        i = j
      else:
        invalidFormatString(formatstr)
    else:
      add result, formatstr[i]
      inc(i)

func format*(formatstr: string; a: openArray[string]): string {.raises.} =
  ## This is the same as `formatstr % a` (see
  ## `% func<#%25,string,openArray[string]>`_)
  result = formatstr % a

func strip*(s: string; leading = true; trailing = true;
            chars: set[char] = Whitespace): string =
  ## Strips leading or trailing `chars` (default: whitespace characters)
  ## from `s` and returns the resulting string.
  ##
  ## If `leading` is true (default), leading `chars` are stripped.
  ## If `trailing` is true (default), trailing `chars` are stripped.
  ## If both are false, the string is returned unchanged.
  runnableExamples:
    let a = "  vhellov   "
    let b = strip(a)
    assert b == "vhellov"

    assert a.strip(leading = false) == "  vhellov"
    assert a.strip(trailing = false) == "vhellov   "

    assert b.strip(chars = {'v'}) == "hello"
    assert b.strip(leading = false, chars = {'v'}) == "vhello"

    let c = "blaXbla"
    assert c.strip(chars = {'b', 'a'}) == "laXbl"
    assert c.strip(chars = {'b', 'a', 'l'}) == "X"

  var
    first = 0
    last = len(s)-1
  if leading:
    while first <= last and s[first] in chars: inc(first)
  if trailing:
    while last >= first and s[last] in chars: dec(last)
  result = if first > last: "" else: substr(s, first, last)

func trimZeros*(x: var string; decimalSep = '.') =
  ## Trim trailing zeros from a formatted floating point
  ## value `x` (must be declared as `var`).
  ##
  ## This modifies `x` itself, it does not return a copy.
  runnableExamples:
    var x = "123.456000000"
    x.trimZeros()
    doAssert x == "123.456"

  let sPos = find(x, decimalSep)
  if sPos >= 0:
    var last = find(x, 'e', start = sPos)
    last = if last >= 0: last - 1 else: high(x)
    var pos = last
    while pos >= 0 and x[pos] == '0': dec(pos)
    if pos > sPos: inc(pos)
    if last >= pos:
      try:
        x.delete(pos..last)
      except:
        assert false

type
  BinaryPrefixMode* = enum ## The different names for binary prefixes.
    bpIEC,                 # use the IEC/ISO standard prefixes such as kibi
    bpColloquial           # use the colloquial kilo, mega etc

func formatSize*(bytes: int64; decimalSep = '.'; prefix = bpIEC; includeSpace = false): string =
  ## Rounds and formats `bytes`.
  ##
  ## By default, uses the IEC/ISO standard binary prefixes, so 1024 will be
  ## formatted as 1KiB.  Set prefix to `bpColloquial` to use the colloquial
  ## names from the SI standard (e.g. k for 1000 being reused as 1024).
  ##
  ## `includeSpace` can be set to true to include the (SI preferred) space
  ## between the number and the unit (e.g. 1 KiB).
  ##
  ## See also:
  ## * `strformat module<strformat.html>`_ for string interpolation and formatting
  runnableExamples:
    assert formatSize((1'i64 shl 31) + (300'i64 shl 20)) == "2.293GiB"
    assert formatSize((2.234*1024*1024).int) == "2.233MiB"
    assert formatSize(4096, includeSpace = true) == "4 KiB"
    assert formatSize(4096, prefix = bpColloquial, includeSpace = true) == "4 kB"
    assert formatSize(4096) == "4KiB"
    assert formatSize(5_378_934, prefix = bpColloquial, decimalSep = ',') == "5,129MB"

  assert bytes >= 0
  # It doesn't needs Zi and larger units until we use int72 or larger ints.
  const iecPrefixes = ["", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei"]
  const collPrefixes = ["", "k", "M", "G", "T", "P", "E"]

  # TODO: use fastLog2 when it is added.
  #let lg2 = if bytes == 0: 0 else: fastLog2(bytes)
  let Lg2MaxDiv10 = sizeof(bytes) * 8 div 10
  var lg2 = Lg2MaxDiv10 * 10
  var matchedIndex = Lg2MaxDiv10
  for i in 1 .. Lg2MaxDiv10:
    if (bytes shr (i * 10)) == 0:
      lg2 = (i - 1) * 10
      matchedIndex = i - 1
      break
  # Lower bits that are smaller than 0.001 when `bytes` is converted to a real number and added prefix, are discard.
  # Then it is converted to float with round down.
  let discardBits = (lg2 div 10 - 1) * 10

  var prefixes: array[7, string]
  if prefix == bpColloquial:
    prefixes = collPrefixes
  else:
    prefixes = iecPrefixes

  let fbytes = if lg2 < 10: bytes.float elif lg2 < 20: bytes.float / 1024.0 else: (bytes shr discardBits).float / 1024.0
  result = formatFloat(fbytes, format = ffDecimal, precision = 3,
      decimalSep = decimalSep)
  result.trimZeros(decimalSep)
  if includeSpace:
    result.add ' '
  result.add prefixes[matchedIndex]
  result.add 'B'

func contains*(s, sub: string): bool =
  ## Same as `find(s, sub) >= 0`.
  ##
  ## See also:
  ## * `find func<#find,string,string,Natural,int>`_
  return find(s, sub) >= 0

func contains*(s: string, chars: set[char]): bool =
  ## Same as `find(s, chars) >= 0`.
  ##
  ## See also:
  ## * `find func<#find,string,set[char],Natural,int>`_
  return find(s, chars) >= 0
