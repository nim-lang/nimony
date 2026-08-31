## ASCII-focused helpers for splitting, searching, comparing, replacing, escaping,
## percent-formatting (`%`, `format`), float rendering (`formatFloat`,
## `formatBiggestFloat`), and human-readable sizes (`formatSize`).
##
## Shared `set[char]` constants (`Whitespace`, `Letters`, …) describe common character
## classes; see each constant for details.

{.feature: "lenientnils".}

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

func spaces*(n: int): string =
  ## Returns a string with `n` space characters.
  result = newString(n)
  for i in 0 ..< n: result[i] = ' '

func repeat*(c: char; n: int): string =
  ## Returns a string made of `c` repeated `n` times.
  result = newString(n)
  for i in 0 ..< n: result[i] = c

func repeat*(s: string; n: int): string =
  ## Returns `s` repeated `n` times.
  result = ""
  for i in 0 ..< n: result.add s

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

func endsWith*(s: string; c: char): bool {.inline.} =
  ## True if `s` is non-empty and its last character is `c`.
  if s.len > 0: s[s.len-1] == c else: false

when defined(nimNativeIo):
  func strlen*(x: cstring): int =
    ## Length of the C string `x` (not counting the terminating zero).
    ## Freestanding (`nimony n`, libc-free): scan for the NUL terminator directly.
    var i = 0
    while x[i] != '\0': inc i
    result = i
else:
  func strlen*(x: cstring): int {.importc: "strlen", header: "<string.h>".}
    ## Length of the C string `x` (not counting the terminating zero); `0` when `x` is nil.

func `$`*(x: cstring): string =
  ## Copies a nil-terminated C string into a Nim `string`.
  if x == nil:
    result = ""
  else:
    let L = int strlen(x)
    result = newString(L)
    for i in 0..<result.len:
      result[i] = x[i]

func `$`*(x: char): string =
  ## Returns a one-character string containing `x`.
  result = newString(1)
  result[0] = x

func substrEq(s: string, pos: int, substr: string): bool =
  var length = substr.len
  if length > 0:
    var i = 0
    while i < length and pos + i < s.len and s[pos + i] == substr[i]:
      inc i
    i == length
  else:
    false

template stringHasSep(s: string, index: int, seps: set[char]): bool =
  s[index] in seps

template stringHasSep(s: string, index: int, sep: char): bool =
  s[index] == sep

template stringHasSep(s: string, index: int, sep: string): bool =
  s.substrEq(index, sep)

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

template accResult(iter: untyped) {.untyped.} =
  result = @[]
  for x in iter:
    result.add x

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

iterator split*(s: string; sep: char; maxsplit: int = -1): string =
  ## Splits the string `s` into substrings using the separator `sep`.
  ##
  ## Substrings are separated by the character `sep`.
  splitCommon(s, sep, maxsplit, 1)

iterator split*(s: string; sep: string; maxsplit: int = -1): string =
  ## Splits the string `s` into substrings using the separator `sep`.
  let sepLen = if sep.len == 0: 1 else: sep.len
  splitCommon(s, sep, maxsplit, sepLen)

iterator splitLines*(s: string; keepEol = false): string =
  ## Splits the string `s` into its containing lines.
  ## Supports LF, CR, and CR-LF line endings.
  var first = 0
  var last = 0
  var eolpos = 0
  while true:
    while last < s.len and s[last] notin {'\c', '\l'}:
      inc last

    eolpos = last
    if last < s.len:
      if s[last] == '\l':
        inc last
      elif s[last] == '\c':
        inc last
        if last < s.len and s[last] == '\l':
          inc last

    yield substr(s, first, if keepEol: last - 1 else: eolpos - 1)

    if eolpos == last:
      break

    first = last

iterator splitWhitespace*(s: string; maxsplit: int = -1): string =
  ## Splits the string `s` at whitespace, stripping leading and trailing
  ## whitespace and collapsing runs of whitespace (no empty substrings are
  ## produced). If `maxsplit` is positive, at most `maxsplit` splits are made.
  ##
  ##   ```nim
  ##   for word in splitWhitespace("  foo \t bar  baz  "):
  ##     writeLine(stdout, word)
  ##   ```
  ##
  ## ...generates "foo", "bar", "baz".
  var last = 0
  var splits = maxsplit
  while last < len(s):
    while last < len(s) and s[last] in Whitespace: inc(last)
    var first = last
    while last < len(s) and s[last] notin Whitespace: inc(last)
    if first <= last-1:
      if splits == 0: last = len(s)
      yield substr(s, first, last-1)
      if splits == 0: break
      dec(splits)

func split*(s: string; seps: set[char] = Whitespace; maxsplit: int = -1): seq[string] =
  ## The same as the `split` iterator, but returns a sequence of substrings.
  accResult(split(s, seps, maxsplit))

func split*(s: string; sep: char; maxsplit: int = -1): seq[string] =
  ## The same as the `split` iterator, but returns a sequence of substrings.
  accResult(split(s, sep, maxsplit))

func split*(s: string; sep: string; maxsplit: int = -1): seq[string] =
  ## The same as the `split` iterator, but returns a sequence of substrings.
  accResult(split(s, sep, maxsplit))

func splitLines*(s: string; keepEol = false): seq[string] =
  ## The same as the `splitLines` iterator, but returns a sequence of substrings.
  accResult(splitLines(s, keepEol))

func splitWhitespace*(s: string; maxsplit: int = -1): seq[string] =
  ## The same as the `splitWhitespace` iterator, but returns a sequence of substrings.
  accResult(splitWhitespace(s, maxsplit))

func join*(a: openArray[string]; sep: string = ""): string =
  ## Concatenates all strings in `a`, separating them with `sep`.
  runnableExamples:
    doAssert join(["A", "B", "Conclusion"], " -> ") == "A -> B -> Conclusion"
    doAssert join(["ab", "cd"]) == "abcd"
  result = ""
  for i in 0 ..< a.len:
    if i > 0: result.add sep
    result.add a[i]

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
  if slice.a < s.len and slice.a >= 0 and slice.b < s.len:
    discard
  else:
    # `return` here better than assert because it's no side effect
    return
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
  startsWithImpl s, prefix

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

func toLowerAscii*(c: char): char {.inline.} =
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
  return 0

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
  ## Returns a copy of `s` where every `sub` is replaced by `by`.
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

func toHex*(x: BiggestInt; len: Positive): string =
  ## Converts `x` to a hexadecimal string exactly `len` uppercase digits wide
  ## (no `0x` prefix). Negative values render in two's complement, and a value
  ## needing more than `len` digits keeps only its least-significant `len` nibbles.
  runnableExamples:
    doAssert toHex(BiggestInt(1984), 4) == "07C0"
    doAssert toHex(BiggestInt(-1), 2) == "FF"
  var n = x
  result = newString(len)
  for j in countdown(len - 1, 0):
    result[j] = HexChars[int(n and 0xF)]
    n = n shr 4
    # Keep sign nibbles coming for a negative value regardless of whether `shr`
    # is arithmetic or logical, so its two's-complement form fills `len`.
    if n == 0 and x < 0: n = -1

func toHex*[T: SomeInteger](x: T): string {.inline.} =
  ## Full-width hex for `x`: `2 * sizeof(T)` uppercase digits, e.g.
  ## `toHex(0'u16) == "0000"`, `toHex(255'u8) == "FF"`.
  toHex(BiggestInt(x), 2 * sizeof(T))

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
  ## * `addEscapedChar func<system.html#addEscapedChar,string,char>`_
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

type
  FloatFormatMode* = enum
    ## The different modes of floating point formatting.
    ffDefault,   ## use the shorter floating point notation
    ffDecimal,   ## use decimal floating point notation
    ffScientific ## use scientific notation (using `e` character)

# Float formatting reproduces C's `%f`, `%e` and `%g` from the EXACT decimal
# expansion of the double instead of calling `snprintf`. Three reasons:
#
# * WebAssembly has no variadic calling convention. An imported function has
#   ONE type, so a `{.varargs.}` importc cannot be called with different
#   argument tails, and `c_snprintf(buf, n, fmt, precision, f)` emits a call
#   whose f64 argument contradicts the import's declared type. The engine
#   rejects the whole MODULE, not the call, which is how a single
#   `formatFloat` on a diagnostic path takes a program down before it starts.
# * The freestanding targets and `nimony n` have no libc to call.
# * `snprintf` produces the digit separator of the current C locale, which
#   then has to be patched back to `decimalSep`.
#
# A double is `m * 2^e` with `m < 2^53`, so its decimal expansion is finite:
# for `e >= 0` it is the integer `m * 2^e`, and for `e < 0` it is `m * 5^-e`
# read with the point moved `-e` places to the left. Both are computed exactly
# with a small base-10^9 bignum on the stack, and that exactness is what makes
# the rounding agree with C digit for digit: the digit after the cut and
# everything behind it are the true expansion, so a tie is RECOGNIZED rather
# than guessed at, and goes half-to-even like the default rounding mode.
# (Rounding the shortest round-tripping form from `$f` instead would get
# 9.995 wrong at two places: it prints as "9.995" but the double is
# 9.99499999999999957, which rounds DOWN.)

const
  DecBase = 1_000_000_000'u64
    ## One limb holds nine decimal digits.
  DecMaxLimbs = 96
    ## `2^-1074 * (2^53-1)` needs 767 digits, the largest expansion there is.
  Pow5: array[0..12, uint32] = [1'u32, 5, 25, 125, 625, 3125, 15625, 78125,
    390625, 1953125, 9765625, 48828125, 244140625]
    ## `5^12` is the largest power of five below `DecBase`.

type
  DecInt = object ## Non-negative integer, base `DecBase`, least significant limb first.
    limbs: array[DecMaxLimbs, uint32]
    len: int

func mulSmall(x: var DecInt; factor: uint32) =
  var carry = 0'u64
  var i = 0
  while i < x.len:
    let cur = uint64(x.limbs[i]) * uint64(factor) + carry
    x.limbs[i] = uint32(cur mod DecBase)
    carry = cur div DecBase
    inc i
  while carry > 0'u64 and x.len < DecMaxLimbs:
    x.limbs[x.len] = uint32(carry mod DecBase)
    carry = carry div DecBase
    inc x.len

func toDigits(x: DecInt): string =
  ## Decimal digits of `x`, without leading zeros.
  result = ""
  var i = x.len - 1
  while i > 0 and x.limbs[i] == 0'u32: dec i
  if i < 0 or (i == 0 and x.limbs[0] == 0'u32):
    result = "0"
  else:
    result.add $x.limbs[i]
    dec i
    while i >= 0:
      let limb = $x.limbs[i]
      for _ in limb.len ..< 9: result.add '0'
      result.add limb
      dec i

func exactDigits(f: BiggestFloat): tuple[neg: bool, digits: string, exp: int] =
  ## `f` as `0.<digits> * 10^exp`, `digits` exact and free of leading and
  ## trailing zeros. Zero yields no digits and `exp = 1`, which is the
  ## exponent `%e` and `%g` print for it (`0.000e+00`, not `0.000e-01`).
  let bits = cast[uint64](f)
  # Seed the whole tuple: nimony proves initialization per RESULT, not per
  # field, so field-by-field assignment leaves it "possibly uninitialized".
  result = (neg: (bits shr 63) != 0'u64, digits: "", exp: 1)
  let biasedExp = int((bits shr 52) and 0x7FF'u64)
  var mant = bits and 0xF_FFFF_FFFF_FFFF'u64
  var e2 = 0
  if biasedExp == 0:
    if mant == 0'u64: return                  # +-0.0
    e2 = -1074                                # subnormal: no hidden bit
  else:
    mant = mant or (1'u64 shl 52)
    e2 = biasedExp - 1075
  var x = default(DecInt)
  while mant > 0'u64:
    x.limbs[x.len] = uint32(mant mod DecBase)
    mant = mant div DecBase
    inc x.len
  var pointPos = 0                            # digits to the right of the point
  if e2 > 0:
    var k = e2
    while k > 0:
      let step = if k > 29: 29 else: k        # 2^29 is the largest power of two below the base
      mulSmall(x, 1'u32 shl step)
      k = k - step
  elif e2 < 0:
    # `m * 2^-k` is `(m * 5^k) / 10^k`: the same digits, the point moved.
    var k = -e2
    pointPos = k
    while k > 0:
      let step = if k > 12: 12 else: k
      mulSmall(x, Pow5[step])
      k = k - step
  var d = toDigits(x)
  result.exp = d.len - pointPos
  var last = d.len
  while last > 0 and d[last-1] == '0': dec last
  d.setLen last
  result.digits = d

func roundDigits(digits: string; keep: int): tuple[digits: string, carry: bool] =
  ## Round the exact `digits` to `keep` leading digits, half-to-even. `carry`
  ## reports an overflow into a new leading digit, so `999` kept at two digits
  ## is `("10", true)`: the caller reads it with the exponent raised by one.
  result = (digits: "", carry: false)
  if keep < 0: return
  if keep >= digits.len:
    result.digits = digits
    for _ in digits.len ..< keep: result.digits.add '0'
    return
  var kept = if keep == 0: "" else: digits.substr(0, keep-1)
  let first = digits[keep]
  var roundUp = false
  if first > '5':
    roundUp = true
  elif first == '5':
    var restNonZero = false
    for i in keep+1 ..< digits.len:
      if digits[i] != '0': restNonZero = true
    if restNonZero:
      roundUp = true
    else:
      # An exact tie, because the digits are the exact expansion: half-to-even.
      let prev = if keep > 0: digits[keep-1] else: '0'
      roundUp = ((int(prev) - int('0')) and 1) == 1
  if roundUp:
    var i = kept.len - 1
    var carry = true
    while i >= 0 and carry:
      if kept[i] == '9':
        kept[i] = '0'
      else:
        kept[i] = char(int(kept[i]) + 1)
        carry = false
      dec i
    if carry:
      # All nines: the leading 1 takes the place of the last digit and the
      # caller lifts the exponent, `999` -> `10` at exponent+1.
      if kept.len > 0: kept.setLen kept.len - 1
      kept = "1" & kept
      result.carry = true
  result.digits = kept

func fmtDecimal(neg: bool; digits: string; exp, precision: int;
                decimalSep: char; forcePoint: bool): string =
  ## `%.*f`: `precision` digits after the point. `forcePoint` is the `#` flag,
  ## which keeps the point at precision 0 ("2." rather than "2").
  let (d, carry) = roundDigits(digits, exp + precision)
  var e = exp
  if carry: inc e
  result = ""
  if neg: result.add '-'
  if e <= 0:
    result.add '0'
  else:
    var i = 0
    while i < e:
      result.add (if i < d.len: d[i] else: '0')
      inc i
  if precision > 0 or forcePoint:
    result.add decimalSep
    # The k-th digit after the point sits at index `e + k - 1`; a negative
    # index is a leading zero of the fraction, one past the end a trailing one.
    var k = 1
    while k <= precision:
      let idx = e + k - 1
      result.add (if idx >= 0 and idx < d.len: d[idx] else: '0')
      inc k

func fmtScientific(neg: bool; digits: string; exp, precision: int;
                   decimalSep: char; forcePoint: bool): string =
  ## `%.*e`: one digit before the point, `precision` after it.
  let (d, carry) = roundDigits(digits, precision + 1)
  var e = exp
  if carry: inc e
  result = ""
  if neg: result.add '-'
  result.add (if d.len > 0: d[0] else: '0')
  if precision > 0 or forcePoint:
    result.add decimalSep
    var i = 1
    while i <= precision:
      result.add (if i < d.len: d[i] else: '0')
      inc i
  let e10 = e - 1
  result.add 'e'
  if e10 < 0: result.add '-' else: result.add '+'
  let a = abs(e10)
  if a < 10: result.add '0'                   # C pads the exponent to two digits
  result.add $a

func stripTrailingZeros(s: string; decimalSep: char): string =
  ## `%g` without `#` drops the trailing zeros of the fraction, and the point
  ## with them when nothing is left behind it.
  var mantEnd = s.len
  var i = 0
  while i < s.len:
    if s[i] == 'e':
      mantEnd = i
      break
    inc i
  var pointAt = -1
  i = 0
  while i < mantEnd:
    if s[i] == decimalSep:
      pointAt = i
      break
    inc i
  if pointAt < 0:
    result = s
  else:
    var last = mantEnd
    while last > pointAt+1 and s[last-1] == '0': dec last
    if last == pointAt+1: last = pointAt
    result = s.substr(0, last-1) & s.substr(mantEnd, s.len-1)

func fmtDefault(neg: bool; digits: string; exp, precision: int;
                decimalSep: char; forcePoint: bool): string =
  ## `%.*g`: `p` significant digits, printed in `%e` style when the exponent
  ## falls outside `[-4, p)` and in `%f` style otherwise. C reads a precision
  ## of 0 as 1.
  let p = if precision == 0: 1 else: precision
  let (d, carry) = roundDigits(digits, p)
  var e = exp
  if carry: inc e
  let x = e - 1                               # the exponent `%e` would print
  # `d` is already rounded to `p` digits, so the re-rounding below is a no-op.
  let s = if x < -4 or x >= p:
            fmtScientific(neg, d, e, p-1, decimalSep, forcePoint)
          else:
            fmtDecimal(neg, d, e, p-1-x, decimalSep, forcePoint)
  result = if forcePoint: s else: stripTrailingZeros(s, decimalSep)

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
  let bits = cast[uint64](f)
  if int((bits shr 52) and 0x7FF'u64) == 0x7FF:
    result = $f                               # inf, -inf, nan print as themselves
  else:
    let (neg, digits, exp) = exactDigits(f)
    # Without an explicit precision C uses its default of 6 and no `#` flag;
    # with one it used `%#.*g` and friends, whose point is never dropped.
    let p = if precision.int < 0: 6 else: precision.int
    let forcePoint = precision.int >= 0
    case format
    of ffDefault: result = fmtDefault(neg, digits, exp, p, decimalSep, forcePoint)
    of ffDecimal: result = fmtDecimal(neg, digits, exp, p, decimalSep, forcePoint)
    of ffScientific: result = fmtScientific(neg, digits, exp, p, decimalSep, forcePoint)

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

func invalidFormatString(formatstr: string) {.noinline, raises.} =
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
        discard

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

  if bytes < 0: return "<negative amount of bytes>"
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

func parseBiggestInt*(s: string): BiggestInt {.raises.} =
  ## Parses a decimal integer value contained in `s`.
  ##
  ## `ValueError` is raised if `s` is not a valid integer.
  result = BiggestInt(0)
  let L = parseutils.parseBiggestInt(s, result)
  if L != s.len or L == 0:
    raise ValueError

func parseInt*(s: string): int {.raises.} =
  ## Parses a decimal integer value contained in `s`.
  ##
  ## `ValueError` is raised if `s` is not a valid integer.
  ##   ```nim
  ##   assert parseInt("-0042") == -42
  ##   ```
  result = int(parseBiggestInt(s))
