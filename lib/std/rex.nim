#
#
#            Nimony's Standard Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Regular expressions, and a lexer generator built on the same engine.
##
## The engine is a DFA: a pattern is turned into a deterministic automaton
## once, and matching then costs one table step per input character with no
## backtracking. There is no input that makes `(a+)+b` take exponential time
## here, and a search costs time linear in the subject whatever the pattern
## (see *Searching*).
##
## A pattern means what it says because it has to be **deterministic**: read
## from left to right, the next character must always decide which part of
## the pattern it belongs to. `\w+\s*=\s*\w+` is "a key, optional spaces,
## `=`, optional spaces, a value" and nothing else, because a word character,
## a space and `=` can never be confused. See *Deterministic patterns*.
##
## Two ways to use it:
##
## **At runtime** — build a `Regex` from a string and match against it:
##
##   ```nim
##   import std/rex
##
##   let pattern = re"[a-z0-9]+\s*=\s*[a-z0-9]+"
##   echo matchLen("key1 =  cal9", pattern)   # 12
##   ```
##
## **At compile time** — `lex` turns a whole set of patterns into one automaton
## and emits it as a `case` statement, so the lexer in your program contains no
## regex engine at all:
##
##   ```nim
##   var pos = 0
##   while pos < input.len:
##     let start = pos
##     lex input, pos:
##     of r"\d+": echo "an integer ", substr(input, start, pos-1)
##     of "else": echo "the ELSE keyword"
##     of r"[a-zA-Z_]\w*": echo "an identifier"
##     of r".": discard
##   ```
##
## The two share one implementation of what a pattern means
## (`std/private/regexcore`), so a pattern cannot mean one thing in a generated
## lexer and another at runtime.
##
## Patterns and input may contain UTF-8 and arbitrary bytes. Literals, `.`,
## and character classes support Unicode code points; byte values 128..255
## also match the corresponding individual byte. The longest match wins.
## Match lengths, positions, and capture bounds are byte offsets into the
## original string.
##
## Deterministic patterns
## ======================
##
## `re` and `tryRe` reject a pattern in which one character could belong to
## two different parts of it, and the error says where:
##
##   ```
##   re".*x"  # at the start, the character `x` could belong to either `.` or `x`
##   ```
##
## The `.*` would read the `x` as well as the `x` itself; `[^x]*x` says what
## was meant. Most such patterns are a bug waiting for an input, and the
## rewrite the check forces is the pattern that was meant all along:
##
## ===========  ===============  ============================
## Rejected     Write instead    Why
## ===========  ===============  ============================
## `\".*\"`     `\"[^"]*\"`      `.*` also reads quotes
## `\d+|\w+`    `\w+`            a digit starts both branches
## `\d+\.?\d*`  `\d+(?:\.\d*)?`  digits after a missing `.`
## `a|a*b`      `a(?:a*b)?|b`    an `a` starts both branches
## ===========  ===============  ============================
##
## Alternatives that merely start alike are fine: they are factored before
## the check, so `if|in|int` is read as `i(?:f|n(?:t)?)` and `ab|abc` as
## `ab(?:c)?`. That works for identical prefixes, not for overlapping
## classes, which is why `\d+|\w+` still has to be written `\w+`.
##
## A deterministic pattern has exactly one way of reading any match, and the
## rest of this module relies on it: the capture bounds are exact, the
## automaton stays small, at roughly one state per character of the
## pattern, so `tryRe` on a pattern from outside the program does not blow
## up, and a search keeps at most that many threads alive.
##
## `reAllowEvil` turns the check off. The pattern then means whatever the
## automaton makes of it — the longest match from the leftmost start — its
## capture bounds are one of several possible readings, and compiling it can
## take time and memory exponential in its length: `(?:a|b)*a(?:a|b){20}`
## needs two million states. Explicit flags replace the default, so keep
## `reExtended`:
##
##   ```nim
##   let quoted = re(r"\( .* \)", {reExtended, reAllowEvil})
##   ```
##
## `lex` and `rematch` do not apply the check: overlapping rules (a keyword
## and the identifier pattern that also covers it) are their point.
##
## Searching
## =========
##
## `find`, `findBounds`, `findAll`, `replace` and `split` look for the
## leftmost match and take the longest one from there. A search does not
## simply restart the automaton at every position, which costs time
## quadratic in the subject for a pattern like `\w+@\w+` over a long run of
## letters. It tries one start, and when that fails, the starts among the
## bytes the attempt read run together in one pass, a start being dropped as
## soon as an earlier one is in the same state: from there on they would
## match alike, and the earlier one wins. No byte is read by two attempts,
## so the cost is linear in the subject for every pattern, times the number
## of starts alive at once, which for a deterministic pattern is at most its
## number of positions and usually one or two.
##
## Between attempts the search skips ahead: to the next byte a match can
## start with, and, when every match contains a literal (the `@` above), to
## the lowest start that can reach the literal's next occurrence. A subject
## without it costs one scan.
##
## A pattern with back references is the exception: what a group matched is
## part of each start's state, so its search tries the starts one by one.
##
## Captures
## ========
##
## `(x)` records where the group matched; `matchLen`/`match`/`fullMatch` take a
## `var seq[Capture]` and `capture` reads the text back out:
##
##   ```nim
##   var caps: seq[Capture] = @[]
##   if match("key = value", re"(\w+) \s* = \s* (\w+)", caps):
##     echo capture("key = value", caps, 0)   # key
##     echo capture("key = value", caps, 1)   # value
##   ```
##
## `caps` has one entry per group. A group that took no part in the match,
## such as the `(xyz)` of `(abc)|(xyz)` matching `"abc"`, has `last <
## first`, and `capture` returns `""` for it. The match is decided without
## the groups and they are located afterwards, by following every way the
## automaton could have read the match; a deterministic pattern has only one.
##
## With a back reference the groups are part of what is matched, and the
## old limits of a DFA apply: a capture must lie on every accepting path, so
## `(abc)|(xyz)` is an error there.
##
## Syntax
## ======
##
## `.` any character except `\0` · `[abc]` `[^abc]` `[a-z]` character classes ·
## `x*` `x+` `x?` `x{m,n}` repetition · `x|y` alternation · `(x)` capture ·
## `(?:x)` grouping without a capture · `"abc"` a literal run · `\d \D \s \S
## \w \W` classes · `\A` (`^`) start, `\Z` (`$`) end, `\b` `\B` word boundary ·
## `\1` back reference · `\n \r \t \e \a \v \f \b` and `\123` escapes.
##
## `^` and `\A` match at the start of the subject only, also for `match` and
## searches that begin further in. `\b` is between a word character (`\w`'s
## ASCII part and every non-ASCII byte) and something else.
##
## By default patterns are parsed with `reExtended`, so unescaped spaces and
## tabs are ignored and a pattern may be laid out for reading. Match a literal
## space with `\ `, `[ ]` or `" "`.
##
## Differences from Nim's `std/re`
## ===============================
##
## The name is different on purpose: this is not PCRE, and code ported from
## Nim's `std/re` should be checked rather than assumed to work. What stays the
## same is the basic shape — `matchLen`, `match`, `find`, `findBounds`,
## `contains` and `startsWith` take a `start = 0`, and "no match" is `-1`, or
## `(-1, 0)` for `findBounds`. `replace` inserts `by` literally, like Nim's
## `replace`. The differences:
##
## * **Whitespace is ignored by default.** Patterns are parsed with
##   `reExtended`, so `re"a b"` matches `"ab"`. In Nim only `rex` does that.
## * **Patterns must be deterministic** (see *Deterministic patterns*):
##   `.*x`, `\".*\"` and `\d+|\w+` are compile-time errors unless they are
##   given `reAllowEvil`.
## * **The longest match wins**, not the first alternative that works:
##   `a|ab` matches all of `"ab"`; PCRE stops after `"a"`.
## * `"..."` is a literal run, as in `std/pegs`. In PCRE `"` is an ordinary
##   character.
## * Outside a character class `\n` matches any of `\r\n`, `\n` and `\r`.
## * `\123` is decimal: a back reference, or, with a leading zero (`\065`), a
##   character code. It is not octal.
## * `.` matches every Unicode code point and nonzero byte, newlines included.
##   `\s` is `\1..\32` and `\w` includes non-ASCII code points and bytes.
## * `re` takes only a string literal and compiles it with the program;
##   `re(someVar)` is `tryRe` here, which reports an error instead of raising.
## * Captures are positions (`seq[Capture]`, read with `capture`), not a
##   `var openArray[string]`.
## * `findAll` yields `(first, last)` bounds, not strings.
## * Not provided: `=~`/`matches`, `replacef` with `$1`, `multiReplace`,
##   `transformFile`, `endsWith`, `escapeRe`, `rex`, the `split` iterator and
##   its `maxsplit`, and `RegexError`.
## * Not supported in patterns: the flags `reIgnoreCase`, `reMultiLine` and
##   `reDotAll`, inline options such as `(?i)`, lazy quantifiers (`x*?` is
##   accepted but means `x*`), lookaround, named groups, `\x` escapes and
##   Unicode property classes such as `\p{L}`. Some of these features need
##   backtracking, which this engine rules out.

{.feature: "staticContracts".}

import std / private / regexcore

export RegexFlag, reExtended, reNoBackrefs, reNoCaptures, reAllowEvil
export Regex, Capture, RegexOpcode, RegexInstr
export opcRet, opcRetAccept, opcAccept, opcTestSet, opcTestChar, opcTJmp, opcBegin, opcEnd,
       opcWordBound, opcCaptureBegin, opcCaptureEnd, opcBackref

# ---------------------------------------------------------------------------
# Construction
# ---------------------------------------------------------------------------

template re*(pattern: string): Regex {.plugin: "deps/regex".}
  ## The regular expression `pattern`, compiled **while your program is
  ## compiled**.
  ##
  ##   ```nim
  ##   let assignment = re"[a-z0-9]+\s*=\s*[a-z0-9]+"
  ##   ```
  ##
  ## `pattern` must be a string literal. A malformed one is a compile-time
  ## error naming what is wrong with it, and so is an ambiguous one (see
  ## *Deterministic patterns*). No automaton is built at run time:
  ## what reaches the binary is the finished program the matcher walks. That
  ## matters more than it sounds — the subset construction runs over a
  ## 260-letter alphabet, so building even a small regex at start-up costs far
  ## more than matching with it ever will.
  ##
  ## A `re` in a loop still rebuilds its (small) literal on every iteration;
  ## bind it to a `let` outside the loop, as above.
  ##
  ## For a pattern that is only known at run time — from a config file, a
  ## command line, a request — use `tryRe`, which hands back the diagnostic
  ## instead of failing the build.

template re*(pattern: string; flags: set[RegexFlag]): Regex {.
    plugin: "deps/regex".}
  ## `re` with explicit flags. Both arguments must be literals: the pattern a
  ## string literal, `flags` a set constructor such as `{reNoCaptures}`.

proc tryRe*(pattern: string; dest: var Regex; err: var string;
            flags: set[RegexFlag] = {reExtended}): bool =
  ## Compiles a pattern that is not known until run time. On failure `err` says
  ## what is wrong with it and `dest` is left matching nothing.
  ##
  ## This is the form to use on a pattern that came from outside the program.
  ## An invalid pattern is then an ordinary input rather than a bug, and its
  ## diagnostic is something to show, not something to crash on. An
  ## ambiguous pattern is rejected just like with `re` (see *Deterministic
  ## patterns*). Passing `reAllowEvil` for an outside pattern hands its author
  ## the time and memory compiling it takes, which can be exponential.
  compileRegex(pattern, flags, dest, err)

func isEmpty*(r: Regex): bool {.inline.} =
  ## True for a regex that never matches — what `tryRe` leaves behind when it
  ## fails.
  r.code.len == 0

func captureCount*(r: Regex): int {.inline.} =
  ## The number of capture groups in `r`.
  r.captures

# ---------------------------------------------------------------------------
# Matching
# ---------------------------------------------------------------------------

func findByte(s: string; b: char; pos, stop: Natural): int
    {.requires: stop <= s.len, ensures: result >= 0 and result <= s.len.} =
  ## The first position of `b` in `pos ..< stop`; `stop` when there is none.
  var i: Natural = pos
  # Eight bytes at a time: `w` has a zero byte where `s` has `b`, and the
  # test below is true exactly when it has one. The byte loop then finds it.
  let ones = 0x0101010101010101'u64
  let pattern = ones * uint64(ord(b))
  let raw = readRawData(s)
  while i < stop - 7:
    var w = 0'u64
    copyMem(addr w, addr raw[i], 8)
    w = w xor pattern
    if ((w - ones) and not w and (ones shl 7)) != 0: break
    inc i, 8
  while i < stop:
    if s[i] == b: return i
    inc i
  result = stop

func skipToCandidate(s: string; r: Regex; pos, stop: Natural): int
    {.requires: stop <= s.len, ensures: result >= 0 and result <= s.len.} =
  ## The first position in `pos ..< stop` where a match can start; `stop` when
  ## there is none.
  if r.firstByte >= 0: return findByte(s, char(r.firstByte), pos, stop)
  var i: Natural = pos
  let first = r.firstBytes # read in the loop, it is copied every iteration
  while i < stop:
    if s[i] in first: return i
    inc i
  result = stop

func matchLen*(s: string; r: Regex; caps: var seq[Capture]; start = 0): int =
  ## The length of the longest match of `r` starting exactly at `start`, or
  ## `-1` when there is none. Capture bounds are written to `caps`, which is
  ## grown as needed; its entries are absolute positions into `s`.
  var endPos = start
  let rule = exec(r, s, caps, start, endPos)
  result = if rule <= 0: -1 else: endPos - start

func matchLen*(s: string; r: Regex; start = 0): int =
  ## The length of the longest match of `r` starting exactly at `start`, or
  ## `-1` when there is none.
  var caps: seq[Capture] = @[]
  result = matchLen(s, r, caps, start)

func match*(s: string; r: Regex; start = 0): bool =
  ## Does `r` match at `start`? The match need not reach the end of `s` — use
  ## `fullMatch` for that, or anchor the pattern with `\Z`.
  matchLen(s, r, start) >= 0

func match*(s: string; r: Regex; caps: var seq[Capture]; start = 0): bool =
  ## `match`, additionally reporting the capture bounds.
  matchLen(s, r, caps, start) >= 0

func fullMatch*(s: string; r: Regex; start = 0): bool =
  ## Does `r` match all of `s[start..]`?
  matchLen(s, r, start) == s.len - start

func fullMatch*(s: string; r: Regex; caps: var seq[Capture]; start = 0): bool =
  ## `fullMatch`, additionally reporting the capture bounds.
  matchLen(s, r, caps, start) == s.len - start

# Search
# ---------------------------------------------------------------------------
#
# The threads (`regexcore.Threads`) find the leftmost-longest match in one
# pass. While none is alive the search skips to the next start that can
# match: the next byte a match starts with, and, for a pattern with a literal
# every match contains (`regexcore.findAnchor`), the lowest start that can
# reach the literal's next occurrence.

func findLit(s, lit: string; start: Natural): int
    {.requires: lit.len > 0, ensures: result < s.len.} =
  ## The first occurrence of `lit` at or after `start`, or `-1`.
  var i: Natural = start
  while i < s.len:
    i = findByte(s, lit[0], i, s.len)
    if i >= s.len: break
    var p = i + 1
    var m = 1
    while m < lit.len and p < s.len and s[p] == lit[m]:
      inc p
      inc m
    if m == lit.len: return i
    inc i
  result = -1

func lowerBound(s: string; r: Regex; k, floor: int): int
    {.requires: k <= s.len and floor >= 0, ensures: result >= floor.} =
  ## The leftmost start a match using the literal at `k` can have, but never
  ## less than `floor`. Walking back over the bytes the factors before the
  ## literal are made of can only stop early, and it never moves left when `k`
  ## moves right, so starts the search skips below it are never needed later.
  var e = k
  for i in 0 ..< min(r.litBack.len, r.litBackSets.len):
    let limit = r.litBack[i]
    let bytes = r.litBackSets[i]
    if limit < 0 and bytes == AllBytes: return floor
    var stop: Natural = floor
    if limit >= 0:
      let x = e - limit
      if x > floor: stop = x
    while e > stop and e <= s.len and s[e - 1] in bytes: dec e
    if e <= floor: return floor
  result = max(e, floor)

func search(s: string; r: Regex; caps: var seq[Capture]; start: Natural;
            t: var Threads): (int, int) =
  ## The leftmost match at or after `start` as `(first, length)`, or `(-1, 0)`.
  if start > s.len: return (-1, 0)
  if r.backrefs > 0:
    var i: Natural = start
    while i <= s.len:
      i = skipToCandidate(s, r, i, s.len)
      let L = matchLen(s, r, caps, i)
      if L >= 0: return (i, L)
      inc i
    return (-1, 0)
  var best = -1     # the leftmost start that has matched
  var bestEnd = 0   # and where its longest match ends
  var k = -1        # the occurrence of `lit` the last skip found
  var kLow = 0      # and the lowest start that can reach it
  var sp: Natural = start
  while best < 0:
    if r.lit.len > 0:
      let fromPos = sp + max(r.litMin, 0)
      if k < fromPos:
        k = findLit(s, r.lit, fromPos)
        if k < 0: break
        kLow = k - r.litMin
        if r.litBack.len > 0: kLow = lowerBound(s, r, k, sp)
      if kLow > sp: sp = kLow
    sp = skipToCandidate(s, r, sp, s.len)
    var reach = 0
    let L = attempt(r, s, sp, reach)
    if L >= 0:
      best = sp
      bestEnd = sp + L
    elif sp >= s.len:
      break
    else:
      var next: Natural = sp + 1
      runThreads t, r, s, next, reach, best, bestEnd
      if next > s.len: break
      sp = next
  if best < 0: return (-1, 0)
  if r.captures > 0: discard matchLen(s, r, caps, best)
  result = (best, bestEnd - best)

func findBounds*(s: string; r: Regex; caps: var seq[Capture];
                 start = 0): (int, int) =
  ## The bounds of the first match at or after `start`, both ends inclusive, or
  ## `(-1, 0)` when there is none. An empty match reports `last == first - 1`.
  if start < 0: return (-1, 0)
  var t = default(Threads)
  let (first, L) = search(s, r, caps, start, t)
  result = if first < 0: (-1, 0) else: (first, first + L - 1)

func findBounds*(s: string; r: Regex; start = 0): (int, int) =
  ## The bounds of the first match at or after `start`, both ends inclusive, or
  ## `(-1, 0)` when there is none.
  var caps: seq[Capture] = @[]
  result = findBounds(s, r, caps, start)

func find*(s: string; r: Regex; start = 0): int =
  ## The index of the first match at or after `start`, or `-1`.
  let (a, _) = findBounds(s, r, start)
  result = a

func contains*(s: string; r: Regex): bool =
  ## Does `s` contain a match of `r` anywhere? Written for `in`:
  ## `if pattern in line: …`.
  find(s, r) >= 0

func startsWith*(s: string; r: Regex): bool =
  ## Same as `match(s, r)`, spelled for readability.
  match(s, r)

func capture*(s: string; caps: seq[Capture]; i: int): string =
  ## The text of capture group `i` (0-based), or `""` when the group took no
  ## part in the match.
  result = ""
  if i >= 0 and i < caps.len:
    let c = caps[i]
    if c.last >= c.first and c.first >= 0 and c.last < s.len:
      for k in c.first .. c.last: result.add s[k]

func captureTexts*(s: string; caps: seq[Capture]): seq[string] =
  ## Every capture group's text, in group order.
  result = @[]
  for i in 0 ..< caps.len: result.add capture(s, caps, i)

iterator findAll*(s: string; r: Regex; start = 0): (int, int) =
  ## The bounds of every non-overlapping match, left to right, both ends
  ## inclusive. An empty match advances by one byte, so the loop always
  ## terminates.
  var caps: seq[Capture] = @[]
  var t = default(Threads)
  var i: Natural = max(start, 0)
  while i <= s.len:
    let (first, L) = search(s, r, caps, i, t)
    if first < 0: break
    yield (first, first + L - 1)
    i = first + max(L, 1)

func replace*(s: string; r: Regex; by: string): string =
  ## Every non-overlapping match of `r` replaced by `by`, which is inserted
  ## literally — there is no `$1` substitution.
  result = ""
  var caps: seq[Capture] = @[]
  var t = default(Threads)
  var i: Natural = 0
  while i < s.len:
    let (first, L) = search(s, r, caps, i, t)
    if first < 0 or first >= s.len: break
    while i < first:
      result.add s[i]
      inc i
    if L > 0:
      result.add by
      i = first + L
    else:
      result.add s[first]
      i = first + 1
  while i < s.len:
    result.add s[i]
    inc i

func split*(s: string; r: Regex): seq[string] =
  ## `s` split at every non-empty match of `r`. The separators are dropped and
  ## empty fields are kept, so the pieces and the separators together
  ## reconstruct `s`.
  result = @[]
  var caps: seq[Capture] = @[]
  var t = default(Threads)
  var piece = ""
  var i: Natural = 0
  while i < s.len:
    let (first, L) = search(s, r, caps, i, t)
    if first < 0 or first >= s.len: break
    while i < first:
      piece.add s[i]
      inc i
    if L > 0:
      result.add piece
      piece = ""
      i = first + L
    else:
      piece.add s[first]
      i = first + 1
  while i < s.len:
    piece.add s[i]
    inc i
  result.add piece

# ---------------------------------------------------------------------------
# The compile-time lexer
# ---------------------------------------------------------------------------

template lex*(s: string; pos: var int; sections: varargs[untyped]): untyped {.
    plugin: "deps/regex".}
  ## Matches the patterns of its `of` branches against `s` at `pos`, runs the
  ## branch that wins and leaves `pos` just past the match.
  ##
  ##   ```nim
  ##   var pos = 0
  ##   while pos < input.len:
  ##     let start = pos
  ##     lex input, pos:
  ##     of r"\d+": echo "an integer ", substr(input, start, pos-1)
  ##     of "else": echo "the ELSE keyword"
  ##     of r"[a-zA-Z_]\w*": echo "an identifier"
  ##     of r".": discard
  ##   ```
  ##
  ## Every pattern goes into **one** automaton, so a step does not get more
  ## expensive as branches are added: a hundred keywords cost what one costs.
  ##
  ## The longest match wins; between two patterns that match the same text, the
  ## branch written first wins — which is why a keyword has to be listed before
  ## the identifier pattern that also covers it. The scan runs on past an
  ## accepting state looking for something longer and rewinds when it fails, so
  ## a pattern that gets halfway and dies costs nothing.
  ##
  ## When nothing matches, `pos` does not move and no branch runs; an `else`
  ## branch catches that case. Give the last branch a catch-all pattern
  ## (`r"."`) when the loop around it relies on making progress.
  ##
  ## The patterns must be string literals, and they are compiled while your
  ## program is compiled: what reaches the binary is a `case` statement over the
  ## automaton's states, with no regex engine behind it.

template rematch*(s: string; sections: varargs[untyped]): untyped {.
    plugin: "deps/regex".}
  ## Runs the branch whose pattern matches **all** of `s`:
  ##
  ##   ```nim
  ##   rematch token:
  ##   of r"\d+":       echo "a number"
  ##   of r"[a-z]+":    echo "a word"
  ##   of r"\w+", r"-": echo "something else word-shaped"
  ##   else:            echo "no idea"
  ##   ```
  ##
  ## It is `lex`'s sibling for classifying a whole string rather than scanning
  ## one, and the compile-time counterpart of a chain of `fullMatch` tests: one
  ## automaton is built for all the branches together, so the cost of a
  ## classification does not grow with the number of alternatives.
  ##
  ## Between two patterns that both match, the branch written first wins. When
  ## none matches, the `else` branch runs — and when there is none, nothing
  ## does.
