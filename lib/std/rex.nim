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
## The engine is a byte DFA: a pattern is turned into a deterministic automaton
## once, and matching then costs one table step per input byte with no
## backtracking. That is what makes the worst case linear — there is no input
## that makes `(a+)+b` take exponential time here — and it is also what bounds
## what the module can express: a capture must lie on every accepting path
## (`(abc)|(xyz)` is rejected, `(abc|xyz)` is fine). The automaton's size is
## not capped, and it can be exponential in the pattern's: `(a|b)*a(a|b){20}`
## needs two million states. Matching stays linear, but compiling a pattern
## from an untrusted source with `tryRe` can take unbounded time and memory.
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
## Searching
## =========
##
## `find`, `findBounds`, `findAll`, `replace` and `split` look for a match
## anywhere in the subject. Search advances candidate starts together and
## retains only the earliest start reaching each automaton state. For a fixed
## pattern, a search is linear in the subject even without a required literal.
## Its scratch space is proportional to the automaton, not the subject.
## Repeated searches (`findAll`, `replace`, `split`) can still rescan suffixes
## when a short successful match required long lookahead.
## A required literal, when available, also provides a fast absence check.
##
## This bound applies to regular patterns, including assertions and captures
## without backreferences. Backreferences depend on captured text and keep
## the register interpreter; searching with those can still be quadratic.
## Zero-width assertion cycles also retain the interpreter's behavior.
## `reUnanchored` remains accepted for source compatibility but is no longer
## needed: patterns such as `^\d+$` can be compiled normally. As before, `^`
## is relative to each attempted match, not to the beginning of the subject.
##
## Captures
## ========
##
## `(x)` records where the group matched; `matchLen`/`match`/`fullMatch` take a
## `var seq[Capture]` and `capture` reads the text back out:
##
##   ```nim
##   var caps: seq[Capture] = @[]
##   if match("key=value", re"(\w+)=(\w+)", caps):
##     echo capture("key=value", caps, 0)   # key
##     echo capture("key=value", caps, 1)   # value
##   ```
##
## A DFA is a poor place to track captures, and this one is honest about it
## rather than clever. Two limits follow:
##
## * A capture must lie on every accepting path, so `(abc)|(xyz)` is a
##   compile-time error. Write `(abc|xyz)`.
## * Otherwise the *match* is always right, but the capture bounds are
##   best-effort: for a group sitting next to something of variable length
##   (`(\w+)\s*=`) the engine cannot say where the group ended, and `caps`
##   comes back empty. `capture` then returns `""`. Check for it during
##   development rather than assuming; a fixed-shape pattern always works.
##
## Where captures are essential and the pattern is awkward, split it: match the
## overall shape with one regex and pull the pieces out with `split` or plain
## string operations.
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
##   `var openArray[string]`, and are subject to the limits described under
##   *Captures*.
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

export RegexFlag, reExtended, reNoBackrefs, reNoCaptures, reUnanchored
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
  ## error naming what is wrong with it. No automaton is built at run time:
  ## what reaches the binary is the finished byte-class transition table.
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
  ## diagnostic is something to show, not something to crash on.
  compileRegex(pattern, flags, dest, err, requireAnchor = true)

func isEmpty*(r: Regex): bool {.inline.} =
  ## True for a regex that never matches — what `tryRe` leaves behind when it
  ## fails.
  r.code.len == 0 and r.transitions.len == 0

func captureCount*(r: Regex): int {.inline.} =
  ## The number of capture groups in `r`.
  r.captures

# ---------------------------------------------------------------------------
# Matching
# ---------------------------------------------------------------------------

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
# The literal is an absence prefilter only. The automaton owns candidate
# merging and longest-match selection; there is no backwards prefix walk.

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

func search(s: string; r: Regex; caps: var seq[Capture];
            start: Natural): (int, int) =
  caps.setLen 0
  if start > s.len: return (-1, 0)
  # A singleton first byte already gets the same word-at-a-time scan when
  # seeding the automaton; do not scan twice for that same literal.
  if r.lit.len > 0 and (r.lit.len != 1 or r.firstByte != ord(r.lit[0])):
    if findLit(s, r.lit, start) < 0: return (-1, 0)
  if r.transitions.len > 0:
    # The first candidate often succeeds (tokenization, anchored inputs).
    # One scalar attempt avoids frontier allocation in that case. On failure
    # the merged search still bounds the whole operation by O(bytes * states).
    let first = skipToCandidate(s, r, start, s.len)
    let length = matchLen(s, r, caps, first)
    if length >= 0: return (first, length)
    result = searchTable(r, s, first + 1)
    if result[0] >= 0 and r.captures > 0:
      discard matchLen(s, r, caps, result[0])
  else:
    # Backreferences depend on captured text, so state equality alone cannot
    # merge these attempts. Keep their established register-VM semantics.
    var i: Natural = start
    while i <= s.len:
      i = skipToCandidate(s, r, i, s.len)
      let length = matchLen(s, r, caps, i)
      if length >= 0: return (i, length)
      inc i
    result = (-1, 0)

func findBounds*(s: string; r: Regex; caps: var seq[Capture];
                 start = 0): (int, int) =
  ## The bounds of the first match at or after `start`, both ends inclusive, or
  ## `(-1, 0)` when there is none. An empty match reports `last == first - 1`.
  if start < 0: return (-1, 0)
  let (first, L) = search(s, r, caps, start)
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
  var i: Natural = max(start, 0)
  while i <= s.len:
    let (first, L) = search(s, r, caps, i)
    if first < 0: break
    yield (first, first + L - 1)
    i = first + max(L, 1)

func replace*(s: string; r: Regex; by: string): string =
  ## Every non-overlapping match of `r` replaced by `by`, which is inserted
  ## literally — there is no `$1` substitution.
  result = ""
  var caps: seq[Capture] = @[]
  var i: Natural = 0
  while i < s.len:
    let (first, L) = search(s, r, caps, i)
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
  var piece = ""
  var i: Natural = 0
  while i < s.len:
    let (first, L) = search(s, r, caps, i)
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
