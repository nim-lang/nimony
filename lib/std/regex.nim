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
## backtracking. That is what makes the worst case linear — there is no input
## that makes `(a+)+b` take exponential time here — and it is also what bounds
## what the module can express: a capture must lie on every accepting path
## (`(abc)|(xyz)` is rejected, `(abc|xyz)` is fine), and a pattern whose
## automaton would need more than 255 states is rejected rather than compiled.
##
## Two ways to use it:
##
## **At runtime** — build a `Regex` from a string and match against it:
##
##   ```nim
##   import std/regex
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

import std / private / regexcore

export RegexFlag, reExtended, reNoBackrefs, reNoCaptures
export Regex, Capture, RegexOpcode, RegexInstr
export opcRet, opcTestSet, opcTestChar, opcTJmp, opcBegin, opcEnd,
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
  ## error naming what is wrong with it, and no automaton is built at run time:
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
  ## diagnostic is something to show, not something to crash on.
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

func findBounds*(s: string; r: Regex; caps: var seq[Capture];
                 start = 0): (int, int) =
  ## The bounds of the first match at or after `start`, both ends inclusive, or
  ## `(-1, 0)` when there is none. An empty match reports `last == first - 1`.
  var i = start
  while i <= s.len:
    let L = matchLen(s, r, caps, i)
    if L >= 0: return (i, i + L - 1)
    inc i
  result = (-1, 0)

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
  ## inclusive. An empty match advances by one character, so the loop always
  ## terminates.
  var i = start
  while i <= s.len:
    let L = matchLen(s, r, i)
    if L > 0:
      yield (i, i + L - 1)
      inc i, L
    elif L == 0:
      yield (i, i - 1)
      inc i
    else:
      inc i

func replace*(s: string; r: Regex; by: string): string =
  ## Every non-overlapping match of `r` replaced by `by`, which is inserted
  ## literally — there is no `$1` substitution.
  result = ""
  var i = 0
  while i < s.len:
    let L = matchLen(s, r, i)
    if L > 0:
      result.add by
      inc i, L
    else:
      result.add s[i]
      inc i

func split*(s: string; r: Regex): seq[string] =
  ## `s` split at every non-empty match of `r`. The separators are dropped and
  ## empty fields are kept, so the pieces and the separators together
  ## reconstruct `s`.
  result = @[]
  var piece = ""
  var i = 0
  while i < s.len:
    let L = matchLen(s, r, i)
    if L > 0:
      result.add piece
      piece = ""
      inc i, L
    else:
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
