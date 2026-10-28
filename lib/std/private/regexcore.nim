#
#
#            Nimony's Standard Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Regular expression engine core: parser, NFA construction, subset
## construction and DFA minimization. Derived from
## [lexim](https://github.com/Araq/lexim).
##
## This module is shared by two very different consumers:
##
## * `std/rex` compiles a pattern at **runtime** and walks the resulting DFA
##   as bytecode;
## * `std/deps/regex`, the plugin behind `std/rex`'s `lex` construct, runs the
##   very same pipeline at **compile time** and emits the DFA as straight-line
##   Nimony code.
##
## Sharing the pipeline is the point: a pattern cannot mean one thing in a
## generated lexer and another at runtime, because there is only one
## implementation of what it means.
##
## Nothing in here raises. A malformed pattern sets `err` on the context and
## parsing unwinds to an epsilon node; every entry point hands that message
## back so the runtime API can turn it into an exception and the plugin into a
## compile-time diagnostic, each at its own source location.

{.feature: "lenientnils".}

import std / [intsets, tables, algorithm, unicode]

type
  RegexKind* = enum ## the regex AST's node kind
    reEps,          ## epsilon (matches the empty string)
    reChar,         ## a single character
    reStr,          ## a literal string
    reCClass,       ## a character class
    reStar,         ## `x*`
    rePlus,         ## `x+`
    reOpt,          ## `x?`
    reCat,          ## concatenation
    reAlt,          ## alternation `x|y`
    reCapture,      ## `(x)`
    reCaptureEnd,   ## never produced by the parser; the NFA's closing marker
    reBackref,      ## `\1`
    reBegin,        ## `\A` / `^`
    reEnd,          ## `\Z` / `$`
    reWordBoundary, ## `\b`
    reWordBoundaryNot ## `\B`

  RuneRange* = object ## the code points `a .. b`
    a*, b*: int

  RuneSet* = seq[RuneRange] ## sorted, disjoint and not adjacent

  RegexNode* = ref object ## a node of the parsed regular expression
    kind*: RegexKind
    a*, b*: RegexNode ## sub-expressions; which ones are used depends on `kind`
    r*: Rune          ## the code point for `reChar`
    c*: char          ## the index for `reCapture`/`reBackref`
    s*: string        ## the UTF-8 literal for `reStr`
    cc*: RuneSet      ## the code points for `reCClass`
    rule*: int        ## `> 0` marks a final state and names the rule that
                      ## matched there; `0` means "not final"

  RegexFlag* = enum ## how a pattern is parsed
    reExtended,     ## ignore unescaped spaces and tabs
    reNoBackrefs,   ## treat `\1` as a character literal, not a back reference
    reNoCaptures    ## `(x)` means the same as `(?:x)`

const
  wordChars* = {'A'..'Z', 'a'..'z', '0'..'9', '_', '\128'..'\255'}
    ## the bytes `\b` treats as part of a word; every non-ASCII byte is one
  maxRune = 0x10FFFF

# ---------------------------------------------------------------------------
# AST construction
# ---------------------------------------------------------------------------

proc newNode(kind: RegexKind): RegexNode =
  result = RegexNode(kind: kind, a: nil, b: nil, r: Rune(0), c: '\0', s: "",
                     cc: @[], rule: 0)

proc epsExpr*(): RegexNode =
  result = newNode(reEps)

proc charExpr*(r: Rune): RegexNode =
  result = newNode(reChar)
  result.r = r

proc charExpr*(c: char): RegexNode = charExpr(Rune(ord(c)))

proc backrefExpr*(x: int): RegexNode =
  result = newNode(reBackref)
  result.c = char(x)

proc strExpr*(str: string): RegexNode =
  if str.len == 1:
    result = charExpr(str[0])
  else:
    result = newNode(reStr)
    result.s = str

proc cclassExpr*(cc: RuneSet): RegexNode =
  result = newNode(reCClass)
  result.cc = cc

proc starExpr*(r: RegexNode): RegexNode =
  if r.kind == reStar:
    result = r
  else:
    result = newNode(reStar)
    result.a = r

proc plusExpr*(r: RegexNode): RegexNode =
  result = newNode(rePlus)
  result.a = r

proc optExpr*(r: RegexNode): RegexNode =
  result = newNode(reOpt)
  result.a = r

proc catExpr*(a, b: RegexNode): RegexNode =
  result = newNode(reCat)
  result.a = a
  result.b = b

proc altExpr*(a, b: RegexNode): RegexNode =
  result = newNode(reAlt)
  result.a = a
  result.b = b

proc captureExpr*(a: RegexNode): RegexNode =
  result = newNode(reCapture)
  result.a = a

proc mnExpr(r: RegexNode; m, n: int): RegexNode =
  ## `r{m,n}`, expanded to `r^m | r^(m+1) | … | r^n`.
  var ri: RegexNode = nil
  if m > n or n == 0:
    result = epsExpr()
  else:
    if m == 0:
      ri = epsExpr()
    else:
      ri = r
      for i in 2 .. m: ri = catExpr(ri, r)
    result = ri
    for i in m + 1 .. n:
      if ri.kind == reEps: ri = r
      else: ri = catExpr(ri, r)
      result = altExpr(result, ri)

# ---------------------------------------------------------------------------
# Code-point sets
# ---------------------------------------------------------------------------

proc incl(s: var RuneSet; lo, hi: int) =
  ## Adds `lo .. hi`, merging it with the ranges it overlaps or touches.
  var i = 0
  while i < s.len and s[i].b + 1 < lo: inc i
  if i == s.len or hi + 1 < s[i].a:
    s.add RuneRange(a: lo, b: hi)
    var j = s.len - 1
    while j > i:
      s[j] = s[j - 1]
      dec j
    s[i] = RuneRange(a: lo, b: hi)
  else:
    s[i].a = min(s[i].a, lo)
    s[i].b = max(s[i].b, hi)
    while i + 1 < s.len and s[i + 1].a <= s[i].b + 1:
      s[i].b = max(s[i].b, s[i + 1].b)
      s.delete i + 1

proc incl(s: var RuneSet; other: RuneSet) =
  for r in other: s.incl r.a, r.b

proc complement(s: RuneSet): RuneSet =
  ## Every code point but `\0` that is not in `s`.
  result = @[]
  var next = 1
  for r in s:
    if next < r.a: result.add RuneRange(a: next, b: r.a - 1)
    next = r.b + 1
  if next <= maxRune: result.add RuneRange(a: next, b: maxRune)

proc namedClass(ch: char): RuneSet =
  ## `\d`, `\s` or `\w`.
  result = @[]
  case ch
  of 'd':
    result.incl ord('0'), ord('9')
  of 's':
    result.incl 1, 32
  else:
    result.incl ord('0'), ord('9')
    result.incl ord('A'), ord('Z')
    result.incl ord('_'), ord('_')
    result.incl ord('a'), ord('z')
    result.incl 0x80, maxRune

# ---------------------------------------------------------------------------
# Parser
# ---------------------------------------------------------------------------

type
  ReCtx = object
    buf: string
    pos: Natural   ## `Natural`, so `0 <= pos` is a property of the *type*: the
                   ## scanner indexes `buf[pos]` all over, and that half of the
                   ## bounds obligation then needs no guard anywhere.
    flags: set[RegexFlag]
    captures: int ## running count, so each `(` gets its own index
    err: string   ## first error; `""` while everything is fine

proc error(c: var ReCtx; msg: string) =
  if c.err.len == 0: c.err = msg

proc atEnd(c: ReCtx): bool {.inline.} = c.pos >= c.buf.len

proc skipSpaces(c: var ReCtx) {.inline.} =
  if reExtended in c.flags:
    while c.pos < c.buf.len and (c.buf[c.pos] == ' ' or c.buf[c.pos] == '\t'):
      inc c.pos

proc getNext(c: var ReCtx): char =
  ## The next significant character, or `'\0'` at the end of the pattern.
  skipSpaces c
  result = if c.pos < c.buf.len: c.buf[c.pos] else: '\0'

proc readRune(c: var ReCtx): RegexNode {.inline.} =
  ## The pattern is valid UTF-8 (`parseRegExpr` checks it up front).
  if c.buf[c.pos] < '\x80':
    result = charExpr(c.buf[c.pos])
    inc c.pos
  else:
    result = charExpr(runeAt(c.buf, c.pos))
    inc c.pos, runeLenAt(c.buf, c.pos)

proc getChar(c: var ReCtx; inClass: bool): RegexNode =
  ## One character or escape sequence. `inClass` selects the `[...]` reading of
  ## the escapes that mean different things there (`\b` is a backspace inside a
  ## class and a word boundary outside it).
  if not inClass:
    skipSpaces c
  if c.pos < c.buf.len and c.buf[c.pos] != '\\':
    result = readRune(c)
  else:
    let ch = if c.pos + 1 < c.buf.len: c.buf[c.pos + 1] else: '\0'
    case ch
    of 'n':
      # Outside a class, `\n` is a newline in any of its three spellings,
      # longest first. Inside one it can only be a character, and a class is
      # exactly where `[^\n]` wants it, so there it is the line feed alone.
      result =
        if inClass: charExpr('\L')
        else: altExpr(altExpr(strExpr("\r\n"), charExpr('\L')), charExpr('\r'))
      inc c.pos, 2
    of 'r', 'C':
      result = charExpr('\r')
      inc c.pos, 2
    of 'l', 'L':
      result = charExpr('\L')
      inc c.pos, 2
    of 't':
      result = charExpr('\t')
      inc c.pos, 2
    of 'b':
      result = if inClass: charExpr('\b') else: newNode(reWordBoundary)
      inc c.pos, 2
    of 'B':
      result = if inClass: charExpr('\b') else: newNode(reWordBoundaryNot)
      inc c.pos, 2
    of 'e':
      result = charExpr('\e')
      inc c.pos, 2
    of 'a', 'A':
      result = if inClass: charExpr('\a') else: newNode(reBegin)
      inc c.pos, 2
    of 'v':
      result = charExpr('\v')
      inc c.pos, 2
    of 'f':
      result = charExpr('\f')
      inc c.pos, 2
    of 'z', 'Z':
      if inClass:
        error c, "'\\Z' is not supported in a character class"
        result = epsExpr()
      else:
        result = newNode(reEnd)
      inc c.pos, 2
    of 's', 'd', 'w':
      result = cclassExpr(namedClass(ch))
      inc c.pos, 2
    of 'S', 'D', 'W':
      result = cclassExpr(complement(namedClass(char(ord(ch) + 32))))
      inc c.pos, 2
    of '0'..'9':
      let startsWithZero = ch == '0'
      var val = ord(ch) - ord('0')
      inc c.pos, 2
      var i = 1
      while i <= 4 and c.pos < c.buf.len and
            c.buf[c.pos] >= '0' and c.buf[c.pos] <= '9':
        val = val * 10 + ord(c.buf[c.pos]) - ord('0')
        inc c.pos
        inc i
      if startsWithZero or reNoBackrefs in c.flags:
        if val > 255:
          error c, "character code out of range: " & $val
          result = epsExpr()
        else:
          result = charExpr(Rune(val))
      else:
        result = backrefExpr(val)
    of '\0':
      error c, "'\\' at the end of the pattern"
      result = epsExpr()
    else:
      if ch < ' ':
        error c, "invalid character after '\\': #" & $ord(ch)
        result = epsExpr()
      else:
        inc c.pos
        result = readRune(c)

proc parseStr(c: var ReCtx): RegexNode =
  ## `"..."` — a literal run in which the regex operators lose their meaning.
  var s = ""
  inc c.pos # skip the opening quote
  while c.pos < c.buf.len and c.buf[c.pos] != '\"':
    if c.buf[c.pos] == '\r' or c.buf[c.pos] == '\L':
      error c, "'\"' expected"
      return epsExpr()
    let al = getChar(c, false)
    if al.kind == reChar:
      s.add al.r
    else:
      error c, "only single characters are allowed in a quoted string"
      return epsExpr()
  if c.pos < c.buf.len:
    inc c.pos # skip the closing quote
  else:
    error c, "'\"' expected"
  result = strExpr(s)

proc parseCClass(c: var ReCtx): RegexNode =
  var cc: RuneSet = @[]
  inc c.pos # skip '['
  var caret = false
  if c.pos < c.buf.len and c.buf[c.pos] == '^':
    caret = true
    inc c.pos
  while c.pos < c.buf.len and c.buf[c.pos] != ']':
    if c.buf[c.pos] == '\r' or c.buf[c.pos] == '\L':
      error c, "']' expected"
      return epsExpr()
    let a = getChar(c, true)
    if c.err.len > 0: return epsExpr()
    if a.kind == reChar:
      cc.incl int(a.r), int(a.r)
      if c.pos < c.buf.len and c.buf[c.pos] == '-':
        inc c.pos
        if c.pos < c.buf.len and c.buf[c.pos] == ']':
          cc.incl ord('-'), ord('-')
          break
        let b = getChar(c, true)
        if c.err.len > 0: return epsExpr()
        if b.kind == reChar:
          if a.r <=% b.r:
            cc.incl int(a.r), int(b.r)
          else:
            error c, "invalid character range in '[...]'"
            return epsExpr()
        elif b.kind == reCClass:
          cc.incl ord('-'), ord('-')
          cc.incl b.cc
        else:
          error c, "invalid character range in '[...]'"
          return epsExpr()
    elif a.kind == reCClass:
      cc.incl a.cc
    else:
      error c, "'" & c.buf[c.pos] & "' cannot be part of a character class"
      return epsExpr()
  if c.pos < c.buf.len and c.buf[c.pos] == ']':
    inc c.pos
  else:
    error c, "']' expected"
    return epsExpr()
  result = cclassExpr(if caret: complement(cc) else: cc)

proc parseNum(c: var ReCtx): int =
  result = 0
  if c.pos < c.buf.len and c.buf[c.pos] >= '0' and c.buf[c.pos] <= '9':
    while c.pos < c.buf.len and c.buf[c.pos] >= '0' and c.buf[c.pos] <= '9':
      result = result * 10 + ord(c.buf[c.pos]) - ord('0')
      inc c.pos
  else:
    error c, "number expected"

proc parseRegExprImpl(c: var ReCtx): RegexNode

proc factor(c: var ReCtx): RegexNode =
  case getNext(c)
  of '\"':
    result = parseStr(c)
  of '[':
    result = parseCClass(c)
  of '.':
    inc c.pos
    result = cclassExpr(complement(@[]))
  of '(':
    inc c.pos
    var isCapture = reNoCaptures notin c.flags
    if c.pos + 1 < c.buf.len and c.buf[c.pos] == '?' and c.buf[c.pos+1] == ':':
      inc c.pos, 2
      isCapture = false
    result = parseRegExprImpl(c)
    if getNext(c) == ')':
      inc c.pos
    else:
      error c, "')' expected"
      return epsExpr()
    if isCapture:
      inc c.captures
      if c.captures > 255:
        error c, "too many capture groups"
        return epsExpr()
      result = captureExpr(result)
      result.c = char(c.captures)
  of '\\':
    result = getChar(c, false)
  of '{':
    error c, "regex macros ('{NAME}') are not supported"
    result = epsExpr()
  of '*', '+', '?':
    error c, "escape '" & c.buf[c.pos] & "' with '\\'"
    result = epsExpr()
  of '$':
    inc c.pos
    result = newNode(reEnd)
  of '^':
    inc c.pos
    result = newNode(reBegin)
  else:
    result = readRune(c)
  if c.err.len > 0: return result
  # postfix operators bind tighter than concatenation:
  var running = true
  while running:
    case getNext(c)
    of '*':
      inc c.pos
      result = starExpr(result)
    of '+':
      inc c.pos
      result = plusExpr(result)
    of '?':
      inc c.pos
      result = optExpr(result)
    of '{':
      inc c.pos
      var m = parseNum(c)
      if c.err.len > 0: return result
      var n = m
      if getNext(c) == ',':
        inc c.pos
        skipSpaces c
        n = parseNum(c)
        if c.err.len > 0: return result
      result = mnExpr(result, m, n)
      if getNext(c) == '}':
        inc c.pos
      else:
        error c, "'}' expected"
        return result
    else:
      running = false

proc term(c: var ReCtx): RegexNode =
  proc isTermDelim(ch: char): bool {.inline.} =
    ch == '\0' or ch == ':' or ch == '|' or ch == ')'

  if not isTermDelim(getNext(c)):
    result = factor(c)
    while c.err.len == 0 and not isTermDelim(getNext(c)):
      result = catExpr(result, factor(c))
  else:
    result = epsExpr()

proc parseRegExprImpl(c: var ReCtx): RegexNode =
  result = term(c)
  while c.err.len == 0 and getNext(c) == '|':
    inc c.pos
    result = altExpr(result, term(c))

proc parseRegExpr*(pattern: string; flags: set[RegexFlag];
                   err: var string): RegexNode =
  ## Parses `pattern`. On failure `err` describes what went wrong and the
  ## result is an epsilon node, so callers may keep walking the tree without a
  ## nil check as long as they test `err` before using the result.
  var c = ReCtx(buf: pattern, pos: 0, flags: flags, captures: 0, err: "")
  if validateUtf8(pattern) >= 0:
    err = "invalid UTF-8 in regular expression"
    return epsExpr()
  result = parseRegExprImpl(c)
  if c.err.len == 0 and not atEnd(c):
    error c, "unexpected '" & c.buf[c.pos] & "' in regular expression"
  err = c.err

proc containsInvCap(r: RegexNode; inAlt: bool): bool =
  if r == nil:
    result = false
  else:
    result = containsInvCap(r.a, inAlt or r.kind == reAlt) or
             containsInvCap(r.b, inAlt or r.kind == reAlt) or
             (r.kind == reCapture and inAlt)

proc containsInvalidCapture*(r: RegexNode): bool =
  ## A DFA can only track a capture that every accepting path runs through, so
  ## `(abc)|(xyz)` is not expressible. Callers use this to say so up front
  ## instead of returning wrong capture bounds.
  result = containsInvCap(r, false)


# ---------------------------------------------------------------------------
# NFA / DFA
# ---------------------------------------------------------------------------
#
# Nothing below bounds the number of states. A state is an index into `seq`s
# grown as states are made, and a *set* of states is an `IntSet`, which is only
# ever iterated or probed for the members at hand -- no loop walks every state
# number asking whether it is in some set.

type
  Alphabet* = object
    ## A transition's condition as the DFA's consumers see it: a byte, or one
    ## of the zero-width assertions and capture markers that ride the same
    ## transition machinery.
    kind*: RegexKind
    val*: char

  Label = int32 ## a state number
  LabelSet = IntSet

  NfaEdge = object
    cond: Alphabet ## a marker, or `reChar` for the bytes `cond.val .. hi`
    hi: char
    dest: Label

  Nfa = object
    captures, backrefs, stateCount: int ## `0` is the start state
    eps: seq[seq[Label]]     ## per state: where its epsilon edges lead
    trans: seq[seq[NfaEdge]] ## per state: every other edge
    toRules: seq[int]

  Letters = object
    ## The DFA's alphabet. Letter `k < classCount` is the byte class
    ## `firstByte[k] ..< firstByte[k + 1]`; the markers come after the classes.
    firstByte: seq[int] ## ascending, from 0, and ended by 256
    markers: seq[Alphabet]

  DfaEdge = object
    letter: int
    dest: Label

  Dfa* = object
    startState*: int ## not always 1 before minimization
    stateCount*: int ## states are `1 .. stateCount`
    captures*, backrefs*: int
    ruleCount*: int  ## highest rule number; rule 0 means "no match"
    letters: Letters
    trans: seq[seq[DfaEdge]] ## indexed by state; no edge means "no match"
    toRules: seq[int]

func `==`(a, b: Alphabet): bool {.inline.} =
  a.kind == b.kind and a.val == b.val

func cmpInt(a, b: int): int = a - b

proc grow(a: var Nfa; s: int) {.inline.} =
  if a.trans.len <= s:
    a.trans.setLen s + 1
    a.eps.setLen s + 1

proc eps(a: var Nfa; s, d: int) =
  grow a, s
  a.eps[s].add Label(d)

proc trans(a: var Nfa; s: int; c: Alphabet; d: int) =
  grow a, s
  a.trans[s].add NfaEdge(cond: c, hi: c.val, dest: Label(d))

proc transBytes(a: var Nfa; s: int; lo, hi: char; d: int) =
  grow a, s
  a.trans[s].add NfaEdge(cond: Alphabet(kind: reChar, val: lo), hi: hi,
                         dest: Label(d))

type
  Utf8Seq = object
    ## Code points whose UTF-8 encodings are `len` bytes, the i-th of them in
    ## `lo[i] .. hi[i]`.
    len: int
    lo, hi: array[4, char]

proc utf8Sequences(dest: var seq[Utf8Seq]; lo, hi: int) =
  ## Splits `lo .. hi` until every piece is a product of byte ranges: no
  ## surrogates, one encoding width, and every byte after the first one that
  ## differs spans the full continuation range `\x80 .. \xBF`.
  if lo > hi: return
  if lo <= 0xDFFF and hi >= 0xD800:
    utf8Sequences(dest, lo, 0xD7FF)
    utf8Sequences(dest, 0xE000, hi)
    return
  for top in [0x7F, 0x7FF, 0xFFFF]:
    if lo <= top and top < hi:
      utf8Sequences(dest, lo, top)
      utf8Sequences(dest, top + 1, hi)
      return
  if hi > 0x7F:
    for i in 1 .. 3:
      let m = (1 shl (6 * i)) - 1
      if (lo and not m) != (hi and not m):
        if (lo and m) != 0:
          utf8Sequences(dest, lo, lo or m)
          utf8Sequences(dest, (lo or m) + 1, hi)
          return
        if (hi and m) != m:
          utf8Sequences(dest, lo, (hi and not m) - 1)
          utf8Sequences(dest, hi and not m, hi)
          return
  let a = toUTF8(Rune(lo))
  let b = toUTF8(Rune(hi))
  var sq = Utf8Seq(len: a.len)
  for i in 0 ..< a.len:
    sq.lo[i] = a[i]
    sq.hi[i] = b[i]
  dest.add sq

proc auxRegExprToNfa(r: RegexNode; a: var Nfa; currState: int): int =
  ## Thompson's construction; returns the state the sub-expression ends in.
  result = currState
  if r == nil: return
  case r.kind
  of reEps:
    eps a, result, result + 1
    inc result
  of reChar, reStr:
    let bytes = if r.kind == reChar: toUTF8(r.r) else: r.s
    for i in 0 ..< bytes.len:
      transBytes a, result, bytes[i], bytes[i], result + 1
      inc result
  of reWordBoundary, reWordBoundaryNot, reBegin, reEnd:
    trans a, result, Alphabet(kind: r.kind, val: '\0'), result + 1
    inc result
  of reCat:
    result = auxRegExprToNfa(r.a, a, result)
    result = auxRegExprToNfa(r.b, a, result)
  of reCClass:
    # The one place code points become bytes: every UTF-8 sequence of the
    # class runs from `entry` to `exit`, through states of its own.
    var seqs: seq[Utf8Seq] = @[]
    for i in 0 ..< r.cc.len: utf8Sequences(seqs, r.cc[i].a, r.cc[i].b)
    let entry = result + 1
    var next = entry + 1
    for sq in seqs: inc next, sq.len - 1
    let exit = next
    eps a, result, entry
    next = entry + 1
    for sq in seqs:
      var src = entry
      for i in 0 ..< sq.len - 1:
        transBytes a, src, sq.lo[i], sq.hi[i], next
        src = next
        inc next
      transBytes a, src, sq.lo[sq.len - 1], sq.hi[sq.len - 1], exit
    result = exit
  of reStar:
    # one transition too many is drawn here, which is harmless
    let aa = auxRegExprToNfa(r.a, a, result)
    eps a, result, aa + 1
    eps a, aa, aa + 1
    eps a, aa + 1, result
    result = aa + 1
  of rePlus:
    result = auxRegExprToNfa(catExpr(r.a, starExpr(r.a)), a, result)
  of reOpt:
    result = auxRegExprToNfa(altExpr(r.a, epsExpr()), a, result)
  of reAlt:
    eps a, result, result + 1
    inc result
    let oldState = result
    let aa = auxRegExprToNfa(r.a, a, result)
    let bb = auxRegExprToNfa(r.b, a, aa + 1)
    eps a, oldState, aa + 1
    eps a, aa, bb + 1
    eps a, bb, bb + 1
    result = bb + 1
  of reCapture, reCaptureEnd:
    a.captures = max(a.captures, int(r.c))
    trans a, result, Alphabet(kind: reCapture, val: r.c), result + 1
    inc result
    result = auxRegExprToNfa(r.a, a, result)
    trans a, result, Alphabet(kind: reCaptureEnd, val: r.c), result + 1
    inc result
  of reBackref:
    a.backrefs = max(a.backrefs, int(r.c))
    trans a, result, Alphabet(kind: reBackref, val: r.c), result + 1
    inc result
  if r.rule != 0:
    if a.toRules.len <= result: a.toRules.setLen result + 1
    a.toRules[result] = r.rule

proc regExprToNfa(r: RegexNode): Nfa =
  result = default(Nfa)
  result.stateCount = auxRegExprToNfa(r, result, 0)
  # states that only ever receive edges, or carry no rule, still get a slot
  grow result, result.stateCount
  result.toRules.setLen result.stateCount + 1

# ---------------------------------------------------------------------------
# The DFA's alphabet
# ---------------------------------------------------------------------------

proc byteClasses(a: Nfa; classOf: var array[256, int]): seq[int] =
  ## Cuts the bytes into classes that no NFA edge tells apart: a range that an
  ## edge reads starts a class at its first byte and another one after its
  ## last. Returns the first byte of every class, followed by 256.
  var starts: set[char] = {'\0'}
  for s in 0 ..< a.trans.len:
    for e in a.trans[s]:
      if e.cond.kind == reChar:
        starts.incl e.cond.val
        if e.hi < '\xFF': starts.incl succ(e.hi)
  result = @[]
  for b in 0 .. 255:  # in `int`: `inc` on a `char` wraps round at `high(char)`
    if char(b) in starts: result.add b
    classOf[b] = result.len - 1
  result.add 256

proc markers(captures, backrefs: int): seq[Alphabet] =
  ## Every marker a pattern with these groups can have, in the order the
  ## bytecode tests them.
  result = @[]
  for x in 1 .. backrefs:
    result.add Alphabet(kind: reBackref, val: char(x))
  for x in 1 .. captures:
    result.add Alphabet(kind: reCapture, val: char(x))
    result.add Alphabet(kind: reCaptureEnd, val: char(x))
  result.add Alphabet(kind: reBegin, val: '\0')
  result.add Alphabet(kind: reEnd, val: '\0')
  result.add Alphabet(kind: reWordBoundary, val: '\0')
  result.add Alphabet(kind: reWordBoundaryNot, val: '\0')

func classCount(l: Letters): int {.inline.} = l.firstByte.len - 1

func letterCount(l: Letters): int {.inline.} = classCount(l) + l.markers.len

func markerLetter(l: Letters; c: Alphabet): int =
  result = classCount(l)
  while result < letterCount(l) and l.markers[result - classCount(l)] != c:
    inc result

func classBytes(l: Letters; k: int): set[char] {.inline.} =
  {char(l.firstByte[k]) .. char(l.firstByte[k + 1] - 1)}

# ---------------------------------------------------------------------------
# Subset construction and minimization
# ---------------------------------------------------------------------------

proc closure(a: Nfa; s: var LabelSet; stack: var seq[Label]) =
  ## Grows `s` to its epsilon closure.
  stack.setLen 0
  for l in s: stack.add Label(l)
  while stack.len > 0:
    let l = stack.pop()
    for d in a.eps[l]:
      if not containsOrIncl(s, int(d)): stack.add d

type
  SubsetBuilder = object
    ## The subset construction's scratch space. `targets[c]` collects where
    ## letter `c` leads out of the DFA state being expanded; `letters` lists
    ## the `c` that got anything, so resetting costs what was used.
    states: seq[LabelSet]        ## DFA state `j` is the NFA state set `states[j]`
    known: Table[LabelSet, int]  ## and `known` finds `j` again from the set
    targets: seq[LabelSet]
    hasLetter: seq[bool]
    letters: seq[int]
    stack: seq[Label]

proc collectTargets(sb: var SubsetBuilder; a: Nfa; letters: Letters;
                    classOf: array[256, int]; j: int) =
  ## Fills `sb.targets` with every non-epsilon edge out of DFA state `j`, and
  ## `sb.letters` with their letters in ascending order.
  for l in sb.states[j]:
    for e in a.trans[l]:
      var lo = 0
      var hi = 0
      if e.cond.kind == reChar:
        lo = classOf[ord(e.cond.val)]
        hi = classOf[ord(e.hi)]
      else:
        lo = markerLetter(letters, e.cond)
        hi = lo
      for c in lo .. hi:
        if not sb.hasLetter[c]:
          sb.hasLetter[c] = true
          sb.letters.add c
        sb.targets[c].incl int(e.dest)
  sort sb.letters, cmpInt

proc nfaToDfa(a: Nfa; letters: Letters; classOf: array[256, int]): Dfa =
  ## Subset construction (see "Modern Compiler Implementation"). State `0` is
  ## the empty set, "no match"; an edge into it is not recorded at all.
  result = default(Dfa)
  let n = letterCount(letters)
  var sb = SubsetBuilder(states: @[initIntSet()],
                         known: initTable[LabelSet, int](),
                         targets: newSeq[LabelSet](n),
                         hasLetter: newSeq[bool](n),
                         letters: @[], stack: @[])
  var start = initIntSet()
  start.incl 0 # the NFA's start state
  closure(a, start, sb.stack)
  sb.known[start] = 1
  sb.states.add start
  result.trans = newSeq[seq[DfaEdge]](2)
  var j = 1
  while j < sb.states.len:
    collectTargets(sb, a, letters, classOf, j)
    for c in sb.letters:
      var e = move sb.targets[c]
      sb.targets[c] = initIntSet()
      sb.hasLetter[c] = false
      closure(a, e, sb.stack)
      var i = sb.known.getOrDefault(e, 0)
      if i == 0:
        i = sb.states.len
        sb.known[e] = i
        sb.states.add e
        result.trans.add @[]
      result.trans[j].add DfaEdge(letter: c, dest: Label(i))
    sb.letters.setLen 0
    inc j
  result.toRules = newSeq[int](sb.states.len)
  for d in 1 ..< sb.states.len:
    var minRule = high(int)
    for l in sb.states[d]:
      let r = a.toRules[l]
      if r != 0 and r < minRule: minRule = r
    if minRule != high(int):
      result.toRules[d] = minRule
      if minRule > result.ruleCount: result.ruleCount = minRule
  result.stateCount = sb.states.len - 1
  result.startState = 1
  result.captures = a.captures
  result.backrefs = a.backrefs
  result.letters = letters

type
  PredEdge = object
    letter: int
    src: Label

  Refiner = object
    ## Hopcroft's partition refinement over a DFA's states `1 .. stateCount`.
    ## A block is a list of its states in ascending order (every split keeps
    ## that order), so its smallest state — the one whose transitions stand
    ## for the block — is always `blocks[b][0]`.
    blocks: seq[seq[Label]]
    blockOf: seq[int]
    work: seq[int]         ## blocks still to split by, popped from the end
    inWork: seq[bool]
    preds: seq[seq[PredEdge]] ## the edges into each state
    inv: seq[LabelSet]     ## per letter: the states that reach the splitter
    hasLetter: seq[bool]
    letters: seq[int]
    hits: seq[int]         ## per block: how many of its states are in `inv`
    hitBlocks: seq[int]

proc splitBy(rf: var Refiner; inv: LabelSet) =
  ## Splits every block that `inv` cuts into the part inside and the part
  ## outside. Blocks are split highest first and each outside part is appended,
  ## which is the order the block numbers of the result follow.
  for m in inv:
    let b = rf.blockOf[m]
    if rf.hits[b] == 0: rf.hitBlocks.add b
    inc rf.hits[b]
  sort rf.hitBlocks, cmpInt, SortOrder.Descending
  for b in rf.hitBlocks:
    if rf.hits[b] < rf.blocks[b].len:
      var x: seq[Label] = @[]
      var y: seq[Label] = @[]
      for m in rf.blocks[b]:
        if int(m) in inv: x.add m
        else: y.add m
      let k = rf.blocks.len
      for m in y: rf.blockOf[m] = k
      let smaller = if x.len <= y.len: b else: k
      rf.blocks[b] = x
      rf.blocks.add y
      rf.hits.add 0
      rf.inWork.add false
      if rf.inWork[b]:
        # `b` is still to be split by; both halves now have to be
        rf.work.add k
        rf.inWork[k] = true
      else:
        rf.work.add smaller
        rf.inWork[smaller] = true
    rf.hits[b] = 0
  rf.hitBlocks.setLen 0

proc optimizeDfa(a: Dfa): Dfa =
  ## Hopcroft's algorithm. Every state carries the rule it accepts, so the
  ## initial partition is by rule rather than the usual final/non-final split.
  let n = letterCount(a.letters)
  var rf = Refiner(blocks: newSeq[seq[Label]](a.ruleCount + 1),
                   blockOf: newSeq[int](a.stateCount + 1),
                   work: @[], inWork: @[],
                   preds: newSeq[seq[PredEdge]](a.stateCount + 1),
                   inv: newSeq[LabelSet](n),
                   hasLetter: newSeq[bool](n),
                   letters: @[], hits: newSeq[int](a.ruleCount + 1),
                   hitBlocks: @[])
  for d in 1 .. a.stateCount:
    let r = a.toRules[d]
    rf.blocks[r].add Label(d)
    rf.blockOf[d] = r
    for e in a.trans[d]:
      rf.preds[e.dest].add PredEdge(letter: e.letter, src: Label(d))
  for b in 0 .. a.ruleCount:
    rf.work.add b
    rf.inWork.add true
  while rf.work.len > 0:
    let s = rf.work.pop()
    rf.inWork[s] = false
    for t in rf.blocks[s]:
      for e in rf.preds[t]:
        if not rf.hasLetter[e.letter]:
          rf.hasLetter[e.letter] = true
          rf.letters.add e.letter
        rf.inv[e.letter].incl int(e.src)
    sort rf.letters, cmpInt
    for i in 0 ..< rf.letters.len:
      let c = rf.letters[i]
      let inv = move rf.inv[c]
      rf.inv[c] = initIntSet()
      rf.hasLetter[c] = false
      splitBy rf, inv
    rf.letters.setLen 0

  result = default(Dfa)
  result.captures = a.captures
  result.backrefs = a.backrefs
  result.stateCount = rf.blocks.len
  result.ruleCount = a.ruleCount
  result.letters = a.letters
  result.trans = newSeq[seq[DfaEdge]](rf.blocks.len + 1)
  result.toRules = newSeq[int](rf.blocks.len + 1)
  result.startState = rf.blockOf[a.startState] + 1
  for b in 0 ..< rf.blocks.len:
    if rf.blocks[b].len > 0:
      let rep = rf.blocks[b][0]
      result.toRules[b + 1] = a.toRules[rep]
      for e in a.trans[rep]:
        result.trans[b + 1].add DfaEdge(letter: e.letter,
                                        dest: Label(rf.blockOf[e.dest] + 1))

# ---------------------------------------------------------------------------
# Reading the DFA
# ---------------------------------------------------------------------------

func allTransitions*(a: Dfa; source, dest: int): (seq[Alphabet], set[char]) =
  ## Splits the `source -> dest` edges into the assertions and markers (which
  ## have to be tested one by one) and the plain characters (which collapse
  ## into one set test). A single character is handed back as an `Alphabet`
  ## too, because `x == 'a'` beats `x in {'a'}` in the generated code.
  var others: seq[Alphabet] = @[]
  var cs: set[char] = {}
  var card = 0
  for x in a.trans[source]:
    if x.dest == dest:
      if x.letter < classCount(a.letters):
        cs = cs + classBytes(a.letters, x.letter)
        inc card, a.letters.firstByte[x.letter + 1] - a.letters.firstByte[x.letter]
      else:
        others.add a.letters.markers[x.letter - classCount(a.letters)]
  if card == 1:
    for b in 0 .. 255:
      if char(b) in cs: others.add Alphabet(kind: reChar, val: char(b))
    cs = {}
  result = (others, cs)

iterator allDests*(a: Dfa; source: int): int =
  ## Every state reachable from `source`, each yielded once and in state order.
  var dests: seq[int] = @[]
  for x in a.trans[source]:
    # insertion into a sorted list: a state's edges fan out to a handful of
    # distinct states, and `algorithm.sort` would cost this iterator its
    # `func`-callability
    let d = int(x.dest)
    var i = dests.len
    while i > 0 and dests[i - 1] > d: dec i
    if i == 0 or dests[i - 1] != d:
      dests.add d
      var k = dests.len - 1
      while k > i:
        let prev = dests[k - 1]
        dests[k] = prev
        dec k
      dests[i] = d
  for d in dests: yield d

func getRule*(a: Dfa; s: int): int {.inline.} = a.toRules[s]

func firstBytes(a: Dfa): set[char] =
  ## Every byte a match can start with: all of them when a match can start
  ## without reading one, because the start state accepts or leaves on an
  ## anchor or a group marker.
  result = {'\0'..'\xFF'}
  if getRule(a, a.startState) != 0: return
  var first: set[char] = {}
  for e in a.trans[a.startState]:
    if e.letter >= classCount(a.letters): return
    first = first + classBytes(a.letters, e.letter)
  result = first

# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------

proc buildDfa(big: RegexNode): Dfa =
  let n = regExprToNfa(big)
  var classOf = default(array[256, int])
  let letters = Letters(firstByte: byteClasses(n, classOf),
                        markers: markers(n.captures, n.backrefs))
  result = optimizeDfa(nfaToDfa(n, letters, classOf))

proc rulesToDfa*(patterns: openArray[string]; flags: set[RegexFlag];
                 dfa: var Dfa; err: var string) =
  ## The whole pipeline: parse every pattern, tag it with its rule number,
  ## alternate them into one expression and run that through NFA → DFA →
  ## minimization. Rule numbers are 1-based and follow `patterns`' order, which
  ## is what makes "the earlier pattern wins a tie" the rule everywhere.
  err = ""
  var big: RegexNode = nil
  for i in 0 ..< patterns.len:
    var e = ""
    let rex = parseRegExpr(patterns[i], flags, e)
    if e.len > 0:
      err = e
      return
    rex.rule = i + 1
    if big == nil: big = rex
    else: big = altExpr(big, rex)
  if big == nil:
    err = "at least one pattern is required"
    return
  dfa = buildDfa(big)

proc regexToDfa*(pattern: string; flags: set[RegexFlag]; dfa: var Dfa;
                 err: var string) =
  ## `rulesToDfa` for the single-pattern case, plus the capture check that only
  ## makes sense there.
  err = ""
  var e = ""
  let rex = parseRegExpr(pattern, flags, e)
  if e.len > 0:
    err = e
    return
  if containsInvalidCapture(rex):
    err = "captures inside an alternation are not supported"
    return
  rex.rule = 1
  dfa = buildDfa(rex)

# ---------------------------------------------------------------------------
# Bytecode
# ---------------------------------------------------------------------------
#
# The DFA is walked as a tiny program rather than as a table: a state is a run
# of test/jump pairs ended by a `ret`. Accepting states first record their
# rule and position, so a later failed transition can return the longest match.
# This keeps the common test/jump pair adjacent and lets `std/rex`'s `re` emit
# a finished automaton as a literal instead of building tables at run time.

type
  RegexOpcode* = enum
    opcRet          ## stop and return the last accepting state
    opcRetAccept    ## stop at this accepting state
    opcAccept       ## remember an accepting state before trying transitions
    opcTestSet      ## current character against a set in the data section
    opcTestChar     ## current character against the character in the argument
    opcTJmp         ## taken when the preceding test succeeded
    opcBegin        ## `\A`
    opcEnd          ## `\Z`
    opcWordBound    ## `\b` / `\B`
    opcCaptureBegin ## `(`
    opcCaptureEnd   ## `)`
    opcBackref      ## `\1`

  RegexInstr* = object
    ## One instruction. Public only because `std/rex`'s `re` emits these as a
    ## literal; write `re"…"` rather than one of these by hand.
    opc*: RegexOpcode
    arg*: int32

  Regex* = object
    ## A compiled regular expression.
    ##
    ## There may be **two** automata in here. `code` decides the match and is
    ## built with the capture groups turned into plain groups; `capCode`, when
    ## it is not empty, is the same pattern built with the groups kept, and is
    ## run afterwards only to fill in where they landed.
    ##
    ## The split is what keeps a captured pattern matching at all. Capture
    ## markers ride the DFA's alphabet as ordinary letters and the matcher takes
    ## them unconditionally, so a group next to something optional
    ## (`(\w+)\s*=`) reaches a state where closing the group and reading the
    ## next character are both possible and the marker wins — the pattern then
    ## matches nothing, which is what upstream lexim does to this day. Deciding
    ## the match on the capture-free automaton makes that impossible: at worst
    ## the second run fails and the caller sees a correct match with no capture
    ## bounds.
    code*: seq[RegexInstr]
    data*: seq[set[char]]
    startAt*: int
    captures*: int         ## number of capture groups in the pattern
    capCode*: seq[RegexInstr]
    capData*: seq[set[char]]
    capStartAt*: int
    firstBytes*: set[char] ## every match starts with one of these; all bytes
                           ## when a match can start without reading one

  Capture* = object ## The bounds of one capture group, both ends inclusive.
    first*: Natural  ## a position in the subject, so never negative
    last*: int       ## …but this one carries `CaptureOpen` while matching

const
  CaptureOpen* = -2 ## `last` while the group is still being matched

func emptyRegex*(): Regex =
  Regex(code: @[], data: @[], startAt: 0, captures: 0,
        capCode: @[], capData: @[], capStartAt: 0, firstBytes: {})

func genData(c: var Regex; data: set[char]): int32 =
  ## Interns a character set. Sets repeat heavily across a DFA's states, so the
  ## linear scan pays for itself in the size of the emitted program.
  for i in 0 ..< c.data.len:
    if c.data[i] == data: return int32(i)
  result = int32(c.data.len)
  c.data.add data

func gen(c: var Regex; opc: RegexOpcode; arg: int) {.inline.} =
  c.code.add RegexInstr(opc: opc, arg: int32(arg))

func genTest(c: var Regex; x: Alphabet; dest: int) =
  case x.kind
  of reChar:
    gen c, opcTestChar, int(x.val)
    gen c, opcTJmp, dest
  of reBegin:
    gen c, opcBegin, 0
    gen c, opcTJmp, dest
  of reEnd:
    gen c, opcEnd, 0
    gen c, opcTJmp, dest
  of reWordBoundary:
    gen c, opcWordBound, genData(c, wordChars)
    gen c, opcTJmp, dest
  of reWordBoundaryNot:
    gen c, opcWordBound, genData(c, {'\1'..'\255'} - wordChars)
    gen c, opcTJmp, dest
  else: discard

func genCapture(c: var Regex; x: Alphabet; dest: int) =
  case x.kind
  of reCapture:
    gen c, opcCaptureBegin, int(x.val) - 1
    gen c, opcTJmp, dest
    inc c.captures
  of reCaptureEnd:
    gen c, opcCaptureEnd, int(x.val) - 1
    gen c, opcTJmp, dest
  of reBackref:
    gen c, opcBackref, int(x.val) - 1
    gen c, opcTJmp, dest
  else: discard

func genBytecode*(a: Dfa; res: var Regex) =
  ## Lowers a minimized DFA to the instruction stream `exec` walks.
  var stateToLabel = newSeq[int](a.stateCount + 1)
  for src in 1 .. a.stateCount:
    stateToLabel[src] = res.code.len
    let rule = getRule(a, src)
    if rule != 0:
      # Remember this state only if a transition can leave an accepting
      # path. Otherwise a failed test returns this state's rule directly.
      for edge in a.trans[src]:
        if getRule(a, int(edge.dest)) != rule:
          gen res, opcAccept, rule
          break
    for dest in allDests(a, src):
      let (others, cs) = allTransitions(a, src, dest)
      for x in others: genCapture(res, x, dest)
      if cs != {}:
        gen res, opcTestSet, genData(res, cs)
        gen res, opcTJmp, dest
      for x in others: genTest(res, x, dest)
    if rule == 0: gen res, opcRet, 0
    else: gen res, opcRetAccept, rule
  # The jumps were emitted with state numbers; patch them to code offsets now
  # that every state's offset is known.
  for i in 0 ..< res.code.len:
    if res.code[i].opc == opcTJmp:
      res.code[i] = RegexInstr(opc: opcTJmp,
                               arg: int32(stateToLabel[res.code[i].arg]))
  res.startAt = stateToLabel[a.startState]
  res.firstBytes = firstBytes(a)

proc countUses(r: RegexNode; caps, backs: var int) =
  ## Highest capture-group and back-reference index the pattern names.
  if r == nil: return
  if r.kind == reCapture: caps = max(caps, int(r.c))
  elif r.kind == reBackref: backs = max(backs, int(r.c))
  countUses(r.a, caps, backs)
  countUses(r.b, caps, backs)

proc buildInto(pattern: string; flags: set[RegexFlag]; dest: var Regex;
               err: var string): bool =
  var dfa = default(Dfa)
  regexToDfa(pattern, flags, dfa, err)
  result = err.len == 0
  if result: genBytecode(dfa, dest)

proc compileRegex*(pattern: string; flags: set[RegexFlag]; dest: var Regex;
                   err: var string): bool =
  ## Pattern text to finished automaton. `false` with a message in `err` for a
  ## pattern this engine cannot express; `dest` then matches nothing.
  dest = emptyRegex()
  err = ""
  var e = ""
  let rex = parseRegExpr(pattern, flags, e)
  if e.len > 0:
    # `err` is a whole sentence: it is shown as-is both by `tryRe` and by the
    # compile-time `re`, and neither knows any more about it than this does.
    err = "invalid regular expression: " & e
    return false
  var caps = 0
  var backs = 0
  countUses(rex, caps, backs)
  if caps > 0 and containsInvalidCapture(rex):
    err = "invalid regular expression: captures inside an alternation are " &
          "not supported; write `(abc|xyz)` rather than `(abc)|(xyz)`"
    return false

  # A back reference is part of the *language*, not just bookkeeping, so a
  # pattern that has one must decide its match on the automaton that tracks the
  # groups. Everything else decides on the capture-free one — see `Regex`.
  let splitCaptures = caps > 0 and backs == 0
  let matchFlags = if splitCaptures: flags + {reNoCaptures} else: flags
  if not buildInto(pattern, matchFlags, dest, err):
    dest = emptyRegex()
    return false
  if splitCaptures:
    var withCaps = emptyRegex()
    if buildInto(pattern, flags, withCaps, err):
      dest.capCode = withCaps.code
      dest.capData = withCaps.data
      dest.capStartAt = withCaps.startAt
    else:
      # The capture automaton is an extra, never a reason to reject the
      # pattern: matching still works, `capture` just returns "".
      err = ""
  dest.captures = caps
  result = true

# ---------------------------------------------------------------------------
# Execution
# ---------------------------------------------------------------------------

func backrefMatches(s: string; sp: Natural; c: Capture): bool
    {.requires: c.last < s.len.} =
  ## Does the text at `sp` repeat what `c` captured?
  var i = c.first
  var k = sp
  while true:
    if i > c.last: return true
    {.assert: i < s.len.}
    if k >= s.len or s[k] != s[i]: return false
    inc k
    inc i

func run[trackCaptures: static[bool]](code: seq[RegexInstr]; data: seq[set[char]];
         startAt: int; s: string; caps: var seq[Capture]; start: Natural;
         endPos: var int): int =
  ## One automaton over `s` from `start`. Returns the rule that matched (`0`
  ## for none) and reports the position just past the match in `endPos`.
  result = 0
  endPos = start
  if code.len == 0: return
  var pc = startAt
  var sp = start
  var acceptedCaps: seq[Capture] = @[]
  var capturesChanged = false
  while true:
    var instr = code[pc]
    if instr.opc == opcAccept:
      result = int(instr.arg)
      endPos = sp
      when trackCaptures:
        if capturesChanged:
          if acceptedCaps.len != caps.len:
            acceptedCaps.setLen caps.len
          for i in 0 ..< caps.len:
            acceptedCaps[i] = caps[i]
          capturesChanged = false
      inc pc
      instr = code[pc]
    let arg = int(instr.arg)
    case instr.opc
    of opcTestSet:
      # the instruction after a test is always its `TJmp`
      if sp < s.len and s[sp] in data[arg]:
        pc = int(code[pc+1].arg)
        inc sp
      else:
        inc pc, 2
    of opcTestChar:
      if sp < s.len and s[sp] == char(arg):
        pc = int(code[pc+1].arg)
        inc sp
      else:
        inc pc, 2
    of opcRet:
      when trackCaptures:
        caps = acceptedCaps
      return result
    of opcRetAccept:
      endPos = sp
      return arg
    of opcAccept:
      return 0 # an accept is always followed by a test or return
    of opcBegin:
      if sp == start: pc = int(code[pc+1].arg)
      else: inc pc, 2
    of opcEnd:
      if sp >= s.len: pc = int(code[pc+1].arg)
      else: inc pc, 2
    of opcWordBound:
      if sp >= s.len or sp == start or s[sp] notin data[arg]:
        pc = int(code[pc+1].arg)
      else:
        inc pc, 2
    of opcCaptureBegin:
      when trackCaptures:
        if caps.len <= arg: setLen(caps, arg + 1)
        caps[arg] = Capture(first: sp, last: CaptureOpen)
        capturesChanged = true
        pc = int(code[pc+1].arg)
      else:
        # unreachable: code that keeps its groups runs in `matchWithCaptures`
        return 0
    of opcCaptureEnd:
      when trackCaptures:
        if caps.len <= arg: setLen(caps, arg + 1)
        caps[arg].last = sp - 1
        capturesChanged = true
        pc = int(code[pc+1].arg)
      else:
        # unreachable: code that keeps its groups runs in `matchWithCaptures`
        return 0
    of opcBackref:
      if arg < caps.len:
        let cap = caps[arg]
        if cap.last < s.len and backrefMatches(s, sp, cap):
          pc = int(code[pc+1].arg)
          inc sp, cap.last - cap.first + 1
        else:
          inc pc, 2
      else:
        inc pc, 2
    of opcTJmp:
      # unreachable: a `TJmp` is only ever entered through the test above it
      return 0

func matchOnly(code: seq[RegexInstr]; data: seq[set[char]]; startAt: int;
               s: string; caps: var seq[Capture]; start: Natural;
               endPos: var int): int {.inline.} =
  ## `run` for code without capture groups: no capture bookkeeping, and
  ## `caps` is left alone.
  result = run[false](code, data, startAt, s, caps, start, endPos)

func matchWithCaptures(code: seq[RegexInstr]; data: seq[set[char]];
                       startAt: int; s: string; caps: var seq[Capture];
                       start: Natural; endPos: var int): int {.inline.} =
  ## `run` for code that keeps its groups; a failed longer match rewinds
  ## `caps` to the last accepting state.
  result = run[true](code, data, startAt, s, caps, start, endPos)

func exec*(r: Regex; s: string; caps: var seq[Capture]; start: int;
           endPos: var int): int =
  ## Matches `r` at `start`. Returns the rule that matched (`0` for none),
  ## reports the position just past the match in `endPos` and fills `caps` with
  ## the capture bounds.
  ##
  ## The match is decided by `r.code`; `r.capCode`, when present, runs
  ## afterwards purely for the bounds and is allowed to come up empty-handed
  ## without changing the answer.
  caps.setLen 0
  # Only a pattern without groups is known to need no capture bookkeeping.
  # With groups but no back references `code` has none either, but telling
  # the two apart costs more per call than the bookkeeping saves.
  if r.captures == 0:
    result = matchOnly(r.code, r.data, r.startAt, s, caps, start, endPos)
  else:
    result = matchWithCaptures(r.code, r.data, r.startAt, s, caps, start, endPos)
  if result > 0 and r.capCode.len > 0:
    var found: seq[Capture] = @[]
    var ignored = start
    if matchWithCaptures(r.capCode, r.capData, r.capStartAt, s, found, start,
                         ignored) > 0:
      caps = found
