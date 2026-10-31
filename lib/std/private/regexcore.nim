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
    at, width: int    ## where in the pattern a character, class or back
                      ## reference was written; for the diagnostics

  RegexFlag* = enum ## how a pattern is parsed
    reExtended,     ## ignore unescaped spaces and tabs
    reNoBackrefs,   ## treat `\1` as a character literal, not a back reference
    reNoCaptures,   ## `(x)` means the same as `(?:x)`
    reAllowEvil     ## accept an ambiguous pattern (see `checkDeterministic`):
                    ## `.*x`, `\d+|\w+`. It then means what the automaton
                    ## makes of it, and compiling it can take exponential
                    ## time and memory

const
  wordChars* = {'A'..'Z', 'a'..'z', '0'..'9', '_', '\128'..'\255'}
    ## the bytes `\b` treats as part of a word; every non-ASCII byte is one
  maxRune = 0x10FFFF

# ---------------------------------------------------------------------------
# AST construction
# ---------------------------------------------------------------------------

proc newNode(kind: RegexKind): RegexNode =
  result = RegexNode(kind: kind, a: nil, b: nil, r: Rune(0), c: '\0', s: "",
                     cc: @[], rule: 0, at: 0, width: 0)

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
  ## `r{m,n}`, expanded to `r^m (r (r …)?)?` with `n - m` optional copies.
  ## Nesting the options rather than alternating `r^m | … | r^n` keeps a
  ## deterministic `r` deterministic: after each copy the next character
  ## decides between another copy and what follows.
  if m > n or n == 0:
    result = epsExpr()
  else:
    var tail: RegexNode = nil
    for i in m + 1 .. n:
      tail = optExpr(if tail == nil: r else: catExpr(r, tail))
    result = tail
    for i in 1 .. m:
      result = if result == nil: r else: catExpr(r, result)

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
  ## Decode complete UTF-8 sequences; retain other bytes as character values.
  var r = Rune(ord(c.buf[c.pos]))
  var width = 1
  if c.buf[c.pos] >= '\x80':
    let n = runeLenAt(c.buf, c.pos)
    let first = c.buf[c.pos]
    var complete = first in '\xC2'..'\xF4' and n <= 4 and
                   n <= c.buf.len - c.pos
    if complete:
      for i in 1 ..< n:
        if c.buf[c.pos + i] notin '\x80'..'\xBF': complete = false
      let second = c.buf[c.pos + 1]
      if (first == '\xE0' and second < '\xA0') or
          (first == '\xED' and second >= '\xA0') or
          (first == '\xF0' and second < '\x90') or
          (first == '\xF4' and second >= '\x90'):
        complete = false
    if complete:
      r = runeAt(c.buf, c.pos)
      width = n
  result = charExpr(r)
  inc c.pos, width

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
  result = epsExpr()
  inc c.pos # skip the opening quote
  while c.pos < c.buf.len and c.buf[c.pos] != '\"':
    if c.buf[c.pos] == '\r' or c.buf[c.pos] == '\L':
      error c, "'\"' expected"
      return epsExpr()
    let at = c.pos
    let al = getChar(c, false)
    if al.kind == reChar:
      al.at = at
      al.width = c.pos - at
      result = catExpr(result, al)
    else:
      error c, "only single characters are allowed in a quoted string"
      return epsExpr()
  if c.pos < c.buf.len:
    inc c.pos # skip the closing quote
  else:
    error c, "'\"' expected"

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

proc setSpan(n: RegexNode; at, width: int) =
  ## Records where the characters and classes in `n` were written, unless an
  ## inner factor already did.
  if n == nil: return
  if n.kind in {reChar, reStr, reCClass, reBackref} and n.width == 0:
    n.at = at
    n.width = width
  setSpan(n.a, at, width)
  setSpan(n.b, at, width)

proc factor(c: var ReCtx): RegexNode =
  skipSpaces c
  let at = c.pos
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
  setSpan(result, at, c.pos - at)
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
# Left factoring
# ---------------------------------------------------------------------------
#
# `if|in|int` has to read the `i` before it can tell the keywords apart, which
# the determinism check below would reject. Factoring the heads the branches
# share out of the alternation, `i(?:f|n(?:t)?)`, fixes that without changing
# the language.

proc sameSet(a, b: RuneSet): bool =
  result = a.len == b.len
  if result:
    for i in 0 ..< a.len:
      if a[i].a != b[i].a or a[i].b != b[i].b: return false

proc sameNode(a, b: RegexNode): bool =
  if a == b: return true
  if a == nil or b == nil or a.kind != b.kind: return false
  case a.kind
  of reChar: result = int(a.r) == int(b.r)
  of reStr: result = a.s == b.s
  of reCClass: result = sameSet(a.cc, b.cc)
  of reCapture, reCaptureEnd, reBackref:
    result = a.c == b.c and sameNode(a.a, b.a)
  else: result = sameNode(a.a, b.a) and sameNode(a.b, b.b)

proc addFactors(n: RegexNode; dest: var seq[RegexNode]) =
  ## `n` as a list of factors; a literal run is one factor per character, so
  ## `abc|abd` shares its first two.
  case n.kind
  of reCat:
    addFactors(n.a, dest)
    addFactors(n.b, dest)
  of reEps: discard
  of reStr:
    var i = 0
    while i < n.s.len:
      let x = charExpr(runeAt(n.s, i))
      x.at = n.at
      x.width = n.width
      dest.add x
      inc i, runeLenAt(n.s, i)
  else:
    dest.add n

proc catOf(fs: seq[RegexNode]; first: int): RegexNode =
  result = nil
  var i = fs.len - 1
  while i >= first:
    result = if result == nil: fs[i] else: catExpr(fs[i], result)
    dec i
  if result == nil: result = epsExpr()

proc factorBranches(branches: seq[seq[RegexNode]]): RegexNode =
  ## The alternation of `branches`, each a list of factors, with the branches
  ## that start alike merged: `ab|ac|d` becomes `a(?:b|c)|d`, `a|ab` becomes
  ## `a(?:b)?`.
  var done = newSeq[bool](branches.len)
  var alts: seq[RegexNode] = @[]
  var hasEps = false
  for i in 0 ..< branches.len:
    if done[i]: continue
    if branches[i].len == 0:
      hasEps = true
      continue
    let head = branches[i][0]
    var tails: seq[seq[RegexNode]] = @[]
    for j in i ..< branches.len:
      if not done[j] and branches[j].len > 0 and sameNode(branches[j][0], head):
        done[j] = true
        var t: seq[RegexNode] = @[]
        for k in 1 ..< branches[j].len: t.add branches[j][k]
        tails.add t
    if tails.len == 1:
      alts.add catOf(branches[i], 0)
    else:
      let rest = factorBranches(tails)
      alts.add(if rest.kind == reEps: head else: catExpr(head, rest))
  result = nil
  for x in alts:
    result = if result == nil: x else: altExpr(result, x)
  if result == nil: result = epsExpr()
  elif hasEps: result = optExpr(result)

proc collectAlts(n: RegexNode; dest: var seq[RegexNode]) =
  if n.kind == reAlt:
    collectAlts(n.a, dest)
    collectAlts(n.b, dest)
  else:
    dest.add n

proc leftFactor*(n: RegexNode): RegexNode =
  ## `n` with the common heads of every alternation's branches factored out.
  ## The tree is copied, never changed: `x{m,n}` shares its `x`.
  case n.kind
  of reAlt:
    var alts: seq[RegexNode] = @[]
    collectAlts(n, alts)
    var branches: seq[seq[RegexNode]] = @[]
    for x in alts:
      var fs: seq[RegexNode] = @[]
      addFactors(leftFactor(x), fs)
      branches.add fs
    result = factorBranches(branches)
  of reCat: result = catExpr(leftFactor(n.a), leftFactor(n.b))
  of reStar: result = starExpr(leftFactor(n.a))
  of rePlus: result = plusExpr(leftFactor(n.a))
  of reOpt: result = optExpr(leftFactor(n.a))
  of reCapture:
    result = captureExpr(leftFactor(n.a))
    result.c = n.c
  else: result = n

proc stripCaptures(n: RegexNode): RegexNode =
  ## `n` with every group turned into a plain one.
  case n.kind
  of reCapture: result = stripCaptures(n.a)
  of reCat: result = catExpr(stripCaptures(n.a), stripCaptures(n.b))
  of reAlt: result = altExpr(stripCaptures(n.a), stripCaptures(n.b))
  of reStar: result = starExpr(stripCaptures(n.a))
  of rePlus: result = plusExpr(stripCaptures(n.a))
  of reOpt: result = optExpr(stripCaptures(n.a))
  else: result = n

# ---------------------------------------------------------------------------
# Determinism
# ---------------------------------------------------------------------------
#
# A pattern is *deterministic* (Brüggemann-Klein and Wood's "1-unambiguous")
# when, reading the subject from left to right, the next character always
# says which character, class or back reference of the pattern it belongs to.
# `\w+\s*=\s*\w+` is: after a `\w`, a word character continues the `\w+`,
# a space starts the `\s*` and `=` is the `=`. `.*x` is not: after the `.`,
# an `x` could be either. Glushkov's construction decides it: every
# character, class and back reference is a *position*, `follow[p]` is where
# the next character can go after `p`, and the pattern is deterministic when
# no two positions it has to choose between share a character.
#
# Zero-width assertions count as nothing at all, and a back reference counts
# as the characters its group can start with.

type
  Positions = object
    atoms: seq[RegexNode]
    sets: seq[RuneSet]
    follow: seq[seq[int]]
    groupFirst: seq[RuneSet] ## per group: the characters it can start with
    groupNullable: seq[bool]
    groupSeen: seq[bool]

  Glu = object
    nullable: bool
    first, last: seq[int]

proc newPosition(g: var Positions; n: RegexNode; cc: RuneSet;
                 nullable: bool): Glu =
  let p = g.atoms.len
  g.atoms.add n
  g.sets.add cc
  g.follow.add @[]
  result = Glu(nullable: nullable, first: @[p], last: @[p])

proc catGlu(g: var Positions; a, b: Glu): Glu =
  for p in a.last:
    for q in b.first: g.follow[p].add q
  result = Glu(nullable: a.nullable and b.nullable, first: a.first,
               last: b.last)
  if a.nullable:
    for q in b.first: result.first.add q
  if b.nullable:
    for p in a.last: result.last.add p

proc positions(g: var Positions; n: RegexNode): Glu =
  case n.kind
  of reEps, reBegin, reEnd, reWordBoundary, reWordBoundaryNot, reCaptureEnd:
    result = Glu(nullable: true, first: @[], last: @[])
  of reChar:
    var cc: RuneSet = @[]
    cc.incl int(n.r), int(n.r)
    result = newPosition(g, n, cc, false)
  of reStr:
    var fs: seq[RegexNode] = @[]
    addFactors(n, fs)
    result = Glu(nullable: true, first: @[], last: @[])
    for x in fs:
      let next = positions(g, x)
      result = catGlu(g, result, next)
  of reCClass:
    result = newPosition(g, n, n.cc, n.cc.len == 0)
  of reBackref:
    let k = int(n.c)
    if k < g.groupSeen.len and g.groupSeen[k]:
      let cc = g.groupFirst[k]
      result = newPosition(g, n, cc, g.groupNullable[k])
    else:
      result = newPosition(g, n, complement(@[]), true)
  of reCat:
    let a = positions(g, n.a)
    let b = positions(g, n.b)
    result = catGlu(g, a, b)
  of reAlt:
    let a = positions(g, n.a)
    let b = positions(g, n.b)
    result = Glu(nullable: a.nullable or b.nullable, first: a.first,
                 last: a.last)
    for q in b.first: result.first.add q
    for p in b.last: result.last.add p
  of reStar, rePlus:
    result = positions(g, n.a)
    for p in result.last:
      for q in result.first: g.follow[p].add q
    if n.kind == reStar: result.nullable = true
  of reOpt:
    result = positions(g, n.a)
    result.nullable = true
  of reCapture:
    result = positions(g, n.a)
    let k = int(n.c)
    if g.groupSeen.len <= k:
      g.groupSeen.setLen k + 1
      g.groupFirst.setLen k + 1
      g.groupNullable.setLen k + 1
    var cc: RuneSet = @[]
    for p in result.first: cc.incl g.sets[p]
    g.groupFirst[k] = cc
    g.groupNullable[k] = result.nullable
    g.groupSeen[k] = true

proc overlap(a, b: RuneSet; sample: var int): bool =
  ## Do `a` and `b` share a code point? `sample` is one of them.
  var i = 0
  var j = 0
  while i < a.len and j < b.len:
    let lo = max(a[i].a, b[j].a)
    let hi = min(a[i].b, b[j].b)
    if lo <= hi:
      sample = lo
      return true
    if a[i].b < b[j].b: inc i
    else: inc j
  result = false

proc findConflict(g: Positions; list: seq[int]; x, y, sample: var int): bool =
  for i in 0 ..< list.len:
    for j in i + 1 ..< list.len:
      if list[i] != list[j] and
          overlap(g.sets[list[i]], g.sets[list[j]], sample):
        x = list[i]
        y = list[j]
        return true
  result = false

proc runeText(c: int): string =
  ## `c` the way a pattern would spell it.
  if c > 32 and c < 127:
    result = newString(1)
    result[0] = char(c)
  elif c == 32: result = "\\ "
  elif c < 128: result = "\\0" & $c
  else: result = toUTF8(Rune(c))

proc atomText(pattern: string; n: RegexNode): string =
  if n.width > 0 and n.at + n.width <= pattern.len:
    result = substr(pattern, n.at, n.at + n.width - 1)
  elif n.kind == reChar:
    result = runeText(int(n.r))
  else:
    result = "[…]"

proc checkDeterministic*(n: RegexNode; pattern: string;
                         err: var string): bool =
  ## Is `n` deterministic? If not, `err` names the two parts of `pattern`
  ## the same character could belong to.
  var g = Positions(atoms: @[], sets: @[], follow: @[], groupFirst: @[],
                    groupNullable: @[], groupSeen: @[])
  let root = positions(g, n)
  var x = 0
  var y = 0
  var c = 0
  var after = -1
  var found = findConflict(g, root.first, x, y, c)
  if not found:
    for p in 0 ..< g.follow.len:
      if findConflict(g, g.follow[p], x, y, c):
        found = true
        after = p
        break
  result = not found
  if found:
    var a = atomText(pattern, g.atoms[x])
    var b = atomText(pattern, g.atoms[y])
    if a == b:
      a.add "` (column " & $(g.atoms[x].at + 1) & ")"
      b.add "` (column " & $(g.atoms[y].at + 1) & ")"
    else:
      a.add "`"
      b.add "`"
    err = "ambiguous regular expression: "
    if after < 0:
      err.add "at the start"
    else:
      err.add "after `" & atomText(pattern, g.atoms[after]) & "`"
    err.add ", the character `" & runeText(c) & "` could belong to either `" &
            a & " or `" & b & ". Rewrite the pattern so that the next " &
            "character always decides (`[^x]*x` rather than `.*x`), or add " &
            "`reAllowEvil` to the flags (keeping `reExtended`)"


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
    if r.kind == reChar and int(r.r) in 0x80..0xFF:
      transBytes a, result, char(int(r.r)), char(int(r.r)), result + bytes.len
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
    for i in 0 ..< r.cc.len:
      let range = r.cc[i]
      utf8Sequences(seqs, range.a, range.b)
      let lo = max(range.a, 0x80)
      let hi = min(range.b, 0xFF)
      if lo <= hi:
        var sq = Utf8Seq(len: 1)
        sq.lo[0] = char(lo)
        sq.hi[0] = char(hi)
        seqs.add sq
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
    # The loop needs an entry state of its own: looping back to `result`, where
    # the preceding factor ends, made `a*b*` read an `a` after a `b`.
    let entry = result + 1
    eps a, result, entry
    let aa = auxRegExprToNfa(r.a, a, entry)
    eps a, aa, entry
    eps a, entry, aa + 1
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

func onlyByte(cs: set[char]): int =
  ## The one member of `cs`, or `-1` when it has none or several.
  result = -1
  for b in 0 .. 255:
    if char(b) in cs:
      if result >= 0: return -1
      result = b

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
    ## run afterwards over the match only to find where they landed.
    ##
    ## Capture markers ride the DFA's alphabet as ordinary letters, so in
    ## `capCode` a group next to something optional (`(\w+)\s*=`) reaches a
    ## state where closing the group and reading the next character are both
    ## possible. `captureWalk` follows both; a matcher that takes the marker
    ## unconditionally matches nothing there, which is what upstream lexim
    ## does to this day. A pattern with back references has no `capCode`: the
    ## groups are part of what it matches, so `code` keeps them.
    code*: seq[RegexInstr]
    data*: seq[set[char]]
    startAt*: int
    captures*: int         ## number of capture groups in the pattern
    capCode*: seq[RegexInstr]
    capData*: seq[set[char]]
    capStartAt*: int
    firstBytes*: set[char] ## every match starts with one of these; all bytes
                           ## when a match can start without reading one
    firstByte*: int        ## the only byte in `firstBytes`, or `-1`; search
                           ## scans for it a word at a time
    backrefs*: int         ## number of back references in the pattern
    lit*: string           ## a literal every match contains, or `""`; search
                           ## looks for it first and asks the automaton only
                           ## at the starts that can reach it
    litMin*: int           ## a match has at least this many bytes before `lit`
    litBack*: seq[int]     ## from `lit` leftwards, how to bound the match's
                           ## start: step back over at most `litBack[i]` bytes
    litBackSets*: seq[set[char]] ## …that are in `litBackSets[i]`; `-1` means
                                 ## no limit

  Capture* = object ## The bounds of one capture group, both ends inclusive.
    first*: Natural  ## a position in the subject, so never negative
    last*: int       ## …but this one carries `CaptureOpen` while matching

const
  CaptureOpen* = -2 ## `last` while the group is still being matched

func emptyRegex*(): Regex =
  Regex(code: @[], data: @[], startAt: 0, captures: 0,
        capCode: @[], capData: @[], capStartAt: 0, firstBytes: {},
        firstByte: -1, backrefs: 0, lit: "", litMin: 0, litBack: @[],
        litBackSets: @[])

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
    gen c, opcWordBound, 0
    gen c, opcTJmp, dest
  of reWordBoundaryNot:
    gen c, opcWordBound, 1
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
  res.firstByte = onlyByte(res.firstBytes)

proc countUses(r: RegexNode; caps, backs: var int) =
  ## Highest capture-group and back-reference index the pattern names.
  if r == nil: return
  if r.kind == reCapture: caps = max(caps, int(r.c))
  elif r.kind == reBackref: backs = max(backs, int(r.c))
  countUses(r.a, caps, backs)
  countUses(r.b, caps, backs)

# ---------------------------------------------------------------------------
# Search anchors
# ---------------------------------------------------------------------------
#
# A search does not have to restart the automaton at every position. When
# every match contains a literal at the top level of the pattern,
# `P = A lit B`, `std/rex` finds `lit` with a string search and runs the
# automaton only from the starts `A` can reach it from. `litBack` describes
# `A` from right to left so the search can bound those starts without a
# reverse automaton: each factor of `A` is at most so many bytes long and made
# of bytes from some set, and walking back over such bytes from the literal
# can only stop early, never late.

const
  Unbounded = -1
  AllBytes* = {'\0' .. '\255'}

proc utf8Len(r: int): int =
  if r < 0x80: 1
  elif r < 0x800: 2
  elif r < 0x10000: 3
  else: 4

proc lenRange(n: RegexNode; mn, mx: var int) =
  ## The fewest and the most bytes a match of `n` spans; `mx` is `Unbounded`
  ## when there is no limit.
  var amn = 0
  var amx = 0
  case n.kind
  of reEps, reBegin, reEnd, reWordBoundary, reWordBoundaryNot, reCaptureEnd:
    mn = 0
    mx = 0
  of reChar:
    mx = utf8Len(int(n.r))
    # 128..255 also match the single byte of that value
    mn = if int(n.r) in 0x80..0xFF: 1 else: mx
  of reStr:
    mn = n.s.len
    mx = mn
  of reCClass:
    if n.cc.len == 0:
      mn = 0
      mx = 0
    else:
      mn = if n.cc[0].a <= 0xFF: 1 else: utf8Len(n.cc[0].a)
      mx = utf8Len(n.cc[n.cc.len - 1].b)
  of reCat, reAlt:
    lenRange(n.a, amn, amx)
    lenRange(n.b, mn, mx)
    if n.kind == reCat:
      mn = mn + amn
      mx = if mx == Unbounded or amx == Unbounded: Unbounded else: mx + amx
    else:
      mn = min(mn, amn)
      mx = if mx == Unbounded or amx == Unbounded: Unbounded else: max(mx, amx)
  of reStar, rePlus, reOpt:
    lenRange(n.a, amn, amx)
    mn = if n.kind == rePlus: amn else: 0
    mx = if n.kind == reOpt or amx == 0: amx else: Unbounded
  of reCapture:
    lenRange(n.a, mn, mx)
  of reBackref:
    mn = 0
    mx = Unbounded

proc addBytes(n: RegexNode; dest: var set[char]) =
  ## Every byte a match of `n` can contain.
  case n.kind
  of reChar:
    for b in toUTF8(n.r): dest.incl b
    if int(n.r) in 0x80..0xFF: dest.incl char(int(n.r))
  of reStr:
    for i in 0 ..< n.s.len: dest.incl n.s[i]
  of reCClass:
    for i in 0 ..< n.cc.len:
      let rg = n.cc[i]
      if rg.a < 0x80:
        for c in rg.a .. min(rg.b, 0x7F): dest.incl char(c)
      if rg.b >= 0x80:
        dest = dest + {'\128' .. '\255'}
  of reBackref:
    dest = AllBytes
  else:
    if n.a != nil: addBytes(n.a, dest)
    if n.b != nil: addBytes(n.b, dest)

proc factors(n: RegexNode; dest: var seq[RegexNode]) =
  ## `n` as the sequence of parts every match is a concatenation of. A group is
  ## its content as far as matching goes, and `x+` is `x x*`, so the first
  ## copy of a repeated literal is at a fixed place too.
  case n.kind
  of reCat:
    factors(n.a, dest)
    factors(n.b, dest)
  of reCapture:
    factors(n.a, dest)
  of rePlus:
    factors(n.a, dest)
    dest.add starExpr(n.a)
  else:
    dest.add n

proc literalOf(n: RegexNode; dest: var string): bool =
  ## Appends the bytes `n` matches when it matches exactly one byte string.
  ## `\128`..`\255` do not: they also match the single byte.
  case n.kind
  of reChar:
    result = int(n.r) notin 0x80..0xFF
    if result: dest.add toUTF8(n.r)
  of reStr:
    result = true
    dest.add n.s
  else:
    result = false

proc findAnchor(rex: RegexNode; dest: var Regex) =
  ## Fills in the search hints: `lit`, `litMin`, `litBack` and `litBackSets`
  ## for the longest run of literal factors and what precedes it.
  var fs: seq[RegexNode] = @[]
  factors(rex, fs)
  var best = ""
  var bestAt = 0
  var i = 0
  while i < fs.len:
    var run = ""
    var j = i
    var mn = 0
    var mx = 0
    # zero-width factors (`\b`, `$`) do not interrupt a run
    while j < fs.len:
      if not literalOf(fs[j], run):
        lenRange(fs[j], mn, mx)
        if mx != 0: break
      inc j
    if run.len > best.len:
      best = run
      bestAt = i
    i = max(j, i + 1)
  if best.len == 0: return
  # `bestAt` is where the run starts, possibly on a zero-width factor that
  # precedes its first literal; those cost nothing either way.
  dest.lit = best
  dest.litMin = 0
  var k = bestAt - 1
  while k >= 0:
    var mn = 0
    var mx = 0
    lenRange(fs[k], mn, mx)
    inc dest.litMin, mn
    if mx != 0:
      var bytes: set[char] = {}
      addBytes(fs[k], bytes)
      dest.litBack.add mx
      dest.litBackSets.add bytes
    dec k

proc accepting(n: RegexNode): RegexNode =
  ## `n` as rule 1. The rule goes on a node of its own: the trees share
  ## subtrees, and a rule on one of those would make it accept in the other.
  result = catExpr(n, epsExpr())
  result.rule = 1

proc compileRegex*(pattern: string; flags: set[RegexFlag]; dest: var Regex;
                   err: var string): bool =
  ## Pattern text to finished automaton. `false` with a message in `err` for a
  ## pattern this engine cannot express, or, without `reAllowEvil`, one that
  ## is not deterministic (see `checkDeterministic`); `dest` then matches
  ## nothing.
  dest = emptyRegex()
  err = ""
  var e = ""
  let parsed = parseRegExpr(pattern, flags, e)
  if e.len > 0:
    # `err` is a whole sentence: it is shown as-is both by `tryRe` and by the
    # compile-time `re`, and neither knows any more about it than this does.
    err = "invalid regular expression: " & e
    return false
  let rex = leftFactor(parsed)
  if reAllowEvil notin flags and not checkDeterministic(rex, pattern, err):
    return false
  var caps = 0
  var backs = 0
  countUses(rex, caps, backs)
  if backs > 0 and containsInvalidCapture(rex):
    err = "invalid regular expression: a back reference cannot be used " &
          "with captures inside an alternation; write `(abc|xyz)` rather " &
          "than `(abc)|(xyz)`"
    return false
  # A back reference is part of the *language*, not just bookkeeping, so a
  # pattern that has one must decide its match on the automaton that tracks
  # the groups. Everything else decides on the capture-free one — see `Regex`.
  genBytecode(buildDfa(accepting(if backs == 0: stripCaptures(rex) else: rex)),
              dest)
  findAnchor(rex, dest)
  if caps > 0 and backs == 0:
    var withCaps = emptyRegex()
    genBytecode(buildDfa(accepting(rex)), withCaps)
    dest.capCode = withCaps.code
    dest.capData = withCaps.data
    dest.capStartAt = withCaps.startAt
  dest.captures = caps
  dest.backrefs = backs
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

func atWordBoundary(s: string; sp: int): bool {.inline.} =
  ## Is `sp` between a word character and something else?
  let before = sp > 0 and sp <= s.len and s[sp - 1] in wordChars
  let after = sp >= 0 and sp < s.len and s[sp] in wordChars
  result = before != after

func run[trackCaptures: static[bool]](code: seq[RegexInstr]; data: seq[set[char]];
         startAt: int; s: string; caps: var seq[Capture]; start: Natural;
         endPos, reach: var int): int =
  ## One automaton over `s` from `start`. Returns the rule that matched (`0`
  ## for none) and reports the position just past the match in `endPos`, and
  ## the last position it looked at in `reach`.
  result = 0
  endPos = start
  reach = start
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
      reach = sp
      return result
    of opcRetAccept:
      endPos = sp
      reach = sp
      return arg
    of opcAccept:
      return 0 # an accept is always followed by a test or return
    of opcBegin:
      if sp == 0: pc = int(code[pc+1].arg)
      else: inc pc, 2
    of opcEnd:
      if sp >= s.len: pc = int(code[pc+1].arg)
      else: inc pc, 2
    of opcWordBound:
      if atWordBoundary(s, sp) == (arg == 0): pc = int(code[pc+1].arg)
      else: inc pc, 2
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
  var reach = start
  result = run[false](code, data, startAt, s, caps, start, endPos, reach)

func matchWithCaptures(code: seq[RegexInstr]; data: seq[set[char]];
                       startAt: int; s: string; caps: var seq[Capture];
                       start: Natural; endPos: var int): int {.inline.} =
  ## `run` for code that keeps its groups; a failed longer match rewinds
  ## `caps` to the last accepting state.
  var reach = start
  result = run[true](code, data, startAt, s, caps, start, endPos, reach)

func captureWalk(code: seq[RegexInstr]; data: seq[set[char]]; startAt: int;
                 s: string; captures: int; start, endPos: int;
                 caps: var seq[Capture]): bool =
  ## Where the groups of a match from `start` to `endPos` landed. Every way
  ## through the automaton that reads `s[start ..< endPos]` is followed at
  ## once, a capture marker being a step that reads nothing, so closing a
  ## group early cannot lose the match. Two ways that reach the same state at
  ## the same position have the same future and the first one is kept; for a
  ## deterministic pattern there is only ever one way to read the match.
  var unset = newSeq[Capture](captures)
  for i in 0 ..< captures: unset[i] = Capture(first: 0, last: -1)
  var cur: seq[(int, seq[Capture])] = @[(startAt, unset)]
  var next: seq[(int, seq[Capture])] = @[]
  var seenCur = newSeq[int](code.len)  # the position + 1 a state was reached at
  var seenNext = newSeq[int](code.len)
  seenCur[startAt] = start + 1
  var sp = start
  while true:
    var t = 0
    while t < cur.len:
      var pc = cur[t][0]
      let tc = cur[t][1]
      inc t
      while true:
        let instr = code[pc]
        let arg = int(instr.arg)
        var spawn = -1
        case instr.opc
        of opcAccept:
          if sp == endPos:
            caps = tc
            return true
          inc pc
          continue
        of opcRetAccept:
          if sp == endPos:
            caps = tc
            return true
          break
        of opcRet: break
        of opcTestSet, opcTestChar:
          if sp < endPos and (if instr.opc == opcTestSet: s[sp] in data[arg]
                              else: s[sp] == char(arg)):
            let d = int(code[pc+1].arg)
            if seenNext[d] != sp + 2:
              seenNext[d] = sp + 2
              next.add (d, tc)
        of opcBegin:
          if sp == 0: spawn = int(code[pc+1].arg)
        of opcEnd:
          if sp >= s.len: spawn = int(code[pc+1].arg)
        of opcWordBound:
          if atWordBoundary(s, sp) == (arg == 0): spawn = int(code[pc+1].arg)
        of opcCaptureBegin, opcCaptureEnd:
          let d = int(code[pc+1].arg)
          if seenCur[d] != sp + 1:
            seenCur[d] = sp + 1
            var moved = tc
            if instr.opc == opcCaptureBegin:
              moved[arg] = Capture(first: sp, last: CaptureOpen)
            else:
              moved[arg].last = sp - 1
            cur.add (d, moved)
        of opcBackref, opcTJmp:
          discard # `capCode` has no back references
        if spawn >= 0 and seenCur[spawn] != sp + 1:
          seenCur[spawn] = sp + 1
          cur.add (spawn, tc)
        inc pc, 2
    if sp >= endPos or next.len == 0: return false
    swap cur, next
    next.setLen 0
    swap seenCur, seenNext
    inc sp

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
  if r.backrefs == 0:
    result = matchOnly(r.code, r.data, r.startAt, s, caps, start, endPos)
    if result > 0 and r.capCode.len > 0:
      var found: seq[Capture] = @[]
      if captureWalk(r.capCode, r.capData, r.capStartAt, s, r.captures, start,
                     endPos, found):
        caps = found
  else:
    result = matchWithCaptures(r.code, r.data, r.startAt, s, caps, start, endPos)

# ---------------------------------------------------------------------------
# Searching
# ---------------------------------------------------------------------------
#
# A search tries the next start that can match with one plain run of the
# automaton (`attempt`). That is all a search ever does when the attempt
# matches, which is the common case. When it fails, the bytes it read may
# hold a match from a later start, and trying those one by one is what makes
# a naive search quadratic. Instead, the starts up to where the attempt gave
# up run as *threads*, all of them in one pass (`runThreads`). Two threads in
# the same state have the same future, so the one that started later can
# never win and is dropped: there are never more threads than states. The
# next attempt starts past the bytes the last one read, so no byte is read by
# more than one attempt, and a search costs `O(subject × states)` whatever
# the pattern. `std/rex` skips ahead between attempts (see `findAnchor`).
#
# The match code has no capture markers when the pattern has no back
# references, and `^`, `$` and `\b` depend only on the position, so a state
# is all there is to a thread. Patterns with back references are searched
# start by start.

type
  Threads* = object
    ## The threads of a search, ordered by start.
    pc, start: seq[int]
    nextPc, nextStart: seq[int]
    seen: seq[int] ## per state: the step that last put a thread into it
    step: int

func initThreads*(t: var Threads; r: Regex) =
  ## Clears `t` for a search with `r`.
  t.pc.setLen 0
  t.start.setLen 0
  if t.seen.len < r.code.len: t.seen.setLen r.code.len
  inc t.step

func alive*(t: Threads): bool {.inline.} = t.pc.len > 0

func spawn*(t: var Threads; r: Regex; sp: int) =
  ## A thread for a match starting at `sp`, unless one that started earlier
  ## is in the start state already.
  if t.seen[r.startAt] != t.step:
    t.seen[r.startAt] = t.step
    t.pc.add r.startAt
    t.start.add sp

func stepState(r: Regex; pc: int; s: string; sp: int;
               accepted: var bool): int =
  ## Runs the state at `pc` over `s[sp]`, the way `run` does: the state the
  ## byte leads to, or `-1`. `accepted` is set when a match ends at `sp`.
  var pc = pc
  while true:
    let instr = r.code[pc]
    let arg = int(instr.arg)
    case instr.opc
    of opcAccept:
      accepted = true
      inc pc
    of opcTestSet:
      if sp < s.len and s[sp] in r.data[arg]: return int(r.code[pc+1].arg)
      inc pc, 2
    of opcTestChar:
      if sp < s.len and s[sp] == char(arg): return int(r.code[pc+1].arg)
      inc pc, 2
    of opcRetAccept:
      accepted = true
      return -1
    of opcBegin:
      if sp == 0: pc = int(r.code[pc+1].arg)
      else: inc pc, 2
    of opcEnd:
      if sp >= s.len: pc = int(r.code[pc+1].arg)
      else: inc pc, 2
    of opcWordBound:
      if atWordBoundary(s, sp) == (arg == 0): pc = int(r.code[pc+1].arg)
      else: inc pc, 2
    else:
      return -1 # `opcRet`; the match code of a searched pattern has no groups

func advance*(t: var Threads; r: Regex; s: string; sp: int;
              best, bestEnd: var int) =
  ## Moves every thread over `s[sp]`. A thread whose match ends at `sp`
  ## updates `best` (the leftmost start that has matched, `-1` before) and
  ## `bestEnd`; the threads that started after `best` are dropped.
  inc t.step
  t.nextPc.setLen 0
  t.nextStart.setLen 0
  for i in 0 ..< t.pc.len:
    let st = t.start[i]
    if best >= 0 and st > best: break
    var accepted = false
    let d = stepState(r, t.pc[i], s, sp, accepted)
    if accepted:
      if best < 0 or st < best:
        best = st
        bestEnd = sp
      elif st == best and sp > bestEnd:
        bestEnd = sp
    if d >= 0 and t.seen[d] != t.step and (best < 0 or st <= best):
      t.seen[d] = t.step
      t.nextPc.add d
      t.nextStart.add st
  swap t.pc, t.nextPc
  swap t.start, t.nextStart

func canStart*(r: Regex; s: string; sp: int): bool {.inline.} =
  ## Can a match start at `sp`?
  result = r.firstBytes == AllBytes or (sp >= 0 and sp < s.len and
                                        s[sp] in r.firstBytes)

func attempt*(r: Regex; s: string; start: Natural; reach: var int): int =
  ## `matchLen` for a pattern without back references, also reporting the
  ## last position the automaton looked at in `reach`.
  var caps: seq[Capture] = @[]
  var endPos = start
  let rule = run[false](r.code, r.data, r.startAt, s, caps, start, endPos,
                        reach)
  result = if rule <= 0: -1 else: endPos - start

func runThreads*(t: var Threads; r: Regex; s: string; sp: var Natural;
                 until: int; best, bestEnd: var int) =
  ## Runs a thread for every start from `sp` on that can match, at least up
  ## to `until` and then until no thread is alive. `sp` is left where the
  ## threads gave out, past `s.len` when they reached the end.
  initThreads t, r
  while true:
    if best < 0 and canStart(r, s, sp): spawn t, r, sp
    advance t, r, s, sp, best, bestEnd
    inc sp
    if sp > s.len: return
    if not alive(t) and (best >= 0 or sp > until): return
