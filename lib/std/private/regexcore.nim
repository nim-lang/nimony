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
## * `std/regex` compiles a pattern at **runtime** and walks the resulting DFA
##   as bytecode;
## * `std/deps/regex`, the plugin behind `std/regex`'s `lex` construct, runs the
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

const
  MaxLabel* = 255
    ## Highest DFA/NFA state number. States are `1..MaxLabel` (`0` is the NFA's
    ## start state and doubles as "no transition"), so a pattern whose NFA
    ## needs more than this is rejected with `TooComplex` rather than silently
    ## mis-compiled. The bound is what lets a state *set* be a `set[Label]` —
    ## 32 bytes, no allocation — which the subset construction leans on
    ## heavily.

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

  RegexNode* = ref object ## a node of the parsed regular expression
    kind*: RegexKind
    a*, b*: RegexNode ## sub-expressions; which ones are used depends on `kind`
    c*: char          ## the character for `reChar`, the index for
                      ## `reCapture`/`reBackref`
    s*: string        ## the literal for `reStr`
    cc*: set[char]    ## the class for `reCClass`
    rule*: int        ## `> 0` marks a final state and names the rule that
                      ## matched there; `0` means "not final"

  RegexFlag* = enum ## how a pattern is parsed
    reExtended,     ## ignore unescaped spaces and tabs
    reNoBackrefs,   ## treat `\1` as a character literal, not a back reference
    reNoCaptures    ## `(x)` means the same as `(?:x)`

const
  wordChars* = {'A'..'Z', 'a'..'z', '0'..'9', '_', '\128'..'\255'}
  whitespace* = {'\1'..'\32'}
  digits* = {'0'..'9'}

# ---------------------------------------------------------------------------
# AST construction
# ---------------------------------------------------------------------------

proc newNode(kind: RegexKind): RegexNode =
  result = RegexNode(kind: kind, a: nil, b: nil, c: '\0', s: "", cc: {},
                     rule: 0)

proc epsExpr*(): RegexNode =
  result = newNode(reEps)

proc charExpr*(c: char): RegexNode =
  result = newNode(reChar)
  result.c = c

proc backrefExpr*(x: int): RegexNode =
  result = newNode(reBackref)
  result.c = char(x)

proc strExpr*(str: string): RegexNode =
  if str.len == 1:
    result = charExpr(str[0])
  else:
    result = newNode(reStr)
    result.s = str

proc cclassExpr*(charset: set[char]): RegexNode =
  result = newNode(reCClass)
  result.cc = charset

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

proc getChar(c: var ReCtx; inClass: bool): RegexNode =
  ## One character or escape sequence. `inClass` selects the `[...]` reading of
  ## the escapes that mean different things there (`\b` is a backspace inside a
  ## class and a word boundary outside it).
  if not inClass:
    skipSpaces c
  if c.pos < c.buf.len and c.buf[c.pos] != '\\':
    result = charExpr(c.buf[c.pos])
    inc c.pos
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
    of 's':
      result = cclassExpr(whitespace)
      inc c.pos, 2
    of 'S':
      result = cclassExpr({'\1'..'\255'} - whitespace)
      inc c.pos, 2
    of 'd':
      result = cclassExpr(digits)
      inc c.pos, 2
    of 'D':
      result = cclassExpr({'\1'..'\255'} - digits)
      inc c.pos, 2
    of 'w':
      result = cclassExpr(wordChars)
      inc c.pos, 2
    of 'W':
      result = cclassExpr({'\1'..'\255'} - wordChars)
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
          result = charExpr(char(val))
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
        result = charExpr(ch)
        inc c.pos, 2

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
      s.add al.c
    else:
      error c, "only single characters are allowed in a quoted string"
      return epsExpr()
  if c.pos < c.buf.len:
    inc c.pos # skip the closing quote
  else:
    error c, "'\"' expected"
  result = strExpr(s)

proc parseCClass(c: var ReCtx): RegexNode =
  var cc: set[char] = {}
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
      incl cc, a.c
      if c.pos < c.buf.len and c.buf[c.pos] == '-':
        inc c.pos
        if c.pos < c.buf.len and c.buf[c.pos] == ']':
          incl cc, '-'
          break
        let b = getChar(c, true)
        if c.err.len > 0: return epsExpr()
        if b.kind == reChar:
          if a.c <= b.c:
            cc = cc + {a.c .. b.c}
          else:
            error c, "invalid character range in '[...]'"
            return epsExpr()
        elif b.kind == reCClass:
          incl cc, '-'
          cc = cc + b.cc
        else:
          error c, "invalid character range in '[...]'"
          return epsExpr()
    elif a.kind == reCClass:
      cc = cc + a.cc
    else:
      error c, "'" & c.buf[c.pos] & "' cannot be part of a character class"
      return epsExpr()
  if c.pos < c.buf.len and c.buf[c.pos] == ']':
    inc c.pos
  else:
    error c, "']' expected"
    return epsExpr()
  result = if caret: cclassExpr({'\1'..'\xFF'} - cc) else: cclassExpr(cc)

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
    result = cclassExpr({'\1'..'\xFF'})
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
    result = charExpr(if c.pos < c.buf.len: c.buf[c.pos] else: '\0')
    inc c.pos
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

type
  Alphabet* = object
    ## One "letter" the automaton can step over: an ordinary character, or one
    ## of the zero-width assertions and capture markers that ride the same
    ## transition machinery.
    kind*: RegexKind
    val*: char

  Label* = range[0..MaxLabel]
  LabelSet* = set[Label]

  DfaEdge* = object
    cond*: Alphabet
    dest*: Label

  NfaEdge* = object
    cond*: Alphabet
    dest*: LabelSet

  Dfa* = object
    startState*: int ## not always 1 before minimization
    stateCount*: int ## states are `1 .. stateCount`
    captures*, backrefs*: int
    ruleCount*: int  ## highest rule number; rule 0 means "no match"
    trans*: array[Label, seq[DfaEdge]]
    toRules*: array[Label, int]

  Nfa* = object
    captures*, backrefs*, stateCount*: int
    trans*: array[Label, seq[NfaEdge]]
    toRules*: array[Label, int]

  BuildStatus* = enum ## why the pipeline gave up, if it did
    Ok
    TooComplex ## the automaton needs more than `MaxLabel` states

const
  alEpsilon* = Alphabet(kind: reEps, val: '\0')

func lab(i: int): Label {.inline.} =
  ## The one place where a state *number* becomes a state *index*. Every
  ## producer of state numbers below stops at `MaxLabel` and reports
  ## `TooComplex`, so the guard here never fires in a successful build; it is
  ## what lets the range checker see the invariant the algorithms maintain.
  if i >= 0 and i <= MaxLabel: result = Label(i)
  else: result = Label(0)

func `==`(a, b: Alphabet): bool {.inline.} =
  a.kind == b.kind and a.val == b.val

proc addTrans(src: var seq[NfaEdge]; c: Alphabet; d: Label) =
  for i in 0 ..< src.len:
    if src[i].cond == c:
      src[i].dest.incl d
      return
  src.add NfaEdge(cond: c, dest: {d})
  if c.kind == reEps and src.len != 1:
    # `closure` only ever looks at edge 0 for epsilon, so keep epsilon first.
    swap(src[0], src[src.len - 1])

proc addTrans(src: var seq[DfaEdge]; c: Alphabet; d: Label) =
  for i in 0 ..< src.len:
    if src[i].cond == c:
      src[i].dest = d
      return
  src.add DfaEdge(cond: c, dest: d)

type
  NfaBuilder = object
    a: Nfa
    overflow: bool ## a state number ran past `MaxLabel`

proc newState(b: var NfaBuilder; s: int): int =
  ## Guards every state-number increment. Once `overflow` is set the walk keeps
  ## running — so it still terminates — but stops writing anything.
  if s >= MaxLabel:
    b.overflow = true
    result = MaxLabel
  else:
    result = s + 1

proc trans(b: var NfaBuilder; s: int; c: Alphabet; d: int) {.inline.} =
  if not b.overflow:
    addTrans(b.a.trans[lab(s)], c, lab(d))

proc auxRegExprToNfa(r: RegexNode; b: var NfaBuilder; currState: int): int =
  ## Thompson's construction; returns the state the sub-expression ends in.
  result = currState
  if r == nil or b.overflow: return
  case r.kind
  of reEps:
    trans b, result, alEpsilon, newState(b, result)
    result = newState(b, result)
  of reChar:
    trans b, result, Alphabet(kind: reChar, val: r.c), newState(b, result)
    result = newState(b, result)
  of reWordBoundary, reWordBoundaryNot, reBegin, reEnd:
    trans b, result, Alphabet(kind: r.kind, val: '\0'), newState(b, result)
    result = newState(b, result)
  of reStr:
    for i in 0 ..< r.s.len:
      trans b, result, Alphabet(kind: reChar, val: r.s[i]), newState(b, result)
      result = newState(b, result)
      if b.overflow: return
  of reCat:
    result = auxRegExprToNfa(r.a, b, result)
    result = auxRegExprToNfa(r.b, b, result)
  of reCClass:
    trans b, result, alEpsilon, newState(b, result)
    result = newState(b, result)
    # `0 .. 255` in `int` rather than `for c in '\0'..'\xFF'`: `inc` on a
    # `char` wraps round at `high(char)`, so the char spelling never ends.
    for i in 0 .. 255:
      let c = char(i)
      if c in r.cc:
        trans b, result, Alphabet(kind: reChar, val: c), newState(b, result)
    result = newState(b, result)
  of reStar:
    # one transition too many is drawn here, which is harmless
    let aa = auxRegExprToNfa(r.a, b, result)
    trans b, result, alEpsilon, newState(b, aa)
    trans b, aa, alEpsilon, newState(b, aa)
    trans b, newState(b, aa), alEpsilon, result
    result = newState(b, aa)
  of rePlus:
    result = auxRegExprToNfa(catExpr(r.a, starExpr(r.a)), b, result)
  of reOpt:
    result = auxRegExprToNfa(altExpr(r.a, epsExpr()), b, result)
  of reAlt:
    trans b, result, alEpsilon, newState(b, result)
    result = newState(b, result)
    let oldState = result
    let aa = auxRegExprToNfa(r.a, b, result)
    let bb = auxRegExprToNfa(r.b, b, newState(b, aa))
    trans b, oldState, alEpsilon, newState(b, aa)
    trans b, aa, alEpsilon, newState(b, bb)
    trans b, bb, alEpsilon, newState(b, bb)
    result = newState(b, bb)
  of reCapture, reCaptureEnd:
    b.a.captures = max(b.a.captures, int(r.c))
    trans b, result, Alphabet(kind: reCapture, val: r.c), newState(b, result)
    result = newState(b, result)
    result = auxRegExprToNfa(r.a, b, result)
    trans b, result, Alphabet(kind: reCaptureEnd, val: r.c), newState(b, result)
    result = newState(b, result)
  of reBackref:
    b.a.backrefs = max(b.a.backrefs, int(r.c))
    trans b, result, Alphabet(kind: reBackref, val: r.c), newState(b, result)
    result = newState(b, result)
  if r.rule != 0 and not b.overflow:
    b.a.toRules[lab(result)] = r.rule

proc regExprToNfa(r: RegexNode; a: var Nfa): bool =
  ## `false` when the expression needs more than `MaxLabel` states.
  var b = NfaBuilder(a: a, overflow: false)
  let last = auxRegExprToNfa(r, b, 0)
  b.a.stateCount = last
  a = b.a
  result = not b.overflow

proc fullAlphabet(captures, backrefs: int): seq[Alphabet] =
  ## Every letter the subset construction has to consider. Characters first,
  ## then the markers, so the common case stays contiguous.
  result = @[]
  for i in 0 .. 255:  # in `int`, see `auxRegExprToNfa`
    result.add Alphabet(kind: reChar, val: char(i))
  for x in 1 .. backrefs:
    result.add Alphabet(kind: reBackref, val: char(x))
  for x in 1 .. captures:
    result.add Alphabet(kind: reCapture, val: char(x))
    result.add Alphabet(kind: reCaptureEnd, val: char(x))
  result.add Alphabet(kind: reBegin, val: '\0')
  result.add Alphabet(kind: reEnd, val: '\0')
  result.add Alphabet(kind: reWordBoundary, val: '\0')
  result.add Alphabet(kind: reWordBoundaryNot, val: '\0')

proc closure(a: Nfa; s: LabelSet): LabelSet =
  ## Epsilon closure of `s`. `addTrans` guarantees an epsilon edge is edge 0.
  result = s
  var prev: LabelSet = {}
  while true:
    prev = result
    for l in 0 .. a.stateCount:
      if lab(l) in prev:
        if a.trans[lab(l)].len > 0 and a.trans[lab(l)][0].cond.kind == reEps:
          result = result + a.trans[lab(l)][0].dest
    if prev == result: break

proc getDest(a: seq[NfaEdge]; c: Alphabet): LabelSet =
  result = {}
  for t in a:
    if t.cond == c: return t.dest

proc getDest(a: seq[DfaEdge]; c: Alphabet): Label =
  result = Label(0)
  for t in a:
    if t.cond == c: return t.dest

proc getDfaEdge(a: Nfa; d: LabelSet; c: Alphabet): LabelSet =
  var tmp: LabelSet = {}
  for l in 0 .. a.stateCount:
    if lab(l) in d:
      tmp = tmp + getDest(a.trans[lab(l)], c)
  result = closure(a, tmp)

proc searchInStates(states: openArray[LabelSet]; p: int; e: LabelSet): int =
  ## Index of `e` among `states[0..p]`, or `-1`.
  result = -1
  for i in 0 .. p:
    if states[i] == e: return i

proc nfaToDfa(a: Nfa; b: var Dfa; alphabet: openArray[Alphabet]): bool =
  ## Subset construction (see "Modern Compiler Implementation"). `false` when
  ## the DFA would need more than `MaxLabel` states.
  var states: seq[LabelSet] = @[]
  states.add {}
  states.add closure(a, {Label(0)}) # 0 is the NFA's start state
  var p = 1
  var j = 0
  while j <= p:
    for c in alphabet:
      let e = getDfaEdge(a, states[j], c)
      let i = searchInStates(states, p, e)
      if i >= 0:
        addTrans(b.trans[lab(j)], c, lab(i))
      else:
        inc p
        if p > MaxLabel: return false
        states.add e
        addTrans(b.trans[lab(j)], c, lab(p))
    inc j
  for d in 0 .. j - 1:
    var minRule = high(int)
    for i in 0 .. MaxLabel:
      if lab(i) in states[d]:
        if minRule > a.toRules[lab(i)] and a.toRules[lab(i)] != 0:
          minRule = a.toRules[lab(i)]
    if minRule == high(int):
      b.toRules[lab(d)] = 0
    else:
      b.toRules[lab(d)] = minRule
      if minRule > b.ruleCount: b.ruleCount = minRule
  b.stateCount = j - 1
  b.startState = 1 # the subset construction always ends up with 1 here
  b.captures = a.captures
  b.backrefs = a.backrefs
  result = true

proc getPreds(a: Dfa; s: LabelSet; c: Alphabet): LabelSet =
  ## The states that reach `s` over `c`.
  result = {}
  for i in 1 .. a.stateCount:
    for t in a.trans[lab(i)]:
      if t.cond == c and t.dest in s:
        incl result, lab(i)

proc card(s: LabelSet; maxState: int): int =
  result = 0
  for i in 1 .. maxState:
    if lab(i) in s: inc result

proc choose(s: LabelSet; maxState: int): int =
  ## An arbitrary member of `s`; `0` (an invalid state) when it is empty.
  result = 0
  for i in 1 .. maxState:
    if lab(i) in s: return i

proc optimizeDfa(a: Dfa; b: var Dfa; alphabet: openArray[Alphabet]): bool =
  ## Hopcroft's algorithm. Every state carries the rule it accepts, so the
  ## initial partition is by rule rather than the usual final/non-final split.
  b.captures = a.captures
  b.backrefs = a.backrefs
  var w = newSeq[LabelSet](a.ruleCount + 1)
  var p = newSeq[LabelSet](a.ruleCount + 1)
  for d in 1 .. a.stateCount:
    incl w[a.toRules[lab(d)]], lab(d)
    incl p[a.toRules[lab(d)]], lab(d)
  while w.len > 0:
    let s = w.pop()
    for c in alphabet:
      let inv = getPreds(a, s, c)
      if inv == {}: continue # much the common case; skip the partition walk
      var j = p.len - 1
      while j >= 0:
        let r = p[j]
        if (r * inv != {}) and not (r <= inv):
          let x = r * inv
          let y = r - x
          p[j] = x
          p.add y
          let findRes = searchInStates(w, w.len - 1, r)
          if findRes >= 0:
            w[findRes] = x
            w.add y
          else:
            if card(x, a.stateCount) <= card(y, a.stateCount):
              w.add x
            else:
              w.add y
        dec j
  if p.len > MaxLabel: return false
  b.stateCount = p.len
  b.ruleCount = a.ruleCount
  for j in 0 ..< p.len:
    if p[j] != {}:
      let rep = choose(p[j], a.stateCount)
      if lab(a.startState) in p[j]: b.startState = j + 1
      b.toRules[lab(j + 1)] = a.toRules[lab(rep)]
      for c in alphabet:
        let dest = getDest(a.trans[lab(rep)], c)
        if dest != Label(0):
          for k in 0 ..< p.len:
            if dest in p[k]:
              addTrans b.trans[lab(j + 1)], c, lab(k + 1)
              break
  result = true

func allTransitions*(a: Dfa; source, dest: Label): (seq[Alphabet], set[char]) =
  ## Splits the `source -> dest` edges into the assertions and markers (which
  ## have to be tested one by one) and the plain characters (which collapse
  ## into one set test). A single character is handed back as an `Alphabet`
  ## too, because `x == 'a'` beats `x in {'a'}` in the generated code.
  var others: seq[Alphabet] = @[]
  var cs: set[char] = {}
  if a.trans[source].len > 0:
    var card = 0
    var lastChar = -1
    for x in a.trans[source]:
      if x.dest == dest:
        if x.cond.kind == reChar:
          inc card
          if lastChar < 0: lastChar = int(x.cond.val)
          incl cs, x.cond.val
        else:
          others.add x.cond
    if card == 1:
      cs = {}
      others.add Alphabet(kind: reChar, val: char(lastChar))
  result = (others, cs)

iterator allDests*(a: Dfa; source: Label): Label =
  ## Every state reachable from `source`, each yielded once and in state order.
  if a.trans[source].len > 0:
    var dests: LabelSet = {}
    for x in a.trans[source]: dests.incl x.dest
    for d in dests: yield d

func getRule*(a: Dfa; s: Label): int {.inline.} = a.toRules[s]

func state*(a: Dfa; i: int): Label {.inline.} =
  ## `i` as a state index of `a`. Callers loop `1 .. a.stateCount`, which the
  ## build pipeline has already capped at `MaxLabel`.
  lab(i)

# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------

proc buildDfa(big: RegexNode; dfa: var Dfa): BuildStatus =
  var n = default(Nfa)
  if not regExprToNfa(big, n):
    return TooComplex
  let alphabet = fullAlphabet(n.captures, n.backrefs)
  var d = default(Dfa)
  if not nfaToDfa(n, d, alphabet):
    return TooComplex
  dfa = default(Dfa)
  if not optimizeDfa(d, dfa, alphabet):
    return TooComplex
  result = Ok

proc rulesToDfa*(patterns: openArray[string]; flags: set[RegexFlag];
                 dfa: var Dfa; err: var string): BuildStatus =
  ## The whole pipeline: parse every pattern, tag it with its rule number,
  ## alternate them into one expression and run that through NFA → DFA →
  ## minimization. Rule numbers are 1-based and follow `patterns`' order, which
  ## is what makes "the earlier pattern wins a tie" the rule everywhere.
  err = ""
  result = Ok
  var big: RegexNode = nil
  for i in 0 ..< patterns.len:
    var e = ""
    let rex = parseRegExpr(patterns[i], flags, e)
    if e.len > 0:
      err = e
      return Ok
    rex.rule = i + 1
    if big == nil: big = rex
    else: big = altExpr(big, rex)
  if big == nil:
    err = "at least one pattern is required"
    return Ok
  result = buildDfa(big, dfa)

proc regexToDfa*(pattern: string; flags: set[RegexFlag]; dfa: var Dfa;
                 err: var string): BuildStatus =
  ## `rulesToDfa` for the single-pattern case, plus the capture check that only
  ## makes sense there.
  err = ""
  var e = ""
  let rex = parseRegExpr(pattern, flags, e)
  if e.len > 0:
    err = e
    return Ok
  if containsInvalidCapture(rex):
    err = "captures inside an alternation are not supported"
    return Ok
  rex.rule = 1
  result = buildDfa(rex, dfa)

# ---------------------------------------------------------------------------
# Bytecode
# ---------------------------------------------------------------------------
#
# The DFA is walked as a tiny program rather than as a table: a state is a run
# of test/jump pairs ended by a `ret` carrying the rule it accepts. That keeps
# the common step — "test the character against a set, jump" — two adjacent
# instructions, and it is what lets `std/regex`'s `re` emit a finished automaton
# as a literal instead of building tables at run time.

type
  RegexOpcode* = enum
    opcRet          ## stop; the argument is the rule that matched (0 = none)
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
    ## One instruction. Public only because `std/regex`'s `re` emits these as a
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

  Capture* = object ## The bounds of one capture group, both ends inclusive.
    first*, last*: int

const
  CaptureOpen* = -2 ## `last` while the group is still being matched

func emptyRegex*(): Regex =
  Regex(code: @[], data: @[], startAt: 0, captures: 0,
        capCode: @[], capData: @[], capStartAt: 0)

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
    let rule = getRule(a, state(a, src))
    for dest in allDests(a, state(a, src)):
      # The "match longest, but only sometimes" rule regexes are known for:
      # once a state accepts, only transitions that stay within the *same* rule
      # may extend the match, or a later and longer rule would swallow an
      # earlier one's answer.
      if rule == 0 or rule == getRule(a, dest):
        let (others, cs) = allTransitions(a, state(a, src), dest)
        for x in others: genCapture(res, x, int(dest))
        if cs != {}:
          gen res, opcTestSet, genData(res, cs)
          gen res, opcTJmp, int(dest)
        for x in others: genTest(res, x, int(dest))
    if stateToLabel[src] != res.code.len or rule != 0:
      # A state with neither transitions nor a rule is only ever fallen into
      # from the `ret` above it, so it needs no `ret` of its own.
      gen res, opcRet, rule
  # The jumps were emitted with state numbers; patch them to code offsets now
  # that every state's offset is known.
  for i in 0 ..< res.code.len:
    if res.code[i].opc == opcTJmp:
      res.code[i] = RegexInstr(opc: opcTJmp,
                               arg: int32(stateToLabel[res.code[i].arg]))
  res.startAt = stateToLabel[a.startState]

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
  case regexToDfa(pattern, flags, dfa, err)
  of TooComplex:
    err = "regular expression is too complex: it needs more than " &
          $MaxLabel & " automaton states"
    result = false
  of Ok:
    if err.len > 0:
      result = false
    else:
      genBytecode(dfa, dest)
      result = true

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

func backrefMatches(s: string; sp: int; c: Capture): bool =
  ## Does the text at `sp` repeat what `c` captured?
  var i = c.first
  var k = sp
  while true:
    if i > c.last: return true
    if k >= s.len or s[k] != s[i]: return false
    inc k
    inc i

func run(code: seq[RegexInstr]; data: seq[set[char]]; startAt: int;
         s: string; caps: var seq[Capture]; start: int; endPos: var int): int =
  ## One automaton over `s` from `start`. Returns the rule that matched (`0`
  ## for none) and reports the position just past the match in `endPos`.
  result = 0
  endPos = start
  if code.len == 0: return
  var pc = startAt
  var sp = start
  while true:
    let instr = code[pc]
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
      endPos = sp
      return arg
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
      if caps.len <= arg: setLen(caps, arg + 1)
      caps[arg] = Capture(first: sp, last: CaptureOpen)
      pc = int(code[pc+1].arg)
    of opcCaptureEnd:
      if caps.len <= arg: setLen(caps, arg + 1)
      caps[arg].last = sp - 1
      pc = int(code[pc+1].arg)
    of opcBackref:
      if arg < caps.len and backrefMatches(s, sp, caps[arg]):
        pc = int(code[pc+1].arg)
        inc sp, caps[arg].last - caps[arg].first + 1
      else:
        inc pc, 2
    of opcTJmp:
      # unreachable: a `TJmp` is only ever entered through the test above it
      return 0

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
  result = run(r.code, r.data, r.startAt, s, caps, start, endPos)
  if result > 0 and r.capCode.len > 0:
    var found: seq[Capture] = @[]
    var ignored = start
    if run(r.capCode, r.capData, r.capStartAt, s, found, start, ignored) > 0:
      caps = found
