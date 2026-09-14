#       Nifler2
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Plugin behind `parserrt`'s `grammar` template: an LL(1) parser generator
## over `(token kind, indentation class)`. See
## `doc/internals/parser_generator.md` for the notation.
##
## Input: `(stmts grammar (stmts <entry>...))`, one entry per production:
##
##   (cmd name "production" [(stmts actions...)])
##   (cmd (oconstr name (kv param Type)...) (suf "production" "T") ...)
##
## Output: direct-coded recursive descent, one `proc pRule*(p: var Parser...)`
## per rule and a `proc canRule*(p: Parser...): bool` for the rules whose
## alternatives carry predicates. There are no forward declarations: Nimony
## does not need them. The dispatch is
## an if-chain over `(p.tok.kind, indClass(p))`; there are no tables and no
## interpreter. A grammar with a conflict, an undeclared rule or a wrong arity
## is a compile error at the `grammar:` call.
##
## One file on purpose: the build graph gives a plugin executable its main
## source as the only input, so an edit to a module it imported would not
## rebuild it.

{.feature: "lenientnils".}

import std / [tables, sets, strutils]
import plugins

# The analysis: the notation parsed into `Node` trees, then nullability, FIRST
# and FOLLOW over `(TokenKind, IndentClass)` pairs -- the extended domain that
# makes Nim's indentation part of the LL(1) decision rather than a side
# channel -- and the checks that make a bad grammar a compile error.

# --------------------------------------------------------------- terminals

proc isKeyword(s: string): bool =
  case s
  of "addr", "and", "as", "asm", "bind", "block", "break", "case", "cast",
     "concept", "const", "continue", "converter", "defer", "discard",
     "distinct", "div", "do", "elif", "else", "end", "enum", "except",
     "export", "finally", "for", "from", "func", "if", "import", "in",
     "include", "interface", "is", "isnot", "iterator", "let", "macro",
     "method", "mixin", "mod", "nil", "not", "notin", "object", "of", "or",
     "out", "proc", "ptr", "raise", "ref", "return", "shl", "shr", "static",
     "template", "try", "tuple", "type", "using", "var", "when", "while",
     "xor", "yield": true
  else: false

proc punctuation(s: string): string =
  case s
  of "(": "tkParLe"
  of ")": "tkParRi"
  of "[": "tkBracketLe"
  of "]": "tkBracketRi"
  of "{": "tkCurlyLe"
  of "}": "tkCurlyRi"
  of "[.": "tkBracketDotLe"
  of ".]": "tkBracketDotRi"
  of "{.": "tkCurlyDotLe"
  of ".}": "tkCurlyDotRi"
  of "(.": "tkParDotLe"
  of ".)": "tkParDotRi"
  of ",": "tkComma"
  of ";": "tkSemiColon"
  of ":": "tkColon"
  of "::": "tkColonColon"
  of "=": "tkEquals"
  of ".": "tkDot"
  of "..": "tkDotDot"
  of "[:": "tkBracketLeColon"
  of "`": "tkAccent"
  else: ""

proc classKind(name: string): string =
  ## A token class's `TokKind`, or `""` when `name` is not a class.
  ## `tkOpr+isDotLike` is a refinement: the kind plus a predicate on the token.
  case name
  of "IDENT": "tkSymbol"
  of "COMMENT": "tkComment"
  of "EOF": "tkEof"
  of "KEYW": "tkAddr..tkYield"
  of "OPR": "tkOpr"
  of "DOTLIKEOP": "tkOpr+isDotLike"
  of "SIGILLIKEOP": "tkOpr+isSigilLike"
  of "INT_LIT": "tkIntLit"
  of "INT8_LIT": "tkInt8Lit"
  of "INT16_LIT": "tkInt16Lit"
  of "INT32_LIT": "tkInt32Lit"
  of "INT64_LIT": "tkInt64Lit"
  of "UINT_LIT": "tkUIntLit"
  of "UINT8_LIT": "tkUInt8Lit"
  of "UINT16_LIT": "tkUInt16Lit"
  of "UINT32_LIT": "tkUInt32Lit"
  of "UINT64_LIT": "tkUInt64Lit"
  of "FLOAT_LIT": "tkFloatLit"
  of "FLOAT32_LIT": "tkFloat32Lit"
  of "FLOAT64_LIT": "tkFloat64Lit"
  of "FLOAT128_LIT": "tkFloat128Lit"
  of "STR_LIT": "tkStrLit"
  of "RSTR_LIT": "tkRStrLit"
  of "TRIPLESTR_LIT": "tkTripleStrLit"
  of "GENERALIZED_STR_LIT": "tkGStrLit"
  of "GENERALIZED_TRIPLESTR_LIT": "tkGTripleStrLit"
  of "CHAR_LIT": "tkCharLit"
  of "CUSTOM_NUMERIC_LIT": "tkCustomLit"
  else: ""

proc isClass(name: string): bool {.inline.} = classKind(name).len > 0

proc tokenKind(terminal: string): string =
  ## The hardcoded mapping. Keywords are regular (`tk` & capitalize), the
  ## punctuation is a table, and anything else is an operator by spelling:
  ## `tkOpr"*"`.
  if isKeyword(terminal): result = "tk" & capitalizeAscii(terminal)
  else:
    result = punctuation(terminal)
    if result.len == 0: result = "tkOpr\"" & terminal & "\""

# --------------------------------------------------------------- grammar AST

type
  IndClass = enum
    icNoInd, icLt, icEq, icGt
  IndSet = set[IndClass]

const AnyInd: IndSet = {icNoInd, icLt, icEq, icGt}

type
  NodeKind = enum
    nSeq, nAlt, nOpt, nRep0, nRep1, nSepRep, nTerminal, nClass, nRule,
    nTag, nGuard, nPred, nAhead, nIndented, nWithInd, nBinary, nBinTail, nLa2,
    nDefault, nRaw,
    nEmpty,               # `.`: writes an absent child and consumes nothing
    nAtPos                # `%at`: the node's position is the next token's
  Node = ref object
    kind: NodeKind
    text: string         # terminal spelling / class / rule / tag / pred name
    anchored: bool       # `^tag[...]`, `@'x'`, `X?.`
    ind: IndSet
    kids: seq[Node]

  Entry = object
    name, src: string
    paramNames: seq[string]
    paramTypes: seq[int]      ## index into the caller's cursor list
    paramIsRule: seq[bool]    ## `R: rule`, which is monomorphized away
    line: int
    enterCode: seq[int]       ## `enter:` -- runs before the match
    leaveCode: seq[int]       ## `leave:` -- runs after it, match or not
    afterCode: seq[int]       ## a bare body, with `m` bound to the mark
    body: Node

  FirstSet = Table[string, IndSet]

  Grammar = object
    rules: Table[string, seq[Entry]]
    order: seq[string]                      # rules in declaration order
    nullable: Table[string, bool]
    first: Table[string, FirstSet]
    mask: Table[string, IndSet]              # rule -> exported indent mask
    follow: Table[string, FirstSet]          # rule -> FOLLOW
    params: Table[string, seq[string]]      # rule -> parameter names
    argsOf: Table[string, seq[Node]]         # parameter -> arguments seen
    errors: seq[string]

proc sortStrings(s: var seq[string]) =
  ## Insertion sort: the lists are short, and the order only has to be stable
  ## from one run to the next.
  for i in 1 ..< s.len:
    var j = i
    while j > 0 and s[j] < s[j-1]:
      swap s[j], s[j-1]
      dec j

proc newNode(k: NodeKind; text = ""): Node =
  Node(kind: k, text: text, anchored: false, ind: {}, kids: @[])

proc indSetStr(s: IndSet): string =
  result = "{"
  for c in low(IndClass) .. high(IndClass):
    if c in s:
      if result.len > 1: result.add ", "
      result.add (case c
                  of icNoInd: "NO_IND"
                  of icLt: "IND{<}"
                  of icEq: "IND{=}"
                  of icGt: "IND{>}")
  result.add "}"

proc error(g: var Grammar; rule: string; line: int; msg: string) =
  g.errors.add rule & " (line " & $line & "): " & msg

# --------------------------------------------------------------- mini-parser

type Scanner = object
  s: string
  pos: int
  rule: string
  line: int
  errors: seq[string]

proc err(sc: var Scanner; msg: string) =
  sc.errors.add sc.rule & " (line " & $sc.line & "): " & msg

proc ws(sc: var Scanner) =
  while sc.pos < sc.s.len and sc.s[sc.pos] in {' ', '\t', '\n', '\r'}: inc sc.pos

proc atNow(sc: Scanner; c: char): bool =
  ## No whitespace skipping: `name(` is a call, `name (` is a sequence.
  sc.pos < sc.s.len and sc.s[sc.pos] == c

proc at(sc: var Scanner; tok: string): bool =
  ws sc
  result = sc.pos + tok.len <= sc.s.len and sc.s.substr(sc.pos, sc.pos+tok.len-1) == tok

proc take(sc: var Scanner; tok: string): bool =
  if at(sc, tok):
    inc sc.pos, tok.len
    result = true
  else:
    result = false

proc parseAlt(sc: var Scanner): Node

proc parseIdent(sc: var Scanner): string =
  ws sc
  let start = sc.pos
  while sc.pos < sc.s.len and (sc.s[sc.pos] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}):
    inc sc.pos
  result = sc.s.substr(start, sc.pos-1)

proc parsePrim(sc: var Scanner): Node =
  ws sc
  if sc.pos >= sc.s.len:
    err sc, "unexpected end of production"
    return newNode(nSeq)

  let c = sc.s[sc.pos]
  if c == '.':
    inc sc.pos
    return newNode(nEmpty)
  if c == '{':
    inc sc.pos
    let start = sc.pos
    var depth = 1
    while sc.pos < sc.s.len:
      if sc.s[sc.pos] == '{': inc depth
      elif sc.s[sc.pos] == '}':
        dec depth
        if depth == 0: break
      inc sc.pos
    result = newNode(nRaw, sc.s.substr(start, sc.pos-1))
    inc sc.pos
    return
  if c == '%':
    inc sc.pos
    let w = parseIdent(sc)
    if w == "at": return newNode(nAtPos, "%at")
    if w != "else": err sc, "unknown marker %" & w
    return newNode(nDefault, "%else")
  if c == '@' and sc.pos+1 < sc.s.len and sc.s[sc.pos+1] == '\'':
    # `@'not'`: a terminal that is also *emitted*, as a leaf. A plain terminal
    # is punctuation and leaves nothing behind; a keyword operator or the
    # `..` of a prefix expression is content.
    inc sc.pos
    result = parsePrim(sc)
    result.anchored = true
    return
  if c == '\'':
    inc sc.pos
    let start = sc.pos
    while sc.pos < sc.s.len and sc.s[sc.pos] != '\'': inc sc.pos
    result = newNode(nTerminal, sc.s.substr(start, sc.pos-1))
    inc sc.pos
    return
  if c == '(':
    inc sc.pos
    result = parseAlt(sc)
    if not take(sc, ")"): err sc, "expected ')'"
    return
  if c == '&' or c == '!':
    inc sc.pos
    ws sc
    let inner = parsePrim(sc)
    if inner.kind == nRule and not isClass(inner.text) and
       not inner.text.startsWith("IND"):
      # `&name` on a lowercase name is a semantic predicate until
      # `reclassify` finds a declared rule by that name. Arguments come along:
      # `&inTypeDesc(mode)` is how `parser.nim`'s `mode` reaches a predicate.
      result = newNode(nPred, inner.text)
      result.kids = inner.kids
    else:
      result = newNode(nAhead)
      result.kids.add inner
    return
  if c == '^':
    inc sc.pos
    let name = parseIdent(sc)
    if not take(sc, "["): err sc, "expected '[' after ^" & name
    result = newNode(nTag, name)
    result.anchored = true
    result.kids.add parseAlt(sc)
    if not take(sc, "]"): err sc, "expected ']' closing ^" & name & "["
    return

  let name = parseIdent(sc)
  if name.len == 0:
    err sc, "unexpected character '" & $c & "'"
    inc sc.pos
    return newNode(nSeq)

  if name == "IND" and atNow(sc, '{'):
    inc sc.pos
    let relStart = sc.pos
    while sc.pos < sc.s.len and sc.s[sc.pos] != '}': inc sc.pos
    let rel = sc.s.substr(relStart, sc.pos-1)
    inc sc.pos
    result = newNode(nGuard, "IND{" & rel & "}")
    if rel == "=": result.ind = {icEq}
    elif rel == ">": result.ind = {icGt}
    elif rel == "<": result.ind = {icLt}
    else:
      err sc, "unknown indentation relation '" & rel & "'"
      result.ind = AnyInd
    return
  if name == "NO_IND":
    result = newNode(nGuard, "NO_IND")
    result.ind = {icNoInd}
    return
  if atNow(sc, '['):
    inc sc.pos
    result = newNode(nTag, name)
    result.kids.add parseAlt(sc)
    if not take(sc, "]"): err sc, "expected ']' closing " & name & "["
    return
  if atNow(sc, '('):
    inc sc.pos
    var args: seq[Node] = @[]
    if not at(sc, ")"):
      while true:
        args.add parseAlt(sc)
        if not take(sc, ","): break
    if not take(sc, ")"): err sc, "expected ')' closing " & name & "("
    case name
    of "indented": result = newNode(nIndented)
    of "withInd": result = newNode(nWithInd)
    of "binary": result = newNode(nBinary, name)
    of "binaryTail": result = newNode(nBinTail, name)
    of "la2": result = newNode(nLa2)
    else: result = newNode(nRule, name)
    result.kids = args
    return
  if isClass(name):
    return newNode(nClass, name)
  return newNode(nRule, name)

proc parsePostfix(sc: var Scanner): Node =
  result = parsePrim(sc)
  while true:
    if take(sc, "^*") or take(sc, "^+"):
      let r = newNode(nSepRep, sc.s.substr(sc.pos-2, sc.pos-1))
      r.kids.add result
      r.kids.add parsePrim(sc)
      result = r
    elif take(sc, "?"):
      let r = newNode(nOpt)
      r.kids.add result
      result = r
      if atNow(sc, '.'):
        # `X?.`: absent means an explicit `.` in the output, not nothing
        inc sc.pos
        r.anchored = true
    elif take(sc, "*"):
      let r = newNode(nRep0)
      r.kids.add result
      result = r
    elif take(sc, "+"):
      let r = newNode(nRep1)
      r.kids.add result
      result = r
    else:
      break

proc startsItem(sc: var Scanner): bool =
  ws sc
  if sc.pos >= sc.s.len: return false
  result = sc.s[sc.pos] notin {'|', ')', ']', ','}

proc parseSeq(sc: var Scanner): Node =
  result = newNode(nSeq)
  while startsItem(sc):
    result.kids.add parsePostfix(sc)
  if result.kids.len == 1: result = result.kids[0]

proc parseAlt(sc: var Scanner): Node =
  let first = parseSeq(sc)
  if not at(sc, "|"): return first
  result = newNode(nAlt)
  result.kids.add first
  while take(sc, "|"):
    result.kids.add parseSeq(sc)

proc parseProduction(g: var Grammar; e: var Entry) =
  var sc = Scanner(s: e.src, pos: 0, rule: e.name, line: e.line, errors: @[])
  e.body = parseAlt(sc)
  ws sc
  if sc.pos < sc.s.len:
    err sc, "trailing junk: " & sc.s.substr(sc.pos)
  for m in sc.errors: g.errors.add m

# --------------------------------------------------------------- analysis

proc union(a: var FirstSet; b: FirstSet) =
  for k, v in pairs(b):
    a[k] = a.getOrDefault(k) + v

proc restrict(a: var FirstSet; ind: IndSet) =
  var res = initTable[string, IndSet]()
  for k, v in pairs(a):
    let n = v * ind
    if n != {}: res[k] = n
  a = res

proc isNullable(g: Grammar; n: Node): bool =
  case n.kind
  of nSeq:
    for k in n.kids:
      if not isNullable(g, k): return false
    result = true
  of nAlt:
    for k in n.kids:
      if isNullable(g, k): return true
    result = false
  of nOpt, nRep0, nEmpty, nAtPos: result = true
  of nRep1, nSepRep: result = isNullable(g, n.kids[0])
  of nTerminal, nClass: result = false
  of nRule: result = g.nullable.getOrDefault(n.text)
  of nTag, nIndented, nWithInd: result = isNullable(g, n.kids[0])
  of nGuard, nPred, nAhead, nDefault, nRaw: result = true   # zero-width
  of nBinary: result = n.kids.len > 0 and isNullable(g, n.kids[0])
  of nBinTail: result = true    # the operator loop may run zero times
  of nLa2: result = n.kids.len > 0 and isNullable(g, n.kids[0])

proc firstOf(g: Grammar; n: Node): FirstSet

proc leadMask(g: Grammar; n: Node): IndSet =
  ## The indentation classes a zero-width construct still allows. `AnyInd`
  ## means "constrains nothing".
  if not isNullable(g, n): return AnyInd   # never "matches nothing"
  case n.kind
  of nGuard: result = n.ind
  of nSeq:
    # "Matches nothing" means EVERY item matched nothing, so every item's
    # constraint applies -- including a nullable one that *could* have consumed
    # a token. `optInd` is `COMMENT? validInd`: skipping the comment does not
    # excuse the indentation, and treating the optional COMMENT as the end of
    # the mask lost `validInd` entirely.
    result = AnyInd
    for k in n.kids:
      result = result * leadMask(g, k)
      if not isNullable(g, k): return result
  of nAlt:
    # only an alternative that can match *nothing* constrains what follows.
    # `(';' | IND{=})` imposes IND{=} exactly when the ';' was absent, so the
    # non-nullable alternative must not widen the mask to AnyInd.
    result = {}
    for k in n.kids:
      if isNullable(g, k): result = result + leadMask(g, k)
  of nRule:
    result = if g.mask.hasKey(n.text): g.mask.getOrDefault(n.text) else: AnyInd
  of nTag: result = leadMask(g, n.kids[0])
  of nIndented: result = {icGt}
  else: result = AnyInd

proc firstOf(g: Grammar; n: Node): FirstSet =
  result = initTable[string, IndSet]()
  case n.kind
  of nSeq:
    var carried: IndSet = AnyInd
    for k in n.kids:
      if k.kind in {nPred, nAhead, nDefault}: continue
      var f = firstOf(g, k)
      if f.len > 0:
        restrict(f, carried)
        union(result, f)
      if not isNullable(g, k): return
      # It may have matched nothing, in which case whatever constraint it
      # imposes in that case applies to the next token. For a pure guard that
      # is the guard itself; for `(';' | IND{=})` it is IND{=}, contributed by
      # the nullable alternative alone.
      carried = carried * leadMask(g, k)
  of nAlt:
    for k in n.kids: union(result, firstOf(g, k))
  of nOpt, nRep0, nRep1, nSepRep: result = firstOf(g, n.kids[0])
  of nTerminal: result[tokenKind(n.text)] = AnyInd
  of nClass: result[classKind(n.text)] = AnyInd
  of nRule:
    if g.first.hasKey(n.text): result = g.first.getOrDefault(n.text)
    elif g.argsOf.hasKey(n.text):
      # a rule-valued parameter: over-approximate by every argument passed
      for a in g.argsOf.getOrDefault(n.text): union(result, firstOf(g, a))
  of nTag: result = firstOf(g, n.kids[0])
  of nIndented:
    # The body's FIRST set is measured against the INNER indentation, and
    # `pushInd` makes the first token of the block define it -- so a token that
    # is `IND{=}` inside is `IND{>}` outside, and one that is not `IND{=}`
    # inside cannot be the first token at all. (`restrict` to `{icGt}` instead
    # of this remapping made every left-recursive indented alternative
    # unreachable: `objectPart`'s own guard is `notInd`, which excludes
    # `IND{>}`, so the intersection was empty and the branch compiled to
    # `if false:`.) This is the outward counterpart of `remapIn`.
    for k, v in pairs(firstOf(g, n.kids[0])):
      if icEq in v: result[k] = {icGt}
  of nWithInd:
    # `withInd(...)` sets the inner indentation to whatever the first token's
    # is, *without* requiring it to be deeper -- `parser.nim`'s `withInd`
    # template, of which `semiStmtList` is the one use that is not already
    # guarded by `realInd`. So the first token is `IND{=}` inside whatever it
    # was outside (or NO_IND inside if it was NO_IND outside, since `currInd`
    # then becomes -1), and the outward map is the inverse: unconstrained.
    for k, v in pairs(firstOf(g, n.kids[0])):
      var o: IndSet = {}
      if icEq in v: o = AnyInd
      elif icNoInd in v: o = {icNoInd}
      if o != {}: result[k] = o
  of nBinTail: discard    # the head was parsed by whatever precedes it
  of nBinary, nLa2:
    if n.kids.len > 0: result = firstOf(g, n.kids[0])   # rest are Nim procs
  of nGuard, nPred, nAhead, nDefault, nRaw, nEmpty, nAtPos: discard

proc remapIn(t: FirstSet): FirstSet =
  ## Re-measure a FOLLOW set across an `indented(...)` boundary. The inner
  ## `currInd` is strictly greater than the outer one, so a token that was at
  ## the outer level is a dedent inside; a token that was already indented
  ## could be anything relative to the inner level.
  result = initTable[string, IndSet]()
  for k, v in pairs(t):
    var n: IndSet = {}
    if icNoInd in v: n = n + {icNoInd}
    if icEq in v or icLt in v: n = n + {icLt}
    if icGt in v: n = n + AnyInd
    if n != {}: result[k] = n

proc render(n: Node): string

proc atom(n: Node): string =
  ## `render`, parenthesized when the node would not bind as one item.
  if n.kind == nSeq and n.kids.len > 1: "(" & render(n) & ")" else: render(n)

proc render(n: Node): string =
  ## Canonical rendering, used for messages and for prefix comparison.
  case n.kind
  of nSeq:
    var parts: seq[string] = @[]
    for k in n.kids: parts.add render(k)
    result = parts.join(" ")
  of nAlt:
    var parts: seq[string] = @[]
    for k in n.kids: parts.add render(k)
    result = "(" & parts.join(" | ") & ")"
  of nOpt: result = atom(n.kids[0]) & (if n.anchored: "?." else: "?")
  of nEmpty: result = "."
  of nAtPos: result = "%at"
  of nRep0: result = atom(n.kids[0]) & "*"
  of nRep1: result = atom(n.kids[0]) & "+"
  of nSepRep: result = atom(n.kids[0]) & " " & n.text & " " & atom(n.kids[1])
  of nTerminal: result = (if n.anchored: "@'" else: "'") & n.text & "'"
  of nClass: result = n.text
  of nRule:
    if n.kids.len == 0: result = n.text
    else:
      var parts: seq[string] = @[]
      for k in n.kids: parts.add render(k)
      result = n.text & "(" & parts.join(", ") & ")"
  of nTag: result = (if n.anchored: "^" else: "") & n.text & "[" & render(n.kids[0]) & "]"
  of nGuard: result = n.text
  of nDefault: result = "%else"
  of nRaw: result = "{" & n.text & "}"
  of nPred: result = "&" & n.text
  of nAhead: result = "&(" & render(n.kids[0]) & ")"
  of nIndented: result = "indented(" & render(n.kids[0]) & ")"
  of nWithInd: result = "withInd(" & render(n.kids[0]) & ")"
  of nBinTail:
    var ps: seq[string] = @[]
    for k in n.kids: ps.add render(k)
    result = "binaryTail(" & ps.join(", ") & ")"
  of nBinary: result = "binary(...)"
  of nLa2: result = "la2(" & render(n.kids[0]) & ")"

proc give(g: var Grammar; rule: string; t: FirstSet; changed: var bool) =
  if not g.follow.hasKey(rule): return
  var f = g.follow.getOrDefault(rule)
  var grew = false
  for k, v in pairs(t):
    let merged = f.getOrDefault(k) + v
    if not f.hasKey(k) or merged != f.getOrDefault(k):
      f[k] = merged
      grew = true
  if grew:
    g.follow[rule] = f
    changed = true

proc propagate(g: var Grammar; n: Node; after: FirstSet; changed: var bool) =
  ## Push FOLLOW information down `n`, where `after` is what may follow it.
  case n.kind
  of nSeq:
    var acc = after
    var i = n.kids.len - 1
    while i >= 0:
      let k = n.kids[i]
      propagate(g, k, acc, changed)
      let f = firstOf(g, k)
      if isNullable(g, k):
        restrict(acc, leadMask(g, k))
        union(acc, f)
      else:
        acc = f
      dec i
  of nAlt:
    for k in n.kids: propagate(g, k, after, changed)
  of nOpt:
    propagate(g, n.kids[0], after, changed)
  of nRep0, nRep1:
    var a = after
    union(a, firstOf(g, n.kids[0]))          # the body may repeat
    propagate(g, n.kids[0], a, changed)
  of nSepRep:
    var a = after
    union(a, firstOf(g, n.kids[1]))
    propagate(g, n.kids[0], a, changed)
    propagate(g, n.kids[1], firstOf(g, n.kids[0]), changed)
  of nTag:
    propagate(g, n.kids[0], after, changed)
  of nIndented, nWithInd:
    propagate(g, n.kids[0], remapIn(after), changed)
  of nRule:
    give(g, n.text, after, changed)
    for k in n.kids: propagate(g, k, initTable[string, IndSet](), changed)
  of nBinTail:
    if n.kids.len > 0: give(g, n.kids[0].text, after, changed)
  of nAhead, nBinary, nLa2:
    for k in n.kids: propagate(g, k, after, changed)
  else: discard

proc compatible(a, b: string): bool =
  ## `tkOpr+isDotLike` is a refinement of `tkOpr`, so the two can be the same
  ## token. Without this a choice between DOTLIKEOP and OPR looks disjoint.
  a == b or a.startsWith(b & "+") or b.startsWith(a & "+")

proc overlapOf(a, b: FirstSet): seq[string] =
  result = @[]
  for ka, va in pairs(a):
    for kb, vb in pairs(b):
      if compatible(ka, kb) and (va * vb) != {}:
        result.add (if ka == kb: ka else: ka & "~" & kb) & " " & indSetStr(va * vb)

proc intersectFirst(a, b: FirstSet): FirstSet =
  ## What an `&rule` lookahead leaves of an alternative's FIRST set. A
  ## lookahead only ever *narrows*: `&parKeyw` in front of
  ## `complexOrSimpleStmt` means the alternative starts at one of fifteen
  ## keywords, not at everything a statement can start with.
  result = initTable[string, IndSet]()
  for ka, va in pairs(a):
    for kb, vb in pairs(b):
      if compatible(ka, kb):
        let ind = va * vb
        if ind != {}:
          # the longer spelling is the more specific one (`tkOpr+isDotLike`
          # refines `tkOpr`), so it is the one that survives
          let key = if ka.len >= kb.len: ka else: kb
          result[key] = result.getOrDefault(key) + ind

proc leadingItems(n: Node): seq[Node] =
  ## The sequence elements of an alternative, for left-factoring. A leading
  ## tag is transparent: its open token is inserted at the mark once the
  ## alternative is known, so it does not take part in the shared prefix.
  if n.kind == nTag: result = leadingItems(n.kids[0])
  elif n.kind == nSeq:
    if n.kids.len > 0 and n.kids[0].kind == nTag and n.kids.len == 1:
      result = leadingItems(n.kids[0])
    else: result = n.kids
  else: result = @[n]

proc commonPrefix(a, b: seq[Node]): int =
  result = 0
  while result < a.len and result < b.len and
        render(a[result]) == render(b[result]): inc result

proc tail(items: seq[Node]; start: int): Node =
  result = newNode(nSeq)
  for i in start ..< items.len: result.kids.add items[i]

proc leadAhead(n: Node): Node =
  let items = if n.kind == nSeq: n.kids else: @[n]
  for it in items:
    if it.kind == nAhead: return it
    if it.kind notin {nGuard, nPred, nDefault}: break
  result = nil

proc isDefault(n: Node): bool =
  let items = if n.kind == nSeq: n.kids else: @[n]
  for it in items:
    if it.kind == nDefault: return true
  result = false

proc hasPredicate(n: Node): bool =
  for it in leadingItems(n):
    if it.kind in {nPred, nAhead}: return true
    if it.kind != nGuard: break
  result = false

# --------------------------------------------------------------- whole grammar

proc subst(n: Node; param: string; arg: Node): Node =
  if n.kind == nRule and n.text == param and n.kids.len == 0:
    return arg
  result = Node(kind: n.kind, text: n.text, anchored: n.anchored,
                ind: n.ind, kids: @[])
  for k in n.kids: result.kids.add subst(k, param, arg)

type Monomorph = object
  ruleParams: Table[string, int]     # rule -> index of its `: rule` parameter
  specialized: HashSet[string]

proc rewrite(g: var Grammar; mm: var Monomorph; n: Node) =
  if n.kind == nRule and mm.ruleParams.hasKey(n.text) and n.kids.len > 0:
    let idx = mm.ruleParams.getOrDefault(n.text)
    let arg = n.kids[idx]
    let newName = n.text & "_" & render(arg)
    if not mm.specialized.contains(newName):
      mm.specialized.incl newName
      let pname = g.params.getOrDefault(n.text)[idx]
      var copies: seq[Entry] = @[]
      for e in g.rules.getOrDefault(n.text):
        var e2 = e
        e2.name = newName
        e2.paramNames = @[]
        e2.paramTypes = @[]
        e2.paramIsRule = @[]
        e2.body = subst(e.body, pname, arg)
        copies.add e2
      g.rules[newName] = copies
      g.order.add newName
      for c in copies: rewrite(g, mm, c.body)
    n.text = newName
    n.kids = @[]
  for k in n.kids: rewrite(g, mm, k)

proc monomorphize(g: var Grammar) =
  ## `section(R: rule)` is not a rule the generator can emit: `R` has no FIRST
  ## set of its own. One specialization per distinct argument turns it back
  ## into ordinary rules.
  var mm = Monomorph(ruleParams: initTable[string, int](),
                     specialized: initHashSet[string]())
  for name in g.order:
    let es = g.rules.getOrDefault(name)
    if es.len == 0: continue
    for i in 0 ..< es[0].paramIsRule.len:
      if es[0].paramIsRule[i]: mm.ruleParams[name] = i
  if mm.ruleParams.len == 0: return
  let names = g.order
  for name in names:
    for e in g.rules.getOrDefault(name): rewrite(g, mm, e.body)
  var kept: seq[string] = @[]
  for name in g.order:
    if mm.ruleParams.hasKey(name):
      g.rules.del name
      g.params.del name
    else:
      kept.add name
  g.order = kept

proc recordArgs(g: var Grammar; n: Node) =
  ## every argument passed at a call site, keyed by the callee's parameter name
  if n.kind == nRule and n.kids.len > 0 and g.params.hasKey(n.text):
    let ps = g.params.getOrDefault(n.text)
    for i in 0 ..< min(ps.len, n.kids.len):
      var s = g.argsOf.getOrDefault(ps[i])
      s.add n.kids[i]
      g.argsOf[ps[i]] = s
  for k in n.kids: recordArgs(g, k)

proc reclassify(g: Grammar; n: Node) =
  ## `&name` is a FIRST-set lookahead when `name` is a declared rule
  if n.kind == nPred and g.rules.hasKey(n.text):
    let inner = newNode(nRule, n.text)
    inner.kids = n.kids
    n.kind = nAhead
    n.text = ""
    n.kids = @[inner]
  for k in n.kids: reclassify(g, k)

proc collectUsed(n: Node; locals: HashSet[string]; used: var HashSet[string]) =
  if n.kind == nBinary:
    if n.kids.len > 0: collectUsed(n.kids[0], locals, used)  # the rest are Nim procs
    return
  if n.kind == nBinTail:
    if n.kids.len > 0: used.incl n.kids[0].text   # a bare rule name
    return
  if n.kind == nRule and not isClass(n.text) and not locals.contains(n.text):
    used.incl n.text
  for k in n.kids: collectUsed(k, locals, used)

proc checkArity(g: var Grammar; rule: string; line: int; n: Node;
                locals: HashSet[string]) =
  ## A parameterized rule called with no arguments generated a call that
  ## silently dropped them, which is how `primarySuffix*` came to be emitted
  ## without `mode`.
  if n.kind == nRule and not isClass(n.text) and not locals.contains(n.text):
    if g.params.hasKey(n.text):
      let want = g.params.getOrDefault(n.text).len
      if n.kids.len != want:
        g.error rule, line, n.text & " takes " & $want &
          " argument(s), called with " & $n.kids.len
    elif n.kids.len > 0 and g.rules.hasKey(n.text):
      g.error rule, line, n.text & " takes no arguments, called with " & $n.kids.len
  if n.kind == nBinary:
    if n.kids.len > 0: checkArity(g, rule, line, n.kids[0], locals)
    return                                   # the rest are Nim procs
  if n.kind == nBinTail: return  # kids[0] names the rule, it does not call it
  for k in n.kids: checkArity(g, rule, line, k, locals)

proc sameFirst(a, b: FirstSet): bool =
  if a.len != b.len: return false
  for k, v in pairs(a):
    if not b.hasKey(k) or b.getOrDefault(k) != v: return false
  result = true

proc shown(s: var seq[string]): string =
  sortStrings s
  var parts: seq[string] = @[]
  for i in 0 ..< min(6, s.len): parts.add s[i]
  result = parts.join(", ")
  if s.len > 6: result.add " ... (" & $s.len & " total)"

proc checkConflicts(g: var Grammar; name: string) =
  ## LL(1) conflicts between the alternatives of one rule. Everything that is
  ## left-factorable, refined by a lookahead, separated by a predicate or
  ## declared `%else` is fine; what remains is an error.
  let es = g.rules.getOrDefault(name)
  if es.len < 2: return
  for i in 0 ..< es.len:
    for j in i+1 ..< es.len:
      let fi = firstOf(g, es[i].body)
      let fj = firstOf(g, es[j].body)
      var overlap = overlapOf(fi, fj)
      if overlap.len == 0: continue
      if isDefault(es[i].body) or isDefault(es[j].body): continue
      if leadAhead(es[i].body) != nil: continue

      let ai = leadingItems(es[i].body)
      let aj = leadingItems(es[j].body)
      let pre = commonPrefix(ai, aj)
      let where = "alternatives at lines " & $es[i].line & " and " & $es[j].line
      if pre > 0:
        # left-factorable: the decision moves to the continuations
        let ti = tail(ai, pre)
        let tj = tail(aj, pre)
        let ni = isNullable(g, ti)
        let nj = isNullable(g, tj)
        let ci = firstOf(g, ti)
        let cj = firstOf(g, tj)
        var cont = overlapOf(ci, cj)
        if cont.len == 0 and not (ni and nj): continue
        if ni and nj:
          g.error name, es[i].line, where & ": after " & render(ai[0]) &
            ", both continuations are nullable"
          continue
        if ni or nj:
          # the nullable side yields to FOLLOW(rule); the other must not
          # claim anything FOLLOW also claims
          let other = if ni: cj else: ci
          var clash = overlapOf(other, g.follow.getOrDefault(name))
          if clash.len > 0:
            g.error name, es[i].line, "LL(1) conflict between " & where &
              " after factoring " & render(ai[0]) &
              ": one continuation is nullable and FOLLOW overlaps the other on " &
              shown(clash)
          continue
        if leadAhead(ti) != nil: continue
        if hasPredicate(ti) or hasPredicate(tj): continue
        g.error name, es[i].line, "LL(1) conflict between " & where &
          " after factoring " & render(ai[0]) & ": " & render(ti) & " and " &
          render(tj) & " both start with " & shown(cont)
        continue
      if hasPredicate(es[i].body) or hasPredicate(es[j].body): continue
      g.error name, es[i].line, "LL(1) conflict between " & where & ": " &
        render(es[i].body) & " and " & render(es[j].body) &
        " both start with " & shown(overlap)

proc analyse(g: var Grammar) =
  ## Everything after parsing the productions: monomorphization, the fixed
  ## points, and the checks. `g.errors` is empty when the grammar is fine.
  monomorphize g
  for name in g.order:
    for e in g.rules.getOrDefault(name): recordArgs(g, e.body)
  for name in g.order:
    for e in g.rules.getOrDefault(name): reclassify(g, e.body)

  var used = initHashSet[string]()
  used.incl "module"
  for name in g.order:
    for e in g.rules.getOrDefault(name):
      var locals = initHashSet[string]()
      for pn in e.paramNames: locals.incl pn
      collectUsed(e.body, locals, used)
  var undeclared: seq[string] = @[]
  for u in items(used):
    if not g.rules.hasKey(u): undeclared.add u
  sortStrings undeclared
  for u in undeclared: g.errors.add "undeclared rule: " & u

  for name in g.order:
    for e in g.rules.getOrDefault(name):
      var locals = initHashSet[string]()
      for pn in e.paramNames: locals.incl pn
      checkArity(g, name, e.line, e.body, locals)

  # nullability to a fixed point
  for name in g.order: g.nullable[name] = false
  var changed = true
  while changed:
    changed = false
    for name in g.order:
      if g.nullable.getOrDefault(name): continue
      for e in g.rules.getOrDefault(name):
        if isNullable(g, e.body):
          g.nullable[name] = true
          changed = true
          break

  # FIRST to a fixed point
  for name in g.order:
    g.first[name] = initTable[string, IndSet]()
    g.mask[name] = AnyInd
  changed = true
  var rounds = 0
  while changed and rounds < 200:
    changed = false
    inc rounds
    for name in g.order:
      var mk: IndSet = {}
      for e in g.rules.getOrDefault(name): mk = mk + leadMask(g, e.body)
      if mk != g.mask.getOrDefault(name):
        g.mask[name] = mk
        changed = true
      var f = initTable[string, IndSet]()
      for e in g.rules.getOrDefault(name): union(f, firstOf(g, e.body))
      if not sameFirst(f, g.first.getOrDefault(name)):
        g.first[name] = f
        changed = true

  # FOLLOW
  for name in g.order: g.follow[name] = initTable[string, IndSet]()
  if g.follow.hasKey("module"):
    g.follow.getOrQuit("module")["tkEof"] = AnyInd
  changed = true
  rounds = 0
  while changed and rounds < 200:
    changed = false
    inc rounds
    for name in g.order:
      let after = g.follow.getOrDefault(name)
      for e in g.rules.getOrDefault(name): propagate(g, e.body, after, changed)

  for name in g.order: checkConflicts(g, name)

# --------------------------------------------------------------- reading

type
  Input = object
    cursors: seq[NifCursor]      ## action statements and parameter types
    infos: Table[string, LineInfo]
    err: string
    errAt: NifCursor

proc productionText(n: NifCursor; ok: var bool): string =
  ## A triple-quoted production reaches the plugin as `(suf "..." "T")`.
  ok = false
  result = ""
  if n.kind == StrLit:
    ok = true
    result = stringValue(n)
  elif n.kind == TagLit and n.tagText == "suf":
    let f = firstChild(n)
    if f.kind == StrLit:
      ok = true
      result = stringValue(f)

proc isActionBlock(n: NifCursor; name: string): bool =
  ## `enter: stmt` and `leave:` with a block are `(call enter (stmts ...))`.
  if n.kind != TagLit: return false
  let t = n.tagText
  if t != "call" and t != "cmd": return false
  var k = firstChild(n)
  if k.kind != Ident or identText(k) != name: return false
  skip k
  result = k.hasMore and k.kind == TagLit and k.tagText == "stmts"

proc addStmts(inp: var Input; n: NifCursor; dest: var seq[int]) =
  ## The statements of `(call enter (stmts ...))`.
  var k = firstChild(n)
  skip k
  var s = firstChild(k)
  while s.hasMore:
    inp.cursors.add s
    dest.add inp.cursors.len - 1
    skip s

proc readEntry(inp: var Input; g: var Grammar; n: NifCursor) =
  let info = n.info
  var e = Entry(name: "", src: "", paramNames: @[], paramTypes: @[],
                paramIsRule: @[], line: lineCol(info).line, enterCode: @[],
                leaveCode: @[], afterCode: @[], body: nil)
  let t = n.tagText
  if t != "cmd" and t != "callstrlit":
    inp.err = "a grammar entry is `name \"production\"`"
    inp.errAt = n
    return
  var k = firstChild(n)
  if k.kind == Ident:
    e.name = identText(k)
  elif k.kind == TagLit and (k.tagText == "oconstr" or k.tagText == "call"):
    # `name(a: T, b: U)`
    var h = firstChild(k)
    if h.kind == Ident: e.name = identText(h)
    skip h
    while h.hasMore:
      if h.kind == TagLit and h.tagText == "kv":
        var kv = firstChild(h)
        let pname = if kv.kind == Ident: identText(kv) else: ""
        skip kv
        e.paramNames.add pname
        e.paramIsRule.add(kv.kind == Ident and identText(kv) == "rule")
        inp.cursors.add kv
        e.paramTypes.add inp.cursors.len - 1
      else:
        inp.err = "a rule parameter is written `name: Type`"
        inp.errAt = h
        return
      skip h
  if e.name.len == 0:
    inp.err = "a grammar entry starts with the rule's name"
    inp.errAt = n
    return
  skip k
  var ok = false
  if k.hasMore: e.src = productionText(k, ok)
  if not ok:
    inp.err = "the production of `" & e.name & "` must be a string literal"
    inp.errAt = n
    return
  skip k
  if k.hasMore:
    if k.kind == TagLit and k.tagText == "stmts":
      var s = firstChild(k)
      while s.hasMore:
        if isActionBlock(s, "enter"): addStmts(inp, s, e.enterCode)
        elif isActionBlock(s, "leave"): addStmts(inp, s, e.leaveCode)
        else:
          inp.cursors.add s
          e.afterCode.add inp.cursors.len - 1
        skip s
    else:
      inp.err = "the actions of `" & e.name & "` must be a block"
      inp.errAt = k
      return
  parseProduction(g, e)
  if not g.rules.hasKey(e.name):
    g.rules[e.name] = @[]
    g.order.add e.name
    inp.infos[e.name] = info
  var es = g.rules.getOrDefault(e.name)
  es.add e
  g.rules[e.name] = es
  if e.paramNames.len > 0: g.params[e.name] = e.paramNames

proc readGrammar(inp: var Input; g: var Grammar; n: NifCursor) =
  var args = callArgs(n)
  while args.hasMore:
    if args.kind == TagLit and args.tagText == "stmts":
      var s = firstChild(args)
      while s.hasMore:
        readEntry(inp, g, s)
        if inp.err.len > 0: return
        skip s
    else:
      readEntry(inp, g, args)
      if inp.err.len > 0: return
    skip args

# --------------------------------------------------------------- code

type
  CodeKind = enum
    cIdent, cInt, cStr, cCall, cInfix, cCurly, cDot, cOconstr, cKv,
    cStmts, cLet, cVar, cAsgn, cIf, cElif, cElse, cWhile, cDiscard,
    cInput, cProc, cParams, cParam, cMut, cEmpty
  Code = ref object
    ## What the emitter builds before it becomes NIF: small, and structured
    ## the way `nifler` writes Nim, so the output is re-sem'd like source.
    kind: CodeKind
    s: string
    i: int
    info: LineInfo
    kids: seq[Code]

var curInfo = NoLineInfo    ## the rule being emitted, for every node's position

proc code(k: CodeKind; s = ""; kids: openArray[Code] = []): Code =
  result = Code(kind: k, s: s, i: 0, info: curInfo, kids: @[])
  for x in kids: result.kids.add x

proc id(s: string): Code = code(cIdent, s)
proc intLit(i: int): Code =
  result = code(cInt)
  result.i = i
proc strLit(s: string): Code = code(cStr, s)
proc call(name: string; args: openArray[Code]): Code =
  result = code(cCall, name, args)
proc infix(op: string; a, b: Code): Code = code(cInfix, op, [a, b])
proc andC(a, b: Code): Code = infix("and", a, b)
proc dot(a: Code; field: string): Code = code(cDot, field, [a])
proc curly(elems: openArray[Code]): Code = code(cCurly, "", elems)
proc letC(name: string; value: Code): Code = code(cLet, name, [value])
proc varC(name: string; value: Code): Code = code(cVar, name, [value])
proc asgn(a, b: Code): Code = code(cAsgn, "", [a, b])
proc stmts(): Code = code(cStmts)
proc withIdx(c: Code; i: int): Code =
  ## `cInput`: which of the input's statements or types this node copies.
  c.i = i
  result = c

proc p(): Code = id("p")
proc pinfo(): Code = dot(p(), "info")
proc tokKind(): Code = dot(dot(p(), "tok"), "kind")
proc indClassC(): Code = call("indClass", [p()])

proc emitCode(b: var NifBuilder; c: Code; cursors: seq[NifCursor]) =
  template tree(tag: string; body: untyped) =
    b.openTree(tag, c.info)
    body
    b.closeTree()
  case c.kind
  of cIdent: b.addIdent c.s
  of cInt: b.addIntLit c.i
  of cStr: b.addStrLit c.s
  of cEmpty: b.addEmptyNode
  of cInput: b.addSubtree cursors[c.i]
  of cCall:
    tree "call":
      b.addIdent c.s
      for k in c.kids: emitCode(b, k, cursors)
  of cInfix:
    tree "infix":
      b.addIdent c.s
      for k in c.kids: emitCode(b, k, cursors)
  of cCurly:
    tree "curly":
      for k in c.kids: emitCode(b, k, cursors)
  of cDot:
    tree "dot":
      emitCode(b, c.kids[0], cursors)
      b.addIdent c.s
  of cOconstr:
    tree "oconstr":
      b.addIdent c.s
      for k in c.kids: emitCode(b, k, cursors)
  of cKv:
    tree "kv":
      b.addIdent c.s
      emitCode(b, c.kids[0], cursors)
  of cStmts:
    tree "stmts":
      for k in c.kids: emitCode(b, k, cursors)
  of cLet, cVar:
    tree (if c.kind == cLet: "let" else: "var"):
      b.addIdent c.s
      b.addEmptyNode3             # export marker, pragmas, type
      emitCode(b, c.kids[0], cursors)
  of cAsgn:
    tree "asgn":
      for k in c.kids: emitCode(b, k, cursors)
  of cIf:
    tree "if":
      for k in c.kids: emitCode(b, k, cursors)
  of cElif:
    tree "elif":
      for k in c.kids: emitCode(b, k, cursors)
  of cElse:
    tree "else":
      for k in c.kids: emitCode(b, k, cursors)
  of cWhile:
    tree "while":
      for k in c.kids: emitCode(b, k, cursors)
  of cDiscard:
    tree "discard":
      b.addEmptyNode
  of cProc:
    # (proc name x pattern typevars params ret pragmas effects body)
    tree "proc":
      b.addIdent c.s
      b.addIdent "x"
      b.addEmptyNode2
      emitCode(b, c.kids[0], cursors)
      emitCode(b, c.kids[1], cursors)
      b.addEmptyNode2
      emitCode(b, c.kids[2], cursors)
  of cParams:
    tree "params":
      for k in c.kids: emitCode(b, k, cursors)
  of cParam:
    tree "param":
      b.addIdent c.s
      b.addEmptyNode2             # export marker, pragmas
      emitCode(b, c.kids[0], cursors)
      b.addEmptyNode              # default value
  of cMut:
    tree "mut":
      emitCode(b, c.kids[0], cursors)

proc parseArg(s: string; pos: var int): Code =
  ## The `{...}` arguments inside a production are Nim expressions, and small
  ## ones: an integer, a name, or a call of names.
  while pos < s.len and s[pos] == ' ': inc pos
  let start = pos
  if pos < s.len and (s[pos] == '-' or s[pos] in {'0'..'9'}):
    let negative = s[pos] == '-'
    if negative: inc pos
    var v = 0
    while pos < s.len and s[pos] in {'0'..'9'}:
      v = v * 10 + (ord(s[pos]) - ord('0'))
      inc pos
    return intLit(if negative: -v else: v)
  while pos < s.len and s[pos] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}: inc pos
  let name = s.substr(start, pos-1)
  if pos < s.len and s[pos] == '(':
    inc pos
    var args: seq[Code] = @[]
    while pos < s.len and s[pos] != ')':
      args.add parseArg(s, pos)
      while pos < s.len and s[pos] in {' ', ','}: inc pos
    inc pos
    result = call(name, args)
  else:
    result = id(name)

proc nodeArg(n: Node): Code =
  ## A rule argument as code: `{...}` is Nim, a bare name is a parameter.
  case n.kind
  of nRaw:
    var pos = 0
    result = parseArg(n.text, pos)
  of nRule:
    if n.kids.len == 0: result = id(n.text)
    else:
      var args: seq[Code] = @[]
      for k in n.kids: args.add nodeArg(k)
      result = call(n.text, args)
  else:
    result = id(render(n))

# --------------------------------------------------------------- emission

type
  Alt = object
    ## An alternative as emission sees it: the tag that wraps it (inserted
    ## retroactively at the mark, so factoring cannot disturb it) and the
    ## sequence items that remain once the tag is peeled off.
    tag: string
    anchored: bool
    items: seq[Node]
    enterCode, leaveCode, afterCode: seq[int]
    presetPos: string   ## a `%at` the group already captured, see `emitAlts`

  Emitter = object
    g: Grammar
    tmp: int
    curRule: string
    needsAnchor: HashSet[string]
    predicated: HashSet[string]   # rules whose alternatives carry predicates
    atVar: string                 # the variable a `%at` in this node assigns
    cur: Code                     # the statement list being written

proc line(e: var Emitter; c: Code) = e.cur.kids.add c

proc beginBody(e: var Emitter): Code =
  result = e.cur
  e.cur = stmts()

proc endBody(e: var Emitter; saved: Code): Code =
  ## The finished branch, `discard` when nothing was written into it.
  result = e.cur
  if result.kids.len == 0: result.kids.add code(cDiscard)
  e.cur = saved

proc procName(rule: string): string = "p" & capitalizeAscii(rule)

proc tagSym(tag: string): Code =
  ## A `tag[...]` as the generated code names it: the `NiflerKind` value from
  ## `src/models/nifler_tags.nim`, `stmts` -> `StmtsL`. An unknown tag is then a
  ## compile error, not a string nobody checks.
  id(capitalizeAscii(tag) & "L")

proc mkTmp(e: var Emitter; prefix: string): string =
  inc e.tmp
  result = prefix & $e.tmp

proc indBits(s: IndSet): int =
  result = 0
  for c in low(IndClass) .. high(IndClass):
    if c in s: result = result or (1 shl ord(c))

proc indSetLit(ind: IndSet): Code =
  var elems: seq[Code] = @[]
  for c in low(IndClass) .. high(IndClass):
    if c in ind:
      elems.add id(case c
                   of icNoInd: "icNoInd"
                   of icLt: "icLt"
                   of icEq: "icEq"
                   of icGt: "icGt")
  result = curly(elems)

proc kindElem(k: string): Code =
  let i = k.find("..")
  if i >= 0: result = infix("..", id(k.substr(0, i-1)), id(k.substr(i+2)))
  else: result = id(k)

proc kindTest(key: string): Code =
  ## One `(kind, refinement)` key as a condition on `p.tok`.
  let plus = key.find('+')
  let quote = key.find('"')
  if plus >= 0:
    result = andC(infix("==", tokKind(), id(key.substr(0, plus-1))),
                  call(key.substr(plus+1), [dot(p(), "tok")]))
  elif quote >= 0:
    result = andC(infix("==", tokKind(), id(key.substr(0, quote-1))),
                  infix("==", dot(dot(p(), "tok"), "s"),
                        strLit(key.substr(quote+1, key.len-2))))
  elif key.find("..") >= 0:
    result = infix("in", tokKind(), curly([kindElem(key)]))
  else:
    result = infix("==", tokKind(), id(key))

proc withInd(c: Code; ind: IndSet): Code =
  if ind == AnyInd: c
  else: andC(c, infix("in", indClassC(), indSetLit(ind)))

proc orAll(parts: seq[Code]): Code =
  if parts.len == 0: return id("false")
  result = parts[0]
  for i in 1 ..< parts.len: result = infix("or", result, parts[i])

proc andAll(parts: seq[Code]): Code =
  result = parts[0]
  for i in 1 ..< parts.len: result = andC(result, parts[i])

proc condFor(f: FirstSet): Code =
  ## A FIRST set as one boolean expression. Keys that share an indentation
  ## constraint and need no refinement collapse into a single set test.
  var plain: seq[seq[string]] = @[]
  for i in 0 .. 15: plain.add @[]
  var special: seq[Code] = @[]
  var keys: seq[string] = @[]
  for k in keys(f): keys.add k
  sortStrings keys
  for k in keys:
    let v = f.getOrDefault(k)
    if k.find('+') >= 0 or k.find('"') >= 0:
      special.add withInd(kindTest(k), v)
    else:
      plain[indBits(v)].add k
  var parts: seq[Code] = @[]
  for bits in 0 .. 15:
    if plain[bits].len == 0: continue
    var ind: IndSet = {}
    for c in low(IndClass) .. high(IndClass):
      if (bits and (1 shl ord(c))) != 0: ind.incl c
    var elems: seq[Code] = @[]
    for k in plain[bits]: elems.add kindElem(k)
    parts.add withInd(infix("in", tokKind(), curly(elems)), ind)
  for sp in special: parts.add sp
  result = orAll(parts)

proc allAnyInd(f: FirstSet): bool =
  for k, v in pairs(f):
    if v != AnyInd: return false
  result = true

proc anyInd(f: FirstSet): FirstSet =
  result = initTable[string, IndSet]()
  for k in keys(f): result[k] = AnyInd

proc predCall(n: Node): Code =
  ## `&noSpaceBefore` is `noSpaceBefore(p)`; `&inTypeDesc(mode)` is
  ## `inTypeDesc(p, mode)`. Without the arguments the generated dispatch drops
  ## exactly the information `parser.nim` decides on.
  var args = @[p()]
  for k in n.kids: args.add nodeArg(k)
  result = call(n.text, args)

proc canCall(n: Node): Code =
  ## The `canX` companion takes whatever `pX` takes, because its predicates do.
  var args = @[p()]
  for k in n.kids: args.add nodeArg(k)
  result = call("can" & capitalizeAscii(n.text), args)

proc guardsOf(items: seq[Node]): seq[Code] =
  ## The semantic predicates leading an alternative.
  result = @[]
  for it in items:
    case it.kind
    of nPred: result.add predCall(it)
    of nAhead, nGuard, nDefault: discard   # the lookahead is in the FIRST test
    else: break

proc withGuards(c: Code; gd: seq[Code]): Code =
  if gd.len == 0: c else: andC(c, andAll(gd))

proc toAlt(n: Node): Alt =
  var inner = n
  if inner.kind == nSeq and inner.kids.len == 1: inner = inner.kids[0]
  if inner.kind == nTag:
    result = Alt(tag: inner.text, anchored: inner.anchored,
                 items: leadingItems(inner.kids[0]), enterCode: @[],
                 leaveCode: @[], afterCode: @[], presetPos: "")
  else:
    result = Alt(tag: "", anchored: false, items: leadingItems(n),
                 enterCode: @[], leaveCode: @[], afterCode: @[], presetPos: "")

proc hasFreeAnchor(n: Node): bool =
  ## True when the node contains a `^tag[...]` that is NOT inside a repetition
  ## of its own rule -- such a tag anchors on the *caller's* accumulated value,
  ## so the mark has to cross the rule boundary as a parameter.
  if n.kind == nTag and n.anchored: return true
  if n.kind in {nOpt, nRep0, nRep1, nSepRep}: return false
  for k in n.kids:
    if hasFreeAnchor(k): return true
  result = false

proc seqOf(items: seq[Node]): Node =
  result = newNode(nSeq)
  result.kids = items

proc condOf(e: Emitter; n: Node): Code =
  # the item that actually decides, which is the first one that can consume a
  # token: `(&suffixStart primarySuffix(mode))*` is entered on
  # `canPrimarySuffix`, not on FIRST(primarySuffix).
  var head = n
  if n.kind == nSeq:
    head = nil
    for k in n.kids:
      if k.kind notin {nPred, nAhead, nGuard, nDefault}:
        head = k
        break
  var base: Code = nil
  if head != nil and head.kind == nRule and e.predicated.contains(head.text):
    # `canX` knows X's own predicates but not the guards written in front
    # of X at this call site: `(IND{>} stmt)?` must still require IND{>}.
    let guards = newNode(nSeq)
    if n.kind == nSeq:
      for k in n.kids:
        if k == head: break
        guards.kids.add k
    base = withInd(canCall(head), leadMask(e.g, guards))
  else:
    let ah = leadAhead(n)
    if ah != nil: base = condFor(intersectFirst(firstOf(e.g, n), firstOf(e.g, ah.kids[0])))
    else: base = condFor(firstOf(e.g, n))
  # An optional or repeated body that LEADS with a semantic predicate is only
  # entered when the predicate holds, and a FIRST set cannot express one -- so
  # it has to be ANDed in here, exactly as `emitAlts` does for alternatives.
  result = withGuards(base, guardsOf(if n.kind == nSeq: n.kids else: @[n]))

proc emitNode(e: var Emitter; n: Node; mark, anchor: string)

proc hasAtPos(items: seq[Node]): bool =
  ## A `%at` that belongs to this node: directly in its sequence, or in an
  ## option or alternative of it -- not inside a nested tag or rule, which
  ## are nodes of their own.
  for it in items:
    case it.kind
    of nAtPos: return true
    of nOpt, nSeq, nAlt:
      if hasAtPos(it.kids): return true
    else: discard
  result = false

proc writesWhenEmpty(n: Node): bool =
  ## A `.` or an `X?.` produces output even when it matches nothing, so the
  ## item is not "just a guard" and an empty match still needs its branch.
  if n.kind == nEmpty or (n.kind == nOpt and n.anchored): return true
  if n.kind in {nRule, nRep0, nRep1, nSepRep}: return false
  for k in n.kids:
    if writesWhenEmpty(k): return true
  result = false

proc isPureGuard(g: Grammar; n: Node): bool =
  ## Matches no token, only constrains the next one.
  firstOf(g, n).len == 0 and isNullable(g, n) and not writesWhenEmpty(n)

proc emitSeq(e: var Emitter; items: seq[Node]; mark, anchor: string) =
  for it in items: emitNode(e, it, mark, anchor)

proc altCond(g: Grammar; a: Alt): FirstSet =
  ## FIRST of the alternative narrowed by a leading `&rule`. Only the
  ## *dispatch* uses this; grouping and nullability still go by the
  ## unrestricted set, because the lookahead says nothing about what the
  ## alternative may then consume.
  let n = seqOf(a.items)
  result = firstOf(g, n)
  let ah = leadAhead(n)
  if ah != nil: result = intersectFirst(result, firstOf(g, ah.kids[0]))

proc markCall(): Code = call("mark", [p()])

proc emitMarked(e: var Emitter; n: Node; mark: string) =
  ## A body with a mark of its own: `let mN = mark(p)` ... `discardUnused mN`.
  let m2 = e.mkTmp("m")
  e.line letC(m2, markCall())
  emitNode(e, n, m2, mark)
  e.line call("discardUnused", [id(m2)])

proc finishAlt(e: var Emitter; a: Alt; mark, anchor: string) =
  for i in a.enterCode: e.line code(cInput).withIdx(i)
  # The node's position. parser.nim's `newNodeP` takes the token that is
  # current when the node is *created*: the first token for most nodes, the
  # operator for an anchored one (`^call[ '(' ...]` is created at the `(`),
  # and whatever `%at` says otherwise.
  let saved = e.atVar
  var posVar = a.presetPos
  if posVar.len == 0 and (a.anchored or hasAtPos(a.items)):
    posVar = e.mkTmp("at")
    e.line varC(posVar, if a.anchored: pinfo() else: dot(id(mark), "info"))
  if posVar.len > 0: e.atVar = posVar
  emitSeq(e, a.items, mark, anchor)
  e.atVar = saved
  for i in a.leaveCode: e.line code(cInput).withIdx(i)
  let base = if a.anchored: anchor else: mark
  if a.tag.len > 0:
    if posVar.len > 0:
      e.line call("wrapAt", [p(), id(base), tagSym(a.tag), id(posVar)])
    else:
      e.line call("wrap", [p(), id(base), tagSym(a.tag)])
  if a.afterCode.len > 0:
    # the bare body inspects what was just parsed, so it needs the mark --
    # the anchor when the alternative's tag is anchored, even when a
    # predicate in front of it kept `toAlt` from peeling the tag off
    var anchoredTag = a.anchored
    for it in a.items:
      if it.kind == nTag and it.anchored: anchoredTag = true
    let mk = if anchoredTag: anchor else: mark
    if posVar.len > 0 and not anchoredTag:
      e.line letC("m", code(cOconstr, "Mark", [code(cKv, "pos", [dot(id(mk), "pos")]),
                                              code(cKv, "info", [id(posVar)])]))
    else:
      e.line letC("m", id(mk))
    for i in a.afterCode: e.line code(cInput).withIdx(i)

proc emitAlts(e: var Emitter; alts: seq[Alt]; rule, mark, anchor: string) =
  ## Left-factor what shares a prefix, then dispatch on one token.
  if alts.len == 1:
    finishAlt(e, alts[0], mark, anchor)
    return

  # a rule that is nothing but indentation guards is one assertion, not a
  # dispatch: `notInd = NO_IND | IND{=} | IND{<}`
  var allGuards = true
  for a in alts:
    let n = seqOf(a.items)
    if a.tag.len > 0 or firstOf(e.g, n).len > 0 or not isNullable(e.g, n) or
       a.enterCode.len + a.leaveCode.len + a.afterCode.len > 0 or
       writesWhenEmpty(n):
      allGuards = false
  if allGuards:
    var m: IndSet = {}
    for a in alts: m = m + leadMask(e.g, seqOf(a.items))
    e.line call("checkInd", [p(), indSetLit(m)])
    return

  var keys: seq[string] = @[]
  var groups: seq[seq[Alt]] = @[]
  var defaults: seq[Alt] = @[]
  for a in alts:
    var isGuardOnly = false
    if a.tag.len == 0 and a.items.len > 0:
      isGuardOnly = isPureGuard(e.g, seqOf(a.items))
    var marked = false
    for it in a.items:
      if it.kind == nDefault: marked = true
    if marked or a.items.len == 0 or isGuardOnly:
      # `%else`, an alternative exhausted by factoring, or a bare guard: all
      # three are "what is left when nothing else claimed the token"
      defaults.add a
      continue
    let key = render(a.items[0])
    var gi = -1
    for i in 0 ..< keys.len:
      if keys[i] == key: gi = i
    if gi < 0:
      keys.add key
      groups.add @[a]
    else:
      groups[gi].add a

  let ifc = code(cIf)
  var allF = initTable[string, IndSet]()
  for grp in groups:
    var f = initTable[string, IndSet]()
    for a in grp:
      for k, v in pairs(altCond(e.g, a)): f[k] = f.getOrDefault(k) + v
    for k, v in pairs(f): allF[k] = allF.getOrDefault(k) + v
    var gd: seq[Code] = @[]
    if grp.len == 1: gd = guardsOf(grp[0].items)
    elif grp[0].items[0].kind == nPred: gd = @[predCall(grp[0].items[0])]
    let cond = withGuards(condFor(f), gd)
    let saved = e.beginBody()
    if grp.len == 1:
      finishAlt(e, grp[0], mark, anchor)
    else:
      # A shared `%at` is captured here, before the shared token is
      # consumed, and handed to every tail: `kv[ expr %at ':' expr ]` and
      # `vv[ expr %at '=' expr ]` factor into one `%at` and two tails.
      var preset = grp[0].presetPos
      if grp[0].items[0].kind == nAtPos:
        preset = e.mkTmp("at")
        e.line varC(preset, pinfo())
      else:
        emitNode(e, grp[0].items[0], mark, anchor)   # the shared prefix, once
      var tails: seq[Alt] = @[]
      for a in grp:
        var rest: seq[Node] = @[]
        for i in 1 ..< a.items.len: rest.add a.items[i]
        tails.add Alt(tag: a.tag, anchored: a.anchored, items: rest,
                      enterCode: a.enterCode, leaveCode: a.leaveCode,
                      afterCode: a.afterCode, presetPos: preset)
      emitAlts(e, tails, rule, mark, anchor)
    ifc.kids.add code(cElif, "", [cond, e.endBody(saved)])

  if groups.len > 0: e.line ifc
  if defaults.len > 0:
    if groups.len == 0:
      finishAlt(e, defaults[0], mark, anchor)
    else:
      let saved = e.beginBody()
      finishAlt(e, defaults[0], mark, anchor)
      ifc.kids.add code(cElse, "", [e.endBody(saved)])
  elif groups.len > 0:
    # An alternative that can match the empty string makes "nothing matched" a
    # legal outcome, so there is nothing to report: `indAndComment` is
    # `(IND{>} COMMENT)? | COMMENT?` and both sides can match nothing. Such an
    # alternative only needs a branch of its own if it carries a tag or an
    # action, which an empty match still has to run.
    var empty = -1
    for i in 0 ..< alts.len:
      if empty < 0 and isNullable(e.g, seqOf(alts[i].items)): empty = i
    if empty < 0:
      # A token some alternative starts with, at an indentation none of them
      # allows, is parser.nim's "invalid indentation" rather than a message
      # about what the rule expected.
      var misplaced = initTable[string, IndSet]()
      for k, v in pairs(allF):
        if v != AnyInd and k.find('+') < 0 and k.find('"') < 0:
          misplaced[k] = AnyInd - v
      # And when every alternative opens with an indentation guard (`optInd
      # expr`), a token at any other indentation trips that guard first.
      var lead: IndSet = {}
      for a in alts:
        var m = AnyInd
        for it in a.items:
          if it.kind in {nPred, nAhead, nDefault}: continue
          if not isNullable(e.g, it): break
          m = m * leadMask(e.g, it)
        lead = lead + m
      var conds: seq[Code] = @[]
      if misplaced.len > 0: conds.add condFor(misplaced)
      if lead != AnyInd: conds.add infix("notin", indClassC(), indSetLit(lead))
      let saved = e.beginBody()
      e.line call("ruleError", [p(), strLit(if rule == "alternative": e.curRule else: rule),
                                (if conds.len > 0: orAll(conds) else: id("false"))])
      ifc.kids.add code(cElse, "", [e.endBody(saved)])
    else:
      let ea = alts[empty]
      if ea.tag.len > 0 or ea.enterCode.len + ea.leaveCode.len + ea.afterCode.len > 0 or
         writesWhenEmpty(seqOf(ea.items)):
        let saved = e.beginBody()
        finishAlt(e, ea, mark, anchor)
        ifc.kids.add code(cElse, "", [e.endBody(saved)])

proc terminalArgs(n: Node): seq[Code] =
  ## `expect p, tkComma`, or `expect p, tkOpr, "*"` for an operator spelling.
  let tk = tokenKind(n.text)
  let q = tk.find('"')
  if q >= 0: result = @[p(), id(tk.substr(0, q-1)), strLit(n.text)]
  else: result = @[p(), id(tk)]

proc ruleArgs(e: Emitter; n: Node; anchor: string): seq[Code] =
  result = @[p()]
  if e.needsAnchor.contains(n.text): result.add id(anchor)
  for k in n.kids: result.add nodeArg(k)

proc emitNode(e: var Emitter; n: Node; mark, anchor: string) =
  case n.kind
  of nSeq:
    emitSeq(e, n.kids, mark, anchor)
  of nAlt:
    let mk = e.mkTmp("m")
    e.line letC(mk, markCall())
    var as2: seq[Alt] = @[]
    for k in n.kids: as2.add toAlt(k)
    emitAlts(e, as2, "alternative", mk, anchor)
    e.line call("discardUnused", [id(mk)])
  of nTerminal:
    e.line call(if n.anchored: "expectLeaf" else: "expect", terminalArgs(n))
  of nClass:
    e.line call("emitLeaf", [p()])
  of nRule:
    if e.g.rules.hasKey(n.text):
      e.line call(procName(n.text), ruleArgs(e, n, anchor))
  of nRaw, nPred, nAhead, nDefault, nLa2:
    discard                       # part of the dispatch condition
  of nGuard:
    e.line call("checkInd", [p(), indSetLit(n.ind)])
  of nOpt:
    # An option's body gets a mark of its own, exactly like a repetition's:
    # `(else[ 'else' colcom stmt ])?` wraps the `else` branch and nothing else.
    # A tag that is meant to wrap what came *before* it --
    # `(^infix[ 'not' primary ])?` -- says so with `^`, which resolves to the
    # enclosing mark.
    let ifc = code(cIf)
    let cond = condOf(e, n.kids[0])
    var saved = e.beginBody()
    emitMarked(e, n.kids[0], mark)
    ifc.kids.add code(cElif, "", [cond, e.endBody(saved)])
    if n.anchored:
      saved = e.beginBody()
      e.line call("emitEmpty", [p()])
      ifc.kids.add code(cElse, "", [e.endBody(saved)])
    e.line ifc
  of nEmpty:
    e.line call("emitEmpty", [p()])
  of nAtPos:
    if e.atVar.len > 0: e.line asgn(id(e.atVar), pinfo())
  of nRep0:
    let cond = condOf(e, n.kids[0])
    let saved = e.beginBody()
    emitMarked(e, n.kids[0], mark)
    e.line code(cWhile, "", [cond, e.endBody(saved)])
  of nRep1:
    # the first iteration is no different from the others: its own mark, and
    # `^tag` reaching back to the enclosing one
    emitMarked(e, n.kids[0], mark)
    let cond = condOf(e, n.kids[0])
    let saved = e.beginBody()
    emitMarked(e, n.kids[0], mark)
    e.line code(cWhile, "", [cond, e.endBody(saved)])
  of nSepRep:
    let again = seqOf(@[n.kids[1], n.kids[0]])
    let optional = n.text == "^*"
    var outer: Code = nil
    var cond: Code = nil
    if optional:
      cond = condOf(e, n.kids[0])
      outer = e.beginBody()
    let m2 = e.mkTmp("m")
    e.line letC(m2, markCall())
    emitNode(e, n.kids[0], m2, mark)
    let againFirst = firstOf(e.g, again)
    let saved = e.beginBody()
    emitNode(e, n.kids[1], m2, mark)
    emitNode(e, n.kids[0], m2, mark)
    e.line code(cWhile, "", [condFor(againFirst), e.endBody(saved)])
    if not allAnyInd(againFirst):
      # Only a token that is *deeper* than expected is unambiguously wrong: a
      # dedent legitimately hands control back to an enclosing construct,
      # which is why this cannot simply be `could have continued but did not`.
      let s2 = e.beginBody()
      e.line call("indentError", [p()])
      e.line code(cIf, "", [code(cElif, "", [
        andC(condFor(anyInd(againFirst)), infix("==", indClassC(), id("icGt"))),
        e.endBody(s2)])])
    e.line call("discardUnused", [id(m2)])
    if optional:
      let inner = e.endBody(outer)
      e.line code(cIf, "", [code(cElif, "", [cond, inner])])
  of nTag:
    let tagItems = if n.kids[0].kind == nSeq: n.kids[0].kids else: @[n.kids[0]]
    let savedAt = e.atVar
    var posVar = ""
    if n.anchored or hasAtPos(tagItems):
      posVar = e.mkTmp("at")
      e.line varC(posVar, if n.anchored: pinfo() else: dot(id(mark), "info"))
      e.atVar = posVar
    if n.anchored:
      var body = n.kids[0]
      let items = if body.kind == nSeq: body.kids else: @[body]
      if items.len > 0 and items[0].kind == nTerminal and items[0].anchored:
        # `^infix[ @'not' primary ]`: NIF writes the operator first,
        # `(infix not a b)`, but `a` is already on the buffer. The operator
        # goes in at the anchor, exactly as `binary(...)` does it.
        let args = terminalArgs(items[0])
        e.line call("insertTokAt", [p(), id(anchor), args[1]])
        var rest: seq[Node] = @[]
        for i in 1 ..< items.len: rest.add items[i]
        body = seqOf(rest)
      emitNode(e, body, mark, anchor)
      e.line call("wrapAt", [p(), id(anchor), tagSym(n.text), id(posVar)])
    else:
      emitNode(e, n.kids[0], mark, anchor)
      if posVar.len > 0:
        e.line call("wrapAt", [p(), id(mark), tagSym(n.text), id(posVar)])
      else:
        e.line call("wrap", [p(), id(mark), tagSym(n.text)])
    e.atVar = savedAt
  of nWithInd:
    e.line call("pushIndAny", [p()])
    emitNode(e, n.kids[0], mark, anchor)
    e.line call("popInd", [p()])
  of nIndented:
    e.line call("pushInd", [p()])
    emitNode(e, n.kids[0], mark, anchor)
    e.line call("popInd", [p()])
  of nBinary:
    # binary(operand, precedenceProc, rightAssocProc, tag[, limit, args...])
    let precP = render(n.kids[1])
    let assocP = render(n.kids[2])
    let tg = render(n.kids[3])
    # the fifth argument names the parameter that carries the limit; the
    # recursion tightens that one and passes every other parameter through
    let limitName = if n.kids.len > 4: render(n.kids[4]) else: "limit"
    emitNode(e, n.kids[0], mark, anchor)
    e.line varC("prec", call(precP, [p()]))
    let saved = e.beginBody()
    e.line varC("assoc", intLit(1))
    e.line code(cIf, "", [code(cElif, "", [call(assocP, [p()]),
                                         code(cStmts, "", [asgn(id("assoc"), intLit(0))])])])
    e.line letC("opInfo", pinfo())    # `parseOperators` creates the node here
    e.line call("insertLeafAt", [p(), id(mark), dot(dot(p(), "tok"), "s")])
    e.line call("getTok", [p()])
    e.line call("afterOperator", [p()])
    var args = @[p()]
    if e.needsAnchor.contains(e.curRule): args.add id(anchor)
    let tighter = infix("+", id("prec"), id("assoc"))
    if e.g.params.hasKey(e.curRule):
      # arguments after the fifth are what the *right* operand gets for the
      # other parameters, in order -- `parseOperators` does not pass its
      # mode through unchanged
      var extra = 5
      for pn in e.g.params.getOrDefault(e.curRule):
        if pn == limitName: args.add tighter
        elif extra < n.kids.len:
          args.add nodeArg(n.kids[extra])
          inc extra
        else: args.add id(pn)
    else:
      args.add tighter
    e.line call(procName(e.curRule), args)
    e.line call("wrapAt", [p(), id(mark), tagSym(tg), id("opInfo")])
    e.line asgn(id("prec"), call(precP, [p()]))
    e.line code(cWhile, "", [
      andC(infix(">=", id("prec"), id(limitName)),
           infix("==", indClassC(), id("icNoInd"))),
      e.endBody(saved)])
  of nBinTail:
    # `parseOperators` applied to a node that is already on the buffer:
    # `parser.nim` does this in three places, of which `type(x) is type(y)`
    # is the one that matters. Here the left operand is whatever the
    # enclosing sequence just parsed, and the recursion names its own rule.
    let pv = e.mkTmp("prec")
    let av = "assoc" & $e.tmp
    let oi = "op" & av
    let tprec = render(n.kids[1])
    let tassoc = render(n.kids[2])
    let ttag = render(n.kids[3])
    let tlimit = if n.kids.len > 4: nodeArg(n.kids[4]) else: intLit(-1)
    e.line varC(pv, call(tprec, [p()]))
    let saved = e.beginBody()
    e.line varC(av, intLit(1))
    e.line code(cIf, "", [code(cElif, "", [call(tassoc, [p()]),
                                         code(cStmts, "", [asgn(id(av), intLit(0))])])])
    e.line letC(oi, pinfo())
    e.line call("insertLeafAt", [p(), id(mark), dot(dot(p(), "tok"), "s")])
    e.line call("getTok", [p()])
    e.line call("afterOperator", [p()])
    var args = @[p(), infix("+", id(pv), id(av))]
    for i in 5 ..< n.kids.len: args.add nodeArg(n.kids[i])
    e.line call(procName(n.kids[0].text), args)
    e.line call("wrapAt", [p(), id(mark), tagSym(ttag), id(oi)])
    e.line asgn(id(pv), call(tprec, [p()]))
    e.line code(cWhile, "", [
      andC(infix(">=", id(pv), tlimit), infix("==", indClassC(), id("icNoInd"))),
      e.endBody(saved)])

proc paramList(g: Grammar; rule: string; first: Code; anchor: bool): Code =
  result = code(cParams, "", [first])
  if anchor: result.kids.add code(cParam, "anchor", [id("Mark")])
  let es = g.rules.getOrDefault(rule)
  if es.len > 0:
    for i in 0 ..< es[0].paramNames.len:
      result.kids.add code(cParam, es[0].paramNames[i],
                           [code(cInput).withIdx(es[0].paramTypes[i])])

proc emitParser(e: var Emitter; inp: Input): Code =
  let g = e.g
  result = stmts()
  for r in g.order:
    for a in g.rules.getOrDefault(r):
      if hasFreeAnchor(a.body): e.needsAnchor.incl r
      if guardsOf(leadingItems(a.body)).len > 0: e.predicated.incl r

  # "can this rule start here" for the predicated rules: a caller's loop test
  # has to respect the predicate, and a FIRST set cannot express one.
  var canProcs: seq[Code] = @[]
  var ruleProcs: seq[Code] = @[]
  for r in g.order:
    curInfo = inp.infos.getOrDefault(r)
    let canName = "can" & capitalizeAscii(r)
    let canParams = paramList(g, r, code(cParam, "p", [id("Parser")]), false)
    let pParams = paramList(g, r, code(cParam, "p", [code(cMut, "", [id("Parser")])]),
                            e.needsAnchor.contains(r))
    if e.predicated.contains(r):
      var parts: seq[Code] = @[]
      for a in g.rules.getOrDefault(r):
        parts.add withGuards(condFor(firstOf(g, a.body)), guardsOf(leadingItems(a.body)))
      canProcs.add code(cProc, canName, [canParams, id("bool"),
        code(cStmts, "", [asgn(id("result"), orAll(parts))])])

    e.curRule = r
    e.tmp = 0
    e.cur = stmts()
    e.line letC("m0", markCall())
    var bodies: seq[Alt] = @[]
    for a in g.rules.getOrDefault(r):
      var al = toAlt(a.body)
      al.enterCode = a.enterCode
      al.leaveCode = a.leaveCode
      al.afterCode = a.afterCode
      bodies.add al
    if not (bodies.len == 1 and bodies[0].items.len == 0 and bodies[0].tag.len == 0):
      emitAlts(e, bodies, r, "m0", if e.needsAnchor.contains(r): "anchor" else: "m0")
    e.line call("discardUnused", [id("m0")])
    ruleProcs.add code(cProc, procName(r), [pParams, code(cEmpty), e.cur])
  for c in canProcs: result.kids.add c
  for c in ruleProcs: result.kids.add c

proc transform(n: NifCursor): NifBuilder =
  var inp = Input(cursors: @[], infos: initTable[string, LineInfo](), err: "",
                  errAt: n)
  var g = Grammar(rules: initTable[string, seq[Entry]](), order: @[],
                  params: initTable[string, seq[string]](), errors: @[])
  readGrammar(inp, g, n)
  if inp.err.len > 0:
    return errorTree(inp.err, inp.errAt)
  if g.errors.len == 0: analyse g
  if g.errors.len > 0:
    return errorTree("grammar: " & g.errors.join("\n"), n)
  var e = Emitter(g: g, tmp: 0, curRule: "", needsAnchor: initHashSet[string](),
                  predicated: initHashSet[string](), atVar: "", cur: nil)
  let prog = emitParser(e, inp)
  result = createTree()
  emitCode(result, prog, inp.cursors)

saveTree transform(loadPluginInput())
