## Standalone LL(1) checker for the grammar notation described in
## `doc/internals/parser_generator.md`.
##
## This is the *analysis* half of the future `deps/parsegen` plugin, wired to a
## throwaway text front end so it can be run today:
##
##   nim c -r src/nifler2/tools/gramcheck.nim src/nifler2/nimgrammar.nim
##
## It reads the `Name "production"` entries out of a .nim file, parses the
## mini-language, and computes nullability and FIRST over
## `(TokenKind, IndentClass)` pairs — the extended domain that makes Nim's
## indentation part of the LL(1) decision rather than a side channel.

import std / [os, strutils, tables, sets, algorithm, syncio]

# --------------------------------------------------------------- terminals

const Keywords = ["addr", "and", "as", "asm", "bind", "block", "break",
  "case", "cast", "concept", "const", "continue", "converter", "defer",
  "discard", "distinct", "div", "do", "elif", "else", "end", "enum", "except",
  "export", "finally", "for", "from", "func", "if", "import", "in", "include",
  "interface", "is", "isnot", "iterator", "let", "macro", "method", "mixin",
  "mod", "nil", "not", "notin", "object", "of", "or", "out", "proc", "ptr",
  "raise", "ref", "return", "shl", "shr", "static", "template", "try", "tuple",
  "type", "using", "var", "when", "while", "xor", "yield"]

const Punctuation = {
  "(": "tkParLe", ")": "tkParRi", "[": "tkBracketLe", "]": "tkBracketRi",
  "{": "tkCurlyLe", "}": "tkCurlyRi", "[.": "tkBracketDotLe", ".]": "tkBracketDotRi",
  "{.": "tkCurlyDotLe", ".}": "tkCurlyDotRi", "(.": "tkParDotLe", ".)": "tkParDotRi",
  ",": "tkComma", ";": "tkSemiColon", ":": "tkColon", "::": "tkColonColon",
  "=": "tkEquals", ".": "tkDot", "..": "tkDotDot", "[:": "tkBracketLeColon",
  "`": "tkAccent"}.toTable

const Classes = {
  "IDENT": "tkSymbol", "COMMENT": "tkComment", "EOF": "tkEof",
  "KEYW": "tkAddr..tkYield", "OPR": "tkOpr",
  "DOTLIKEOP": "tkOpr+isDotLike", "SIGILLIKEOP": "tkOpr+isSigilLike",
  "INT_LIT": "tkIntLit", "INT8_LIT": "tkInt8Lit", "INT16_LIT": "tkInt16Lit",
  "INT32_LIT": "tkInt32Lit", "INT64_LIT": "tkInt64Lit",
  "UINT_LIT": "tkUIntLit", "UINT8_LIT": "tkUInt8Lit", "UINT16_LIT": "tkUInt16Lit",
  "UINT32_LIT": "tkUInt32Lit", "UINT64_LIT": "tkUInt64Lit",
  "FLOAT_LIT": "tkFloatLit", "FLOAT32_LIT": "tkFloat32Lit",
  "FLOAT64_LIT": "tkFloat64Lit", "FLOAT128_LIT": "tkFloat128Lit",
  "STR_LIT": "tkStrLit", "RSTR_LIT": "tkRStrLit", "TRIPLESTR_LIT": "tkTripleStrLit",
  "GENERALIZED_STR_LIT": "tkGStrLit", "GENERALIZED_TRIPLESTR_LIT": "tkGTripleStrLit",
  "CHAR_LIT": "tkCharLit", "CUSTOM_NUMERIC_LIT": "tkCustomLit"}.toTable

proc tokenKind(terminal: string): string =
  ## The hardcoded mapping. Keywords are regular (`tk` & capitalize), the
  ## punctuation is a table, and anything else is an operator by spelling.
  if terminal in Keywords: "tk" & capitalizeAscii(terminal)
  elif terminal in Punctuation: Punctuation[terminal]
  else: "tkOpr\"" & terminal & "\""

# --------------------------------------------------------------- grammar AST

type
  IndClass = enum
    icNoInd = "NO_IND", icLt = "IND{<}", icEq = "IND{=}", icGt = "IND{>}"
  IndSet = set[IndClass]

const AnyInd: IndSet = {icNoInd, icLt, icEq, icGt}

type
  NodeKind = enum
    nSeq, nAlt, nOpt, nRep0, nRep1, nSepRep, nTerminal, nClass, nRule,
    nTag, nGuard, nPred, nAhead, nIndented, nWithInd, nBinary, nBinTail, nLa2,
    nDefault, nRaw
  Node = ref object
    kind: NodeKind
    text: string          # terminal spelling / class / rule / tag / pred name
    anchored: bool        # `^tag[...]`
    ind: IndSet
    kids: seq[Node]

  Entry = object
    name, params, src: string
    paramNames: seq[string]
    line: int
    hasActions: bool
    enterCode: seq[string]   ## `enter:` -- runs before the match
    leaveCode: seq[string]   ## `leave:` -- runs after it, match or not
    afterCode: seq[string]   ## a bare body, with `m` bound to the mark
    body: Node

proc newNode(k: NodeKind; text = ""): Node = Node(kind: k, text: text, kids: @[])

# --------------------------------------------------------------- mini-parser

type Scanner = object
  s: string
  pos: int
  rule: string
  line: int

var errors = 0

proc err(sc: Scanner; msg: string) =
  echo "[error] ", sc.rule, " (line ", sc.line, "): ", msg
  inc errors

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
    if w != "else": err sc, "unknown marker %" & w
    return newNode(nDefault, "%else")
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
    if inner.kind == nRule and inner.text notin Classes and
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
    err sc, "unexpected character '" & c & "'"
    inc sc.pos
    return newNode(nSeq)

  if name == "IND" and atNow(sc, '{'):
    inc sc.pos
    let relStart = sc.pos
    while sc.pos < sc.s.len and sc.s[sc.pos] != '}': inc sc.pos
    let rel = sc.s.substr(relStart, sc.pos-1)
    inc sc.pos
    result = newNode(nGuard, "IND{" & rel & "}")
    result.ind = case rel
      of "=": {icEq}
      of ">": {icGt}
      of "<": {icLt}
      else:
        err sc, "unknown indentation relation '" & rel & "'"
        AnyInd
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
    of "indented":
      result = newNode(nIndented)
      result.kids = args
    of "withInd":
      result = newNode(nWithInd)
      result.kids = args
    of "binary":
      result = newNode(nBinary, name)
      result.kids = args
    of "binaryTail":
      result = newNode(nBinTail, name)
      result.kids = args
    of "la2":
      result = newNode(nLa2)
      result.kids = args
    else:
      result = newNode(nRule, name)
      result.kids = args
    return
  if name in Classes:
    return newNode(nClass, name)
  return newNode(nRule, name)

proc parsePostfix(sc: var Scanner): Node =
  result = parsePrim(sc)
  while true:
    if take(sc, "^*"):
      let r = newNode(nSepRep)
      r.text = "^*"
      r.kids.add result
      r.kids.add parsePrim(sc)
      result = r
    elif take(sc, "^+"):
      let r = newNode(nSepRep)
      r.text = "^+"
      r.kids.add result
      r.kids.add parsePrim(sc)
      result = r
    elif take(sc, "?"):
      let r = newNode(nOpt); r.kids.add result; result = r
    elif take(sc, "*"):
      let r = newNode(nRep0); r.kids.add result; result = r
    elif take(sc, "+"):
      let r = newNode(nRep1); r.kids.add result; result = r
    else:
      break

proc startsItem(sc: var Scanner): bool =
  ws sc
  if sc.pos >= sc.s.len: return false
  let c = sc.s[sc.pos]
  result = c notin {'|', ')', ']', ','}

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

# --------------------------------------------------------------- extraction

proc indentOf(s: string): int =
  result = 0
  while result < s.len and s[result] == ' ': inc result

proc parseActions(e: var Entry; body: seq[string]) =
  ## `enter: <stmt>` / `leave: <stmt>` and their block forms; anything else is
  ## the bare body that runs once the alternative has matched.
  var i = 0
  while i < body.len:
    let ln = body[i]
    let t = ln.strip()
    if t.len == 0 or t.startsWith("#"):
      inc i
      continue
    if t.startsWith("enter:") or t.startsWith("leave:"):
      let isEnter = t.startsWith("enter:")
      let rest = t.substr(6).strip()
      if rest.len > 0:
        if isEnter: e.enterCode.add rest else: e.leaveCode.add rest
      else:
        let base = indentOf(ln)
        var j = i + 1
        while j < body.len and body[j].strip().len > 0 and
              indentOf(body[j]) > base:
          let stmt = body[j].substr(base + 2)
          if isEnter: e.enterCode.add stmt else: e.leaveCode.add stmt
          inc j
        i = j - 1
    else:
      e.afterCode.add t
    inc i

proc extract(path: string): seq[Entry] =
  result = @[]
  let lines = readFile(path).splitLines()
  var i = 0
  while i < lines.len:
    let raw = lines[i]
    let t = raw.strip()
    # `  Name "..."` or `  Name(args) """..."""`, possibly with a trailing `:`
    if t.len > 0 and t[0] in {'a'..'z'} and (raw.startsWith("  ") and not raw.startsWith("   ")) and
       ('"' in t):
      var j = 0
      while j < t.len and t[j] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}: inc j
      var e = Entry(name: t.substr(0, j-1), line: i+1)
      var k = j
      if k < t.len and t[k] == '(':
        var depth = 0
        while k < t.len:
          if t[k] == '(': inc depth
          elif t[k] == ')':
            dec depth
            if depth == 0: inc k; break
          inc k
        e.params = t.substr(j, k-1)
        for piece in e.params.strip(chars = {'(', ')'}).split(','):
          let nm = piece.split(':')[0].strip()
          if nm.len > 0: e.paramNames.add nm
      while k < t.len and t[k] == ' ': inc k
      if k >= t.len or t[k] != '"':
        inc i; continue
      var body = ""
      if t.substr(k, k+2) == "\"\"\"":
        var acc = t.substr(k+3)
        while "\"\"\"" notin acc:
          inc i
          acc.add "\n" & lines[i]
        body = acc.substr(0, acc.find("\"\"\"")-1)
        e.hasActions = acc.endsWith("\"\"\":")
      else:
        let close = t.rfind('"')
        body = t.substr(k+1, close-1)
        e.hasActions = t.endsWith("\":")
      e.src = body
      if e.hasActions:
        var acts: seq[string] = @[]
        var k2 = i + 1
        while k2 < lines.len and lines[k2].strip().len > 0 and
              indentOf(lines[k2]) >= 4:
          acts.add lines[k2].substr(4)
          inc k2
        if acts.len == 0:
          echo "[error] ", e.name, " (line ", e.line,
               "): the `:` declares an action block, but none follows"
          inc errors
        parseActions(e, acts)
        i = k2 - 1
      result.add e
    inc i

# --------------------------------------------------------------- analysis

type
  Grammar = object
    rules: OrderedTable[string, seq[Entry]]
    nullable: Table[string, bool]
    first: Table[string, Table[string, IndSet]]
    mask: Table[string, IndSet]                   # rule -> exported indent mask
    follow: Table[string, Table[string, IndSet]]  # rule -> FOLLOW
    params: Table[string, seq[string]]            # rule -> parameter names
    paramSig: Table[string, string]               # rule -> raw Nim parameters
    argsOf: Table[string, seq[Node]]              # "rule.param" -> arguments seen

proc union(a: var Table[string, IndSet]; b: Table[string, IndSet]) =
  for k, v in b:
    if a.hasKey(k): a[k] = a[k] + v
    else: a[k] = v

proc restrict(a: var Table[string, IndSet]; ind: IndSet) =
  var res = initTable[string, IndSet]()
  for k, v in a:
    let n = v * ind
    if n != {}: res[k] = n
  a = res

proc isNullable(g: Grammar; n: Node): bool =
  case n.kind
  of nSeq:
    for k in n.kids:
      if not isNullable(g, k): return false
    true
  of nAlt:
    for k in n.kids:
      if isNullable(g, k): return true
    false
  of nOpt, nRep0: true
  of nRep1, nSepRep: isNullable(g, n.kids[0])
  of nTerminal, nClass: false
  of nRule: g.nullable.getOrDefault(n.text, false)
  of nTag, nIndented, nWithInd: isNullable(g, n.kids[0])
  of nGuard, nPred, nAhead, nDefault, nRaw: true   # zero-width
  of nBinary: n.kids.len > 0 and isNullable(g, n.kids[0])
  of nBinTail: true             # the operator loop may run zero times
  of nLa2: n.kids.len > 0 and isNullable(g, n.kids[0])

proc firstOf(g: Grammar; n: Node): Table[string, IndSet]

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
  of nRule: result = g.mask.getOrDefault(n.text, AnyInd)
  of nTag: result = leadMask(g, n.kids[0])
  of nIndented: result = {icGt}
  of nWithInd: result = AnyInd
  of nBinTail: result = AnyInd
  else: result = AnyInd

proc firstOf(g: Grammar; n: Node): Table[string, IndSet] =
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
  of nOpt, nRep0, nRep1: result = firstOf(g, n.kids[0])
  of nSepRep: result = firstOf(g, n.kids[0])
  of nTerminal: result[tokenKind(n.text)] = AnyInd
  of nClass: result[Classes[n.text]] = AnyInd
  of nRule:
    if g.first.hasKey(n.text): result = g.first[n.text]
    elif g.argsOf.hasKey(n.text):
      # a rule-valued parameter: over-approximate by every argument passed
      for a in g.argsOf[n.text]: union(result, firstOf(g, a))
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
    result = initTable[string, IndSet]()
    for k, v in firstOf(g, n.kids[0]):
      if icEq in v: result[k] = {icGt}
  of nWithInd:
    # `withInd(...)` sets the inner indentation to whatever the first token's
    # is, *without* requiring it to be deeper -- `parser.nim`'s `withInd`
    # template, of which `semiStmtList` is the one use that is not already
    # guarded by `realInd`. So the first token is `IND{=}` inside whatever it
    # was outside (or NO_IND inside if it was NO_IND outside, since `currInd`
    # then becomes -1), and the outward map is the inverse: unconstrained.
    result = initTable[string, IndSet]()
    for k, v in firstOf(g, n.kids[0]):
      var o: IndSet = {}
      if icEq in v: o = AnyInd
      elif icNoInd in v: o = {icNoInd}
      if o != {}: result[k] = o
  of nBinTail: discard    # the head was parsed by whatever precedes it
  of nBinary:
    if n.kids.len > 0: result = firstOf(g, n.kids[0])   # rest are Nim procs
  of nLa2:
    if n.kids.len > 0: result = firstOf(g, n.kids[0])
  of nGuard, nPred, nAhead, nDefault, nRaw: discard

proc remapIn(t: Table[string, IndSet]): Table[string, IndSet] =
  ## Re-measure a FOLLOW set across an `indented(...)` boundary. The inner
  ## `currInd` is strictly greater than the outer one, so a token that was at
  ## the outer level is a dedent inside; a token that was already indented
  ## could be anything relative to the inner level.
  result = initTable[string, IndSet]()
  for k, v in t:
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
  ## Canonical rendering, used both for the report and for prefix comparison.
  case n.kind
  of nSeq:
    var parts: seq[string] = @[]
    for k in n.kids: parts.add render(k)
    parts.join(" ")
  of nAlt:
    var parts: seq[string] = @[]
    for k in n.kids: parts.add render(k)
    "(" & parts.join(" | ") & ")"
  of nOpt: atom(n.kids[0]) & "?"
  of nRep0: atom(n.kids[0]) & "*"
  of nRep1: atom(n.kids[0]) & "+"
  of nSepRep: atom(n.kids[0]) & " " & n.text & " " & atom(n.kids[1])
  of nTerminal: "'" & n.text & "'"
  of nClass: n.text
  of nRule:
    if n.kids.len == 0: n.text
    else:
      var parts: seq[string] = @[]
      for k in n.kids: parts.add render(k)
      n.text & "(" & parts.join(", ") & ")"
  of nTag: (if n.anchored: "^" else: "") & n.text & "[" & render(n.kids[0]) & "]"
  of nGuard: n.text
  of nDefault: "%else"
  of nRaw: "{" & n.text & "}"
  of nPred: "&" & n.text
  of nAhead: "&(" & render(n.kids[0]) & ")"
  of nIndented: "indented(" & render(n.kids[0]) & ")"
  of nWithInd: "withInd(" & render(n.kids[0]) & ")"
  of nBinTail:
    var ps: seq[string] = @[]
    for k in n.kids: ps.add render(k)
    "binaryTail(" & ps.join(", ") & ")"
  of nBinary: "binary(...)"
  of nLa2: "la2(" & render(n.kids[0]) & ")"

proc give(g: var Grammar; rule: string; t: Table[string, IndSet];
          changed: var bool) =
  if not g.follow.hasKey(rule): return
  for k, v in t:
    if g.follow[rule].hasKey(k):
      let merged = g.follow[rule][k] + v
      if merged != g.follow[rule][k]:
        g.follow[rule][k] = merged
        changed = true
    else:
      g.follow[rule][k] = v
      changed = true

proc propagate(g: var Grammar; n: Node; after: Table[string, IndSet];
               changed: var bool) =
  ## Push FOLLOW information down `n`, where `after` is what may follow it.
  case n.kind
  of nSeq:
    var acc = after
    for i in countdown(n.kids.len-1, 0):
      let k = n.kids[i]
      propagate(g, k, acc, changed)
      var f = firstOf(g, k)
      if isNullable(g, k):
        restrict(acc, leadMask(g, k))
        union(acc, f)
      else:
        acc = f
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

proc overlapOf(a, b: Table[string, IndSet]): seq[string] =
  result = @[]
  for ka, va in a:
    for kb, vb in b:
      if compatible(ka, kb) and (va * vb) != {}:
        result.add (if ka == kb: ka else: ka & "~" & kb) & " " & $(va * vb)

proc intersectFirst(a, b: Table[string, IndSet]): Table[string, IndSet] =
  ## What an `&rule` lookahead leaves of an alternative's FIRST set. A
  ## lookahead only ever *narrows*: `&parKeyw` in front of
  ## `complexOrSimpleStmt` means the alternative starts at one of fifteen
  ## keywords, not at everything a statement can start with.
  result = initTable[string, IndSet]()
  for ka, va in a:
    for kb, vb in b:
      if compatible(ka, kb):
        let ind = va * vb
        if ind != {}:
          # the longer spelling is the more specific one (`tkOpr+isDotLike`
          # refines `tkOpr`), so it is the one that survives
          let key = if ka.len >= kb.len: ka else: kb
          if result.hasKey(key): result[key] = result[key] + ind
          else: result[key] = ind

proc leadingItems(n: Node): seq[Node] =
  ## The sequence elements of an alternative, for left-factoring. A leading
  ## tag is transparent: its open token is inserted at the mark once the
  ## alternative is known, so it does not take part in the shared prefix.
  if n.kind == nTag: leadingItems(n.kids[0])
  elif n.kind == nSeq:
    if n.kids.len > 0 and n.kids[0].kind == nTag and n.kids.len == 1:
      leadingItems(n.kids[0])
    else: n.kids
  else: @[n]

proc commonPrefix(a, b: seq[Node]): int =
  result = 0
  while result < a.len and result < b.len and
        render(a[result]) == render(b[result]): inc result

proc tail(items: seq[Node]; start: int): Node =
  result = newNode(nSeq)
  for i in start ..< items.len: result.kids.add items[i]

proc leadAhead(n: Node): Node =
  for it in (if n.kind == nSeq: n.kids else: @[n]):
    if it.kind == nAhead: return it
    if it.kind notin {nGuard, nPred, nDefault}: break
  nil

proc isDefault(n: Node): bool =
  for it in (if n.kind == nSeq: n.kids else: @[n]):
    if it.kind == nDefault: return true
  false

proc hasPredicate(n: Node): bool =
  let items = leadingItems(n)
  for it in items:
    if it.kind in {nPred, nAhead}: return true
    if it.kind notin {nGuard}: break
  false


# --------------------------------------------------------------- emission
#
# Direct-coded recursive descent: one proc per rule, dispatch as an if-chain
# over `(kind, indentClass)`. No tables, no interpreter -- the output is meant
# to be steppable in a debugger, which matters for a compiler front end.

type Emitter = object
  g: Grammar
  outp: string
  indent: int
  tmp: int
  setNames: Table[string, string]     # set literal -> const name
  setOrder: seq[string]
  curRule: string
  needsAnchor: HashSet[string]
  predicated: HashSet[string]   # rules whose alternatives carry predicates

var em: ptr Emitter = nil             # the emitter condFor is building into

proc setLit(lit: string): string =
  ## Hoist anything but a tiny set into a named constant: the generated code
  ## is meant to be read, and a 90-element inline set is not readable.
  if em == nil or lit.count(',') < 4: return lit
  if not em.setNames.hasKey(lit):
    em.setNames[lit] = "Tk" & $(em.setNames.len + 1)
    em.setOrder.add lit
  em.setNames[lit]

proc line(e: var Emitter; s: string) =
  for i in 1 .. e.indent: e.outp.add "  "
  e.outp.add s
  e.outp.add "\n"

proc procName(rule: string): string = "p" & capitalizeAscii(rule)

proc kindTest(key: string): string =
  ## One `(kind, refinement)` key as a Nim condition on `p.tok`.
  if key.contains("+"):
    let parts = key.split('+')
    "(p.tok.kind == " & parts[0] & " and " & parts[1] & "(p.tok))"
  elif key.contains("\""):
    let i = key.find('"')
    "(p.tok.kind == " & key.substr(0, i-1) & " and p.tok.s == " & key.substr(i) & ")"
  elif key.contains(".."):
    "(p.tok.kind in {" & key & "})"
  else:
    "p.tok.kind == " & key

proc indTest(ind: IndSet): string =
  if ind == AnyInd: ""
  else:
    var parts: seq[string] = @[]
    for c in ind:
      parts.add (case c
        of icNoInd: "icNoInd"
        of icLt: "icLt"
        of icEq: "icEq"
        of icGt: "icGt")
    " and indClass(p) in {" & parts.join(", ") & "}"

proc condFor(f: Table[string, IndSet]): string =
  ## A FIRST set as one Nim boolean expression. Keys that share an indentation
  ## constraint and need no refinement collapse into a single set test.
  var plain = initTable[string, seq[string]]()      # $IndSet -> simple kinds
  var special: seq[string] = @[]
  var keys: seq[string] = @[]
  for k in f.keys: keys.add k
  sort keys
  for k in keys:
    let v = f[k]
    if k.contains("+") or k.contains("\""):
      special.add "(" & kindTest(k) & indTest(v) & ")"
    else:
      let tag = $v
      if not plain.hasKey(tag): plain[tag] = @[]
      # a range goes in bare: the enclosing `{...}` is added below, and
      # bracing it here produced `{{tkAddr..tkYield}, tkAnd}` -- a set of sets.
      plain[tag].add k
  var parts: seq[string] = @[]
  var tags: seq[string] = @[]
  for t in plain.keys: tags.add t
  sort tags
  for t in tags:
    var ind: IndSet = {}
    for c in IndClass:
      if ($c) in t: ind = ind + {c}
    parts.add "(p.tok.kind in " & setLit("{" & plain[t].join(", ") & "}") & indTest(ind) & ")"
  for sp in special: parts.add sp
  if parts.len == 0: "false" else: parts.join(" or ")

proc anyInd(f: Table[string, IndSet]): Table[string, IndSet] =
  result = initTable[string, IndSet]()
  for k in f.keys: result[k] = AnyInd

proc predCall(n: Node): string =
  ## `&noSpaceBefore` is `noSpaceBefore(p)`; `&inTypeDesc(mode)` is
  ## `inTypeDesc(p, mode)`. Without the arguments the generated dispatch drops
  ## exactly the information `parser.nim` decides on.
  var args: seq[string] = @["p"]
  for k in n.kids:
    args.add (if k.kind == nRaw: k.text else: render(k))
  n.text & "(" & args.join(", ") & ")"

proc canCall(n: Node): string =
  ## The `canX` companion takes whatever `pX` takes, because its predicates do.
  var args: seq[string] = @["p"]
  for k in n.kids:
    args.add (if k.kind == nRaw: k.text else: render(k))
  "can" & capitalizeAscii(n.text) & "(" & args.join(", ") & ")"

proc condOf(e: Emitter; n: Node): string

proc guardsOf(items: seq[Node]): string =
  ## The semantic predicates and lookaheads leading an alternative.
  var parts: seq[string] = @[]
  for it in items:
    case it.kind
    of nPred: parts.add predCall(it)
    of nAhead: discard        # folded into the FIRST test by the caller
    of nGuard, nDefault: discard
    else: break
  parts.join(" and ")

type Alt = object
  ## An alternative as emission sees it: the tag that wraps it (inserted
  ## retroactively at the mark, so factoring cannot disturb it) and the
  ## sequence items that remain once the tag is peeled off.
  tag: string
  anchored: bool
  items: seq[Node]
  enterCode, leaveCode, afterCode: seq[string]

proc toAlt(n: Node): Alt =
  var inner = n
  if inner.kind == nSeq and inner.kids.len == 1: inner = inner.kids[0]
  if inner.kind == nTag:
    Alt(tag: inner.text, anchored: inner.anchored,
        items: leadingItems(inner.kids[0]))
  else:
    Alt(tag: "", anchored: false, items: leadingItems(n))

proc hasFreeAnchor(n: Node): bool =
  ## True when the node contains a `^tag[...]` that is NOT inside a repetition
  ## of its own rule -- such a tag anchors on the *caller's* accumulated value,
  ## so the mark has to cross the rule boundary as a parameter.
  if n.kind == nTag and n.anchored: return true
  if n.kind in {nRep0, nRep1, nSepRep}: return false
  for k in n.kids:
    if hasFreeAnchor(k): return true
  false

proc raw(n: Node): string =
  ## A `{...}` argument is a Nim expression, not grammar: it reaches the
  ## generated call verbatim, without the braces.
  if n.kind == nRaw: n.text else: render(n)

proc condOf(e: Emitter; n: Node): string =
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
  var base =
    if head != nil and head.kind == nRule and e.predicated.contains(head.text):
      canCall(head)
    else:
      let ah = leadAhead(n)
      if ah != nil: condFor(intersectFirst(firstOf(e.g, n), firstOf(e.g, ah.kids[0])))
      else: condFor(firstOf(e.g, n))
  # An optional or repeated body that LEADS with a semantic predicate is only
  # entered when the predicate holds, and a FIRST set cannot express one -- so
  # it has to be ANDed in here, exactly as `emitAlts` does for alternatives.
  let gd = guardsOf(if n.kind == nSeq: n.kids else: @[n])
  if gd.len > 0: base = "(" & base & ") and " & gd
  base

proc emitNode(e: var Emitter; n: Node; mark, anchor: string)

proc isPureGuard(g: Grammar; n: Node): bool =
  ## Matches no token, only constrains the next one.
  firstOf(g, n).len == 0 and isNullable(g, n)

proc indSetLit(ind: IndSet): string =
  var ps: seq[string] = @[]
  for c in ind:
    ps.add (case c
      of icNoInd: "icNoInd"
      of icLt: "icLt"
      of icEq: "icEq"
      of icGt: "icGt")
  "{" & ps.join(", ") & "}"

template body(e: var Emitter; code: untyped) =
  ## Emit an indented branch body, falling back to `discard` when it is empty.
  inc e.indent
  let before = e.outp.len
  code
  if e.outp.len == before: e.line "discard"
  dec e.indent

proc emitSeq(e: var Emitter; items: seq[Node]; mark, anchor: string) =
  for it in items: emitNode(e, it, mark, anchor)

proc altFirst(g: Grammar; a: Alt): Table[string, IndSet] =
  var n = newNode(nSeq)
  n.kids = a.items
  firstOf(g, n)

proc altCond(g: Grammar; a: Alt): Table[string, IndSet] =
  ## `altFirst` narrowed by a leading `&rule`. Only the *dispatch* uses this;
  ## grouping and nullability still go by the unrestricted set, because the
  ## lookahead says nothing about what the alternative may then consume.
  var n = newNode(nSeq)
  n.kids = a.items
  result = firstOf(g, n)
  let ah = leadAhead(n)
  if ah != nil: result = intersectFirst(result, firstOf(g, ah.kids[0]))

proc altNullable(g: Grammar; a: Alt): bool =
  var n = newNode(nSeq)
  n.kids = a.items
  isNullable(g, n)

proc emitAlts(e: var Emitter; alts: seq[Alt]; rule, mark, anchor: string) =
  ## Left-factor what shares a prefix, then dispatch on one token.
  proc finish(e: var Emitter; a: Alt) =
    for ln in a.enterCode: e.line ln
    emitSeq(e, a.items, mark, anchor)
    for ln in a.leaveCode: e.line ln
    if a.tag.len > 0:
      e.line "wrap p, " & (if a.anchored: anchor else: mark) & ", \"" & a.tag & "\""
    if a.afterCode.len > 0:
      # the bare body inspects what was just parsed, so it needs the mark
      e.line "let m = " & (if a.anchored: anchor else: mark)
      for ln in a.afterCode: e.line ln

  if alts.len == 1:
    finish(e, alts[0])
    return

  # a rule that is nothing but indentation guards is one assertion, not a
  # dispatch: `notInd = NO_IND | IND{=} | IND{<}`
  var allGuards = true
  for a in alts:
    if a.tag.len > 0 or altFirst(e.g, a).len > 0 or not altNullable(e.g, a) or
       a.enterCode.len + a.leaveCode.len + a.afterCode.len > 0:
      allGuards = false
  if allGuards:
    var m: IndSet = {}
    for a in alts:
      var n = newNode(nSeq)
      n.kids = a.items
      m = m + leadMask(e.g, n)
    e.line "checkInd p, " & indSetLit(m)
    return

  var order: seq[string] = @[]
  var groups = initTable[string, seq[Alt]]()
  var defaults: seq[Alt] = @[]
  for a in alts:
    var isGuardOnly = false
    if a.tag.len == 0 and a.items.len > 0:
      var n = newNode(nSeq)
      n.kids = a.items
      isGuardOnly = isPureGuard(e.g, n)
    var marked = false
    for it in a.items:
      if it.kind == nDefault: marked = true
    if marked or a.items.len == 0 or isGuardOnly:
      # `%else`, an alternative exhausted by factoring, or a bare guard: all
      # three are "what is left when nothing else claimed the token"
      defaults.add a
      continue
    let key = render(a.items[0])
    if not groups.hasKey(key):
      groups[key] = @[]
      order.add key
    groups[key].add a

  var first = true
  for key in order:
    let grp = groups[key]
    var f = initTable[string, IndSet]()
    for a in grp: union(f, altCond(e.g, a))
    var cond = condFor(f)
    let gd = (if grp.len == 1: guardsOf(grp[0].items)
              elif grp[0].items[0].kind == nPred: predCall(grp[0].items[0])
              else: "")
    if gd.len > 0: cond = "(" & cond & ") and " & gd
    e.line (if first: "if " else: "elif ") & cond & ":"
    first = false
    body e:
      if grp.len == 1:
        finish(e, grp[0])
      else:
        emitNode(e, grp[0].items[0], mark, anchor)   # the shared prefix, once
        var tails: seq[Alt] = @[]
        for a in grp:
          tails.add Alt(tag: a.tag, anchored: a.anchored,
                        items: a.items[1 .. ^1], enterCode: a.enterCode,
                        leaveCode: a.leaveCode, afterCode: a.afterCode)
        emitAlts(e, tails, rule, mark, anchor)

  if defaults.len > 0:
    if first:
      finish(e, defaults[0])
    else:
      e.line "else:"
      body e:
        finish(e, defaults[0])
  elif not first:
    # An alternative that can match the empty string makes "nothing matched" a
    # legal outcome, so there is nothing to report: `indAndComment` is
    # `(IND{>} COMMENT)? | COMMENT?` and both sides can match nothing. Such an
    # alternative only needs a branch of its own if it carries a tag or an
    # action, which an empty match still has to run.
    var empty = -1
    for i in 0 ..< alts.len:
      if empty < 0 and altNullable(e.g, alts[i]): empty = i
    if empty < 0:
      e.line "else:"
      body e:
        e.line "error p, \"expected " & rule & "\""
    elif alts[empty].tag.len > 0 or alts[empty].enterCode.len +
         alts[empty].leaveCode.len + alts[empty].afterCode.len > 0:
      e.line "else:"
      body e:
        finish(e, alts[empty])

proc emitNode(e: var Emitter; n: Node; mark, anchor: string) =
  case n.kind
  of nSeq:
    emitSeq(e, n.kids, mark, anchor)
  of nAlt:
    inc e.tmp
    let mk = "m" & $e.tmp
    e.line "let " & mk & " = mark(p)"
    var as2: seq[Alt] = @[]
    for k in n.kids: as2.add toAlt(k)
    emitAlts(e, as2, "alternative", mk, anchor)
    e.line "discardUnused " & mk
  of nTerminal:
    e.line "expect p, " & tokenKind(n.text).split('"')[0] &
           (if tokenKind(n.text).contains("\""): ", " & "\"" & n.text & "\"" else: "")
  of nClass:
    e.line "emitLeaf p            # " & n.text
  of nRule:
    if e.g.rules.hasKey(n.text):
      var args: seq[string] = @["p"]
      if e.needsAnchor.contains(n.text): args.add anchor
      for k in n.kids:
        args.add (if k.kind == nRaw: k.text else: render(k))
      if args.len == 1:
        e.line procName(n.text) & " p"
      else:
        e.line procName(n.text) & "(" & args.join(", ") & ")"
    else:
      e.line "# parameter or extern: " & n.text
  of nRaw:
    discard
  of nGuard:
    e.line "checkInd p, " & indSetLit(n.ind)
  of nPred, nAhead, nDefault:
    discard                       # part of the dispatch condition
  of nOpt:
    e.line "if " & condOf(e, n.kids[0]) & ":"
    body e:
      emitNode(e, n.kids[0], mark, anchor)
  of nRep0:
    inc e.tmp
    let m2 = "m" & $e.tmp
    e.line "while " & condOf(e, n.kids[0]) & ":"
    body e:
      e.line "let " & m2 & " = mark(p)"
      emitNode(e, n.kids[0], m2, mark)
      e.line "discardUnused " & m2
  of nRep1:
    inc e.tmp
    let m2 = "m" & $e.tmp
    emitNode(e, n.kids[0], mark, anchor)
    e.line "while " & condOf(e, n.kids[0]) & ":"
    body e:
      e.line "let " & m2 & " = mark(p)"
      emitNode(e, n.kids[0], m2, mark)
      e.line "discardUnused " & m2
  of nSepRep:
    inc e.tmp
    let m2 = "m" & $e.tmp
    var again = newNode(nSeq)
    again.kids.add n.kids[1]
    again.kids.add n.kids[0]
    if n.text == "^*":
      e.line "if " & condOf(e, n.kids[0]) & ":"
      inc e.indent
    e.line "let " & m2 & " = mark(p)"
    emitNode(e, n.kids[0], m2, mark)
    let loopCond = condFor(firstOf(e.g, again))
    let loose = condFor(anyInd(firstOf(e.g, again)))
    e.line "while " & loopCond & ":"
    body e:
      emitNode(e, n.kids[1], m2, mark)
      emitNode(e, n.kids[0], m2, mark)
    if loose != loopCond:
      # Only a token that is *deeper* than expected is unambiguously wrong: a
      # dedent legitimately hands control back to an enclosing construct,
      # which is why this cannot simply be `could have continued but did not`.
      e.line "if (" & loose & ") and indClass(p) == icGt:"
      body e:
        e.line "error p, \"invalid indentation in " & e.curRule & "\""
    e.line "discardUnused " & m2
    if n.text == "^*": dec e.indent
  of nTag:
    if n.anchored:
      emitNode(e, n.kids[0], mark, anchor)
      e.line "wrap p, " & anchor & ", \"" & n.text & "\""
    else:
      emitNode(e, n.kids[0], mark, anchor)
      e.line "wrap p, " & mark & ", \"" & n.text & "\""
  of nWithInd:
    e.line "pushIndAny p"
    emitNode(e, n.kids[0], mark, anchor)
    e.line "popInd p"
  of nIndented:
    e.line "pushInd p"
    emitNode(e, n.kids[0], mark, anchor)
    e.line "popInd p"
  of nBinary:
    # binary(operand, precedenceProc, rightAssocProc, tag)
    let opnd = n.kids[0]
    let precP = render(n.kids[1])
    let assocP = render(n.kids[2])
    let tg = render(n.kids[3])
    # the fifth argument names the parameter that carries the limit; the
    # recursion tightens that one and passes every other parameter through
    let limitExpr = if n.kids.len > 4: render(n.kids[4]) else: "limit"
    emitNode(e, opnd, mark, anchor)
    e.line "var prec = " & precP & "(p)"
    # the operator itself must not start a line -- parser.nim's rule
    e.line "while prec >= " & limitExpr & " and indClass(p) == icNoInd:"
    body e:
      e.line "let assoc = (if " & assocP & "(p): 0 else: 1)"
      e.line "insertLeafAt p, " & mark & ", p.tok.s"
      e.line "getTok p"
      var args: seq[string] = @["p"]
      if e.needsAnchor.contains(e.curRule): args.add anchor
      if e.g.params.hasKey(e.curRule):
        for pn in e.g.params[e.curRule]:
          args.add (if pn == limitExpr: "prec + assoc" else: pn)
      else:
        args.add "prec + assoc"
      e.line procName(e.curRule) & "(" & args.join(", ") & ")"
      e.line "wrap p, " & mark & ", \"" & tg & "\""
      e.line "prec = " & precP & "(p)"
  of nBinTail:
    # `parseOperators` applied to a node that is already on the buffer:
    # `parser.nim` does this in three places, of which `type(x) is type(y)`
    # is the one that matters. `binary(...)` cannot express it, because its
    # first item *is* the left operand and its recursion goes back into the
    # rule it appears in. Here the left operand is whatever the enclosing
    # sequence just parsed, and the recursion names its own rule.
    inc e.tmp
    let pv = "prec" & $e.tmp
    let av = "assoc" & $e.tmp
    let tprec = render(n.kids[1])
    let tassoc = render(n.kids[2])
    let ttag = render(n.kids[3])
    let tlimit = if n.kids.len > 4: raw(n.kids[4]) else: "-1"
    var rest: seq[string] = @[]
    for i in 5 ..< n.kids.len: rest.add raw(n.kids[i])
    e.line "var " & pv & " = " & tprec & "(p)"
    e.line "while " & pv & " >= " & tlimit & " and indClass(p) == icNoInd:"
    body e:
      e.line "let " & av & " = (if " & tassoc & "(p): 0 else: 1)"
      e.line "insertLeafAt p, " & mark & ", p.tok.s"
      e.line "getTok p"
      var args: seq[string] = @["p", pv & " + " & av]
      for r in rest: args.add r
      e.line procName(n.kids[0].text) & "(" & args.join(", ") & ")"
      e.line "wrap p, " & mark & ", \"" & ttag & "\""
      e.line pv & " = " & tprec & "(p)"
  of nLa2:
    e.line "# la2: " & render(n)

proc emitParser(g: Grammar; order: seq[string]; runtime: string): string =
  var e = Emitter(g: g, outp: "", indent: 0, tmp: 0,
                  setNames: initTable[string, string](), setOrder: @[],
                  needsAnchor: initHashSet[string](),
                  predicated: initHashSet[string]())
  for r in order:
    for a in g.rules[r]:
      if hasFreeAnchor(a.body): e.needsAnchor.incl r
      if guardsOf(leadingItems(a.body)).len > 0: e.predicated.incl r
  em = addr e

  # "can this rule start here" for the predicated rules: a caller's loop test
  # has to respect the predicate, and a FIRST set cannot express one.
  for r in order:
    if not e.predicated.contains(r): continue
    e.line "proc can" & capitalizeAscii(r) & "*(p: Parser" &
           (if g.paramSig.hasKey(r): "; " & g.paramSig[r] else: "") & "): bool"
  for r in order:
    if not e.predicated.contains(r): continue
    var parts: seq[string] = @[]
    for a in g.rules[r]:
      let items = leadingItems(a.body)
      var c = condFor(firstOf(g, a.body))
      let gd = guardsOf(items)
      if gd.len > 0: c = "(" & c & ") and " & gd
      parts.add "(" & c & ")"
    e.line "proc can" & capitalizeAscii(r) & "*(p: Parser" &
           (if g.paramSig.hasKey(r): "; " & g.paramSig[r] else: "") & "): bool ="
    inc e.indent
    e.line parts.join(" or ")
    dec e.indent
    e.line ""
  for r in order:
    e.line "proc " & procName(r) & "*(p: var Parser" &
           (if e.needsAnchor.contains(r): "; anchor: Mark" else: "") &
           (if g.paramSig.hasKey(r): "; " & g.paramSig[r] else: "") & ")"
  e.line ""
  for r in order:
    let alts = g.rules[r]
    e.curRule = r
    e.line "proc " & procName(r) & "*(p: var Parser" &
           (if e.needsAnchor.contains(r): "; anchor: Mark" else: "") &
           (if g.paramSig.hasKey(r): "; " & g.paramSig[r] else: "") & ") ="
    inc e.indent
    e.line "let m0 = mark(p)"
    var bodies: seq[Alt] = @[]
    for a in alts:
      var al = toAlt(a.body)
      al.enterCode = a.enterCode
      al.leaveCode = a.leaveCode
      al.afterCode = a.afterCode
      bodies.add al
    if bodies.len == 1 and bodies[0].items.len == 0 and bodies[0].tag.len == 0:
      e.line "discard"
    else:
      emitAlts(e, bodies, r, "m0",
               (if e.needsAnchor.contains(r): "anchor" else: "m0"))
    e.line "discardUnused m0"
    dec e.indent
    e.line ""
  em = nil

  var head = "## GENERATED by src/nifler2/tools/gramcheck.nim -- do not edit.\n" &
             "## One proc per rule; the dispatch is an if-chain over\n" &
             "## (token kind, indentation class).\n\n" &
             "import " & runtime & "\nexport " & runtime & "\n\n"
  if e.setOrder.len > 0:
    head.add "const\n"
    for lit in e.setOrder:
      head.add "  " & e.setNames[lit] & " = " & lit & "\n"
    head.add "\n"
  result = head & e.outp

# --------------------------------------------------------------- main

proc main =
  var emitTo = ""
  var runtime = "parserrt"
  var args: seq[string] = @[]
  for i in 1 .. paramCount():
    let a = paramStr(i)
    if a.startsWith("--emit:"): emitTo = a.substr(7)
    elif a.startsWith("--rt:"): runtime = a.substr(5)
    else: args.add a
  let path = if args.len >= 1: args[0] else: "src/nifler2/nimgrammar.nim"
  let entries = extract(path)
  echo "read ", entries.len, " productions from ", path

  var g = Grammar(rules: initOrderedTable[string, seq[Entry]]())
  for e in entries:
    var e2 = e
    var sc = Scanner(s: e.src, pos: 0, rule: e.name, line: e.line)
    e2.body = parseAlt(sc)
    ws sc
    if sc.pos < sc.s.len:
      err sc, "trailing junk: " & sc.s.substr(sc.pos)
    if not g.rules.hasKey(e.name): g.rules[e.name] = @[]
    g.rules[e.name].add e2
    if e2.paramNames.len > 0:
      g.params[e.name] = e2.paramNames
      g.paramSig[e.name] = e2.params.strip().strip(chars = {'(', ')'})

  # ---- monomorphize rule-valued parameters
  #
  # `section(R: rule)` is not a rule the generator can emit: `R` has no FIRST
  # set of its own. One specialization per distinct argument turns it back
  # into ordinary rules, which is also what code generation would have to do.
  proc subst(n: Node; param: string; arg: Node): Node =
    if n.kind == nRule and n.text == param and n.kids.len == 0:
      return arg
    result = Node(kind: n.kind, text: n.text, anchored: n.anchored,
                  ind: n.ind, kids: @[])
    for k in n.kids: result.kids.add subst(k, param, arg)

  var ruleParams = initTable[string, int]()      # rule -> index of `: rule`
  for name, es in g.rules:
    if es.len == 0 or es[0].params.len == 0: continue
    var i = 0
    for piece in es[0].params.strip(chars = {'(', ')'}).split(','):
      let parts = piece.split(':')
      if parts.len == 2 and parts[1].strip() == "rule":
        ruleParams[name] = i
      inc i

  if ruleParams.len > 0:
    var specialized = initTable[string, bool]()
    proc rewrite(n: Node) =
      if n.kind == nRule and ruleParams.hasKey(n.text) and n.kids.len > 0:
        let idx = ruleParams[n.text]
        let arg = n.kids[idx]
        let newName = n.text & "_" & render(arg)
        if not specialized.hasKey(newName):
          specialized[newName] = true
          let pname = g.params[n.text][idx]
          var copies: seq[Entry] = @[]
          for e in g.rules[n.text]:
            var e2 = e
            e2.name = newName
            e2.params = ""
            e2.paramNames = @[]
            e2.body = subst(e.body, pname, arg)
            copies.add e2
          g.rules[newName] = copies
          for c in copies: rewrite(c.body)
        n.text = newName
        n.kids = @[]
      for k in n.kids: rewrite k
    var names: seq[string] = @[]
    for name in g.rules.keys: names.add name
    for name in names:
      for e in g.rules[name]: rewrite e.body
    for r in ruleParams.keys:
      g.rules.del r
      g.params.del r
      g.paramSig.del r
    echo "monomorphized ", ruleParams.len, " rule-valued parameter(s) into ",
         specialized.len, " specialization(s)"

  echo "distinct rules: ", g.rules.len
  if errors > 0:
    echo "\n", errors, " parse error(s) in the grammar notation."

  # undeclared / unused
  # every argument passed at a call site, keyed by the callee's parameter name
  proc recordArgs(n: Node) =
    if n.kind == nRule and n.kids.len > 0 and g.params.hasKey(n.text):
      let ps = g.params[n.text]
      for i in 0 ..< min(ps.len, n.kids.len):
        if not g.argsOf.hasKey(ps[i]): g.argsOf[ps[i]] = @[]
        g.argsOf[ps[i]].add n.kids[i]
    for k in n.kids: recordArgs k
  for name, es in g.rules:
    for e in es: recordArgs e.body

  # reclassify `&name` -> lookahead where `name` is a declared rule
  proc reclassify(n: Node) =
    if n.kind == nPred and g.rules.hasKey(n.text):
      let inner = newNode(nRule, n.text)
      inner.kids = n.kids
      n.kind = nAhead
      n.text = ""
      n.kids = @[inner]
    for k in n.kids: reclassify k
  for name, es in g.rules:
    for e in es: reclassify e.body

  var used = initHashSet[string]()
  used.incl "module"
  var localParams: HashSet[string]
  proc collect(n: Node) =
    if n.kind == nBinary:
      if n.kids.len > 0: collect n.kids[0]     # the rest are Nim procs
      return
    if n.kind == nBinTail:
      if n.kids.len > 0: used.incl n.kids[0].text   # a bare rule name
      return
    if n.kind == nRule and n.text notin Classes and n.text notin localParams:
      used.incl n.text
    for k in n.kids: collect k
  for name, es in g.rules:
    for e in es:
      localParams = initHashSet[string]()
      for pn in e.paramNames: localParams.incl pn
      collect e.body
  echo "\n--- non-terminals"
  var undeclared: seq[string] = @[]
  for u in used:
    if not g.rules.hasKey(u): undeclared.add u
  var unused: seq[string] = @[]
  for name in g.rules.keys:
    if name notin used: unused.add name
  sort undeclared
  sort unused
  for u in undeclared: echo "  undeclared: ", u
  for u in unused: echo "  unused:     ", u
  if undeclared.len == 0 and unused.len == 0: echo "  all resolved"

  # arity: a parameterized rule called with no arguments generated a call that
  # silently dropped them, which is how `primarySuffix*` came to be emitted
  # without `mode`.
  echo "\n--- arity"
  var arityErrors = 0
  proc checkArity(rule: string; line: int; n: Node; locals: HashSet[string]) =
    if n.kind == nRule and n.text notin Classes and n.text notin locals:
      if g.params.hasKey(n.text):
        let want = g.params[n.text].len
        if n.kids.len != want:
          echo "  [arity] ", rule, " (line ", line, "): ", n.text, " takes ",
               want, " argument(s), called with ", n.kids.len
          inc arityErrors
      elif n.kids.len > 0 and g.rules.hasKey(n.text):
        echo "  [arity] ", rule, " (line ", line, "): ", n.text,
             " takes no arguments, called with ", n.kids.len
        inc arityErrors
    if n.kind == nBinary:
      if n.kids.len > 0: checkArity(rule, line, n.kids[0], locals)
      return                                   # the rest are Nim procs
    if n.kind == nBinTail: return  # kids[0] names the rule, it does not call it
    for k in n.kids: checkArity(rule, line, k, locals)
  for name, es in g.rules:
    for e in es:
      var locals = initHashSet[string]()
      for pn in e.paramNames: locals.incl pn
      checkArity(name, e.line, e.body, locals)
  if arityErrors == 0: echo "  all call sites pass the declared arguments"
  errors += arityErrors

  # a parameter without a type emitted `proc pPrimary*(p: var Parser; mode)`,
  # which is not Nim
  for name, sig in g.paramSig:
    for piece in sig.split(','):
      if ':' notin piece:
        echo "  [arity] ", name, ": parameter '", piece.strip(),
             "' needs a type"
        inc errors

  # nullability to a fixed point
  for name in g.rules.keys: g.nullable[name] = false
  var changed = true
  while changed:
    changed = false
    for name, es in g.rules:
      if g.nullable[name]: continue
      for e in es:
        if isNullable(g, e.body):
          g.nullable[name] = true
          changed = true
          break

  # FIRST to a fixed point
  for name in g.rules.keys:
    g.first[name] = initTable[string, IndSet]()
    g.mask[name] = AnyInd
  changed = true
  var rounds = 0
  while changed and rounds < 200:
    changed = false
    inc rounds
    for name, es in g.rules:
      var mk: IndSet = {}
      for e in es: mk = mk + leadMask(g, e.body)
      if mk != g.mask[name]:
        g.mask[name] = mk
        changed = true
      var f = initTable[string, IndSet]()
      for e in es: union(f, firstOf(g, e.body))
      if f.len != g.first[name].len:
        g.first[name] = f
        changed = true
      else:
        for k, v in f:
          if not g.first[name].hasKey(k) or g.first[name][k] != v:
            g.first[name] = f
            changed = true
            break
  echo "\nFIRST converged after ", rounds, " rounds"

  # nullable repetition bodies
  # FOLLOW
  for name in g.rules.keys: g.follow[name] = initTable[string, IndSet]()
  g.follow["module"]["tkEof"] = AnyInd
  changed = true
  var frounds = 0
  while changed and frounds < 200:
    changed = false
    inc frounds
    for name, es in g.rules:
      let after = g.follow[name]
      for e in es: propagate(g, e.body, after, changed)
  echo "FOLLOW converged after ", frounds, " rounds"

  echo "\n--- nullable repetition bodies (would not terminate)"
  var nullRep = 0
  proc checkRep(rule: string; n: Node) =
    if n.kind in {nRep0, nRep1, nSepRep} and isNullable(g, n.kids[0]):
      echo "  ", rule, ": ", render(n)
      inc nullRep
    for k in n.kids: checkRep(rule, k)
  for name, es in g.rules:
    for e in es: checkRep(name, e.body)
  if nullRep == 0: echo "  none"

  # LL(1) conflicts between alternatives of one rule
  echo "\n--- LL(1) conflicts between alternatives"
  var conflicts = 0
  var factored = 0
  var predResolved = 0
  var needFollow = 0
  var defaulted = 0
  var refined = 0
  for name, es in g.rules:
    if es.len < 2: continue
    for i in 0 ..< es.len:
      for j in i+1 ..< es.len:
        var fi = firstOf(g, es[i].body)
        var fj = firstOf(g, es[j].body)
        var overlap = overlapOf(fi, fj)
        if overlap.len == 0: continue
        if isDefault(es[i].body) or isDefault(es[j].body):
          inc defaulted
          continue
        block refine:
          let ah = leadAhead(es[i].body)
          if ah == nil: break refine
          inc refined
          echo "  [refined] ", name, ": &(", render(ah.kids[0]), ") selects"
          echo "           ", render(es[i].body)
          echo "           over ", render(es[j].body)
          continue

        let ai = leadingItems(es[i].body)
        let aj = leadingItems(es[j].body)
        let pre = commonPrefix(ai, aj)
        if pre > 0:
          # left-factorable: the decision moves to the continuations
          let ti = tail(ai, pre)
          let tj = tail(aj, pre)
          let ni = isNullable(g, ti)
          let nj = isNullable(g, tj)
          var ci = firstOf(g, ti)
          var cj = firstOf(g, tj)
          var cont = overlapOf(ci, cj)
          if cont.len == 0 and not (ni and nj):
            inc factored
            continue
          if ni and nj:
            inc needFollow
            echo "  [needs FOLLOW] ", name, " (lines ", es[i].line, ", ", es[j].line, ")"
            echo "           after ", render(ai[0]), ", both continuations are nullable"
            continue
          if ni or nj:
            # the nullable side yields to FOLLOW(rule); the other must not
            # claim anything FOLLOW also claims
            let other = if ni: cj else: ci
            var clash = overlapOf(other, g.follow[name])
            if clash.len > 0:
              inc conflicts
              sort clash
              echo "  [CONFLICT] ", name, " (lines ", es[i].line, ", ", es[j].line,
                   ") after factoring ", render(ai[0])
              echo "           one continuation is nullable and FOLLOW(", name,
                   ") overlaps the other on: ",
                   clash[0 ..< min(6, clash.len)].join(", "),
                   (if clash.len > 6: " ... (" & $clash.len & " total)" else: "")
              continue
          let ahf = leadAhead(ti)
          if ahf != nil:
            inc refined
            echo "  [refined] ", name, " after factoring ", render(ai[0]),
                 ": &(", render(ahf.kids[0]), ") selects"
            echo "           ", render(ti)
            echo "           over ", render(tj)
            continue
          if hasPredicate(ti) or hasPredicate(tj):
            inc predResolved
            echo "  [pred] ", name, " after factoring ", render(ai[0])
            echo "           ", render(ti)
            echo "           ", render(tj)
            continue
          inc conflicts
          echo "  [CONFLICT] ", name, " (lines ", es[i].line, ", ", es[j].line, ") after factoring ", render(ai[0])
          echo "           ", render(ti)
          echo "           ", render(tj)
          sort cont
          echo "           on: ", cont[0 ..< min(6, cont.len)].join(", "),
               (if cont.len > 6: " ... (" & $cont.len & " total)" else: "")
          continue

        if hasPredicate(es[i].body) or hasPredicate(es[j].body):
          inc predResolved
          echo "  [pred] ", name, ":"
          echo "           ", render(es[i].body)
          echo "           ", render(es[j].body)
          continue
        inc conflicts
        echo "  [CONFLICT] ", name, " (lines ", es[i].line, ", ", es[j].line, ")"
        echo "           ", render(es[i].body)
        echo "           ", render(es[j].body)
        sort overlap
        echo "           on: ", overlap[0 ..< min(6, overlap.len)].join(", "),
             (if overlap.len > 6: " ... (" & $overlap.len & " total)" else: "")

  if emitTo.len > 0:
    var order: seq[string] = @[]
    for name in g.rules.keys: order.add name
    writeFile emitTo, emitParser(g, order, runtime)
    echo "\nemitted ", emitTo

  if args.len >= 2:
    let q = args[1]
    echo "\n--- ", q
    if g.rules.hasKey(q):
      echo "  nullable: ", g.nullable[q], "   mask: ", g.mask[q]
      var ks: seq[string] = @[]
      for k, v in g.first[q]: ks.add k & " " & $v
      sort ks
      echo "  FIRST  (", ks.len, "): ", ks.join(", ")
      ks = @[]
      for k, v in g.follow[q]: ks.add k & " " & $v
      sort ks
      echo "  FOLLOW (", ks.len, "): ", ks.join(", ")
    else:
      echo "  no such rule"

  var withFollow = 0
  for name in g.rules.keys:
    if g.follow[name].len > 0: inc withFollow

  echo "\n=== summary"
  echo "  productions:            ", entries.len
  echo "  rules:                  ", g.rules.len
  echo "  rules with FOLLOW:      ", withFollow
  echo "  notation errors:        ", errors
  echo "  left-factored pairs:    ", factored
  echo "  predicate-separated:    ", predResolved
  echo "  FIRST-set refinements:  ", refined
  echo "  declared %else:         ", defaulted
  echo "  need FOLLOW:            ", needFollow
  echo "  unresolved conflicts:   ", conflicts

main()
