#
#
#           Nifler2: Nim to NIF
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Source code filters: a first line `#? stdtmpl | standard` turns the rest of
## the file into Nim code before it is parsed. Ported from Nim's
## `syntaxes.nim` (the `#?` pipe), `filters.nim` (`strip`, `replace`) and
## `filter_tmpl.nim` (`stdtmpl`), text in and text out, so that the parser
## and everything after it never know a filter was there.
##
## The pipe itself is Nim code and is parsed by nifler2's own parser; the
## filters read their arguments from that tree.

import std / [strutils, syncio]
import nimgrammar

type
  LineReader = object
    ## `llStreamReadLine`. The file stream and the string stream a filter
    ## writes do not agree on a trailing empty line: the file's `readLine`
    ## returns it, the string stream's loop does not.
    s: string
    rd: int
    fromFile: bool

proc readLine(r: var LineReader; line: var string): bool =
  line.setLen 0
  if r.fromFile:
    if r.rd >= r.s.len: return false
    while r.rd < r.s.len and r.s[r.rd] != '\n':
      line.add r.s[r.rd]
      inc r.rd
    if r.rd < r.s.len: inc r.rd
    if line.len > 0 and line[line.len-1] == '\r': line.setLen line.len - 1
    result = true
  else:
    while r.rd < r.s.len:
      let c = r.s[r.rd]
      if c == '\r':
        inc r.rd
        if r.rd < r.s.len and r.s[r.rd] == '\n': inc r.rd
        break
      elif c == '\n':
        inc r.rd
        break
      else:
        line.add c
        inc r.rd
    result = line.len > 0 or r.rd < r.s.len

type
  FilterContext = object
    filename: string
    failed*: bool

proc filterError(ctx: var FilterContext; line, col: int; msg: string) =
  ## `localError`: reported, and the parse goes on -- nifler exits with an
  ## error at the end, but its first line is this one.
  echo ctx.filename, "(", line, ", ", col + 1, ") Error: ", msg
  ctx.failed = true

# ------------------------------------------------------------ the pipe tree

proc tag(c: Cursor): string {.inline.} = c.tags.tags[resolvedTagId(c)]

proc render(c: Cursor): string =
  ## Enough of `renderTree` for the arguments a filter can be given.
  case c.kind
  of Ident: result = strVal(c)
  of IntLit: result = $intVal(c)
  of StrLit: result = "\"" & strVal(c) & "\""
  of CharLit:
    result = "'"
    result.add charLit(c)
    result.add "'"
  of TagLit:
    var k = childCursor(c)
    case tag(c)
    of "vv":
      result = render(k)
      skip k
      result.add " = " & render(k)
    of "suf":
      result = render(k)
    of "call", "cmd":
      result = render(k)
      skip k
      result.add "("
      var first = true
      while k.hasMore:
        if not first: result.add ", "
        first = false
        result.add render(k)
        skip k
      result.add ")"
    of "infix":
      let op = render(k)
      skip k
      result = render(k)
      skip k
      result.add " " & op & " " & render(k)
    else:
      result = tag(c)
  else:
    result = ""

proc invalidPragma(ctx: var FilterContext; n: Cursor) =
  let info = rawLineInfo(n)
  filterError ctx, info.line, info.col, "'" & render(n) & "' not allowed here"

proc getArg(n: Cursor; name: string; pos: int; found: var Cursor): bool =
  ## The argument called `name`, or else the `pos`-th one (the callee is 0).
  if n.kind != TagLit: return false
  var k = childCursor(n)
  skip k
  var i = 1
  while k.hasMore:
    if k.kind == TagLit and tag(k) == "vv":
      let key = childCursor(k)
      if key.kind == Ident and cmpIgnoreStyle(strVal(key), name) == 0:
        found = key
        skip found
        return true
    elif i == pos:
      found = k
      return true
    skip k
    inc i
  false

proc charArg(ctx: var FilterContext; n: Cursor; name: string; pos: int;
             default: char): char =
  var x = n
  if not getArg(n, name, pos, x): result = default
  elif x.kind == CharLit: result = charLit(x)
  else:
    result = '\0'
    invalidPragma ctx, n

proc strArg(ctx: var FilterContext; n: Cursor; name: string; pos: int;
            default: string): string =
  var x = n
  if not getArg(n, name, pos, x): return default
  if x.kind == TagLit and tag(x) == "suf":
    x = childCursor(x)             # a raw or triple-quoted string
  if x.kind == StrLit: result = strVal(x)
  else:
    result = ""
    invalidPragma ctx, n

proc boolArg(ctx: var FilterContext; n: Cursor; name: string; pos: int;
             default: bool): bool =
  var x = n
  if not getArg(n, name, pos, x): result = default
  elif x.kind == Ident and cmpIgnoreStyle(strVal(x), "true") == 0: result = true
  elif x.kind == Ident and cmpIgnoreStyle(strVal(x), "false") == 0: result = false
  else:
    result = false
    invalidPragma ctx, n

# ------------------------------------------------------------ strip, replace

const Whitespace = {' ', '\t', '\v', '\r', '\n', '\f'}

proc strip(s: string; leading, trailing: bool): string =
  var a = 0
  var b = s.len - 1
  if leading:
    while a <= b and s[a] in Whitespace: inc a
  if trailing:
    while b >= a and s[b] in Whitespace: dec b
  result = s.substr(a, b)

proc filterStrip(ctx: var FilterContext; input: var LineReader; call: Cursor): string =
  let pattern = strArg(ctx, call, "startswith", 1, "")
  let leading = boolArg(ctx, call, "leading", 2, true)
  let trailing = boolArg(ctx, call, "trailing", 3, true)
  result = ""
  var line = ""
  while readLine(input, line):
    let stripped = strip(line, leading, trailing)
    if pattern.len == 0 or startsWith(stripped, pattern):
      result.add stripped
    else:
      result.add line
    result.add '\n'

proc filterReplace(ctx: var FilterContext; input: var LineReader; call: Cursor): string =
  let sub = strArg(ctx, call, "sub", 1, "")
  if sub.len == 0: invalidPragma ctx, call
  let by = strArg(ctx, call, "by", 2, "")
  result = ""
  var line = ""
  while readLine(input, line):
    result.add (if sub.len == 0: line else: replace(line, sub, by))
    result.add '\n'

# ------------------------------------------------------------ stdtmpl

type
  TmplState = enum
    psDirective, psTempl
  TmplParser = object
    state: TmplState
    line, col: int
    indent, emitPar: int
    x: string                  ## the current input line
    outp: string
    subsChar, nimDirective: char
    emit, conc, toStr: string
    curly, bracket, par: int
    pendingExprLine: bool

const
  PatternChars = {'a'..'z', 'A'..'Z', '0'..'9', '\x80'..'\xFF', '.', '_'}
  LineContinuationOprs = {'+', '-', '*', '/', '\\', '<', '>', '^',
                          '|', '%', '&', '$', '@', '~', ','}

proc at(p: TmplParser; i: int): char {.inline.} =
  ## Nim's `p.x[j]` one past the end reads the string's terminator.
  if i < p.x.len: p.x[i] else: '\0'

proc spacesN(n: int): string {.inline.} = spaces(max(n, 0))

proc newLine(p: var TmplParser) =
  p.outp.add repeat(')', p.emitPar)
  p.emitPar = 0
  if p.line > 1: p.outp.add "\n"
  if p.pendingExprLine:
    p.outp.add spacesN(2)
    p.pendingExprLine = false

proc scanPar(p: var TmplParser; d: int) =
  var i = d
  while i < p.x.len:
    case p.x[i]
    of '(': inc p.par
    of ')': dec p.par
    of '[': inc p.bracket
    of ']': dec p.bracket
    of '{': inc p.curly
    of '}': dec p.curly
    else: discard
    inc i

proc withInExpr(p: TmplParser): bool {.inline.} =
  p.par > 0 or p.bracket > 0 or p.curly > 0

proc hexByte(c: char): string =
  const HexChars = "0123456789ABCDEF"
  result = ""
  result.add HexChars[ord(c) shr 4]
  result.add HexChars[ord(c) and 15]

proc parseLine(ctx: var FilterContext; p: var TmplParser) =
  var j = 0
  let len = p.x.len

  while j < len and p.x[j] == ' ': inc j

  if len >= 2 and p.x[0] == p.nimDirective and p.x[1] == '?':
    newLine p
  elif j < len and p.x[j] == p.nimDirective:
    newLine p
    inc j
    while j < len and p.x[j] == ' ': inc j
    let d = j
    var keyw = ""
    while j < len and p.x[j] in PatternChars:
      keyw.add p.x[j]
      inc j

    scanPar p, j
    p.pendingExprLine = withInExpr(p) or
      (len > 0 and p.x[len-1] in LineContinuationOprs)
    case keyw
    of "end":
      if p.indent >= 2:
        dec p.indent, 2
      else:
        filterError ctx, p.line, j, "'end' does not close a control flow construct"
      p.outp.add spacesN(p.indent)
      p.outp.add "#end"
    of "if", "when", "try", "while", "for", "block", "case", "proc", "iterator",
       "converter", "macro", "template", "method", "func":
      p.outp.add spacesN(p.indent)
      p.outp.add substr(p.x, d)
      inc p.indent, 2
    of "elif", "of", "else", "except", "finally":
      p.outp.add spacesN(p.indent - 2)
      p.outp.add substr(p.x, d)
    of "let", "var", "const", "type":
      p.outp.add spacesN(p.indent)
      p.outp.add substr(p.x, d)
      if not (p.x.contains(':') or p.x.contains('=')):
        # no inline element --> treat as block:
        inc p.indent, 2
    else:
      p.outp.add spacesN(p.indent)
      p.outp.add substr(p.x, d)
    p.state = psDirective
  else:
    # data line
    p.par = 0
    p.curly = 0
    p.bracket = 0
    j = 0
    case p.state
    of psTempl:
      # next line of string literal:
      p.outp.add p.conc
      p.outp.add "\n"
      p.outp.add spacesN(p.indent + 2)
      p.outp.add "\""
    of psDirective:
      newLine p
      p.outp.add spacesN(p.indent)
      p.outp.add p.emit
      p.outp.add "(\""
      inc p.emitPar
    p.state = psTempl
    while j < len:
      let c = p.x[j]
      if c in {'\x01'..'\x1F', '\x80'..'\xFF'}:
        p.outp.add "\\x"
        p.outp.add hexByte(c)
        inc j
      elif c == '\\':
        p.outp.add "\\\\"
        inc j
      elif c == '\'':
        p.outp.add "\\\'"
        inc j
      elif c == '\"':
        p.outp.add "\\\""
        inc j
      elif c == p.subsChar:
        # parse Nim expression:
        inc j
        let e = p.at(j)
        if e == '{':
          p.col = j
          p.outp.add '\"'
          p.outp.add p.conc
          p.outp.add p.toStr
          p.outp.add '('
          inc j
          var curly = 0
          while j < len:
            case p.x[j]
            of '{':
              inc j
              inc curly
              p.outp.add '{'
            of '}':
              inc j
              if curly == 0: break
              if curly > 0: dec curly
              p.outp.add '}'
            else:
              p.outp.add p.x[j]
              inc j
          if curly > 0:
            filterError ctx, p.line, p.col, "expected closing '}'"
            break
          p.outp.add ')'
          p.outp.add p.conc
          p.outp.add '\"'
        elif e in {'a'..'z', 'A'..'Z', '\x80'..'\xFF'}:
          p.outp.add '\"'
          p.outp.add p.conc
          p.outp.add p.toStr
          p.outp.add '('
          while j < len and p.x[j] in PatternChars:
            p.outp.add p.x[j]
            inc j
          p.outp.add ')'
          p.outp.add p.conc
          p.outp.add '\"'
        elif e == p.subsChar:
          p.outp.add p.subsChar
          inc j
        else:
          filterError ctx, p.line, j, "invalid expression"
      else:
        p.outp.add c
        inc j
    p.outp.add "\\n\""

proc filterTmpl(ctx: var FilterContext; input: var LineReader; call: Cursor): string =
  var p = TmplParser(
    subsChar: charArg(ctx, call, "subschar", 1, '$'),
    nimDirective: charArg(ctx, call, "metachar", 2, '#'),
    emit: strArg(ctx, call, "emit", 3, "result.add"),
    conc: strArg(ctx, call, "conc", 4, " & "),
    toStr: strArg(ctx, call, "tostring", 5, "$"),
    outp: "", x: "")
  # do not process the first line which contains the directive:
  if readLine(input, p.x):
    inc p.line
  while readLine(input, p.x):
    inc p.line
    parseLine ctx, p
  newLine p
  result = move p.outp

# ------------------------------------------------------------ the dispatcher

type
  FilterKind = enum
    filtNone = "none"
    filtTemplate = "stdtmpl"
    filtReplace = "replace"
    filtStrip = "strip"

proc getFilter(name: string): FilterKind =
  result = filtNone
  for i in low(FilterKind) .. high(FilterKind):
    if cmpIgnoreStyle(name, $i) == 0:
      return i

proc applyFilter(ctx: var FilterContext; n: Cursor; input: var LineReader) =
  var callee = ""
  if n.kind == Ident:
    callee = strVal(n)
  elif n.kind == TagLit and tag(n) in ["call", "cmd", "callstrlit", "infix",
                                         "prefix", "postfix"] and
       childCursor(n).kind == Ident:
    callee = strVal(childCursor(n))
  else:
    let info = rawLineInfo(n)
    filterError ctx, info.line, info.col, "invalid filter: " & render(n)
  var output = ""
  case getFilter(callee)
  of filtNone: return
  of filtTemplate: output = filterTmpl(ctx, input, n)
  of filtStrip: output = filterStrip(ctx, input, n)
  of filtReplace: output = filterReplace(ctx, input, n)
  input = LineReader(s: output, rd: 0, fromFile: false)

proc evalPipe(ctx: var FilterContext; n: Cursor; input: var LineReader) =
  if n.kind == TagLit and tag(n) == "infix" and childCursor(n).kind == Ident and
      strVal(childCursor(n)) == "|":
    var k = childCursor(n)
    skip k
    for i in 1..2:
      if k.kind == TagLit and tag(k) == "infix":
        evalPipe ctx, k, input
      else:
        applyFilter ctx, k, input
      skip k
  elif n.kind == TagLit and tag(n) == "stmts":
    let k = childCursor(n)
    if k.hasMore: evalPipe ctx, k, input
  else:
    applyFilter ctx, n, input

proc containsShebang(s: string; i: int): bool =
  if i+1 < s.len and s[i] == '#' and s[i+1] == '!':
    var j = i + 2
    while j < s.len and s[j] in Whitespace: inc j
    result = j < s.len and s[j] == '/'
  else:
    result = false

proc applyFilters*(src, filename: string; failed: var bool): string =
  ## `syntaxes.openParser`: the source the parser gets, which is `src` itself
  ## unless the first line (after a BOM and a shebang line) is a `#?` pipe.
  var r = LineReader(s: src, rd: 0, fromFile: true)
  var line = ""
  discard readLine(r, line)
  var i = if line.len >= 3 and line[0] == '\xEF' and line[1] == '\xBB' and
             line[2] == '\xBF': 3 else: 0
  if containsShebang(line, i):
    discard readLine(r, line)
    i = 0
  if not (i+1 < line.len and line[i] == '#' and line[i+1] == '?'):
    return src
  inc i, 2
  while i < line.len and line[i] in Whitespace: inc i
  var pipe = openParser(substr(line, i), filename)
  pModule pipe
  var ctx = FilterContext(filename: filename)
  var input = LineReader(s: src, rd: 0, fromFile: true)
  var c = beginRead(pipe.dest)
  evalPipe ctx, c, input
  endRead c
  failed = ctx.failed
  result = if input.fromFile: src else: move input.s
