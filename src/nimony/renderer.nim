#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import ".." / lib / [bitabs, lineinfos, nifstreams, nifcursors, filelinecache, symParser]

import nimony_model, decls

import compiler / lexer

import std/[strutils, assertions]

## Rendering of Nim code from a cursor.

proc skipParRi(n: var Cursor) =
  assert n.kind == ParRi
  inc n

type
  TRenderFlag* = enum
    renderNone, renderNoBody, renderNoComments, renderDocComments,
    renderNoPragmas, renderIds, renderNoProcDefs, renderSyms, renderRunnableExamples,
    renderIr, renderNonExportedFields, renderExpandUsing, renderNoPostfix

  TRenderFlags* = set[TRenderFlag]
  TRenderTok* = object
    kind*: TokType
    length*: int16
    sym*: SymId

  Section = enum
    GenericParams
    ObjectDef

  TRenderTokSeq* = seq[TRenderTok]
  TSrcGen* = object
    indent*: int
    lineLen*: int
    col: int
    pos*: int              # current position for iteration over the buffer
    idx*: int              # current token index for iteration over the buffer
    tokens*: TRenderTokSeq
    buf*: string
    pendingNL*: int        # negative if not active; else contains the
                           # indentation value
    pendingWhitespace: int
    # comStack*: seq[PNode]  # comment stack
    flags*: TRenderFlags
    inside: set[Section] # Keeps track of contexts we are in
    checkAnon: bool        # we're in a context that can contain sfAnon
    inPragma: int
    when defined(nimpretty):
      pendingNewlineCount: int
    # fid*: FileIndex
    # config*: ConfigRef
    mangler: seq[SymId]

  TSubFlag = enum
    rfLongMode, rfInConstExpr
  TSubFlags = set[TSubFlag]
  TContext = tuple[spacing: int, flags: TSubFlags]

const
  Space = " "
  emptyContext: TContext = (spacing: 0, flags: {})
  IndentWidth = 2
  longIndentWid = IndentWidth * 2
  MaxLineLen = 80
  LineCommentColumn = 30


proc initContext(): TContext =
  result = (spacing: 0, flags: {})

proc addTok(g: var TSrcGen, kind: TokType, s: string; sym: SymId = SymId(0)) =
  g.tokens.add TRenderTok(kind: kind, length: int16(s.len), sym: sym)
  g.buf.add(s)
  if kind != tkSpaces:
    inc g.col, s.len

proc addPendingNL(g: var TSrcGen) =
  if g.pendingNL >= 0:
    when defined(nimpretty):
      let newlines = repeat("\n", clamp(g.pendingNewlineCount, 1, 3))
    else:
      const newlines = "\n"
    addTok(g, tkSpaces, newlines & spaces(g.pendingNL))
    g.lineLen = g.pendingNL
    g.col = g.pendingNL
    g.pendingNL = - 1
    g.pendingWhitespace = -1
  elif g.pendingWhitespace >= 0:
    addTok(g, tkSpaces, spaces(g.pendingWhitespace))
    g.pendingWhitespace = -1

proc putNL(g: var TSrcGen, indent: int) =
  if g.pendingNL >= 0: addPendingNL(g)
  else:
    addTok(g, tkSpaces, "\n")
    g.col = 0

  g.pendingNL = indent
  g.lineLen = indent
  g.pendingWhitespace = -1

proc previousNL(g: TSrcGen): bool =
  result = g.pendingNL >= 0 or (g.tokens.len > 0 and
                                g.tokens[^1].kind == tkSpaces)

proc putNL(g: var TSrcGen) =
  putNL(g, g.indent)

proc optNL(g: var TSrcGen, indent: int) =
  g.pendingNL = indent
  g.lineLen = indent
  g.col = g.indent
  when defined(nimpretty): g.pendingNewlineCount = 0

proc optNL(g: var TSrcGen) =
  optNL(g, g.indent)

proc indentNL(g: var TSrcGen) =
  inc(g.indent, IndentWidth)
  g.pendingNL = g.indent
  g.lineLen = g.indent

proc dedent(g: var TSrcGen) =
  dec(g.indent, IndentWidth)
  assert(g.indent >= 0)
  if g.pendingNL > IndentWidth:
    dec(g.pendingNL, IndentWidth)
    dec(g.lineLen, IndentWidth)

proc put(g: var TSrcGen, kind: TokType, s: string; sym: SymId = SymId(0)) =
  if kind != tkSpaces:
    addPendingNL(g)
    if s.len > 0 or kind in {tkHideableStart, tkHideableEnd}:
      addTok(g, kind, s, sym)
  else:
    g.pendingWhitespace = s.len
    inc g.col, s.len
  inc(g.lineLen, s.len)

proc putComment(g: var TSrcGen, s: string) =
  if s.len == 0: return
  var i = 0
  let hi = s.len - 1
  let isCode = (s.len >= 2) and (s[1] != ' ')
  let ind = g.col
  var com = "## "
  while i <= hi:
    case s[i]
    of '\0':
      break
    of '\r':
      put(g, tkComment, com)
      com = "## "
      inc(i)
      if i <= hi and s[i] == '\n': inc(i)
      optNL(g, ind)
    of '\n':
      put(g, tkComment, com)
      com = "## "
      inc(i)
      optNL(g, ind)
    of ' ', '\t':
      com.add(s[i])
      inc(i)
    else:
      # we may break the comment into a multi-line comment if the line
      # gets too long:
      # compute length of the following word:
      var j = i
      while j <= hi and s[j] > ' ': inc(j)
      if not isCode and (g.col + (j - i) > MaxLineLen):
        put(g, tkComment, com)
        optNL(g, ind)
        com = "## "
      while i <= hi and s[i] > ' ':
        com.add(s[i])
        inc(i)
  put(g, tkComment, com)
  optNL(g)

proc maxLineLength(s: string): int =
  result = 0
  if s.len == 0: return 0
  var i = 0
  let hi = s.len - 1
  var lineLen = 0
  while i <= hi:
    case s[i]
    of '\0':
      break
    of '\r':
      inc(i)
      if i <= hi and s[i] == '\n': inc(i)
      result = max(result, lineLen)
      lineLen = 0
    of '\n':
      inc(i)
      result = max(result, lineLen)
      lineLen = 0
    else:
      inc(lineLen)
      inc(i)

proc putRawStr(g: var TSrcGen, kind: TokType, s: string) =
  var i = 0
  let hi = s.len - 1
  var str = ""
  while i <= hi:
    case s[i]
    of '\r':
      put(g, kind, str)
      str = ""
      inc(i)
      if i <= hi and s[i] == '\n': inc(i)
      optNL(g, 0)
    of '\n':
      put(g, kind, str)
      str = ""
      inc(i)
      optNL(g, 0)
    else:
      str.add(s[i])
      inc(i)
  put(g, kind, str)

proc containsNL(s: string): bool =
  for i in 0..<s.len:
    case s[i]
    of '\r', '\n':
      return true
    else:
      discard
  result = false


proc putWithSpace(g: var TSrcGen; kind: TokType, s: string) =
  put(g, kind, s)
  put(g, tkSpaces, Space)



proc lsub(g: TSrcGen; n: Cursor): int =
  result = 0

proc gsub(g: var TSrcGen; n: var Cursor, fromStmtList = false, isTopLevel = false)


proc gstmts(g: var TSrcGen, n: var Cursor, c: TContext, doIndent=false) =
  inc n
  if doIndent: indentNL(g)

  while n.kind != ParRi:
    optNL(g)
    gsub(g, n)

  if doIndent: dedent(g)
  skipParRi(n)

proc gblock(g: var TSrcGen, n: var Cursor) =
  var c: TContext = initContext()
  inc n

  if n.kind != DotToken:
    putWithSpace(g, tkBlock, "block")
    gsub(g, n)
  else:
    put(g, tkBlock, "block")
    inc n

  putWithSpace(g, tkColon, ":")

  gstmts(g, n, c, doIndent = true)

  skipParRi(n)

proc gcond(g: var TSrcGen, n: var Cursor) =
  # if n.kind == nkStmtListExpr:
  #   put(g, tkParLe, "(")
  gsub(g, n)
  # if n.kind == nkStmtListExpr:
  #   put(g, tkParRi, ")")

proc gif(g: var TSrcGen, n: var Cursor) =

  inc n
  var c: TContext = initContext()

  var isFirst = true

  while n.kind != ParRi:
    case n.substructureKind
    of ElifU:
      inc n
      if isFirst:
        isFirst = false
      else:
        optNL(g)
        putWithSpace(g, tkElif, "elif")
      gcond(g, n)
      putWithSpace(g, tkColon, ":")
      gstmts(g, n, c, doIndent = true)
      skipParRi(n)
    of ElseU:
      optNL(g)
      put(g, tkElse, "else")
      putWithSpace(g, tkColon, ":")
      inc n
      gstmts(g, n, c, doIndent = true)
      skipParRi(n)
    else:
      raiseAssert "unreachable"

  skipParRi(n)

proc gproc(g: var TSrcGen, n: var Cursor) =
  skip n

proc gsub(g: var TSrcGen, n: var Cursor, c: TContext, fromStmtList = false, isTopLevel = false) =
  case n.kind
  of ParLe:
    case n.exprKind
    of NoExpr:
      case n.stmtKind
      of StmtsS:
        gstmts(g, n, c)
      of VarS, LetS, CursorS, ConstS, GvarS, TvarS, GletS, TletS:
        let descriptor: string
        let tk: TokType
        case n.stmtKind
        of VarS, GvarS, TvarS:
          descriptor = "var"
          tk = tkVar
        of CursorS, LetS, GletS, TletS:
          descriptor = "let"
          tk = tkLet
        of ConstS:
          descriptor = "const"
          tk = tkConst
        else:
          raiseAssert "unreachable"
        let decl = takeLocal(n, SkipFinalParRi)
        putWithSpace(g, tk, descriptor)
        var name = decl.name
        var value = decl.val
        gsub(g, name)

        if value.kind != DotToken:
          put(g, tkSpaces, Space)
          putWithSpace(g, tkEquals, "=")
          gsub(g, value)

      of BlockS:
        gblock(g, n)

      of IfS:
        putWithSpace(g, tkIf, "if")
        gif(g, n)

      of WhenS:
        putWithSpace(g, tkIf, "when")
        gif(g, n)

      of ProcS: # FuncS, MacroS, MethodS, ConverterS, IteratorS
        putWithSpace(g, tkProc, "proc")
        gproc(g, n)

      of DiscardS:
        putWithSpace(g, tkDiscard, "discard")
        inc n
        gsub(g, n)
        skipParRi(n)
      else:
        skip n
    of TrueX:
      put(g, tkSymbol, "true")
      skip n
    of FalseX:
      put(g, tkSymbol, "false")
      skip n
    of NilX:
      put(g, tkSymbol, "nil")
      skip n
    else:
      skip n
  of ParRi:
    inc n
  of IntLit:
    put(g, tkIntLit, toString(n, false))
    inc n
  of UIntLit:
    put(g, tkUIntLit, toString(n, false))
    inc n
  of FloatLit:
    put(g, tkFloatLit, toString(n, false))
    inc n
  of StringLit:
    put(g, tkStrLit, toString(n, false))
    inc n
  of Symbol, SymbolDef:
    var name = pool.syms[n.symId]
    extractBasename(name)
    put(g, tkSymbol, name)
    inc n
  else:
    inc n

proc gsub(g: var TSrcGen; n: var Cursor, fromStmtList = false, isTopLevel = false) =
  var c: TContext = initContext()
  gsub(g, n, c, isTopLevel = isTopLevel)

proc renderTree(n: Cursor, renderFlags: TRenderFlags = {}): string =
  var g: TSrcGen = TSrcGen()
  var n = n
  gsub(g, n, isTopLevel = true)
  result = g.buf

proc asNimCode*(n: Cursor): string =
  var m0: PackedLineInfo = NoLineInfo
  var m1: PackedLineInfo = NoLineInfo
  var nested = 0
  var n2 = n
  var file0 = FileId 0

  # echo renderTree(n2)

  while true:
    if n2.info.isValid:
      let currentFile = getFileId(pool.man, n2.info)
      if not m0.isValid:
        m0 = n2.info
        file0 = currentFile
      elif not m1.isValid and currentFile == file0:
        m1 = n2.info
    case n2.kind
    of ParLe:
      inc nested
    of ParRi:
      dec nested
    else:
      discard
    if nested == 0: break
    inc n2

  when false: #if m0.isValid:
    if file0.isValid:
      let (_, line0, col0) = unpack(pool.man, m0)
      if m1.isValid:
        let (_, line1, col1) = unpack(pool.man, m1)
        result = extract(pool.files[file0],
                        FilePosition(line: line0, col: col0),
                        FilePosition(line: line1, col: col1))
      else:
        result = extract(pool.files[file0], FilePosition(line: line0, col: col0))
    else:
      result = ""
    var visible = false
    for i in 0..<result.len:
      if result[i] > ' ':
        visible = true
        break
    if not visible:
      result = toString(n, false)
  else:
    # Fallback to the NIF representation as it is much better than nothing:
    result = toString(n, false)

proc typeToString*(n: Cursor): string =
  result = asNimCode(n)
