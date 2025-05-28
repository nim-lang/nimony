#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import ".." / lib / [bitabs, lineinfos, nifstreams, nifcursors, filelinecache, symparser]

import nimony_model, decls

import "$nim" / compiler / lexer

import std/[strutils, assertions]

## Rendering of Nim code from a cursor.

proc skipParRi(n: var Cursor) =
  if n.kind != ParRi:
    raiseAssert $(n.exprKind, n.stmtKind, n.typeKind, $n)
  inc n

type
  RenderFlag* = enum
    renderNone, renderNoBody, renderNoComments, renderDocComments,
    renderNoPragmas, renderIds, renderNoProcDefs, renderSyms, renderRunnableExamples,
    renderIr, renderNonExportedFields, renderExpandUsing, renderNoPostfix

  RenderFlags* = set[RenderFlag]
  RenderTok* = object
    kind*: TokType
    length*: int16
    sym*: SymId

  Section = enum
    GenericParams
    ObjectDef

  RenderTokSeq* = seq[RenderTok]
  SrcGen* = object
    indent*: int
    lineLen*: int
    col: int
    pos*: int              # current position for iteration over the buffer
    idx*: int              # current token index for iteration over the buffer
    tokens*: RenderTokSeq
    buf*: string
    pendingNL*: int        # negative if not active; else contains the
                           # indentation value
    pendingWhitespace: int
    # comStack*: seq[PNode]  # comment stack
    flags*: RenderFlags
    inside: set[Section] # Keeps track of contexts we are in
    checkAnon: bool        # we're in a context that can contain sfAnon
    inPragma: int
    when defined(nimpretty):
      pendingNewlineCount: int
    # fid*: FileIndex
    # config*: ConfigRef
    mangler: seq[SymId]

  SubFlag = enum
    rfLongMode, rfInConstExpr
  SubFlags = set[SubFlag]
  Context = tuple[spacing: int, flags: SubFlags]

  BracketKind = enum
    bkNone, bkBracket, bkBracketAsgn, bkCurly, bkCurlyAsgn, bkPar

const
  Space = " "
  emptyContext: Context = (spacing: 0, flags: {})
  IndentWidth = 2
  longIndentWid = IndentWidth * 2
  MaxLineLen = 80
  LineCommentColumn = 30


proc initContext(): Context =
  result = (spacing: 0, flags: {})

proc initSrcGen(renderFlags: RenderFlags): SrcGen =
  result = SrcGen(tokens: @[], indent: 0,
                   lineLen: 0, pos: 0, idx: 0, buf: "",
                   flags: renderFlags, pendingNL: -1,
                   pendingWhitespace: -1, inside: {},
                   )

proc addTok(g: var SrcGen, kind: TokType, s: string; sym: SymId = SymId(0)) =
  g.tokens.add RenderTok(kind: kind, length: int16(s.len), sym: sym)
  g.buf.add(s)
  if kind != tkSpaces:
    inc g.col, s.len

proc addPendingNL(g: var SrcGen) =
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

proc putNL(g: var SrcGen, indent: int) =
  if g.pendingNL >= 0: addPendingNL(g)
  else:
    addTok(g, tkSpaces, "\n")
    g.col = 0

  g.pendingNL = indent
  g.lineLen = indent
  g.pendingWhitespace = -1

proc previousNL(g: SrcGen): bool =
  result = g.pendingNL >= 0 or (g.tokens.len > 0 and
                                g.tokens[^1].kind == tkSpaces)

proc putNL(g: var SrcGen) =
  putNL(g, g.indent)

proc optNL(g: var SrcGen, indent: int) =
  g.pendingNL = indent
  g.lineLen = indent
  g.col = g.indent
  when defined(nimpretty): g.pendingNewlineCount = 0

proc optNL(g: var SrcGen) =
  optNL(g, g.indent)

proc indentNL(g: var SrcGen) =
  inc(g.indent, IndentWidth)
  g.pendingNL = g.indent
  g.lineLen = g.indent

proc dedent(g: var SrcGen) =
  dec(g.indent, IndentWidth)
  assert(g.indent >= 0)
  if g.pendingNL > IndentWidth:
    dec(g.pendingNL, IndentWidth)
    dec(g.lineLen, IndentWidth)

proc put(g: var SrcGen, kind: TokType, s: string; sym: SymId = SymId(0)) =
  if kind != tkSpaces:
    addPendingNL(g)
    if s.len > 0 or kind in {tkHideableStart, tkHideableEnd}:
      addTok(g, kind, s, sym)
  else:
    g.pendingWhitespace = s.len
    inc g.col, s.len
  inc(g.lineLen, s.len)

proc putComment(g: var SrcGen, s: string) =
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

proc putRawStr(g: var SrcGen, kind: TokType, s: string) =
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


proc putWithSpace(g: var SrcGen; kind: TokType, s: string) =
  put(g, kind, s)
  put(g, tkSpaces, Space)



proc lsub(g: SrcGen; n: Cursor): int =
  result = 0

proc gsub(g: var SrcGen; n: var Cursor, fromStmtList = false, isTopLevel = false)
proc gsub(g: var SrcGen, n: var Cursor, c: Context, fromStmtList = false, isTopLevel = false)

proc gtype(g: var SrcGen, n: var Cursor, c: Context)

proc gstmts(g: var SrcGen, n: var Cursor, c: Context, doIndent=false) =
  inc n
  if doIndent: indentNL(g)

  while n.kind != ParRi:
    optNL(g)
    gsub(g, n)

  if doIndent: dedent(g)
  skipParRi(n)

proc gcomma(g: var SrcGen) =
  putWithSpace(g, tkComma, ",")


proc gpragmas(g: var SrcGen, n: var Cursor) =
  inc n
  put(g, tkSpaces, Space)
  put(g, tkCurlyDotLe, "{.")
  var afterFirst = false

  while n.kind != ParRi:
    if afterFirst:
      gcomma(g)
    else:
      afterFirst = true

    put(g, tkSymbol, pool.tags[n.tagId])
    inc n

    if n.kind == ParRi:
      skipParRi(n)
    else:
      putWithSpace(g, tkColon, ":")
      gsub(g, n)
      skipParRi(n)

  put(g, tkCurlyDotRi, ".}")
  skipParRi(n)

proc gblock(g: var SrcGen, n: var Cursor) =
  var c: Context = initContext()
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

proc gcond(g: var SrcGen, n: var Cursor) =
  # if n.kind == nkStmtListExpr:
  #   put(g, tkParLe, "(")
  gsub(g, n)
  # if n.kind == nkStmtListExpr:
  #   put(g, tkParRi, ")")

proc gif(g: var SrcGen, n: var Cursor) =

  inc n
  var c: Context = initContext()

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

proc gcase(g: var SrcGen, n: var Cursor) =
  var c: Context = initContext()
  inc n
  putWithSpace(g, tkCase, "case")

  gsub(g, n)

  optNL(g)

  while n.kind != ParRi:
    case n.substructureKind
    of OfU:
      inc n
      optNL(g)
      putWithSpace(g, tkOf, "of")
      assert n.substructureKind == RangesU
      inc n
      while n.kind != ParRi:
        case n.substructureKind
        of RangeU:
          inc n
          gsub(g, n)
          put(g, tkDotDot, "..")
          gsub(g, n)
          skipParRi(n)
        else:
          gsub(g, n)

      skipParRi(n)

      putWithSpace(g, tkColon, ":")
      gstmts(g, n, c, doIndent = true)
      skipParRi(n)
    of ElseU:
      inc n
      optNL(g)
      putWithSpace(g, tkElse, "else")
      putWithSpace(g, tkColon, ":")
      gstmts(g, n, c, doIndent = true)
      skipParRi(n)
    else:
      raiseAssert "unreachable"

  skipParRi(n)

# type
#   Routine* = object
#     kind*: SymKind
#     name*: Cursor
#     exported*: Cursor
#     pattern*: Cursor # for TR templates/macros
#     typevars*: Cursor # generic parameters
#     params*: Cursor
#     retType*: Cursor
#     pragmas*: Cursor
#     effects*: Cursor
#     body*: Cursor

proc takeTypeVars(g: var SrcGen, n: var Cursor) =
  if n.substructureKind == TypevarsU:
    inc n
    put(g, tkBracketLe, "[")
    var afterFirst = false

    while n.kind != ParRi:
      if afterFirst:
        gcomma(g)
      else:
        afterFirst = true

      let typevar = takeLocal(n, SkipFinalParRi)
      var name = typevar.name
      gsub(g, name)
      var typ = typevar.typ
      if typ.kind != DotToken:
        putWithSpace(g, tkColon, ":")
        gtype(g, typ, emptyContext)

    skipParRi(n)
    put(g, tkBracketRi, "]")
  else:
    skip n

proc gproc(g: var SrcGen, n: var Cursor) =
  var c: Context = initContext()

  let decl = takeRoutine(n, SkipFinalParRi)

  var name = decl.name
  gsub(g, name)

  var typevars = decl.typevars
  takeTypeVars(g, typevars)

  put(g, tkParLe, "(")

  var params = decl.params
  if params.kind != DotToken:
    inc params
    var afterFirst = false
    while params.kind != ParRi:
      if afterFirst:
        gcomma(g)
      else:
        afterFirst = true
      let param = takeLocal(params, SkipFinalParRi)
      var name = param.name
      gsub(g, name)
      putWithSpace(g, tkColon, ":")

      var typ = param.typ
      gtype(g, typ, emptyContext)

    skipParRi(params)

  put(g, tkParRi, ")")

  if renderNoBody notin g.flags:
    var retType = decl.retType
    if retType.kind != DotToken:
      putWithSpace(g, tkColon, ":")
      gtype(g, retType, c)

    var pragmas = decl.pragmas
    if renderNoPragmas notin g.flags:
      gsub(g, pragmas)

    if decl.body.kind != DotToken:
      put(g, tkSpaces, Space)
      putWithSpace(g, tkEquals, "=")

      c = initContext()
      var body = decl.body
      gstmts(g, body, c, doIndent = true)
      putNL(g)
    else:
      discard

proc gcall(g: var SrcGen, n: var Cursor) =
  inc n
  gsub(g, n)
  put(g, tkParLe, "(")

  var afterFirst = false

  while n.kind != ParRi:
    if afterFirst:
      gcomma(g)
    else:
      afterFirst = true
    gsub(g, n)

  put(g, tkParRi, ")")
  skipParRi(n)

proc gcmd(g: var SrcGen, n: var Cursor) =
  inc n
  gsub(g, n)
  put(g, tkSpaces, Space)

  var afterFirst = false

  while n.kind != ParRi:
    if afterFirst:
      gcomma(g)
    else:
      afterFirst = true
    gsub(g, n)

  skipParRi(n)

proc ginfix(g: var SrcGen, n: var Cursor) =
  inc n

  var opr = n
  skip n

  var afterFirst = false

  while n.kind != ParRi:
    if afterFirst:
      gsub(g, n)
    else:
      gsub(g, n)
      put(g, tkSpaces, Space)
      gsub(g, opr)
      put(g, tkSpaces, Space)
      afterFirst = true

  skipParRi(n)

proc gsufx(g: var SrcGen, n: var Cursor) =
  inc n
  var value = n
  skip n

  case pool.strings[n.litId]
  of "i": put(g, tkIntLit, $pool.integers[value.intId])
  of "i8": put(g, tkIntLit, $pool.integers[value.intId] & "'i8")
  of "i16": put(g, tkIntLit, $pool.integers[value.intId] & "'i16")
  of "i32": put(g, tkIntLit, $pool.integers[value.intId] & "'i32")
  of "i64": put(g, tkIntLit, $pool.integers[value.intId] & "'i64")
  of "u": put(g, tkUIntLit, $pool.uintegers[value.uintId] & "'u")
  of "u8": put(g, tkUIntLit, $pool.uintegers[value.uintId] & "'u8")
  of "u16": put(g, tkUIntLit, $pool.uintegers[value.uintId] & "'u16")
  of "u32": put(g, tkUIntLit, $pool.uintegers[value.uintId] & "'u32")
  of "u64": put(g, tkUIntLit, $pool.uintegers[value.uintId] & "'u64")
  of "f": put(g, tkFloatLit, toString(value, false))
  of "f32": put(g, tkFloatLit, toString(value, false) & "f32")
  of "f64": put(g, tkFloatLit, toString(value, false) & "f64")
  of "R", "T": put(g, tkStrLit, toString(value, false))
  of "C": put(g, tkStrLit, "cstring" & toString(value, false))
  else: discard

  skip n
  skipParRi(n)

# type
#   TypeDecl* = object
#     kind*: SymKind
#     name*: Cursor
#     exported*: Cursor
#     typevars*: Cursor
#     pragmas*: Cursor
#     body*: Cursor

proc takeNumberType(g: var SrcGen, n: var Cursor, typ: string) =
  inc n
  var name = typ
  let size = pool.integers[n.intId]
  if size != -1:
    name.add $size

  inc n

  if n.kind != ParRi:
    skip n

  skipParRi(n)

  put(g, tkSymbol, name)

proc gconcept(g: var SrcGen, n: var Cursor, c: Context) =
  putWithSpace(g, tkConcept, "concept")

  indentNL(g)
  inc n
  skip n
  skip n
  skip n # typevars
  gstmts(g, n, c)
  skipParRi(n)

  dedent(g)

proc gtype(g: var SrcGen, n: var Cursor, c: Context) =
  case n.kind
  of ParLe:
    case n.typekind
    of IT:
      takeNumberType(g, n, "int")
    of UT:
      takeNumberType(g, n, "uint")
    of FT:
      takeNumberType(g, n, "float")
    of CT:
      put(g, tkSymbol, "char")
      skip n
    of BoolT, VoidT, CstringT, PointerT,
          UntypedT, TypedT, AutoT:
      put(g, tkSymbol, $n.typeKind)
      skip n
    of OrdinalT:
      put(g, tkSymbol, "Ordinal")
      inc n
      if n.kind != ParRi:
        put(g, tkBracketLe, "[")
        gtype(g, n, c)
        put(g, tkBracketRi, "]")
      skipParRi(n)
    of TypedescT:
      put(g, tkSymbol, "typedesc")
      inc n
      if n.kind != ParRi:
        put(g, tkBracketLe, "[")
        gtype(g, n, c)
        put(g, tkBracketRi, "]")
      skipParRi(n)
    of TypekindT:
      inc n
      gtype(g, n, c)
      skipParRi(n)
    of PtrT:
      putWithSpace(g, tkPtr, "ptr")
      inc n
      if n.kind != ParRi:
        gtype(g, n, c)
      skipParRi(n)

    of SetT:
      put(g, tkSymbol, "set")
      inc n
      if n.kind != ParRi:
        put(g, tkBracketLe, "[")
        gtype(g, n, c)
        put(g, tkBracketRi, "]")
      skipParRi(n)

    of RefT:
      putWithSpace(g, tkRef, "ref")
      inc n
      if n.kind != ParRi:
        gtype(g, n, c)
      skipParRi(n)
    of MutT:
      putWithSpace(g, tkVar, "var")
      inc n
      gtype(g, n, c)
      skipParRi(n)
    of OutT:
      putWithSpace(g, tkOut, "out")
      inc n
      gtype(g, n, c)
      skipParRi(n)
    of LentT, SinkT, DistinctT:
      putWithSpace(g, tkSymbol, $n.typeKind)
      inc n
      gtype(g, n, c)
      skipParRi(n)

    of OrT:
      inc n
      var afterFirst = false
      while n.kind != ParRi:
        if afterFirst:
          put(g, tkOpr, "|")
        else:
          afterFirst = true
        gtype(g, n, c)

      skipParRi(n)

    of AtT:
      inc n
      var afterFirst = false
      gtype(g, n, c)
      put(g, tkBracketLe, "[")
      while n.kind != ParRi:
        if afterFirst:
          put(g, tkComma, ",")
        else:
          afterFirst = true
        gtype(g, n, c)

      skipParRi(n)
      put(g, tkBracketRi, "]")

    of RangetypeT:
      inc n
      if n.kind != ParRi:
        skip n
        gtype(g, n, c)
        put(g, tkDotDot, "..")
        gtype(g, n, c)
      else:
        put(g, tkSymbol, "range")
      skipParRi(n)

    of ArrayT:
      inc n

      put(g, tkSymbol, "array")
      put(g, tkBracketLe, "[")

      var base = n
      skip n
      gtype(g, n, c)
      gcomma(g)
      gtype(g, base, c)

      put(g, tkBracketRi, "]")

      skipParRi(n)

    of UarrayT:
      inc n

      put(g, tkSymbol, "UncheckedArray")
      put(g, tkBracketLe, "[")

      gtype(g, n, c)

      put(g, tkBracketRi, "]")

      skipParRi(n)

    of ObjectT:
      putWithSpace(g, tkObject, "object")
      let obj = asObjectDecl(n)
      skip n
      var fields = obj.firstField

      indentNL(g)

      while fields.kind != ParRi:
        case fields.substructureKind
        of CaseU:
          raiseAssert "todo"
        of FldU:
          let local = takeLocal(fields, SkipFinalParRi)
          var name = local.name
          var typ = local.typ

          gsub(g, name)
          putWithSpace(g, tkColon, ":")
          gtype(g, typ, emptyContext)
          optNL(g)
        else:
          raiseAssert "todo"

      dedent(g)

      skipParRi(fields)

    of EnumT:
      inc n

      if n.kind != ParRi:
        putWithSpace(g, tkEnum, "enum")
        skip n

        indentNL(g)

        while n.kind != ParRi:
          case n.substructureKind
          of EFldU:
            let local = takeLocal(n, SkipFinalParRi)
            var name = local.name
            var value = local.val

            gsub(g, name)
            put(g, tkSpaces, Space)
            put(g, tkEquals, "=")
            put(g, tkSpaces, Space)
            gsub(g, value)
            optNL(g)
          else:
            raiseAssert "unreachable"

        dedent(g)
      else:
        put(g, tkSymbol, "OrdinalEnum")

      skipParRi(n)

    of OnumT:
      put(g, tkSymbol, "HoleyEnum")
      skip n

    of ConceptT:
      gconcept(g, n, c)

    of TupleT:
      inc n

      put(g, tkTuple, "tuple")
      put(g, tkBracketLe, "[")

      var afterFirst = false

      while n.kind != ParRi:
        if afterFirst:
          gcomma(g)
        else:
          afterFirst = true

        case n.substructureKind
        of KvU:
          inc n
          gsub(g, n)
          putWithSpace(g, tkColon, ":")
          gtype(g, n, c)
          skipParRi(n)
        else:
          gtype(g, n, c)

      skipParRi(n)
      put(g, tkBracketRi, "]")

    of ErrT:
      put(g, tkStrLit, toString(n, false))
      skip n

    else:
      case n.exprKind
      of CchoiceX, OchoiceX:
        gsub(g, n, c)
      else:
        skip n
  else:
    gsub(g, n, c)

proc longMode(g: var SrcGen, n: var Cursor): bool =
  result = false
  discard "todo"

proc gWhile(g: var SrcGen, n: var Cursor) =
  var c: Context = initContext()
  putWithSpace(g, tkWhile, "while")
  inc n
  gcond(g, n)
  putWithSpace(g, tkColon, ":")

  gstmts(g, n, c, doIndent = true)

  skipParRi(n)

proc gfor(g: var SrcGen, n: var Cursor) =
  let forStmt = asForStmt(n)
  skip n
  var c: Context = initContext()
  putWithSpace(g, tkFor, "for")
  # if longMode(g, n) or
  #     (lsub(g, n[^1]) + lsub(g, n[^2]) + 6 + g.lineLen > MaxLineLen):
  #   incl(c.flags, rfLongMode)
  # gcomma(g, n, c, 0, - 3)
  var vars = forStmt.vars
  inc vars

  var afterFirst = false
  while vars.kind != ParRi:
    let local = takeLocal(vars, SkipFinalParRi)
    var name = local.name

    if afterFirst:
      gcomma(g)
    else:
      afterFirst = true

    gsub(g, name, c)

  put(g, tkSpaces, Space)
  putWithSpace(g, tkIn, "in")

  var iter = forStmt.iter
  gsub(g, iter, c)
  putWithSpace(g, tkColon, ":")

  var body = forStmt.body
  gstmts(g, body, c, doIndent = true)

proc gtypedef(g: var SrcGen, n: var Cursor, c: Context) =
  let decl = takeTypeDecl(n, SkipFinalParRi)

  putWithSpace(g, tkType, "type")
  indentNL(g)

  var name = decl.name
  gsub(g, name, c)

  var typevars = decl.typevars
  takeTypeVars(g, typevars)

  var pragmas = decl.pragmas
  gsub(g, pragmas, c)

  var body = decl.body
  if body.kind != DotToken:
    put(g, tkSpaces, Space)
    putWithSpace(g, tkEquals, "=")
    gtype(g, body, c)

  dedent(g)

proc gtry(g: var SrcGen, n: var Cursor) =
  skip n
  raiseAssert "todo"
  # var c: Context = initContext()
  # put(g, tkTry, "try")
  # putWithSpace(g, tkColon, ":")
  # # if longMode(g, n) or (lsub(g, n[0]) + g.lineLen > MaxLineLen):
  # #   incl(c.flags, rfLongMode)
  # gstmts(g, n, c, doIndent = true)
  # # gsons(g, n, c, 1)

proc gconstr(g: var SrcGen, n: var Cursor, kind: BracketKind) =
  inc n
  skip n

  case kind
  of bkBracket:
    put(g, tkBracketLe, "[")
  of bkCurly:
    put(g, tkCurlyLe, "{")
  of bkPar:
    put(g, tkParLe, "(")
  else:
    raiseAssert "todo"

  var afterFirst = false
  while n.kind != ParRi:
    if afterFirst:
      gcomma(g)
    else:
      afterFirst = true
    gsub(g, n)

  skipParRi(n)

  case kind
  of bkBracket:
    put(g, tkBracketRi, "]")
  of bkCurly:
    put(g, tkCurlyRi, "}")
  of bkPar:
    put(g, tkParRi, ")")
  else:
    raiseAssert "todo"

proc gsub(g: var SrcGen, n: var Cursor, c: Context, fromStmtList = false, isTopLevel = false) =
  case n.kind
  of ParLe:
    case n.exprKind
    of NoExpr:
      case n.stmtKind
      of StmtsS:
        gstmts(g, n, c)
      of VarS, LetS, CursorS, ConstS, GvarS, TvarS, GletS, TletS, ResultS:
        let descriptor: string
        let tk: TokType
        case n.stmtKind
        of VarS, GvarS, TvarS, ResultS:
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
        var typ = decl.typ
        gsub(g, name)

        if typ.kind != DotToken:
          putWithSpace(g, tkColon, ":")
          gtype(g, typ, c)

        if value.kind != DotToken:
          put(g, tkSpaces, Space)
          putWithSpace(g, tkEquals, "=")
          gsub(g, value, c)

      of BlockS:
        gblock(g, n)

      of TypeS:
        gtypedef(g, n, emptyContext)

      of IfS:
        putWithSpace(g, tkIf, "if")
        gif(g, n)

      of WhenS:
        putWithSpace(g, tkIf, "when")
        gif(g, n)

      of CaseS:
        gcase(g, n)

      of ForS:
        gfor(g, n)

      of WhileS:
        gWhile(g, n)

      of ProcS:
        putWithSpace(g, tkProc, "proc")
        gproc(g, n)

      of FuncS:
        putWithSpace(g, tkFunc, "func")
        gproc(g, n)

      of MacroS:
        putWithSpace(g, tkMacro, "macro")
        gproc(g, n)

      of MethodS:
        putWithSpace(g, tkMethod, "method")
        gproc(g, n)

      of ConverterS:
        putWithSpace(g, tkConverter, "converter")
        gproc(g, n)

      of IteratorS:
        putWithSpace(g, tkIterator, "iterator")
        gproc(g, n)

      of DiscardS:
        putWithSpace(g, tkDiscard, "discard")
        inc n
        gsub(g, n)
        skipParRi(n)

      of CallS:
        gcall(g, n)

      of CmdS:
        gcmd(g, n)

      of AsgnS:
        inc n
        gsub(g, n)

        put(g, tkSpaces, Space)
        putWithSpace(g, tkEquals, "=")

        gsub(g, n)

        skipParRi(n)

      of RetS:
        inc n
        putWithSpace(g, tkReturn, "return")
        gsub(g, n)
        skipParRi(n)

      of BreakS:
        inc n
        putWithSpace(g, tkBreak, "break")
        gsub(g, n)
        skipParRi(n)

      of ContinueS:
        inc n
        putWithSpace(g, tkContinue, "continue")
        gsub(g, n)
        skipParRi(n)

      of YldS:
        inc n
        putWithSpace(g, tkYield, "yield")
        gsub(g, n)
        skipParRi(n)

      of RaiseS:
        inc n
        putWithSpace(g, tkRaise, "raise")
        gsub(g, n)
        skipParRi(n)

      of TryS:
        gtry(g, n)


      of ScopeS:
        inc n
        while n.kind != ParRi:
          gsub(g, n, c)

        skipParRi(n)

      of NoStmt:
        skip n
        # raiseAssert "unreachable"

      of PragmasS:
        gpragmas(g, n)

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

    of CastX:
      inc n
      put(g, tkCast, "cast")
      put(g, tkBracketLe, "[")
      gtype(g, n, c)
      put(g, tkBracketRi, "]")
      put(g, tkParLe, "(")
      gsub(g, n)
      put(g, tkParRi, ")")
      skipParRi(n)

    of AtX, PatX, ArrAtX:
      inc n

      gsub(g, n)
      put(g, tkBracketLe, "[")

      gsub(g, n)

      put(g, tkBracketRi, "]")

      while n.kind != ParRi:
        skip n

      skipParRi(n)

    of ParX:
      inc n
      put(g, tkParLe, "(")
      gsub(g, n)
      skipParRi(n)

    of SufX:
      gsufx(g, n)

    of InfX:
      put(g, tkSymbol, "Inf")
      skip n

    of NeginfX:
      put(g, tkOpr, "-")
      put(g, tkSymbol, "Inf")
      skip n

    of NanX:
      put(g, tkSymbol, "NaN")
      skip n

    of AddrX:
      inc n
      put(g, tkAddr, "addr")
      gsub(g, n)

      skipParRi(n)

    of CallX, CallstrlitX, HighX, LowX, TypeofX,
          SizeofX, AlignofX, OffsetofX:
      gcall(g, n)

    of PrefixX:
      # TODO:
      gcall(g, n)

    of TupX:
      put(g, tkParLe, "(")

      inc n

      var afterFirst = false
      while n.kind != ParRi:
        if afterFirst:
          gcomma(g)
        else:
          afterFirst = true
        gsub(g, n)

      skipParRi(n)

      put(g, tkParRi, ")")

    of TupconstrX:
      gconstr(g, n, bkPar)

    of AconstrX:
      gconstr(g, n, bkBracket)

    of SetconstrX:
      gconstr(g, n, bkCurly)

    of OconstrX:
      inc n
      gtype(g, n, c)
      put(g, tkParLe, "(")
      var afterFirst = false
      while n.kind != ParRi:
        if afterFirst:
          gcomma(g)
        else:
          afterFirst = true
        assert n.substructureKind == KvU
        inc n
        gsub(g, n)
        putWithSpace(g, tkColon, ":")
        gsub(g, n)
        skipParRi(n)

      skipParRi(n)
      put(g, tkParRi, ")")

    of CmdX:
      gcmd(g, n)

    of InfixX:
      ginfix(g, n)

    of AndX, OrX, XorX:
      let opr: string
      case n.exprKind
      of AndX:
        opr = "and"
      of OrX:
        opr = "or"
      of XorX:
        opr = "xor"
      else:
        raiseAssert "unreachable"
      inc n
      gsub(g, n)
      put(g, tkSpaces, Space)
      put(g, tkSymbol, opr)
      put(g, tkSpaces, Space)
      gsub(g, n)
      skipParRi(n)

    of EqX, NeqX, LeX, LtX, AddX,
        SubX, MulX, DivX:
      let opr: string
      case n.exprKind
      of EqX:
        opr = "=="
      of NeqX:
        opr = "!="
      of LeX:
        opr = "<="
      of LtX:
        opr = "<"
      of AddX:
        opr = "+"
      of SubX:
        opr = "+"
      of MulX:
        opr = "*"
      of DivX:
        opr = "/"
      else:
        raiseAssert "unreachable"
      inc n
      skip n
      gsub(g, n)
      put(g, tkSpaces, Space)
      put(g, tkSymbol, opr)
      put(g, tkSpaces, Space)
      gsub(g, n)
      skipParRi(n)

    of ModX, ShrX, ShlX, BitandX, BitorX, BitxorX, BitnotX:
      let opr = $n.exprKind
      inc n
      skip n
      gsub(g, n)
      put(g, tkSpaces, Space)
      put(g, tkSymbol, opr)
      put(g, tkSpaces, Space)
      gsub(g, n)
      skipParRi(n)

    of NegX:
      putWithSpace(g, tkOpr, $n.exprKind)
      inc n
      skip n
      gsub(g, n)
      skipParRi(n)

    of NotX:
      putWithSpace(g, tkOpr, $n.exprKind)
      inc n
      gsub(g, n)
      skipParRi(n)

    of ExprX:
      inc n
      var isFirst = true
      while n.kind != ParRi:
        if isFirst:
          isFirst = false
        else:
          put(g, tkSemiColon, ";")
        gsub(g, n)

      skipParRi(n)

    of HDerefX, HaddrX:
      inc n
      gsub(g, n)
      skipParRi(n)

    of DerefX:
      inc n
      gsub(g, n)
      put(g, tkOpr, "[]")
      skipParRi(n)

    of ConvX:
      inc n
      gtype(g, n, c)
      put(g, tkParLe, "(")
      gsub(g, n)
      put(g, tkParRi, ")")
      skipParRi(n)

    of CchoiceX, OchoiceX:
      inc n
      gsub(g, n)
      while n.kind != ParRi:
        skip n

      skipParRi(n)

    of HconvX, DconvX:
      inc n
      skip n
      gsub(g, n)
      skipParRi(n)

    of DotX:
      inc n
      gsub(g, n)
      put(g, tkDot, ".")
      gsub(g, n)
      skip n
      skipParRi(n)
    else:
      skip n
  of ParRi:
    inc n
  of IntLit:
    put(g, tkIntLit, $pool.integers[n.intId])
    inc n
  of UIntLit:
    put(g, tkUIntLit, $pool.uintegers[n.uintId])
    inc n
  of FloatLit:
    put(g, tkFloatLit, toString(n, false))
    inc n
  of StringLit:
    put(g, tkStrLit, toString(n, false))
    inc n
  of CharLit:
    var lit = "\'"
    lit.addEscapedChar(char(n.uoperand))
    lit.add "\'"
    put(g, tkCharLit, lit)
    inc n
  of Symbol, SymbolDef:
    var name = pool.syms[n.symId]
    extractBasename(name)
    put(g, tkSymbol, name)
    inc n
  of Ident:
    put(g, tkSymbol, pool.strings[n.litId])
    inc n
  of DotToken:
    inc n
  else:
    inc n

proc gsub(g: var SrcGen; n: var Cursor, fromStmtList = false, isTopLevel = false) =
  var c: Context = initContext()
  gsub(g, n, c, isTopLevel = isTopLevel)

proc renderTree(n: Cursor, renderFlags: RenderFlags = {}, renderType = false): string =
  var g: SrcGen = initSrcGen(renderFlags)
  var n = n
  if renderType:
    var c: Context = initContext()
    gtype(g, n, c)
  else:
    gsub(g, n, isTopLevel = true)
  result = g.buf

proc asNimCode*(n: Cursor; renderFlags: RenderFlags = {}): string =
  var m0: PackedLineInfo = NoLineInfo
  var m1: PackedLineInfo = NoLineInfo
  var nested = 0
  var n2 = n
  var file0 = FileId 0

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
    result = renderTree(n, renderFlags = renderFlags)

proc typeToString*(n: Cursor; renderFlags: RenderFlags = {}): string =
  result = renderTree(n, renderFlags = renderFlags, renderType = true)
