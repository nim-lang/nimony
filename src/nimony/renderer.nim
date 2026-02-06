#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import ".." / lib / [bitabs, lineinfos, nifstreams, nifcursors, filelinecache, symparser]

import nimony_model, decls


import std/[strutils, assertions, formatfloat]

## Rendering of Nim code from a cursor.

proc skipParRi(n: var Cursor) =
  if n.kind != ParRi:
    raiseAssert $(n.exprKind, n.stmtKind, n.typeKind, $n)
  inc n

type
  TokType = enum
    tkInvalid = "tkInvalid", tkEof = "[EOF]", # order is important here!
    tkSymbol = "tkSymbol", # keywords:
    tkAddr = "addr", tkAnd = "and", tkAs = "as", tkAsm = "asm",
    tkBind = "bind", tkBlock = "block", tkBreak = "break", tkCase = "case", tkCast = "cast",
    tkConcept = "concept", tkConst = "const", tkContinue = "continue", tkConverter = "converter",
    tkDefer = "defer", tkDiscard = "discard", tkDistinct = "distinct", tkDiv = "div", tkDo = "do",
    tkElif = "elif", tkElse = "else", tkEnd = "end", tkEnum = "enum", tkExcept = "except", tkExport = "export",
    tkFinally = "finally", tkFor = "for", tkFrom = "from", tkFunc = "func",
    tkIf = "if", tkImport = "import", tkIn = "in", tkInclude = "include", tkInterface = "interface",
    tkIs = "is", tkIsnot = "isnot", tkIterator = "iterator",
    tkLet = "let",
    tkMacro = "macro", tkMethod = "method", tkMixin = "mixin", tkMod = "mod", tkNil = "nil", tkNot = "not", tkNotin = "notin",
    tkObject = "object", tkOf = "of", tkOr = "or", tkOut = "out",
    tkProc = "proc", tkPtr = "ptr", tkRaise = "raise", tkRef = "ref", tkReturn = "return",
    tkShl = "shl", tkShr = "shr", tkStatic = "static",
    tkTemplate = "template",
    tkTry = "try", tkTuple = "tuple", tkType = "type", tkUsing = "using",
    tkVar = "var", tkWhen = "when", tkWhile = "while", tkXor = "xor",
    tkYield = "yield", # end of keywords

    tkIntLit = "tkIntLit", tkInt8Lit = "tkInt8Lit", tkInt16Lit = "tkInt16Lit",
    tkInt32Lit = "tkInt32Lit", tkInt64Lit = "tkInt64Lit",
    tkUIntLit = "tkUIntLit", tkUInt8Lit = "tkUInt8Lit", tkUInt16Lit = "tkUInt16Lit",
    tkUInt32Lit = "tkUInt32Lit", tkUInt64Lit = "tkUInt64Lit",
    tkFloatLit = "tkFloatLit", tkFloat32Lit = "tkFloat32Lit",
    tkFloat64Lit = "tkFloat64Lit", tkFloat128Lit = "tkFloat128Lit",
    tkStrLit = "tkStrLit", tkRStrLit = "tkRStrLit", tkTripleStrLit = "tkTripleStrLit",
    tkGStrLit = "tkGStrLit", tkGTripleStrLit = "tkGTripleStrLit", tkCharLit = "tkCharLit",
    tkCustomLit = "tkCustomLit",

    tkParLe = "(", tkParRi = ")", tkBracketLe = "[",
    tkBracketRi = "]", tkCurlyLe = "{", tkCurlyRi = "}",
    tkBracketDotLe = "[.", tkBracketDotRi = ".]",
    tkCurlyDotLe = "{.", tkCurlyDotRi = ".}",
    tkParDotLe = "(.", tkParDotRi = ".)",
    tkComma = ",", tkSemiColon = ";",
    tkColon = ":", tkColonColon = "::", tkEquals = "=",
    tkDot = ".", tkDotDot = "..", tkBracketLeColon = "[:",
    tkOpr, tkComment, tkAccent = "`",
    # these are fake tokens used by renderer.nim
    tkSpaces, tkInfixOpr, tkPrefixOpr, tkPostfixOpr, tkHideableStart, tkHideableEnd

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
  # longIndentWid = IndentWidth * 2
  # MaxLineLen = 80
  # LineCommentColumn = 30


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

when false:
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

when false:
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


when false:
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

proc mayAddExportMarker(g: var SrcGen, n: var Cursor) =
  if n.kind == Ident and pool.strings[n.litId] == "x":
    put(g, tkSymbol, "*")
  skip n

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

proc gcase(g: var SrcGen, n: var Cursor; isCaseObject = false)

proc takeField(g: var SrcGen, fields: var Cursor) =
  let local = takeLocal(fields, SkipFinalParRi)
  var name = local.name
  var typ = local.typ

  gsub(g, name)
  var exportMarker = local.exported
  mayAddExportMarker(g, exportMarker)

  putWithSpace(g, tkColon, ":")
  gtype(g, typ, emptyContext)
  optNL(g)

proc takeObjectFields(g: var SrcGen, fields: var Cursor) =
  case fields.substructureKind
  of CaseU:
    gcase(g, fields, isCaseObject = true)
  of FldU:
    takeField(g, fields)
  else:
    raiseAssert "todo"

proc takeCaseStmts(g: var SrcGen, n: var Cursor; c: var Context; isCaseObject = false) =
  if isCaseObject:
    inc n
    indentNL(g)

    while n.kind != ParRi:
      optNL(g)
      takeObjectFields(g, n)

    dedent(g)
    skipParRi(n)
  else:
    gstmts(g, n, c, doIndent = true)

proc gcase(g: var SrcGen, n: var Cursor; isCaseObject = false) =
  var c: Context = initContext()
  inc n
  putWithSpace(g, tkCase, "case")

  if isCaseObject:
    takeField(g, n)
  else:
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
        gsub(g, n)

      skipParRi(n)

      putWithSpace(g, tkColon, ":")
      takeCaseStmts(g, n, c, isCaseObject = isCaseObject)
      skipParRi(n)
    of ElseU:
      inc n
      optNL(g)
      put(g, tkElse, "else")
      putWithSpace(g, tkColon, ":")
      takeCaseStmts(g, n, c, isCaseObject = isCaseObject)
      skipParRi(n)
    else:
      raiseAssert "unreachable"

  skipParRi(n)

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

  var exportMarker = decl.exported
  mayAddExportMarker(g, exportMarker)

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

proc bracketKind(g: SrcGen, n: Cursor): BracketKind =
  if renderIds notin g.flags:
    if n.exprKind in {OchoiceX, CchoiceX}:
      var firstSon = n
      inc firstSon
      result = bracketKind(g, firstSon)
    elif n.kind == Symbol:
      var name = pool.syms[n.symId]
      extractBasename(name)

      case name
      of "[]": result = bkBracket
      of "[]=": result = bkBracketAsgn
      of "{}": result = bkCurly
      of "{}=": result = bkCurlyAsgn
      else: result = bkNone
    else:
      result = bkNone
  else:
    result = bkNone

proc gcallComma(g: var SrcGen, n: var Cursor) =
  var afterFirst = false

  while n.kind != ParRi:
    if afterFirst:
      gcomma(g)
    else:
      afterFirst = true

    if n.substructureKind == VvU:
      inc n
      gsub(g, n)
      put(g, tkSpaces, Space)
      put(g, tkEquals, "=")
      put(g, tkSpaces, Space)
      gsub(g, n)
    else:
      gsub(g, n)

proc gcall(g: var SrcGen, n: var Cursor) =
  inc n
  case bracketKind(g, n)
  of bkBracket:
    skip n
    gsub(g, n)
    put(g, tkBracketLe, "[")
    gcallComma(g, n)
    put(g, tkBracketRi, "]")
    skipParRi(n)
  of bkCurly:
    skip n
    gsub(g, n)
    put(g, tkCurlyLe, "{")
    gcallComma(g, n)
    put(g, tkCurlyRi, "}")
    skipParRi(n)
  of bkNone, bkPar, bkBracketAsgn, bkCurlyAsgn:
    # TODO:
    gsub(g, n)
    put(g, tkParLe, "(")
    gcallComma(g, n)
    put(g, tkParRi, ")")
    skipParRi(n)

proc gcallsystem(g: var SrcGen, n: var Cursor; name: string) =
  inc n
  put(g, tkSymbol, name)
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
  of "f": put(g, tkFloatLit, $pool.floats[value.floatId])
  of "f32": put(g, tkFloatLit, $pool.floats[value.floatId] & "f32")
  of "f64": put(g, tkFloatLit, $pool.floats[value.floatId] & "f64")
  of "R", "T": put(g, tkStrLit, toString(value, false))
  of "C": put(g, tkStrLit, "cstring" & toString(value, false))
  else: discard

  skip n
  skipParRi(n)

proc takeNumberType(g: var SrcGen, n: var Cursor, typ: string) =
  inc n
  var name = typ
  let size = pool.integers[n.intId]
  if size != -1:
    name.add $size

  inc n

  while n.kind != ParRi:
    # skips importc and headers etc.
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
      put(g, tkPtr, "ptr")
      inc n
      if n.kind != ParRi:
        put(g, tkSpaces, Space)
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
      put(g, tkRef, "ref")
      inc n
      if n.kind != ParRi:
        put(g, tkSpaces, Space)
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
          gcase(g, fields, isCaseObject = true)
        of FldU:
          takeField(g, fields)
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

    of RoutineTypes:
      case n.typeKind
      of ProcT:
        putWithSpace(g, tkProc, "proc")
      of IteratorT:
        putWithSpace(g, tkIterator, "iterator")
      of ConverterT:
        putWithSpace(g, tkConverter, "converter")
      of MacroT:
        putWithSpace(g, tkMacro, "macro")
      of TemplateT:
        putWithSpace(g, tkTemplate, "template")
      of MethodT:
        putWithSpace(g, tkMethod, "method")
      of FuncT:
        putWithSpace(g, tkFunc, "func")
      of ProctypeT:
        putWithSpace(g, tkProc, "proc")
      else:
        raiseAssert "cannot happen"
      inc n

      for i in 1..4: skip n
      if n.substructureKind == ParamsU:
        put(g, tkParLe, "(")
        inc n
        while n.kind != ParRi:
          let decl = takeLocal(n, SkipFinalParRi)
          var name = decl.name
          var value = decl.val
          var typ = decl.typ
          gsub(g, name, c)

          if typ.kind != DotToken:
            putWithSpace(g, tkColon, ":")
            gtype(g, typ, c)

          if value.kind != DotToken:
            put(g, tkSpaces, Space)
            putWithSpace(g, tkEquals, "=")
            gsub(g, value, c)

          if n.kind != ParRi:
            putWithSpace(g, tkComma, ",")
        inc n
        put(g, tkParRi, ")")
      else:
        skip n

      # return type
      if n.kind != DotToken:
        putWithSpace(g, tkColon, ":")
        gtype(g, n, c)
      else:
        inc n
      if n.kind != DotToken:
        # pragmas
        gsub(g, n, c)
      else:
        inc n
      skip n # effects
      skip n # body
      skipParRi(n)
    else:
      case n.exprKind
      of CchoiceX, OchoiceX:
        gsub(g, n, c)
      else:
        skip n
  of DotToken:
    put(g, tkSymbol, "void")
    inc n
  else:
    gsub(g, n, c)

when false:
  proc longMode(g: var SrcGen, n: var Cursor): bool =
    result = false

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

  var exportMarker = decl.exported
  mayAddExportMarker(g, exportMarker)

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
  var c: Context = initContext()
  inc n
  put(g, tkTry, "try")
  putWithSpace(g, tkColon, ":")
  # # if longMode(g, n) or (lsub(g, n[0]) + g.lineLen > MaxLineLen):
  # #   incl(c.flags, rfLongMode)
  gstmts(g, n, c, doIndent = true)
  while n.substructureKind == ExceptU:
    optNL(g)
    inc n
    if n.kind != DotToken:
      putWithSpace(g, tkExcept, "except")
      gsub(g, n, c)
      raiseAssert "todo"
    else:
      put(g, tkExcept, "except")
      inc n
    putWithSpace(g, tkColon, ":")
    gstmts(g, n, c, doIndent = true)
    skipParRi(n)

  if n.substructureKind == FinU:
    optNL(g)
    put(g, tkFinally, "finally")
    inc n
    putWithSpace(g, tkColon, ":")
    gstmts(g, n, c, doIndent = true)
    skipParRi(n)

  skipParRi(n)


proc gconstr(g: var SrcGen, n: var Cursor, kind: BracketKind, isUntyped = false) =
  inc n

  if not isUntyped:
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

proc gpragmaBlock(g: var SrcGen, n: var Cursor) =
  inc n
  var c: Context = initContext()
  gsub(g, n)
  putWithSpace(g, tkColon, ":")
  # if longMode(g, n) or (lsub(g, n[1]) + g.lineLen > MaxLineLen):
  #   incl(c.flags, rfLongMode)
  # gcoms(g)                    # a good place for comments
  gstmts(g, n, c, doIndent = true)
  skipParRi(n)

proc isUseSpace(n: Cursor): bool =
  template isAlpha(s: Cursor): bool =
    pool.strings[s.litId][0] in {'a'..'z', 'A'..'Z'}

  result = true
  var n = n

  assert n.kind != ParRi
  let firstSon = n
  skip n

  if n.kind != ParRi:
    let secondSon = n
    skip n
    if n.kind == ParRi:
      assert firstSon.kind == Ident and secondSon.kind == Ident
      # handle `=destroy`, `'big' and handle setters, e.g. `foo=`
      if (pool.strings[firstSon.litId] in ["=", "'"] and isAlpha(secondSon)) or
          (pool.strings[secondSon.litId] == "=" and isAlpha(firstSon)):
        result = false

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

        var exportMarker = decl.exported
        mayAddExportMarker(g, exportMarker)

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

      of TemplateS:
        putWithSpace(g, tkMacro, "template")
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
        case n.substructureKind
        of RangeU:
          inc n
          gsub(g, n)
          put(g, tkDotDot, "..")
          gsub(g, n)
          skipParRi(n)
        else:
          if n.typeKind != NoType:
            gtype(g, n, c)
          else:
            skip n
        # raiseAssert "unreachable"

      of PragmasS:
        gpragmas(g, n)

      of CommentS:
        raiseAssert "todo"

      else:
        # raiseAssert $pool.tags[n.tagId]
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

    of AtX, PatX, TupatX, ArrAtX:
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
      put(g, tkParRi, ")")
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

    of DelayX:
      inc n
      skip n # don't render the type `Continuation` here
      put(g, tkSymbol, "delay")
      put(g, tkParLe, "(")
      gsub(g, n)
      put(g, tkParRi, ")")
      skipParRi(n)

    of EmoveX:
      inc n
      put(g, tkSymbol, "ensureMove")

      put(g, tkParLe, "(")
      gsub(g, n)
      put(g, tkParRi, ")")

      skipParRi(n)

    of CallX, CallstrlitX:
      gcall(g, n)

    of HighX, LowX, TypeofX,
       SizeofX, AlignofX, OffsetofX,
       CardX, UnpackX, FieldsX, CompilesX,
       DeclaredX, DefinedX, AstToStrX:
      gcallsystem(g, n, $n.exprKind)

    of ProccallX:
      gcallsystem(g, n, "procCall")

    of NewrefX:
      inc n

      put(g, tkSymbol, "newConstr")

      skip n

      put(g, tkParLe, "(")
      gsub(g, n)
      put(g, tkParRi, ")")

      skipParRi(n)

    of FieldpairsX:
      gcallsystem(g, n, "fieldPairs")

    of WasmovedX:
      gcallsystem(g, n, "=wasMoved")

    of DestroyX:
      gcallsystem(g, n, "=destroy")

    of DupX:
      gcallsystem(g, n, "=dup")

    of CopyX:
      gcallsystem(g, n, "=copy")

    of SinkhX:
       gcallsystem(g, n, "=sink")

    of TraceX:
       gcallsystem(g, n, "=sink")

    of DefaultobjX, DefaulttupX, DefaultdistinctX:
      gcallsystem(g, n, "default")

    of InsetX:
      inc n
      put(g, tkSymbol, "contains")
      put(g, tkParLe, "(")

      skip n

      var afterFirst = false

      while n.kind != ParRi:
        if afterFirst:
          gcomma(g)
        else:
          afterFirst = true
        gsub(g, n)

      put(g, tkParRi, ")")
      skipParRi(n)

    of PrefixX:
      # TODO:
      gcall(g, n)

    of TupX:
      gconstr(g, n, bkPar, isUntyped = true)

    of BracketX:
      gconstr(g, n, bkBracket, isUntyped = true)

    of CurlyX:
      gconstr(g, n, bkCurly, isUntyped = true)

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

        if n.kind != ParRi:
          skip n

        skipParRi(n)

      skipParRi(n)
      put(g, tkParRi, ")")

    of NewobjX:
      inc n
      # TODO: find the ref types
      put(g, tkParLe, "(")
      gtype(g, n, c)
      put(g, tkParRi, ")")

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
        if n.kind != ParRi:
          skip n
        skipParRi(n)

      skipParRi(n)
      put(g, tkParRi, ")")

    of CmdX:
      gcmd(g, n)

    of EnumtostrX:
      inc n
      put(g, tkSymbol, "$")

      gsub(g, n)

      skipParRi(n)

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

    of IsX, InstanceofX:
      let opr: string
      case n.exprKind
      of IsX:
        opr = "is"
      of InstanceofX:
        opr = "of"
      else:
        raiseAssert "unreachable"
      inc n
      gsub(g, n)
      put(g, tkSpaces, Space)
      put(g, tkSymbol, opr)
      put(g, tkSpaces, Space)
      gtype(g, n, c)
      skipParRi(n)

    of EqX, NeqX, LeX, LtX, AddX,
        SubX, MulX, DivX,
        PlussetX, MinussetX, MulsetX, XorsetX,
        EqsetX, LesetX, LtsetX:
      let opr: string
      case n.exprKind
      of EqX, EqsetX:
        opr = "=="
      of NeqX:
        opr = "!="
      of LeX, LesetX:
        opr = "<="
      of LtX, LtsetX:
        opr = "<"
      of AddX, PlussetX:
        opr = "+"
      of SubX, MinussetX:
        opr = "-"
      of MulX, MulsetX:
        opr = "*"
      of DivX:
        opr = "/"
      of XorsetX:
        opr = "-+-"
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

    of AshrX:
      inc n
      skip n
      put(g, tkSymbol, "ashr")
      put(g, tkParLe, "(")

      gsub(g, n)
      gcomma(g)
      gsub(g, n)

      put(g, tkParRi, ")")
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

      var ncopy = n

      var hasChildren = false
      if n.kind != ParRi:
        skip ncopy
        hasChildren = ncopy.kind != ParRi

      if hasChildren:
        put(g, tkParLe, "(")
      while n.kind != ParRi:
        if isFirst:
          isFirst = false
        else:
          putWithSpace(g, tkSemiColon, ";")
        gsub(g, n)

      if hasChildren:
        put(g, tkParRi, ")")

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

    of BaseobjX:
      inc n
      put(g, tkParLe, "(")
      gtype(g, n, c)
      put(g, tkParRi, ")")

      skip n # levels

      put(g, tkParLe, "(")
      gsub(g, n)
      put(g, tkParRi, ")")
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

    of HconvX, DconvX, HcallX:
      inc n
      skip n
      gsub(g, n)
      skipParRi(n)

    of DotX, DdotX:
      inc n
      gsub(g, n)
      put(g, tkDot, ".")
      gsub(g, n)

      if n.kind != ParRi:
        skip n
      skipParRi(n)

    of PragmaxX:
      gpragmaBlock(g, n)

    of OvfX:
      put(g, tkSymbol, "overflowFlag")
      put(g, tkParLe, "(")
      put(g, tkParRi, ")")
      skip n

    of ErrX:
      put(g, tkStrLit, toString(n, false))
      skip n

    of QuotedX:
      inc n

      let useSpace = isUseSpace(n)
      put(g, tkAccent, "`")

      var afterFirst = false
      while n.kind != ParRi:
        if afterFirst:
          if useSpace:
            put(g, tkSpaces, Space)
        else:
          afterFirst = true
        gsub(g, n, c)

      put(g, tkAccent, "`")
      skipParRi(n)

    of TabconstrX:
      inc n
      put(g, tkCurlyLe, "{")

      if n.kind != ParRi:
        var afterFirst = false
        while n.kind != ParRi:
          if afterFirst:
            gcomma(g)
          else:
            afterFirst = true

          if n.substructureKind == KvU:
            inc n
            gsub(g, n, c)
            putWithSpace(g, tkColon, ":")
            gsub(g, n, c)
            skipParRi(n)
          else:
            gsub(g, n, c)
      else:
        put(g, tkColon, ":")

      put(g, tkCurlyRi, "}")
      skipParRi(n)

    of EnvpX:
      inc n
      put(g, tkSymbol, "envp")
      put(g, tkParLe, "(")
      gsub(g, n)
      put g, tkComma, ","
      gsub(g, n)
      put(g, tkParRi, ")")
      skipParRi(n)

    of CurlyatX, IsmainmoduleX,
        DoX, InternalTypeNameX, InternalFieldPairsX, FailedX:
      raiseAssert "todo"

  of IntLit:
    put(g, tkIntLit, $pool.integers[n.intId])
    inc n
  of UIntLit:
    put(g, tkUIntLit, $pool.uintegers[n.uintId])
    inc n
  of FloatLit:
    put(g, tkFloatLit, $pool.floats[n.floatId])
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
  of ParRi:
    discard "for illformed tokens"
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
    result = renderTree(n, renderFlags = renderFlags)

proc typeToString*(n: Cursor; renderFlags: RenderFlags = {}): string =
  result = renderTree(n, renderFlags = renderFlags, renderType = true)
