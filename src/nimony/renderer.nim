#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import ".." / lib / [bitabs, lineinfos, nifpools, filelinecache, symparser]
import ".." / njvl / njvl_model

import nimony_model, decls


import std/[strutils, assertions, formatfloat]

## Rendering of Nim code from a cursor.

type
  TokType* = enum
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

    tkParLe = "(", tkParRi = ")", tkBracketLe = "[", tkBracketRi = "]", tkCurlyLe = "{", tkCurlyRi = "}",
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
    isDef*: bool

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

proc isErrNode*(n: Cursor): bool =
  n.kind == ParLe and (n.typeKind == ErrT or n.exprKind == ErrX)

proc errPayload*(n: Cursor): Cursor =
  ## First child of `(err …)`: the wrapped expression, or `.` if none was recorded.
  result = n
  assert isErrNode(result)
  inc result

proc errMsgFromCursor*(n: Cursor): string =
  ## Extract the human-readable message from an `(err …)` node. Mirrors the
  ## walk in `reportErrors` so diagnostics and the reporter stay in sync.
  var c = n
  assert isErrNode(c)
  c = sub(c)
  if c.kind == DotToken:
    inc c
  else:
    skip c
  while c.kind == DotToken:
    inc c
  assert c.kind == StringLit
  result = pool.strings[c.litId]

proc typeToString*(n: Cursor; renderFlags: RenderFlags = {}): string

proc addTok(g: var SrcGen, kind: TokType, s: string; sym: SymId = SymId(0);
            isDef = false) =
  g.tokens.add RenderTok(kind: kind, length: int16(s.len), sym: sym, isDef: isDef)
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

proc put(g: var SrcGen, kind: TokType, s: string; sym: SymId = SymId(0);
         isDef = false) =
  if kind != tkSpaces:
    addPendingNL(g)
    if s.len > 0 or kind in {tkHideableStart, tkHideableEnd}:
      addTok(g, kind, s, sym, isDef)
  else:
    g.pendingWhitespace = s.len
    inc g.col, s.len
  inc(g.lineLen, s.len)

when false:
  proc putComment(g: var SrcGen, s: string) =
    if s.len == 0: return
    const SpecialWhitespace = {' ', '\t', '\r', '\n', '\0'}
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
        while j <= hi and s[j] notin SpecialWhitespace: inc(j)
        if not isCode and (g.col + (j - i) > MaxLineLen):
          put(g, tkComment, com)
          optNL(g, ind)
          com = "## "
        while i <= hi and s[i] notin SpecialWhitespace:
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
  n.into:
    if doIndent: indentNL(g)

    while n.hasMore:
      optNL(g)
      gsub(g, n)

    if doIndent: dedent(g)

proc gcomma(g: var SrcGen) =
  putWithSpace(g, tkComma, ",")

proc mayAddExportMarker(g: var SrcGen, n: var Cursor) =
  if n.isIdent and pool.strings[n.strId] == "x" and
      renderNoPostfix notin g.flags:
    put(g, tkPostfixOpr, "*")
  skip n

proc gpragmas(g: var SrcGen, n: var Cursor) =
  n.into:
    put(g, tkSpaces, Space)
    put(g, tkCurlyDotLe, "{.")
    var afterFirst = false

    while n.hasMore:
      if afterFirst:
        gcomma(g)
      else:
        afterFirst = true

      put(g, tkSymbol, globalTags.tags[n.cursorTagId])
      n.into:
        if n.hasMore:
          putWithSpace(g, tkColon, ":")
          gsub(g, n)

    put(g, tkCurlyDotRi, ".}")

proc gblock(g: var SrcGen, n: var Cursor) =
  var c: Context = initContext()
  n.into:
    if not n.isDotToken:
      putWithSpace(g, tkBlock, "block")
      gsub(g, n)
    else:
      put(g, tkBlock, "block")
      inc n

    putWithSpace(g, tkColon, ":")

    gstmts(g, n, c, doIndent = true)

proc gcond(g: var SrcGen, n: var Cursor) =
  # if n.kind == nkStmtListExpr:
  #   put(g, tkParLe, "(")
  gsub(g, n)
  # if n.kind == nkStmtListExpr:
  #   put(g, tkParRi, ")")

proc gif(g: var SrcGen, n: var Cursor) =
  var c: Context = initContext()

  var isFirst = true

  n.into:
    while n.hasMore:
      case n.substructureKind
      of ElifU:
        n.into:
          if isFirst:
            isFirst = false
          else:
            optNL(g)
            putWithSpace(g, tkElif, "elif")
          gcond(g, n)
          putWithSpace(g, tkColon, ":")
          gstmts(g, n, c, doIndent = true)
      of ElseU:
        optNL(g)
        put(g, tkElse, "else")
        putWithSpace(g, tkColon, ":")
        n.into:
          gstmts(g, n, c, doIndent = true)
      else:
        raiseAssert "unreachable"

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
  of FldU, GfldU:
    takeField(g, fields)
  else:
    raiseAssert "todo"

proc takeCaseStmts(g: var SrcGen, n: var Cursor; c: var Context; isCaseObject = false) =
  if isCaseObject:
    n.into:
      indentNL(g)

      while n.hasMore:
        optNL(g)
        takeObjectFields(g, n)

      dedent(g)
  else:
    gstmts(g, n, c, doIndent = true)

proc gcase(g: var SrcGen, n: var Cursor; isCaseObject = false) =
  var c: Context = initContext()
  n.into:
    putWithSpace(g, tkCase, "case")

    if isCaseObject:
      takeField(g, n)
    else:
      gsub(g, n)

    optNL(g)

    while n.hasMore:
      case n.substructureKind
      of OfU:
        n.into:                                   # (of ...)
          optNL(g)
          putWithSpace(g, tkOf, "of")
          assert n.substructureKind == RangesU
          n.into:                                 # (ranges ...)
            while n.hasMore:
              gsub(g, n)

          putWithSpace(g, tkColon, ":")
          takeCaseStmts(g, n, c, isCaseObject = isCaseObject)
      of ElseU:
        n.into:                                   # (else ...)
          optNL(g)
          put(g, tkElse, "else")
          putWithSpace(g, tkColon, ":")
          takeCaseStmts(g, n, c, isCaseObject = isCaseObject)
      else:
        raiseAssert "unreachable"

proc takeTypeVars(g: var SrcGen, n: var Cursor) =
  if n.substructureKind == TypevarsU:
    n.into:
      put(g, tkBracketLe, "[")
      var afterFirst = false

      while n.hasMore:
        if afterFirst:
          gcomma(g)
        else:
          afterFirst = true

        let isStatic = n.symKind == StaticTypevarY
        let typevar = takeLocal(n, SkipFinalParRi)
        var name = typevar.name
        gsub(g, name)
        var typ = typevar.typ
        if not typ.isDotToken:
          putWithSpace(g, tkColon, ":")
          if isStatic:
            put(g, tkSymbol, "static")
            put(g, tkBracketLe, "[")
            gtype(g, typ, emptyContext)
            put(g, tkBracketRi, "]")
          else:
            gtype(g, typ, emptyContext)

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
  if not params.isDotToken:
    params.into:
      var afterFirst = false
      while params.hasMore:
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

  put(g, tkParRi, ")")

  if renderNoBody notin g.flags:
    var retType = decl.retType
    if not retType.isDotToken:
      putWithSpace(g, tkColon, ":")
      gtype(g, retType, c)

    var pragmas = decl.pragmas
    if renderNoPragmas notin g.flags:
      gsub(g, pragmas)

    if not decl.body.isDotToken:
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
      var childCursor = n
      inc childCursor
      result = bracketKind(g, childCursor)
    elif n.isSymbol:
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

  while n.hasMore:
    if afterFirst:
      gcomma(g)
    else:
      afterFirst = true

    if n.substructureKind == VvU:
      n.into:
        gsub(g, n)
        put(g, tkSpaces, Space)
        put(g, tkEquals, "=")
        put(g, tkSpaces, Space)
        gsub(g, n)
    else:
      gsub(g, n)

proc gcall(g: var SrcGen, n: var Cursor) =
  n.into:
    case bracketKind(g, n)
    of bkBracket:
      skip n
      gsub(g, n)
      put(g, tkBracketLe, "[")
      gcallComma(g, n)
      put(g, tkBracketRi, "]")
    of bkCurly:
      skip n
      gsub(g, n)
      put(g, tkCurlyLe, "{")
      gcallComma(g, n)
      put(g, tkCurlyRi, "}")
    of bkNone, bkPar, bkBracketAsgn, bkCurlyAsgn:
      # TODO:
      gsub(g, n)
      put(g, tkParLe, "(")
      gcallComma(g, n)
      put(g, tkParRi, ")")

proc gcallsystem(g: var SrcGen, n: var Cursor; name: string) =
  n.into:
    put(g, tkSymbol, name)
    put(g, tkParLe, "(")

    var afterFirst = false

    while n.hasMore:
      if afterFirst:
        gcomma(g)
      else:
        afterFirst = true
      gsub(g, n)

    put(g, tkParRi, ")")

proc gcmd(g: var SrcGen, n: var Cursor) =
  n.into:
    gsub(g, n)
    put(g, tkSpaces, Space)

    var afterFirst = false

    while n.hasMore:
      if afterFirst:
        gcomma(g)
      else:
        afterFirst = true
      gsub(g, n)

proc ginfix(g: var SrcGen, n: var Cursor) =
  n.into:
    var opr = n
    skip n

    var afterFirst = false

    while n.hasMore:
      if afterFirst:
        gsub(g, n)
      else:
        gsub(g, n)
        put(g, tkSpaces, Space)
        gsub(g, opr)
        put(g, tkSpaces, Space)
        afterFirst = true

proc gsufx(g: var SrcGen, n: var Cursor) =
  n.into:
    var value = n
    skip n

    case pool.strings[n.strId]
    of "i": put(g, tkIntLit, $value.intVal)
    of "i8": put(g, tkIntLit, $value.intVal & "'i8")
    of "i16": put(g, tkIntLit, $value.intVal & "'i16")
    of "i32": put(g, tkIntLit, $value.intVal & "'i32")
    of "i64": put(g, tkIntLit, $value.intVal & "'i64")
    of "u": put(g, tkUIntLit, $value.uintVal & "'u")
    of "u8": put(g, tkUIntLit, $value.uintVal & "'u8")
    of "u16": put(g, tkUIntLit, $value.uintVal & "'u16")
    of "u32": put(g, tkUIntLit, $value.uintVal & "'u32")
    of "u64": put(g, tkUIntLit, $value.uintVal & "'u64")
    of "f": put(g, tkFloatLit, $value.floatVal)
    of "f32": put(g, tkFloatLit, $value.floatVal & "f32")
    of "f64": put(g, tkFloatLit, $value.floatVal & "f64")
    of "R", "T": put(g, tkStrLit, toString(value, false))
    of "C": put(g, tkStrLit, "cstring" & toString(value, false))
    else: discard

    skip n

proc takeNumberType(g: var SrcGen, n: var Cursor, typ: string) =
  var name = typ
  n.into:
    let size = n.intVal
    if size != -1:
      name.add $size

    inc n

    while n.hasMore:
      # skips importc and headers etc.
      skip n

  put(g, tkSymbol, name)

proc gconceptParents(g: var SrcGen, n: var Cursor) =
  if n.typeKind == AndT or n.exprKind == ParX:
    var first = true
    n.into:
      while n.hasMore:
        if not first:
          gcomma(g)
        else:
          first = false
        gtype(g, n, initContext())
    skip n
  elif n.isDotToken:
    skip n
  else:
    gtype(g, n, initContext())

proc gconcept(g: var SrcGen, n: var Cursor, c: Context) =
  putWithSpace(g, tkConcept, "concept")
  inc n
  skip n
  skip n
  if not n.isDotToken:
    putWithSpace(g, tkOf, "of")
    gconceptParents(g, n)
  else:
    skip n
  skip n, SkipGenParams
  if n.stmtKind == StmtsS:
    # `gstmts` enters the `(stmts)`, indents, renders any requirements and
    # consumes the closing `)`. An empty body (e.g. `concept of Base`) renders
    # nothing — the pending indent is undone before any token is emitted.
    gstmts(g, n, c, doIndent = true)
  else:
    skip n

proc gtype(g: var SrcGen, n: var Cursor, c: Context) =
  if not n.hasMore: return
  case n.kind
  of TagLit:
    case n.typeKind
    of IT:
      takeNumberType(g, n, "int")
    of UT:
      takeNumberType(g, n, "uint")
    of FT:
      takeNumberType(g, n, "float")
    of CT:
      put(g, tkSymbol, "char")
      skip n
    of BoolT, VoidT, UntypedT, TypedT, AutoT:
      put(g, tkSymbol, $n.typeKind)
      skip n
    of CstringT, PointerT:
      put(g, tkSymbol, $n.typeKind)
      # Render the leading nilness annotation; `peekInto` then skips any
      # trailing importc/header attrs (inlined when a `{.importc.}` pointer
      # alias is expanded) and the closing `)` — no manual mop-up needed.
      n.peekInto:
        if n.hasMore and n.substructureKind == NotnilU:
          put(g, tkSpaces, Space)
          put(g, tkSymbol, "not")
          put(g, tkSpaces, Space)
          put(g, tkNil, "nil")
          skip n
        elif n.hasMore and n.substructureKind == NilU:
          # rendered as prefix: nil cstring
          skip n
        elif n.hasMore:
          skip n # unchecked or other annotation
    of OrdinalT:
      put(g, tkSymbol, "Ordinal")
      n.into:
        if n.hasMore:
          put(g, tkBracketLe, "[")
          gtype(g, n, c)
          put(g, tkBracketRi, "]")
    of TypedescT:
      put(g, tkSymbol, "typedesc")
      n.into:
        if n.hasMore:
          put(g, tkBracketLe, "[")
          gtype(g, n, c)
          put(g, tkBracketRi, "]")
    of TypekindT:
      n.into:
        gtype(g, n, c)
    of PtrT:
      put(g, tkPtr, "ptr")
      n.into:
        if n.hasMore and n.substructureKind notin {NotnilU, NilU, UncheckedU}:
          put(g, tkSpaces, Space)
          gtype(g, n, c)
        if n.hasMore and n.substructureKind == NotnilU:
          put(g, tkSpaces, Space)
          put(g, tkSymbol, "not")
          put(g, tkSpaces, Space)
          put(g, tkNil, "nil")
          skip n
        elif n.hasMore:
          skip n # nil, unchecked annotation

    of SetT:
      put(g, tkSymbol, "set")
      n.into:
        if n.hasMore:
          put(g, tkBracketLe, "[")
          gtype(g, n, c)
          put(g, tkBracketRi, "]")

    of VarargsT:
      put(g, tkSymbol, "varargs")
      n.into:
        if n.hasMore:
          put(g, tkBracketLe, "[")
          gtype(g, n, c)
          if n.hasMore and not n.isStringLit:
            # optional converter (the trailing StringLit, if any, is the
            # openArray mangle hint planted by `semcompat` — skip silently)
            put(g, tkComma, ",")
            put(g, tkSpaces, Space)
            gsub(g, n, c)
          if n.isStringLit:
            inc n
          put(g, tkBracketRi, "]")

    of RefT:
      put(g, tkRef, "ref")
      n.into:
        if n.hasMore and n.substructureKind notin {NotnilU, NilU, UncheckedU}:
          put(g, tkSpaces, Space)
          gtype(g, n, c)
        if n.hasMore and n.substructureKind == NotnilU:
          put(g, tkSpaces, Space)
          put(g, tkSymbol, "not")
          put(g, tkSpaces, Space)
          put(g, tkNil, "nil")
          skip n
        elif n.hasMore:
          skip n # nil, unchecked annotation
    of MutT:
      putWithSpace(g, tkVar, "var")
      n.into:
        gtype(g, n, c)
    of OutT:
      putWithSpace(g, tkOut, "out")
      n.into:
        gtype(g, n, c)
    of LentT, SinkT, DistinctT:
      putWithSpace(g, tkSymbol, $n.typeKind)
      n.into:
        gtype(g, n, c)

    of OrT:
      n.into:
        var afterFirst = false
        while n.hasMore:
          if afterFirst:
            put(g, tkOpr, "|")
          else:
            afterFirst = true
          gtype(g, n, c)

    of AtT:
      n.into:
        var afterFirst = false
        gtype(g, n, c)
        put(g, tkBracketLe, "[")
        while n.hasMore:
          if afterFirst:
            put(g, tkComma, ",")
          else:
            afterFirst = true
          gtype(g, n, c)

      put(g, tkBracketRi, "]")

    of RangetypeT:
      n.into:
        if n.hasMore:
          skip n
          gtype(g, n, c)
          put(g, tkDotDot, "..")
          gtype(g, n, c)
        else:
          put(g, tkSymbol, "range")

    of ArrayT:
      n.into:
        put(g, tkSymbol, "array")
        put(g, tkBracketLe, "[")

        if n.hasMore:
          var base = n
          skip n
          if n.hasMore:
            gtype(g, n, c)
            gcomma(g)
          gtype(g, base, c)
          while n.hasMore: skip n

        put(g, tkBracketRi, "]")

    of UarrayT:
      n.into:
        put(g, tkSymbol, "UncheckedArray")
        put(g, tkBracketLe, "[")

        gtype(g, n, c)

        put(g, tkBracketRi, "]")

    of ObjectT:
      putWithSpace(g, tkObject, "object")
      let obj = asObjectDecl(n)
      skip n
      var fields = obj.body

      indentNL(g)

      fields.into:
        skip fields, AnyType  # parent type / inheritance slot
        while fields.hasMore:
          case fields.substructureKind
          of CaseU:
            gcase(g, fields, isCaseObject = true)
          of FldU, GfldU:
            takeField(g, fields)
          else:
            raiseAssert "todo"

      dedent(g)

    of EnumT:
      n.into:
        if n.hasMore:
          putWithSpace(g, tkEnum, "enum")
          skip n

          indentNL(g)

          while n.hasMore:
            case n.substructureKind
            of EfldU:
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

    of OnumT:
      put(g, tkSymbol, "HoleyEnum")
      skip n

    of AnumT:
      put(g, tkSymbol, "anum")
      skip n

    of ConceptT:
      gconcept(g, n, c)

    of TupleT:
      n.into:
        put(g, tkTuple, "tuple")
        put(g, tkBracketLe, "[")

        var afterFirst = false

        while n.hasMore:
          if afterFirst:
            gcomma(g)
          else:
            afterFirst = true

          case n.substructureKind
          of KvU:
            n.into:
              gsub(g, n)
              putWithSpace(g, tkColon, ":")
              gtype(g, n, c)
          else:
            gtype(g, n, c)

      put(g, tkBracketRi, "]")

    of ErrT:
      put(g, tkStrLit, typeToString(n, g.flags))
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
      of ItertypeT:
        putWithSpace(g, tkIterator, "iterator")
      else:
        raiseAssert "cannot happen"
      let isProctype = n.typeKind in {ProctypeT, ItertypeT}
      n.into:
        # mirrors `skipToParams` inside the bounded scope:
        if isProctype:
          skip n # nilability tag
        else:
          skip n # name
          skip n # export marker
          skip n # pattern
          skip n # generics
        if n.substructureKind == ParamsU:
          put(g, tkParLe, "(")
          n.into:
            while n.hasMore:
              let decl = takeLocal(n, SkipFinalParRi)
              var name = decl.name
              var value = decl.val
              var typ = decl.typ
              gsub(g, name, c)

              if not typ.isDotToken:
                putWithSpace(g, tkColon, ":")
                gtype(g, typ, c)

              if not value.isDotToken:
                put(g, tkSpaces, Space)
                putWithSpace(g, tkEquals, "=")
                gsub(g, value, c)

              if n.hasMore:
                putWithSpace(g, tkComma, ",")
          put(g, tkParRi, ")")
        else:
          skip n

        # return type
        if not n.isDotToken:
          putWithSpace(g, tkColon, ":")
          gtype(g, n, c)
        else:
          inc n
        if not n.isDotToken:
          # pragmas
          gsub(g, n, c)
        else:
          inc n
        if not isProctype:
          skip n, SkipEffects # effects
          skip n, SkipBody # body
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
  n.into:
    gcond(g, n)
    putWithSpace(g, tkColon, ":")

    gstmts(g, n, c, doIndent = true)

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
  vars.into:
    var afterFirst = false
    while vars.hasMore:
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
  if not body.isDotToken:
    put(g, tkSpaces, Space)
    putWithSpace(g, tkEquals, "=")
    gtype(g, body, c)

  dedent(g)

proc gtry(g: var SrcGen, n: var Cursor) =
  var c: Context = initContext()
  n.into:
    put(g, tkTry, "try")
    putWithSpace(g, tkColon, ":")
    # # if longMode(g, n) or (lsub(g, n[0]) + g.lineLen > MaxLineLen):
    # #   incl(c.flags, rfLongMode)
    gstmts(g, n, c, doIndent = true)
    while n.substructureKind == ExceptU:
      optNL(g)
      n.into:
        if not n.isDotToken:
          putWithSpace(g, tkExcept, "except")
          gsub(g, n, c)
          raiseAssert "todo"
        else:
          put(g, tkExcept, "except")
          inc n
        putWithSpace(g, tkColon, ":")
        gstmts(g, n, c, doIndent = true)

    if n.substructureKind == FinU:
      optNL(g)
      put(g, tkFinally, "finally")
      n.into:
        putWithSpace(g, tkColon, ":")
        gstmts(g, n, c, doIndent = true)


proc gconstr(g: var SrcGen, n: var Cursor, kind: BracketKind, isUntyped = false) =
  n.into:
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
    while n.hasMore:
      if afterFirst:
        gcomma(g)
      else:
        afterFirst = true
      gsub(g, n)

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
  var c: Context = initContext()
  n.into:
    gsub(g, n)
    putWithSpace(g, tkColon, ":")
    # if longMode(g, n) or (lsub(g, n[1]) + g.lineLen > MaxLineLen):
    #   incl(c.flags, rfLongMode)
    # gcoms(g)                    # a good place for comments
    gstmts(g, n, c, doIndent = true)

proc isUseSpace(n: Cursor): bool =
  template isAlpha(s: Cursor): bool =
    pool.strings[s.strId][0] in {'a'..'z', 'A'..'Z'}

  result = true
  var n = n

  assert n.hasMore
  let childCursor = n
  skip n

  if n.hasMore:
    let secondSon = n
    skip n
    if not n.hasMore:
      assert childCursor.isIdent and secondSon.isIdent
      # handle `=destroy`, `'big' and handle setters, e.g. `foo=`
      if (pool.strings[childCursor.strId] in ["=", "'"] and isAlpha(secondSon)) or
          (pool.strings[secondSon.strId] == "=" and isAlpha(childCursor)):
        result = false

proc gsub(g: var SrcGen, n: var Cursor, c: Context, fromStmtList = false, isTopLevel = false) =
  if not n.hasMore: return
  case n.kind
  of TagLit:
    case n.exprKind
    of NoExpr:
      case n.stmtKind
      of StmtsS:
        gstmts(g, n, c)
      of VarS, LetS, CursorS, PatternvarS, ConstS, GvarS, TvarS, GletS, TletS, ResultS:
        let descriptor: string
        let tk: TokType
        case n.stmtKind
        of VarS, GvarS, TvarS, ResultS, PatternvarS:
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

        if not typ.isDotToken:
          putWithSpace(g, tkColon, ":")
          gtype(g, typ, c)

        if not value.isDotToken:
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
        n.into:
          gsub(g, n)

      of CallS:
        gcall(g, n)

      of CmdS:
        gcmd(g, n)

      of AsgnS:
        n.into:
          gsub(g, n)

          put(g, tkSpaces, Space)
          putWithSpace(g, tkEquals, "=")

          gsub(g, n)

      of RetS:
        putWithSpace(g, tkReturn, "return")
        n.into:
          gsub(g, n)

      of BreakS:
        putWithSpace(g, tkBreak, "break")
        n.into:
          gsub(g, n)

      of ContinueS:
        putWithSpace(g, tkContinue, "continue")
        n.into:
          gsub(g, n)

      of YldS:
        putWithSpace(g, tkYield, "yield")
        n.into:
          gsub(g, n)

      of RaiseS:
        putWithSpace(g, tkRaise, "raise")
        n.into:
          gsub(g, n)

      of TryS:
        gtry(g, n)

      of ScopeS:
        n.into:
          while n.hasMore:
            gsub(g, n, c)

      of NoStmt:
        case n.substructureKind
        of RangeU:
          n.into:
            gsub(g, n)
            put(g, tkDotDot, "..")
            gsub(g, n)
        else:
          if n.typeKind != NoType:
            gtype(g, n, c)
          elif n.njvlKind == VV:
            n.into:
              gsub g, n, c, fromStmtList, isTopLevel
              skip n # version
          else:
            skip n
        # raiseAssert "unreachable"

      of PragmasS:
        gpragmas(g, n)

      of CommentS:
        # Comments are kept in the IR for tooling (typenav, doc-gen) but
        # have no Nim source rendering — drop the subtree.
        skip n

      else:
        # raiseAssert $globalTags.tags[n.tagId]
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

    of KvX:
      n.into:
        gsub(g, n)
        putWithSpace(g, tkColon, ":")
        gsub(g, n)
        if n.hasMore:
          skip n

    of CastX:
      n.into:
        put(g, tkCast, "cast")
        put(g, tkBracketLe, "[")
        gtype(g, n, c)
        put(g, tkBracketRi, "]")
        put(g, tkParLe, "(")
        gsub(g, n)
        put(g, tkParRi, ")")

    of AtX, PatX, TupatX, ArratX:
      n.into:
        gsub(g, n)
        put(g, tkBracketLe, "[")

        gsub(g, n)

        put(g, tkBracketRi, "]")

        while n.hasMore:
          skip n

    of ParX:
      n.into:
        put(g, tkParLe, "(")
        gsub(g, n)
        put(g, tkParRi, ")")

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
      n.into:
        put(g, tkAddr, "addr")
        gsub(g, n)

    of DelayX:
      # (delay fn args...) -> delay(fn(args...))
      n.into:
        put(g, tkSymbol, "delay")
        put(g, tkParLe, "(")
        gsub(g, n)  # fn
        put(g, tkParLe, "(")
        gcallComma(g, n)  # args
        put(g, tkParRi, ")")
        put(g, tkParRi, ")")

    of Delay0X:
      # (delay0) -> delay()
      n.into:
        put(g, tkSymbol, "delay")
        put(g, tkParLe, "(")
        put(g, tkParRi, ")")

    of SuspendX:
      # (suspend) -> suspend()
      n.into:
        put(g, tkSymbol, "suspend")
        put(g, tkParLe, "(")
        put(g, tkParRi, ")")

    of EmoveX:
      n.into:
        put(g, tkSymbol, "ensureMove")

        put(g, tkParLe, "(")
        gsub(g, n)
        put(g, tkParRi, ")")

    of CallX, CallstrlitX:
      gcall(g, n)

    of HighX, LowX, TypeofX,
       SizeofX, AlignofX, OffsetofX,
       CardX, UnpackX, FieldsX, CompilesX,
       DeclaredX, DefinedX, AstToStrX, BindSymX, BindSymNameX:
      gcallsystem(g, n, $n.exprKind)

    of ProccallX:
      # New flat format: (proccall fn args...) -> render as procCall(fn(args...))
      n.into:
        put(g, tkSymbol, "procCall")
        put(g, tkParLe, "(")
        gsub(g, n)  # fn
        put(g, tkParLe, "(")
        gcallComma(g, n)  # args
        put(g, tkParRi, ")")
        put(g, tkParRi, ")")

    of NewrefX:
      n.into:
        put(g, tkSymbol, "newConstr")

        skip n

        put(g, tkParLe, "(")
        gsub(g, n)
        put(g, tkParRi, ")")

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
      gcallsystem(g, n, "=trace")

    of DefaultobjX, DefaulttupX, DefaultdistinctX:
      gcallsystem(g, n, "default")

    of InsetX:
      n.into:
        put(g, tkSymbol, "contains")
        put(g, tkParLe, "(")

        skip n

        var afterFirst = false

        while n.hasMore:
          if afterFirst:
            gcomma(g)
          else:
            afterFirst = true
          gsub(g, n)

        put(g, tkParRi, ")")

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
      n.into:
        gtype(g, n, c)
        put(g, tkParLe, "(")
        var afterFirst = false
        while n.hasMore:
          if afterFirst:
            gcomma(g)
          else:
            afterFirst = true
          assert n.substructureKind == KvU
          n.into:
            gsub(g, n)
            putWithSpace(g, tkColon, ":")
            gsub(g, n)

            if n.hasMore:
              skip n

      put(g, tkParRi, ")")

    of NewobjX:
      n.into:
        # TODO: find the ref types
        put(g, tkParLe, "(")
        gtype(g, n, c)
        put(g, tkParRi, ")")

        put(g, tkParLe, "(")
        var afterFirst = false
        while n.hasMore:
          if afterFirst:
            gcomma(g)
          else:
            afterFirst = true
          assert n.substructureKind == KvU
          n.into:
            gsub(g, n)
            putWithSpace(g, tkColon, ":")
            gsub(g, n)
            if n.hasMore:
              skip n

      put(g, tkParRi, ")")

    of CmdX:
      gcmd(g, n)

    of EnumtostrX:
      n.into:
        put(g, tkSymbol, "$")

        gsub(g, n)

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
      n.into:
        gsub(g, n)
        put(g, tkSpaces, Space)
        put(g, tkSymbol, opr)
        put(g, tkSpaces, Space)
        gsub(g, n)

    of IsX, InstanceofX:
      let opr: string
      case n.exprKind
      of IsX:
        opr = "is"
      of InstanceofX:
        opr = "of"
      else:
        raiseAssert "unreachable"
      n.into:
        gsub(g, n)
        put(g, tkSpaces, Space)
        put(g, tkSymbol, opr)
        put(g, tkSpaces, Space)
        gtype(g, n, c)

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
      n.into:
        skip n
        gsub(g, n)
        put(g, tkSpaces, Space)
        put(g, tkSymbol, opr)
        put(g, tkSpaces, Space)
        gsub(g, n)

    of ModX, ShrX, ShlX, BitandX, BitorX, BitxorX, BitnotX:
      let opr = $n.exprKind
      n.into:
        skip n
        gsub(g, n)
        put(g, tkSpaces, Space)
        put(g, tkSymbol, opr)
        put(g, tkSpaces, Space)
        gsub(g, n)

    of AshrX:
      n.into:
        skip n
        put(g, tkSymbol, "ashr")
        put(g, tkParLe, "(")

        gsub(g, n)
        gcomma(g)
        gsub(g, n)

        put(g, tkParRi, ")")

    of NegX:
      putWithSpace(g, tkOpr, $n.exprKind)
      n.into:
        skip n
        gsub(g, n)

    of NotX:
      putWithSpace(g, tkOpr, $n.exprKind)
      n.into:
        gsub(g, n)

    of ExprX:
      n.into:
        var isFirst = true

        var ncopy = n

        var hasChildren = false
        if n.hasMore:
          skip ncopy
          hasChildren = ncopy.hasMore

        if hasChildren:
          put(g, tkParLe, "(")
        while n.hasMore:
          if isFirst:
            isFirst = false
          else:
            putWithSpace(g, tkSemiColon, ";")
          gsub(g, n)

        if hasChildren:
          put(g, tkParRi, ")")

    of HderefX, HaddrX:
      n.into:
        gsub(g, n)

    of DerefX:
      n.into:
        gsub(g, n)
        put(g, tkOpr, "[]")

    of BaseobjX:
      n.into:
        put(g, tkParLe, "(")
        gtype(g, n, c)
        put(g, tkParRi, ")")

        skip n # levels

        put(g, tkParLe, "(")
        gsub(g, n)
        put(g, tkParRi, ")")

    of ConvX:
      n.into:
        gtype(g, n, c)
        put(g, tkParLe, "(")
        gsub(g, n)
        put(g, tkParRi, ")")

    of CchoiceX, OchoiceX:
      n.into:
        gsub(g, n)
        while n.hasMore:
          skip n

    of HconvX, DconvX, HcallX:
      n.into:
        skip n
        gsub(g, n)

    of DotX, DdotX:
      n.into:
        gsub(g, n)
        put(g, tkDot, ".")
        gsub(g, n)

        if n.hasMore:
          # inheritance depth
          skip n
        if n.hasMore:
          # access-token string lit (only for private fields)
          skip n

    of PragmaxX:
      gpragmaBlock(g, n)

    of OvfX:
      put(g, tkSymbol, "overflowFlag")
      put(g, tkParLe, "(")
      put(g, tkParRi, ")")
      skip n

    of ErrX:
      if renderIr in g.flags:
        put(g, tkStrLit, toString(n, false))
      else:
        put(g, tkStrLit, errMsgFromCursor(n))
      skip n

    of QuotedX:
      n.into:
        let useSpace = isUseSpace(n)
        put(g, tkAccent, "`")

        var afterFirst = false
        while n.hasMore:
          if afterFirst:
            if useSpace:
              put(g, tkSpaces, Space)
          else:
            afterFirst = true
          gsub(g, n, c)

        put(g, tkAccent, "`")

    of TabconstrX:
      n.into:
        put(g, tkCurlyLe, "{")

        if n.hasMore:
          var afterFirst = false
          while n.hasMore:
            if afterFirst:
              gcomma(g)
            else:
              afterFirst = true

            if n.substructureKind == KvU:
              n.into:
                gsub(g, n, c)
                putWithSpace(g, tkColon, ":")
                gsub(g, n, c)
            else:
              gsub(g, n, c)
        else:
          put(g, tkColon, ":")

        put(g, tkCurlyRi, "}")

    of EnvpX:
      n.into:
        put(g, tkSymbol, "envp")
        put(g, tkParLe, "(")
        gsub(g, n)
        put g, tkComma, ","
        gsub(g, n)
        put(g, tkParRi, ")")

    of CurlyatX:
      n.into:
        gsub(g, n)
        put(g, tkCurlyLe, "{")
        gsub(g, n)
        put(g, tkCurlyRi, "}")
        while n.hasMore:
          skip n

    of ToClosureX:
      n.into:
        put(g, tkSymbol, "toClosure")
        put(g, tkParLe, "(")
        gsub(g, n)
        put(g, tkParRi, ")")

    of IsmainmoduleX,
        DoX, InternalTypeNameX, InternalFieldPairsX, FailedX:
      raiseAssert "todo"

  of IntLit:
    put(g, tkIntLit, $n.intVal)
    inc n
  of UIntLit:
    put(g, tkUIntLit, $n.uintVal)
    inc n
  of FloatLit:
    put(g, tkFloatLit, $n.floatVal)
    inc n
  of StrLit:
    put(g, tkStrLit, toString(n, false))
    inc n
  of CharLit:
    var lit = "\'"
    lit.addEscapedChar(char(n.uoperand))
    lit.add "\'"
    put(g, tkCharLit, lit)
    inc n
  of Symbol:
    var name = pool.syms[n.symId]
    extractBasename(name)
    put(g, tkSymbol, name, n.symId)
    inc n
  of SymbolDef:
    var name = pool.syms[n.symId]
    extractBasename(name)
    put(g, tkSymbol, name, n.symId, isDef = true)
    inc n
  of Ident:
    put(g, tkSymbol, pool.strings[n.strId])
    inc n
  of DotToken:
    inc n
  else:
    # a scope end / illformed token; the `if not n.hasMore: return` at the top
    # already caught the (virtual or real) close in both builds.
    inc n

proc gsub(g: var SrcGen; n: var Cursor, fromStmtList = false, isTopLevel = false) =
  var c: Context = initContext()
  gsub(g, n, c, isTopLevel = isTopLevel)

proc renderTree(n: Cursor, renderFlags: RenderFlags = {}, renderType = false): string =
  var g: SrcGen = initSrcGen(renderFlags)
  let orig = n
  var n = n
  if renderType:
    var c: Context = initContext()
    gtype(g, n, c)
  else:
    gsub(g, n, isTopLevel = true)
  result = g.buf
  if result.len == 0:
    result = if isErrNode(orig): typeToString(orig, g.flags)
             else: toString(orig, false)

proc asNimCode*(n: Cursor; renderFlags: RenderFlags = {}): string =
  var m0: PackedLineInfo = NoLineInfo
  var m1: PackedLineInfo = NoLineInfo
  var n2 = n
  var file0 = FileId 0

  var togo = subtreeWidth(n2)  # tokens incl. suffixes; consume `tokenWidth` per step
  while togo > 0:
    if n2.info.isValid:
      let currentFile = getFileId(lineMan, n2.info)
      if not m0.isValid:
        m0 = n2.info
        file0 = currentFile
      elif not m1.isValid and currentFile == file0:
        m1 = n2.info
    dec togo, tokenWidth(n2)
    if togo > 0: inc n2

  when false: #if m0.isValid:
    if file0.isValid:
      let (_, line0, col0) = unpack(lineMan, m0)
      if m1.isValid:
        let (_, line1, col1) = unpack(lineMan, m1)
        result = extract(pool.filenames[file0],
                        FilePosition(line: line0, col: col0),
                        FilePosition(line: line1, col: col1))
      else:
        result = extract(pool.filenames[file0], FilePosition(line: line0, col: col0))
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
  var typ = n
  if isErrNode(typ):
    if renderIr in renderFlags:
      return toString(typ, false)
    let payload = errPayload(typ)
    if payload.kind == DotToken:
      return "<type error>"
    if isErrNode(payload):
      return typeToString(payload, renderFlags)
    typ = payload
  result = renderTree(typ, renderFlags = renderFlags, renderType = true)

proc typeExprToString*(n: Cursor): string {.inline.} =
  ## Like `typeToString`, but always unwraps `(err …)` to the wrapped type.
  typeToString(n)

proc typeToSrcGen*(n: Cursor; renderFlags: RenderFlags = {}): SrcGen =
  ## Render a type expression and return both rendered buffer and token stream.
  ## Useful for consumers (like docs) that need Nim's renderer plus custom
  ## post-processing (for example: symbol hyperlinks).
  result = initSrcGen(renderFlags)
  var n = n
  var c: Context = initContext()
  gtype(result, n, c)
  if result.buf.len == 0:
    result.buf = toString(n, false)
