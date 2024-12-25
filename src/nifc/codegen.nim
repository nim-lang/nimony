#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# We produce C code as a list of tokens.

import std / [assertions, syncio, tables, sets, intsets, formatfloat, strutils, packedsets]
from std / os import changeFileExt, splitFile, extractFilename

import .. / lib / [bitabs, packedtrees, lineinfos]
import mangler, nifc_model, cprelude, noptions

type
  Token = distinct uint32

proc `==`(a, b: Token): bool {.borrow.}

type
  PredefinedToken = enum
    IgnoreMe = "<unused>"
    EmptyToken = ""
    CurlyLe = "{"
    CurlyRi = "}"
    ParLe = "("
    ParRi = ")"
    BracketLe = "["
    BracketRi = "]"
    NewLine = "\n"
    Semicolon = ";"
    Comma = ", "
    Space = " "
    Colon = ": "
    Dot = "."
    Arrow = "->"
    Star = "*"
    Amp = "&"
    AsgnOpr = " = "
    ScopeOpr = "::"
    ConstKeyword = "const "
    StaticKeyword = "static "
    ExternKeyword = "extern "
    WhileKeyword = "while "
    GotoKeyword = "goto "
    IfKeyword = "if ("
    ElseKeyword = "else "
    SwitchKeyword = "switch "
    CaseKeyword = "case "
    DefaultKeyword = "default:"
    BreakKeyword = "break"
    NullPtr = "NIM_NIL"
    IfNot = "if (!("
    ReturnKeyword = "return"
    TypedefStruct = "typedef struct "
    TypedefUnion = "typedef union "
    TypedefKeyword = "typedef "
    IncludeKeyword = "#include "
    LineDirKeyword = "#line "
    DiscardToken = "(void) "
    TryKeyword = "try "
    CatchKeyword = "catch ("
    ThrowKeyword = "throw"
    ErrToken = "NIFC_ERR_"
    ThreadVarToken = "NIM_THREADVAR "

proc fillTokenTable(tab: var BiTable[Token, string]) =
  for e in EmptyToken..high(PredefinedToken):
    let id = tab.getOrIncl $e
    assert id == Token(e), $(id, " ", ord(e))

type
  GenFlags* = enum
    gfMainModule # isMainModule
    gfHasError   # already generated the error variable

  GeneratedCode* = object
    m: Module
    includes: seq[Token]
    includedHeaders: IntSet
    data: seq[Token]
    protos: seq[Token]
    code: seq[Token]
    init: seq[Token]
    fileIds: PackedSet[FileId]
    tokens: BiTable[Token, string]
    inSimpleInit: int
    headerFile: seq[Token]
    generatedTypes: IntSet
    requestedSyms: HashSet[string]
    flags: set[GenFlags]

proc initGeneratedCode*(m: sink Module, isMain: bool): GeneratedCode =
  result = GeneratedCode(m: m, code: @[], tokens: initBiTable[Token, string](),
      fileIds: initPackedSet[FileId]())
  if isMain:
    result.flags.incl gfMainModule
  fillTokenTable(result.tokens)

proc add*(c: var GeneratedCode; t: PredefinedToken) {.inline.} =
  c.code.add Token(t)

proc add*(c: var GeneratedCode; s: string) {.inline.} =
  c.code.add c.tokens.getOrIncl(s)

type
  CppFile = object
    f: File

proc write(f: var CppFile; s: string) = write(f.f, s)
proc write(f: var CppFile; c: char) = write(f.f, c)

proc writeTokenSeq(f: var CppFile; s: seq[Token]; c: GeneratedCode) =
  var indent = 0
  for i in 0..<s.len:
    let x = s[i]
    case x
    of Token(CurlyLe):
      inc indent
      write f, c.tokens[x]
      write f, "\n"
      for i in 1..indent*2: write f, ' '
    of Token(CurlyRi):
      dec indent
      write f, c.tokens[x]
      if i+1 < s.len and s[i+1] == Token(CurlyRi):
        discard
      else:
        write f, "\n"
        for i in 1..indent*2: write f, ' '
    of Token(Semicolon):
      write f, c.tokens[x]
      if i+1 < s.len and s[i+1] == Token(CurlyRi):
        discard "no newline before }"
      else:
        write f, "\n"
        for i in 1..indent*2: write f, ' '
    of Token(NewLine):
      write f, c.tokens[x]
      for i in 1..indent*2: write f, ' '
    else:
      write f, c.tokens[x]

proc error(m: Module; msg: string; tree: PackedTree[NifcKind]; n: NodePos) {.noreturn.} =
  write stdout, "[Error] "
  write stdout, msg
  writeLine stdout, toString(tree, n, m)
  when defined(debug):
    echo getStackTrace()
  quit 1

# Atoms

proc genIntLit(c: var GeneratedCode; litId: StrId) =
  let i = parseBiggestInt(c.m.lits.strings[litId])
  if i > low(int32) and i <= high(int32):
    c.add $i
  elif i == low(int32):
    # Nim has the same bug for the same reasons :-)
    c.add "(-2147483647 -1)"
  elif i > low(int64):
    c.add "IL64("
    c.add $i
    c.add ")"
  else:
    c.add "(IL64(-9223372036854775807) - IL64(1))"

proc genUIntLit(c: var GeneratedCode; litId: StrId) =
  let i = parseBiggestUInt(c.m.lits.strings[litId])
  if i <= high(uint32):
    c.add $i
    c.add "u"
  else:
    c.add $i
    c.add "ull"

# Type graph

const
  CallingConvToStr: array[CdeclC..NoinlineC, string] = [
    "N_CDECL",
    "N_STDCALL", "N_SAFECALL",
    "N_SYSCALL", # this is probably not correct for all platforms,
                 # but one can #define it to what one wants
    "N_FASTCALL",
    "N_THISCALL",
    "N_NOCONV",
    "N_NOCONV", #ccMember is N_NOCONV
    "N_INLINE", "N_NOINLINE"
    ]

include gentypes

# Procs

template emitData(s: string) = c.data.add c.tokens.getOrIncl(s)
template emitData(t: Token) = c.data.add t
template emitData(t: PredefinedToken) = c.data.add Token(t)

proc genStrLit(c: var GeneratedCode; litId: StrId): Token =
  let cstr = makeCString(c.m.lits.strings[litId])
  result = c.tokens.getOrIncl cstr

proc inclHeader(c: var GeneratedCode, name: string) =
  let header = c.tokens.getOrIncl(name)
  if not c.includedHeaders.containsOrIncl(int header):
    c.includes.add Token(IncludeKeyword)
    c.includes.add header
    c.includes.add Token NewLine

include selectany

type
  ProcFlag = enum
    isSelectAny, isVarargs

proc genProcPragmas(c: var GeneratedCode; t: Tree; n: NodePos;
                    flags: var set[ProcFlag]) =
  # ProcPragma ::= (inline) | (noinline) | CallingConvention | (varargs) | (was Identifier) |
  #               (selectany) | Attribute | (raises) | (errs)
  if t[n].kind == Empty:
    discard
  elif t[n].kind == PragmasC:
    for ch in sons(t, n):
      case t[ch].kind
      of CallingConventions, InlineC, NoinlineC:
        discard "already handled"
      of VarargsC:
        flags.incl isVarargs
      of SelectanyC:
        flags.incl isSelectAny
      of AttrC:
        c.add " __attribute__((" & c.m.lits.strings[t[ch.firstSon].litId] & "))"
      of WasC:
        c.add "/* " & toString(t, ch.firstSon, c.m) & " */"
      of ErrsC, RaisesC:
        discard
      else:
        error c.m, "invalid proc pragma: ", t, ch
  else:
    error c.m, "expected proc pragmas but got: ", t, n

proc genSymDef(c: var GeneratedCode; t: Tree; n: NodePos): string =
  if t[n].kind == SymDef:
    let lit = t[n].litId
    result = mangle(c.m.lits.strings[lit])
    c.add result
  else:
    error c.m, "expected SymbolDef but got: ", t, n
    result = ""

proc genParamPragmas(c: var GeneratedCode; t: Tree; n: NodePos) =
  # ProcPragma ::= (was Identifier) | Attribute
  if t[n].kind == Empty:
    discard
  elif t[n].kind == PragmasC:
    for ch in sons(t, n):
      case t[ch].kind
      of AttrC:
        c.add " __attribute__((" & c.m.lits.strings[t[ch.firstSon].litId] & "))"
      of WasC:
        c.add "/* " & toString(t, ch.firstSon, c.m) & " */"
      else:
        error c.m, "invalid pragma: ", t, ch
  else:
    error c.m, "expected pragmas but got: ", t, n

proc genParam(c: var GeneratedCode; t: Tree; n: NodePos) =
  let d = asParamDecl(t, n)
  if t[d.name].kind == SymDef:
    let lit = t[d.name].litId
    let name = mangle(c.m.lits.strings[lit])
    genType c, t, d.typ, name
    genParamPragmas c, t, d.pragmas
  else:
    error c.m, "expected SymbolDef but got: ", t, n

proc genVarPragmas(c: var GeneratedCode; t: Tree; n: NodePos) =
  if t[n].kind == Empty:
    discard
  elif t[n].kind == PragmasC:
    for ch in sons(t, n):
      case t[ch].kind
      of AlignC:
        c.add " NIM_ALIGN(" & toString(t, ch.firstSon, c.m) & ")"
      of AttrC:
        c.add " __attribute__((" & c.m.lits.strings[t[ch.firstSon].litId] & "))"
      of WasC:
        c.add "/* " & toString(t, ch.firstSon, c.m) & " */"
      else:
        error c.m, "invalid pragma: ", t, ch
  else:
    error c.m, "expected pragmas but got: ", t, n

proc genCLineDir(c: var GeneratedCode; t: Tree; info: PackedLineInfo) =
  if optLineDir in c.m.config.options:
    let (id, line, _) = unpack(c.m.lits.man, info)
    if c.m.lits.files.hasId(id):
      let name = "FX_" & $(int id)
      c.add LineDirKeyword
      c.add $line
      c.add Space
      c.add name
      c.add NewLine

      c.fileIds.incl id

template moveToDataSection(body: untyped) =
  let oldLen = c.code.len
  body
  for i in oldLen ..< c.code.len:
    c.data.add c.code[i]
  setLen c.code, oldLen

include genexprs

type
  VarKind = enum
    IsLocal, IsGlobal, IsThreadlocal, IsConst

proc genVarDecl(c: var GeneratedCode; t: Tree; n: NodePos; vk: VarKind; toExtern = false) =
  let d = asVarDecl(t, n)
  genCLineDir(c, t, info(t, n))
  if t[d.name].kind == SymDef:
    let lit = t[d.name].litId
    let name = mangle(c.m.lits.strings[lit])
    if toExtern:
      c.add ExternKeyword

    if vk == IsConst:
      c.add ConstKeyword
    if vk == IsThreadlocal:
      c.add "__thread "
    genType c, t, d.typ, name
    genVarPragmas c, t, d.pragmas
    if t[d.value].kind != Empty:
      c.add AsgnOpr
      if vk != IsLocal: inc c.inSimpleInit
      genx c, t, d.value
      if vk != IsLocal: dec c.inSimpleInit
    c.add Semicolon
  else:
    error c.m, "expected SymbolDef but got: ", t, n

include genstmts


proc genProcDecl(c: var GeneratedCode; t: Tree; n: NodePos; isExtern: bool) =
  let signatureBegin = c.code.len
  let prc = asProcDecl(t, n)

  if isExtern:
    c.add ExternKeyword

  var lastCallConv = Empty
  if t[prc.pragmas].kind == PragmasC:
    for ch in sons(t, prc.pragmas):
      case t[ch].kind
      of CallingConventions, InlineC, NoinlineC:
        lastCallConv = t[ch].kind
      else:
        discard

  let name: string
  if lastCallConv != Empty:
    if lastCallConv == InlineC:
      c.add StaticKeyword
    c.add CallingConvToStr[lastCallConv]
    c.add ParLe
    if t[prc.returnType].kind == Empty:
      c.add "void"
    else:
      genType c, t, prc.returnType
    c.add Comma
    name = genSymDef(c, t, prc.name)
    c.add ParRi
  else:
    if t[prc.returnType].kind == Empty:
      c.add "void"
    else:
      genType c, t, prc.returnType
    c.add Space
    name = genSymDef(c, t, prc.name)

  var flags: set[ProcFlag] = {}
  genProcPragmas c, t, prc.pragmas, flags

  c.add ParLe

  var params = 0
  if t[prc.params].kind != Empty:
    for ch in sons(t, prc.params):
      if params > 0: c.add Comma
      genParam c, t, ch
      inc params

  if isVarargs in flags:
    if params > 0: c.add Comma
    c.add "..."
    inc params

  if params == 0:
    c.add "void"
  c.add ParRi

  if isExtern or c.requestedSyms.contains(name):
    # symbol was used before its declaration has been processed so
    # add a signature:
    for i in signatureBegin ..< c.code.len:
      c.protos.add c.code[i]
    c.protos.add Token Semicolon

  if isExtern:
    c.code.setLen signatureBegin
  else:
    if isSelectAny in flags:
      genRoutineGuardBegin(c, name)
    c.add CurlyLe
    genStmt c, t, prc.body
    c.add CurlyRi
    if isSelectAny in flags:
      genRoutineGuardEnd(c)

proc genInclude(c: var GeneratedCode; t: Tree; n: NodePos) =
  let lit = t[n.firstSon].litId
  let headerAsStr {.cursor.} = c.m.lits.strings[lit]
  let header = c.tokens.getOrIncl(headerAsStr)
  if headerAsStr.len > 0 and not c.includedHeaders.containsOrIncl(int header):
    if headerAsStr[0] == '#':
      discard "skip the #include keyword"
    else:
      c.includes.add Token(IncludeKeyword)
    c.includes.add header
    c.includes.add Token NewLine

proc genImp(c: var GeneratedCode; t: Tree; n: NodePos) =
  let arg = n.firstSon
  case t[arg].kind
  of ProcC: genProcDecl c, t, arg, true
  of VarC, GvarC, TvarC, ConstC:
    genVar c, t, arg, true
  else:
    error c.m, "expected declaration for `imp` but got: ", t, n

proc genNodecl(c: var GeneratedCode; t: Tree; n: NodePos) =
  let signatureBegin = c.code.len
  let arg = n.firstSon
  case t[arg].kind
  of ProcC: genProcDecl c, t, arg, false
  of VarC: genStmt c, t, arg
  of ConstC: genStmt c, t, arg
  else:
    error c.m, "expected declaration for `nodecl` but got: ", t, n
  c.code.setLen signatureBegin

proc genToplevel(c: var GeneratedCode; t: Tree; n: NodePos) =
  # ExternDecl ::= (imp ProcDecl | VarDecl | ConstDecl)
  # Include ::= (incl StringLiteral)
  # TopLevelConstruct ::= ExternDecl | ProcDecl | VarDecl | ConstDecl |
  #                       TypeDecl | Include | EmitStmt
  case t[n].kind
  of ImpC: genImp c, t, n
  of NodeclC: genNodecl c, t, n
  of InclC: genInclude c, t, n
  of ProcC: genProcDecl c, t, n, false
  of VarC, GvarC, TvarC: genStmt c, t, n
  of ConstC: genStmt c, t, n
  of TypeC: discard "handled in a different pass"
  of EmitC: genEmitStmt c, t, n
  else:
    error c.m, "expected top level construct but got: ", t, n

proc traverseCode(c: var GeneratedCode; t: Tree; n: NodePos) =
  case t[n].kind
  of StmtsC:
    for ch in sons(t, n): genToplevel(c, t, ch)
  else:
    error c.m, "expected `stmts` but got: ", t, n

  when false:
    var i = NodePos(0)
    while i.int < c.m.code.len:
      let oldLen = c.code.len
      let moveToInitSection = false # c.m.code[NodePos(i)].kind notin AllowedInToplevelC

      gen c, c.m.code, NodePos(i)
      next c.m.code, i

      if moveToInitSection:
        for i in oldLen ..< c.code.len:
          c.init.add c.code[i]
        setLen c.code, oldLen

proc writeLineDir(f: var CppFile, c: var GeneratedCode) =
  for id in items(c.fileIds):
    let name = "FX_" & $(int id)
    let def = "#define " & name & " \"" & c.m.lits.files[id] & "\""
    write f, def
    write f, "\n"

proc generateCode*(s: var State, inp, outp: string; isMain: bool) =
  var m = load(inp)
  m.config = s.config
  var c = initGeneratedCode(m, isMain)

  var co = TypeOrder()
  traverseTypes(c.m, co)

  generateTypes(c, c.m.code, co)
  let typeDecls = move c.code

  traverseCode c, c.m.code, StartPos
  var f = CppFile(f: open(outp, fmWrite))
  f.write "#define NIM_INTBITS " & $s.bits & "\n"
  f.write Prelude
  if isMain:
    f.write $ThreadVarToken & "NB8 " & $ErrToken & $Semicolon & "\n"

  writeTokenSeq f, c.includes, c
  if optLineDir in c.m.config.options:
    writeLineDir f, c
  writeTokenSeq f, typeDecls, c
  writeTokenSeq f, c.data, c
  writeTokenSeq f, c.protos, c
  writeTokenSeq f, c.code, c
  if c.init.len > 0:
    f.write "void __attribute__((constructor)) init(void) {"
    writeTokenSeq f, c.init, c
    f.write "}\n\n"
  f.f.close

  if c.headerFile.len > 0:
    let selectHeader = outp.changeFileExt(".h")
    s.selects.add selectHeader
    var h = open(selectHeader, fmWrite)
    for x in items(c.headerFile):
      write h, c.tokens[x]
    h.close()
