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

include ".." / lib / nifprelude
import mangler, nifc_model, cprelude, noptions, typenav

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
    DoubleQuote = "\""
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
  GenFlag* = enum
    gfMainModule # isMainModule
    gfHasError   # already generated the error variable
    gfProducesMainProc # needs main proc

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
    generatedTypes: HashSet[SymId]
    requestedSyms: HashSet[string]
    flags: set[GenFlag]

proc initGeneratedCode*(m: sink Module, flags: set[GenFlag]): GeneratedCode =
  result = GeneratedCode(m: m, code: @[], tokens: initBiTable[Token, string](),
      fileIds: initPackedSet[FileId](), flags: flags)
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

proc error(m: Module; msg: string; n: Cursor) {.noreturn.} =
  write stdout, "[Error] "
  write stdout, msg
  writeLine stdout, toString(n, false)
  when defined(debug):
    echo getStackTrace()
  quit 1

# Atoms

proc genIntLit(c: var GeneratedCode; litId: IntId) =
  let i = pool.integers[litId]
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

proc genUIntLit(c: var GeneratedCode; litId: UIntId) =
  let i = pool.uintegers[litId]
  if i <= high(uint32):
    c.add $i
    c.add "u"
  else:
    c.add $i
    c.add "ull"

# Type graph

proc callingConvToStr(cc: CallConv): string =
  case cc
  of NoCallConv: ""
  of Cdecl: "N_CDECL"
  of Stdcall: "N_STDCALL"
  of Safecall: "N_SAFECALL"
  of Syscall: "N_SYSCALL"
  of Fastcall: "N_FASTCALL"
  of Thiscall: "N_THISCALL"
  of Noconv: "N_NOCONV"
  of Member: "N_NOCONV"
  of Nimcall: "N_FASTCALL"

include gentypes

# Procs

template emitData(s: string) = c.data.add c.tokens.getOrIncl(s)
template emitData(t: Token) = c.data.add t
template emitData(t: PredefinedToken) = c.data.add Token(t)

proc genStrLit(c: var GeneratedCode; litId: StrId): Token =
  let cstr = makeCString(pool.strings[litId])
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

proc genProcPragmas(c: var GeneratedCode; n: var Cursor;
                    flags: var set[ProcFlag]) =
  # ProcPragma ::= (inline) | (noinline) | CallingConvention | (varargs) | (was Identifier) |
  #               (selectany) | Attribute | (raises) | (errs)
  if n.kind == DotToken:
    inc n
  elif n.substructureKind == PragmasU:
    inc n
    while n.kind != ParRi:
      case n.pragmaKind
      of NoPragma, AlignP, BitsP, VectorP, NodeclP, StaticP:
        if n.callConvKind != NoCallConv:
          skip n
        else:
          error c.m, "invalid proc pragma: ", n
      of InlineP, NoinlineP:
        discard "already handled"
        skip n
      of VarargsP:
        flags.incl isVarargs
        skip n
      of SelectanyP:
        flags.incl isSelectAny
        skip n
      of AttrP:
        discard "already handled"
        skip n
      of WasP:
        inc n
        c.add "/* " & toString(n, false) & " */"
        skip n
        skipParRi n
      of ErrsP, RaisesP:
        skip n
    inc n # ParRi
  else:
    error c.m, "expected proc pragmas but got: ", n

proc genSymDef(c: var GeneratedCode; n: Cursor): string =
  if n.kind == SymbolDef:
    let lit = n.symId
    result = mangle(pool.syms[lit])
    c.add result
  else:
    result = ""
    error c.m, "expected SymbolDef but got: ", n

proc genParamPragmas(c: var GeneratedCode; n: var Cursor) =
  # ProcPragma ::= (was Identifier) | Attribute
  if n.kind == DotToken:
    inc n
  elif n.substructureKind == PragmasU:
    inc n
    while n.kind != ParRi:
      case n.pragmaKind
      of AttrP:
        inc n
        c.add " __attribute__((" & pool.strings[n.litId] & "))"
        inc n
        skipParRi n
      of WasP:
        inc n
        c.add "/* " & toString(n, false) & " */"
        skip n
        skipParRi n
      else:
        error c.m, "invalid pragma: ", n
    inc n # ParRi
  else:
    error c.m, "expected pragmas but got: ", n

proc genParam(c: var GeneratedCode; n: var Cursor) =
  var d = takeParamDecl(n)
  if d.name.kind == SymbolDef:
    let name = mangle(pool.syms[d.name.symId])
    genType c, d.typ, name
    genParamPragmas c, d.pragmas
  else:
    error c.m, "expected SymbolDef but got: ", d.name

proc genVarPragmas(c: var GeneratedCode; n: var Cursor): NifcPragma =
  result = NoPragma
  if n.kind == DotToken:
    inc n
  elif n.substructureKind == PragmasU:
    inc n
    while n.kind != ParRi:
      case n.pragmaKind
      of AlignP:
        inc n
        c.add " NIM_ALIGN(" & $pool.integers[n.intId] & ")"
        inc n
        skipParRi n
      of AttrP:
        inc n
        c.add " __attribute__((" & pool.strings[n.litId] & "))"
        skip n
        skipParRi n
      of WasP:
        inc n
        c.add "/* " & toString(n, false) & " */"
        skip n
        skipParRi n
      of StaticP:
        result = StaticP
      else:
        error c.m, "invalid pragma: ", n
    inc n # ParRi
  else:
    error c.m, "expected pragmas but got: ", n

proc genCLineDir(c: var GeneratedCode; info: PackedLineInfo) =
  if optLineDir in c.m.config.options and info.isValid:
    let (id, line, _) = unpack(pool.man, info)
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

template moveToInitSection(body: untyped) =
  let oldLen = c.code.len
  body
  for i in oldLen ..< c.code.len:
    c.init.add c.code[i]
  setLen c.code, oldLen

include genexprs

type
  VarKind = enum
    IsLocal, IsGlobal, IsThreadlocal, IsConst

proc isLiteral(n: var Cursor): bool =
  case n.kind
  of IntLit, UIntLit, FloatLit, CharLit, StringLit:
    result = true
    inc n
  else:
    case n.exprKind
    of FalseC, TrueC, InfC, NegInfC, NanC, SufC:
      result = true
      skip n
    of AconstrC, OconstrC:
      result = true
      inc n
      skip n # type
      while n.kind != ParRi:
        if not isLiteral(n): return false
      skipParRi n
    else:
      result = false

proc genVarDecl(c: var GeneratedCode; n: var Cursor; vk: VarKind; toExtern = false) =
  genCLineDir(c, info(n))
  var d = takeVarDecl(n)
  if d.name.kind == SymbolDef:
    let lit = d.name.symId
    let name = mangle(pool.syms[lit])
    let beforeDecl = c.code.len
    if toExtern:
      c.add ExternKeyword

    if vk == IsConst:
      c.add ConstKeyword
    if vk == IsThreadlocal:
      c.add "__thread "
    genType c, d.typ, name
    let vis = genVarPragmas(c, d.pragmas)
    if vis == StaticP:
      c.code.insert(Token(StaticKeyword), beforeDecl)
    if d.value.kind != DotToken:
      var value = d.value
      if vk == IsGlobal and not isLiteral(value):
        c.add Semicolon
        moveToInitSection:
          c.add name
          c.add AsgnOpr
          genx c, d.value
          c.add Semicolon
      else:
        c.add AsgnOpr
        if vk != IsLocal: inc c.inSimpleInit
        genx c, d.value
        if vk != IsLocal: dec c.inSimpleInit
        c.add Semicolon
    else:
      c.add Semicolon
  else:
    error c.m, "expected SymbolDef but got: ", d.name

include genstmts


proc genProcDecl(c: var GeneratedCode; n: var Cursor; isExtern: bool) =
  let signatureBegin = c.code.len
  var prc = takeProcDecl(n)

  if isExtern:
    c.add ExternKeyword

  var lastCallConv = NoCallConv
  var lastAttrString = ""
  if prc.pragmas.kind == ParLe:
    var p = prc.pragmas.firstSon
    while p.kind != ParRi:
      case p.pragmaKind
      of InlineP:
        c.add StaticKeyword
        c.add "inline "
        skip p
      of NoinlineP:
        c.add "N_NOINLINE "
        skip p
      of AttrP:
        inc p
        lastAttrString = "__attribute__((" & pool.strings[p.litId] & ")) "
        inc p
        skipParRi p
      else:
        if p.callConvKind != NoCallConv:
          lastCallConv = p.callConvKind
          skip p
        else:
          error c.m, "invalid pragma: ", p

  let name: string
  if lastCallConv != NoCallConv:
    c.add callingConvToStr(lastCallConv)
    c.add ParLe
    if prc.returnType.kind == DotToken:
      c.add "void"
    else:
      genType c, prc.returnType
    c.add Comma
    c.add lastAttrString
    name = genSymDef(c, prc.name)
    c.add ParRi
  else:
    if prc.returnType.kind == DotToken:
      c.add "void"
    else:
      genType c, prc.returnType
    c.add Space
    c.add lastAttrString
    name = genSymDef(c, prc.name)

  var flags: set[ProcFlag] = {}
  genProcPragmas c, prc.pragmas, flags

  c.add ParLe

  var params = 0
  if prc.params.kind != DotToken:
    var p = prc.params.firstSon
    while p.kind != ParRi:
      if params > 0: c.add Comma
      genParam c, p
      inc params
      skip p
    skipParRi p

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
    genStmt c, prc.body
    c.add CurlyRi
    if isSelectAny in flags:
      genRoutineGuardEnd(c)

proc genInclude(c: var GeneratedCode; n: var Cursor) =
  inc n
  let lit = n.litId
  let headerAsStr {.cursor.} = pool.strings[lit]
  let header = c.tokens.getOrIncl(headerAsStr)
  if headerAsStr.len > 0 and not c.includedHeaders.containsOrIncl(int header):
    if headerAsStr[0] == '#':
      discard "skip the #include keyword"
    else:
      c.includes.add Token(IncludeKeyword)
    if headerAsStr[0] == '<':
      c.includes.add header
    else:
      c.includes.add Token(DoubleQuote)
      c.includes.add header
      c.includes.add Token(DoubleQuote)

    c.includes.add Token NewLine
  skipParRi n

proc genImp(c: var GeneratedCode; n: var Cursor) =
  inc n
  case n.stmtKind
  of ProcS: genProcDecl c, n, true
  of VarS:
    # XXX Disallow this: You can only import global variables!
    genVar c, n, IsGlobal, true
  of GvarS:
    genVar c, n, IsGlobal, true
  of TvarS:
    genVar c, n, IsThreadlocal, true
  of ConstS:
    genVar c, n, IsConst, true
  else:
    error c.m, "expected declaration for `imp` but got: ", n
  skipParRi n

proc genNodecl(c: var GeneratedCode; n: var Cursor) =
  let signatureBegin = c.code.len
  inc n
  case n.stmtKind
  of ProcS: genProcDecl c, n, false
  of VarS: genStmt c, n
  of ConstS: genStmt c, n
  else:
    error c.m, "expected declaration for `nodecl` but got: ", n
  skipParRi n
  c.code.setLen signatureBegin

proc genToplevel(c: var GeneratedCode; n: var Cursor) =
  # ExternDecl ::= (imp ProcDecl | VarDecl | ConstDecl)
  # Include ::= (incl StringLiteral)
  # TopLevelConstruct ::= ExternDecl | ProcDecl | VarDecl | ConstDecl |
  #                       TypeDecl | Include | EmitStmt
  case n.stmtKind
  of ImpS: genImp c, n
  of InclS: genInclude c, n
  of ProcS: genProcDecl c, n, false
  of VarS, GvarS, TvarS: genStmt c, n
  of ConstS: genStmt c, n
  of DiscardS, AsgnS, ScopeS, IfS,
      WhileS, CaseS, LabS, JmpS, TryS, RaiseS, CallS, OnErrS:
    moveToInitSection:
      genStmt c, n
  of TypeS: discard "handled in a different pass"
  of EmitS: genEmitStmt c, n
  else:
    if n.pragmaKind == NodeclP:
      genNodecl c, n
    else:
      error c.m, "expected top level construct but got: ", n

proc traverseCode(c: var GeneratedCode; n: var Cursor) =
  if n.stmtKind == StmtsS:
    inc n
    while n.kind != ParRi: genToplevel(c, n)
    # missing `inc n` here is intentional
  else:
    error c.m, "expected `stmts` but got: ", n

proc writeLineDir(f: var CppFile, c: var GeneratedCode) =
  for id in items(c.fileIds):
    let name = "FX_" & $(int id)
    let def = "#define " & name & " \"" & pool.files[id] & "\""
    write f, def
    write f, "\n"

proc generateCode*(s: var State, inp, outp: string; flags: set[GenFlag]) =
  var m = load(inp)
  m.config = s.config
  var c = initGeneratedCode(m, flags)

  var co = TypeOrder()
  traverseTypes(c.m, co)

  generateTypes(c, co)
  let typeDecls = move c.code

  var n = beginRead(c.m.code)
  traverseCode c, n
  var f = CppFile(f: open(outp, fmWrite))
  f.write "#define NIM_INTBITS " & $s.bits & "\n"
  f.write Prelude
  if gfMainModule in c.flags:
    f.write $ThreadVarToken & "NB8 " & $ErrToken & $Semicolon & "\n"

  writeTokenSeq f, c.includes, c
  if optLineDir in c.m.config.options:
    writeLineDir f, c
  writeTokenSeq f, typeDecls, c
  writeTokenSeq f, c.data, c
  writeTokenSeq f, c.protos, c
  writeTokenSeq f, c.code, c

  if gfProducesMainProc in c.flags:
    f.write "int cmdCount;\n"
    f.write "char **cmdLine;\n"
    f.write "int main(int argc, char **argv) {\n"
    f.write "  cmdCount = argc;\n"
    f.write "  cmdLine = argv;\n"
    writeTokenSeq f, c.init, c
    f.write "}\n\n"
  elif c.init.len > 0:
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
