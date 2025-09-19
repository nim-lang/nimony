#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# We produce C code as a list of tokens.

import std / [assertions, syncio, tables, sets, intsets, formatfloat, strutils, packedsets]
from std / os import changeFileExt, splitFile, extractFilename
from std / sequtils import insert

include ".." / lib / nifprelude
import mangler, nifc_model, cprelude, noptions, typenav, symparser

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
    IfKeyword = "if "
    ElseKeyword = "else "
    SwitchKeyword = "switch "
    CaseKeyword = "case "
    DefaultKeyword = "default:"
    BreakKeyword = "break"
    NullPtr = "NIM_NIL"
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
    OvfToken = "NIFC_OVF_"
    ThreadVarToken = "NIM_THREADVAR "
    AnonStruct = "struct "
    AnonUnion = "union "

proc fillTokenTable(tab: var BiTable[Token, string]) =
  for e in EmptyToken..high(PredefinedToken):
    let id = tab.getOrIncl $e
    assert id == Token(e), $(id, " ", ord(e))

type
  GenFlag* = enum
    gfMainModule # isMainModule
    gfHasError   # already generated the error variable
    gfProducesMainProc # needs main proc
    gfInCallImportC # in importC call context

  CurrentProc* = object
    needsOverflowFlag: bool
    nextTemp: int

  GeneratedCode* = object
    m: Module
    includes: seq[Token]
    includedHeaders: IntSet
    protos: seq[Token]
    data: seq[Token]
    code: seq[Token]
    init: seq[Token]
    fileIds: PackedSet[FileId]
    tokens: BiTable[Token, string]
    headerFile: seq[Token]
    generatedTypes: HashSet[SymId]
    requestedSyms: HashSet[string]
    flags: set[GenFlag]
    inToplevel: bool
    objConstrNeedsType: bool
    bits: int
    currentProc: CurrentProc

proc initGeneratedCode*(m: sink Module, flags: set[GenFlag]; bits: int): GeneratedCode =
  result = GeneratedCode(m: m, code: @[], tokens: initBiTable[Token, string](),
      fileIds: initPackedSet[FileId](), flags: flags, inToplevel: true,
      objConstrNeedsType: true, bits: bits)
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
  let info = n.info
  if info.isValid:
    let rawInfo = unpack(pool.man, info)
    if rawInfo.file.isValid:
      write stdout, pool.files[rawInfo.file]
      write stdout, "(" & $rawInfo.line & ", " & $(rawInfo.col+1) & ") "
  write stdout, "[Error] "
  write stdout, msg
  writeLine stdout, toString(n, false)
  when defined(debug):
    echo getStackTrace()
  quit 1

# Atoms

proc genIntLit(c: var GeneratedCode; litId: IntId) =
  let i = pool.integers[litId]
  if i > low(int32) and i <= high(int32) and c.bits != 64:
    c.add $i
  elif i == low(int32) and c.bits != 64:
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
  if i <= high(uint32) and c.bits != 64:
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
  of Nimcall: "N_NIMCALL"

include gentypes

# Procs

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
      of NoPragma, AlignP, BitsP, VectorP, NodeclP, StaticP, PackedP:
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
        genWasPragma c, n
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
        genWasPragma c, n
      else:
        error c.m, "invalid pragma: ", n
    inc n # ParRi
  else:
    error c.m, "expected pragmas but got: ", n

proc genParam(c: var GeneratedCode; n: var Cursor) =
  var d = takeParamDecl(n)
  if d.name.kind == SymbolDef:
    let lit = d.name.symId
    c.m.registerLocal(lit, d.typ)
    let name = mangle(pool.syms[lit])
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
        genWasPragma c, n
      of StaticP:
        result = StaticP
        skip n
      else:
        error c.m, "invalid pragma: ", n
    inc n # ParRi
  else:
    error c.m, "expected pragmas but got: ", n

proc genCLineDir(c: var GeneratedCode; info: PackedLineInfo) =
  if optLineDir in c.m.config.options and info.isValid:
    let rawInfo = unpack(pool.man, info)
    let id = rawInfo.file
    let line = rawInfo.line
    let name = "FX_" & $(int id)
    c.add LineDirKeyword
    c.add $line
    c.add Space
    c.add name
    c.add NewLine
    if id.isValid:
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
  of IntLit, UIntLit, FloatLit, CharLit, StringLit, DotToken:
    result = true
    inc n
  else:
    case n.exprKind
    of FalseC, TrueC, InfC, NegInfC, NanC, SufC, NilC:
      result = true
      skip n
    of AconstrC, OconstrC, CastC, ConvC:
      result = true
      inc n
      skip n # type
      while n.kind != ParRi:
        if n.substructureKind == KvU:
          inc n
          if not isLiteral(n): return false
          skip n # key
          if n.kind != ParRi:
            # optional inheritance
            skip n
          skipParRi n
        else:
          if not isLiteral(n): return false
      skipParRi n
    else:
      result = false

proc genStmt(c: var GeneratedCode; n: var Cursor)

proc genOnError(c: var GeneratedCode; n: var Cursor) =
  c.add IfKeyword
  c.add ParLe
  c.add ErrToken
  c.add ParRi
  c.add Space
  c.add CurlyLe
  c.genStmt n
  c.add CurlyRi

proc genVarInitValue(c: var GeneratedCode; n: var Cursor) =
  if n.kind == DotToken:
    inc n
    c.add Semicolon
  elif n.stmtKind == OnErrS:
    var onErrAction = n
    inc onErrAction
    c.add AsgnOpr
    genCallCanRaise c, n
    c.add Semicolon
    if onErrAction.kind != DotToken:
      genOnError(c, onErrAction)
  else:
    c.add AsgnOpr
    genx c, n
    c.add Semicolon

proc genVarDecl(c: var GeneratedCode; n: var Cursor; vk: VarKind; toExtern = false; useStatic = false) =
  genCLineDir(c, info(n))
  var d = takeVarDecl(n)
  if d.name.kind == SymbolDef:
    let lit = d.name.symId
    c.m.registerLocal(lit, d.typ)
    let name = mangle(pool.syms[lit])
    let beforeDecl = c.code.len

    if toExtern or isImportC(d.name):
      c.add ExternKeyword

    if vk == IsThreadlocal:
      c.add "__thread "
    genType c, d.typ, name, isConst = vk == IsConst
    let vis = genVarPragmas(c, d.pragmas)
    if vis == StaticP or useStatic:
      c.code.insert(Token(StaticKeyword), beforeDecl)
    let beforeInit = c.code.len

    var value = d.value
    let mustMoveToInit = (vk == IsGlobal and not isLiteral(value))
    if toExtern:
      c.add Semicolon
    else:
      if vk != IsLocal and not mustMoveToInit: c.objConstrNeedsType = false
      genVarInitValue c, d.value
      if vk != IsLocal and not mustMoveToInit: c.objConstrNeedsType = true

    if vk == IsLocal and c.inToplevel:
      for i in beforeDecl ..< c.code.len:
        c.init.add c.code[i]
      setLen c.code, beforeDecl
      c.add Semicolon
    elif mustMoveToInit and not toExtern:
      c.init.add c.tokens.getOrIncl(name)
      for i in beforeInit ..< c.code.len:
        c.init.add c.code[i]
      setLen c.code, beforeInit
      c.add Semicolon
  else:
    error c.m, "expected SymbolDef but got: ", d.name

include genstmts

proc addOverflowDecl(c: var GeneratedCode; code: var seq[Token]; beforeBody: int) =
  let tokens = @[
    c.tokens.getOrIncl("NB8"),
    Token(Space),
    Token(OvfToken),
    Token(AsgnOpr),
    c.tokens.getOrIncl("NIM_FALSE"),
    Token(Semicolon)
  ]
  code.insert(tokens, beforeBody)

proc genProcDecl(c: var GeneratedCode; n: var Cursor; isExtern: bool) =
  c.m.openScope()
  c.inToplevel = false
  let oldProc = c.currentProc
  c.currentProc = CurrentProc(needsOverflowFlag: false)
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
      of ErrsP, RaisesP, VarargsP, SelectanyP:
        skip p
      of WasP:
        genWasPragma c, p
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
    let beforeBody = c.code.len
    genStmt c, prc.body
    if c.currentProc.needsOverflowFlag:
      addOverflowDecl c, c.code, beforeBody
    c.add CurlyRi
    if isSelectAny in flags:
      genRoutineGuardEnd(c)
  c.m.closeScope()
  c.inToplevel = true
  c.currentProc = oldProc

proc genInclude(c: var GeneratedCode; n: var Cursor) =
  inc n
  let lit = n.litId
  let headerAsStr {.cursor.} = pool.strings[lit]
  let header = c.tokens.getOrIncl(headerAsStr)
  inc n
  if headerAsStr.len > 0 and not c.includedHeaders.containsOrIncl(int header):
    if headerAsStr[0] == '#':
      # keeps the #include statements as they are
      c.includes.add header
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
    if n.kind != ParRi:
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
  of ConstS: genVar c, n, IsConst
  of DiscardS, AsgnS, KeepovfS, ScopeS, IfS,
      WhileS, CaseS, LabS, JmpS, TryS, RaiseS, CallS, OnErrS:
    moveToInitSection:
      genStmt c, n
  of TypeS:
    discard "handled in a different pass"
    skip n
  of EmitS: genEmitStmt c, n
  of StmtsS:
    inc n
    while n.kind != ParRi: genToplevel c, n
    skipParRi n
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
  var c = initGeneratedCode(m, flags, s.bits)
  c.m.openScope()

  var co = TypeOrder()
  traverseTypes(c.m, co)

  generateTypes(c, co)
  let typeDecls = move c.code

  var n = beginRead(c.m.src)
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
  # so that v-tables can be generated protos must be written before data:
  writeTokenSeq f, c.protos, c
  writeTokenSeq f, c.data, c
  writeTokenSeq f, c.code, c

  if gfProducesMainProc in c.flags:
    f.write "int cmdCount;\n"
    f.write "NC8 **cmdLine;\n"
    # Changing argv type other than `char**` results in compile error in clang.
    f.write "int main(int argc, char **argv) {\n"
    f.write "  cmdCount = argc;\n"
    f.write "  cmdLine = (NC8**)argv;\n"
    writeTokenSeq f, c.init, c
    f.write "}\n\n"
  elif c.init.len > 0:
    f.write "static void __attribute__((constructor)) init(void) {"
    if c.currentProc.needsOverflowFlag:
      addOverflowDecl c, c.init, 0
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

  c.m.closeScope()
