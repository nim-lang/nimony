#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# We produce LLVM IR as text (.ll files) so that we are not
# dependent on LLVM's changing C++ API.

import std / [assertions, syncio, sets, intsets, formatfloat, packedsets, strutils, sequtils, tables]
from std / os import changeFileExt, splitFile, extractFilename, fileExists

include ".." / lib / nifprelude
import mangler, nifc_model, noptions, typenav, symparser, nifmodules

type
  LToken = distinct uint32

proc `==`(a, b: LToken): bool {.borrow.}

type
  PredefinedToken = enum
    IgnoreMe = "<unused>"
    EmptyToken = ""
    NewLine = "\n"
    Indent = "  "
    Space = " "
    Comma = ", "
    ColonSpace = ": "
    BrOpen = "{"
    BrClose = "}"
    ParOpen = "("
    ParClose = ")"
    SqOpen = "["
    SqClose = "]"
    Equals = " = "
    AtSign = "@"
    Percent = "%"
    Zeroinit = "zeroinitializer"
    Undef = "undef"
    NullToken = "null"
    VoidToken = "void"
    PtrToken = "ptr"
    I1Token = "i1"
    I8Token = "i8"
    I16Token = "i16"
    I32Token = "i32"
    I64Token = "i64"
    FloatToken = "float"
    DoubleToken = "double"
    Fp128Token = "fp128"
    LoadToken = "load "
    StoreToken = "store "
    AllocaToken = "alloca "
    RetToken = "ret "
    RetVoid = "ret void"
    BrToken = "br "
    BrI1Token = "br i1 "
    LabelToken = "label %"
    CallToken = "call "
    DefineToken = "define "
    DeclareToken = "declare "
    GlobalToken = "global "
    ConstantToken = "constant "
    ExternalToken = "external "
    PrivateToken = "private "
    ThreadLocalToken = "thread_local "
    TypeToken = " = type "
    OpaqueToken = " = type opaque"
    GepToken = "getelementptr inbounds "
    GepTokenNI = "getelementptr "
    IcmpToken = "icmp "
    FcmpToken = "fcmp "
    AddToken = "add "
    SubToken = "sub "
    MulToken = "mul "
    SdivToken = "sdiv "
    UdivToken = "udiv "
    SremToken = "srem "
    UremToken = "urem "
    ShlToken = "shl "
    AshrToken = "ashr "
    LshrToken = "lshr "
    AndToken = "and "
    OrToken = "or "
    XorToken = "xor "
    ZextToken = "zext "
    SextToken = "sext "
    TruncToken = "trunc "
    FpextToken = "fpext "
    FptruncToken = "fptrunc "
    SitofpToken = "sitofp "
    FptosiToken = "fptosi "
    BitcastToken = "bitcast "
    InttoptrToken = "inttoptr "
    PtrtointToken = "ptrtoint "
    InsertvalToken = "insertvalue "
    ExtractvalToken = "extractvalue "
    AtomicrmwToken = "atomicrmw "
    CmpxchgToken = "cmpxchg "
    FenceToken = "fence "
    SeqCstToken = "seq_cst"
    AlwaysInline = " alwaysinline"
    Noinline = " noinline"
    ToToken = " to "
    EntryLabel = "entry:\n"
    CommaI32Zero = ", i32 0"
    CommaI32 = ", i32 "
    FalseI1 = "i1 false"
    ErrGlobal = "@NIFC_ERR_"
    OvfGlobal = "@NIFC_OVF_"

proc fillTokenTable(tab: var BiTable[LToken, string]) =
  for e in EmptyToken..high(PredefinedToken):
    let id = tab.getOrIncl $e
    assert id == LToken(e), $(id, " ", ord(e))

type
  LLVMGenFlag* = enum
    gfMainModule

  LLValue* = object
    name*: LToken  # e.g. "%t5", "@global", "42", "null"
    typ*: LToken   # e.g. "i32", "ptr", "double", "void"

  LLVMCurrentProc* = object
    allocas*: seq[LToken]  # alloca instructions for the entry block
    nextTemp*: int
    nextLabel*: int
    vflags*: HashSet[SymId]
    needsTerminator*: bool  # whether the current basic block needs a terminator
    breakStack*: seq[LToken]  # stack of loop-end labels for `break`
    subprogramId*: int  # metadata ID of the current DISubprogram
    retType*: LToken  # LLVM IR return type token
    retTypeCursor*: Cursor  # NIF type cursor for the return type

  DebugInfo* = object
    nextMetadataId*: int
    metadata*: seq[string]     # accumulated metadata nodes
    fileIds*: Table[int, int]  # FileId (as int) -> DIFile metadata id
    cuId*: int                 # DICompileUnit metadata id

  LLVMCode* = object
    m: MainModule
    tokens: BiTable[LToken, string]
    types*: seq[LToken]       # type declarations
    globals*: seq[LToken]     # global variable declarations
    externs*: seq[LToken]     # external function declarations
    funcBodies*: seq[LToken]  # function definitions
    initBody*: seq[LToken]    # global constructor body
    body*: seq[LToken]        # current function body being built
    bits: int
    flags: set[LLVMGenFlag]
    generatedTypes*: HashSet[SymId]
    requestedSyms*: HashSet[SymId]
    declaredExterns*: HashSet[string] # to avoid duplicate extern declarations
    inToplevel: bool
    currentProc: LLVMCurrentProc
    strLitCounter*: int       # global counter for string literal names
    debug*: DebugInfo

proc initLLVMCode*(m: sink MainModule; flags: set[LLVMGenFlag]; bits: int): LLVMCode =
  result = LLVMCode(m: m, flags: flags, bits: bits, inToplevel: true,
                    tokens: initBiTable[LToken, string]())
  fillTokenTable(result.tokens)

proc error(m: MainModule; msg: string; n: Cursor) {.noreturn.} =
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

# ---- Token helpers ----

proc tok(c: var LLVMCode; s: string): LToken {.inline.} =
  ## Intern a string and return its token.
  c.tokens.getOrIncl(s)

proc str(c: LLVMCode; t: LToken): lent string {.inline.} =
  ## Get the string for a token.
  c.tokens[t]

proc add(c: var LLVMCode; t: PredefinedToken) {.inline, used.} =
  c.body.add LToken(t)

proc add(c: var LLVMCode; t: LToken) {.inline, used.} =
  c.body.add t

proc add(c: var LLVMCode; s: string) {.inline, used.} =
  c.body.add c.tokens.getOrIncl(s)

proc addTo(c: var LLVMCode; dest: var seq[LToken]; t: PredefinedToken) {.inline, used.} =
  dest.add LToken(t)

proc addTo(c: var LLVMCode; dest: var seq[LToken]; s: string) {.inline.} =
  dest.add c.tokens.getOrIncl(s)

proc emit(c: var LLVMCode; s: string) {.used.} =
  c.body.add c.tokens.getOrIncl(s)

proc emitLine(c: var LLVMCode; s: string) =
  c.body.add c.tokens.getOrIncl(s & "\n")

proc temp(c: var LLVMCode): LToken =
  result = c.tokens.getOrIncl("%t" & $c.currentProc.nextTemp)
  inc c.currentProc.nextTemp

proc label(c: var LLVMCode): LToken =
  result = c.tokens.getOrIncl("L" & $c.currentProc.nextLabel)
  inc c.currentProc.nextLabel

proc addAlloca(c: var LLVMCode; name, typ: LToken; align: int64 = 0) =
  var s = "  " & c.str(name) & " = alloca " & c.str(typ)
  if align > 0:
    s.add ", align " & $align
  s.add "\n"
  c.currentProc.allocas.add c.tokens.getOrIncl(s)

proc mangleSym(c: var LLVMCode; s: SymId): string =
  let x = c.m.getDeclOrNil(s)
  if x != nil and x.extern != StrId(0):
    result = pool.strings[x.extern]
  else:
    result = mangleToC(pool.syms[s])

proc symTok(c: var LLVMCode; s: SymId): LToken {.used.} =
  ## Mangle a symbol and return its token.
  c.tok(mangleSym(c, s))

proc localTok(c: var LLVMCode; s: SymId): LToken {.used.} =
  ## Return token for a local variable reference: %name
  c.tok("%" & mangleSym(c, s))

proc globalTok(c: var LLVMCode; s: SymId): LToken {.used.} =
  ## Return token for a global variable reference: @name
  c.tok("@" & mangleSym(c, s))

# ---- Debug info helpers ----

proc addMetadata(c: var LLVMCode; node: string): int =
  ## Add a metadata node, return its ID.
  result = c.debug.nextMetadataId
  c.debug.metadata.add node
  inc c.debug.nextMetadataId

proc initDebugInfo(c: var LLVMCode; filename: string) =
  ## Initialize debug metadata: compile unit and primary file.
  let (dir, name, ext) = splitFile(filename)
  let fullName = name & ext
  let directory = if dir == "": "." else: dir
  let fileId = c.addMetadata("!DIFile(filename: \"" & fullName & "\", directory: \"" & directory & "\")")
  let cuId = c.addMetadata("distinct !DICompileUnit(language: DW_LANG_C99, file: !" & $fileId &
    ", producer: \"nifc\", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)")
  c.debug.cuId = cuId

proc getOrCreateDIFile(c: var LLVMCode; fid: FileId): int =
  ## Get or create a DIFile metadata node for the given FileId.
  let key = int(fid)
  if key in c.debug.fileIds:
    return c.debug.fileIds[key]
  let path = pool.files[fid]
  let (dir, name, ext) = splitFile(path)
  let fullName = name & ext
  let directory = if dir == "": "." else: dir
  result = c.addMetadata("!DIFile(filename: \"" & fullName & "\", directory: \"" & directory & "\")")
  c.debug.fileIds[key] = result

proc dbgLocation(c: var LLVMCode; info: PackedLineInfo): string =
  ## Return a `, !dbg !N` suffix for the given source location, or "" if invalid.
  if not info.isValid: return ""
  let rawInfo = unpack(pool.man, info)
  if not rawInfo.file.isValid: return ""
  let fileId = getOrCreateDIFile(c, rawInfo.file)
  let locId = c.addMetadata("!DILocation(line: " & $rawInfo.line &
    ", column: " & $(rawInfo.col + 1) &
    ", scope: !" & $c.currentProc.subprogramId & ")")
  result = ", !dbg !" & $locId

proc createSubprogram(c: var LLVMCode; name: string; info: PackedLineInfo): int =
  ## Create a DISubprogram metadata node for a function.
  var fileId = 0
  var line = 0
  if info.isValid:
    let rawInfo = unpack(pool.man, info)
    if rawInfo.file.isValid:
      fileId = getOrCreateDIFile(c, rawInfo.file)
      line = rawInfo.line
  let subroutineTypeId = c.addMetadata("!DISubroutineType(types: !{})")
  result = c.addMetadata("distinct !DISubprogram(name: \"" & name &
    "\", scope: !" & $fileId &
    ", file: !" & $fileId &
    ", line: " & $line &
    ", type: !" & $subroutineTypeId &
    ", scopeLine: " & $line &
    ", spFlags: DISPFlagDefinition, unit: !" & $c.debug.cuId & ")")

proc emitLineDbg(c: var LLVMCode; s: string; info: PackedLineInfo) =
  ## Emit an instruction line with debug location metadata attached.
  let dbg = dbgLocation(c, info)
  c.body.add c.tokens.getOrIncl(s & dbg & "\n")

proc extractWasPragma(n: Cursor): string =
  ## Extract the original name from a (was "name") pragma, or return "".
  result = ""
  if n.substructureKind == PragmasU:
    var p = n
    inc p
    while p.kind != ParRi:
      if p.pragmaKind == WasP:
        inc p # enter (was
        if p.kind == StringLit:
          result = pool.strings[p.litId]
        elif p.kind == Ident:
          result = pool.strings[p.litId]
        return
      skip p

proc emitDbgDeclare(c: var LLVMCode; localName: string; wasName: string;
                    info: PackedLineInfo) =
  ## Emit a #dbg_declare for a local variable with its original name.
  if wasName.len == 0: return
  if not info.isValid: return
  let rawInfo = unpack(pool.man, info)
  if not rawInfo.file.isValid: return
  let fileId = getOrCreateDIFile(c, rawInfo.file)
  let varId = c.addMetadata("!DILocalVariable(name: \"" & wasName &
    "\", scope: !" & $c.currentProc.subprogramId &
    ", file: !" & $fileId &
    ", line: " & $rawInfo.line & ")")
  let locId = c.addMetadata("!DILocation(line: " & $rawInfo.line &
    ", column: " & $(rawInfo.col + 1) &
    ", scope: !" & $c.currentProc.subprogramId & ")")
  c.emitLine "  #dbg_declare(ptr " & localName & ", !" & $varId & ", !DIExpression(), !" & $locId & ")"

proc writeTokenSeq(f: var string; s: seq[LToken]; c: LLVMCode) =
  for x in s:
    f.add c.tokens[x]

proc extractAlignValue(pragmas: Cursor): int64 =
  ## Extract the (align N) value from pragmas, or 0 if none.
  result = 0
  if pragmas.substructureKind == PragmasU:
    var p = pragmas.firstSon
    while p.kind != ParRi:
      if p.pragmaKind == AlignP:
        inc p
        result = pool.integers[p.intId]
        return
      skip p

proc extractBitfieldBits(pragmas: Cursor): int64 =
  ## Extract the (bits N) value from field pragmas, or 0 if none.
  result = 0
  if pragmas.substructureKind == PragmasU:
    var p = pragmas.firstSon
    while p.kind != ParRi:
      if p.pragmaKind == BitsP:
        inc p
        result = pool.integers[p.intId]
        return
      skip p

proc baseTypeOfObject*(m: var MainModule; objBody: Cursor): Cursor =
  ## For an object type with inheritance, return the cursor to the base type symbol.
  ## Returns a nil cursor if there's no base type.
  result = default(Cursor)
  if objBody.typeKind == ObjectT:
    var body = objBody
    inc body
    if body.kind == Symbol:
      result = body

# ---- Type generation ----

include llvmgentypes

# ---- Expression generation ----

include llvmgenexprs

# ---- Forward declarations ----

proc genOnErrorLLVM(c: var LLVMCode; n: var Cursor)
proc genStmtLLVM(c: var LLVMCode; n: var Cursor)

# ---- Variable declarations (needed by stmts) ----

proc genVarPragmasLLVM(c: var LLVMCode; n: var Cursor): set[NifcPragma] =
  result = {}
  if n.kind == DotToken:
    inc n
  elif n.substructureKind == PragmasU:
    inc n
    while n.kind != ParRi:
      let pk = n.pragmaKind
      case pk
      of AlignP, AttrP, WasP:
        skip n
      of HeaderP:
        inc n
        if n.kind == StringLit:
          inc n
        else:
          error c.m, "expected string literal in header pragma but got: ", n
        skipParRi n
      of StaticP, ImportcP, ImportcppP, ExportcP, NodeclP:
        result.incl pk
        skip n
      else:
        error c.m, "invalid pragma: ", n
    inc n # ParRi
  else:
    error c.m, "expected pragmas but got: ", n

type
  VarKindLLVM = enum
    IsLocal, IsGlobal, IsThreadlocal, IsConst

proc genGlobalVarDeclLLVM(c: var LLVMCode; n: var Cursor; vk: VarKindLLVM; toExtern = false) =
  var d = takeVarDecl(n)
  if d.name.kind == SymbolDef:
    let lit = d.name.symId
    c.m.registerLocal(lit, d.typ)

    var externName = StrId(0)
    var isImport = false
    var isNodecl = false
    let alignVal = extractAlignValue(d.pragmas)
    if d.pragmas.substructureKind == PragmasU:
      var p = d.pragmas.firstSon
      while p.kind != ParRi:
        case p.pragmaKind
        of ImportcP, ImportcppP:
          externName = nifmodules.externName(lit, p)
          isImport = true
        of ExportcP:
          externName = nifmodules.externName(lit, p)
        of NodeclP:
          isNodecl = true
        of HeaderP:
          discard # ignored for LLVM backend
        else: discard
        skip p

    let flags = genVarPragmasLLVM(c, d.pragmas)
    if isNodecl and isImport and externName != StrId(0):
      # C preprocessor constants (e.g. __ATOMIC_*) don't exist as LLVM symbols;
      # emit as private constants with known values
      let extName = pool.strings[externName]
      var t = d.typ
      let typ = genTypeLLVM(c, t)
      case extName
      of "__ATOMIC_RELAXED":
        c.addTo(c.globals, "@" & extName & " = private constant " & typ & " 0\n")
      of "__ATOMIC_CONSUME":
        c.addTo(c.globals, "@" & extName & " = private constant " & typ & " 1\n")
      of "__ATOMIC_ACQUIRE":
        c.addTo(c.globals, "@" & extName & " = private constant " & typ & " 2\n")
      of "__ATOMIC_RELEASE":
        c.addTo(c.globals, "@" & extName & " = private constant " & typ & " 3\n")
      of "__ATOMIC_ACQ_REL":
        c.addTo(c.globals, "@" & extName & " = private constant " & typ & " 4\n")
      of "__ATOMIC_SEQ_CST":
        c.addTo(c.globals, "@" & extName & " = private constant " & typ & " 5\n")
      else:
        discard
      skip d.value
      return

    let name = if externName != StrId(0): pool.strings[externName]
               else: mangleToC(pool.syms[lit])

    var t = d.typ
    let typ = genTypeLLVM(c, t)

    let alignSuffix = if alignVal > 0: ", align " & $alignVal else: ""
    let tls = if vk == IsThreadlocal: "thread_local " else: ""
    if toExtern or isImport:
      c.addTo(c.globals, "@" & name & " = external " & tls & "global " & typ & alignSuffix & "\n")
    else:
      var initVal = if typ == "ptr": "null" else: "zeroinitializer"
      if d.value.kind != DotToken:
        var v = d.value
        initVal = genConstantLLVM(c, v, typ)
      else:
        skip d.value

      let linkage = if vk == IsConst: "constant" else: "global"
      c.addTo(c.globals, "@" & name & " = " & tls & linkage & " " & typ & " " & initVal & alignSuffix & "\n")
  else:
    error c.m, "expected SymbolDef but got: ", d.name

proc genLocalVarDeclLLVM(c: var LLVMCode; n: var Cursor) =
  let varInfo = n.info
  var d = takeVarDecl(n)
  if d.name.kind == SymbolDef:
    let lit = d.name.symId
    c.m.registerLocal(lit, d.typ)

    let wasName = extractWasPragma(d.pragmas)
    let alignVal = extractAlignValue(d.pragmas)
    let flags = genVarPragmasLLVM(c, d.pragmas)
    if NodeclP in flags:
      skip d.value
      return

    let name = mangleToC(pool.syms[lit])
    var t = d.typ
    let typ = genTypeLLVM(c, t)
    let localName = "%" & name
    c.addAlloca(c.tok(localName), c.tok(typ), alignVal)

    emitDbgDeclare(c, localName, wasName, varInfo)

    if d.value.kind != DotToken:
      if d.value.stmtKind == OnErrS:
        var onErr = d.value
        inc onErr
        var onErrAction = onErr
        var val = LLValue(); genCallExprLLVM(c, d.value, val)
        c.emitLineDbg "  store " & c.str(val.typ) & " " & c.str(val.name) & ", ptr " & localName, varInfo
        if onErrAction.kind != DotToken:
          genOnErrorLLVM(c, onErrAction)
      else:
        var val = LLValue(); genExprLLVM(c, d.value, val)
        c.emitLineDbg "  store " & c.str(val.typ) & " " & c.str(val.name) & ", ptr " & localName, varInfo
    else:
      inc d.value
      let zeroVal = if typ == "ptr": "null" else: "zeroinitializer"
      c.emitLineDbg "  store " & typ & " " & zeroVal & ", ptr " & localName, varInfo
  else:
    error c.m, "expected SymbolDef but got: ", d.name

# ---- Statement generation ----

include llvmgenstmts

# ---- Proc and toplevel generation ----

type
  PragmaInfo = object
    flags: set[NifcPragma]
    extern: StrId
    callConv: CallConv
    wasName: string  # original proc name from (was ...) pragma

proc parseProcPragmasLLVM(c: var LLVMCode; n: var Cursor): PragmaInfo =
  result = PragmaInfo()
  if n.kind == DotToken:
    inc n
  elif n.substructureKind == PragmasU:
    inc n
    while n.kind != ParRi:
      let pk = n.pragmaKind
      case pk
      of NoPragma, AlignP, BitsP, VectorP, StaticP, PackedP:
        if n.callConvKind != NoCallConv:
          result.callConv = n.callConvKind
          skip n
        else:
          error c.m, "invalid proc pragma: ", n
      of NodeclP:
        result.flags.incl NodeclP
        skip n
      of ImportcppP, ImportcP, ExportcP:
        inc n
        if n.kind == StringLit:
          result.extern = n.litId
          inc n
        result.flags.incl pk
        skipParRi n
      of HeaderP:
        inc n
        if n.kind != StringLit:
          error c.m, "expected string literal in header pragma but got: ", n
        else:
          # LLVM doesn't need header includes, but we still consume the token
          inc n
        skipParRi n
      of SelectanyP:
        result.flags.incl pk
        skip n
      of WasP:
        inc n
        result.wasName = toString(n, false)
        skip n
        skipParRi n
      of ErrsP, RaisesP:
        skip n
      of InlineP:
        result.flags.incl pk
        skip n
      of NoinlineP:
        result.flags.incl pk
        skip n
      of AttrP:
        skip n # ignore attributes for now
    inc n # ParRi
  else:
    error c.m, "expected proc pragmas but got: ", n

proc genSymDefLLVM(c: var LLVMCode; n: Cursor; prag: PragmaInfo): string =
  if n.kind == SymbolDef:
    let lit = n.symId
    if {ImportcP, ImportcppP, ExportcP} * prag.flags != {}:
      if prag.extern != StrId(0):
        result = pool.strings[prag.extern]
      else:
        result = pool.syms[lit]
        extractBasename(result)
    else:
      result = mangleToC(pool.syms[lit])
  else:
    result = ""
    error c.m, "expected SymbolDef but got: ", n

proc genParamPragmasLLVM(c: var LLVMCode; n: var Cursor) =
  if n.kind == DotToken:
    inc n
  elif n.substructureKind == PragmasU:
    inc n
    while n.kind != ParRi:
      case n.pragmaKind
      of AttrP, WasP:
        skip n
      else:
        error c.m, "invalid pragma: ", n
    inc n # ParRi
  else:
    error c.m, "expected pragmas but got: ", n

proc callingConvToLLVM(cc: CallConv): string =
  case cc
  of NoCallConv, Nimcall, Noconv, Member: ""
  of Cdecl: "ccc"
  of Stdcall: "x86_stdcallcc"
  of Safecall: "x86_stdcallcc"
  of Syscall: "ccc"
  of Fastcall: "x86_fastcallcc"
  of Thiscall: "x86_thiscallcc"

proc genProcDeclLLVM(c: var LLVMCode; n: var Cursor; isExtern: bool) =
  c.m.openScope()
  c.inToplevel = false
  let oldProc = c.currentProc
  c.currentProc = LLVMCurrentProc(nextTemp: 0, nextLabel: 0, needsTerminator: false)

  let procInfo = n.info
  var prc = takeProcDecl(n)
  let prag = parseProcPragmasLLVM(c, prc.pragmas)

  let name = genSymDefLLVM(c, prc.name, prag)

  # Determine return type
  var retType: string
  if prc.returnType.kind == DotToken:
    retType = "void"
  else:
    var rt = prc.returnType
    retType = genTypeLLVM(c, rt)

  # Generate parameter list
  var paramTypes: seq[string] = @[]
  var paramNames: seq[string] = @[]
  var paramWasNames: seq[string] = @[]
  if prc.params.kind != DotToken:
    var p = prc.params.firstSon
    while p.kind != ParRi:
      assert p.substructureKind == ParamU
      var d = takeParamDecl(p)
      if d.name.kind == SymbolDef:
        let s = d.name.symId
        c.m.registerLocal(s, d.typ)
        var t = d.typ
        let paramType = genTypeLLVM(c, t)
        paramTypes.add paramType
        let paramName = mangleToC(pool.syms[s])
        paramNames.add paramName
        paramWasNames.add extractWasPragma(d.pragmas)
        genParamPragmasLLVM(c, d.pragmas)
      else:
        error c.m, "expected SymbolDef but got: ", d.name
    # skip the ParRi of params
    discard

  if {NodeclP, HeaderP} * prag.flags != {}:
    # Don't generate anything for nodecl/header-only procs
    discard
  elif isExtern or {ImportcP, ImportcppP} * prag.flags != {}:
    # External declaration
    let externName = name
    if externName notin c.declaredExterns:
      c.declaredExterns.incl externName
      var decl = "declare " & retType & " @" & externName & "("
      for i, pt in paramTypes:
        if i > 0: decl.add ", "
        decl.add pt
      decl.add ")\n"
      c.addTo(c.externs, decl)
  else:
    # Function definition
    let displayName = if prag.wasName.len > 0: prag.wasName else: name
    let spId = createSubprogram(c, displayName, procInfo)
    c.currentProc.subprogramId = spId

    let ccStr = callingConvToLLVM(prag.callConv)
    var funcHeader = "define "
    if ccStr != "":
      funcHeader.add ccStr & " "
    funcHeader.add retType & " @" & name & "("
    for i, pt in paramTypes:
      if i > 0: funcHeader.add ", "
      funcHeader.add pt & " %" & paramNames[i] & ".param"
    funcHeader.add ")"
    if InlineP in prag.flags:
      funcHeader.add " alwaysinline"
    if NoinlineP in prag.flags:
      funcHeader.add " noinline"
    funcHeader.add " !dbg !" & $spId
    funcHeader.add " {\n"
    funcHeader.add "entry:\n"

    c.body = @[]
    c.currentProc.allocas = @[]
    c.currentProc.needsTerminator = false
    c.currentProc.retType = c.tok(retType)
    c.currentProc.retTypeCursor = prc.returnType

    # Alloca for each parameter and store the param value
    for i, pn in paramNames:
      let allocaName = "%" & pn
      c.addAlloca(c.tok(allocaName), c.tok(paramTypes[i]))
      c.emitLine "  store " & paramTypes[i] & " %" & pn & ".param, ptr " & allocaName
      emitDbgDeclare(c, allocaName, paramWasNames[i], procInfo)

    # Generate body
    genStmtLLVM c, prc.body

    # Add implicit return if needed
    if c.currentProc.needsTerminator:
      discard "block already terminated"
    else:
      if retType == "void":
        c.emitLine "  ret void"
      else:
        let zeroVal = if retType == "ptr": "null" else: "zeroinitializer"
        c.emitLine "  ret " & retType & " " & zeroVal

    # Assemble function: header string + alloca tokens + body tokens + closing
    var funcDef: string = funcHeader
    for a in c.currentProc.allocas:
      funcDef.add c.tokens[a]
    for tok in c.body:
      funcDef.add c.tokens[tok]
    funcDef.add "}\n\n"
    c.addTo(c.funcBodies, funcDef)

  c.m.closeScope()
  c.inToplevel = true
  c.currentProc = oldProc

proc genImportedSymsLLVM(c: var LLVMCode) =
  while true:
    let fsyms = move c.m.requestedForeignSyms
    if fsyms.len == 0: break
    for fsym in fsyms:
      var n = fsym
      case fsym.stmtKind
      of ProcS:
        genProcDeclLLVM c, n, true
      of VarS:
        discard
      of GvarS:
        genGlobalVarDeclLLVM c, n, IsGlobal, true
      of TvarS:
        genGlobalVarDeclLLVM c, n, IsThreadlocal, true
      of ConstS:
        genGlobalVarDeclLLVM c, n, IsConst, true
      else:
        discard

proc genToplevelLLVM(c: var LLVMCode; n: var Cursor) =
  case n.stmtKind
  of ProcS: genProcDeclLLVM c, n, false
  of GvarS:
    genGlobalVarDeclLLVM c, n, IsGlobal
  of TvarS:
    genGlobalVarDeclLLVM c, n, IsThreadlocal
  of ConstS:
    genGlobalVarDeclLLVM c, n, IsConst
  of VarS:
    # Toplevel local vars go into the init function
    genGlobalVarDeclLLVM c, n, IsGlobal
  of TypeS:
    discard "handled in a different pass"
    skip n
  of EmitS:
    # Emit statements don't make sense for LLVM IR; skip them
    skip n
  of DiscardS, AsgnS, KeepovfS, ScopeS, IfS,
      WhileS, CaseS, LabS, JmpS, TryS, RaiseS, CallS, OnErrS:
    # These go into the global init function
    var oldBody = move c.body
    var oldProc = c.currentProc
    c.body = @[]
    c.currentProc = LLVMCurrentProc(nextTemp: c.currentProc.nextTemp)
    genStmtLLVM c, n
    for line in c.body:
      c.initBody.add line
    c.body = move oldBody
    c.currentProc = oldProc
  of StmtsS:
    inc n
    while n.kind != ParRi: genToplevelLLVM c, n
    skipParRi n
  else:
    error c.m, "expected top level construct but got: ", n

proc traverseCodeLLVM(c: var LLVMCode; n: var Cursor) =
  if n.stmtKind == StmtsS:
    inc n
    while n.kind != ParRi: genToplevelLLVM(c, n)
    genImportedSymsLLVM c
  else:
    error c.m, "expected `stmts` but got: ", n

proc generateLLVMTypes(c: var LLVMCode) =
  # Generate type declarations for all types in the module
  var co = TypeOrderLLVM()
  traverseTypesLLVM(c.m, co)
  for (d, isForward) in co.ordered:
    var n = d
    let decl = takeTypeDecl(n)
    if not c.generatedTypes.containsOrIncl(decl.name.symId):
      var skipDecl = false
      var packed = false
      # Check for nodecl/header/packed pragmas
      if decl.pragmas.substructureKind == PragmasU:
        var p = decl.pragmas.firstSon
        while p.kind != ParRi:
          case p.pragmaKind
          of NodeclP, HeaderP:
            skipDecl = true
          of PackedP:
            packed = true
          else: discard
          skip p
      if skipDecl: continue

      let name = mangleToC(pool.syms[decl.name.symId])
      if isForward:
        c.addTo(c.types, "%" & name & " = type opaque\n")
      else:
        var body = decl.body
        let typeDef = genTypeDefLLVM(c, body, name, packed)
        if typeDef != "":
          c.addTo(c.types, typeDef)

proc generateLLVMCode*(s: var State, inp, outp: string; flags: set[LLVMGenFlag]) =
  var m = load(inp)
  m.config = s.config
  var c = initLLVMCode(m, flags, s.bits)
  c.m.openScope()

  # Initialize debug info
  initDebugInfo(c, inp)

  # First pass: traverse code to discover types and generate functions
  var n = beginRead(c.m.src)
  traverseCodeLLVM c, n

  # Generate type declarations
  generateLLVMTypes c

  # Assemble output
  var f: string = ""
  f.add "; LLVM IR generated by NIFC\n"
  f.add "target datalayout = \"e-m:o-i64:64-i128:128-n32:64-S128\"\n"
  when defined(macos):
    f.add "target triple = \"arm64-apple-macosx\"\n"
  else:
    f.add "; target triple should be set for your platform\n"
  f.add "\n"

  # Type declarations
  writeTokenSeq f, c.types, c
  if c.types.len > 0: f.add "\n"

  # External declarations
  writeTokenSeq f, c.externs, c
  if c.externs.len > 0: f.add "\n"

  # Global variables
  writeTokenSeq f, c.globals, c
  if c.globals.len > 0: f.add "\n"

  # Error flag for main module
  if gfMainModule in c.flags:
    f.add "@NIFC_ERR_ = thread_local global i8 0\n\n"

  # Function bodies
  writeTokenSeq f, c.funcBodies, c

  # Global constructor (init function)
  if c.initBody.len > 0:
    f.add "define internal void @nifc_init() {\n"
    f.add "entry:\n"
    writeTokenSeq f, c.initBody, c
    f.add "  ret void\n"
    f.add "}\n\n"
    # Register as global constructor
    f.add "@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @nifc_init, ptr null }]\n"

  # Debug metadata
  if c.debug.metadata.len > 0:
    let dwarfId = c.addMetadata("!{i32 7, !\"Dwarf Version\", i32 4}")
    let diVersionId = c.addMetadata("!{i32 2, !\"Debug Info Version\", i32 3}")
    f.add "\n"
    f.add "!llvm.dbg.cu = !{!" & $c.debug.cuId & "}\n"
    f.add "!llvm.module.flags = !{!" & $dwarfId & ", !" & $diVersionId & "}\n"
    for i, md in c.debug.metadata:
      f.add "!" & $i & " = " & md & "\n"

  if fileExists(outp) and readFile(outp) == f:
    discard "unchanged, keep mtime for incremental builds"
  else:
    writeFile outp, f

  c.m.closeScope()
