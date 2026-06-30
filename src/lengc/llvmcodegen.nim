#
#
#           Leng Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# We produce LLVM IR via a structured model (llvmirmodel.nim) which a
# serializer (llvmserializer.nim) renders to .ll text. All LLVM syntax
# knowledge lives in the serializer.

import std / [assertions, syncio, sets, intsets, formatfloat, packedsets,
    strutils, sequtils, tables]
from std / os import changeFileExt, splitFile, extractFilename, fileExists,
    getCurrentDir, absolutePath
import ".." / lib / vfs

import ".." / lib / nifcoreparse # re-exports nifcore
import ".." / lib / nifcdecl # leng_model replacement
import mangler
import noptions
import ".." / lib / symparser
import typenav, nifmodules # nifcore MainModule + getType (local)
import llvmirmodel, llvmserializer

# nifcore compatibility shims (see shoggoth/codegen.nim for rationale).
proc litId(c: Cursor): StrId {.inline.} = strId(c)
proc info(c: Cursor): NifLineInfo {.inline.} = rawLineInfo(c)
proc firstSon(c: Cursor): Cursor {.inline.} =
  result = c
  inc result
proc toString(c: Cursor; spaces: bool): string =
  var buf = createTokenBuf(8, c.pool, c.tags)
  buf.addSubtree c
  result = nifcoreparse.toString(buf)

const InitFuncIdx* = -1

type
  LLVMGenFlag* = enum
    gfMainModule

  LLVMCurrentProc* = object
    funcIdx*: int            # index into module.funcs (InitFuncIdx for init func)
    dbgLoc*: string          # current debug location for next instruction
    needsTerminator*: bool
    breakStack*: seq[string] # loop-end labels for `break`
    vflags*: HashSet[SymId]
    retType*: LLType
    retTypeCursor*: Cursor
    spName*: string
    spLine*: int
    spScopeLine*: int
    subprogramId*: int
    subprogramFileId*: int
    retainedNodes*: seq[int]

  DebugInfo* = object
    nextMetadataId*: int
    metadata*: seq[string]                # accumulated metadata nodes
    fileIds*: Table[int, int]             # FileId (as int) -> DIFile metadata id
    cuId*: int                            # DICompileUnit metadata id
    diTypeCache*: Table[SymId, int]       # SymId -> DIType metadata id
    diBasicTypeCache*: Table[string, int] # "i32"/"float"/… -> DIBasicType id
    compositeTypeDone*: HashSet[SymId]    # symIds with fully built DICompositeType
    globalExprs*: seq[int] # DIGlobalVariableExpression IDs for DICompileUnit globals

const
  DW_ATE_address = 1
  DW_ATE_boolean = 2
  DW_ATE_float = 4
  DW_ATE_signed = 5
  DW_ATE_signed_char = 6
  DW_ATE_unsigned = 7
  DW_ATE_unsigned_char = 8

type
  LLVMCode* = object
    m: MainModule
    module*: LLModule
    currentBlockIdx*: int
    nextTempCounter*: int
    nextLabelCounter*: int
    typeCache*: Table[SymId, LLType]
    primVoid*: LLType
    primI1*: LLType
    primI8*: LLType
    primI16*: LLType
    primI32*: LLType
    primI64*: LLType
    primPtr*: LLType
    primFloat*: LLType
    primDouble*: LLType
    bits: int
    flags: set[LLVMGenFlag]
    generatedTypes*: HashSet[SymId]
    requestedSyms*: HashSet[SymId]
    declaredExterns*: HashSet[string]        # to avoid duplicate extern declarations
    varargsFuncTypes*: Table[string, string] # "@name" -> function-type-signature
    emittedConsts*: HashSet[SymId]           # local consts emitted as global constants
    inToplevel: bool
    currentProc: LLVMCurrentProc
    strLitCounter*: int                      # global counter for string literal names
    debug*: DebugInfo

proc initLLVMCode*(m: sink MainModule; flags: set[LLVMGenFlag];
    bits: int): LLVMCode =
  result = LLVMCode(m: m, flags: flags, bits: bits, inToplevel: true)
  result.primVoid = newLLVoidType()
  result.primI1 = newLLIntType(1)
  result.primI8 = newLLIntType(8)
  result.primI16 = newLLIntType(16)
  result.primI32 = newLLIntType(32)
  result.primI64 = newLLIntType(64)
  result.primPtr = newLLPtrType()
  result.primFloat = newLLFloatType(32)
  result.primDouble = newLLFloatType(64)

template llVoid*(c: LLVMCode): LLType = c.primVoid
template llI1*(c: LLVMCode): LLType = c.primI1
template llI8*(c: LLVMCode): LLType = c.primI8
template llI16*(c: LLVMCode): LLType = c.primI16
template llI32*(c: LLVMCode): LLType = c.primI32
template llI64*(c: LLVMCode): LLType = c.primI64
template llPtr*(c: LLVMCode): LLType = c.primPtr
template llFloat*(c: LLVMCode): LLType = c.primFloat
template llDouble*(c: LLVMCode): LLType = c.primDouble

proc llIntBits*(c: LLVMCode; n: int): LLType =
  case n
  of 1: c.primI1
  of 8: c.primI8
  of 16: c.primI16
  of 32: c.primI32
  of 64: c.primI64
  else: newLLIntType(n)

proc error(m: MainModule; msg: string; n: Cursor) {.noreturn.} =
  let info = rawLineInfo(n)
  if info.isValid:
    write stdout, m.pool.filenames[info.file]
    write stdout, "(" & $info.line & ", " & $(info.col+1) & ") "
  write stdout, "[Error] "
  write stdout, msg
  writeLine stdout, toString(n, false)
  when defined(debug):
    echo getStackTrace()
  quit 1

# ---- Block / emission helpers ----

proc dbgLocation(c: var LLVMCode; info: NifLineInfo): string

proc currentFuncIdx(c: LLVMCode): int {.inline.} = c.currentProc.funcIdx

proc funcByIndex(c: var LLVMCode; idx: int): var LLFunc {.inline.} =
  if idx == InitFuncIdx:
    result = c.module.initFunc
  else:
    result = c.module.funcs[idx]

template currentFunc(c: var LLVMCode): var LLFunc =
  funcByIndex(c, c.currentProc.funcIdx)

template currentBlock(c: var LLVMCode): var LLBlock =
  currentFunc(c).blocks[c.currentBlockIdx]

proc nextTemp*(c: var LLVMCode): string {.inline.} =
  result = "t" & $c.nextTempCounter
  inc c.nextTempCounter

proc nextLabel*(c: var LLVMCode): string {.inline.} =
  result = "L" & $c.nextLabelCounter
  inc c.nextLabelCounter

proc newBlock*(c: var LLVMCode; label: string): int {.inline.} =
  ## Append a new block to the current function, return its index.
  currentFunc(c).blocks.add LLBlock(label: label)
  result = currentFunc(c).blocks.high

proc switchToBlock*(c: var LLVMCode; idx: int) {.inline.} =
  c.currentBlockIdx = idx

proc startBlock*(c: var LLVMCode; label: string): int {.inline.} =
  ## Create a new block, switch to it, reset terminator state. Returns index.
  result = c.newBlock(label)
  c.currentBlockIdx = result
  c.currentProc.needsTerminator = false

proc setLoc*(c: var LLVMCode; info: NifLineInfo) =
  c.currentProc.dbgLoc = dbgLocation(c, info)

const
  Terminators = {llBr, llCondBr, llSwitch, llRet, llUnreachable}

proc emit*(c: var LLVMCode; instr: sink LLInstr) =
  instr.dbgLoc = c.currentProc.dbgLoc
  currentBlock(c).instrs.add instr
  if instr.kind in Terminators:
    c.currentProc.needsTerminator = true

proc emitRaw*(c: var LLVMCode; text: string) =
  var instr = LLInstr(kind: llRaw, rawText: text)
  instr.dbgLoc = c.currentProc.dbgLoc
  currentBlock(c).instrs.add instr

proc emitAlloca*(c: var LLVMCode; name: string; typ: LLType; align: int64 = 0) =
  var instr = LLInstr(kind: llAlloca, result: llReg(name, c.primPtr),
                      allocaType: typ, allocaAlign: int align)
  currentFunc(c).entryAllocas.add instr

proc emitGEP*(c: var LLVMCode; baseType: LLType; base: LLValue;
              indices: openArray[LLValue]; inbounds = true): LLValue =
  let t = c.nextTemp()
  result = llReg(t, c.primPtr)
  var idxs: seq[LLValue] = @[]
  for x in indices: idxs.add x
  c.emit LLInstr(kind: llGep, result: result, gepBaseType: baseType,
                 gepBase: base, gepIndices: idxs, gepInbounds: inbounds)

proc emitLoad*(c: var LLVMCode; ptrVal: LLValue; typ: LLType): LLValue =
  let t = c.nextTemp()
  result = llReg(t, typ)
  c.emit LLInstr(kind: llLoad, result: result, loadPtr: ptrVal)

proc emitStore*(c: var LLVMCode; value, ptrVal: LLValue) =
  c.emit LLInstr(kind: llStore, storeValue: value, storePtr: ptrVal)

proc llIntConst*(c: var LLVMCode; val: BiggestInt;
    typ: LLType): LLValue {.inline.} =
  LLValue(kind: llvInt, intText: $val, typ: typ)

proc llIntTextC*(text: string; typ: LLType): LLValue {.inline.} =
  LLValue(kind: llvInt, intText: text, typ: typ)

# ---- Type / value helpers ----

proc typeEq*(a, b: LLType): bool {.inline.} =
  ## Structural type equality. Fast path on pointer identity for interned types.
  if system.`==`(a, b): return true
  if a.isNil or b.isNil: return false
  serialize(a) == serialize(b)

proc withType*(v: LLValue; typ: LLType): LLValue {.inline.} =
  result = v
  result.typ = typ

proc disp*(v: LLValue): string {.inline.} =
  ## Display text of a value (for building callee names / signatures).
  serializeUnqualified(v)

proc mangleSym(c: var LLVMCode; s: SymId): string =
  let x = c.m.getDeclOrNil(s)
  if x != nil and x.extern != StrId(0):
    result = c.m.pool.strings[x.extern]
  else:
    result = mangleToC(c.m.pool.syms[s])

proc nifSymBaseName*(c: var LLVMCode; symId: SymId): string =
  ## Extract the original Nim identifier from a NIF symbol like
  ## ``myVar.0.module`` → ``myVar``.
  let full = c.m.pool.syms[symId]
  var isGlobal = false
  result = extractBasename(full, isGlobal)
  if result.len == 0:
    result = full

# ---- Debug info helpers ----

proc addMetadata(c: var LLVMCode; node: string): int =
  ## Add a metadata node, return its ID.
  result = c.debug.nextMetadataId
  c.debug.metadata.add node
  inc c.debug.nextMetadataId

proc initDebugInfo(c: var LLVMCode; filename: string) =
  ## Debug metadata is initialized lazily: the first call to
  ## ``getOrCreateDIFile`` creates the DICompileUnit using the first
  ## real source file as its file reference.
  discard

proc bitsFromLLVMType(typ: LLType; defaultBits: int): int =
  if typ != nil and typ.kind == llInt: typ.intBits
  else: defaultBits

proc genDIBasicType(c: var LLVMCode; name: string; sizeBits,
    encoding: int): int =
  ## Get or create a DIBasicType metadata node. Cached by name+size+encoding.
  let key = name & ":" & $sizeBits & ":" & $encoding
  result = c.debug.diBasicTypeCache.getOrDefault(key)
  if result != 0: return
  result = c.addMetadata("!DIBasicType(name: \"" & name &
    "\", size: " & $sizeBits &
    ", encoding: " & $encoding & ")")
  c.debug.diBasicTypeCache[key] = result

proc getOrCreateDIFile(c: var LLVMCode; fid: FileId): int =
  ## Get or create a DIFile metadata node for the given FileId.
  let key = int(fid)
  if key in c.debug.fileIds:
    return c.debug.fileIds[key]
  let path = c.m.pool.filenames[fid]
  let (dir, name, ext) = splitFile(path)
  let fullName = name & ext
  let directory = if dir == "": getCurrentDir() else: absolutePath(dir)
  result = c.addMetadata("!DIFile(filename: \"" & fullName &
      "\", directory: \"" & directory & "\")")
  c.debug.fileIds[key] = result
  # First real source file → create the compile unit using this file
  if c.debug.cuId == 0:
    c.debug.cuId = c.addMetadata("distinct !DICompileUnit(language: DW_LANG_C99, file: !" &
      $result & ", producer: \"lengc\", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)")

proc dbgLocation(c: var LLVMCode; info: NifLineInfo): string =
  ## Return a `, !dbg !N` suffix for the given source location, or "" if invalid.
  if not info.isValid: return ""
  let rawInfo = info
  if not rawInfo.file.isValid: return ""
  let fileId = getOrCreateDIFile(c, rawInfo.file)
  let scopeId =
    if fileId == c.currentProc.subprogramFileId:
      c.currentProc.subprogramId
    else:
      c.addMetadata("!DILexicalBlockFile(scope: !" &
          $c.currentProc.subprogramId &
        ", file: !" & $fileId & ", discriminator: 0)")
  let locId = c.addMetadata("!DILocation(line: " & $rawInfo.line &
    ", column: " & $(rawInfo.col + 1) &
    ", scope: !" & $scopeId & ")")
  result = ", !dbg !" & $locId

proc createSubprogram(c: var LLVMCode; name: string; info: NifLineInfo): int =
  ## Create a DISubprogram metadata node for a function.
  var fileId = 0
  var line = 0
  if info.isValid:
    let rawInfo = info
    if rawInfo.file.isValid:
      fileId = getOrCreateDIFile(c, rawInfo.file)
      line = rawInfo.line
  c.currentProc.spName = name
  c.currentProc.spLine = line
  c.currentProc.spScopeLine = line
  c.currentProc.subprogramFileId = fileId
  # placeholder (content replaced later by finalizeSubprogram)
  result = c.addMetadata("")

proc finalizeSubprogram(c: var LLVMCode; spId: int;
                         paramDITypes, retainedNodes: seq[int]) =
  if spId == 0: return
  let fid = c.currentProc.subprogramFileId
  var typesStr = ""
  for i, pt in paramDITypes:
    if i > 0: typesStr.add ", "
    typesStr.add "!" & $pt
  let stId = c.addMetadata("!DISubroutineType(types: !{" & typesStr & "})")
  var rnList = ""
  for i, rn in retainedNodes:
    if i > 0: rnList.add ", "
    rnList.add "!" & $rn
  let rnId = c.addMetadata("!{" & rnList & "}")
  let finalSp = "distinct !DISubprogram(name: \"" & c.currentProc.spName &
    "\", scope: !" & $fid &
    ", file: !" & $fid &
    ", line: " & $c.currentProc.spLine &
    ", type: !" & $stId &
    ", scopeLine: " & $c.currentProc.spScopeLine &
    ", spFlags: DISPFlagDefinition, unit: !" & $c.debug.cuId &
    ", retainedNodes: !" & $rnId & ")"
  c.debug.metadata[spId] = finalSp

proc extractWasPragma(n: Cursor): string =
  result = ""
  if n.substructureKind == PragmasU:
    var p = n
    p.loopInto:
      if p.pragmaKind == WasP:
        p.into:
          if p.kind in {StrLit, Ident}:
            result = strVal(p)
          while p.hasMore: skip p
        return
      skip p

proc emitDbgDeclare(c: var LLVMCode; localName: string; symId: SymId;
                    wasName: string; info: NifLineInfo; diType: int = 0;
                    argNo: int = 0; llvmTyp: LLType = nil) =
  ## Emit a #dbg_declare for a local variable (as a raw intrinsic line).
  if not info.isValid: return
  let rawInfo = info
  if not rawInfo.file.isValid: return
  var useType = diType
  if useType == 0:
    let bits = bitsFromLLVMType(llvmTyp, c.bits)
    useType = genDIBasicType(c, "int " & $bits, bits, DW_ATE_signed)
  let debugName = if wasName.len > 0: wasName else: nifSymBaseName(c, symId)
  let fileId = getOrCreateDIFile(c, rawInfo.file)
  var varMetadata = "!DILocalVariable(name: \"" & debugName & "\""
  if argNo > 0:
    varMetadata.add ", arg: " & $argNo
  varMetadata.add ", scope: !" & $c.currentProc.subprogramId &
    ", file: !" & $fileId &
    ", line: " & $rawInfo.line &
    ", type: !" & $useType & ")"
  let varId = c.addMetadata(varMetadata)
  c.currentProc.retainedNodes.add varId
  let locId = c.addMetadata("!DILocation(line: " & $rawInfo.line &
    ", column: " & $(rawInfo.col + 1) &
    ", scope: !" & $c.currentProc.subprogramId & ")")
  c.emitRaw "#dbg_declare(ptr " & localName & ", !" & $varId &
      ", !DIExpression(), !" & $locId & ")"

proc extractAlignValue(pragmas: Cursor): int64 =
  result = 0
  if pragmas.substructureKind == PragmasU:
    var p = pragmas
    p.loopInto:
      if p.pragmaKind == AlignP:
        p.into:
          result = intVal(p)
          while p.hasMore: skip p
        return
      skip p

proc extractBitfieldBits(pragmas: Cursor): int64 =
  result = 0
  if pragmas.substructureKind == PragmasU:
    var p = pragmas
    p.loopInto:
      if p.pragmaKind == BitsP:
        p.into:
          result = intVal(p)
          while p.hasMore: skip p
        return
      skip p

proc baseTypeOfObject*(m: var MainModule; objBody: Cursor): Cursor =
  ## For an object type with inheritance, return the cursor to the base type symbol.
  result = default(Cursor)
  if objBody.typeKind == ObjectT:
    var body = objBody
    inc body
    if body.kind == Symbol:
      result = body

# ---- Forward declarations (visible to included files) ----

proc genStmtLLVM(c: var LLVMCode; n: var Cursor)
proc genExprLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue)
proc genLvalueLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue)
proc genCondLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue)
proc genOnErrorLLVM(c: var LLVMCode; n: var Cursor)
proc genTypeLLVM(c: var LLVMCode; n: var Cursor): LLType
proc genTypeLLVMReadOnly(c: var LLVMCode; n: Cursor): LLType
proc genDITypeReadOnly(c: var LLVMCode; n: Cursor): int
proc genGlobalConstr(c: var LLVMCode; n: var Cursor;
    declaredType: Cursor): LLValue
proc coerceValueLLVM(c: var LLVMCode; val: LLValue; srcTypeCursor, destTypeCursor: Cursor;
                     isCast: bool; result: var LLValue)
proc genCallWithType(c: var LLVMCode; n: var Cursor; retType: LLType;
    result: var LLValue)
proc genCallExprLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue)

# ---- Type generation ----

include llvmgentypes

# ---- DWARF debug type generation ----

include llvmditypes

# ---- Expression generation ----

include llvmgenexprs

# ---- Variable declarations (needed by stmts) ----

proc genVarPragmasLLVM(c: var LLVMCode; n: var Cursor): set[LengPragma] =
  result = {}
  if n.kind == DotToken:
    inc n
  elif n.substructureKind == PragmasU:
    n.loopInto:
      let pk = n.pragmaKind
      case pk
      of AlignP, AttrP, WasP:
        skip n
      of HeaderP:
        n.into:
          if n.kind == StrLit:
            inc n
          else:
            error c.m, "expected string literal in header pragma but got: ", n
          while n.hasMore: skip n
      of StaticP, ImportcP, ImportcppP, ExportcP, NodeclP:
        result.incl pk
        skip n
      else:
        error c.m, "invalid pragma: ", n
  else:
    error c.m, "expected pragmas but got: ", n

type
  VarKindLLVM = enum
    IsLocal, IsGlobal, IsThreadlocal, IsConst

proc emitGlobalDbgVar(c: var LLVMCode; name: string; varInfo: NifLineInfo;
                      symId: SymId; diType: int): string =
  if c.debug.cuId == 0: return ""
  var fileId = 0
  var line = 0
  if varInfo.isValid and varInfo.file.isValid:
    fileId = getOrCreateDIFile(c, varInfo.file)
    line = int(varInfo.line)
  if fileId == 0:
    fileId = c.currentProc.subprogramFileId
  if fileId == 0: return ""
  if diType == 0: return ""
  let displayName = nifSymBaseName(c, symId)
  let gvId = c.addMetadata("distinct !DIGlobalVariable(name: \"" & displayName &
    "\", scope: !" & $c.debug.cuId &
    ", file: !" & $fileId &
    ", line: " & $line &
    ", type: !" & $diType &
    ", isLocal: false, isDefinition: true)")
  let gveId = c.addMetadata("!DIGlobalVariableExpression(var: !" &
    $gvId & ", expr: !DIExpression())")
  c.debug.globalExprs.add gveId
  result = ", !dbg !" & $gveId

proc genGlobalVarDeclLLVM(c: var LLVMCode; n: var Cursor; vk: VarKindLLVM;
    toExtern = false) =
  let varInfo = n.info
  var d = takeVarDecl(n)
  if d.name.kind == SymbolDef:
    let lit = d.name.symId
    c.m.registerLocal(lit, d.typ)
    var diType = 0
    if not toExtern:
      diType = genDITypeReadOnly(c, d.typ)

    var externName = StrId(0)
    var isImport = false
    var isNodecl = false
    let alignVal = extractAlignValue(d.pragmas)
    if d.pragmas.substructureKind == PragmasU:
      var p = d.pragmas
      p.loopInto:
        case p.pragmaKind
        of ImportcP, ImportcppP:
          externName = nifmodules.externName(lit, p)
          isImport = true
        of ExportcP:
          externName = nifmodules.externName(lit, p)
        of NodeclP:
          isNodecl = true
        of HeaderP:
          discard
        else: discard
        skip p

    let flags = genVarPragmasLLVM(c, d.pragmas)
    if isNodecl and isImport and externName != StrId(0):
      let extName = c.m.pool.strings[externName]
      var t = d.typ
      let typ = genTypeLLVM(c, t)
      let zeroI: array[6, int] = [0, 1, 2, 3, 4, 5]
      case extName
      of "__ATOMIC_RELAXED", "__ATOMIC_CONSUME", "__ATOMIC_ACQUIRE",
         "__ATOMIC_RELEASE", "__ATOMIC_ACQ_REL", "__ATOMIC_SEQ_CST":
        let val = case extName
          of "__ATOMIC_RELAXED": 0
          of "__ATOMIC_CONSUME": 1
          of "__ATOMIC_ACQUIRE": 2
          of "__ATOMIC_RELEASE": 3
          of "__ATOMIC_ACQ_REL": 4
          else: 5
        c.module.globals.add LLGlobal(name: extName, typ: typ,
            initVal: llIntText($val, typ), isConstant: true)
      else:
        discard
      skip d.value
      return

    let name = if externName != StrId(0): c.m.pool.strings[externName]
               else: mangleToC(c.m.pool.syms[lit])

    var t = d.typ
    let typ = genTypeLLVM(c, t)

    let dbgvSuffix = if not toExtern and not isImport:
                       emitGlobalDbgVar(c, name, varInfo, lit, diType) else: ""
    if toExtern or isImport:
      var g = LLGlobal(name: name, typ: typ, isExternal: true,
          isThreadLocal: (vk == IsThreadlocal), align: int alignVal)
      g.initVal = llZeroInit(typ)
      c.module.globals.add g
    else:
      if d.value.kind != DotToken:
        var v = d.value
        let initVal = genGlobalConstr(c, v, d.typ)
        c.module.globals.add LLGlobal(name: name, typ: initVal.typ,
            initVal: initVal, isThreadLocal: (vk == IsThreadlocal),
            isConstant: (vk == IsConst), align: int alignVal)
      else:
        skip d.value
        let zeroVal = if d.typ.typeKind in {PtrT, AptrT,
            ProctypeT}: llNull(typ) else: llZeroInit(typ)
        c.module.globals.add LLGlobal(name: name, typ: typ, initVal: zeroVal,
            isThreadLocal: (vk == IsThreadlocal),
            isConstant: (vk == IsConst), align: int alignVal)
    if vk == IsConst:
      c.emittedConsts.incl lit
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

    let name = mangleToC(c.m.pool.syms[lit])
    var t = d.typ
    let diType = genDITypeReadOnly(c, t)
    let typ = genTypeLLVM(c, t)
    let localName = "%" & name
    c.emitAlloca(name, typ, alignVal)
    emitDbgDeclare(c, localName, lit, wasName, varInfo, diType, llvmTyp = typ)

    if d.value.kind != DotToken:
      if d.value.stmtKind == OnerrS:
        var onErr = d.value
        inc onErr
        var onErrAction = onErr
        var val = LLValue(); genCallExprLLVM(c, d.value, val)
        c.setLoc(varInfo)
        c.emit LLInstr(kind: llStore, storeValue: val,
                       storePtr: llReg(name, c.primPtr))
        if onErrAction.kind != DotToken:
          genOnErrorLLVM(c, onErrAction)
      else:
        var val = LLValue(); genExprLLVM(c, d.value, val)
        c.setLoc(varInfo)
        c.emit LLInstr(kind: llStore, storeValue: val,
                       storePtr: llReg(name, c.primPtr))
    else:
      inc d.value
      let zeroVal = if d.typ.typeKind in {PtrT, AptrT,
          ProctypeT}: llNull(typ) else: llZeroInit(typ)
      c.setLoc(varInfo)
      c.emit LLInstr(kind: llStore, storeValue: zeroVal,
                     storePtr: llReg(name, c.primPtr))
  else:
    error c.m, "expected SymbolDef but got: ", d.name

# ---- Statement generation ----

include llvmgenstmts

# ---- Proc and toplevel generation ----

type
  PragmaInfo = object
    flags: set[LengPragma]
    extern: StrId
    callConv: CallConv
    wasName: string

proc parseProcPragmasLLVM(c: var LLVMCode; n: var Cursor): PragmaInfo =
  result = PragmaInfo()
  if n.kind == DotToken:
    inc n
  elif n.substructureKind == PragmasU:
    n.loopInto:
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
        n.into:
          if n.kind == StrLit:
            result.extern = n.litId
            inc n
          result.flags.incl pk
          while n.hasMore: skip n
      of HeaderP:
        n.into:
          if n.kind != StrLit:
            error c.m, "expected string literal in header pragma but got: ", n
          else:
            inc n
          while n.hasMore: skip n
      of SelectanyP:
        result.flags.incl pk
        skip n
      of WasP:
        n.into:
          result.wasName = toString(n, false)
          while n.hasMore: skip n
      of ErrsP, RaisesP, SmryP:
        skip n
      of InlineP:
        result.flags.incl pk
        skip n
      of NoinlineP:
        result.flags.incl pk
        skip n
      of AttrP:
        skip n
  else:
    error c.m, "expected proc pragmas but got: ", n

proc genSymDefLLVM(c: var LLVMCode; n: Cursor; prag: PragmaInfo): string =
  if n.kind == SymbolDef:
    let lit = n.symId
    if {ImportcP, ImportcppP, ExportcP} * prag.flags != {}:
      if prag.extern != StrId(0):
        result = c.m.pool.strings[prag.extern]
      else:
        result = c.m.pool.syms[lit]
        extractBasename(result)
    else:
      result = mangleToC(c.m.pool.syms[lit])
  else:
    result = ""
    error c.m, "expected SymbolDef but got: ", n

proc genParamPragmasLLVM(c: var LLVMCode; n: var Cursor) =
  if n.kind == DotToken:
    inc n
  elif n.substructureKind == PragmasU:
    n.loopInto:
      case n.pragmaKind
      of AttrP, WasP:
        skip n
      else:
        error c.m, "invalid pragma: ", n
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
  let oldBlock = c.currentBlockIdx
  c.currentProc = LLVMCurrentProc(funcIdx: 0, needsTerminator: false)
  c.nextTempCounter = 0
  c.nextLabelCounter = 0

  let procInfo = n.info
  var prc = takeProcDecl(n)
  let prag = parseProcPragmasLLVM(c, prc.pragmas)

  let name = genSymDefLLVM(c, prc.name, prag)

  var retType: LLType
  if prc.returnType.kind == DotToken:
    retType = c.primVoid
  else:
    var rt = prc.returnType
    retType = genTypeLLVM(c, rt)

  var isVarargs = false
  var paramTypes: seq[LLType] = @[]
  var paramNames: seq[string] = @[]
  var paramWasNames: seq[string] = @[]
  var paramSyms: seq[SymId] = @[]
  var paramDITypes: seq[int] = @[]
  if prc.params.kind != DotToken:
    var p = prc.params
    p.loopInto:
      assert p.substructureKind == ParamU
      var d = takeParamDecl(p)
      if d.name.kind == SymbolDef:
        let s = d.name.symId
        c.m.registerLocal(s, d.typ)
        var t = d.typ
        let paramType = genTypeLLVM(c, t)
        if d.typ.typeKind != VarargsT:
          paramTypes.add paramType
          let paramName = mangleToC(c.m.pool.syms[s])
          paramNames.add paramName
          paramWasNames.add extractWasPragma(d.pragmas)
          paramSyms.add s
          var pdi = genDITypeReadOnly(c, d.typ)
          if pdi == 0:
            let bits = bitsFromLLVMType(paramType, c.bits)
            pdi = genDIBasicType(c, "int " & $bits, bits, DW_ATE_signed)
          paramDITypes.add pdi
          genParamPragmasLLVM(c, d.pragmas)
        else:
          isVarargs = true
      else:
        error c.m, "expected SymbolDef but got: ", d.name

  var sig = "(" & paramTypes.mapIt(serialize(it)).join(", ")
  if isVarargs:
    if paramTypes.len > 0: sig.add ", "
    sig.add "..."
  sig.add ")"

  if {NodeclP, HeaderP} * prag.flags != {}:
    discard
  elif isExtern or {ImportcP, ImportcppP} * prag.flags != {}:
    let externName = name
    if externName notin c.declaredExterns:
      c.declaredExterns.incl externName
      if isVarargs: c.varargsFuncTypes["@" & externName] = sig
      let decl = "declare " & serialize(retType) & " @" & externName & sig & "\n"
      c.module.externs.add LLExternDecl(declaration: decl.strip(chars = {'\n'}),
          name: externName)
  else:
    # Function definition: build an LLFunc
    let displayName = if prag.wasName.len > 0: prag.wasName else: name
    let spId = createSubprogram(c, displayName, procInfo)
    c.currentProc.subprogramId = spId
    if isVarargs: c.varargsFuncTypes["@" & name] = sig

    var f = LLFunc(name: name, retType: retType, isVarargs: isVarargs,
        alwaysInline: (InlineP in prag.flags),
        noInline: (NoinlineP in prag.flags))
    var params: seq[(string, LLType)] = @[]
    for i, pn in paramNames:
      params.add (pn, paramTypes[i])
    f.params = move params
    f.metadata.subprogramId = spId
    f.blocks.add LLBlock(label: "entry") # entry block
    c.module.funcs.add f
    c.currentProc.funcIdx = c.module.funcs.high
    c.currentProc.retType = retType
    c.currentProc.retTypeCursor = prc.returnType
    c.currentBlockIdx = 0

    # Alloca + store for each parameter
    for i, pn in paramNames:
      c.emitAlloca(pn, paramTypes[i])
      let paramVal = llReg(pn & ".param", paramTypes[i])
      c.emit LLInstr(kind: llStore, storeValue: paramVal,
                     storePtr: llReg(pn, c.primPtr))
      let pdi = if i < paramDITypes.len: paramDITypes[i] else: 0
      let psym = if i < paramSyms.len: paramSyms[i] else: SymId(0)
      emitDbgDeclare(c, "%" & pn, psym, paramWasNames[i], procInfo, pdi,
          argNo = i+1, llvmTyp = paramTypes[i])

    # Generate body
    genStmtLLVM c, prc.body

    finalizeSubprogram(c, spId, paramDITypes, c.currentProc.retainedNodes)

    if not c.currentProc.needsTerminator:
      if prc.returnType.kind == DotToken:
        c.setLoc(procInfo)
        c.emit LLInstr(kind: llRet, retVal: llNoneVal())
      else:
        let zeroVal = if prc.returnType.typeKind in {PtrT, AptrT,
            ProctypeT}: llNull(retType) else: llZeroInit(retType)
        c.setLoc(procInfo)
        c.emit LLInstr(kind: llRet, retVal: zeroVal)

  c.m.closeScope()
  c.inToplevel = true
  c.currentProc = oldProc
  c.currentBlockIdx = oldBlock

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
    genGlobalVarDeclLLVM c, n, IsGlobal
  of TypeS:
    discard "handled in a different pass"
    skip n
  of EmitS:
    skip n
  of DiscardS, AsgnS, KeepovfS, ScopeS, IfS,
      WhileS, CaseS, LabS, JmpS, TryS, RaiseS, CallS, OnerrS:
    # Route into the init function.
    let savedFunc = c.currentProc.funcIdx
    let savedBlock = c.currentBlockIdx
    let savedLoc = c.currentProc.dbgLoc
    if not c.module.hasInitBody:
      c.module.initFunc.blocks.add LLBlock(label: "entry")
      c.module.hasInitBody = true
    c.currentProc.funcIdx = InitFuncIdx
    c.currentProc.needsTerminator = false
    c.currentBlockIdx = c.module.initFunc.blocks.high
    genStmtLLVM c, n
    c.currentProc.funcIdx = savedFunc
    c.currentBlockIdx = savedBlock
    c.currentProc.dbgLoc = savedLoc
  of StmtsS:
    n.loopInto: genToplevelLLVM c, n
  else:
    error c.m, "expected top level construct but got: ", n

proc traverseCodeLLVM(c: var LLVMCode; n: var Cursor) =
  if n.stmtKind == StmtsS:
    n.loopInto: genToplevelLLVM(c, n)
    genImportedSymsLLVM c
  else:
    error c.m, "expected `stmts` but got: ", n

proc generateLLVMTypes(c: var LLVMCode) =
  var co = TypeOrderLLVM()
  traverseTypesLLVM(c.m, co)
  for (d, isForward) in co.ordered:
    var n = d
    let decl = takeTypeDecl(n)
    if not c.generatedTypes.containsOrIncl(decl.name.symId):
      var skipDecl = false
      var packed = false
      if decl.pragmas.substructureKind == PragmasU:
        var p = decl.pragmas
        p.loopInto:
          case p.pragmaKind
          of NodeclP, HeaderP:
            skipDecl = true
          of PackedP:
            packed = true
          else: discard
          skip p
      if skipDecl: continue

      let name = mangleToC(c.m.pool.syms[decl.name.symId])
      if isForward:
        c.module.typeDefs.add "%" & name & " = type opaque\n"
      else:
        var body = decl.body
        let typeDef = genTypeDefLLVM(c, body, name, packed)
        if typeDef.len > 0:
          c.module.typeDefs.add typeDef

proc generateLLVMCode*(s: var State; inp, outp: string; flags: set[LLVMGenFlag]) =
  var m = load(inp)
  m.config = s.config
  var c = initLLVMCode(m, flags, s.bits)
  c.m.openScope()

  initDebugInfo(c, inp)

  var n = beginRead(c.m.src)
  traverseCodeLLVM c, n

  generateLLVMTypes c

  let triple = when defined(macos): "arm64-apple-macosx" else: ""
  var f = "; LLVM IR generated by Lengc\n"
  f.add "target datalayout = \"e-m:o-i64:64-i128:128-n32:64-S128\"\n"
  if triple.len > 0:
    f.add "target triple = \"" & triple & "\"\n"
  else:
    f.add "; target triple should be set for your platform\n"
  f.add "\n"

  for td in c.module.typeDefs:
    f.add td
  if c.module.typeDefs.len > 0: f.add "\n"

  for e in c.module.externs:
    f.add e.declaration & "\n"
  if c.module.externs.len > 0: f.add "\n"

  for g in c.module.globals:
    f.add serialize(g) & "\n"
  if c.module.globals.len > 0: f.add "\n"

  if gfMainModule in c.flags:
    f.add "@LENGC_ERR_ = thread_local global i8 0\n"
    f.add "@LENGC_OVF_ = thread_local global i8 0\n\n"
  else:
    f.add "@LENGC_ERR_ = external thread_local global i8\n"
    f.add "@LENGC_OVF_ = external thread_local global i8\n\n"

  for fn in c.module.funcs:
    f.add serialize(fn) & "\n\n"

  if c.module.hasInitBody:
    var initFn = c.module.initFunc
    initFn.name = "lengc_init"
    initFn.retType = c.primVoid
    f.add serialize(initFn) & "\n"
    f.add "@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @lengc_init, ptr null }]\n"

  if c.debug.metadata.len > 0:
    if c.debug.globalExprs.len > 0:
      var gList = ""
      for i, gid in c.debug.globalExprs:
        if i > 0: gList.add ", "
        gList.add "!" & $gid
      let globalsId = c.addMetadata("!{" & gList & "}")
      let oldCu = c.debug.metadata[c.debug.cuId]
      c.debug.metadata[c.debug.cuId] =
        oldCu[0..^2] & ", globals: !" & $globalsId & ", splitDebugInlining: false)"

    let dwarfId = c.addMetadata("!{i32 7, !\"Dwarf Version\", i32 4}")
    let diVersionId = c.addMetadata("!{i32 2, !\"Debug Info Version\", i32 3}")
    f.add "\n"
    f.add "!llvm.dbg.cu = !{!" & $c.debug.cuId & "}\n"
    f.add "!llvm.module.flags = !{!" & $dwarfId & ", !" & $diVersionId & "}\n"
    for i, md in c.debug.metadata:
      f.add "!" & $i & " = " & md & "\n"

  if vfsExists(outp) and vfsRead(outp) == f:
    discard "unchanged, keep mtime for incremental builds"
  else:
    vfsWrite outp, f

  c.m.closeScope()
