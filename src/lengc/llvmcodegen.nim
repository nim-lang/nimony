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

import std / [assertions, syncio, sets, formatfloat, packedsets,
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

type
  PrimTypes* = object
    voidT*, i1*, i8*, i16*, i32*, i64*, ptrT*, f32*, f64*: LLType

  LLVMCode* = object
    m: MainModule
    module*: LLModule
    currentBlockIdx*: int
    nextTempCounter*: int
    nextLabelCounter*: int
    prim*: PrimTypes
    bits: int
    flags: set[LLVMGenFlag]
    requestedSyms*: HashSet[SymId]
    declaredExterns*: HashSet[string] # to avoid duplicate extern declarations
    emittedConsts*: HashSet[SymId]    # local consts emitted as global constants
    inToplevel: bool
    currentProc: LLVMCurrentProc
    strLitCounter*: int               # global counter for string literal names
    debug*: DebugInfo

proc initPrimTypes*(): PrimTypes =
  result = default(PrimTypes)
  result.voidT = newLLVoidType()
  result.i1 = newLLIntType(1)
  result.i8 = newLLIntType(8)
  result.i16 = newLLIntType(16)
  result.i32 = newLLIntType(32)
  result.i64 = newLLIntType(64)
  result.ptrT = newLLPtrType()
  result.f32 = newLLFloatType(32)
  result.f64 = newLLFloatType(64)

proc initLLVMCode*(m: sink MainModule; flags: set[LLVMGenFlag];
    bits: int): LLVMCode =
  result = LLVMCode(m: m, flags: flags, bits: bits, inToplevel: true,
                    prim: initPrimTypes())

proc llIntBits*(c: LLVMCode; n: int): LLType =
  case n
  of 1: c.prim.i1
  of 8: c.prim.i8
  of 16: c.prim.i16
  of 32: c.prim.i32
  of 64: c.prim.i64
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
  var instr = LLInstr(kind: llAlloca, result: llReg(name, c.prim.ptrT),
                      allocaType: typ, allocaAlign: int align)
  currentFunc(c).entryAllocas.add instr

proc emitGEP*(c: var LLVMCode; baseType: LLType; base: LLValue;
              indices: openArray[LLValue]; inbounds = true): LLValue =
  let t = c.nextTemp()
  result = llReg(t, c.prim.ptrT)
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
  result = ""
  serializeUnqualified(v, result)

proc mangleSym(c: var LLVMCode; s: SymId): string =
  ## extern name if declared, else mangleToC. Shared-SymId locals (inlined copies
  ## of one source var) are dedup'd at the alloca site, not here.
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

# ---- Pragma / type helpers ----

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

proc llFloatHexText(f: float): string {.inline.} =
  # LLVM float literal as hex: `0x` + IEEE-754 bits — exact, avoids the decimal-point rule.
  "0x" & toHex(cast[uint64](f), 16)

# ---- Type generation ----

include llvmgentypes

# ---- Func type helper (used by genexprs call-site) ----

proc genFuncTypeLLVM*(c: var LLVMCode; procDecl: Cursor): LLType =
  ## Build an llFunc type signature from a ProcS decl (type only).
  var n = procDecl
  let prc = takeProcDecl(n)
  var retType: LLType
  if prc.returnType.kind == DotToken:
    retType = c.prim.voidT
  else:
    var rt = prc.returnType
    retType = genTypeLLVM(c, rt)
  var paramTypes: seq[LLType] = @[]
  var isVarargs = false
  if prc.params.kind != DotToken:
    var p = prc.params
    p.loopInto:
      var d = takeParamDecl(p)
      if d.typ.typeKind == VarargsT:
        isVarargs = true
      else:
        var t = d.typ
        paramTypes.add genTypeLLVM(c, t)
  result = newLLFuncType(retType, paramTypes, isVarargs)

# ---- DWARF debug info (metadata infra + type generation) ----

include llvmdebug

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
      const AtomicOrderNames = ["__ATOMIC_RELAXED", "__ATOMIC_CONSUME",
        "__ATOMIC_ACQUIRE", "__ATOMIC_RELEASE", "__ATOMIC_ACQ_REL",
        "__ATOMIC_SEQ_CST"]
      var idx = -1
      for i, nm in AtomicOrderNames:
        if nm == extName:
          idx = i
          break
      if idx >= 0:
        c.module.globals.add LLGlobal(name: extName, typ: typ,
            initVal: llIntText($idx, typ), isConstant: true)
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
          isThreadLocal: (vk == IsThreadlocal), align: int alignVal,
          dbgLoc: dbgvSuffix)
      g.initVal = llZeroInit(typ)
      c.module.globals.add g
    else:
      # Module-private constants (plain `const`, string literals) hide their
      # global; exportc/importc/template keep external linkage.
      let isPriv = vk == IsConst and externName == StrId(0) and not c.inToplevel
      if d.value.kind != DotToken:
        var v = d.value
        let initVal = genGlobalConstr(c, v, d.typ)
        c.module.globals.add LLGlobal(name: name, typ: initVal.typ,
            initVal: initVal, isThreadLocal: (vk == IsThreadlocal),
            isConstant: (vk == IsConst), isPrivate: isPriv,
            align: int alignVal, dbgLoc: dbgvSuffix)
      else:
        skip d.value
        let zeroVal = llDefaultZero(typ)
        c.module.globals.add LLGlobal(name: name, typ: typ, initVal: zeroVal,
            isThreadLocal: (vk == IsThreadlocal),
            isConstant: (vk == IsConst), isPrivate: isPriv,
            align: int alignVal, dbgLoc: dbgvSuffix)
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

    let name = mangleSym(c, lit)
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
                       storePtr: llReg(name, c.prim.ptrT))
        if onErrAction.kind != DotToken:
          genOnErrorLLVM(c, onErrAction)
      else:
        var val = LLValue(); genExprLLVM(c, d.value, val)
        c.setLoc(varInfo)
        c.emit LLInstr(kind: llStore, storeValue: val,
                       storePtr: llReg(name, c.prim.ptrT))
    else:
      inc d.value
      let zeroVal = llDefaultZero(typ)
      c.setLoc(varInfo)
      c.emit LLInstr(kind: llStore, storeValue: zeroVal,
                     storePtr: llReg(name, c.prim.ptrT))
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
    retType = c.prim.voidT
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

  if {NodeclP, HeaderP} * prag.flags != {}:
    discard
  elif isExtern or {ImportcP, ImportcppP} * prag.flags != {}:
    let externName = name
    if externName notin c.declaredExterns:
      c.declaredExterns.incl externName
      c.module.externs.add LLExternDecl(name: externName, retType: retType,
          params: paramTypes, isVarargs: isVarargs)
  else:
    # Function definition: build an LLFunc
    let displayName = if prag.wasName.len > 0: prag.wasName else: name
    let spId = createSubprogram(c, displayName, procInfo)
    c.currentProc.subprogramId = spId

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
                     storePtr: llReg(pn, c.prim.ptrT))
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
        let zeroVal = llDefaultZero(retType)
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

# Named types are registered lazily by genTypeLLVM via ensureTypeDef
# (see llvmgentypes.nim) — no separate type-emission pass.

proc generateLLVMCode*(s: var State; inp, outp: string; flags: set[LLVMGenFlag]) =
  var m = load(inp)
  m.config = s.config
  var c = initLLVMCode(m, flags, s.bits)
  c.m.openScope()

  initDebugInfo(c, inp)

  var n = beginRead(c.m.src)
  traverseCodeLLVM c, n

  # Add runtime globals as structured LLGlobal
  let isMain = gfMainModule in c.flags
  c.module.globals.add LLGlobal(name: "LENGC_ERR_", typ: c.prim.i8,
      isThreadLocal: true, isExternal: not isMain, initVal: llIntText("0", c.prim.i8))
  c.module.globals.add LLGlobal(name: "LENGC_OVF_", typ: c.prim.i8,
      isThreadLocal: true, isExternal: not isMain, initVal: llIntText("0", c.prim.i8))

  # Patch debug metadata onto the module before serialization
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
    c.module.cuId = c.debug.cuId
    c.module.dwarfVerId = dwarfId
    c.module.diVerId = diVersionId
    c.module.metadata = move c.debug.metadata
    c.module.globalExprIds = move c.debug.globalExprs

  if c.module.hasInitBody:
    c.module.initFunc.name = "lengc_init"
    c.module.initFunc.retType = c.prim.voidT

  let llText = serializeModule(c.module)

  if vfsExists(outp) and vfsRead(outp) == llText:
    discard "unchanged, keep mtime for incremental builds"
  else:
    vfsWrite outp, llText

  c.m.closeScope()
