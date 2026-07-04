#
#
#           Leng Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Structured LLVM IR data model.
##
## This module is a pure data layer sitting between NIF traversal and the `.ll`
## text serializer. All LLVM syntax knowledge lives in `llvmserializer.nim`;
## nothing here knows how an instruction is spelled. Types are interned `ref`
## objects so identity is pointer-equality. Values are tagged unions so the
## serializer switches on `kind` instead of parsing string prefixes.

type
  LLTypeKind* = enum
    llVoid, llInt, llFloat, llPtr, llArray, llStruct, llUnion, llFunc

  LLType* = ref LLTypeObj
  LLTypeObj* = object
    name*: string                       # e.g. "Person", "" for anonymous
    case kind*: LLTypeKind
    of llVoid: discard
    of llInt: intBits*: int
    of llFloat: floatBits*: int
    of llPtr: ptrPointee*: LLType
    of llArray:
      arrLen*: int
      arrElem*: LLType
    of llStruct:
      structFields*: seq[LLStructField]
      structSizeBits*: int
      structAlignBits*: int
      structPacked*: bool
    of llUnion:
      unionMembers*: seq[LLUnionMember] # all branches (for GEP reference)
      repType*: LLType                  # the LLVM struct actually emitted
    of llFunc:
      funcRetType*: LLType
      funcParamTypes*: seq[LLType]
      funcIsVarargs*: bool

  LLStructField* = object
    name*: string
    typ*: LLType
    offsetBits*: int # 0 unless bitfield

  LLUnionMember* = object
    typ*: LLType
    sizeBits*: int
    alignBits*: int

  LLAtomicOrdering* = enum
    llaoUnordered, llaoMonotonic, llaoAcquire, llaoRelease,
    llaoAcqRel, llaoSeqCst

  LLAtomicrmwOp* = enum
    llrmwXchg, llrmwAdd, llrmwSub, llrmwAnd, llrmwOr, llrmwXor,
    llrmwNand, llrmwMin, llrmwMax, llrmwUmin, llrmwUmax

  LLValueKind* = enum
    llvNone,    # no value (for void results) — MUST be first = zero default
    llvReg,     # result of an instruction: %t5
    llvInt,     # integer literal: stored textually (e.g. "42", "-1")
    llvFloat,   # float/double literal: stored textually
    llvBool,    # boolean literal (true/false) emitted as i8 0/1 by codegen
    llvGlobal,  # @name — reference to a global
    llvNull,    # null
    llvUndef,   # undef
    llvZero,    # zeroinitializer
    llvPoison,  # poison
    llvCString, # c"..." string constant
    llvRawText, # pre-formatted text (complex constant initializers)

  LLValue* = object
    typ*: LLType          # type of this value
    case kind*: LLValueKind
    of llvReg:
      regName*: string    # stored WITHOUT the leading %
    of llvInt:
      intText*: string    # canonical integer text
    of llvFloat:
      floatText*: string  # canonical float text (may be 0x... hex)
    of llvBool:
      boolVal*: bool
    of llvGlobal:
      globalName*: string # stored WITHOUT the leading @
    of llvCString:
      strVal*: string
    of llvRawText:
      rawText*: string
    of llvNull, llvUndef, llvZero, llvPoison, llvNone:
      discard

  LLInstrKind* = enum
    llAdd, llSub, llMul, llSDiv, llUDiv, llSRem, llURem,
    llShl, llAShr, llLShr, llAnd, llOr, llXor,
    llIcmp, llFcmp,
    llZext, llSext, llTrunc, llFpext, llFptrunc, llSitofp, llFptosi,
    llBitcast, llInttoptr, llPtrtoint,
    llAlloca, llLoad, llStore, llGep,
    llCall, llExtractValue, llInsertValue,
    llPhi, llSelect,
    llBr, llCondBr, llSwitch, llRet, llUnreachable,
    llAtomicrmw, llCmpxchg, llFence,
    llRaw # escape hatch for intrinsics mixing values + metadata

  LLInstr* = object
    result*: LLValue                       # result register; llvNone when void
    dbgLoc*: string                        # ", !dbg !N" suffix, empty if none
    case kind*: LLInstrKind
    of llAdd..llXor:
      binOp*: string                       # "add", "sub", ... (allows nuw/nsw via flags)
      binLhs*: LLValue
      binRhs*: LLValue
      binNuw*: bool
      binNsw*: bool
    of llIcmp:
      icmpPred*: string                    # "eq", "ne", "slt", ...
      icmpLhs*: LLValue
      icmpRhs*: LLValue
    of llFcmp:
      fcmpPred*: string
      fcmpLhs*: LLValue
      fcmpRhs*: LLValue
    of llZext..llPtrtoint:
      castOp*: string                      # "zext", "sext", ...
      castSrc*: LLValue
      castDstType*: LLType
    of llAlloca:
      allocaType*: LLType
      allocaAlign*: int
      allocaNumElts*: LLValue              # llvNone for single-element alloca
    of llLoad:
      loadPtr*: LLValue
      loadAtomic*: bool
      loadOrdering*: LLAtomicOrdering
      loadAlign*: int
    of llStore:
      storeValue*: LLValue
      storePtr*: LLValue
      storeAtomic*: bool
      storeOrdering*: LLAtomicOrdering
      storeAlign*: int
    of llGep:
      gepBaseType*: LLType
      gepBase*: LLValue
      gepIndices*: seq[LLValue]
      gepInbounds*: bool
    of llCall:
      callCallee*: string                  # "@name" for direct, "%reg" for indirect
      callRetType*: LLType
      callArgs*: seq[LLValue]
      callFuncType*: string                # non-empty => varargs signature prefix
    of llExtractValue:
      evAggregate*: LLValue
      evAggType*: string                   # the aggregate type text, e.g. "{ i32, i1 }"
      evIndex*: int
    of llInsertValue:
      ivAggregate*: LLValue
      ivElement*: LLValue
      ivAggType*: string
      ivIndex*: int
    of llPhi:
      phiIncoming*: seq[(LLValue, string)] # (value, label)
    of llSelect:
      selCond*: LLValue
      selTrue*: LLValue
      selFalse*: LLValue
    of llRet:
      retVal*: LLValue                     # llvNone if void
    of llBr:
      brTarget*: string
    of llCondBr:
      condBrCond*: LLValue
      condBrTrue*: string
      condBrFalse*: string
    of llSwitch:
      switchValType*: string               # the value's type text
      switchVal*: LLValue
      switchDefault*: string
      switchCases*: seq[(LLValue, string)] # (value, label)
    of llUnreachable: discard
    of llAtomicrmw:
      armwOp*: LLAtomicrmwOp
      armwPtr*: LLValue
      armwVal*: LLValue
      armwOrdering*: LLAtomicOrdering
      armwSyncscope*: string               # "" default, "singlethread" for signal fence
      armwAlign*: int
    of llCmpxchg:
      cxPtr*: LLValue
      cxExpected*: LLValue
      cxDesired*: LLValue
      cxAggType*: string                   # the {T, i1} aggregate type text
      cxSuccessOrdering*: LLAtomicOrdering
      cxFailureOrdering*: LLAtomicOrdering
      cxWeak*: bool
      cxSyncscope*: string
      cxAlign*: int
    of llFence:
      fenceOrdering*: LLAtomicOrdering
      fenceSyncscope*: string
    of llRaw:
      rawText*: string                     # fully-formatted line (dbg intrinsics, etc.)

  LLBlock* = object
    label*: string
    instrs*: seq[LLInstr]

  LLFuncMetadata* = object
    subprogramId*: int
    fileId*: int
    paramDITypes*: seq[int]
    retainedNodeIds*: seq[int]

  LLFunc* = object
    name*: string
    retType*: LLType
    params*: seq[(string, LLType)]
    isVarargs*: bool
    entryAllocas*: seq[LLInstr] # allocas hoisted into the entry block
    blocks*: seq[LLBlock]
    alwaysInline*: bool
    noInline*: bool
    metadata*: LLFuncMetadata

  LLGlobal* = object
    name*: string   # WITHOUT leading @
    typ*: LLType
    initVal*: LLValue
    isThreadLocal*: bool
    isConstant*: bool
    isExternal*: bool
    isPrivate*: bool
    align*: int
    dbgLoc*: string # ", !dbg !N" suffix, empty if none

  LLExternDecl* = object
    declaration*: string # full declare line (kept as text)
    name*: string        # for dedup

  LLModule* = object
    typeDefs*: seq[string]   # full named type definition lines, in emission order
    globals*: seq[LLGlobal]
    externs*: seq[LLExternDecl]
    funcs*: seq[LLFunc]
    initFunc*: LLFunc        # the @lengc_init global constructor
    hasInitBody*: bool
    metadata*: seq[string]   # raw metadata nodes
    cuId*: int               # DICompileUnit metadata id
    globalExprIds*: seq[int] # DIGlobalVariableExpression ids

# ---- Value constructors ----

proc llReg*(name: string; typ: LLType): LLValue {.inline.} =
  LLValue(kind: llvReg, regName: name, typ: typ)

proc llIntText*(text: string; typ: LLType): LLValue {.inline.} =
  LLValue(kind: llvInt, intText: text, typ: typ)

proc llFloatText*(text: string; typ: LLType): LLValue {.inline.} =
  LLValue(kind: llvFloat, floatText: text, typ: typ)

proc llBoolVal*(b: bool; typ: LLType): LLValue {.inline.} =
  LLValue(kind: llvBool, boolVal: b, typ: typ)

proc llGlobalRef*(name: string; typ: LLType): LLValue {.inline.} =
  LLValue(kind: llvGlobal, globalName: name, typ: typ)

proc llNull*(typ: LLType): LLValue {.inline.} =
  LLValue(kind: llvNull, typ: typ)

proc llUndef*(typ: LLType): LLValue {.inline.} =
  LLValue(kind: llvUndef, typ: typ)

proc llZeroInit*(typ: LLType): LLValue {.inline.} =
  LLValue(kind: llvZero, typ: typ)

proc llNoneVal*(): LLValue {.inline.} =
  LLValue(kind: llvNone)

proc llCString*(s: string): LLValue {.inline.} =
  LLValue(kind: llvCString, strVal: s)

# ---- Type constructors ----

proc newLLVoidType*(): LLType =
  LLType(kind: llVoid)

proc newLLIntType*(bits: int): LLType =
  LLType(kind: llInt, intBits: bits)

proc newLLFloatType*(bits: int): LLType =
  LLType(kind: llFloat, floatBits: bits)

proc newLLPtrType*(pointee: LLType = nil): LLType =
  LLType(kind: llPtr, ptrPointee: pointee)

proc newLLArrayType*(len: int; elem: LLType): LLType =
  LLType(kind: llArray, arrLen: len, arrElem: elem)

proc newLLStructType*(fields: seq[LLStructField]; packed = false): LLType =
  LLType(kind: llStruct, structFields: fields, structPacked: packed)

proc newLLNamedType*(name: string): LLType =
  LLType(kind: llStruct, name: name)

proc newLLUnionType*(members: seq[LLUnionMember]; repType: LLType): LLType =
  LLType(kind: llUnion, unionMembers: members, repType: repType)

proc newLLFuncType*(retType: LLType; params: seq[LLType];
    isVarargs = false): LLType =
  LLType(kind: llFunc, funcRetType: retType, funcParamTypes: params,
         funcIsVarargs: isVarargs)

# ---- Instruction constructors ----

proc newLLBinInstr*(kind: LLInstrKind; res: LLValue; binOp: string;
                     binLhs, binRhs: LLValue; dbgLoc = ""): LLInstr =
  ## Construct a binary-op instruction whose `kind` is determined at runtime.
  ## The `case` inside the proc lets the Nim compiler verify that `binOp`,
  ## `binLhs`, `binRhs` — which only exist in the `llAdd..llXor` branch — are
  ## always valid for the value of `kind` passed by the caller.
  case kind
  of llAdd..llXor:
    LLInstr(kind: kind, result: res, dbgLoc: dbgLoc,
            binOp: binOp, binLhs: binLhs, binRhs: binRhs)
  else:
    raise newException(ValueError, "newLLBinInstr: kind " & $kind &
        " is not in the llAdd..llXor range")
