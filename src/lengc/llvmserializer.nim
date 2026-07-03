#
#
#           Leng Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## LLVM IR serializer: turns the structured `LLModule` into `.ll` text.
##
## This is the *only* place that knows how LLVM IR is spelled. Because
## `LLValue` is a tagged union, value rendering switches on `kind` and never
## inspects string prefixes.

import llvmirmodel

const OrderingStr*: array[LLAtomicOrdering, string] = [
  "unordered", "monotonic", "acquire", "release", "acq_rel", "seq_cst"]

const RmwOpStr*: array[LLAtomicrmwOp, string] = [
  "xchg", "add", "sub", "and", "or", "xor",
  "nand", "min", "max", "umin", "umax"]

proc serialize*(typ: LLType; result: var string) =
  ## Append the LLVM type string to `result`. Uses `var string` to avoid
  ## exponential allocation when serializing nested pointer/array/struct types.
  if typ == nil:
    result.add "ptr" # treat nil pointee as opaque ptr
  case typ.kind
  of llVoid: result.add "void"
  of llInt: result.add "i" & $typ.intBits
  of llFloat:
    case typ.floatBits
    of 32: result.add "float"
    of 64: result.add "double"
    of 128: result.add "fp128"
    else: result.add "double"
  of llPtr: result.add "ptr"
  of llArray:
    result.add "[" & $typ.arrLen & " x "
    serialize(typ.arrElem, result)
    result.add "]"
  of llStruct:
    if typ.name.len > 0:
      result.add "%" & typ.name
    else:
      result.add "{ "
      for i, f in typ.structFields:
        if i > 0: result.add ", "
        serialize(f.typ, result)
      result.add " }"
  of llUnion: serialize(typ.repType, result)
  of llFunc: result.add "ptr"

proc serialize*(typ: LLType): string =
  ## Convenience overload. Prefer `serialize(typ, result)` when building into an
  ## existing string to avoid an extra allocation.
  result = ""
  serialize(typ, result)

proc serializeUnqualified*(v: LLValue; result: var string) =
  ## Append the bare value text WITHOUT its type prefix.
  case v.kind
  of llvReg: result.add "%" & v.regName
  of llvInt: result.add v.intText
  of llvFloat: result.add v.floatText
  of llvBool: result.add (if v.boolVal: "1" else: "0")
  of llvGlobal: result.add "@" & v.globalName
  of llvNull: result.add "null"
  of llvUndef: result.add "undef"
  of llvZero: result.add "zeroinitializer"
  of llvPoison: result.add "poison"
  of llvCString: result.add "c\"" & v.strVal & "\""
  of llvRawText: result.add v.rawText
  of llvNone: discard

proc operand*(v: LLValue; result: var string) =
  ## Append typed operand: "<type> <value>".
  if v.kind == llvNone: return
  serialize(v.typ, result)
  result.add " "
  serializeUnqualified(v, result)

proc resultPrefix(i: LLInstr; result: var string) =
  if i.result.kind != llvNone:
    result.add "%" & i.result.regName & " = "

proc serialize*(i: LLInstr; result: var string) =
  ## Append a single instruction WITHOUT leading indentation or trailing
  ## newline. The caller adds those.
  case i.kind
  of llAdd, llSub, llMul, llSDiv, llUDiv, llSRem, llURem,
     llShl, llAShr, llLShr, llAnd, llOr, llXor:
    resultPrefix(i, result)
    result.add i.binOp
    if i.binNuw: result.add " nuw"
    if i.binNsw: result.add " nsw"
    result.add " "
    operand(i.binLhs, result)
    result.add ", "
    serializeUnqualified(i.binRhs, result)
  of llIcmp:
    resultPrefix(i, result)
    result.add "icmp " & i.icmpPred & " "
    operand(i.icmpLhs, result)
    result.add ", "
    serializeUnqualified(i.icmpRhs, result)
  of llFcmp:
    resultPrefix(i, result)
    result.add "fcmp " & i.fcmpPred & " "
    operand(i.fcmpLhs, result)
    result.add ", "
    serializeUnqualified(i.fcmpRhs, result)
  of llZext, llSext, llTrunc, llFpext, llFptrunc, llSitofp, llFptosi,
     llBitcast, llInttoptr, llPtrtoint:
    resultPrefix(i, result)
    result.add i.castOp & " "
    operand(i.castSrc, result)
    result.add " to "
    serialize(i.castDstType, result)
  of llAlloca:
    resultPrefix(i, result)
    result.add "alloca "
    serialize(i.allocaType, result)
    if i.allocaNumElts.kind != llvNone:
      result.add ", "
      serialize(i.allocaType, result)
      result.add " "
      serializeUnqualified(i.allocaNumElts, result)
    if i.allocaAlign > 0:
      result.add ", align " & $i.allocaAlign
  of llLoad:
    resultPrefix(i, result)
    result.add "load"
    if i.loadAtomic: result.add " atomic"
    result.add " "
    serialize(i.result.typ, result)
    result.add ", ptr "
    serializeUnqualified(i.loadPtr, result)
    if i.loadAtomic:
      result.add " " & OrderingStr[i.loadOrdering]
      if i.loadAlign > 0: result.add ", align " & $i.loadAlign
  of llStore:
    result.add "store"
    if i.storeAtomic: result.add " atomic"
    result.add " "
    operand(i.storeValue, result)
    result.add ", ptr "
    serializeUnqualified(i.storePtr, result)
    if i.storeAtomic:
      result.add " " & OrderingStr[i.storeOrdering]
      if i.storeAlign > 0: result.add ", align " & $i.storeAlign
  of llGep:
    resultPrefix(i, result)
    result.add "getelementptr "
    if i.gepInbounds: result.add "inbounds "
    serialize(i.gepBaseType, result)
    result.add ", ptr "
    serializeUnqualified(i.gepBase, result)
    for idx in i.gepIndices:
      result.add ", "
      operand(idx, result)
  of llCall:
    if i.result.kind != llvNone:
      result.add "%" & i.result.regName & " = "
    result.add "call "
    serialize(i.callRetType, result)
    if i.callFuncType.len > 0:
      result.add " " & i.callFuncType
    result.add " " & i.callCallee & "("
    for j, a in i.callArgs:
      if j > 0: result.add ", "
      operand(a, result)
    result.add ")"
  of llExtractValue:
    resultPrefix(i, result)
    result.add "extractvalue " & i.evAggType & " "
    serializeUnqualified(i.evAggregate, result)
    result.add ", " & $i.evIndex
  of llInsertValue:
    resultPrefix(i, result)
    result.add "insertvalue " & i.ivAggType & " "
    serializeUnqualified(i.ivAggregate, result)
    result.add ", "
    operand(i.ivElement, result)
    result.add ", " & $i.ivIndex
  of llPhi:
    resultPrefix(i, result)
    result.add "phi "
    serialize(i.result.typ, result)
    for j, (val, label) in i.phiIncoming:
      if j > 0: result.add ", "
      result.add " [ "
      serializeUnqualified(val, result)
      result.add ", %" & label & " ]"
  of llSelect:
    resultPrefix(i, result)
    result.add "select i1 "
    serializeUnqualified(i.selCond, result)
    result.add ", "
    operand(i.selTrue, result)
    result.add ", "
    operand(i.selFalse, result)
  of llRet:
    result.add "ret"
    if i.retVal.kind == llvNone:
      result.add " void"
    else:
      result.add " "
      operand(i.retVal, result)
  of llBr:
    result.add "br label %" & i.brTarget
  of llCondBr:
    result.add "br i1 "
    serializeUnqualified(i.condBrCond, result)
    result.add ", label %" & i.condBrTrue & ", label %" & i.condBrFalse
  of llSwitch:
    result.add "switch " & i.switchValType & " "
    serializeUnqualified(i.switchVal, result)
    result.add ", label %" & i.switchDefault
    if i.switchCases.len > 0:
      result.add " [\n"
      for (cv, label) in i.switchCases:
        result.add "    " & i.switchValType & " "
        serializeUnqualified(cv, result)
        result.add ", label %" & label & "\n"
      result.add "  ]"
  of llUnreachable:
    result.add "unreachable"
  of llAtomicrmw:
    resultPrefix(i, result)
    result.add "atomicrmw " & RmwOpStr[i.armwOp]
    if i.armwSyncscope.len > 0:
      result.add " syncscope(\"" & i.armwSyncscope & "\")"
    result.add " ptr "
    serializeUnqualified(i.armwPtr, result)
    result.add ", "
    operand(i.armwVal, result)
    result.add " " & OrderingStr[i.armwOrdering]
    if i.armwAlign > 0: result.add ", align " & $i.armwAlign
  of llCmpxchg:
    resultPrefix(i, result)
    result.add "cmpxchg"
    if i.cxSyncscope.len > 0:
      result.add " syncscope(\"" & i.cxSyncscope & "\")"
    result.add " ptr "
    serializeUnqualified(i.cxPtr, result)
    result.add ", "
    operand(i.cxExpected, result)
    result.add ", "
    operand(i.cxDesired, result)
    result.add " " & OrderingStr[i.cxSuccessOrdering]
    result.add " " & OrderingStr[i.cxFailureOrdering]
    if i.cxWeak: result.add " weak"
    if i.cxAlign > 0: result.add ", align " & $i.cxAlign
  of llFence:
    result.add "fence"
    if i.fenceSyncscope.len > 0:
      result.add " syncscope(\"" & i.fenceSyncscope & "\")"
    result.add " " & OrderingStr[i.fenceOrdering]
  of llRaw:
    result.add i.rawText

proc serialize*(blk: LLBlock; result: var string) =
  result.add blk.label & ":\n"
  for instr in blk.instrs:
    result.add "  "
    serialize(instr, result)
    if instr.kind != llRaw:
      result.add instr.dbgLoc
    result.add "\n"

proc paramText(f: LLFunc; result: var string) =
  for i, (name, typ) in f.params:
    if i > 0: result.add ", "
    serialize(typ, result)
    result.add " %" & name & ".param"
  if f.isVarargs:
    if f.params.len > 0: result.add ", "
    result.add "..."

proc serialize*(f: LLFunc; result: var string) =
  result.add "define "
  serialize(f.retType, result)
  result.add " @" & f.name & "("
  paramText(f, result)
  result.add ")"
  if f.alwaysInline: result.add " alwaysinline"
  if f.noInline: result.add " noinline"
  if f.metadata.subprogramId != 0:
    result.add " !dbg !" & $f.metadata.subprogramId
  result.add " {\n"
  for i, blk in f.blocks:
    if i == 0:
      result.add blk.label & ":\n"
      for a in f.entryAllocas:
        result.add "  "
        serialize(a, result)
        result.add a.dbgLoc
        result.add "\n"
      for instr in blk.instrs:
        result.add "  "
        serialize(instr, result)
        if instr.kind != llRaw:
          result.add instr.dbgLoc
        result.add "\n"
    else:
      serialize(blk, result)
  result.add "}\n"

proc serialize*(g: LLGlobal; result: var string) =
  result.add "@" & g.name & " = "
  if g.isPrivate: result.add "private "
  if g.isExternal:
    result.add "external "
    if g.isThreadLocal: result.add "thread_local "
    result.add "global "
    serialize(g.typ, result)
  else:
    if g.isThreadLocal: result.add "thread_local "
    result.add (if g.isConstant: "constant " else: "global ")
    serialize(g.typ, result)
    result.add " "
    serializeUnqualified(g.initVal, result)
  if g.align > 0: result.add ", align " & $g.align
  if g.dbgLoc.len > 0: result.add g.dbgLoc

proc serialize*(m: LLModule; triple: string; result: var string) =
  result.add "; LLVM IR generated by Lengc\n"
  result.add "target datalayout = \"e-m:o-i64:64-i128:128-n32:64-S128\"\n"
  if triple.len > 0:
    result.add "target triple = \"" & triple & "\"\n"
  else:
    result.add "; target triple should be set for your platform\n"
  result.add "\n"

  for td in m.typeDefs:
    result.add td
  if m.typeDefs.len > 0: result.add "\n"

  for e in m.externs:
    result.add e.declaration & "\n"
  if m.externs.len > 0: result.add "\n"

  for g in m.globals:
    serialize(g, result)
    result.add "\n"
  if m.globals.len > 0: result.add "\n"

  # error and overflow flags are added by the caller before serializing,
  # but to keep this self-contained we emit placeholders that the integrator
  # overwrites. The integrator splices them in directly.

  for f in m.funcs:
    serialize(f, result)
    result.add "\n\n"

  if m.hasInitBody:
    serialize(m.initFunc, result)
    result.add "\n"
