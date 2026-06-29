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

import std / [strutils, sequtils]
import llvmirmodel

const OrderingStr*: array[LLAtomicOrdering, string] = [
  "unordered", "monotonic", "acquire", "release", "acq_rel", "seq_cst"]

const RmwOpStr*: array[LLAtomicrmwOp, string] = [
  "xchg", "add", "sub", "and", "or", "xor",
  "nand", "min", "max", "umin", "umax"]

proc serialize*(typ: LLType): string =
  if typ == nil: return "ptr" # treat nil pointee as opaque ptr
  case typ.kind
  of llVoid: "void"
  of llInt: "i" & $typ.intBits
  of llFloat:
    case typ.floatBits
    of 32: "float"
    of 64: "double"
    of 128: "fp128"
    else: "double"
  of llPtr: "ptr"
  of llArray: "[" & $typ.arrLen & " x " & serialize(typ.arrElem) & "]"
  of llStruct:
    if typ.name.len > 0:
      "%" & typ.name
    else:
      "{ " & typ.structFields.mapIt(serialize(it.typ)).join(", ") & " }"
  of llUnion: serialize(typ.repType)
  of llFunc: "ptr"

proc serializeUnqualified*(v: LLValue): string =
  ## The bare value text WITHOUT its type prefix.
  case v.kind
  of llvReg: "%" & v.regName
  of llvInt: v.intText
  of llvFloat: v.floatText
  of llvBool: (if v.boolVal: "1" else: "0")
  of llvGlobal: "@" & v.globalName
  of llvNull: "null"
  of llvUndef: "undef"
  of llvZero: "zeroinitializer"
  of llvPoison: "poison"
  of llvCString: "c\"" & v.strVal & "\""
  of llvRawText: v.rawText
  of llvNone: ""

proc operand*(v: LLValue): string =
  ## Typed operand: "<type> <value>".
  if v.kind == llvNone: ""
  else: serialize(v.typ) & " " & serializeUnqualified(v)

proc resultPrefix(i: LLInstr): string =
  if i.result.kind != llvNone:
    "%" & i.result.regName & " = "
  else:
    ""

proc serialize*(i: LLInstr): string =
  ## Renders a single instruction WITHOUT leading indentation or trailing
  ## newline. The caller adds those.
  result = ""
  case i.kind
  of llAdd, llSub, llMul, llSDiv, llUDiv, llSRem, llURem,
     llShl, llAShr, llLShr, llAnd, llOr, llXor:
    result = i.resultPrefix() & i.binOp
    if i.binNuw: result.add " nuw"
    if i.binNsw: result.add " nsw"
    result.add " " & operand(i.binLhs) & ", " & serializeUnqualified(i.binRhs)
  of llIcmp:
    result = i.resultPrefix() & "icmp " & i.icmpPred & " " &
             operand(i.icmpLhs) & ", " & serializeUnqualified(i.icmpRhs)
  of llFcmp:
    result = i.resultPrefix() & "fcmp " & i.fcmpPred & " " &
             operand(i.fcmpLhs) & ", " & serializeUnqualified(i.fcmpRhs)
  of llZext, llSext, llTrunc, llFpext, llFptrunc, llSitofp, llFptosi,
     llBitcast, llInttoptr, llPtrtoint:
    result = i.resultPrefix() & i.castOp & " " & operand(i.castSrc) &
             " to " & serialize(i.castDstType)
  of llAlloca:
    result = i.resultPrefix() & "alloca " & serialize(i.allocaType)
    if i.allocaNumElts.kind != llvNone:
      result.add ", " & serialize(i.allocaType) & " " &
                 serializeUnqualified(i.allocaNumElts)
    if i.allocaAlign > 0:
      result.add ", align " & $i.allocaAlign
  of llLoad:
    result = i.resultPrefix() & "load"
    if i.loadAtomic: result.add " atomic"
    result.add " " & serialize(i.result.typ) & ", ptr " &
             serializeUnqualified(i.loadPtr)
    if i.loadAtomic:
      result.add " " & OrderingStr[i.loadOrdering]
      if i.loadAlign > 0: result.add ", align " & $i.loadAlign
  of llStore:
    result = "store"
    if i.storeAtomic: result.add " atomic"
    result.add " " & operand(i.storeValue) & ", ptr " &
             serializeUnqualified(i.storePtr)
    if i.storeAtomic:
      result.add " " & OrderingStr[i.storeOrdering]
      if i.storeAlign > 0: result.add ", align " & $i.storeAlign
  of llGep:
    result = i.resultPrefix() & "getelementptr "
    if i.gepInbounds: result.add "inbounds "
    result.add serialize(i.gepBaseType) & ", ptr " &
             serializeUnqualified(i.gepBase)
    for idx in i.gepIndices:
      result.add ", " & operand(idx)
  of llCall:
    if i.result.kind != llvNone:
      result = "%" & i.result.regName & " = "
    result.add "call " & serialize(i.callRetType)
    if i.callFuncType.len > 0:
      result.add " " & i.callFuncType
    result.add " " & i.callCallee & "("
    for j, a in i.callArgs:
      if j > 0: result.add ", "
      result.add operand(a)
    result.add ")"
  of llExtractValue:
    result = i.resultPrefix() & "extractvalue " & i.evAggType & " " &
             serializeUnqualified(i.evAggregate) & ", " & $i.evIndex
  of llInsertValue:
    result = i.resultPrefix() & "insertvalue " & i.ivAggType & " " &
             serializeUnqualified(i.ivAggregate) & ", " & operand(i.ivElement) &
             ", " & $i.ivIndex
  of llPhi:
    result = i.resultPrefix() & "phi " & serialize(i.result.typ)
    for j, (val, label) in i.phiIncoming:
      if j > 0: result.add ", "
      result.add " [ " & serializeUnqualified(val) & ", %" & label & " ]"
  of llSelect:
    result = i.resultPrefix() & "select i1 " & serializeUnqualified(i.selCond) &
             ", " & operand(i.selTrue) & ", " & operand(i.selFalse)
  of llRet:
    if i.retVal.kind == llvNone:
      result = "ret void"
    else:
      result = "ret " & operand(i.retVal)
  of llBr:
    result = "br label %" & i.brTarget
  of llCondBr:
    result = "br i1 " & serializeUnqualified(i.condBrCond) &
             ", label %" & i.condBrTrue & ", label %" & i.condBrFalse
  of llSwitch:
    result = "switch " & i.switchValType & " " &
             serializeUnqualified(i.switchVal) & ", label %" & i.switchDefault
    if i.switchCases.len > 0:
      result.add " [\n"
      for (cv, label) in i.switchCases:
        result.add "    " & i.switchValType & " " &
            serializeUnqualified(cv) & ", label %" & label & "\n"
      result.add "  ]"
  of llUnreachable:
    result = "unreachable"
  of llAtomicrmw:
    result = i.resultPrefix() & "atomicrmw " & RmwOpStr[i.armwOp]
    if i.armwSyncscope.len > 0:
      result.add " syncscope(\"" & i.armwSyncscope & "\")"
    result.add " ptr " & serializeUnqualified(i.armwPtr) & ", " &
             operand(i.armwVal) & " " & OrderingStr[i.armwOrdering]
    if i.armwAlign > 0: result.add ", align " & $i.armwAlign
  of llCmpxchg:
    result = i.resultPrefix() & "cmpxchg"
    if i.cxSyncscope.len > 0:
      result.add " syncscope(\"" & i.cxSyncscope & "\")"
    result.add " ptr " & serializeUnqualified(i.cxPtr) & ", " &
             operand(i.cxExpected) & ", " & serializeUnqualified(i.cxDesired) &
             " " & OrderingStr[i.cxSuccessOrdering] & " " &
             OrderingStr[i.cxFailureOrdering]
    if i.cxWeak: result.add " weak"
    if i.cxAlign > 0: result.add ", align " & $i.cxAlign
  of llFence:
    result = "fence"
    if i.fenceSyncscope.len > 0:
      result.add " syncscope(\"" & i.fenceSyncscope & "\")"
    result.add " " & OrderingStr[i.fenceOrdering]
  of llRaw:
    result = i.rawText

proc serialize*(blk: LLBlock): string =
  result = blk.label & ":\n"
  for instr in blk.instrs:
    result.add "  " & serialize(instr)
    if instr.kind != llRaw:
      result.add instr.dbgLoc
    result.add "\n"

proc paramText(f: LLFunc): string =
  var parts: seq[string] = @[]
  for (name, typ) in f.params:
    parts.add serialize(typ) & " %" & name & ".param"
  if f.isVarargs:
    parts.add "..."
  result = parts.join(", ")

proc serialize*(f: LLFunc): string =
  result = "define "
  result.add serialize(f.retType) & " @" & f.name & "(" & paramText(f) & ")"
  if f.alwaysInline: result.add " alwaysinline"
  if f.noInline: result.add " noinline"
  if f.metadata.subprogramId != 0:
    result.add " !dbg !" & $f.metadata.subprogramId
  result.add " {\n"
  for i, blk in f.blocks:
    if i == 0:
      result.add blk.label & ":\n"
      for a in f.entryAllocas:
        result.add "  " & serialize(a) & a.dbgLoc & "\n"
      for instr in blk.instrs:
        result.add "  " & serialize(instr)
        if instr.kind != llRaw:
          result.add instr.dbgLoc
        result.add "\n"
    else:
      result.add serialize(blk)
  result.add "}\n"

proc serialize*(g: LLGlobal): string =
  result = "@" & g.name & " = "
  if g.isPrivate: result.add "private "
  if g.isExternal:
    result.add "external "
    if g.isThreadLocal: result.add "thread_local "
    result.add "global " & serialize(g.typ)
  else:
    if g.isThreadLocal: result.add "thread_local "
    result.add (if g.isConstant: "constant " else: "global ")
    result.add serialize(g.typ) & " " & serializeUnqualified(g.initVal)
  if g.align > 0: result.add ", align " & $g.align

proc serialize*(m: LLModule; triple: string): string =
  result = "; LLVM IR generated by Lengc\n"
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
    result.add serialize(g) & "\n"
  if m.globals.len > 0: result.add "\n"

  # error and overflow flags are added by the caller before serializing,
  # but to keep this self-contained we emit placeholders that the integrator
  # overwrites. The integrator splices them in directly.

  for f in m.funcs:
    result.add serialize(f) & "\n\n"

  if m.hasInitBody:
    result.add serialize(m.initFunc) & "\n"
