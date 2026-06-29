#
#
#           Leng Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included from llvmcodegen.nim
# Generates LLVM IR for expressions. Each expression returns an LLValue
# and may emit LLInstr instructions to the current block.

proc zeroVal(typ: LLType): LLValue {.inline.} =
  if typ != nil and typ.kind == llPtr: llNull(typ) else: llIntTextC("0", typ)

proc isFloatType(t: LLType): bool {.inline.} =
  t != nil and t.kind == llFloat

proc scalarTypeKind(c: var LLVMCode; typ: Cursor): LengType =
  var t = navigateToObjectBody(c.m, typ)
  if t.typeKind == EnumT:
    inc t
    t = navigateToObjectBody(c.m, t)
  result = t.typeKind

proc pointeeType(c: var LLVMCode; typ: Cursor): Cursor =
  let t = navigateToObjectBody(c.m, typ)
  if t.typeKind in {PtrT, AptrT, FlexarrayT}:
    result = t.firstSon
  else:
    result = default(Cursor)

proc llBinopKind(op: string): LLInstrKind =
  case op
  of "add": llAdd
  of "sub": llSub
  of "mul": llMul
  of "sdiv": llSDiv
  of "udiv": llUDiv
  of "srem": llSRem
  of "urem": llURem
  of "shl": llShl
  of "ashr": llAShr
  of "lshr": llLShr
  of "and": llAnd
  of "or": llOr
  of "xor": llXor
  of "fneg": llFcmp # unused placeholder
  else: llAdd

proc signedBinOp(c: var LLVMCode; n: var Cursor; op: string;
    result: var LLValue) =
  n.into:
    let typCursor = n
    let typLL = genTypeLLVM(c, n)
    let srcLhs = getType(c.m, n)
    var lhs = LLValue(); genExprLLVM(c, n, lhs)
    let srcRhs = getType(c.m, n)
    var rhs = LLValue(); genExprLLVM(c, n, rhs)
    if not typeEq(lhs.typ, typLL):
      coerceValueLLVM(c, lhs, srcLhs, typCursor, true, lhs)
    if not typeEq(rhs.typ, typLL):
      coerceValueLLVM(c, rhs, srcRhs, typCursor, true, rhs)
    let t = c.nextTemp()
    let res = llReg(t, typLL)
    c.emit newLLBinInstr(llBinopKind(op), res = res, binOp = op,
                         binLhs = lhs, binRhs = rhs)
    result = res
    while n.hasMore: skip n

proc unsignedBinOp(c: var LLVMCode; n: var Cursor; signedOp, unsignedOp: string;
    result: var LLValue) =
  n.into:
    let isUnsigned = n.typeKind == UT
    let typCursor = n
    let typLL = genTypeLLVM(c, n)
    let srcLhs = getType(c.m, n)
    var lhs = LLValue(); genExprLLVM(c, n, lhs)
    let srcRhs = getType(c.m, n)
    var rhs = LLValue(); genExprLLVM(c, n, rhs)
    if not typeEq(lhs.typ, typLL):
      coerceValueLLVM(c, lhs, srcLhs, typCursor, true, lhs)
    if not typeEq(rhs.typ, typLL):
      coerceValueLLVM(c, rhs, srcRhs, typCursor, true, rhs)
    let op = if isUnsigned: unsignedOp else: signedOp
    let t = c.nextTemp()
    let res = llReg(t, typLL)
    c.emit newLLBinInstr(llBinopKind(op), res = res, binOp = op,
                         binLhs = lhs, binRhs = rhs)
    result = res
    while n.hasMore: skip n

proc cmpOp(c: var LLVMCode; n: var Cursor; signedPred, unsignedPred: string;
    result: var LLValue) =
  let cmpInfo = n.info
  n.into:
    let lhsExpr = n
    let lhsType = getType(c.m, lhsExpr)
    let lhsTK = scalarTypeKind(c, lhsType)
    var lhs = LLValue(); genExprLLVM(c, n, lhs)
    var rhs = LLValue(); genExprLLVM(c, n, rhs)
    let t = c.nextTemp()
    if isFloatType(lhs.typ):
      let fpPred = case signedPred
        of "eq": "oeq"
        of "ne": "one"
        of "slt": "olt"
        of "sle": "ole"
        else: signedPred
      let res = llReg(t, c.llI1())
      c.setLoc(cmpInfo)
      c.emit LLInstr(kind: llFcmp, result: res, fcmpPred: fpPred,
                     fcmpLhs: lhs, fcmpRhs: rhs)
      result = res
    else:
      let pred = if unsignedPred != "" and lhsTK in {UT, CT, BoolT}:
                   unsignedPred
                 else:
                   signedPred
      let res = llReg(t, c.llI1())
      c.setLoc(cmpInfo)
      c.emit LLInstr(kind: llIcmp, result: res, icmpPred: pred,
                     icmpLhs: lhs, icmpRhs: rhs)
      result = res
    while n.hasMore: skip n

proc genBoolCmpOp(c: var LLVMCode; n: var Cursor; signedPred,
    unsignedPred: string; result: var LLValue) =
  var cmp = LLValue(); cmpOp(c, n, signedPred, unsignedPred, cmp)
  let t = c.nextTemp()
  let res = llReg(t, c.llI8())
  c.emit LLInstr(kind: llZext, result: res, castOp: "zext", castSrc: cmp,
                 castDstType: c.llI8())
  result = res

proc getExternName(c: var LLVMCode; s: SymId): string =
  let d = c.m.getDeclOrNil(s)
  if d != nil and d.extern != StrId(0) and d.isImport:
    result = c.m.pool.strings[d.extern]
  else:
    result = ""

proc memoryOrderToLLVM(val: LLValue): LLAtomicOrdering =
  result = llaoSeqCst

proc atomicAlign(c: var LLVMCode; typ: LLType): int {.inline.} =
  if typ == nil: c.bits div 8
  elif typ.kind == llPtr: c.bits div 8
  elif typ.kind == llInt:
    case typ.intBits
    of 8: 1
    of 16: 2
    of 32: 4
    of 64: 8
    else: c.bits div 8
  else: c.bits div 8

proc declareExtern(c: var LLVMCode; declLine: string; name: string) =
  if name notin c.declaredExterns:
    c.declaredExterns.incl name
    c.module.externs.add LLExternDecl(declaration: declLine, name: name)

proc genAtomicCall(c: var LLVMCode; externName: string; args: seq[LLValue];
    retType: LLType; result: var LLValue) =
  let ordering = memoryOrderToLLVM(args[^1])
  let align = atomicAlign(c, retType)
  case externName
  of "__atomic_load_n":
    let t = c.nextTemp()
    let res = llReg(t, retType)
    c.emit LLInstr(kind: llLoad, result: res, loadPtr: args[0],
                   loadAtomic: true, loadOrdering: ordering, loadAlign: align)
    result = res
  of "__atomic_store_n":
    c.emit LLInstr(kind: llStore, storeValue: args[1], storePtr: args[0],
                   storeAtomic: true, storeOrdering: ordering,
                       storeAlign: align)
    result = llNoneVal()
  of "__atomic_exchange_n":
    let t = c.nextTemp()
    let res = llReg(t, retType)
    c.emit LLInstr(kind: llAtomicrmw, result: res, armwOp: llrmwXchg,
                   armwPtr: args[0], armwVal: args[1], armwOrdering: ordering,
                   armwAlign: align)
    result = res
  of "__atomic_compare_exchange_n":
    let valTyp = args[2].typ
    let valAlign = atomicAlign(c, valTyp)
    let le = c.nextTemp()
    let leRes = llReg(le, valTyp)
    c.emit LLInstr(kind: llLoad, result: leRes, loadPtr: args[1])
    let t = c.nextTemp()
    let cmpxRes = llReg(t, LLType(kind: llStruct,
        structFields: @[LLStructField(typ: valTyp), LLStructField(typ: c.llI1())]))
    let aggText = "{ " & serialize(valTyp) & ", i1 }"
    c.emit LLInstr(kind: llCmpxchg, result: cmpxRes, cxPtr: args[0],
                   cxExpected: leRes, cxDesired: args[2], cxAggType: aggText,
                   cxSuccessOrdering: ordering, cxFailureOrdering: ordering,
                   cxAlign: valAlign)
    let success = c.nextTemp()
    let succRes = llReg(success, c.llI1())
    c.emit LLInstr(kind: llExtractValue, result: succRes, evAggregate: cmpxRes,
                   evAggType: aggText, evIndex: 1)
    let oldVal = c.nextTemp()
    let oldRes = llReg(oldVal, valTyp)
    c.emit LLInstr(kind: llExtractValue, result: oldRes, evAggregate: cmpxRes,
                   evAggType: aggText, evIndex: 0)
    c.emitStore(oldRes, args[1])
    let r = c.nextTemp()
    let rRes = llReg(r, c.llI8())
    c.emit LLInstr(kind: llZext, result: rRes, castOp: "zext", castSrc: succRes,
                   castDstType: c.llI8())
    result = rRes
  of "__atomic_add_fetch":
    let t = c.nextTemp()
    let res = llReg(t, retType)
    c.emit LLInstr(kind: llAtomicrmw, result: res, armwOp: llrmwAdd,
                   armwPtr: args[0], armwVal: args[1], armwOrdering: ordering,
                   armwAlign: align)
    let r = c.nextTemp()
    let rRes = llReg(r, retType)
    c.emit LLInstr(kind: llAdd, result: rRes, binOp: "add", binLhs: res,
        binRhs: args[1])
    result = rRes
  of "__atomic_sub_fetch":
    let t = c.nextTemp()
    let res = llReg(t, retType)
    c.emit LLInstr(kind: llAtomicrmw, result: res, armwOp: llrmwSub,
                   armwPtr: args[0], armwVal: args[1], armwOrdering: ordering,
                   armwAlign: align)
    let r = c.nextTemp()
    let rRes = llReg(r, retType)
    c.emit LLInstr(kind: llSub, result: rRes, binOp: "sub", binLhs: res,
        binRhs: args[1])
    result = rRes
  of "__atomic_fetch_add":
    let t = c.nextTemp()
    let res = llReg(t, retType)
    c.emit LLInstr(kind: llAtomicrmw, result: res, armwOp: llrmwAdd,
                   armwPtr: args[0], armwVal: args[1], armwOrdering: ordering,
                   armwAlign: align)
    result = res
  of "__atomic_fetch_sub":
    let t = c.nextTemp()
    let res = llReg(t, retType)
    c.emit LLInstr(kind: llAtomicrmw, result: res, armwOp: llrmwSub,
                   armwPtr: args[0], armwVal: args[1], armwOrdering: ordering,
                   armwAlign: align)
    result = res
  of "__atomic_fetch_and":
    let t = c.nextTemp()
    let res = llReg(t, retType)
    c.emit LLInstr(kind: llAtomicrmw, result: res, armwOp: llrmwAnd,
                   armwPtr: args[0], armwVal: args[1], armwOrdering: ordering,
                   armwAlign: align)
    result = res
  of "__atomic_fetch_or":
    let t = c.nextTemp()
    let res = llReg(t, retType)
    c.emit LLInstr(kind: llAtomicrmw, result: res, armwOp: llrmwOr,
                   armwPtr: args[0], armwVal: args[1], armwOrdering: ordering,
                   armwAlign: align)
    result = res
  of "__atomic_fetch_xor":
    let t = c.nextTemp()
    let res = llReg(t, retType)
    c.emit LLInstr(kind: llAtomicrmw, result: res, armwOp: llrmwXor,
                   armwPtr: args[0], armwVal: args[1], armwOrdering: ordering,
                   armwAlign: align)
    result = res
  of "__atomic_test_and_set":
    let t = c.nextTemp()
    let res = llReg(t, c.llI8())
    c.emit LLInstr(kind: llAtomicrmw, result: res, armwOp: llrmwXchg,
                   armwPtr: args[0], armwVal: llIntTextC("1", c.llI8()),
                   armwOrdering: ordering, armwAlign: 1)
    let r = c.nextTemp()
    let rRes = llReg(r, c.llI1())
    c.emit LLInstr(kind: llIcmp, result: rRes, icmpPred: "ne",
                   icmpLhs: res, icmpRhs: llIntTextC("0", c.llI8()))
    let r2 = c.nextTemp()
    let r2Res = llReg(r2, c.llI8())
    c.emit LLInstr(kind: llZext, result: r2Res, castOp: "zext", castSrc: rRes,
                   castDstType: c.llI8())
    result = r2Res
  of "__atomic_clear":
    c.emit LLInstr(kind: llStore, storeValue: llIntTextC("0", c.llI8()),
                   storePtr: args[0], storeAtomic: true,
                       storeOrdering: ordering,
                   storeAlign: 1)
    result = llNoneVal()
  of "__atomic_thread_fence":
    c.emit LLInstr(kind: llFence, fenceOrdering: ordering)
    result = llNoneVal()
  of "__atomic_signal_fence":
    c.emit LLInstr(kind: llFence, fenceOrdering: ordering,
                   fenceSyncscope: "singlethread")
    result = llNoneVal()
  else:
    let argStr = args.mapIt(operand(it)).join(", ")
    if retType.kind == llVoid:
      c.emit LLInstr(kind: llCall, callCallee: "@" & externName,
                     callRetType: retType, callArgs: args)
      result = llNoneVal()
    else:
      let t = c.nextTemp()
      let res = llReg(t, retType)
      c.emit LLInstr(kind: llCall, result: res, callCallee: "@" & externName,
                     callRetType: retType, callArgs: args)
      result = res

proc genMemIntrinsicCall(c: var LLVMCode; externName: string; args: seq[
    LLValue]; retType: LLType; result: var LLValue) =
  case externName
  of "memcpy":
    c.emit LLInstr(kind: llCall, callCallee: "@llvm.memcpy.p0.p0.i64",
      callRetType: c.primVoid,
      callArgs: @[args[0], args[1], args[2], llIntTextC("0", c.llI1())])
    declareExtern(c, "declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg)", "llvm.memcpy.p0.p0.i64")
    result = args[0].withType(c.primPtr)
  of "memmove":
    c.emit LLInstr(kind: llCall, callCallee: "@llvm.memmove.p0.p0.i64",
      callRetType: c.primVoid,
      callArgs: @[args[0], args[1], args[2], llIntTextC("0", c.llI1())])
    declareExtern(c, "declare void @llvm.memmove.p0.p0.i64(ptr nocapture writeonly, ptr nocapture readonly, i64, i1 immarg)", "llvm.memmove.p0.p0.i64")
    result = args[0].withType(c.primPtr)
  of "memset":
    let val8 = c.nextTemp()
    let val8Res = llReg(val8, c.llI8())
    c.emit LLInstr(kind: llTrunc, result: val8Res, castOp: "trunc",
                   castSrc: args[1], castDstType: c.llI8())
    c.emit LLInstr(kind: llCall, callCallee: "@llvm.memset.p0.i64",
      callRetType: c.primVoid,
      callArgs: @[args[0], val8Res, args[2], llIntTextC("0", c.llI1())])
    declareExtern(c, "declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg)", "llvm.memset.p0.i64")
    result = args[0].withType(c.primPtr)
  of "memcmp":
    let t = c.nextTemp()
    let res = llReg(t, c.llI32())
    c.emit LLInstr(kind: llCall, result: res, callCallee: "@memcmp",
      callRetType: c.llI32(), callArgs: @[args[0], args[1], args[2]])
    declareExtern(c, "declare i32 @memcmp(ptr nocapture, ptr nocapture, i64)", "memcmp")
    result = res
  else:
    let argStr = args.mapIt(operand(it)).join(", ")
    if retType.kind == llVoid:
      c.emit LLInstr(kind: llCall, callCallee: "@" & externName,
                     callRetType: retType, callArgs: args)
      result = llNoneVal()
    else:
      let t = c.nextTemp()
      let res = llReg(t, retType)
      c.emit LLInstr(kind: llCall, result: res, callCallee: "@" & externName,
                     callRetType: retType, callArgs: args)
      result = res

const
  AtomicBuiltins = [
    "__atomic_load_n", "__atomic_store_n", "__atomic_exchange_n",
    "__atomic_compare_exchange_n",
    "__atomic_add_fetch", "__atomic_sub_fetch",
    "__atomic_fetch_add", "__atomic_fetch_sub",
    "__atomic_fetch_and", "__atomic_fetch_or", "__atomic_fetch_xor",
    "__atomic_test_and_set", "__atomic_clear",
    "__atomic_thread_fence", "__atomic_signal_fence"
  ]
  MemIntrinsics = ["memcpy", "memmove", "memset", "memcmp"]
  GccBuiltins = [
    "__builtin_ctzll", "__builtin_ctz",
    "__builtin_clzll", "__builtin_clz",
    "__builtin_popcountll", "__builtin_popcount",
    "__builtin_bswap16", "__builtin_bswap32", "__builtin_bswap64",
    "__builtin_expect"
  ]

proc genGccBuiltinCall(c: var LLVMCode; externName: string; args: seq[LLValue];
    retType: LLType; result: var LLValue) =
  let isI32 = retType.kind == llInt and retType.intBits == 32
  template callIntrinsic(iname: string; rtype: LLType): LLValue =
    let t = c.nextTemp()
    let res = llReg(t, rtype)
    c.emit LLInstr(kind: llCall, result: res, callCallee: "@" & iname,
      callRetType: rtype, callArgs: @[args[0], llIntTextC("0", c.llI1())])
    res
  case externName
  of "__builtin_ctzll":
    let r = callIntrinsic("llvm.cttz.i64", c.llI64())
    if isI32:
      let t = c.nextTemp(); let res = llReg(t, c.llI32())
      c.emit LLInstr(kind: llTrunc, result: res, castOp: "trunc", castSrc: r,
          castDstType: c.llI32())
      result = res
    else: result = r
  of "__builtin_ctz":
    result = callIntrinsic("llvm.cttz.i32", c.llI32())
  of "__builtin_clzll":
    let r = callIntrinsic("llvm.ctlz.i64", c.llI64())
    if isI32:
      let t = c.nextTemp(); let res = llReg(t, c.llI32())
      c.emit LLInstr(kind: llTrunc, result: res, castOp: "trunc", castSrc: r,
          castDstType: c.llI32())
      result = res
    else: result = r
  of "__builtin_clz":
    result = callIntrinsic("llvm.ctlz.i32", c.llI32())
  of "__builtin_popcountll":
    let r = callIntrinsic("llvm.ctpop.i64", c.llI64())
    if isI32:
      let t = c.nextTemp(); let res = llReg(t, c.llI32())
      c.emit LLInstr(kind: llTrunc, result: res, castOp: "trunc", castSrc: r,
          castDstType: c.llI32())
      result = res
    else: result = r
  of "__builtin_popcount":
    result = callIntrinsic("llvm.ctpop.i32", c.llI32())
  of "__builtin_bswap16":
    result = callIntrinsic("llvm.bswap.i16", c.llI16())
  of "__builtin_bswap32":
    result = callIntrinsic("llvm.bswap.i32", c.llI32())
  of "__builtin_bswap64":
    result = callIntrinsic("llvm.bswap.i64", c.llI64())
  of "__builtin_expect":
    result = args[0]
  else:
    discard

proc genCallWithType(c: var LLVMCode; n: var Cursor; retType: LLType;
    result: var LLValue) =
  let callInfo = n.info
  n.into:

    var calleeName: string
    var calleeExtern: string = ""
    var calleeSym = SymId(0)
    if n.kind == Symbol:
      calleeSym = n.symId
      c.requestedSyms.incl calleeSym
      calleeExtern = getExternName(c, calleeSym)
      let decl = c.m.getDeclOrNil(calleeSym)
      if decl != nil and decl.kind == ProcY:
        calleeName = "@" & mangleSym(c, calleeSym)
        inc n
      else:
        var callee = LLValue(); genExprLLVM(c, n, callee)
        calleeName = disp(callee)
    else:
      var callee = LLValue(); genExprLLVM(c, n, callee)
      calleeName = disp(callee)

    var args: seq[LLValue] = @[]
    while n.hasMore:
      var arg = LLValue(); genExprLLVM(c, n, arg)
      args.add arg
    while n.hasMore: skip n

  if calleeExtern != "":
    if calleeExtern in AtomicBuiltins:
      genAtomicCall(c, calleeExtern, args, retType, result)
      return
    if calleeExtern in MemIntrinsics:
      genMemIntrinsicCall(c, calleeExtern, args, retType, result)
      return
    if calleeExtern in GccBuiltins:
      genGccBuiltinCall(c, calleeExtern, args, retType, result)
      return

  let funcType = if calleeName in c.varargsFuncTypes: c.varargsFuncTypes[
      calleeName] else: ""
  if retType.kind == llVoid:
    c.setLoc(callInfo)
    c.emit LLInstr(kind: llCall, callCallee: calleeName, callRetType: retType,
                   callArgs: args, callFuncType: funcType)
    result = llNoneVal()
  else:
    let t = c.nextTemp()
    let res = llReg(t, retType)
    c.setLoc(callInfo)
    c.emit LLInstr(kind: llCall, result: res, callCallee: calleeName,
                   callRetType: retType, callArgs: args, callFuncType: funcType)
    result = res

proc genCallLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  genCallWithType(c, n, c.primPtr, result)

proc genCallExprLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  genCallLLVM(c, n, result)

proc isLengInt(tk: LengType): bool {.inline.} =
  tk in {IT, UT, CT, BoolT, EnumT}

proc isLengFloat(tk: LengType): bool {.inline.} =
  tk == FT

proc isLengPtr(tk: LengType): bool {.inline.} =
  tk in {PtrT, AptrT, ProctypeT}

proc coerceValueLLVM(c: var LLVMCode; val: LLValue; srcTypeCursor, destTypeCursor: Cursor;
                     isCast: bool; result: var LLValue) =
  let destTyp = genTypeLLVMReadOnly(c, destTypeCursor)
  if typeEq(val.typ, destTyp):
    result = val
    return

  let srcTK = scalarTypeKind(c, srcTypeCursor)
  let destTK = scalarTypeKind(c, destTypeCursor)
  let t = c.nextTemp()

  if isLengPtr(srcTK) and isLengPtr(destTK):
    result = val.withType(destTyp)
    return
  elif isLengPtr(destTK) and isLengInt(srcTK):
    let res = llReg(t, destTyp)
    c.emit LLInstr(kind: llInttoptr, result: res, castOp: "inttoptr",
        castSrc: val, castDstType: destTyp)
    result = res
  elif isLengInt(destTK) and isLengPtr(srcTK):
    let res = llReg(t, destTyp)
    c.emit LLInstr(kind: llPtrtoint, result: res, castOp: "ptrtoint",
        castSrc: val, castDstType: destTyp)
    result = res
  elif isLengFloat(srcTK) and isLengFloat(destTK):
    let srcBits = typeSizeBits(c, srcTypeCursor)
    let destBits = typeSizeBits(c, destTypeCursor)
    let res = llReg(t, destTyp)
    if srcBits < destBits:
      c.emit LLInstr(kind: llFpext, result: res, castOp: "fpext", castSrc: val,
          castDstType: destTyp)
    else:
      c.emit LLInstr(kind: llFptrunc, result: res, castOp: "fptrunc",
          castSrc: val, castDstType: destTyp)
    result = res
  elif isLengFloat(srcTK) and isLengInt(destTK):
    let res = llReg(t, destTyp)
    c.emit LLInstr(kind: llFptosi, result: res, castOp: "fptosi", castSrc: val,
        castDstType: destTyp)
    result = res
  elif isLengInt(srcTK) and isLengFloat(destTK):
    let res = llReg(t, destTyp)
    c.emit LLInstr(kind: llSitofp, result: res, castOp: "sitofp", castSrc: val,
        castDstType: destTyp)
    result = res
  elif isLengInt(srcTK) and isLengInt(destTK):
    let srcBits = typeSizeBits(c, srcTypeCursor)
    let destBits = typeSizeBits(c, destTypeCursor)
    if srcBits < destBits:
      let res = llReg(t, destTyp)
      if isCast or srcTK in {UT, CT, BoolT}:
        c.emit LLInstr(kind: llZext, result: res, castOp: "zext", castSrc: val,
            castDstType: destTyp)
      else:
        c.emit LLInstr(kind: llSext, result: res, castOp: "sext", castSrc: val,
            castDstType: destTyp)
      result = res
    elif srcBits > destBits:
      let res = llReg(t, destTyp)
      c.emit LLInstr(kind: llTrunc, result: res, castOp: "trunc", castSrc: val,
          castDstType: destTyp)
      result = res
    else:
      result = val.withType(destTyp)
      return
  else:
    var destResolved = navigateToObjectBody(c.m, destTypeCursor)
    let anonDestType = genTypeLLVMReadOnly(c, destResolved)
    if typeEq(anonDestType, val.typ):
      result = val.withType(destTyp)
      return
    let res = llReg(t, destTyp)
    c.emit LLInstr(kind: llBitcast, result: res, castOp: "bitcast",
        castSrc: val, castDstType: destTyp)
    result = res

proc genConvOrCast(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  let isCast = n.exprKind == CastC
  n.into:
    let destTypeCursor = n
    skip n
    let srcTypeCursor = getType(c.m, n)
    var val = LLValue(); genExprLLVM(c, n, val)
    coerceValueLLVM(c, val, srcTypeCursor, destTypeCursor, isCast, result)
    while n.hasMore: skip n

proc genAddrLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  var lval = LLValue()
  n.into:
    genLvalueLLVM(c, n, lval)
    if n.hasMore and n.typeQual == CppRefQ:
      skip n
    while n.hasMore: skip n
  result = lval.withType(c.primPtr)

proc genFieldPtrLLVM(c: var LLVMCode; objBody: Cursor; fldSym: SymId;
                     gepType: LLType; gepTarget: LLValue): LLValue =
  let access = fieldAccessLLVM(c, objBody, fldSym)
  let idx0 = llIntTextC("0", c.llI32())
  if access.isBranch:
    let unionPtr = c.emitGEP(gepType, gepTarget, [idx0, llIntTextC(
        $access.index, c.llI32())])
    result = c.emitGEP(access.branchType, unionPtr, [idx0, llIntTextC(
        $access.branchIndex, c.llI32())])
  else:
    result = c.emitGEP(gepType, gepTarget, [idx0, llIntTextC($access.index,
        c.llI32())])

proc emitBaseGEP(c: var LLVMCode; objType: LLType; baseValue: LLValue;
                 nifBody: var Cursor; inhDepth: int): (LLType, LLValue) =
  result = (objType, baseValue)
  var curBody = nifBody
  for i in 0 ..< inhDepth:
    let baseTypeCursor = baseTypeOfObject(c.m, curBody)
    result = (genTypeLLVMReadOnly(c, baseTypeCursor),
              c.emitGEP(result[0], result[1],
                        [llIntTextC("0", c.llI32()), llIntTextC("0", c.llI32())]))
    if not cursorIsNil(baseTypeCursor):
      curBody = navigateToObjectBody(c.m, baseTypeCursor)
  nifBody = curBody

proc genDotLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  var objType: Cursor
  var obj = LLValue()
  var fldSym: SymId
  var inhDepth = 0
  n.into:
    objType = getNominalType(c.m, n)
    genLvalueLLVM(c, n, obj)
    fldSym = n.symId
    skip n
    if n.kind == IntLit:
      inhDepth = int(intVal(n))
      inc n
    while n.hasMore: skip n

  var curType = objType
  var curBody = navigateToObjectBody(c.m, curType)
  let objTypeLL = genTypeLLVMReadOnly(c, curType)
  var gepTarget = obj
  var gepType = objTypeLL
  for i in 0 ..< inhDepth:
    gepTarget = c.emitGEP(gepType, gepTarget, [llIntTextC("0", c.llI32()),
        llIntTextC("0", c.llI32())])
    let baseTypeCursor = baseTypeOfObject(c.m, curBody)
    if not cursorIsNil(baseTypeCursor):
      curType = baseTypeCursor
      curBody = navigateToObjectBody(c.m, curType)
      gepType = genTypeLLVMReadOnly(c, curType)
  result = genFieldPtrLLVM(c, curBody, fldSym, gepType, gepTarget).withType(c.primPtr)

proc genAtLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  var arrType: Cursor
  var arr = LLValue()
  var idx = LLValue()
  n.into:
    arrType = getType(c.m, n)
    genLvalueLLVM(c, n, arr)
    genExprLLVM(c, n, idx)
    while n.hasMore: skip n
  let arrTypeName = genTypeLLVMReadOnly(c, arrType)
  result = c.emitGEP(arrTypeName, arr,
            [llIntTextC("0", c.llI32()), idx]).withType(c.primPtr)

proc genPatLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  var baseType: Cursor
  var base = LLValue()
  var idx = LLValue()
  n.into:
    baseType = getType(c.m, n)
    genExprLLVM(c, n, base)
    genExprLLVM(c, n, idx)
    while n.hasMore: skip n
  let elemCursor = pointeeType(c, baseType)
  if cursorIsNil(elemCursor):
    error c.m, "expected pointer type for `pat` but got: ", baseType
  let elemType = genTypeLLVMReadOnly(c, elemCursor)
  result = c.emitGEP(elemType, base, [idx], inbounds = false).withType(c.primPtr)

proc genSizeofLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  var typ: LLType
  n.into:
    typ = genTypeLLVM(c, n)
    while n.hasMore: skip n
  let t1 = c.emitGEP(typ, llNull(c.primPtr), [llIntTextC("1", c.llI32())],
      inbounds = false)
  let t2 = c.nextTemp()
  let res = llReg(t2, c.llIntBits(c.bits))
  c.emit LLInstr(kind: llPtrtoint, result: res, castOp: "ptrtoint", castSrc: t1,
                 castDstType: c.llIntBits(c.bits))
  result = res

proc isGlobalSym(c: var LLVMCode; s: SymId): bool =
  let d = c.m.getDeclOrNil(s)
  result = (d != nil and d.kind in {GvarY, TvarY, ConstY, ProcY}) or
           s in c.emittedConsts

proc genLvalueLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  case n.exprKind
  of NoExpr:
    if n.kind == Symbol:
      let s = n.symId
      c.requestedSyms.incl s
      let name = mangleSym(c, s)
      inc n
      if isGlobalSym(c, s):
        result = llGlobalRef(name, c.primPtr)
      else:
        result = llReg(name, c.primPtr)
    else:
      error c.m, "expected expression but got: ", n
  of DerefC:
    var ptrVal = LLValue()
    n.into:
      genExprLLVM(c, n, ptrVal)
      if n.hasMore and n.typeQual == CppRefQ:
        skip n
      while n.hasMore: skip n
    result = ptrVal.withType(c.primPtr)
  of AtC:
    genAtLLVM(c, n, result)
  of PatC:
    genPatLLVM(c, n, result)
  of DotC:
    genDotLLVM(c, n, result)
  of ErrvC:
    result = llGlobalRef("LENGC_ERR_", c.primPtr)
    skip n
  of OvfC:
    result = llGlobalRef("LENGC_OVF_", c.primPtr)
    skip n
  of BaseobjC:
    n.into:
      skip n
      skip n
      genLvalueLLVM(c, n, result)
      while n.hasMore: skip n
  of SufC, ParC, AddrC, NilC, InfC, NeginfC, NanC, FalseC, TrueC,
     AndC, OrC, NotC, NegC, SizeofC, AlignofC, OffsetofC,
     OconstrC, AconstrC,
     AddC, SubC, MulC, DivC, ModC, ShrC, ShlC,
     BitandC, BitorC, BitxorC, BitnotC,
     EqC, NeqC, LeC, LtC, CastC, ConvC, CallC:
    error c.m, "not an lvalue: ", n

proc genExprLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  case n.exprKind
  of NoExpr:
    case n.kind
    of IntLit:
      let i = intVal(n)
      inc n
      result = llIntTextC($i, c.llIntBits(c.bits))
    of UIntLit:
      let i = uintVal(n)
      inc n
      result = llIntTextC($i, c.llIntBits(c.bits))
    of FloatLit:
      let f = floatVal(n)
      inc n
      result = LLValue(kind: llvFloat, floatText: $f, typ: c.primDouble)
    of CharLit:
      let ch = n.charLit
      inc n
      result = llIntTextC($ord(ch), c.llI8())
    of StrLit:
      let s = c.m.pool.strings[n.litId]
      inc n
      let globalName = ".str." & $c.strLitCounter
      inc c.strLitCounter
      var escaped = newStringOfCap(s.len + 10)
      for ch in s:
        let o = ord(ch)
        if o < 32 or o > 126 or ch == '"' or ch == '\\':
          escaped.add '\\'
          escaped.add "0123456789ABCDEF"[o shr 4]
          escaped.add "0123456789ABCDEF"[o and 0xF]
        else:
          escaped.add ch
      let arrTyp = newLLArrayType(s.len + 1, c.llI8())
      c.module.globals.add LLGlobal(name: globalName,
        typ: arrTyp, initVal: LLValue(kind: llvCString,
          strVal: escaped & "\\00", typ: arrTyp), isConstant: true, isPrivate: true)
      result = llGlobalRef(globalName, c.primPtr)
    of Symbol:
      let s = n.symId
      c.requestedSyms.incl s
      let name = mangleSym(c, s)
      let symType = getType(c.m, n)
      let decl = c.m.getDeclOrNil(s)
      inc n
      if symType.typeKind == EnumT and decl != nil and decl.kind notin {VarY,
          GvarY, TvarY}:
        var enumBody = symType
        inc enumBody
        let baseTyp = genTypeLLVMReadOnly(c, enumBody)
        skip enumBody
        while enumBody.hasMore:
          if enumBody.substructureKind == EfldU:
            enumBody.into:
              if enumBody.kind == SymbolDef and enumBody.symId == s:
                inc enumBody
                if enumBody.kind == IntLit:
                  result = llIntTextC($intVal(enumBody), baseTyp)
                elif enumBody.kind == UIntLit:
                  result = llIntTextC($uintVal(enumBody), baseTyp)
                else:
                  result = llIntTextC("0", baseTyp)
                return
              while enumBody.hasMore: skip enumBody
          else:
            skip enumBody
        result = llIntTextC("0", baseTyp)
        return
      if decl != nil and decl.kind == ProcY:
        let typ = genTypeLLVMReadOnly(c, symType)
        result = llGlobalRef(name, typ)
        return
      let typ = genTypeLLVMReadOnly(c, symType)
      let prefix = if isGlobalSym(c, s): "@" else: "%"
      let base = if prefix == "@": llGlobalRef(name, c.primPtr)
                 else: llReg(name, c.primPtr)
      result = c.emitLoad(base, typ)
    else:
      let exprType = getType(c.m, n)
      let typ = genTypeLLVMReadOnly(c, exprType)
      var lval = LLValue(); genLvalueLLVM(c, n, lval)
      result = c.emitLoad(lval, typ)
  of FalseC:
    skip n
    result = llIntTextC("0", c.llI8())
  of TrueC:
    skip n
    result = llIntTextC("1", c.llI8())
  of NilC:
    skip n
    result = llNull(c.primPtr)
  of InfC:
    skip n
    result = LLValue(kind: llvFloat, floatText: "0x7FF0000000000000",
        typ: c.primDouble)
  of NegInfC:
    skip n
    result = LLValue(kind: llvFloat, floatText: "0xFFF0000000000000",
        typ: c.primDouble)
  of NanC:
    skip n
    result = LLValue(kind: llvFloat, floatText: "0x7FF8000000000000",
        typ: c.primDouble)
  of AddC: signedBinOp(c, n, "add", result)
  of SubC: signedBinOp(c, n, "sub", result)
  of MulC: signedBinOp(c, n, "mul", result)
  of DivC: unsignedBinOp(c, n, "sdiv", "udiv", result)
  of ModC: unsignedBinOp(c, n, "srem", "urem", result)
  of ShlC: signedBinOp(c, n, "shl", result)
  of ShrC: unsignedBinOp(c, n, "ashr", "lshr", result)
  of BitandC: signedBinOp(c, n, "and", result)
  of BitorC: signedBinOp(c, n, "or", result)
  of BitxorC: signedBinOp(c, n, "xor", result)
  of BitnotC:
    var typ: LLType
    var val = LLValue()
    n.into:
      typ = genTypeLLVM(c, n)
      genExprLLVM(c, n, val)
      while n.hasMore: skip n
    let t = c.nextTemp()
    let res = llReg(t, typ)
    c.emit LLInstr(kind: llXor, result: res, binOp: "xor", binLhs: val,
                   binRhs: llIntTextC("-1", typ))
    result = res
  of NegC:
    var typ: LLType
    var val = LLValue()
    n.into:
      typ = genTypeLLVM(c, n)
      genExprLLVM(c, n, val)
      while n.hasMore: skip n
    let t = c.nextTemp()
    let res = llReg(t, typ)
    if isFloatType(typ):
      c.emitRaw "%" & t & " = fneg " & serialize(typ) & " " & disp(val)
      result = res
    else:
      c.emit LLInstr(kind: llSub, result: res, binOp: "sub",
                     binLhs: llIntTextC("0", typ), binRhs: val)
      result = res
  of EqC: genBoolCmpOp(c, n, "eq", "eq", result)
  of NeqC: genBoolCmpOp(c, n, "ne", "ne", result)
  of LeC: genBoolCmpOp(c, n, "sle", "ule", result)
  of LtC: genBoolCmpOp(c, n, "slt", "ult", result)
  of AndC:
    let andInfo = n.info
    let resName = c.nextTemp()
    var lhs = LLValue()
    var rhs = LLValue()
    n.into:
      c.emitAlloca(resName, c.llI8())
      c.emitStore(llIntTextC("0", c.llI8()), llReg(resName, c.primPtr))
      genExprLLVM(c, n, lhs)
      let lhsBool = c.nextTemp()
      let lhsBoolRes = llReg(lhsBool, c.llI1())
      c.setLoc(andInfo)
      c.emit LLInstr(kind: llIcmp, result: lhsBoolRes, icmpPred: "ne",
                     icmpLhs: lhs, icmpRhs: zeroVal(lhs.typ))
      let rhsLabel = c.nextLabel()
      let endLabel = c.nextLabel()
      c.emit LLInstr(kind: llCondBr, condBrCond: lhsBoolRes,
                     condBrTrue: rhsLabel, condBrFalse: endLabel)
      discard c.startBlock(rhsLabel)
      genExprLLVM(c, n, rhs)
      let rhsBool = c.nextTemp()
      let rhsBoolRes = llReg(rhsBool, c.llI1())
      c.setLoc(andInfo)
      c.emit LLInstr(kind: llIcmp, result: rhsBoolRes, icmpPred: "ne",
                     icmpLhs: rhs, icmpRhs: zeroVal(rhs.typ))
      let rhsExt = c.nextTemp()
      let rhsExtRes = llReg(rhsExt, c.llI8())
      c.emit LLInstr(kind: llZext, result: rhsExtRes, castOp: "zext",
                     castSrc: rhsBoolRes, castDstType: c.llI8())
      c.emitStore(rhsExtRes, llReg(resName, c.primPtr))
      c.setLoc(andInfo)
      c.emit LLInstr(kind: llBr, brTarget: endLabel)
      discard c.startBlock(endLabel)
      result = c.emitLoad(llReg(resName, c.primPtr), c.llI8())
      while n.hasMore: skip n
  of OrC:
    let orInfo = n.info
    let resName = c.nextTemp()
    var lhs = LLValue()
    var rhs = LLValue()
    n.into:
      c.emitAlloca(resName, c.llI8())
      c.emitStore(llIntTextC("1", c.llI8()), llReg(resName, c.primPtr))
      genExprLLVM(c, n, lhs)
      let lhsBool = c.nextTemp()
      let lhsBoolRes = llReg(lhsBool, c.llI1())
      c.setLoc(orInfo)
      c.emit LLInstr(kind: llIcmp, result: lhsBoolRes, icmpPred: "ne",
                     icmpLhs: lhs, icmpRhs: zeroVal(lhs.typ))
      let rhsLabel = c.nextLabel()
      let endLabel = c.nextLabel()
      c.emit LLInstr(kind: llCondBr, condBrCond: lhsBoolRes,
                     condBrTrue: endLabel, condBrFalse: rhsLabel)
      discard c.startBlock(rhsLabel)
      genExprLLVM(c, n, rhs)
      let rhsBool = c.nextTemp()
      let rhsBoolRes = llReg(rhsBool, c.llI1())
      c.setLoc(orInfo)
      c.emit LLInstr(kind: llIcmp, result: rhsBoolRes, icmpPred: "ne",
                     icmpLhs: rhs, icmpRhs: zeroVal(rhs.typ))
      let rhsExt = c.nextTemp()
      let rhsExtRes = llReg(rhsExt, c.llI8())
      c.emit LLInstr(kind: llZext, result: rhsExtRes, castOp: "zext",
                     castSrc: rhsBoolRes, castDstType: c.llI8())
      c.emitStore(rhsExtRes, llReg(resName, c.primPtr))
      c.setLoc(orInfo)
      c.emit LLInstr(kind: llBr, brTarget: endLabel)
      discard c.startBlock(endLabel)
      result = c.emitLoad(llReg(resName, c.primPtr), c.llI8())
      while n.hasMore: skip n
  of NotC:
    let notInfo = n.info
    var val = LLValue()
    n.into:
      genExprLLVM(c, n, val)
      while n.hasMore: skip n
    let t1 = c.nextTemp()
    let t1Res = llReg(t1, c.llI1())
    c.setLoc(notInfo)
    c.emit LLInstr(kind: llIcmp, result: t1Res, icmpPred: "eq",
                   icmpLhs: val, icmpRhs: zeroVal(val.typ))
    let t2 = c.nextTemp()
    let t2Res = llReg(t2, c.llI8())
    c.emit LLInstr(kind: llZext, result: t2Res, castOp: "zext", castSrc: t1Res,
                   castDstType: c.llI8())
    result = t2Res
  of CastC, ConvC:
    genConvOrCast(c, n, result)
  of CallC:
    let retTypeCursor = getType(c.m, n)
    let retType = genTypeLLVMReadOnly(c, retTypeCursor)
    genCallWithType(c, n, retType, result)
  of AddrC:
    genAddrLLVM(c, n, result)
  of DerefC:
    let derefType = getType(c.m, n)
    let loadType = genTypeLLVMReadOnly(c, derefType)
    var ptrVal = LLValue()
    n.into:
      genExprLLVM(c, n, ptrVal)
      if n.hasMore and n.typeQual == CppRefQ:
        skip n
      while n.hasMore: skip n
    result = c.emitLoad(ptrVal, loadType)
  of AtC:
    let elemType = getType(c.m, n)
    let loadType = genTypeLLVMReadOnly(c, elemType)
    var lval = LLValue(); genAtLLVM(c, n, lval)
    result = c.emitLoad(lval, loadType)
  of PatC:
    let elemType = getType(c.m, n)
    let loadType = genTypeLLVMReadOnly(c, elemType)
    var lval = LLValue(); genPatLLVM(c, n, lval)
    result = c.emitLoad(lval, loadType)
  of DotC:
    let fldType = getType(c.m, n)
    let loadType = genTypeLLVMReadOnly(c, fldType)
    var lval = LLValue(); genDotLLVM(c, n, lval)
    if fldType.typeKind == FlexarrayT:
      result = lval.withType(c.primPtr)
    else:
      result = c.emitLoad(lval, loadType)
  of SizeofC:
    genSizeofLLVM(c, n, result)
  of AlignofC:
    genSizeofLLVM(c, n, result)
  of OffsetofC:
    var typCursor: Cursor
    var fldSym: SymId
    var typ: LLType
    n.into:
      typCursor = n
      typ = genTypeLLVM(c, n)
      fldSym = n.symId
      inc n
      while n.hasMore: skip n
    let objBody = navigateToObjectBody(c.m, typCursor)
    let fldIdx = fieldIndex(c, objBody, fldSym)
    let t1 = c.emitGEP(typ, llNull(c.primPtr),
            [llIntTextC("0", c.llI32()), llIntTextC($fldIdx, c.llI32())],
                inbounds = false)
    let t2 = c.nextTemp()
    let res = llReg(t2, c.llIntBits(c.bits))
    c.emit LLInstr(kind: llPtrtoint, result: res, castOp: "ptrtoint", castSrc: t1,
                   castDstType: c.llIntBits(c.bits))
    result = res
  of ParC:
    n.into:
      genExprLLVM(c, n, result)
      while n.hasMore: skip n
  of SufC:
    var value: Cursor
    var suffix: Cursor
    var suffixStr: string
    n.into:
      value = n
      skip n
      suffix = n
      suffixStr = c.m.pool.strings[suffix.litId]
      while n.hasMore: skip n
    if value.kind == StrLit:
      genExprLLVM(c, value, result)
    else:
      var val = LLValue(); genExprLLVM(c, value, val)
      var targetTyp = c.llI64()
      case suffixStr
      of "i64", "u64": targetTyp = c.llI64()
      of "i32", "u32": targetTyp = c.llI32()
      of "i16", "u16": targetTyp = c.llI16()
      of "i8", "u8": targetTyp = c.llI8()
      of "f64": targetTyp = c.primDouble
      of "f32": targetTyp = c.primFloat
      else: discard
      if typeEq(val.typ, targetTyp):
        result = val
      else:
        let t = c.nextTemp()
        let res = llReg(t, targetTyp)
        if isFloatType(targetTyp):
          c.emit LLInstr(kind: llSitofp, result: res, castOp: "sitofp",
                         castSrc: val, castDstType: targetTyp)
        else:
          let destBits = case suffixStr
            of "i8", "u8": 8
            of "i16", "u16": 16
            of "i32", "u32": 32
            else: 64
          if c.bits < destBits:
            c.emit LLInstr(kind: llSext, result: res, castOp: "sext",
                           castSrc: val, castDstType: targetTyp)
          elif c.bits > destBits:
            c.emit LLInstr(kind: llTrunc, result: res, castOp: "trunc",
                           castSrc: val, castDstType: targetTyp)
          else:
            result = val
            return
        result = res
  of OconstrC:
    var objTypeCursor: Cursor
    var typ: LLType
    var tmpName: string
    n.into:
      objTypeCursor = n
      typ = genTypeLLVM(c, n)
      tmpName = c.nextTemp()
      c.emitAlloca(tmpName, typ)
      c.emitStore(llZeroInit(typ), llReg(tmpName, c.primPtr))
      while n.hasMore:
        if n.substructureKind == KvU:
          n.into:
            let fldSym = n.symId
            skip n
            var fieldVal = LLValue(); genExprLLVM(c, n, fieldVal)
            let inhDepth = if n.hasMore and n.kind == IntLit: (let d = int(
                intVal(n)); skip n; d) else: 0
            var curType = objTypeCursor
            var curBody = navigateToObjectBody(c.m, curType)
            var gepType = genTypeLLVMReadOnly(c, curType)
            var gepTarget = llReg(tmpName, c.primPtr)
            for i in 0 ..< inhDepth:
              gepTarget = c.emitGEP(gepType, gepTarget,
                [llIntTextC("0", c.llI32()), llIntTextC("0", c.llI32())])
              let baseTypeCursor = baseTypeOfObject(c.m, curBody)
              if not cursorIsNil(baseTypeCursor):
                curType = baseTypeCursor
                curBody = navigateToObjectBody(c.m, curType)
                gepType = genTypeLLVMReadOnly(c, curType)
            let fldPtr = genFieldPtrLLVM(c, curBody, fldSym, gepType, gepTarget)
            c.emitStore(fieldVal, fldPtr)
            while n.hasMore: skip n
        else:
          var baseVal = LLValue(); genExprLLVM(c, n, baseVal)
          let basePtr = c.emitGEP(typ, llReg(tmpName, c.primPtr),
            [llIntTextC("0", c.llI32()), llIntTextC("0", c.llI32())])
          c.emitStore(baseVal, basePtr)
      result = c.emitLoad(llReg(tmpName, c.primPtr), typ)
  of AconstrC:
    var arrayTypeCursor: Cursor
    var typ: LLType
    var expectedLen: int
    n.into:
      arrayTypeCursor = n
      typ = genTypeLLVM(c, n)
      expectedLen = fixedArrayLen(c, arrayTypeCursor)
      var elemBody = navigateToObjectBody(c.m, arrayTypeCursor)
      inc elemBody
      let elemTyp = genTypeLLVMReadOnly(c, elemBody)
      var current = llUndef(typ)
      var idx = 0
      while n.hasMore:
        var elemVal = LLValue(); genExprLLVM(c, n, elemVal)
        let t = c.nextTemp()
        let res = llReg(t, typ)
        c.emit LLInstr(kind: llInsertValue, result: res, ivAggregate: current,
                       ivElement: elemVal, ivAggType: serialize(typ), ivIndex: idx)
        current = res
        inc idx
      if expectedLen >= 0 and idx != expectedLen:
        error c.m, "array literal element count does not match its declared length: ", arrayTypeCursor
      result = current
  of BaseobjC:
    let loadType = genTypeLLVMReadOnly(c, getType(c.m, n))
    var lval = LLValue(); genLvalueLLVM(c, n, lval)
    result = c.emitLoad(lval, loadType)
  of ErrvC, OvfC:
    var lval = LLValue(); genLvalueLLVM(c, n, lval)
    result = c.emitLoad(lval, c.llI8())

proc genCondLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  let condInfo = n.info
  genExprLLVM(c, n, result)
  if result.typ == c.llI1():
    return
  let t = c.nextTemp()
  let res = llReg(t, c.llI1())
  c.setLoc(condInfo)
  c.emit LLInstr(kind: llIcmp, result: res, icmpPred: "ne",
                 icmpLhs: result, icmpRhs: zeroVal(result.typ))
  result = res
