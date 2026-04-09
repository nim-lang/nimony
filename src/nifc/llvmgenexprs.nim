#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included from llvmcodegen.nim
# Generates LLVM IR for expressions. Each expression returns an LLValue
# (a name + type pair) and may emit instructions to c.body.

proc genExprLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue)
proc genLvalueLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue)
proc coerceValueLLVM(c: var LLVMCode; val: LLValue; srcTypeCursor, destTypeCursor: Cursor;
                     isCast: bool; result: var LLValue)

proc zeroVal(typ: LToken): string {.inline.} =
  ## Return the zero constant for a given LLVM type token.
  if typ == LToken(PtrToken): "null" else: "0"

proc isFloatType(t: LToken): bool {.inline.} =
  t == LToken(FloatToken) or t == LToken(DoubleToken) or t == LToken(Fp128Token)

proc scalarTypeKind(c: var LLVMCode; typ: Cursor): NifcType =
  var t = navigateToObjectBody(c.m, typ)
  if t.typeKind == EnumT:
    inc t
    t = navigateToObjectBody(c.m, t)
  result = t.typeKind

proc pointeeType(c: var LLVMCode; typ: Cursor): Cursor =
  let t = navigateToObjectBody(c.m, typ)
  if t.typeKind in {PtrT, APtrT, FlexarrayT}:
    result = t.firstSon
  else:
    result = default(Cursor)

proc genCallExprLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue)

proc signedBinOp(c: var LLVMCode; n: var Cursor; op: string; result: var LLValue) =
  ## Typed binary op: (op type lhs rhs)
  inc n
  let typCursor = n
  let typ = genTypeLLVM(c, n)
  let srcLhs = getType(c.m, n)
  var lhs = LLValue(); genExprLLVM(c, n, lhs)
  let srcRhs = getType(c.m, n)
  var rhs = LLValue(); genExprLLVM(c, n, rhs)
  let typTok = c.tok(typ)
  if lhs.typ != typTok:
    coerceValueLLVM(c, lhs, srcLhs, typCursor, true, lhs)
  if rhs.typ != typTok:
    coerceValueLLVM(c, rhs, srcRhs, typCursor, true, rhs)
  let t = c.temp()
  c.emitLine "  " & c.str(t) & " = " & op & " " & typ & " " & c.str(lhs.name) & ", " & c.str(rhs.name)
  skipParRi n
  result = LLValue(name: t, typ: typTok)

proc unsignedBinOp(c: var LLVMCode; n: var Cursor; signedOp, unsignedOp: string; result: var LLValue) =
  ## Binary op that differs for signed/unsigned: checks the NIF type tag.
  inc n
  let isUnsigned = n.typeKind == UT
  let typCursor = n
  let typ = genTypeLLVM(c, n)
  let srcLhs = getType(c.m, n)
  var lhs = LLValue(); genExprLLVM(c, n, lhs)
  let srcRhs = getType(c.m, n)
  var rhs = LLValue(); genExprLLVM(c, n, rhs)
  let typTok = c.tok(typ)
  if lhs.typ != typTok:
    coerceValueLLVM(c, lhs, srcLhs, typCursor, true, lhs)
  if rhs.typ != typTok:
    coerceValueLLVM(c, rhs, srcRhs, typCursor, true, rhs)
  let t = c.temp()
  let op = if isUnsigned: unsignedOp else: signedOp
  c.emitLine "  " & c.str(t) & " = " & op & " " & typ & " " & c.str(lhs.name) & ", " & c.str(rhs.name)
  skipParRi n
  result = LLValue(name: t, typ: typTok)

proc cmpOp(c: var LLVMCode; n: var Cursor; signedPred, unsignedPred: string; result: var LLValue) =
  ## Comparison op: (op lhs rhs) → i1
  inc n
  let lhsExpr = n
  let lhsType = getType(c.m, lhsExpr)
  let lhsTK = scalarTypeKind(c, lhsType)
  var lhs = LLValue(); genExprLLVM(c, n, lhs)
  var rhs = LLValue(); genExprLLVM(c, n, rhs)
  let t = c.temp()
  # Determine if float or int comparison
  let typ = c.str(lhs.typ)
  if typ in ["float", "double", "fp128"]:
    let fpPred = case signedPred
      of "eq": "oeq"
      of "ne": "one"
      of "slt": "olt"
      of "sle": "ole"
      else: signedPred
    c.emitLine "  " & c.str(t) & " = fcmp " & fpPred & " " & typ & " " & c.str(lhs.name) & ", " & c.str(rhs.name)
  else:
    let pred = if unsignedPred != "" and lhsTK in {UT, CT, BoolT}:
                 unsignedPred
               else:
                 signedPred
    c.emitLine "  " & c.str(t) & " = icmp " & pred & " " & typ & " " & c.str(lhs.name) & ", " & c.str(rhs.name)
  skipParRi n
  result = LLValue(name: t, typ: LToken(I8Token))

proc genBoolCmpOp(c: var LLVMCode; n: var Cursor; signedPred, unsignedPred: string; result: var LLValue) =
  ## Like cmpOp but extends result to i8 for NIF bool compatibility.
  var cmp = LLValue(); cmpOp(c, n, signedPred, unsignedPred, cmp)
  let t = c.temp()
  c.emitLine "  " & c.str(t) & " = zext i1 " & c.str(cmp.name) & " to i8"
  result = LLValue(name: t, typ: LToken(I8Token))

proc getExternName(c: var LLVMCode; s: SymId): string =
  ## Get the importc name for a symbol, or "" if not imported.
  let d = c.m.getDeclOrNil(s)
  if d != nil and d.extern != StrId(0) and d.isImport:
    result = pool.strings[d.extern]
  else:
    result = ""

proc memoryOrderToLLVM(val: LLValue): string =
  ## Map GCC __ATOMIC_* memory order constant to LLVM ordering keyword.
  ## __ATOMIC_RELAXED=0, __ATOMIC_CONSUME=1, __ATOMIC_ACQUIRE=2,
  ## __ATOMIC_RELEASE=3, __ATOMIC_ACQ_REL=4, __ATOMIC_SEQ_CST=5
  # For now, always use seq_cst (the safe default).
  # A more sophisticated version would inspect the value.
  result = "seq_cst"

proc atomicAlign(c: var LLVMCode; typ: LToken): string {.inline.} =
  ## Return the alignment in bytes for atomic operations.
  if typ == LToken(PtrToken): $(c.bits div 8)
  elif typ == LToken(I8Token): "1"
  elif typ == LToken(I16Token): "2"
  elif typ == LToken(I32Token): "4"
  elif typ == LToken(I64Token): "8"
  else: $(c.bits div 8)

proc genAtomicCall(c: var LLVMCode; externName: string; args: seq[LLValue]; retType: string; result: var LLValue) =
  ## Translate __atomic_* GCC builtins to LLVM atomic instructions.
  let ordering = memoryOrderToLLVM(args[^1]) # last arg is always memory order
  let align = atomicAlign(c, c.tok(retType))
  case externName
  of "__atomic_load_n":
    # __atomic_load_n(ptr, memorder) -> LLVM: load atomic type, ptr p ordering
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = load atomic " & retType & ", ptr " & c.str(args[0].name) & " " & ordering & ", align " & align
    result = LLValue(name: t, typ: c.tok(retType))
  of "__atomic_store_n":
    # __atomic_store_n(ptr, val, memorder) -> LLVM: store atomic type val, ptr p ordering
    c.emitLine "  store atomic " & c.str(args[1].typ) & " " & c.str(args[1].name) & ", ptr " & c.str(args[0].name) & " " & ordering & ", align " & align
    result = LLValue(name: LToken(EmptyToken), typ: LToken(VoidToken))
  of "__atomic_exchange_n":
    # __atomic_exchange_n(ptr, val, memorder) -> LLVM: atomicrmw xchg
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = atomicrmw xchg ptr " & c.str(args[0].name) & ", " & c.str(args[1].typ) & " " & c.str(args[1].name) & " " & ordering
    result = LLValue(name: t, typ: c.tok(retType))
  of "__atomic_compare_exchange_n":
    # __atomic_compare_exchange_n(ptr, expected_ptr, desired, weak, succ_order, fail_order)
    # -> LLVM: cmpxchg ptr, old, new ordering ordering
    let loadExpected = c.temp()
    c.emitLine "  " & c.str(loadExpected) & " = load " & retType & ", ptr " & c.str(args[1].name)
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = cmpxchg ptr " & c.str(args[0].name) & ", " & retType & " " & c.str(loadExpected) & ", " & c.str(args[2].typ) & " " & c.str(args[2].name) & " " & ordering & " " & ordering
    # cmpxchg returns {T, i1}; extract the success flag
    let success = c.temp()
    c.emitLine "  " & c.str(success) & " = extractvalue { " & retType & ", i1 } " & c.str(t) & ", 1"
    # Store back the old value into the expected pointer on failure
    let oldVal = c.temp()
    c.emitLine "  " & c.str(oldVal) & " = extractvalue { " & retType & ", i1 } " & c.str(t) & ", 0"
    c.emitLine "  store " & retType & " " & c.str(oldVal) & ", ptr " & c.str(args[1].name)
    let r = c.temp()
    c.emitLine "  " & c.str(r) & " = zext i1 " & c.str(success) & " to i8"
    result = LLValue(name: r, typ: LToken(I8Token))
  of "__atomic_add_fetch":
    # __atomic_add_fetch(ptr, val, memorder) -> atomicrmw add, then add
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = atomicrmw add ptr " & c.str(args[0].name) & ", " & c.str(args[1].typ) & " " & c.str(args[1].name) & " " & ordering
    let r = c.temp()
    c.emitLine "  " & c.str(r) & " = add " & retType & " " & c.str(t) & ", " & c.str(args[1].name)
    result = LLValue(name: r, typ: c.tok(retType))
  of "__atomic_sub_fetch":
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = atomicrmw sub ptr " & c.str(args[0].name) & ", " & c.str(args[1].typ) & " " & c.str(args[1].name) & " " & ordering
    let r = c.temp()
    c.emitLine "  " & c.str(r) & " = sub " & retType & " " & c.str(t) & ", " & c.str(args[1].name)
    result = LLValue(name: r, typ: c.tok(retType))
  of "__atomic_fetch_add":
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = atomicrmw add ptr " & c.str(args[0].name) & ", " & c.str(args[1].typ) & " " & c.str(args[1].name) & " " & ordering
    result = LLValue(name: t, typ: c.tok(retType))
  of "__atomic_fetch_sub":
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = atomicrmw sub ptr " & c.str(args[0].name) & ", " & c.str(args[1].typ) & " " & c.str(args[1].name) & " " & ordering
    result = LLValue(name: t, typ: c.tok(retType))
  of "__atomic_fetch_and":
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = atomicrmw and ptr " & c.str(args[0].name) & ", " & c.str(args[1].typ) & " " & c.str(args[1].name) & " " & ordering
    result = LLValue(name: t, typ: c.tok(retType))
  of "__atomic_fetch_or":
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = atomicrmw or ptr " & c.str(args[0].name) & ", " & c.str(args[1].typ) & " " & c.str(args[1].name) & " " & ordering
    result = LLValue(name: t, typ: c.tok(retType))
  of "__atomic_fetch_xor":
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = atomicrmw xor ptr " & c.str(args[0].name) & ", " & c.str(args[1].typ) & " " & c.str(args[1].name) & " " & ordering
    result = LLValue(name: t, typ: c.tok(retType))
  of "__atomic_test_and_set":
    # atomicrmw xchg ptr, i8 1
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = atomicrmw xchg ptr " & c.str(args[0].name) & ", i8 1 " & ordering
    let r = c.temp()
    c.emitLine "  " & c.str(r) & " = icmp ne i8 " & c.str(t) & ", 0"
    let r2 = c.temp()
    c.emitLine "  " & c.str(r2) & " = zext i1 " & c.str(r) & " to i8"
    result = LLValue(name: r2, typ: LToken(I8Token))
  of "__atomic_clear":
    # store atomic i8 0, ptr p
    c.emitLine "  store atomic i8 0, ptr " & c.str(args[0].name) & " " & ordering
    result = LLValue(name: LToken(EmptyToken), typ: LToken(VoidToken))
  of "__atomic_thread_fence":
    c.emitLine "  fence " & ordering
    result = LLValue(name: LToken(EmptyToken), typ: LToken(VoidToken))
  of "__atomic_signal_fence":
    # LLVM doesn't have a direct signal fence; use singlethread fence
    c.emitLine "  fence syncscope(\"singlethread\") " & ordering
    result = LLValue(name: LToken(EmptyToken), typ: LToken(VoidToken))
  else:
    # Unknown atomic - fall through to regular call
    let argStr = args.mapIt(c.str(it.typ) & " " & c.str(it.name)).join(", ")
    if retType == "void":
      c.emitLine "  call void @" & externName & "(" & argStr & ")"
      result = LLValue(name: LToken(EmptyToken), typ: LToken(VoidToken))
    else:
      let t = c.temp()
      c.emitLine "  " & c.str(t) & " = call " & retType & " @" & externName & "(" & argStr & ")"
      result = LLValue(name: t, typ: c.tok(retType))

proc genMemIntrinsicCall(c: var LLVMCode; externName: string; args: seq[LLValue]; retType: string; result: var LLValue) =
  ## Translate memcpy/memset/memcmp to LLVM intrinsics or instructions.
  case externName
  of "memcpy":
    # memcpy(dest, src, size) -> llvm.memcpy.p0.p0.i64
    c.emitLine "  call void @llvm.memcpy.p0.p0.i64(ptr " & c.str(args[0].name) & ", ptr " & c.str(args[1].name) & ", i64 " & c.str(args[2].name) & ", i1 false)"
    if "llvm.memcpy.p0.p0.i64" notin c.declaredExterns:
      c.declaredExterns.incl "llvm.memcpy.p0.p0.i64"
      c.addTo(c.externs, "declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg)\n")
    result = LLValue(name: args[0].name, typ: LToken(PtrToken)) # memcpy returns dest
  of "memmove":
    c.emitLine "  call void @llvm.memmove.p0.p0.i64(ptr " & c.str(args[0].name) & ", ptr " & c.str(args[1].name) & ", i64 " & c.str(args[2].name) & ", i1 false)"
    if "llvm.memmove.p0.p0.i64" notin c.declaredExterns:
      c.declaredExterns.incl "llvm.memmove.p0.p0.i64"
      c.addTo(c.externs, "declare void @llvm.memmove.p0.p0.i64(ptr nocapture writeonly, ptr nocapture readonly, i64, i1 immarg)\n")
    result = LLValue(name: args[0].name, typ: LToken(PtrToken))
  of "memset":
    # memset(dest, val, size) -> llvm.memset.p0.i64
    # Note: LLVM memset takes i8 val, C memset takes int val
    let val8 = c.temp()
    c.emitLine "  " & c.str(val8) & " = trunc " & c.str(args[1].typ) & " " & c.str(args[1].name) & " to i8"
    c.emitLine "  call void @llvm.memset.p0.i64(ptr " & c.str(args[0].name) & ", i8 " & c.str(val8) & ", i64 " & c.str(args[2].name) & ", i1 false)"
    if "llvm.memset.p0.i64" notin c.declaredExterns:
      c.declaredExterns.incl "llvm.memset.p0.i64"
      c.addTo(c.externs, "declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg)\n")
    result = LLValue(name: args[0].name, typ: LToken(PtrToken))
  of "memcmp":
    # memcmp(a, b, size) -> call i32 @memcmp(ptr, ptr, i64)
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = call i32 @memcmp(ptr " & c.str(args[0].name) & ", ptr " & c.str(args[1].name) & ", i64 " & c.str(args[2].name) & ")"
    if "memcmp" notin c.declaredExterns:
      c.declaredExterns.incl "memcmp"
      c.addTo(c.externs, "declare i32 @memcmp(ptr nocapture, ptr nocapture, i64)\n")
    result = LLValue(name: t, typ: c.tok("i32"))
  else:
    # Not a memory intrinsic
    let argStr = args.mapIt(c.str(it.typ) & " " & c.str(it.name)).join(", ")
    if retType == "void":
      c.emitLine "  call void @" & externName & "(" & argStr & ")"
      result = LLValue(name: LToken(EmptyToken), typ: LToken(VoidToken))
    else:
      let t = c.temp()
      c.emitLine "  " & c.str(t) & " = call " & retType & " @" & externName & "(" & argStr & ")"
      result = LLValue(name: t, typ: c.tok(retType))

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

proc genGccBuiltinCall(c: var LLVMCode; externName: string; args: seq[LLValue]; retType: string; result: var LLValue) =
  ## Translate GCC __builtin_* functions to LLVM intrinsics.
  case externName
  of "__builtin_ctzll":
    # count trailing zeros i64 → llvm.cttz.i64(val, is_zero_poison=false)
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = call i64 @llvm.cttz.i64(i64 " & c.str(args[0].name) & ", i1 false)"
    if retType == "i32":
      let r = c.temp()
      c.emitLine "  " & c.str(r) & " = trunc i64 " & c.str(t) & " to i32"
      result = LLValue(name: r, typ: c.tok("i32"))
    else:
      result = LLValue(name: t, typ: c.tok("i64"))
  of "__builtin_ctz":
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = call i32 @llvm.cttz.i32(i32 " & c.str(args[0].name) & ", i1 false)"
    result = LLValue(name: t, typ: c.tok("i32"))
  of "__builtin_clzll":
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = call i64 @llvm.ctlz.i64(i64 " & c.str(args[0].name) & ", i1 false)"
    if retType == "i32":
      let r = c.temp()
      c.emitLine "  " & c.str(r) & " = trunc i64 " & c.str(t) & " to i32"
      result = LLValue(name: r, typ: c.tok("i32"))
    else:
      result = LLValue(name: t, typ: c.tok("i64"))
  of "__builtin_clz":
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = call i32 @llvm.ctlz.i32(i32 " & c.str(args[0].name) & ", i1 false)"
    result = LLValue(name: t, typ: c.tok("i32"))
  of "__builtin_popcountll":
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = call i64 @llvm.ctpop.i64(i64 " & c.str(args[0].name) & ")"
    if retType == "i32":
      let r = c.temp()
      c.emitLine "  " & c.str(r) & " = trunc i64 " & c.str(t) & " to i32"
      result = LLValue(name: r, typ: c.tok("i32"))
    else:
      result = LLValue(name: t, typ: c.tok("i64"))
  of "__builtin_popcount":
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = call i32 @llvm.ctpop.i32(i32 " & c.str(args[0].name) & ")"
    result = LLValue(name: t, typ: c.tok("i32"))
  of "__builtin_bswap16":
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = call i16 @llvm.bswap.i16(i16 " & c.str(args[0].name) & ")"
    result = LLValue(name: t, typ: c.tok("i16"))
  of "__builtin_bswap32":
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = call i32 @llvm.bswap.i32(i32 " & c.str(args[0].name) & ")"
    result = LLValue(name: t, typ: c.tok("i32"))
  of "__builtin_bswap64":
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = call i64 @llvm.bswap.i64(i64 " & c.str(args[0].name) & ")"
    result = LLValue(name: t, typ: c.tok("i64"))
  of "__builtin_expect":
    # __builtin_expect(val, expected) — just return val, it's a branch hint
    result = args[0]
  else:
    discard

proc genCallWithType(c: var LLVMCode; n: var Cursor; retType: string; result: var LLValue) =
  ## Generate a call where we know the return type from context.
  ## Intercepts special C functions and emits LLVM native instructions.
  let callInfo = n.info
  inc n

  var calleeName: string
  var calleeExtern: string = "" # the importc name if any
  var calleeSym = SymId(0)
  if n.kind == Symbol:
    calleeSym = n.symId
    c.requestedSyms.incl calleeSym
    calleeExtern = getExternName(c, calleeSym)
    calleeName = "@" & mangleSym(c, calleeSym)
    inc n
  else:
    var callee = LLValue(); genExprLLVM(c, n, callee)
    calleeName = c.str(callee.name)

  var args: seq[LLValue] = @[]
  while n.kind != ParRi:
    var arg = LLValue(); genExprLLVM(c, n, arg)
    args.add arg
  skipParRi n

  # Check for special C functions that map to LLVM instructions
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

  let argStr = args.mapIt(c.str(it.typ) & " " & c.str(it.name)).join(", ")

  if retType == "void":
    c.emitLineDbg "  call void " & calleeName & "(" & argStr & ")", callInfo
    result = LLValue(name: LToken(EmptyToken), typ: LToken(VoidToken))
  else:
    let t = c.temp()
    c.emitLineDbg "  " & c.str(t) & " = call " & retType & " " & calleeName & "(" & argStr & ")", callInfo
    result = LLValue(name: t, typ: c.tok(retType))

proc genCallLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  ## Call without known return type - used when context doesn't provide type info.
  genCallWithType(c, n, "ptr", result)

proc genCallExprLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  ## Call expression, alias for genCallLLVM.
  genCallLLVM(c, n, result)

proc isNifcInt(tk: NifcType): bool {.inline.} =
  tk in {IT, UT, CT, BoolT, EnumT}

proc isNifcFloat(tk: NifcType): bool {.inline.} =
  tk == FT

proc isNifcPtr(tk: NifcType): bool {.inline.} =
  tk in {PtrT, APtrT, ProctypeT}

proc coerceValueLLVM(c: var LLVMCode; val: LLValue; srcTypeCursor, destTypeCursor: Cursor;
                     isCast: bool; result: var LLValue) =
  let destType = genTypeLLVMReadOnly(c, destTypeCursor)
  let destTok = c.tok(destType)
  if val.typ == destTok:
    result = val
    return

  let srcTK = scalarTypeKind(c, srcTypeCursor)
  let destTK = scalarTypeKind(c, destTypeCursor)
  let t = c.temp()
  let srcStr = c.str(val.typ)

  if isNifcPtr(srcTK) and isNifcPtr(destTK):
    # Opaque pointers collapse all pointee types to `ptr`.
    result = LLValue(name: val.name, typ: destTok)
    return
  elif isNifcPtr(destTK) and isNifcInt(srcTK):
    c.emitLine "  " & c.str(t) & " = inttoptr " & srcStr & " " & c.str(val.name) & " to ptr"
  elif isNifcInt(destTK) and isNifcPtr(srcTK):
    c.emitLine "  " & c.str(t) & " = ptrtoint ptr " & c.str(val.name) & " to " & destType
  elif isNifcFloat(srcTK) and isNifcFloat(destTK):
    let srcBits = typeSizeBits(c, srcTypeCursor)
    let destBits = typeSizeBits(c, destTypeCursor)
    if srcBits < destBits:
      c.emitLine "  " & c.str(t) & " = fpext " & srcStr & " " & c.str(val.name) & " to " & destType
    else:
      c.emitLine "  " & c.str(t) & " = fptrunc " & srcStr & " " & c.str(val.name) & " to " & destType
  elif isNifcFloat(srcTK) and isNifcInt(destTK):
    c.emitLine "  " & c.str(t) & " = fptosi " & srcStr & " " & c.str(val.name) & " to " & destType
  elif isNifcInt(srcTK) and isNifcFloat(destTK):
    c.emitLine "  " & c.str(t) & " = sitofp " & srcStr & " " & c.str(val.name) & " to " & destType
  elif isNifcInt(srcTK) and isNifcInt(destTK):
    let srcBits = typeSizeBits(c, srcTypeCursor)
    let destBits = typeSizeBits(c, destTypeCursor)
    if srcBits < destBits:
      if isCast or srcTK in {UT, CT, BoolT}:
        c.emitLine "  " & c.str(t) & " = zext " & srcStr & " " & c.str(val.name) & " to " & destType
      else:
        c.emitLine "  " & c.str(t) & " = sext " & srcStr & " " & c.str(val.name) & " to " & destType
    elif srcBits > destBits:
      c.emitLine "  " & c.str(t) & " = trunc " & srcStr & " " & c.str(val.name) & " to " & destType
    else:
      result = LLValue(name: val.name, typ: destTok)
      return
  else:
    c.emitLine "  " & c.str(t) & " = bitcast " & srcStr & " " & c.str(val.name) & " to " & destType

  result = LLValue(name: t, typ: destTok)

proc genConvOrCast(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  ## Handle (conv type expr) and (cast type expr)
  let isCast = n.exprKind == CastC
  inc n
  let destTypeCursor = n
  skip n # destination type
  let srcTypeCursor = getType(c.m, n)
  var val = LLValue(); genExprLLVM(c, n, val)
  skipParRi n
  coerceValueLLVM(c, val, srcTypeCursor, destTypeCursor, isCast, result)

proc genAddrLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  inc n
  var lval = LLValue(); genLvalueLLVM(c, n, lval)
  # Skip optional CppRefQ qualifier
  if n.kind != ParRi and n.typeQual == CppRefQ:
    skip n
  skipParRi n
  # The lvalue is already a pointer
  result = LLValue(name: lval.name, typ: LToken(PtrToken))

proc genDotLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  ## Field access: (dot obj field inheritanceDepth?)
  inc n
  let objType = getNominalType(c.m, n)
  var obj = LLValue(); genLvalueLLVM(c, n, obj) # get address of object for GEP

  # Get field symbol
  let fldSym = n.symId
  skip n

  # Get inheritance depth
  var inhDepth = 0
  if n.kind == IntLit:
    inhDepth = int(pool.integers[n.intId])
    inc n

  skipParRi n

  # Resolve to the final object body (after walking through inheritance)
  # We walk inhDepth levels to get the actual object that contains the field.
  var curType = objType
  var curBody = navigateToObjectBody(c.m, curType)
  let objTypeName = genTypeLLVMReadOnly(c, curType)
  var gepTarget = c.str(obj.name)
  var gepType = objTypeName

  # Navigate through inheritance chain via GEP field 0
  for i in 0 ..< inhDepth:
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = getelementptr inbounds " & gepType & ", ptr " & gepTarget & ", i32 0, i32 0"
    gepTarget = c.str(t)
    # Resolve base type for next iteration
    let baseTypeCursor = baseTypeOfObject(c.m, curBody)
    if not cursorIsNil(baseTypeCursor):
      curType = baseTypeCursor
      curBody = navigateToObjectBody(c.m, curType)
      gepType = genTypeLLVMReadOnly(c, curType)

  # Look up the field index in the resolved object body
  let fldIdx = fieldIndex(c, curBody, fldSym)

  let t = c.temp()
  c.emitLine "  " & c.str(t) & " = getelementptr inbounds " & gepType & ", ptr " & gepTarget & ", i32 0, i32 " & $fldIdx
  result = LLValue(name: t, typ: LToken(PtrToken))

proc genAtLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  ## Array indexing: (at array index)
  inc n
  let arrType = getType(c.m, n)
  var arr = LLValue(); genLvalueLLVM(c, n, arr) # GEP needs address, not value
  var idx = LLValue(); genExprLLVM(c, n, idx)
  skipParRi n

  let arrTypeName = genTypeLLVMReadOnly(c, arrType)

  # GEP to index into the array
  let t = c.temp()
  c.emitLine "  " & c.str(t) & " = getelementptr inbounds " & arrTypeName & ", ptr " & c.str(arr.name) & ", i32 0, " & c.str(idx.typ) & " " & c.str(idx.name)
  result = LLValue(name: t, typ: LToken(PtrToken))

proc genPatLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  ## Pointer arithmetic indexing: (pat ptr index)
  ## In NIF, pat does typed pointer arithmetic — the stride is determined by the
  ## pointed-to element type (like C's ptr[i]).
  inc n
  let baseType = getType(c.m, n)
  var base = LLValue(); genExprLLVM(c, n, base)
  var idx = LLValue(); genExprLLVM(c, n, idx)
  skipParRi n

  # Determine the element type for GEP stride
  let elemCursor = pointeeType(c, baseType)
  if cursorIsNil(elemCursor):
    error c.m, "expected pointer type for `pat` but got: ", baseType
  let elemType = genTypeLLVMReadOnly(c, elemCursor)

  let t = c.temp()
  c.emitLine "  " & c.str(t) & " = getelementptr " & elemType & ", ptr " & c.str(base.name) & ", " & c.str(idx.typ) & " " & c.str(idx.name)
  result = LLValue(name: t, typ: LToken(PtrToken))

proc genSizeofLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  inc n
  let typ = genTypeLLVM(c, n)
  skipParRi n

  # Use getelementptr null trick to compute sizeof
  let t1 = c.temp()
  let t2 = c.temp()
  c.emitLine "  " & c.str(t1) & " = getelementptr " & typ & ", ptr null, i32 1"
  c.emitLine "  " & c.str(t2) & " = ptrtoint ptr " & c.str(t1) & " to i" & $c.bits
  result = LLValue(name: t2, typ: c.tok("i" & $c.bits))

proc isGlobalSym(c: var LLVMCode; s: SymId): bool =
  let d = c.m.getDeclOrNil(s)
  result = d != nil and d.kind in {GvarY, TvarY, ConstY, ProcY}

proc genLvalueLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  ## Generate an lvalue (pointer to storage location).
  case n.exprKind
  of NoExpr:
    if n.kind == Symbol:
      let s = n.symId
      c.requestedSyms.incl s
      let name = mangleSym(c, s)
      inc n
      # Check if it's a global
      if isGlobalSym(c, s):
        result = LLValue(name: c.tok("@" & name), typ: LToken(PtrToken))
      else:
        result = LLValue(name: c.tok("%" & name), typ: LToken(PtrToken))
    else:
      error c.m, "expected expression but got: ", n
  of DerefC:
    # Dereference gives us the pointer value
    inc n
    var ptrVal = LLValue(); genExprLLVM(c, n, ptrVal)
    if n.kind != ParRi and n.typeQual == CppRefQ:
      skip n
    skipParRi n
    result = LLValue(name: ptrVal.name, typ: LToken(PtrToken))
  of AtC:
    genAtLLVM(c, n, result)
  of PatC:
    genPatLLVM(c, n, result)
  of DotC:
    genDotLLVM(c, n, result)
  of ErrvC:
    # Error flag is a global
    result = LLValue(name: c.tok("@NIFC_ERR_"), typ: LToken(PtrToken))
    skip n
  of OvfC:
    # Overflow flag
    result = LLValue(name: c.tok("@NIFC_OVF_"), typ: LToken(PtrToken))
    skip n
  of BaseobjC:
    # Base object access as lvalue: base is always at offset 0 in the struct,
    # so the pointer is the same — just skip the baseobj wrapper.
    inc n
    skip n # target type
    skip n # depth
    genLvalueLLVM(c, n, result)
    skipParRi n
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
      let i = pool.integers[n.intId]
      inc n
      result = LLValue(name: c.tok($i), typ: c.tok("i" & $c.bits))
    of UIntLit:
      let i = pool.uintegers[n.uintId]
      inc n
      result = LLValue(name: c.tok($i), typ: c.tok("i" & $c.bits))
    of FloatLit:
      let f = pool.floats[n.floatId]
      inc n
      result = LLValue(name: c.tok($f), typ: c.tok("double"))
    of CharLit:
      let ch = n.charLit
      inc n
      result = LLValue(name: c.tok($ord(ch)), typ: LToken(I8Token))
    of StringLit:
      # Create a global string constant and return pointer to it
      let s = pool.strings[n.litId]
      inc n
      let globalName = "@.str." & $c.strLitCounter
      inc c.strLitCounter
      # Emit global string constant with LLVM-style escaping
      var escaped = newStringOfCap(s.len + 10)
      for ch in s:
        let o = ord(ch)
        if o < 32 or o > 126 or ch == '"' or ch == '\\':
          escaped.add '\\'
          escaped.add "0123456789ABCDEF"[o shr 4]
          escaped.add "0123456789ABCDEF"[o and 0xF]
        else:
          escaped.add ch
      c.addTo(c.globals, globalName & " = private unnamed_addr constant [" & $(s.len + 1) & " x i8] c\"" & escaped & "\\00\"\n")
      result = LLValue(name: c.tok(globalName), typ: LToken(PtrToken))
    of Symbol:
      let s = n.symId
      c.requestedSyms.incl s
      let name = mangleSym(c, s)
      # Look up the type from the scope before advancing
      let symType = getType(c.m, n)
      let decl = c.m.getDeclOrNil(s)
      inc n
      # Check if this is an enum field — resolve to its integer constant
      if symType.typeKind == EnumT:
        var enumBody = symType
        inc enumBody # skip EnumT tag
        let baseTyp = genTypeLLVMReadOnly(c, enumBody)
        skip enumBody # skip base type
        # Search for the field
        while enumBody.kind != ParRi:
          if enumBody.substructureKind == EfldU:
            inc enumBody
            if enumBody.kind == SymbolDef and enumBody.symId == s:
              inc enumBody
              # Next is the value (IntLit or UIntLit)
              if enumBody.kind == IntLit:
                result = LLValue(name: c.tok($pool.integers[enumBody.intId]), typ: c.tok(baseTyp))
              elif enumBody.kind == UIntLit:
                result = LLValue(name: c.tok($pool.uintegers[enumBody.uintId]), typ: c.tok(baseTyp))
              else:
                result = LLValue(name: c.tok("0"), typ: c.tok(baseTyp))
              return
            else:
              skip enumBody # skip SymbolDef
              skip enumBody # skip value
              skipParRi enumBody
          else:
            skip enumBody
        result = LLValue(name: c.tok("0"), typ: c.tok(baseTyp))
        return
      if decl != nil and decl.kind == ProcY:
        let typ = genTypeLLVMReadOnly(c, symType)
        result = LLValue(name: c.tok("@" & name), typ: c.tok(typ))
        return
      let typ = genTypeLLVMReadOnly(c, symType)
      let prefix = if isGlobalSym(c, s): "@" else: "%"
      let t = c.temp()
      c.emitLine "  " & c.str(t) & " = load " & typ & ", ptr " & prefix & name
      result = LLValue(name: t, typ: c.tok(typ))
    else:
      let exprType = getType(c.m, n)
      let typ = genTypeLLVMReadOnly(c, exprType)
      var lval = LLValue(); genLvalueLLVM(c, n, lval)
      let t = c.temp()
      c.emitLine "  " & c.str(t) & " = load " & typ & ", ptr " & c.str(lval.name)
      result = LLValue(name: t, typ: c.tok(typ))
  of FalseC:
    skip n
    result = LLValue(name: c.tok("0"), typ: LToken(I8Token))
  of TrueC:
    skip n
    result = LLValue(name: c.tok("1"), typ: LToken(I8Token))
  of NilC:
    skip n
    result = LLValue(name: LToken(NullToken), typ: LToken(PtrToken))
  of InfC:
    skip n
    result = LLValue(name: c.tok("0x7FF0000000000000"), typ: c.tok("double")) # +inf in IEEE 754
  of NegInfC:
    skip n
    result = LLValue(name: c.tok("0xFFF0000000000000"), typ: c.tok("double")) # -inf
  of NanC:
    skip n
    result = LLValue(name: c.tok("0x7FF8000000000000"), typ: c.tok("double")) # NaN
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
    # (bitnot type expr)
    inc n
    let typ = genTypeLLVM(c, n)
    var val = LLValue(); genExprLLVM(c, n, val)
    skipParRi n
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = xor " & typ & " " & c.str(val.name) & ", -1"
    result = LLValue(name: t, typ: c.tok(typ))
  of NegC:
    # (neg type expr)
    inc n
    let typ = genTypeLLVM(c, n)
    var val = LLValue(); genExprLLVM(c, n, val)
    skipParRi n
    let t = c.temp()
    if typ in ["float", "double", "fp128"]:
      c.emitLine "  " & c.str(t) & " = fneg " & typ & " " & c.str(val.name)
    else:
      c.emitLine "  " & c.str(t) & " = sub " & typ & " 0, " & c.str(val.name)
    result = LLValue(name: t, typ: c.tok(typ))
  of EqC: genBoolCmpOp(c, n, "eq", "eq", result)
  of NeqC: genBoolCmpOp(c, n, "ne", "ne", result)
  of LeC: genBoolCmpOp(c, n, "sle", "ule", result)
  of LtC: genBoolCmpOp(c, n, "slt", "ult", result)
  of AndC:
    # Short-circuit AND: (and lhs rhs) — rhs only evaluated if lhs is true
    inc n
    let res = c.temp()
    c.addAlloca(res, LToken(I8Token))
    c.emitLine "  store i8 0, ptr " & c.str(res)
    var lhs = LLValue(); genExprLLVM(c, n, lhs)
    let lhsBool = c.temp()
    c.emitLine "  " & c.str(lhsBool) & " = icmp ne " & c.str(lhs.typ) & " " & c.str(lhs.name) & ", " & zeroVal(lhs.typ)
    let rhsLabel = c.label()
    let endLabel = c.label()
    c.emitLine "  br i1 " & c.str(lhsBool) & ", label %" & c.str(rhsLabel) & ", label %" & c.str(endLabel)
    c.emitLine c.str(rhsLabel) & ":"
    c.currentProc.needsTerminator = false
    var rhs = LLValue(); genExprLLVM(c, n, rhs)
    let rhsBool = c.temp()
    c.emitLine "  " & c.str(rhsBool) & " = icmp ne " & c.str(rhs.typ) & " " & c.str(rhs.name) & ", " & zeroVal(rhs.typ)
    let rhsExt = c.temp()
    c.emitLine "  " & c.str(rhsExt) & " = zext i1 " & c.str(rhsBool) & " to i8"
    c.emitLine "  store i8 " & c.str(rhsExt) & ", ptr " & c.str(res)
    c.emitLine "  br label %" & c.str(endLabel)
    c.emitLine c.str(endLabel) & ":"
    c.currentProc.needsTerminator = false
    let r = c.temp()
    c.emitLine "  " & c.str(r) & " = load i8, ptr " & c.str(res)
    skipParRi n
    result = LLValue(name: r, typ: LToken(I8Token))
  of OrC:
    # Short-circuit OR: (or lhs rhs) — rhs only evaluated if lhs is false
    inc n
    let res = c.temp()
    c.addAlloca(res, LToken(I8Token))
    c.emitLine "  store i8 1, ptr " & c.str(res)
    var lhs = LLValue(); genExprLLVM(c, n, lhs)
    let lhsBool = c.temp()
    c.emitLine "  " & c.str(lhsBool) & " = icmp ne " & c.str(lhs.typ) & " " & c.str(lhs.name) & ", " & zeroVal(lhs.typ)
    let rhsLabel = c.label()
    let endLabel = c.label()
    c.emitLine "  br i1 " & c.str(lhsBool) & ", label %" & c.str(endLabel) & ", label %" & c.str(rhsLabel)
    c.emitLine c.str(rhsLabel) & ":"
    c.currentProc.needsTerminator = false
    var rhs = LLValue(); genExprLLVM(c, n, rhs)
    let rhsBool = c.temp()
    c.emitLine "  " & c.str(rhsBool) & " = icmp ne " & c.str(rhs.typ) & " " & c.str(rhs.name) & ", " & zeroVal(rhs.typ)
    let rhsExt = c.temp()
    c.emitLine "  " & c.str(rhsExt) & " = zext i1 " & c.str(rhsBool) & " to i8"
    c.emitLine "  store i8 " & c.str(rhsExt) & ", ptr " & c.str(res)
    c.emitLine "  br label %" & c.str(endLabel)
    c.emitLine c.str(endLabel) & ":"
    c.currentProc.needsTerminator = false
    let r = c.temp()
    c.emitLine "  " & c.str(r) & " = load i8, ptr " & c.str(res)
    skipParRi n
    result = LLValue(name: r, typ: LToken(I8Token))
  of NotC:
    inc n
    var val = LLValue(); genExprLLVM(c, n, val)
    skipParRi n
    let t1 = c.temp()
    let t2 = c.temp()
    c.emitLine "  " & c.str(t1) & " = icmp eq " & c.str(val.typ) & " " & c.str(val.name) & ", " & zeroVal(val.typ)
    c.emitLine "  " & c.str(t2) & " = zext i1 " & c.str(t1) & " to i8"
    result = LLValue(name: t2, typ: LToken(I8Token))
  of CastC, ConvC:
    genConvOrCast(c, n, result)
  of CallC:
    let retTypeCursor = getType(c.m, n)
    let retType = genTypeLLVMReadOnly(c, retTypeCursor)
    genCallWithType(c, n, retType, result)
  of AddrC:
    genAddrLLVM(c, n, result)
  of DerefC:
    # Dereference a pointer: load from the pointer
    let derefType = getType(c.m, n)
    let loadType = genTypeLLVMReadOnly(c, derefType)
    inc n
    var ptrVal = LLValue(); genExprLLVM(c, n, ptrVal)
    if n.kind != ParRi and n.typeQual == CppRefQ:
      skip n
    skipParRi n
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = load " & loadType & ", ptr " & c.str(ptrVal.name)
    result = LLValue(name: t, typ: c.tok(loadType))
  of AtC:
    let elemType = getType(c.m, n)
    let loadType = genTypeLLVMReadOnly(c, elemType)
    var lval = LLValue(); genAtLLVM(c, n, lval)
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = load " & loadType & ", ptr " & c.str(lval.name)
    result = LLValue(name: t, typ: c.tok(loadType))
  of PatC:
    let elemType = getType(c.m, n)
    let loadType = genTypeLLVMReadOnly(c, elemType)
    var lval = LLValue(); genPatLLVM(c, n, lval)
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = load " & loadType & ", ptr " & c.str(lval.name)
    result = LLValue(name: t, typ: c.tok(loadType))
  of DotC:
    let fldType = getType(c.m, n)
    let loadType = genTypeLLVMReadOnly(c, fldType)
    var lval = LLValue(); genDotLLVM(c, n, lval)
    if fldType.typeKind == FlexarrayT:
      # Flexible array members can't be loaded; return the GEP pointer directly
      result = LLValue(name: lval.name, typ: LToken(PtrToken))
    else:
      let t = c.temp()
      c.emitLine "  " & c.str(t) & " = load " & loadType & ", ptr " & c.str(lval.name)
      result = LLValue(name: t, typ: c.tok(loadType))
  of SizeofC:
    genSizeofLLVM(c, n, result)
  of AlignofC:
    # Approximation: emit sizeof as alignof (correct for power-of-2 types)
    genSizeofLLVM(c, n, result)
  of OffsetofC:
    inc n
    let typCursor = n
    let typ = genTypeLLVM(c, n)
    let fldSym = n.symId
    inc n
    skipParRi n
    # Resolve the field index from the type body
    let objBody = navigateToObjectBody(c.m, typCursor)
    let fldIdx = fieldIndex(c, objBody, fldSym)
    let t1 = c.temp()
    let t2 = c.temp()
    c.emitLine "  " & c.str(t1) & " = getelementptr " & typ & ", ptr null, i32 0, i32 " & $fldIdx
    c.emitLine "  " & c.str(t2) & " = ptrtoint ptr " & c.str(t1) & " to i" & $c.bits
    result = LLValue(name: t2, typ: c.tok("i" & $c.bits))
  of ParC:
    inc n
    genExprLLVM(c, n, result)
    skipParRi n
  of SufC:
    # (suf value suffix)
    inc n
    var value = n
    skip n
    let suffix = n
    let suffixStr = pool.strings[suffix.litId]
    skip n
    skipParRi n
    if value.kind == StringLit:
      genExprLLVM(c, value, result)
    else:
      var val = LLValue(); genExprLLVM(c, value, val)
      # Determine target type token from suffix
      var targetTok = LToken(I64Token)
      case suffixStr
      of "i64": targetTok = LToken(I64Token)
      of "i32": targetTok = LToken(I32Token)
      of "i16": targetTok = LToken(I16Token)
      of "i8": targetTok = LToken(I8Token)
      of "u64": targetTok = LToken(I64Token)
      of "u32": targetTok = LToken(I32Token)
      of "u16": targetTok = LToken(I16Token)
      of "u8": targetTok = LToken(I8Token)
      of "f64": targetTok = LToken(DoubleToken)
      of "f32": targetTok = LToken(FloatToken)
      else: discard
      if val.typ == targetTok:
        result = val
      else:
        let t = c.temp()
        if isFloatType(targetTok):
          c.emitLine "  " & c.str(t) & " = sitofp " & c.str(val.typ) & " " & c.str(val.name) & " to " & c.str(targetTok)
        else:
          # Integer literal (always c.bits wide) to target integer
          let destBits = case suffixStr
            of "i8", "u8": 8
            of "i16", "u16": 16
            of "i32", "u32": 32
            else: 64
          if c.bits < destBits:
            c.emitLine "  " & c.str(t) & " = sext " & c.str(val.typ) & " " & c.str(val.name) & " to " & c.str(targetTok)
          elif c.bits > destBits:
            c.emitLine "  " & c.str(t) & " = trunc " & c.str(val.typ) & " " & c.str(val.name) & " to " & c.str(targetTok)
          else:
            result = val
            return
        result = LLValue(name: t, typ: targetTok)
  of OconstrC:
    # Object constructor — use alloca + store for each field.
    # This handles union fields correctly (stores through pointer cast).
    inc n
    let objTypeCursor = n
    let typ = genTypeLLVM(c, n)

    let tmp = c.temp()
    c.addAlloca(tmp, c.tok(typ))
    # Zero-initialize
    c.emitLine "  store " & typ & " zeroinitializer, ptr " & c.str(tmp)

    while n.kind != ParRi:
      if n.substructureKind == KvU:
        inc n
        let fldSym = n.symId
        skip n # field name
        var fieldVal = LLValue(); genExprLLVM(c, n, fieldVal)
        let inhDepth = if n.kind != ParRi and n.kind == IntLit: (let d = int(pool.integers[n.intId]); skip n; d) else: 0
        skipParRi n
        # Navigate to the field — objTypeCursor is the type symbol itself
        var curType = objTypeCursor
        var curBody = navigateToObjectBody(c.m, curType)
        let objTypeName = genTypeLLVMReadOnly(c, curType)
        var gepTarget = c.str(tmp)
        var gepType = objTypeName
        for i in 0 ..< inhDepth:
          let g = c.temp()
          c.emitLine "  " & c.str(g) & " = getelementptr inbounds " & gepType & ", ptr " & gepTarget & ", i32 0, i32 0"
          gepTarget = c.str(g)
          let baseTypeCursor = baseTypeOfObject(c.m, curBody)
          if not cursorIsNil(baseTypeCursor):
            curType = baseTypeCursor
            curBody = navigateToObjectBody(c.m, curType)
            gepType = genTypeLLVMReadOnly(c, curType)
        let fldIdx = fieldIndex(c, curBody, fldSym)
        let fldPtr = c.temp()
        c.emitLine "  " & c.str(fldPtr) & " = getelementptr inbounds " & gepType & ", ptr " & gepTarget & ", i32 0, i32 " & $fldIdx
        c.emitLine "  store " & c.str(fieldVal.typ) & " " & c.str(fieldVal.name) & ", ptr " & c.str(fldPtr)
      else:
        # Non-KV child: base object constructor or vtable pointer — store at field 0
        var baseVal = LLValue(); genExprLLVM(c, n, baseVal)
        let basePtr = c.temp()
        c.emitLine "  " & c.str(basePtr) & " = getelementptr inbounds " & typ & ", ptr " & c.str(tmp) & ", i32 0, i32 0"
        c.emitLine "  store " & c.str(baseVal.typ) & " " & c.str(baseVal.name) & ", ptr " & c.str(basePtr)
    skipParRi n
    let loaded = c.temp()
    c.emitLine "  " & c.str(loaded) & " = load " & typ & ", ptr " & c.str(tmp)
    result = LLValue(name: loaded, typ: c.tok(typ))
  of AconstrC:
    # Array constructor
    inc n
    let arrayTypeCursor = n
    let typ = genTypeLLVM(c, n)
    let expectedLen = fixedArrayLen(c, arrayTypeCursor)

    var current = "undef"
    var idx = 0
    while n.kind != ParRi:
      var elemVal = LLValue(); genExprLLVM(c, n, elemVal)
      let t = c.temp()
      c.emitLine "  " & c.str(t) & " = insertvalue " & typ & " " & current & ", " & c.str(elemVal.typ) & " " & c.str(elemVal.name) & ", " & $idx
      current = c.str(t)
      inc idx
    skipParRi n
    if expectedLen >= 0 and idx != expectedLen:
      error c.m, "array literal element count does not match its declared length: ", arrayTypeCursor
    result = LLValue(name: c.tok(current), typ: c.tok(typ))
  of BaseobjC:
    let loadType = genTypeLLVMReadOnly(c, getType(c.m, n))
    var lval = LLValue(); genLvalueLLVM(c, n, lval)
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = load " & loadType & ", ptr " & c.str(lval.name)
    result = LLValue(name: t, typ: c.tok(loadType))
  of ErrvC, OvfC:
    var lval = LLValue(); genLvalueLLVM(c, n, lval)
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = load i8, ptr " & c.str(lval.name)
    result = LLValue(name: t, typ: LToken(I8Token))

proc genCondLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  ## Generate a condition expression, returning an i1 value for branch instructions.
  genExprLLVM(c, n, result)
  if c.str(result.typ) == "i1":
    return
  # Convert to i1 by comparing with 0
  let t = c.temp()
  c.emitLine "  " & c.str(t) & " = icmp ne " & c.str(result.typ) & " " & c.str(result.name) & ", " & zeroVal(result.typ)
  result = LLValue(name: t, typ: c.tok("i1"))
