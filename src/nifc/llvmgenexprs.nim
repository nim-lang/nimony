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
proc genCallExprLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue)

proc signedBinOp(c: var LLVMCode; n: var Cursor; op: string; result: var LLValue) =
  ## Typed binary op: (op type lhs rhs)
  inc n
  let typ = genTypeLLVM(c, n)
  var lhs = LLValue(); genExprLLVM(c, n, lhs)
  var rhs = LLValue(); genExprLLVM(c, n, rhs)
  let t = c.temp()
  c.emitLine "  " & c.str(t) & " = " & op & " " & typ & " " & c.str(lhs.name) & ", " & c.str(rhs.name)
  skipParRi n
  result = LLValue(name: t, typ: c.tok(typ))

proc unsignedBinOp(c: var LLVMCode; n: var Cursor; signedOp, unsignedOp: string; result: var LLValue) =
  ## Binary op that differs for signed/unsigned: checks the NIF type tag.
  inc n
  let isUnsigned = n.typeKind == UT
  let typ = genTypeLLVM(c, n)
  var lhs = LLValue(); genExprLLVM(c, n, lhs)
  var rhs = LLValue(); genExprLLVM(c, n, rhs)
  let t = c.temp()
  let op = if isUnsigned: unsignedOp else: signedOp
  c.emitLine "  " & c.str(t) & " = " & op & " " & typ & " " & c.str(lhs.name) & ", " & c.str(rhs.name)
  skipParRi n
  result = LLValue(name: t, typ: c.tok(typ))

proc cmpOp(c: var LLVMCode; n: var Cursor; signedPred, unsignedPred: string; result: var LLValue) =
  ## Comparison op: (op lhs rhs) → i1
  inc n
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
    let pred = if unsignedPred != "" and typ.startsWith("u"):
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

proc genAtomicCall(c: var LLVMCode; externName: string; args: seq[LLValue]; retType: string; result: var LLValue) =
  ## Translate __atomic_* GCC builtins to LLVM atomic instructions.
  let ordering = memoryOrderToLLVM(args[^1]) # last arg is always memory order
  case externName
  of "__atomic_load_n":
    # __atomic_load_n(ptr, memorder) -> LLVM: load atomic type, ptr p ordering
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = load atomic " & retType & ", ptr " & c.str(args[0].name) & " " & ordering
    result = LLValue(name: t, typ: c.tok(retType))
  of "__atomic_store_n":
    # __atomic_store_n(ptr, val, memorder) -> LLVM: store atomic type val, ptr p ordering
    c.emitLine "  store atomic " & c.str(args[1].typ) & " " & c.str(args[1].name) & ", ptr " & c.str(args[0].name) & " " & ordering
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

proc genConvOrCast(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  ## Handle (conv type expr) and (cast type expr)
  let isCast = n.exprKind == CastC
  inc n
  let destType = genTypeLLVM(c, n)
  var val = LLValue(); genExprLLVM(c, n, val)
  skipParRi n

  let srcType = c.str(val.typ)

  if srcType == destType:
    result = val
    return

  let t = c.temp()

  # Determine the appropriate conversion instruction
  if destType == "ptr" and srcType == "ptr":
    result = val # ptr to ptr is a no-op with opaque pointers
    return
  elif destType == "ptr":
    # int to ptr
    c.emitLine "  " & c.str(t) & " = inttoptr " & srcType & " " & c.str(val.name) & " to ptr"
  elif srcType == "ptr":
    # ptr to int
    c.emitLine "  " & c.str(t) & " = ptrtoint ptr " & c.str(val.name) & " to " & destType
  elif srcType in ["float", "double", "fp128"] and destType in ["float", "double", "fp128"]:
    # float to float
    if srcType == "float" and destType == "double":
      c.emitLine "  " & c.str(t) & " = fpext float " & c.str(val.name) & " to double"
    elif srcType == "double" and destType == "float":
      c.emitLine "  " & c.str(t) & " = fptrunc double " & c.str(val.name) & " to float"
    else:
      c.emitLine "  " & c.str(t) & " = fpext " & srcType & " " & c.str(val.name) & " to " & destType
  elif srcType in ["float", "double", "fp128"]:
    # float to int
    if isCast:
      c.emitLine "  " & c.str(t) & " = fptosi " & srcType & " " & c.str(val.name) & " to " & destType
    else:
      c.emitLine "  " & c.str(t) & " = fptosi " & srcType & " " & c.str(val.name) & " to " & destType
  elif destType in ["float", "double", "fp128"]:
    # int to float
    c.emitLine "  " & c.str(t) & " = sitofp " & srcType & " " & c.str(val.name) & " to " & destType
  else:
    # int to int (resize)
    let srcBits = srcType.replace("i", "").parseInt
    let destBits = destType.replace("i", "").parseInt
    if srcBits < destBits:
      if isCast:
        c.emitLine "  " & c.str(t) & " = zext " & srcType & " " & c.str(val.name) & " to " & destType
      else:
        c.emitLine "  " & c.str(t) & " = sext " & srcType & " " & c.str(val.name) & " to " & destType
    elif srcBits > destBits:
      c.emitLine "  " & c.str(t) & " = trunc " & srcType & " " & c.str(val.name) & " to " & destType
    else:
      c.emitLine "  " & c.str(t) & " = bitcast " & srcType & " " & c.str(val.name) & " to " & destType

  result = LLValue(name: t, typ: c.tok(destType))

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
  var obj = LLValue(); genExprLLVM(c, n, obj) # should be a pointer

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
  let fldIdx = fieldIndex(c.m, curBody, fldSym)

  let t = c.temp()
  c.emitLine "  " & c.str(t) & " = getelementptr inbounds " & gepType & ", ptr " & gepTarget & ", i32 0, i32 " & $fldIdx
  result = LLValue(name: t, typ: LToken(PtrToken))

proc genAtLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  ## Array indexing: (at array index)
  inc n
  let arrType = getType(c.m, n)
  var arr = LLValue(); genExprLLVM(c, n, arr)
  var idx = LLValue(); genExprLLVM(c, n, idx)
  skipParRi n

  let arrTypeName = genTypeLLVMReadOnly(c, arrType)

  # GEP into the wrapper struct's array field, then index
  let t = c.temp()
  c.emitLine "  " & c.str(t) & " = getelementptr inbounds " & arrTypeName & ", ptr " & c.str(arr.name) & ", i32 0, i32 0, " & c.str(idx.typ) & " " & c.str(idx.name)
  result = LLValue(name: t, typ: LToken(PtrToken))

proc genPatLLVM(c: var LLVMCode; n: var Cursor; result: var LLValue) =
  ## Pointer arithmetic indexing: (pat ptr index)
  inc n
  var base = LLValue(); genExprLLVM(c, n, base)
  var idx = LLValue(); genExprLLVM(c, n, idx)
  skipParRi n

  let t = c.temp()
  c.emitLine "  " & c.str(t) & " = getelementptr i8, ptr " & c.str(base.name) & ", " & c.str(idx.typ) & " " & c.str(idx.name)
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
  of SufC, ParC, AddrC, NilC, InfC, NeginfC, NanC, FalseC, TrueC,
     AndC, OrC, NotC, NegC, SizeofC, AlignofC, OffsetofC,
     OconstrC, AconstrC, BaseobjC,
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
      inc n
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
    # Short-circuit AND: (and lhs rhs)
    inc n
    var lhs = LLValue(); genExprLLVM(c, n, lhs)
    var rhs = LLValue(); genExprLLVM(c, n, rhs)
    skipParRi n
    # Convert both to i1 and AND
    let t1 = c.temp()
    let t2 = c.temp()
    let t3 = c.temp()
    let t4 = c.temp()
    c.emitLine "  " & c.str(t1) & " = icmp ne " & c.str(lhs.typ) & " " & c.str(lhs.name) & ", 0"
    c.emitLine "  " & c.str(t2) & " = icmp ne " & c.str(rhs.typ) & " " & c.str(rhs.name) & ", 0"
    c.emitLine "  " & c.str(t3) & " = and i1 " & c.str(t1) & ", " & c.str(t2)
    c.emitLine "  " & c.str(t4) & " = zext i1 " & c.str(t3) & " to i8"
    result = LLValue(name: t4, typ: LToken(I8Token))
  of OrC:
    inc n
    var lhs = LLValue(); genExprLLVM(c, n, lhs)
    var rhs = LLValue(); genExprLLVM(c, n, rhs)
    skipParRi n
    let t1 = c.temp()
    let t2 = c.temp()
    let t3 = c.temp()
    let t4 = c.temp()
    c.emitLine "  " & c.str(t1) & " = icmp ne " & c.str(lhs.typ) & " " & c.str(lhs.name) & ", 0"
    c.emitLine "  " & c.str(t2) & " = icmp ne " & c.str(rhs.typ) & " " & c.str(rhs.name) & ", 0"
    c.emitLine "  " & c.str(t3) & " = or i1 " & c.str(t1) & ", " & c.str(t2)
    c.emitLine "  " & c.str(t4) & " = zext i1 " & c.str(t3) & " to i8"
    result = LLValue(name: t4, typ: LToken(I8Token))
  of NotC:
    inc n
    var val = LLValue(); genExprLLVM(c, n, val)
    skipParRi n
    let t1 = c.temp()
    let t2 = c.temp()
    c.emitLine "  " & c.str(t1) & " = icmp eq " & c.str(val.typ) & " " & c.str(val.name) & ", 0"
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
    var lval = LLValue(); genPatLLVM(c, n, lval)
    let t = c.temp()
    c.emitLine "  " & c.str(t) & " = load i8, ptr " & c.str(lval.name)
    result = LLValue(name: t, typ: LToken(I8Token))
  of DotC:
    let fldType = getType(c.m, n)
    let loadType = genTypeLLVMReadOnly(c, fldType)
    var lval = LLValue(); genDotLLVM(c, n, lval)
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
    let fldIdx = fieldIndex(c.m, objBody, fldSym)
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
      # Determine target type from suffix
      var targetType = "i64"
      case suffixStr
      of "i64": targetType = "i64"
      of "i32": targetType = "i32"
      of "i16": targetType = "i16"
      of "i8": targetType = "i8"
      of "u64": targetType = "i64"
      of "u32": targetType = "i32"
      of "u16": targetType = "i16"
      of "u8": targetType = "i8"
      of "f64": targetType = "double"
      of "f32": targetType = "float"
      else: discard
      if c.str(val.typ) == targetType:
        result = val
      else:
        let t = c.temp()
        if targetType in ["float", "double"]:
          c.emitLine "  " & c.str(t) & " = sitofp " & c.str(val.typ) & " " & c.str(val.name) & " to " & targetType
        else:
          # Integer resize
          c.emitLine "  " & c.str(t) & " = sext " & c.str(val.typ) & " " & c.str(val.name) & " to " & targetType
        result = LLValue(name: t, typ: c.tok(targetType))
  of OconstrC:
    # Object constructor - build an aggregate value
    inc n
    let typ = genTypeLLVM(c, n)

    # Start with undef and insertvalue for each field
    var current = "undef"
    var fieldIdx = 0
    while n.kind != ParRi:
      if n.substructureKind == KvU:
        inc n
        skip n # field name
        var fieldVal = LLValue(); genExprLLVM(c, n, fieldVal)
        if n.kind != ParRi: skip n # inheritance depth
        skipParRi n
        let t = c.temp()
        c.emitLine "  " & c.str(t) & " = insertvalue " & typ & " " & current & ", " & c.str(fieldVal.typ) & " " & c.str(fieldVal.name) & ", " & $fieldIdx
        current = c.str(t)
        inc fieldIdx
      elif n.exprKind == OconstrC:
        # Nested object constructor (inheritance)
        var nested = LLValue(); genExprLLVM(c, n, nested)
        let t = c.temp()
        c.emitLine "  " & c.str(t) & " = insertvalue " & typ & " " & current & ", " & c.str(nested.typ) & " " & c.str(nested.name) & ", 0"
        current = c.str(t)
        fieldIdx = 1
      else:
        var fieldVal = LLValue(); genExprLLVM(c, n, fieldVal)
        let t = c.temp()
        c.emitLine "  " & c.str(t) & " = insertvalue " & typ & " " & current & ", " & c.str(fieldVal.typ) & " " & c.str(fieldVal.name) & ", " & $fieldIdx
        current = c.str(t)
        inc fieldIdx
    skipParRi n
    result = LLValue(name: c.tok(current), typ: c.tok(typ))
  of AconstrC:
    # Array constructor
    inc n
    let typ = genTypeLLVM(c, n)

    var current = "undef"
    var idx = 0
    while n.kind != ParRi:
      var elemVal = LLValue(); genExprLLVM(c, n, elemVal)
      let t = c.temp()
      c.emitLine "  " & c.str(t) & " = insertvalue " & typ & " " & current & ", " & c.str(elemVal.typ) & " " & c.str(elemVal.name) & ", 0, " & $idx
      current = c.str(t)
      inc idx
    skipParRi n
    result = LLValue(name: c.tok(current), typ: c.tok(typ))
  of BaseobjC:
    # Base object access: (baseobj TargetType depth expr)
    inc n
    let targetType = genTypeLLVM(c, n)
    var counter = pool.integers[n.intId]
    skip n
    var obj = LLValue(); genExprLLVM(c, n, obj)
    skipParRi n
    # Navigate through inheritance chain via GEP field 0 (base field)
    var current = c.str(obj.name)
    var curTyp = c.str(obj.typ)
    for i in 0 ..< counter:
      let t = c.temp()
      c.emitLine "  " & c.str(t) & " = getelementptr inbounds " & curTyp & ", ptr " & current & ", i32 0, i32 0"
      current = c.str(t)
    result = LLValue(name: c.tok(current), typ: c.tok(targetType))
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
  c.emitLine "  " & c.str(t) & " = icmp ne " & c.str(result.typ) & " " & c.str(result.name) & ", 0"
  result = LLValue(name: t, typ: c.tok("i1"))
