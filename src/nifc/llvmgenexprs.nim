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

proc genExprLLVM(c: var LLVMCode; n: var Cursor): LLValue
proc genLvalueLLVM(c: var LLVMCode; n: var Cursor): LLValue
proc genCallExprLLVM(c: var LLVMCode; n: var Cursor): LLValue

proc genLoadLLVM(c: var LLVMCode; ptrVal: LLValue; loadType: string): LLValue =
  ## Generate a load instruction from a pointer.
  let t = c.temp()
  c.emitLine "  " & t & " = load " & loadType & ", ptr " & ptrVal.name
  result = llvmValue(t, loadType)

proc signedBinOp(c: var LLVMCode; n: var Cursor; op: string): LLValue =
  ## Typed binary op: (op type lhs rhs)
  inc n
  var typCursor = n
  let typ = genTypeLLVM(c, n)
  let lhs = genExprLLVM(c, n)
  let rhs = genExprLLVM(c, n)
  let t = c.temp()
  c.emitLine "  " & t & " = " & op & " " & typ & " " & lhs.name & ", " & rhs.name
  skipParRi n
  result = llvmValue(t, typ)

proc unsignedBinOp(c: var LLVMCode; n: var Cursor; signedOp, unsignedOp: string): LLValue =
  ## Binary op that differs for signed/unsigned: checks the NIF type tag.
  inc n
  let typeStart = n
  let isUnsigned = n.typeKind == UT
  var typCursor = n
  let typ = genTypeLLVM(c, n)
  let lhs = genExprLLVM(c, n)
  let rhs = genExprLLVM(c, n)
  let t = c.temp()
  let op = if isUnsigned: unsignedOp else: signedOp
  c.emitLine "  " & t & " = " & op & " " & typ & " " & lhs.name & ", " & rhs.name
  skipParRi n
  result = llvmValue(t, typ)

proc cmpOp(c: var LLVMCode; n: var Cursor; signedPred, unsignedPred: string): LLValue =
  ## Comparison op: (op lhs rhs) → i1
  inc n
  let lhs = genExprLLVM(c, n)
  let rhs = genExprLLVM(c, n)
  let t = c.temp()
  # Determine if float or int comparison
  let typ = lhs.typ
  if typ in ["float", "double", "fp128"]:
    let fpPred = case signedPred
      of "eq": "oeq"
      of "ne": "one"
      of "slt": "olt"
      of "sle": "ole"
      else: signedPred
    c.emitLine "  " & t & " = fcmp " & fpPred & " " & typ & " " & lhs.name & ", " & rhs.name
  else:
    # Check if this should be unsigned comparison
    let pred = if unsignedPred != "" and lhs.typ.startsWith("u"):
                 unsignedPred
               else:
                 signedPred
    c.emitLine "  " & t & " = icmp " & signedPred & " " & typ & " " & lhs.name & ", " & rhs.name
  skipParRi n
  result = llvmValue(t, "i1")

proc genBoolCmpOp(c: var LLVMCode; n: var Cursor; signedPred, unsignedPred: string): LLValue =
  ## Like cmpOp but extends result to i8 for NIF bool compatibility.
  let cmp = cmpOp(c, n, signedPred, unsignedPred)
  let t = c.temp()
  c.emitLine "  " & t & " = zext i1 " & cmp.name & " to i8"
  result = llvmValue(t, "i8")

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

proc genAtomicCall(c: var LLVMCode; externName: string; args: seq[LLValue]; retType: string): LLValue =
  ## Translate __atomic_* GCC builtins to LLVM atomic instructions.
  let ordering = memoryOrderToLLVM(args[^1]) # last arg is always memory order
  case externName
  of "__atomic_load_n":
    # __atomic_load_n(ptr, memorder) -> LLVM: load atomic type, ptr p ordering
    let t = c.temp()
    c.emitLine "  " & t & " = load atomic " & retType & ", ptr " & args[0].name & " " & ordering
    result = llvmValue(t, retType)
  of "__atomic_store_n":
    # __atomic_store_n(ptr, val, memorder) -> LLVM: store atomic type val, ptr p ordering
    c.emitLine "  store atomic " & args[1].typ & " " & args[1].name & ", ptr " & args[0].name & " " & ordering
    result = llvmValue("", "void")
  of "__atomic_exchange_n":
    # __atomic_exchange_n(ptr, val, memorder) -> LLVM: atomicrmw xchg
    let t = c.temp()
    c.emitLine "  " & t & " = atomicrmw xchg ptr " & args[0].name & ", " & args[1].typ & " " & args[1].name & " " & ordering
    result = llvmValue(t, retType)
  of "__atomic_compare_exchange_n":
    # __atomic_compare_exchange_n(ptr, expected_ptr, desired, weak, succ_order, fail_order)
    # -> LLVM: cmpxchg ptr, old, new ordering ordering
    let loadExpected = c.temp()
    c.emitLine "  " & loadExpected & " = load " & retType & ", ptr " & args[1].name
    let t = c.temp()
    c.emitLine "  " & t & " = cmpxchg ptr " & args[0].name & ", " & retType & " " & loadExpected & ", " & args[2].typ & " " & args[2].name & " " & ordering & " " & ordering
    # cmpxchg returns {T, i1}; extract the success flag
    let success = c.temp()
    c.emitLine "  " & success & " = extractvalue { " & retType & ", i1 } " & t & ", 1"
    # Store back the old value into the expected pointer on failure
    let oldVal = c.temp()
    c.emitLine "  " & oldVal & " = extractvalue { " & retType & ", i1 } " & t & ", 0"
    c.emitLine "  store " & retType & " " & oldVal & ", ptr " & args[1].name
    let r = c.temp()
    c.emitLine "  " & r & " = zext i1 " & success & " to i8"
    result = llvmValue(r, "i8")
  of "__atomic_add_fetch":
    # __atomic_add_fetch(ptr, val, memorder) -> atomicrmw add, then add
    let t = c.temp()
    c.emitLine "  " & t & " = atomicrmw add ptr " & args[0].name & ", " & args[1].typ & " " & args[1].name & " " & ordering
    let r = c.temp()
    c.emitLine "  " & r & " = add " & retType & " " & t & ", " & args[1].name
    result = llvmValue(r, retType)
  of "__atomic_sub_fetch":
    let t = c.temp()
    c.emitLine "  " & t & " = atomicrmw sub ptr " & args[0].name & ", " & args[1].typ & " " & args[1].name & " " & ordering
    let r = c.temp()
    c.emitLine "  " & r & " = sub " & retType & " " & t & ", " & args[1].name
    result = llvmValue(r, retType)
  of "__atomic_fetch_add":
    let t = c.temp()
    c.emitLine "  " & t & " = atomicrmw add ptr " & args[0].name & ", " & args[1].typ & " " & args[1].name & " " & ordering
    result = llvmValue(t, retType)
  of "__atomic_fetch_sub":
    let t = c.temp()
    c.emitLine "  " & t & " = atomicrmw sub ptr " & args[0].name & ", " & args[1].typ & " " & args[1].name & " " & ordering
    result = llvmValue(t, retType)
  of "__atomic_fetch_and":
    let t = c.temp()
    c.emitLine "  " & t & " = atomicrmw and ptr " & args[0].name & ", " & args[1].typ & " " & args[1].name & " " & ordering
    result = llvmValue(t, retType)
  of "__atomic_fetch_or":
    let t = c.temp()
    c.emitLine "  " & t & " = atomicrmw or ptr " & args[0].name & ", " & args[1].typ & " " & args[1].name & " " & ordering
    result = llvmValue(t, retType)
  of "__atomic_fetch_xor":
    let t = c.temp()
    c.emitLine "  " & t & " = atomicrmw xor ptr " & args[0].name & ", " & args[1].typ & " " & args[1].name & " " & ordering
    result = llvmValue(t, retType)
  of "__atomic_test_and_set":
    # atomicrmw xchg ptr, i8 1
    let t = c.temp()
    c.emitLine "  " & t & " = atomicrmw xchg ptr " & args[0].name & ", i8 1 " & ordering
    let r = c.temp()
    c.emitLine "  " & r & " = icmp ne i8 " & t & ", 0"
    let r2 = c.temp()
    c.emitLine "  " & r2 & " = zext i1 " & r & " to i8"
    result = llvmValue(r2, "i8")
  of "__atomic_clear":
    # store atomic i8 0, ptr p
    c.emitLine "  store atomic i8 0, ptr " & args[0].name & " " & ordering
    result = llvmValue("", "void")
  of "__atomic_thread_fence":
    c.emitLine "  fence " & ordering
    result = llvmValue("", "void")
  of "__atomic_signal_fence":
    # LLVM doesn't have a direct signal fence; use singlethread fence
    c.emitLine "  fence syncscope(\"singlethread\") " & ordering
    result = llvmValue("", "void")
  else:
    # Unknown atomic - fall through to regular call
    let argStr = args.mapIt(it.typ & " " & it.name).join(", ")
    if retType == "void":
      c.emitLine "  call void @" & externName & "(" & argStr & ")"
      result = llvmValue("", "void")
    else:
      let t = c.temp()
      c.emitLine "  " & t & " = call " & retType & " @" & externName & "(" & argStr & ")"
      result = llvmValue(t, retType)

proc genMemIntrinsicCall(c: var LLVMCode; externName: string; args: seq[LLValue]; retType: string): LLValue =
  ## Translate memcpy/memset/memcmp to LLVM intrinsics or instructions.
  case externName
  of "memcpy":
    # memcpy(dest, src, size) -> llvm.memcpy.p0.p0.i64
    c.emitLine "  call void @llvm.memcpy.p0.p0.i64(ptr " & args[0].name & ", ptr " & args[1].name & ", i64 " & args[2].name & ", i1 false)"
    if "llvm.memcpy.p0.p0.i64" notin c.declaredExterns:
      c.declaredExterns.incl "llvm.memcpy.p0.p0.i64"
      c.externs.add "declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg)\n"
    result = llvmValue(args[0].name, "ptr") # memcpy returns dest
  of "memmove":
    c.emitLine "  call void @llvm.memmove.p0.p0.i64(ptr " & args[0].name & ", ptr " & args[1].name & ", i64 " & args[2].name & ", i1 false)"
    if "llvm.memmove.p0.p0.i64" notin c.declaredExterns:
      c.declaredExterns.incl "llvm.memmove.p0.p0.i64"
      c.externs.add "declare void @llvm.memmove.p0.p0.i64(ptr nocapture writeonly, ptr nocapture readonly, i64, i1 immarg)\n"
    result = llvmValue(args[0].name, "ptr")
  of "memset":
    # memset(dest, val, size) -> llvm.memset.p0.i64
    # Note: LLVM memset takes i8 val, C memset takes int val
    let val8 = c.temp()
    c.emitLine "  " & val8 & " = trunc " & args[1].typ & " " & args[1].name & " to i8"
    c.emitLine "  call void @llvm.memset.p0.i64(ptr " & args[0].name & ", i8 " & val8 & ", i64 " & args[2].name & ", i1 false)"
    if "llvm.memset.p0.i64" notin c.declaredExterns:
      c.declaredExterns.incl "llvm.memset.p0.i64"
      c.externs.add "declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg)\n"
    result = llvmValue(args[0].name, "ptr")
  of "memcmp":
    # memcmp(a, b, size) -> call i32 @memcmp(ptr, ptr, i64)
    let t = c.temp()
    c.emitLine "  " & t & " = call i32 @memcmp(ptr " & args[0].name & ", ptr " & args[1].name & ", i64 " & args[2].name & ")"
    if "memcmp" notin c.declaredExterns:
      c.declaredExterns.incl "memcmp"
      c.externs.add "declare i32 @memcmp(ptr nocapture, ptr nocapture, i64)\n"
    result = llvmValue(t, "i32")
  else:
    # Not a memory intrinsic
    let argStr = args.mapIt(it.typ & " " & it.name).join(", ")
    if retType == "void":
      c.emitLine "  call void @" & externName & "(" & argStr & ")"
      result = llvmValue("", "void")
    else:
      let t = c.temp()
      c.emitLine "  " & t & " = call " & retType & " @" & externName & "(" & argStr & ")"
      result = llvmValue(t, retType)

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

proc genCallWithType(c: var LLVMCode; n: var Cursor; retType: string): LLValue =
  ## Generate a call where we know the return type from context.
  ## Intercepts special C functions and emits LLVM native instructions.
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
    let callee = genExprLLVM(c, n)
    calleeName = callee.name

  var args: seq[LLValue] = @[]
  while n.kind != ParRi:
    args.add genExprLLVM(c, n)
  skipParRi n

  # Check for special C functions that map to LLVM instructions
  if calleeExtern != "":
    if calleeExtern in AtomicBuiltins:
      return genAtomicCall(c, calleeExtern, args, retType)
    if calleeExtern in MemIntrinsics:
      return genMemIntrinsicCall(c, calleeExtern, args, retType)

  let argStr = args.mapIt(it.typ & " " & it.name).join(", ")

  if retType == "void":
    c.emitLine "  call void " & calleeName & "(" & argStr & ")"
    result = llvmValue("", "void")
  else:
    let t = c.temp()
    c.emitLine "  " & t & " = call " & retType & " " & calleeName & "(" & argStr & ")"
    result = llvmValue(t, retType)

proc genCallLLVM(c: var LLVMCode; n: var Cursor): LLValue =
  ## Call without known return type - used when context doesn't provide type info.
  result = genCallWithType(c, n, "ptr")

proc genCallExprLLVM(c: var LLVMCode; n: var Cursor): LLValue =
  ## Call expression, alias for genCallLLVM.
  result = genCallLLVM(c, n)

proc genConvOrCast(c: var LLVMCode; n: var Cursor): LLValue =
  ## Handle (conv type expr) and (cast type expr)
  let isCast = n.exprKind == CastC
  inc n
  var typCursor = n
  let destType = genTypeLLVM(c, n)
  let val = genExprLLVM(c, n)
  skipParRi n

  let srcType = val.typ

  if srcType == destType:
    return val

  let t = c.temp()

  # Determine the appropriate conversion instruction
  if destType == "ptr" and srcType == "ptr":
    return val # ptr to ptr is a no-op with opaque pointers
  elif destType == "ptr":
    # int to ptr
    c.emitLine "  " & t & " = inttoptr " & srcType & " " & val.name & " to ptr"
  elif srcType == "ptr":
    # ptr to int
    c.emitLine "  " & t & " = ptrtoint ptr " & val.name & " to " & destType
  elif srcType in ["float", "double", "fp128"] and destType in ["float", "double", "fp128"]:
    # float to float
    if srcType == "float" and destType == "double":
      c.emitLine "  " & t & " = fpext float " & val.name & " to double"
    elif srcType == "double" and destType == "float":
      c.emitLine "  " & t & " = fptrunc double " & val.name & " to float"
    else:
      c.emitLine "  " & t & " = fpext " & srcType & " " & val.name & " to " & destType
  elif srcType in ["float", "double", "fp128"]:
    # float to int
    if isCast:
      c.emitLine "  " & t & " = fptosi " & srcType & " " & val.name & " to " & destType
    else:
      c.emitLine "  " & t & " = fptosi " & srcType & " " & val.name & " to " & destType
  elif destType in ["float", "double", "fp128"]:
    # int to float
    c.emitLine "  " & t & " = sitofp " & srcType & " " & val.name & " to " & destType
  else:
    # int to int (resize)
    let srcBits = srcType.replace("i", "").parseInt
    let destBits = destType.replace("i", "").parseInt
    if srcBits < destBits:
      if isCast:
        c.emitLine "  " & t & " = zext " & srcType & " " & val.name & " to " & destType
      else:
        c.emitLine "  " & t & " = sext " & srcType & " " & val.name & " to " & destType
    elif srcBits > destBits:
      c.emitLine "  " & t & " = trunc " & srcType & " " & val.name & " to " & destType
    else:
      c.emitLine "  " & t & " = bitcast " & srcType & " " & val.name & " to " & destType

  result = llvmValue(t, destType)

proc genAddrLLVM(c: var LLVMCode; n: var Cursor): LLValue =
  inc n
  let lval = genLvalueLLVM(c, n)
  # Skip optional CppRefQ qualifier
  if n.kind != ParRi and n.typeQual == CppRefQ:
    skip n
  skipParRi n
  # The lvalue is already a pointer
  result = llvmValue(lval.name, "ptr")

proc genDerefLLVM(c: var LLVMCode; n: var Cursor): LLValue =
  inc n
  let ptrVal = genExprLLVM(c, n)
  # Skip optional CppRefQ qualifier
  if n.kind != ParRi and n.typeQual == CppRefQ:
    skip n
  skipParRi n
  # Return the pointer itself as an lvalue (the load happens at use site)
  result = llvmValue(ptrVal.name, "ptr")

proc genDotLLVM(c: var LLVMCode; n: var Cursor): LLValue =
  ## Field access: (dot obj field inheritanceDepth?)
  inc n
  let objType = getNominalType(c.m, n)
  let obj = genExprLLVM(c, n) # should be a pointer

  # Get field symbol
  var fld = n
  let fldSym = n.symId
  skip n

  # Get inheritance depth
  var inhDepth = 0
  if n.kind == IntLit:
    inhDepth = int(pool.integers[n.intId])
    inc n

  skipParRi n

  # Navigate through inheritance to find field index
  # For now, a simplified approach: look up the field index from the object body
  let objBody = navigateToObjectBody(c.m, objType)
  var fieldIndex = 0
  var found = false

  if objBody.typeKind in {ObjectT, UnionT}:
    var body = objBody
    inc body
    if objBody.typeKind == ObjectT:
      if body.kind == Symbol:
        inc body # skip base type
        fieldIndex = 1 # base type is field 0
      elif body.kind == DotToken:
        inc body

    var nested = 1
    while nested > 0:
      case body.kind
      of ParRi:
        dec nested
        inc body
      of ParLe:
        if body.substructureKind == FldU:
          let decl = takeFieldDecl(body)
          if decl.name.kind == SymbolDef and decl.name.symId == fldSym:
            found = true
            break
          inc fieldIndex
        else:
          skip body
      else:
        inc body

  # Generate GEP to access the field
  let objTypeName = genTypeLLVMReadOnly(c, objType)
  var gepTarget = obj.name
  var gepType = objTypeName

  # Navigate through inheritance chain
  for i in 0 ..< inhDepth:
    let t = c.temp()
    c.emitLine "  " & t & " = getelementptr inbounds " & gepType & ", ptr " & gepTarget & ", i32 0, i32 0"
    gepTarget = t
    # We'd need to resolve the base type here; simplification
    gepType = gepType # TODO: track base type properly

  let t = c.temp()
  c.emitLine "  " & t & " = getelementptr inbounds " & gepType & ", ptr " & gepTarget & ", i32 0, i32 " & $fieldIndex
  result = llvmValue(t, "ptr")

proc genAtLLVM(c: var LLVMCode; n: var Cursor): LLValue =
  ## Array indexing: (at array index)
  inc n
  let arrType = getType(c.m, n)
  let arr = genExprLLVM(c, n)
  let idx = genExprLLVM(c, n)
  skipParRi n

  let arrTypeName = genTypeLLVMReadOnly(c, arrType)

  # GEP into the wrapper struct's array field, then index
  let t = c.temp()
  c.emitLine "  " & t & " = getelementptr inbounds " & arrTypeName & ", ptr " & arr.name & ", i32 0, i32 0, " & idx.typ & " " & idx.name
  result = llvmValue(t, "ptr")

proc genPatLLVM(c: var LLVMCode; n: var Cursor): LLValue =
  ## Pointer arithmetic indexing: (pat ptr index)
  inc n
  let base = genExprLLVM(c, n)
  let idx = genExprLLVM(c, n)
  skipParRi n

  let t = c.temp()
  c.emitLine "  " & t & " = getelementptr i8, ptr " & base.name & ", " & idx.typ & " " & idx.name
  result = llvmValue(t, "ptr")

proc genSizeofLLVM(c: var LLVMCode; n: var Cursor): LLValue =
  inc n
  var typCursor = n
  let typ = genTypeLLVM(c, n)
  skipParRi n

  # Use getelementptr null trick to compute sizeof
  let t1 = c.temp()
  let t2 = c.temp()
  c.emitLine "  " & t1 & " = getelementptr " & typ & ", ptr null, i32 1"
  c.emitLine "  " & t2 & " = ptrtoint ptr " & t1 & " to i" & $c.bits
  result = llvmValue(t2, "i" & $c.bits)

proc isGlobalSym(c: var LLVMCode; s: SymId): bool =
  let d = c.m.getDeclOrNil(s)
  result = d != nil and d.kind in {GvarY, TvarY, ConstY, ProcY}

proc genLvalueLLVM(c: var LLVMCode; n: var Cursor): LLValue =
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
        result = llvmValue("@" & name, "ptr")
      else:
        result = llvmValue("%" & name, "ptr")
    else:
      error c.m, "expected expression but got: ", n
  of DerefC:
    # Dereference gives us the pointer value
    inc n
    let ptrVal = genExprLLVM(c, n)
    if n.kind != ParRi and n.typeQual == CppRefQ:
      skip n
    skipParRi n
    result = llvmValue(ptrVal.name, "ptr")
  of AtC:
    result = genAtLLVM(c, n)
  of PatC:
    result = genPatLLVM(c, n)
  of DotC:
    result = genDotLLVM(c, n)
  of ErrvC:
    # Error flag is a global
    result = llvmValue("@NIFC_ERR_", "ptr")
    skip n
  of OvfC:
    # Overflow flag
    result = llvmValue("@NIFC_OVF_", "ptr")
    skip n
  of SufC, ParC, AddrC, NilC, InfC, NeginfC, NanC, FalseC, TrueC,
     AndC, OrC, NotC, NegC, SizeofC, AlignofC, OffsetofC,
     OconstrC, AconstrC, BaseobjC,
     AddC, SubC, MulC, DivC, ModC, ShrC, ShlC,
     BitandC, BitorC, BitxorC, BitnotC,
     EqC, NeqC, LeC, LtC, CastC, ConvC, CallC:
    error c.m, "not an lvalue: ", n

proc genExprLLVM(c: var LLVMCode; n: var Cursor): LLValue =
  case n.exprKind
  of NoExpr:
    case n.kind
    of IntLit:
      let i = pool.integers[n.intId]
      inc n
      result = llvmValue($i, "i" & $c.bits)
    of UIntLit:
      let i = pool.uintegers[n.uintId]
      inc n
      result = llvmValue($i, "i" & $c.bits)
    of FloatLit:
      let f = pool.floats[n.floatId]
      inc n
      result = llvmValue($f, "double")
    of CharLit:
      let ch = n.charLit
      inc n
      result = llvmValue($ord(ch), "i8")
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
      c.globals.add globalName & " = private unnamed_addr constant [" & $(s.len + 1) & " x i8] c\"" & escaped & "\\00\"\n"
      result = llvmValue(globalName, "ptr")
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
      c.emitLine "  " & t & " = load " & typ & ", ptr " & prefix & name
      result = llvmValue(t, typ)
    else:
      let lval = genLvalueLLVM(c, n)
      # Load the value
      let t = c.temp()
      c.emitLine "  " & t & " = load i64, ptr " & lval.name  # fallback type
      result = llvmValue(t, "i64")
  of FalseC:
    skip n
    result = llvmValue("0", "i8")
  of TrueC:
    skip n
    result = llvmValue("1", "i8")
  of NilC:
    skip n
    result = llvmValue("null", "ptr")
  of InfC:
    skip n
    result = llvmValue("0x7FF0000000000000", "double") # +inf in IEEE 754
  of NegInfC:
    skip n
    result = llvmValue("0xFFF0000000000000", "double") # -inf
  of NanC:
    skip n
    result = llvmValue("0x7FF8000000000000", "double") # NaN
  of AddC: result = signedBinOp(c, n, "add")
  of SubC: result = signedBinOp(c, n, "sub")
  of MulC: result = signedBinOp(c, n, "mul")
  of DivC: result = unsignedBinOp(c, n, "sdiv", "udiv")
  of ModC: result = unsignedBinOp(c, n, "srem", "urem")
  of ShlC: result = signedBinOp(c, n, "shl")
  of ShrC: result = unsignedBinOp(c, n, "ashr", "lshr")
  of BitandC: result = signedBinOp(c, n, "and")
  of BitorC: result = signedBinOp(c, n, "or")
  of BitxorC: result = signedBinOp(c, n, "xor")
  of BitnotC:
    # (bitnot type expr)
    inc n
    var typCursor = n
    let typ = genTypeLLVM(c, n)
    let val = genExprLLVM(c, n)
    skipParRi n
    let t = c.temp()
    c.emitLine "  " & t & " = xor " & typ & " " & val.name & ", -1"
    result = llvmValue(t, typ)
  of NegC:
    # (neg type expr)
    inc n
    var typCursor = n
    let typ = genTypeLLVM(c, n)
    let val = genExprLLVM(c, n)
    skipParRi n
    let t = c.temp()
    if typ in ["float", "double", "fp128"]:
      c.emitLine "  " & t & " = fneg " & typ & " " & val.name
    else:
      c.emitLine "  " & t & " = sub " & typ & " 0, " & val.name
    result = llvmValue(t, typ)
  of EqC: result = genBoolCmpOp(c, n, "eq", "eq")
  of NeqC: result = genBoolCmpOp(c, n, "ne", "ne")
  of LeC: result = genBoolCmpOp(c, n, "sle", "ule")
  of LtC: result = genBoolCmpOp(c, n, "slt", "ult")
  of AndC:
    # Short-circuit AND: (and lhs rhs)
    inc n
    let lhs = genExprLLVM(c, n)
    let rhs = genExprLLVM(c, n)
    skipParRi n
    # Convert both to i1 and AND
    let t1 = c.temp()
    let t2 = c.temp()
    let t3 = c.temp()
    let t4 = c.temp()
    c.emitLine "  " & t1 & " = icmp ne " & lhs.typ & " " & lhs.name & ", 0"
    c.emitLine "  " & t2 & " = icmp ne " & rhs.typ & " " & rhs.name & ", 0"
    c.emitLine "  " & t3 & " = and i1 " & t1 & ", " & t2
    c.emitLine "  " & t4 & " = zext i1 " & t3 & " to i8"
    result = llvmValue(t4, "i8")
  of OrC:
    inc n
    let lhs = genExprLLVM(c, n)
    let rhs = genExprLLVM(c, n)
    skipParRi n
    let t1 = c.temp()
    let t2 = c.temp()
    let t3 = c.temp()
    let t4 = c.temp()
    c.emitLine "  " & t1 & " = icmp ne " & lhs.typ & " " & lhs.name & ", 0"
    c.emitLine "  " & t2 & " = icmp ne " & rhs.typ & " " & rhs.name & ", 0"
    c.emitLine "  " & t3 & " = or i1 " & t1 & ", " & t2
    c.emitLine "  " & t4 & " = zext i1 " & t3 & " to i8"
    result = llvmValue(t4, "i8")
  of NotC:
    inc n
    let val = genExprLLVM(c, n)
    skipParRi n
    let t1 = c.temp()
    let t2 = c.temp()
    c.emitLine "  " & t1 & " = icmp eq " & val.typ & " " & val.name & ", 0"
    c.emitLine "  " & t2 & " = zext i1 " & t1 & " to i8"
    result = llvmValue(t2, "i8")
  of CastC, ConvC:
    result = genConvOrCast(c, n)
  of CallC:
    # For calls, we need to figure out the return type
    # Parse the call to get the callee and look up its return type
    var saved = n
    inc saved
    let calleeType = getType(c.m, saved)
    var retType = "ptr" # fallback
    if calleeType.typeKind == ProctypeT or calleeType.symKind == ProcY:
      var ct = calleeType
      if ct.typeKind == ProctypeT or ct.symKind == ProcY:
        inc ct
        skip ct # name
      if ct.typeKind == ParamsT:
        var params = ct
        skip params # skip params to get return type
        retType = genTypeLLVMReadOnly(c, params)
    result = genCallWithType(c, n, retType)
  of AddrC:
    result = genAddrLLVM(c, n)
  of DerefC:
    # Dereference a pointer: load from the pointer
    inc n
    let ptrVal = genExprLLVM(c, n)
    if n.kind != ParRi and n.typeQual == CppRefQ:
      skip n
    skipParRi n
    # We need the target type - use ptr as generic
    let t = c.temp()
    c.emitLine "  " & t & " = load ptr, ptr " & ptrVal.name
    result = llvmValue(t, "ptr")
  of AtC:
    let lval = genAtLLVM(c, n)
    let t = c.temp()
    c.emitLine "  " & t & " = load i64, ptr " & lval.name # TODO: proper element type
    result = llvmValue(t, "i64")
  of PatC:
    let lval = genPatLLVM(c, n)
    let t = c.temp()
    c.emitLine "  " & t & " = load i8, ptr " & lval.name
    result = llvmValue(t, "i8")
  of DotC:
    let lval = genDotLLVM(c, n)
    # Load from the field pointer
    let t = c.temp()
    c.emitLine "  " & t & " = load i64, ptr " & lval.name # TODO: proper field type
    result = llvmValue(t, "i64")
  of SizeofC:
    result = genSizeofLLVM(c, n)
  of AlignofC:
    # Approximation: emit sizeof as alignof (correct for power-of-2 types)
    result = genSizeofLLVM(c, n)
  of OffsetofC:
    inc n
    var typCursor = n
    let typ = genTypeLLVM(c, n)
    let fieldSym = n.symId
    inc n
    skipParRi n
    # Use GEP from null to compute offset
    let t1 = c.temp()
    let t2 = c.temp()
    # We'd need the field index - simplified to 0 for now
    c.emitLine "  " & t1 & " = getelementptr " & typ & ", ptr null, i32 0, i32 0"
    c.emitLine "  " & t2 & " = ptrtoint ptr " & t1 & " to i" & $c.bits
    result = llvmValue(t2, "i" & $c.bits)
  of ParC:
    inc n
    result = genExprLLVM(c, n)
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
      result = genExprLLVM(c, value)
    else:
      let val = genExprLLVM(c, value)
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
      if val.typ == targetType:
        result = val
      else:
        let t = c.temp()
        if targetType in ["float", "double"]:
          c.emitLine "  " & t & " = sitofp " & val.typ & " " & val.name & " to " & targetType
        else:
          # Integer resize
          c.emitLine "  " & t & " = sext " & val.typ & " " & val.name & " to " & targetType
        result = llvmValue(t, targetType)
  of OconstrC:
    # Object constructor - build an aggregate value
    inc n
    var typCursor = n
    let typ = genTypeLLVM(c, n)

    # Start with undef and insertvalue for each field
    var current = "undef"
    var fieldIdx = 0
    while n.kind != ParRi:
      if n.substructureKind == KvU:
        inc n
        skip n # field name
        let fieldVal = genExprLLVM(c, n)
        if n.kind != ParRi: skip n # inheritance depth
        skipParRi n
        let t = c.temp()
        c.emitLine "  " & t & " = insertvalue " & typ & " " & current & ", " & fieldVal.typ & " " & fieldVal.name & ", " & $fieldIdx
        current = t
        inc fieldIdx
      elif n.exprKind == OconstrC:
        # Nested object constructor (inheritance)
        let nested = genExprLLVM(c, n)
        let t = c.temp()
        c.emitLine "  " & t & " = insertvalue " & typ & " " & current & ", " & nested.typ & " " & nested.name & ", 0"
        current = t
        fieldIdx = 1
      else:
        let fieldVal = genExprLLVM(c, n)
        let t = c.temp()
        c.emitLine "  " & t & " = insertvalue " & typ & " " & current & ", " & fieldVal.typ & " " & fieldVal.name & ", " & $fieldIdx
        current = t
        inc fieldIdx
    skipParRi n
    result = llvmValue(current, typ)
  of AconstrC:
    # Array constructor
    inc n
    var typCursor = n
    let typ = genTypeLLVM(c, n)

    var current = "undef"
    var idx = 0
    while n.kind != ParRi:
      let elemVal = genExprLLVM(c, n)
      let t = c.temp()
      c.emitLine "  " & t & " = insertvalue " & typ & " " & current & ", " & elemVal.typ & " " & elemVal.name & ", 0, " & $idx
      current = t
      inc idx
    skipParRi n
    result = llvmValue(current, typ)
  of BaseobjC:
    # Base object access with inheritance depth
    inc n
    skip n # type
    var counter = pool.integers[n.intId]
    skip n
    let obj = genExprLLVM(c, n)
    skipParRi n
    # Navigate through inheritance chain with GEP
    var current = obj.name
    for i in 0 ..< counter:
      let t = c.temp()
      c.emitLine "  " & t & " = extractvalue " & obj.typ & " " & current & ", 0"
      current = t
    result = llvmValue(current, obj.typ) # approximate
  of ErrvC, OvfC:
    let lval = genLvalueLLVM(c, n)
    let t = c.temp()
    c.emitLine "  " & t & " = load i8, ptr " & lval.name
    result = llvmValue(t, "i8")

proc genCondLLVM(c: var LLVMCode; n: var Cursor): LLValue =
  ## Generate a condition expression, returning an i1 value for branch instructions.
  let val = genExprLLVM(c, n)
  if val.typ == "i1":
    return val
  # Convert to i1 by comparing with 0
  let t = c.temp()
  c.emitLine "  " & t & " = icmp ne " & val.typ & " " & val.name & ", 0"
  result = llvmValue(t, "i1")
