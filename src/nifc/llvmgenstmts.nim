#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included from llvmcodegen.nim
# Generates LLVM IR for statements.

proc getVirtualGuardLLVM(c: var LLVMCode; n: Cursor): (SymId, bool) =
  result = (SymId(0), false)
  var n = n
  if n.exprKind == NotC:
    inc n
    var isLast = false
    if n.stmtKind == LabS:
      isLast = true
      inc n
    elif n.kind == Symbol:
      if c.currentProc.vflags.contains(n.symId):
        result = (n.symId, isLast)

proc genOnErrorLLVM(c: var LLVMCode; n: var Cursor) =
  let errVal = c.temp()
  c.emitLine "  " & c.str(errVal) & " = load i8, ptr @NIFC_ERR_"
  let cond = c.temp()
  c.emitLine "  " & c.str(cond) & " = icmp ne i8 " & c.str(errVal) & ", 0"
  let thenLabel = c.label()
  let endLabel = c.label()
  c.emitLine "  br i1 " & c.str(cond) & ", label %" & c.str(thenLabel) & ", label %" & c.str(endLabel)
  c.emitLine c.str(thenLabel) & ":"
  c.currentProc.needsTerminator = false
  genStmtLLVM c, n
  if not c.currentProc.needsTerminator:
    c.emitLine "  br label %" & c.str(endLabel)
  c.emitLine c.str(endLabel) & ":"
  c.currentProc.needsTerminator = false

proc genIfLLVM(c: var LLVMCode; n: var Cursor) =
  inc n
  var endLabel = c.label()
  var needsEnd = false

  while n.kind != ParRi:
    case n.substructureKind
    of ElifU:
      inc n
      var cond = LLValue(); genCondLLVM(c, n, cond)
      let thenLabel = c.label()
      let elseLabel = c.label()
      c.emitLine "  br i1 " & c.str(cond.name) & ", label %" & c.str(thenLabel) & ", label %" & c.str(elseLabel)
      c.emitLine c.str(thenLabel) & ":"
      c.currentProc.needsTerminator = false
      genStmtLLVM c, n
      if not c.currentProc.needsTerminator:
        c.emitLine "  br label %" & c.str(endLabel)
      c.emitLine c.str(elseLabel) & ":"
      c.currentProc.needsTerminator = false
      needsEnd = true
      skipParRi n
    of ElseU:
      inc n
      genStmtLLVM c, n
      if not c.currentProc.needsTerminator:
        c.emitLine "  br label %" & c.str(endLabel)
      needsEnd = true
      skipParRi n
    else:
      error c.m, "`if` expects `elif` or `else` but got: ", n

  skipParRi n
  if needsEnd:
    if not c.currentProc.needsTerminator:
      c.emitLine "  br label %" & c.str(endLabel)
    c.emitLine c.str(endLabel) & ":"
    c.currentProc.needsTerminator = false

proc genIteLLVM(c: var LLVMCode; n: var Cursor) =
  ## If-then-else: (ite cond then else?)
  inc n

  # Check for virtual flag optimization
  let (vflag, isLast) = getVirtualGuardLLVM(c, n)
  if vflag != SymId(0):
    skip n # skip condition
    genStmtLLVM c, n # then-part always taken
    if isLast:
      let labelName = mangleToC(pool.syms[vflag])
      c.emitLine "  br label %" & labelName
      c.emitLine labelName & ":"
      c.currentProc.needsTerminator = false
    skip n # else-part ignored
  else:
    var cond = LLValue(); genCondLLVM(c, n, cond)
    let thenLabel = c.label()
    let elseLabel = c.label()
    let endLabel = c.label()

    if n.kind != DotToken:
      c.emitLine "  br i1 " & c.str(cond.name) & ", label %" & c.str(thenLabel) & ", label %" & c.str(elseLabel)
    else:
      c.emitLine "  br i1 " & c.str(cond.name) & ", label %" & c.str(thenLabel) & ", label %" & c.str(endLabel)

    c.emitLine c.str(thenLabel) & ":"
    c.currentProc.needsTerminator = false
    genStmtLLVM c, n

    if not c.currentProc.needsTerminator:
      c.emitLine "  br label %" & c.str(endLabel)

    if n.kind != DotToken:
      c.emitLine c.str(elseLabel) & ":"
      c.currentProc.needsTerminator = false
      genStmtLLVM c, n
      if not c.currentProc.needsTerminator:
        c.emitLine "  br label %" & c.str(endLabel)
    else:
      inc n

    c.emitLine c.str(endLabel) & ":"
    c.currentProc.needsTerminator = false

  skipParRi n

proc genWhileLLVM(c: var LLVMCode; n: var Cursor) =
  inc n
  let condLabel = c.label()
  let bodyLabel = c.label()
  let endLabel = c.label()

  c.emitLine "  br label %" & c.str(condLabel)
  c.emitLine c.str(condLabel) & ":"

  var cond = LLValue(); genCondLLVM(c, n, cond)
  c.emitLine "  br i1 " & c.str(cond.name) & ", label %" & c.str(bodyLabel) & ", label %" & c.str(endLabel)

  c.emitLine c.str(bodyLabel) & ":"
  c.currentProc.needsTerminator = false
  c.currentProc.breakStack.add endLabel
  genStmtLLVM c, n
  discard c.currentProc.breakStack.pop()

  if not c.currentProc.needsTerminator:
    c.emitLine "  br label %" & c.str(condLabel)

  c.emitLine c.str(endLabel) & ":"
  c.currentProc.needsTerminator = false
  skipParRi n

proc genLoopLLVM(c: var LLVMCode; n: var Cursor) =
  ## Loop: (loop preCondStmts condition body)
  inc n
  let headerLabel = c.label()
  let bodyLabel = c.label()
  let endLabel = c.label()

  c.emitLine "  br label %" & c.str(headerLabel)
  c.emitLine c.str(headerLabel) & ":"
  c.currentProc.needsTerminator = false

  # Pre-condition statements
  genStmtLLVM c, n
  # Condition
  var cond = LLValue(); genCondLLVM(c, n, cond)
  c.emitLine "  br i1 " & c.str(cond.name) & ", label %" & c.str(bodyLabel) & ", label %" & c.str(endLabel)

  c.emitLine c.str(bodyLabel) & ":"
  c.currentProc.needsTerminator = false
  c.currentProc.breakStack.add endLabel
  genStmtLLVM c, n
  discard c.currentProc.breakStack.pop()

  if not c.currentProc.needsTerminator:
    c.emitLine "  br label %" & c.str(headerLabel)

  c.emitLine c.str(endLabel) & ":"
  c.currentProc.needsTerminator = false
  skipParRi n

proc genSwitchLLVM(c: var LLVMCode; n: var Cursor) =
  inc n
  var switchVal = LLValue(); genExprLLVM(c, n, switchVal)
  let endLabel = c.label()
  var defaultLabel = c.str(endLabel)

  # First pass: collect all case branches
  type CaseBranch = object
    values: seq[string]
    label: string

  var branches: seq[CaseBranch] = @[]
  var defaultBranch: string = ""
  var savedPos = n

  # We need to process branches to know labels before emitting the switch
  while n.kind != ParRi:
    case n.substructureKind
    of OfU:
      inc n
      var branch = CaseBranch(label: c.str(c.label()))
      # Parse ranges
      if n.substructureKind == RangesU:
        inc n
        while n.kind != ParRi:
          if n.substructureKind == RangeU:
            inc n
            # Range: add all values (simplified - just add endpoints)
            var lo = LLValue(); genExprLLVM(c, n, lo)
            var hi = LLValue(); genExprLLVM(c, n, hi)
            branch.values.add c.str(lo.name)
            skipParRi n
          else:
            var val = LLValue(); genExprLLVM(c, n, val)
            branch.values.add c.str(val.name)
        skipParRi n
      branches.add branch
      skip n # skip body for now
      skipParRi n
    of ElseU:
      defaultLabel = c.str(c.label())
      defaultBranch = defaultLabel
      inc n
      skip n
      skipParRi n
    else:
      error c.m, "`case` expects `of` or `else` but got: ", n

  # Emit switch instruction
  var switchInstr = "  switch " & c.str(switchVal.typ) & " " & c.str(switchVal.name) & ", label %" & defaultLabel & " [\n"
  for branch in branches:
    for v in branch.values:
      switchInstr.add "    " & c.str(switchVal.typ) & " " & v & ", label %" & branch.label & "\n"
  switchInstr.add "  ]\n"
  c.emitLine switchInstr

  # Now go back and generate the actual bodies
  n = savedPos
  var branchIdx = 0
  c.currentProc.breakStack.add endLabel
  while n.kind != ParRi:
    case n.substructureKind
    of OfU:
      inc n
      skip n # skip ranges
      c.emitLine branches[branchIdx].label & ":"
      c.currentProc.needsTerminator = false
      genStmtLLVM c, n
      if not c.currentProc.needsTerminator:
        c.emitLine "  br label %" & c.str(endLabel)
      skipParRi n
      inc branchIdx
    of ElseU:
      c.emitLine defaultBranch & ":"
      c.currentProc.needsTerminator = false
      inc n
      genStmtLLVM c, n
      if not c.currentProc.needsTerminator:
        c.emitLine "  br label %" & c.str(endLabel)
      skipParRi n
    else:
      error c.m, "`case` expects `of` or `else` but got: ", n
  discard c.currentProc.breakStack.pop()

  c.emitLine c.str(endLabel) & ":"
  c.currentProc.needsTerminator = false
  skipParRi n

proc genLabelLLVM(c: var LLVMCode; n: var Cursor) =
  inc n
  if n.kind == SymbolDef:
    let name = mangleToC(pool.syms[n.symId])
    # End current basic block
    if not c.currentProc.needsTerminator:
      c.emitLine "  br label %" & name
    c.emitLine name & ":"
    c.currentProc.needsTerminator = false
    inc n
  else:
    error c.m, "expected SymbolDef but got: ", n
  skipParRi n

proc genGotoLLVM(c: var LLVMCode; n: var Cursor) =
  inc n
  if n.kind == Symbol:
    let name = mangleToC(pool.syms[n.symId])
    c.emitLine "  br label %" & name
    c.currentProc.needsTerminator = true
    inc n
  else:
    error c.m, "expected Symbol but got: ", n
  skipParRi n

proc genScopeLLVM(c: var LLVMCode; n: var Cursor) =
  inc n
  c.m.openScope()
  while n.kind != ParRi:
    genStmtLLVM c, n
  skipParRi n
  c.m.closeScope()

proc genMflagDeclLLVM(c: var LLVMCode; n: var Cursor) =
  inc n
  if n.kind == SymbolDef:
    let s = n.symId
    c.m.registerLocal(s, createIntegralType(c.m, "(bool)"))
    let name = mangleToC(pool.syms[s])
    c.addAlloca(c.tok("%" & name), LToken(I8Token))
    c.emitLine "  store i8 0, ptr %" & name
    inc n
  else:
    error c.m, "expected SymbolDef but got: ", n
  skipParRi n

proc genVflagDeclLLVM(c: var LLVMCode; n: var Cursor) =
  inc n
  if n.kind == SymbolDef:
    let s = n.symId
    c.m.registerLocal(s, createIntegralType(c.m, "(bool)"))
    c.currentProc.vflags.incl(s)
    inc n
  else:
    error c.m, "expected SymbolDef but got: ", n
  skipParRi n

proc genJtrueLLVM(c: var LLVMCode; n: var Cursor) =
  inc n
  while n.kind != ParRi:
    if n.kind == Symbol:
      let s = n.symId
      if not c.currentProc.vflags.contains(s):
        error c.m, "virtual flag not declared: ", n
      inc n
      if n.kind == ParRi:
        # Last symbol is a goto target
        let name = mangleToC(pool.syms[s])
        c.emitLine "  br label %" & name
        c.currentProc.needsTerminator = true
    else:
      error c.m, "expected Symbol but got: ", n
  skipParRi n

proc genStoreLLVM(c: var LLVMCode; n: var Cursor) =
  ## Store: (store value target) - reversed operand order for eval semantics
  let storeInfo = n.info
  inc n
  var rhs = n
  skip n
  var target = LLValue(); genLvalueLLVM(c, n, target)
  var val = LLValue(); genExprLLVM(c, rhs, val)
  c.emitLineDbg "  store " & c.str(val.typ) & " " & c.str(val.name) & ", ptr " & c.str(target.name), storeInfo
  skipParRi n

proc genKeepOverflowLLVM(c: var LLVMCode; n: var Cursor) =
  ## Overflow-checked arithmetic using LLVM intrinsics
  inc n # keepovf
  let op = n.exprKind

  var intrinsic = "llvm."
  case op
  of AddC: intrinsic.add "sadd"
  of SubC: intrinsic.add "ssub"
  of MulC: intrinsic.add "smul"
  else:
    # For div/mod we don't have LLVM intrinsics, fall through to regular op
    skip n
    skip n # target
    skipParRi n
    return

  inc n # operation tag
  let isUnsigned = n.typeKind == UT
  if isUnsigned:
    intrinsic = intrinsic.replace("sadd", "uadd").replace("ssub", "usub").replace("smul", "umul")

  inc n # type tag (IT or UT)
  var bitsStr = $c.bits
  if n.kind == IntLit:
    let bits = pool.integers[n.intId]
    if bits != -1:
      bitsStr = $bits
    inc n
  skipParRi n # end of type

  let typ = "i" & bitsStr
  intrinsic.add ".with.overflow." & typ

  var lhs = LLValue(); genExprLLVM(c, n, lhs)
  var rhs = LLValue(); genExprLLVM(c, n, rhs)
  var target = LLValue(); genLvalueLLVM(c, n, target)
  skipParRi n

  # Call the intrinsic
  let result_struct = c.temp()
  c.emitLine "  " & c.str(result_struct) & " = call { " & typ & ", i1 } @" & intrinsic & "(" & typ & " " & c.str(lhs.name) & ", " & typ & " " & c.str(rhs.name) & ")"

  # Extract the result value
  let resultVal = c.temp()
  c.emitLine "  " & c.str(resultVal) & " = extractvalue { " & typ & ", i1 } " & c.str(result_struct) & ", 0"

  # Store the result
  c.emitLine "  store " & typ & " " & c.str(resultVal) & ", ptr " & c.str(target.name)

  # Extract the overflow flag
  let ovfFlag = c.temp()
  c.emitLine "  " & c.str(ovfFlag) & " = extractvalue { " & typ & ", i1 } " & c.str(result_struct) & ", 1"

  # If overflow occurred, set the overflow flag
  # OVF_ = OVF_ || overflow
  let currentOvf = c.temp()
  c.emitLine "  " & c.str(currentOvf) & " = load i8, ptr @NIFC_OVF_"
  let currentOvfBool = c.temp()
  c.emitLine "  " & c.str(currentOvfBool) & " = icmp ne i8 " & c.str(currentOvf) & ", 0"
  let combinedOvf = c.temp()
  c.emitLine "  " & c.str(combinedOvf) & " = or i1 " & c.str(currentOvfBool) & ", " & c.str(ovfFlag)
  let newOvfByte = c.temp()
  c.emitLine "  " & c.str(newOvfByte) & " = zext i1 " & c.str(combinedOvf) & " to i8"
  c.emitLine "  store i8 " & c.str(newOvfByte) & ", ptr @NIFC_OVF_"

  # Declare the intrinsic if not already done
  let declStr = "declare { " & typ & ", i1 } @" & intrinsic & "(" & typ & ", " & typ & ")"
  if intrinsic notin c.declaredExterns:
    c.declaredExterns.incl intrinsic
    c.addTo(c.externs, declStr & "\n")

proc genEmitStmtLLVM(c: var LLVMCode; n: var Cursor) =
  ## Emit statements: pass through as LLVM IR comments (can't emit raw C in LLVM)
  inc n
  var comment = "; emit: "
  while n.kind != ParRi:
    if n.kind == StringLit:
      comment.add pool.strings[n.litId]
      inc n
    else:
      skip n
  inc n # ParRi
  c.emitLine comment

proc genStmtLLVM(c: var LLVMCode; n: var Cursor) =
  case n.stmtKind
  of NoStmt:
    if n.kind == DotToken:
      inc n
    else:
      error c.m, "expected statement but got: ", n
  of StmtsS:
    inc n
    while n.kind != ParRi:
      genStmtLLVM(c, n)
    inc n
  of ScopeS:
    genScopeLLVM c, n
  of CallS:
    # Call as statement (discard result)
    var saved = n
    inc saved
    let calleeType = getType(c.m, saved)
    var retType = "void"
    if calleeType.typeKind == ProctypeT or calleeType.symKind == ProcY:
      var ct = calleeType
      if ct.typeKind == ProctypeT or ct.symKind == ProcY:
        inc ct
        skip ct
      if ct.typeKind == ParamsT:
        var params = ct
        skip params
        retType = genTypeLLVMReadOnly(c, params)
    var callResult = LLValue(); genCallWithType(c, n, retType, callResult)
  of VarS:
    genLocalVarDeclLLVM c, n
  of GvarS:
    genGlobalVarDeclLLVM c, n, IsGlobal
  of TvarS:
    genGlobalVarDeclLLVM c, n, IsThreadlocal
  of ConstS:
    genGlobalVarDeclLLVM c, n, IsConst
  of MflagS:
    genMflagDeclLLVM c, n
  of VflagS:
    genVflagDeclLLVM c, n
  of EmitS:
    genEmitStmtLLVM c, n
  of AsgnS:
    let asgnInfo = n.info
    inc n
    var lval = LLValue(); genLvalueLLVM(c, n, lval)
    var rval = LLValue(); genExprLLVM(c, n, rval)
    c.emitLineDbg "  store " & c.str(rval.typ) & " " & c.str(rval.name) & ", ptr " & c.str(lval.name), asgnInfo
    skipParRi n
  of StoreS:
    genStoreLLVM c, n
  of IfS:
    genIfLLVM c, n
  of IteS, ItecS:
    genIteLLVM c, n
  of WhileS:
    genWhileLLVM c, n
  of LoopS:
    genLoopLLVM c, n
  of BreakS:
    inc n
    if c.currentProc.breakStack.len > 0:
      let target = c.currentProc.breakStack[^1]
      c.emitLine "  br label %" & c.str(target)
    else:
      c.emitLine "  unreachable ; break outside loop"
    c.currentProc.needsTerminator = true
    skipParRi n
  of JtrueS:
    genJtrueLLVM c, n
  of CaseS:
    genSwitchLLVM c, n
  of LabS:
    genLabelLLVM c, n
  of JmpS:
    genGotoLLVM c, n
  of RetS:
    let retInfo = n.info
    inc n
    if n.kind != DotToken:
      var val = LLValue(); genExprLLVM(c, n, val)
      let retTK = c.currentProc.retTypeCursor.typeKind
      if retTK in {IT, UT, CT, BoolT} and val.typ != c.currentProc.retType:
        # Truncate integer to match return type (e.g. i64 literal to i32 return)
        let trunced = c.temp()
        c.emitLine "  " & c.str(trunced) & " = trunc " & c.str(val.typ) & " " & c.str(val.name) & " to " & c.str(c.currentProc.retType)
        c.emitLineDbg "  ret " & c.str(c.currentProc.retType) & " " & c.str(trunced), retInfo
      else:
        c.emitLineDbg "  ret " & c.str(val.typ) & " " & c.str(val.name), retInfo
    else:
      inc n
      c.emitLineDbg "  ret void", retInfo
    c.currentProc.needsTerminator = true
    skipParRi n
  of DiscardS:
    inc n
    var discardVal = LLValue(); genExprLLVM(c, n, discardVal) # evaluate for side effects, discard result
    skipParRi n
  of TryS:
    # Exception handling - simplified version using LLVM's landingpad
    # For now, just generate the try body and skip catch/finally
    inc n
    genStmtLLVM c, n
    # Skip catch and finally sections
    if n.kind != DotToken: skip n else: inc n
    if n.kind != DotToken: skip n else: inc n
    skipParRi n
  of RaiseS:
    inc n
    if n.kind != DotToken:
      var raiseVal = LLValue(); genExprLLVM(c, n, raiseVal)
    else:
      inc n
    # For now, trap on raise
    c.emitLine "  call void @llvm.trap()"
    c.emitLine "  unreachable"
    c.currentProc.needsTerminator = true
    skipParRi n
    # Declare llvm.trap if needed
    if "llvm.trap" notin c.declaredExterns:
      c.declaredExterns.incl "llvm.trap"
      c.addTo(c.externs, "declare void @llvm.trap() noreturn nounwind\n")
  of OnErrS:
    var onErrAction = n
    inc onErrAction
    var saved = n
    inc saved
    let calleeType = getType(c.m, saved)
    var retType = "void"
    if calleeType.typeKind == ProctypeT or calleeType.symKind == ProcY:
      var ct = calleeType
      if ct.typeKind == ProctypeT or ct.symKind == ProcY:
        inc ct
        skip ct
      if ct.typeKind == ParamsT:
        var params = ct
        skip params
        retType = genTypeLLVMReadOnly(c, params)
    var onErrCallResult = LLValue(); genCallWithType(c, n, retType, onErrCallResult)
    if onErrAction.kind != DotToken:
      genOnErrorLLVM(c, onErrAction)
  of ProcS, TypeS:
    error c.m, "expected statement but got: ", n
  of KeepovfS:
    genKeepOverflowLLVM c, n
