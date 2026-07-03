#
#
#           Leng Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included from llvmcodegen.nim
# Generates LLVM IR for statements.

type
  CaseBranch = object
    ## Used by genSwitchLLVM. Declared at module scope rather than inside
    ## the proc's `n.into:` block so the type isn't re-instantiated each
    ## time the template expands.
    values: seq[string]
    label: string

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
  let onErrInfo = n.info
  let errPtr = llGlobalRef("LENGC_ERR_", c.prim.ptrT)
  let errVal = c.emitLoad(errPtr, c.prim.i8)
  let cond = c.nextTemp()
  let condRes = llReg(cond, c.prim.i1)
  c.setLoc(onErrInfo)
  c.emit LLInstr(kind: llIcmp, result: condRes, icmpPred: "ne",
                 icmpLhs: errVal, icmpRhs: llIntTextC("0", c.prim.i8))
  let thenLabel = c.nextLabel()
  let endLabel = c.nextLabel()
  c.emit LLInstr(kind: llCondBr, condBrCond: condRes,
                 condBrTrue: thenLabel, condBrFalse: endLabel)
  discard c.startBlock(thenLabel)
  genStmtLLVM c, n
  if not c.currentProc.needsTerminator:
    c.setLoc(onErrInfo)
    c.emit LLInstr(kind: llBr, brTarget: endLabel)
  discard c.startBlock(endLabel)

proc genIfLLVM(c: var LLVMCode; n: var Cursor) =
  let ifInfo = n.info
  var endLabel = c.nextLabel()
  var needsEnd = false
  n.loopInto:
    let branchInfo = n.info
    case n.substructureKind
    of ElifU:
      n.into:
        var cond = LLValue(); genCondLLVM(c, n, cond)
        let thenLabel = c.nextLabel()
        let elseLabel = c.nextLabel()
        c.setLoc(branchInfo)
        c.emit LLInstr(kind: llCondBr, condBrCond: cond,
                       condBrTrue: thenLabel, condBrFalse: elseLabel)
        discard c.startBlock(thenLabel)
        genStmtLLVM c, n
        if not c.currentProc.needsTerminator:
          c.setLoc(branchInfo)
          c.emit LLInstr(kind: llBr, brTarget: endLabel)
        discard c.startBlock(elseLabel)
        needsEnd = true
        while n.hasMore: skip n
    of ElseU:
      n.into:
        genStmtLLVM c, n
        if not c.currentProc.needsTerminator:
          c.setLoc(branchInfo)
          c.emit LLInstr(kind: llBr, brTarget: endLabel)
        needsEnd = true
        while n.hasMore: skip n
    else:
      error c.m, "`if` expects `elif` or `else` but got: ", n

  if needsEnd:
    if not c.currentProc.needsTerminator:
      c.setLoc(ifInfo)
      c.emit LLInstr(kind: llBr, brTarget: endLabel)
    discard c.startBlock(endLabel)

proc genIteLLVM(c: var LLVMCode; n: var Cursor) =
  let iteInfo = n.info
  n.into:

    let (vflag, isLast) = getVirtualGuardLLVM(c, n)
    if vflag != SymId(0):
      skip n
      genStmtLLVM c, n
      if isLast:
        let labelName = mangleToC(c.m.pool.syms[vflag])
        c.setLoc(iteInfo)
        c.emit LLInstr(kind: llBr, brTarget: labelName)
        discard c.startBlock(labelName)
      skip n
    else:
      var cond = LLValue(); genCondLLVM(c, n, cond)
      let thenLabel = c.nextLabel()
      let elseLabel = c.nextLabel()
      let endLabel = c.nextLabel()

      if n.kind != DotToken:
        c.setLoc(iteInfo)
        c.emit LLInstr(kind: llCondBr, condBrCond: cond,
                       condBrTrue: thenLabel, condBrFalse: elseLabel)
      else:
        c.setLoc(iteInfo)
        c.emit LLInstr(kind: llCondBr, condBrCond: cond,
                       condBrTrue: thenLabel, condBrFalse: endLabel)

      discard c.startBlock(thenLabel)
      genStmtLLVM c, n

      if not c.currentProc.needsTerminator:
        c.setLoc(iteInfo)
        c.emit LLInstr(kind: llBr, brTarget: endLabel)

      if n.kind != DotToken:
        discard c.startBlock(elseLabel)
        genStmtLLVM c, n
        if not c.currentProc.needsTerminator:
          c.setLoc(iteInfo)
          c.emit LLInstr(kind: llBr, brTarget: endLabel)
      else:
        inc n

      discard c.startBlock(endLabel)
    while n.hasMore: skip n

proc genWhileLLVM(c: var LLVMCode; n: var Cursor) =
  let whileInfo = n.info
  n.into:
    let condLabel = c.nextLabel()
    let bodyLabel = c.nextLabel()
    let endLabel = c.nextLabel()

    c.setLoc(whileInfo)
    c.emit LLInstr(kind: llBr, brTarget: condLabel)
    discard c.startBlock(condLabel)

    var cond = LLValue(); genCondLLVM(c, n, cond)
    c.setLoc(whileInfo)
    c.emit LLInstr(kind: llCondBr, condBrCond: cond,
                   condBrTrue: bodyLabel, condBrFalse: endLabel)

    discard c.startBlock(bodyLabel)
    c.currentProc.breakStack.add endLabel
    genStmtLLVM c, n
    discard c.currentProc.breakStack.pop()

    if not c.currentProc.needsTerminator:
      c.setLoc(whileInfo)
      c.emit LLInstr(kind: llBr, brTarget: condLabel)

    discard c.startBlock(endLabel)
    while n.hasMore: skip n

proc genLoopLLVM(c: var LLVMCode; n: var Cursor) =
  let loopInfo = n.info
  n.into:
    let headerLabel = c.nextLabel()
    let bodyLabel = c.nextLabel()
    let endLabel = c.nextLabel()

    c.setLoc(loopInfo)
    c.emit LLInstr(kind: llBr, brTarget: headerLabel)
    discard c.startBlock(headerLabel)

    genStmtLLVM c, n
    var cond = LLValue(); genCondLLVM(c, n, cond)
    c.setLoc(loopInfo)
    c.emit LLInstr(kind: llCondBr, condBrCond: cond,
                   condBrTrue: bodyLabel, condBrFalse: endLabel)

    discard c.startBlock(bodyLabel)
    c.currentProc.breakStack.add endLabel
    genStmtLLVM c, n
    discard c.currentProc.breakStack.pop()

    if not c.currentProc.needsTerminator:
      c.setLoc(loopInfo)
      c.emit LLInstr(kind: llBr, brTarget: headerLabel)

    discard c.startBlock(endLabel)
    while n.hasMore: skip n

proc genSwitchLLVM(c: var LLVMCode; n: var Cursor) =
  n.into:
    var switchVal = LLValue(); genExprLLVM(c, n, switchVal)
    let endLabel = c.nextLabel()
    var defaultLabel = endLabel

    var branches: seq[CaseBranch] = @[]
    var defaultBranch = ""
    var savedPos = n

    while n.hasMore:
      case n.substructureKind
      of OfU:
        n.into:
          var branch = CaseBranch(label: c.nextLabel())
          if n.substructureKind == RangesU:
            n.into:
              while n.hasMore:
                if n.substructureKind == RangeU:
                  n.into:
                    var lo = LLValue(); genExprLLVM(c, n, lo)
                    var hi = LLValue(); genExprLLVM(c, n, hi)
                    branch.values.add disp(lo)
                    while n.hasMore: skip n
                else:
                  var val = LLValue(); genExprLLVM(c, n, val)
                  branch.values.add disp(val)
              while n.hasMore: skip n
          branches.add branch
          skip n
          while n.hasMore: skip n
      of ElseU:
        n.into:
          defaultLabel = c.nextLabel()
          defaultBranch = defaultLabel
          skip n
          while n.hasMore: skip n
      else:
        error c.m, "`case` expects `of` or `else` but got: ", n

    var cases: seq[(LLValue, string)] = @[]
    for branch in branches:
      for v in branch.values:
        cases.add (llIntTextC(v, switchVal.typ), branch.label)
    c.emit LLInstr(kind: llSwitch, switchValType: serialize(switchVal.typ),
                   switchVal: switchVal, switchDefault: defaultLabel,
                   switchCases: cases)

    n = savedPos
    var branchIdx = 0
    c.currentProc.breakStack.add endLabel
    while n.hasMore:
      let branchInfo = n.info
      case n.substructureKind
      of OfU:
        n.into:
          skip n
          discard c.startBlock(branches[branchIdx].label)
          genStmtLLVM c, n
          if not c.currentProc.needsTerminator:
            c.setLoc(branchInfo)
            c.emit LLInstr(kind: llBr, brTarget: endLabel)
          while n.hasMore: skip n
        inc branchIdx
      of ElseU:
        n.into:
          discard c.startBlock(defaultBranch)
          genStmtLLVM c, n
          if not c.currentProc.needsTerminator:
            c.setLoc(branchInfo)
            c.emit LLInstr(kind: llBr, brTarget: endLabel)
          while n.hasMore: skip n
      else:
        error c.m, "`case` expects `of` or `else` but got: ", n
    discard c.currentProc.breakStack.pop()

    discard c.startBlock(endLabel)

proc genLabelLLVM(c: var LLVMCode; n: var Cursor) =
  let labelInfo = n.info
  n.into:
    if n.kind == SymbolDef:
      let name = mangleToC(c.m.pool.syms[n.symId])
      if not c.currentProc.needsTerminator:
        c.setLoc(labelInfo)
        c.emit LLInstr(kind: llBr, brTarget: name)
      discard c.startBlock(name)
      inc n
    else:
      error c.m, "expected SymbolDef but got: ", n
    while n.hasMore: skip n

proc genGotoLLVM(c: var LLVMCode; n: var Cursor) =
  let gotoInfo = n.info
  n.into:
    if n.kind == Symbol:
      let name = mangleToC(c.m.pool.syms[n.symId])
      c.setLoc(gotoInfo)
      c.emit LLInstr(kind: llBr, brTarget: name)
      c.currentProc.needsTerminator = true
      inc n
    else:
      error c.m, "expected Symbol but got: ", n
    while n.hasMore: skip n

proc genScopeLLVM(c: var LLVMCode; n: var Cursor) =
  n.into:
    c.m.openScope()
    while n.hasMore:
      genStmtLLVM c, n
    c.m.closeScope()

proc genMflagDeclLLVM(c: var LLVMCode; n: var Cursor) =
  n.into:
    if n.kind == SymbolDef:
      let s = n.symId
      c.m.registerLocal(s, createIntegralType(c.m, "(bool)"))
      let name = mangleToC(c.m.pool.syms[s])
      c.emitAlloca(name, c.prim.i8)
      c.emitStore(llIntTextC("0", c.prim.i8), llReg(name, c.prim.ptrT))
      inc n
    else:
      error c.m, "expected SymbolDef but got: ", n
    while n.hasMore: skip n

proc genVflagDeclLLVM(c: var LLVMCode; n: var Cursor) =
  n.into:
    if n.kind == SymbolDef:
      let s = n.symId
      c.m.registerLocal(s, createIntegralType(c.m, "(bool)"))
      c.currentProc.vflags.incl(s)
      inc n
    else:
      error c.m, "expected SymbolDef but got: ", n
    while n.hasMore: skip n

proc genJtrueLLVM(c: var LLVMCode; n: var Cursor) =
  let jtrueInfo = n.info
  n.loopInto:
    if n.kind == Symbol:
      let s = n.symId
      if not c.currentProc.vflags.contains(s):
        error c.m, "virtual flag not declared: ", n
      inc n
      if not n.hasMore:
        let name = mangleToC(c.m.pool.syms[s])
        c.setLoc(jtrueInfo)
        c.emit LLInstr(kind: llBr, brTarget: name)
        c.currentProc.needsTerminator = true
    else:
      error c.m, "expected Symbol but got: ", n

proc genStoreLLVM(c: var LLVMCode; n: var Cursor) =
  let storeInfo = n.info
  n.into:
    var rhs = n
    skip n
    var target = LLValue(); genLvalueLLVM(c, n, target)
    var val = LLValue(); genExprLLVM(c, rhs, val)
    c.setLoc(storeInfo)
    c.emitStore(val, target)
    while n.hasMore: skip n

proc genKeepOverflowLLVM(c: var LLVMCode; n: var Cursor) =
  let ovfInfo = n.info
  var typ: LLType = nil
  var intrinsic = ""
  var bitsStr = $c.bits
  var lhs = LLValue()
  var rhs = LLValue()
  var target = LLValue()
  n.into:
    let op = n.exprKind

    intrinsic = "llvm."
    case op
    of AddC: intrinsic.add "sadd"
    of SubC: intrinsic.add "ssub"
    of MulC: intrinsic.add "smul"
    else:
      while n.hasMore: skip n
      return

    n.into:
      let isUnsigned = n.typeKind == UT
      if isUnsigned:
        intrinsic = intrinsic.replace("sadd", "uadd").replace("ssub",
            "usub").replace("smul", "umul")

      n.into:
        if n.kind == IntLit:
          let bits = intVal(n)
          if bits != -1:
            bitsStr = $bits
          inc n
        while n.hasMore: skip n

      typ = c.llIntBits(parseInt(bitsStr))
      intrinsic.add ".with.overflow."
      serialize(typ, intrinsic)

      genExprLLVM(c, n, lhs)
      genExprLLVM(c, n, rhs)
      while n.hasMore: skip n

    genLvalueLLVM(c, n, target)
    while n.hasMore: skip n

  let aggTyp = LLType(kind: llStruct,
      structFields: @[LLStructField(typ: typ), LLStructField(typ: c.prim.i1)])
  let aggText = "{ " & serialize(typ) & ", i1 }"
  let rs = c.nextTemp()
  let rsRes = llReg(rs, aggTyp)
  c.emit LLInstr(kind: llCall, result: rsRes, callCallee: "@" & intrinsic,
                 callRetType: aggTyp, callArgs: @[lhs, rhs])
  let resultVal = c.nextTemp()
  let rvRes = llReg(resultVal, typ)
  c.emit LLInstr(kind: llExtractValue, result: rvRes, evAggregate: rsRes,
                 evAggType: aggText, evIndex: 0)
  c.emitStore(rvRes, target)
  let ovfFlag = c.nextTemp()
  let ovfRes = llReg(ovfFlag, c.prim.i1)
  c.emit LLInstr(kind: llExtractValue, result: ovfRes, evAggregate: rsRes,
                 evAggType: aggText, evIndex: 1)
  let currentOvf = c.emitLoad(llGlobalRef("LENGC_OVF_", c.prim.ptrT), c.prim.i8)
  let currentOvfBool = c.nextTemp()
  let cobRes = llReg(currentOvfBool, c.prim.i1)
  c.setLoc(ovfInfo)
  c.emit LLInstr(kind: llIcmp, result: cobRes, icmpPred: "ne",
                 icmpLhs: currentOvf, icmpRhs: llIntTextC("0", c.prim.i8))
  let combinedOvf = c.nextTemp()
  let coRes = llReg(combinedOvf, c.prim.i1)
  c.emit LLInstr(kind: llOr, result: coRes, binOp: "or", binLhs: cobRes,
      binRhs: ovfRes)
  let newOvfByte = c.nextTemp()
  let nobRes = llReg(newOvfByte, c.prim.i8)
  c.emit LLInstr(kind: llZext, result: nobRes, castOp: "zext", castSrc: coRes,
                 castDstType: c.prim.i8)
  c.emitStore(nobRes, llGlobalRef("LENGC_OVF_", c.prim.ptrT))

  let s = serialize(typ)
  let declStr = "declare { " & s & ", i1 } @" & intrinsic & "(" &
      s & ", " & s & ")"
  declareExtern(c, declStr, intrinsic)

proc genEmitStmtLLVM(c: var LLVMCode; n: var Cursor) =
  n.into:
    var comment = "; emit: "
    while n.hasMore:
      if n.kind == StrLit:
        comment.add c.m.pool.strings[n.litId]
        inc n
      else:
        skip n
    c.emitRaw comment

proc genStmtLLVM(c: var LLVMCode; n: var Cursor) =
  case n.stmtKind
  of NoStmt:
    if n.kind == DotToken:
      inc n
    else:
      error c.m, "expected statement but got: ", n
  of StmtsS:
    n.loopInto:
      genStmtLLVM(c, n)
      if c.currentProc.needsTerminator:
        while n.hasMore and n.stmtKind != LabS: skip n
  of ScopeS:
    genScopeLLVM c, n
  of CallS:
    var saved = n
    inc saved
    let calleeType = getType(c.m, saved)
    var retType = c.prim.voidT
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
    n.into:
      var lval = LLValue(); genLvalueLLVM(c, n, lval)
      var rval = LLValue(); genExprLLVM(c, n, rval)
      c.setLoc(asgnInfo)
      c.emitStore(rval, lval)
      while n.hasMore: skip n
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
    let breakInfo = n.info
    n.into:
      if c.currentProc.breakStack.len > 0:
        let target = c.currentProc.breakStack[^1]
        c.setLoc(breakInfo)
        c.emit LLInstr(kind: llBr, brTarget: target)
      else:
        c.emit LLInstr(kind: llUnreachable)
      c.currentProc.needsTerminator = true
      while n.hasMore: skip n
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
    n.into:
      if n.kind != DotToken:
        let valueExpr = n
        let valueType = getType(c.m, valueExpr)
        var val = LLValue(); genExprLLVM(c, n, val)
        var coerced = LLValue()
        coerceValueLLVM(c, val, valueType, c.currentProc.retTypeCursor, false, coerced)
        c.setLoc(retInfo)
        c.emit LLInstr(kind: llRet, retVal: coerced)
      else:
        inc n
        c.setLoc(retInfo)
        c.emit LLInstr(kind: llRet, retVal: llNoneVal())
      c.currentProc.needsTerminator = true
      while n.hasMore: skip n
  of DiscardS:
    n.into:
      var discardVal = LLValue(); genExprLLVM(c, n, discardVal)
      while n.hasMore: skip n
  of TryS:
    n.into:
      genStmtLLVM c, n
      if n.kind != DotToken: skip n else: inc n
      if n.kind != DotToken: skip n else: inc n
      while n.hasMore: skip n
  of RaiseS:
    n.into:
      if n.kind != DotToken:
        var raiseVal = LLValue(); genExprLLVM(c, n, raiseVal)
      else:
        inc n
      c.emit LLInstr(kind: llCall, callCallee: "@llvm.trap",
                     callRetType: c.prim.voidT, callArgs: @[])
      c.emit LLInstr(kind: llUnreachable)
      c.currentProc.needsTerminator = true
      declareExtern(c, "declare void @llvm.trap() noreturn nounwind", "llvm.trap")
      while n.hasMore: skip n
  of OnerrS:
    var onErrAction = n
    inc onErrAction
    var saved = n
    inc saved
    let calleeType = getType(c.m, saved)
    var retType = c.prim.voidT
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
