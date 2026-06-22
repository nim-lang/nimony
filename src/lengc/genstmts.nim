#
#
#           Leng Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included from codegen.nim

proc genEmitStmt(c: var GeneratedCode; n: var Cursor) =
  n.loopInto:
    if n.kind == StrLit:
      c.add c.m.pool.strings[n.litId]
      inc n
    else:
      genx c, n
  c.add NewLine

proc genIf(c: var GeneratedCode; n: var Cursor) =
  let oldInToplevel = c.inToplevel
  c.inToplevel = false
  var hasElse = false
  var hasElif = false
  let first = n.firstSon
  n.loopInto:
    case n.substructureKind
    of ElifU:
      if hasElse:
        error c.m, "no `elif` allowed after `else` but got: ", n
      else:
        if hasElif:
          c.add ElseKeyword
        c.add IfKeyword
        n.into:
          c.genCond n
          c.add CurlyLe
          genStmt c, n
          c.add CurlyRi
          while n.hasMore: skip n
      hasElif = true
    of ElseU:
      hasElse = true
      if not hasElif:
        error c.m, "no `elif` before `else` but got: ", n
      else:
        c.add ElseKeyword
        c.add CurlyLe
        n.into:
          genStmt c, n
          while n.hasMore: skip n
        c.add CurlyRi
    else:
      error c.m, "`if` expects `elif` or `else` but got: ", n
      skip n  # avoid infinite loop on unexpected input
  if not hasElif and not hasElse:
    error c.m, "`if` expects `elif` or `else` but got: ", first
  c.inToplevel = oldInToplevel

proc getVirtualGuard(c: var GeneratedCode; n: Cursor): (SymId, bool) =
  result = (SymId(0), false)
  var n = n
  # Leng requirement: The last usage of a virtual flag
  # must be annotated with (lab).
  if n.exprKind == NotC:
    inc n
    var isLast = false
    if n.stmtKind == LabS:
      isLast = true
      inc n
    elif n.kind == Symbol:
      if c.currentProc.vflags.contains(n.symId):
        result = (n.symId, isLast)

proc genIte(c: var GeneratedCode; n: var Cursor) =
  let oldInToplevel = c.inToplevel
  c.inToplevel = false
  n.into:
    let (vflag, isLast) = getVirtualGuard(c, n)
    if vflag != SymId(0):
      #[
         vflag x = false
         ...
         jtrue x
         ...
         if not x:
           actions

         -->
           actions
      ]#
      skip n
      c.genStmt n # then-part is always taken
      # emit the label:
      if isLast:
        c.add mangleToC(c.m.pool.syms[vflag])
        c.add Colon
        c.add Semicolon
      skip n      # else-part is always ignored
    else:
      c.add IfKeyword
      c.add Space
      c.genCond n
      c.add Space
      c.add CurlyLe
      c.genStmt n
      c.add CurlyRi
      if n.kind != DotToken:
        c.add ElseKeyword
        c.add CurlyLe
        genStmt c, n
        c.add CurlyRi
      else:
        inc n
    while n.hasMore: skip n
  c.inToplevel = oldInToplevel
  c.add Semicolon

proc genLoop(c: var GeneratedCode; n: var Cursor) =
  let oldInToplevel = c.inToplevel
  c.inToplevel = false
  n.into:
    c.add WhileKeyword
    c.add Space
    c.add ParLe
    c.add "NIM_TRUE"
    c.add ParRi
    c.add Space
    c.add CurlyLe
    c.genStmt n # pre condition statements
    c.add IfKeyword
    c.add Space
    c.add ParLe
    c.add "!"
    c.add ParLe
    c.genx n # loop condition
    c.add ParRi
    c.add ParRi
    c.add BreakKeyword
    c.add Semicolon
    c.genStmt n # loop body
    c.add CurlyRi
    while n.hasMore: skip n
  c.inToplevel = oldInToplevel

proc genWhile(c: var GeneratedCode; n: var Cursor) =
  let oldInToplevel = c.inToplevel
  c.inToplevel = false
  n.into:
    c.add WhileKeyword
    c.genCond n
    c.add CurlyLe
    c.genStmt n
    c.add CurlyRi
    while n.hasMore: skip n
  c.inToplevel = oldInToplevel

proc genTryCpp(c: var GeneratedCode; n: var Cursor) =
  #[ The generated code is equivalent to:
    bool needsFinalRethrow = false;
    try {
        // original code
    } catch (...) {
        needsFinalRethrow = true;
        // catch block code
    }

    // finally section
    if (needsFinalRethrow) { throw; }

    Possible cases:
    1. No exception in original code:
       - needsFinalRethrow stays false
       - finally runs normally
       - needsFinalRethrow check fails, no rethrow

    2. Exception in original code, no exception in finally:
       - needsFinalRethrow set to true
       - finally runs normally
       - needsFinalRethrow succeeds, original exception rethrown

    3. No exception in original code, exception in finally:
       - needsFinalRethrow stays false
       - finally throws directly
       - never reaches rethrow check

    4. Exception in original code, exception in finally:
       - needsFinalRethrow set to true
       - finally throws directly
       - never reaches rethrow check
  ]#
  n.into:

    # Add needsFinalRethrow flag
    let varName = "needsFinalRethrow" & $c.currentProc.nextTemp
    inc c.currentProc.nextTemp
    c.add "bool " & varName & " = false;"
    c.add NewLine

    # Try block
    c.add TryKeyword
    c.add CurlyLe
    c.genStmt n
    c.add CurlyRi

    # Catch block for original exception
    c.add CatchKeyword
    c.add "..."
    c.add ParRi
    c.add Space
    c.add CurlyLe
    let beforeAsgn = c.code.len
    var sections = 0
    c.add varName & " = true;"
    c.add NewLine
    if n.kind != DotToken:
      c.genStmt n
      inc sections
    else:
      inc n
    c.add CurlyRi

    # Finally section
    if n.kind != DotToken:
      c.genStmt n
      inc sections
    else:
      inc n
    if sections == 0:
      c.code.shrink beforeAsgn
      c.add CurlyRi

    # Rethrow original exception if needed
    c.add "if (" & varName & ") { throw; }"
    c.add NewLine

    while n.hasMore: skip n

proc genScope(c: var GeneratedCode; n: var Cursor) =
  c.add CurlyLe
  c.m.openScope()
  n.loopInto:
    c.genStmt n
  c.add CurlyRi
  c.m.closeScope()

proc isBranchValue(n: Cursor): bool =
  var n = n
  if n.kind in {IntLit, UIntLit, CharLit, Symbol} or n.exprKind in {TrueC, FalseC}:
    result = true
  elif n.exprKind == SufC:
    inc n  # peek; n is a local copy
    result = n.kind in {IntLit, UIntLit, CharLit}
  elif n.exprKind in {ConvC, CastC}:
    # `(conv T <lit>)` → `((T)<lit>)` is a constant expression in C and is
    # valid inside a `case` label.
    inc n  # peek; n is a local copy
    skip n # type
    result = isBranchValue(n)
  else:
    result = false

proc genBranchValue(c: var GeneratedCode; n: var Cursor) =
  if isBranchValue(n):
    c.genx n
  else:
    error c.m, "expected valid `of` value but got: ", n

proc genCaseCond(c: var GeneratedCode; n: var Cursor) =
  # BranchValue ::= Number | CharLiteral | Symbol | (true) | (false)
  # BranchRange ::= BranchValue | (range BranchValue BranchValue)
  # BranchRanges ::= (ranges BranchRange+)
  if n.substructureKind == RangesU:
    n.loopInto:
      c.add CaseKeyword
      if n.substructureKind == RangeU:
        n.into:
          genBranchValue c, n
          c.add " ... "
          genBranchValue c, n
          while n.hasMore: skip n
      else:
        genBranchValue c, n
      c.add ":"
      c.add NewLine
  else:
    error c.m, "`ranges` expected but got: ", n

proc genLabel(c: var GeneratedCode; n: var Cursor) =
  n.into:
    if n.kind == SymbolDef:
      let name = mangleToC(c.m.pool.syms[n.symId])
      c.add name
      c.add Colon
      c.add Semicolon
      inc n
    else:
      error c.m, "expected SymbolDef but got: ", n
    while n.hasMore: skip n

proc genGoto(c: var GeneratedCode; n: var Cursor) =
  n.into:
    if n.kind == Symbol:
      let name = mangleToC(c.m.pool.syms[n.symId])
      c.add GotoKeyword
      c.add name
      c.add Semicolon
      inc n
    else:
      error c.m, "expected Symbol but got: ", n
    while n.hasMore: skip n

proc genSwitch(c: var GeneratedCode; n: var Cursor) =
  # (case Expr (of BranchRanges StmtList)* (else StmtList)?) |
  let oldInToplevel = c.inToplevel
  c.inToplevel = false
  c.add SwitchKeyword
  let first = n.firstSon
  n.into:
    c.genCond n
    c.add CurlyLe

    var hasElse = false
    var hasElif = false
    while n.hasMore:
      case n.substructureKind
      of OfU:
        if hasElse:
          error c.m, "no `of` allowed after `else` but got: ", n
        else:
          n.into:
            c.genCaseCond n
            c.add CurlyLe
            genStmt c, n
            c.add CurlyRi
            c.add BreakKeyword
            c.add Semicolon
            while n.hasMore: skip n
        hasElif = true
      of ElseU:
        hasElse = true
        if not hasElif:
          error c.m, "no `of` before `else` but got: ", n
        else:
          c.add DefaultKeyword
          c.add NewLine
          c.add CurlyLe
          n.into:
            genStmt c, n
            while n.hasMore: skip n
          c.add CurlyRi
          c.add BreakKeyword
          c.add Semicolon
      else:
        error c.m, "`case` expects `of` or `else` but got: ", n
        skip n  # avoid infinite loop on unexpected input
    if not hasElif and not hasElse:
      error c.m, "`case` expects `of` or `else` but got: ", first
    c.add CurlyRi
  c.inToplevel = oldInToplevel

proc genMflagDecl(c: var GeneratedCode; n: var Cursor) =
  genCLineDir(c, n.info)
  n.into:
    if n.kind == SymbolDef:
      let s = n.symId
      c.m.registerLocal(s, createIntegralType(c.m, "(bool)"))
      c.add "NB8"
      c.add Space
      c.add mangleToC(c.m.pool.syms[s])
      c.add Semicolon
      inc n
    else:
      error c.m, "expected SymbolDef but got: ", n
    while n.hasMore: skip n

proc genVflagDecl(c: var GeneratedCode; n: var Cursor) =
  genCLineDir(c, n.info)
  n.into:
    if n.kind == SymbolDef:
      let s = n.symId
      c.m.registerLocal(s, createIntegralType(c.m, "(bool)"))
      c.currentProc.vflags.incl(s)
      inc n
    else:
      error c.m, "expected SymbolDef but got: ", n
    while n.hasMore: skip n

proc genJtrue(c: var GeneratedCode; n: var Cursor) =
  n.into:
    while n.hasMore:
      if n.kind == Symbol:
        let s = n.symId
        if not c.currentProc.vflags.contains(s):
          error c.m, "virtual flag not declared: ", n
        inc n
        if not n.hasMore:
          # last symbol becomes a target goto:
          c.add GotoKeyword
          c.add mangleToC(c.m.pool.syms[s])
          c.add Semicolon
      else:
        error c.m, "expected Symbol but got: ", n
        inc n  # avoid infinite loop

proc genVar(c: var GeneratedCode; n: var Cursor; vk: VarKind; toExtern = false; useStatic = false) =
  case vk
  of IsLocal:
    genVarDecl c, n, IsLocal, toExtern
  of IsMflag:
    genMflagDecl c, n
  of IsGlobal:
    moveToDataSection:
      genVarDecl c, n, IsGlobal, toExtern
  of IsThreadlocal:
    moveToDataSection:
      genVarDecl c, n, IsThreadlocal, toExtern
  of IsConst:
    moveToDataSection:
      genVarDecl c, n, IsConst, toExtern, useStatic

proc genKeepOverflow(c: var GeneratedCode; n: var Cursor) =
  n.into:  # (keepovf …)
    let op = n.exprKind
    var gcc = ""
    var prefix = "__builtin_"
    case op
    of AddC:
      gcc.add "add"
    of SubC:
      gcc.add "sub"
    of MulC:
      gcc.add "mul"
    of DivC:
      gcc.add "div_"
      prefix = "_Qlengc_"
    of ModC:
      gcc.add "mod_"
      prefix = "_Qlengc_"
    else:
      error c.m, "expected arithmetic operation but got: ", n
    var isLongLong = false
    n.into:  # (add | sub | mul | … <type> lhs rhs)
      if n.typeKind == IT:
        gcc = prefix & "s" & gcc
      elif n.typeKind == UT:
        gcc = prefix & "u" & gcc
      else:
        error c.m, "expected integer type but got: ", n
      n.into:  # (i bits) | (u bits)
        if n.kind == IntLit:
          let bits = intVal(n)
          if bits == 64 or (bits == -1 and c.bits == 64):
            gcc.add "ll"
            isLongLong = true
          inc n
        else:
          error c.m, "expected integer literal but got: ", n
        while n.hasMore: skip n
      c.currentProc.needsOverflowFlag = true
      c.add IfKeyword
      c.add ParLe
      gcc.add "_overflow"
      c.add gcc
      c.add ParLe
      genx c, n
      c.add Comma
      genx c, n
      while n.hasMore: skip n
    c.add Comma
    if isLongLong:
      c.add "(long long int*)"
      c.add ParLe
    c.add Amp
    genLvalue c, n
    if isLongLong:
      c.add ParRi
    c.add ParRi
    c.add ParRi # end of condition
    c.add CurlyLe
    c.add OvfToken
    c.add AsgnOpr
    c.add OvfToken
    c.add " || "
    c.add "NIM_TRUE"
    c.add Semicolon
    c.add CurlyRi
    while n.hasMore: skip n

proc genStore(c: var GeneratedCode; n: var Cursor) =
  n.into:
    var rhs = n
    skip n
    genLvalue c, n
    c.add AsgnOpr
    genx c, rhs
    c.add Semicolon
    while n.hasMore: skip n

proc genStmt(c: var GeneratedCode; n: var Cursor) =
  case n.stmtKind
  of NoStmt:
    if n.kind == DotToken:
      inc n
    else:
      error c.m, "expected statement but got: ", n
  of StmtsS:
    n.loopInto:
      genStmt(c, n)
  of ScopeS:
    let oldInToplevel = c.inToplevel
    c.inToplevel = false
    genScope c, n
    c.inToplevel = oldInToplevel
  of CallS:
    genCall c, n
    c.add Semicolon
  of VarS:
    genVar c, n, IsLocal
  of GvarS:
    genVar c, n, IsGlobal
  of TvarS:
    genVar c, n, IsThreadlocal
  of MflagS:
    genMflagDecl c, n
  of VflagS:
    genVflagDecl c, n
  of ConstS:
    genVar c, n, IsConst, useStatic = true
  of EmitS:
    genEmitStmt c, n
  of AsgnS:
    genCLineDir(c, info(n))
    n.into:
      genLvalue c, n
      c.add AsgnOpr
      genx c, n
      c.add Semicolon
      while n.hasMore: skip n
  of StoreS: genStore c, n
  of IfS: genIf c, n
  of IteS, ItecS: genIte c, n
  of WhileS: genWhile c, n
  of LoopS: genLoop c, n
  of BreakS:
    n.into:
      c.add BreakKeyword
      c.add Semicolon
      while n.hasMore: skip n
  of JtrueS: genJtrue c, n
  of CaseS: genSwitch c, n
  of LabS: genLabel c, n
  of JmpS: genGoto c, n
  of RetS:
    c.add ReturnKeyword
    n.into:
      if n.kind != DotToken:
        c.add Space
        c.genx n
      else:
        inc n
      c.add Semicolon
      while n.hasMore: skip n
  of DiscardS:
    n.into:
      c.add DiscardToken
      c.genx n
      c.add Semicolon
      while n.hasMore: skip n
  of TryS:
    genTryCpp c, n
  of RaiseS:
    c.add ThrowKeyword
    n.into:
      if n.kind != DotToken:
        c.add Space
        c.genx n
      else:
        inc n
      c.add Semicolon
      while n.hasMore: skip n
  of OnerrS:
    var onErrAction = n
    inc onErrAction
    genCallCanRaise c, n
    c.add Semicolon
    if onErrAction.kind != DotToken:
      genOnError(c, onErrAction)
  of ProcS, TypeS:
    error c.m, "expected statement but got: ", n
  of KeepovfS:
    genKeepOverflow c, n
