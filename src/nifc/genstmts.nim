#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included from codegen.nim

proc genEmitStmt(c: var GeneratedCode; n: var Cursor) =
  inc n
  while n.kind != ParRi:
    if n.kind == StringLit:
      c.add pool.strings[n.litId]
      inc n
    else:
      genx c, n
  inc n # ParRi
  c.add NewLine

proc genIf(c: var GeneratedCode; n: var Cursor) =
  let oldInToplevel = c.inToplevel
  c.inToplevel = false
  var hasElse = false
  var hasElif = false
  inc n
  let first = n
  while n.kind != ParRi:
    case n.substructureKind
    of ElifU:
      if hasElse:
        error c.m, "no `elif` allowed after `else` but got: ", n
      else:
        if hasElif:
          c.add ElseKeyword
        c.add IfKeyword
        inc n
        c.genCond n
        c.add CurlyLe
        genStmt c, n
        c.add CurlyRi
        skipParRi n
      hasElif = true
    of ElseU:
      hasElse = true
      if not hasElif:
        error c.m, "no `elif` before `else` but got: ", n
      else:
        c.add ElseKeyword
        c.add CurlyLe
        inc n
        genStmt c, n
        c.add CurlyRi
        skipParRi n
    else:
      error c.m, "`if` expects `elif` or `else` but got: ", n
  skipParRi n
  if not hasElif and not hasElse:
    error c.m, "`if` expects `elif` or `else` but got: ", first
  c.inToplevel = oldInToplevel

proc genWhile(c: var GeneratedCode; n: var Cursor) =
  let oldInToplevel = c.inToplevel
  c.inToplevel = false
  inc n
  c.add WhileKeyword
  c.genCond n
  c.add CurlyLe
  c.genStmt n
  c.add CurlyRi
  skipParRi n
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
  inc n

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

  skipParRi n

proc genScope(c: var GeneratedCode; n: var Cursor) =
  c.add CurlyLe
  inc n
  c.m.openScope()
  while n.kind != ParRi:
    c.genStmt n
  skipParRi n
  c.add CurlyRi
  c.m.closeScope()

proc isBranchValue(n: Cursor): bool =
  var n = n
  if n.kind in {IntLit, UIntLit, CharLit, Symbol} or n.exprKind in {TrueC, FalseC}:
    result = true
  elif n.exprKind == SufC:
    inc n
    result = n.kind in {IntLit, UIntLit, CharLit}
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
    inc n
    while n.kind != ParRi:
      c.add CaseKeyword
      if n.substructureKind == RangeU:
        inc n
        genBranchValue c, n
        c.add " ... "
        genBranchValue c, n
        skipParRi n
      else:
        genBranchValue c, n
      c.add ":"
      c.add NewLine
    skipParRi n
  else:
    error c.m, "`ranges` expected but got: ", n

proc genLabel(c: var GeneratedCode; n: var Cursor) =
  inc n
  if n.kind == SymbolDef:
    let name = mangle(pool.syms[n.symId])
    c.add name
    c.add Colon
    c.add Semicolon
    inc n
  else:
    error c.m, "expected SymbolDef but got: ", n
  skipParRi n

proc genGoto(c: var GeneratedCode; n: var Cursor) =
  inc n
  if n.kind == Symbol:
    let name = mangle(pool.syms[n.symId])
    c.add GotoKeyword
    c.add name
    c.add Semicolon
    inc n
  else:
    error c.m, "expected Symbol but got: ", n
  skipParRi n

proc genSwitch(c: var GeneratedCode; n: var Cursor) =
  # (case Expr (of BranchRanges StmtList)* (else StmtList)?) |
  let oldInToplevel = c.inToplevel
  c.inToplevel = false
  c.add SwitchKeyword
  inc n
  let first = n
  c.genCond n
  c.add CurlyLe

  var hasElse = false
  var hasElif = false
  while n.kind != ParRi:
    case n.substructureKind
    of OfU:
      if hasElse:
        error c.m, "no `of` allowed after `else` but got: ", n
      else:
        inc n
        c.genCaseCond n
        c.add CurlyLe
        genStmt c, n
        c.add CurlyRi
        c.add BreakKeyword
        c.add Semicolon
        skipParRi n
      hasElif = true
    of ElseU:
      hasElse = true
      if not hasElif:
        error c.m, "no `of` before `else` but got: ", n
      else:
        c.add DefaultKeyword
        c.add NewLine
        c.add CurlyLe
        inc n
        genStmt c, n
        skipParRi n
        c.add CurlyRi
        c.add BreakKeyword
        c.add Semicolon
    else:
      error c.m, "`case` expects `of` or `else` but got: ", n
  if not hasElif and not hasElse:
    error c.m, "`case` expects `of` or `else` but got: ", first
  c.add CurlyRi
  skipParRi n
  c.inToplevel = oldInToplevel

proc genVar(c: var GeneratedCode; n: var Cursor; vk: VarKind; toExtern = false; useStatic = false) =
  case vk
  of IsLocal:
    genVarDecl c, n, IsLocal, toExtern
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
  inc n # keepovf
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
    prefix = "_Qnifc_"
  of ModC:
    gcc.add "mod_"
    prefix = "_Qnifc_"
  else:
    error c.m, "expected arithmetic operation but got: ", n
  inc n # operation
  if n.typeKind == IT:
    gcc = prefix & "s" & gcc
  elif n.typeKind == UT:
    gcc = prefix & "u" & gcc
  else:
    error c.m, "expected integer type but got: ", n
  inc n # type
  var isLongLong = false
  if n.kind == IntLit:
    let bits = pool.integers[n.intId]
    if bits == 64 or (bits == -1 and c.bits == 64):
      gcc.add "ll"
      isLongLong = true
    inc n
  else:
    error c.m, "expected integer literal but got: ", n
  c.currentProc.needsOverflowFlag = true
  skipParRi n # end of type
  c.add IfKeyword
  c.add ParLe
  gcc.add "_overflow"
  c.add gcc
  c.add ParLe
  genx c, n
  c.add Comma
  genx c, n
  skipParRi n
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
  skipParRi n

proc genStmt(c: var GeneratedCode; n: var Cursor) =
  case n.stmtKind
  of NoStmt:
    if n.kind == DotToken:
      inc n
    else:
      error c.m, "expected statement but got: ", n
  of StmtsS:
    inc n
    while n.kind != ParRi:
      genStmt(c, n)
    inc n # ParRi
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
  of ConstS:
    genVar c, n, IsConst, useStatic = true
  of EmitS:
    genEmitStmt c, n
  of AsgnS:
    genCLineDir(c, info(n))
    inc n
    genLvalue c, n
    c.add AsgnOpr
    genx c, n
    c.add Semicolon
    skipParRi n
  of IfS: genIf c, n
  of WhileS: genWhile c, n
  of BreakS:
    inc n
    c.add BreakKeyword
    c.add Semicolon
    skipParRi n
  of CaseS: genSwitch c, n
  of LabS: genLabel c, n
  of JmpS: genGoto c, n
  of RetS:
    c.add ReturnKeyword
    inc n
    if n.kind != DotToken:
      c.add Space
      c.genx n
    else:
      inc n
    c.add Semicolon
    skipParRi n
  of DiscardS:
    inc n
    c.add DiscardToken
    c.genx n
    c.add Semicolon
    skipParRi n
  of TryS:
    genTryCpp c, n
  of RaiseS:
    c.add ThrowKeyword
    inc n
    if n.kind != DotToken:
      c.add Space
      c.genx n
    else:
      inc n
    c.add Semicolon
    skipParRi n
  of OnErrS:
    var onErrAction = n
    inc onErrAction
    genCallCanRaise c, n
    c.add Semicolon
    if onErrAction.kind != DotToken:
      genOnError(c, onErrAction)
  of ProcS, TypeS, ImpS, InclS:
    error c.m, "expected statement but got: ", n
  of KeepovfS:
    genKeepOverflow c, n
