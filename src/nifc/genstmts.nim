#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# included from codegen.nim

proc genEmitStmt(c: var GeneratedCode; n: var Cursor) =
  for ch in sons(t, n):
    if t[ch].kind == StrLit:
      c.add c.m.lits.strings[t[ch].litId]
    else:
      genx c, t, ch
  c.add NewLine

proc genStmt(c: var GeneratedCode; n: var Cursor)

proc genIf(c: var GeneratedCode; t: Tree; ifStmt: NodePos) =
  var hasElse = false
  var hasElif = false
  for n in sons(t, ifStmt):
    case t[n].kind
    of ElifC:
      if hasElse:
        error c.m, "no `elif` allowed after `else` but got: ", n
      else:
        if hasElif:
          c.add ElseKeyword
        c.add IfKeyword
        let (cond, action) = sons2(t, n)
        c.genx t, cond
        c.add ParRi
        c.add CurlyLe
        genStmt c, t, action
        c.add CurlyRi
      hasElif = true
    of ElseC:
      hasElse = true
      if not hasElif:
        error c.m, "no `elif` before `else` but got: ", n
      else:
        c.add ElseKeyword
        c.add CurlyLe
        genStmt c, n.firstSon
        c.add CurlyRi
    else:
      error c.m, "`if` expects `elif` or `else` but got: ", n
  if not hasElif and not hasElse:
    error c.m, "`if` expects `elif` or `else` but got: ", t, ifStmt

proc genWhile(c: var GeneratedCode; n: var Cursor) =
  let (cond, body) = sons2(t, n)
  c.add WhileKeyword
  c.add ParLe
  c.genx t, cond
  c.add ParRi
  c.add CurlyLe
  c.genStmt t, body
  c.add CurlyRi

proc genTryCpp(c: var GeneratedCode; n: var Cursor) =
  let (actions, onerr, final) = sons3(t, n)

  c.add TryKeyword
  c.add CurlyLe
  c.genStmt(t, actions)
  c.add CurlyRi

  c.add CatchKeyword
  c.add "..."
  c.add ParRi
  c.add Space
  c.add CurlyLe
  if t[onerr].kind != Empty:
    c.genStmt(t, onerr)
  c.add CurlyRi

  if t[final].kind != Empty:
    c.add CurlyLe
    c.genStmt(t, final)
    c.add CurlyRi

proc genScope(c: var GeneratedCode; n: var Cursor) =
  c.add CurlyLe
  for ch in sons(t, n):
    c.genStmt t, ch
  c.add CurlyRi

proc genBranchValue(c: var GeneratedCode; n: var Cursor) =
  if t[n].kind in {IntLit, UIntLit, CharLit, Sym, TrueC, FalseC}:
    c.genx t, n
  else:
    error c.m, "expected valid `of` value but got: ", n

proc genCaseCond(c: var GeneratedCode; n: var Cursor) =
  # BranchValue ::= Number | CharLiteral | Symbol | (true) | (false)
  # BranchRange ::= BranchValue | (range BranchValue BranchValue)
  # BranchRanges ::= (ranges BranchRange+)
  if t[n].kind == RangesC:
    for ch in sons(t, n):
      c.add CaseKeyword
      if t[ch].kind == RangeC:
        let (a, b) = sons2(t, ch)
        genBranchValue c, t, a
        c.add " ... "
        genBranchValue c, t, b
      else:
        genBranchValue c, t, ch
      c.add ":"
      c.add NewLine
  else:
    error c.m, "no `ranges` expected but got: ", n

proc genLabel(c: var GeneratedCode; n: var Cursor) =
  let dname = n.firstSon
  if t[dname].kind == SymDef:
    let lit = t[dname].litId
    let name = mangle(c.m.lits.strings[lit])
    c.add name
    c.add Colon
    c.add Semicolon
  else:
    error c.m, "expected SymbolDef but got: ", n

proc genGoto(c: var GeneratedCode; n: var Cursor) =
  let dname = n.firstSon
  if t[dname].kind == Sym:
    let lit = t[dname].litId
    let name = mangle(c.m.lits.strings[lit])
    c.add GotoKeyword
    c.add name
    c.add Semicolon
  else:
    error c.m, "expected Symbol but got: ", n

proc genSwitch(c: var GeneratedCode; t: Tree; caseStmt: NodePos) =
  # (case Expr (of BranchRanges StmtList)* (else StmtList)?) |
  c.add SwitchKeyword
  c.add ParLe
  let selector = caseStmt.firstSon
  c.genx t, selector
  c.add ParRi
  c.add CurlyLe

  var hasElse = false
  var hasElif = false
  for n in sonsFromX(t, caseStmt):
    case t[n].kind
    of OfC:
      if hasElse:
        error c.m, "no `of` allowed after `else` but got: ", n
      else:
        let (cond, action) = sons2(t, n)
        c.genCaseCond t, cond
        c.add CurlyLe
        genStmt c, t, action
        c.add CurlyRi
        c.add BreakKeyword
        c.add Semicolon
      hasElif = true
    of ElseC:
      hasElse = true
      if not hasElif:
        error c.m, "no `of` before `else` but got: ", n
      else:
        c.add DefaultKeyword
        c.add NewLine
        c.add CurlyLe
        genStmt c, n.firstSon
        c.add CurlyRi
        c.add BreakKeyword
        c.add Semicolon
    else:
      error c.m, "`case` expects `of` or `else` but got: ", n
  if not hasElif and not hasElse:
    error c.m, "`case` expects `of` or `else` but got: ", t, caseStmt
  c.add CurlyRi

proc genVar(c: var GeneratedCode; n: var Cursor; toExtern = false) =
  case t[n].kind
  of VarC:
    genVarDecl c, n, IsLocal, toExtern
  of GvarC:
    moveToDataSection:
      genVarDecl c, n, IsGlobal, toExtern
  of TvarC:
    moveToDataSection:
      genVarDecl c, n, IsThreadlocal, toExtern
  of ConstC:
    moveToDataSection:
      genVarDecl c, n, IsConst, toExtern
  else:
    quit "unreachable"

proc genOnError(c: var GeneratedCode; n: var Cursor) =
  c.add IfKeyword
  c.add ErrToken
  c.add ParRi
  c.add Space
  c.add CurlyLe
  c.genStmt(t, n)
  c.add CurlyRi

proc genStmt(c: var GeneratedCode; n: var Cursor) =
  case t[n].kind
  of Empty:
    discard
  of StmtsC:
    for ch in sons(t, n):
      genStmt(c, t, ch)
  of ScopeC:
    genScope c, n
  of CallC:
    genCall c, n
    c.add Semicolon
  of VarC, GvarC, TvarC, ConstC:
    genVar c, n
    let value = ithSon(t, n, 3)
    if t[value].kind == OnErrC and
        t[value.firstSon].kind != Empty:
      genOnError(c, t, value.firstSon)
  of EmitC:
    genEmitStmt c, n
  of AsgnC:
    genCLineDir(c, t, info(t, n))
    let (dest, src) = sons2(t, n)
    genLvalue c, t, dest
    c.add AsgnOpr
    genx c, t, src
    c.add Semicolon
  of IfC: genIf c, n
  of WhileC: genWhile c, n
  of BreakC:
    c.add BreakKeyword
    c.add Semicolon
  of CaseC: genSwitch c, n
  of LabC: genLabel c, n
  of JmpC: genGoto c, n
  of RetC:
    c.add ReturnKeyword
    if t[n.firstSon].kind != Empty:
      c.add Space
      c.genx t, n.firstSon
    c.add Semicolon
  of DiscardC:
    c.add DiscardToken
    c.genx t, n.firstSon
    c.add Semicolon
  of TryC:
    genTryCpp(c, n)
  of RaiseC:
    c.add ThrowKeyword
    if t[n.firstSon].kind != Empty:
      c.add Space
      c.genx t, n.firstSon
    c.add Semicolon
  of OnErrC:
    genCallCanRaise c, n
    c.add Semicolon
    if t[n.firstSon].kind != Empty:
      genOnError(c, n.firstSon)
  else:
    error c.m, "expected statement but got: ", n
