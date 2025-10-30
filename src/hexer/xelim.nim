#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Eliminate eXpressions in complex situations. In other words turns
## `let x = if cond: 3 else: 4` into
## `let tmp; if cond: tmp = 3 else: temp = 4; let x = tmp`

import std / [assertions]
include ".." / lib / nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof]

type
  Goal* = enum
    ElimExprs   # normal mode: eliminate expressions
    TowardsNjvl # goal mode: prepare for transformation into njvl

proc isComplex(n: Cursor; goal: Goal): bool =
  var nested = 0
  var n = n
  while true:
    case n.kind
    of IntLit, UIntLit, FloatLit, StringLit, CharLit, UnknownToken, EofToken, Ident, Symbol, SymbolDef, DotToken:
      inc n
    of ParLe:
      if n.stmtKind in {IfS, CaseS, WhileS, AsgnS, LetS, VarS, CursorS, StmtsS, ResultS, GletS, TletS, GvarS, TvarS}:
        return true
      elif n.exprKind == ExprX:
        inc n
        let inner = n
        skip n
        if n.kind == ParRi:
          # ExprX with exactly one son might be harmless:
          if isComplex(inner, goal):
            return true
        else:
          # More than one son is always complex:
          return true
        inc nested
      elif goal == TowardsNjvl and n.exprKind in CallKinds:
        return true
      else:
        inc n
        inc nested
    of ParRi:
      inc n
      dec nested
    if nested == 0: break
  return false

type
  Mode = enum
    IsEmpty, IsAppend, IsIgnored, IsCfvar
  Target = object
    m: Mode
    t: TokenBuf
  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    goal: Goal

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target)
proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor)

proc declareTemp(c: var Context; dest: var TokenBuf; n: Cursor): SymId =
  let info = n.info
  let typ = getType(c.typeCache, n)
  let s = "`x." & $c.counter & "." & c.thisModuleSuffix
  inc c.counter
  result = pool.syms.getOrIncl(s)
  copyIntoKind dest, VarS, info:
    dest.addSymDef result, info
    dest.addDotToken() # export, pragmas
    dest.addDotToken()
    copyTree dest, typ # type
    dest.addDotToken() # value

proc declareTempBool(c: var Context; dest: var TokenBuf; info: PackedLineInfo): SymId =
  let s = "`x." & $c.counter & "." & c.thisModuleSuffix
  inc c.counter
  result = pool.syms.getOrIncl(s)
  copyIntoKind dest, VarS, info:
    dest.addSymDef result, info
    dest.addDotToken() # export, pragmas
    dest.addDotToken()
    copyTree dest, c.typeCache.builtins.boolType # type
    dest.addDotToken() # value

proc add(dest: var TokenBuf; tar: Target) =
  dest.copyTree tar.t

proc trExprInto(c: var Context; dest: var TokenBuf; n: var Cursor; v: SymId) =
  var tar = Target(m: IsEmpty)
  let typ = getType(c.typeCache, n)
  trExpr c, dest, n, tar

  if typ.typeKind in {VoidT, AutoT}:
    dest.add tar
  else:
    let info = n.info
    copyIntoKind dest, AsgnS, info:
      dest.addSymUse v, info
      dest.add tar

proc skipParRi(n: var Cursor) =
  if n.kind == ParRi:
    inc n
  else:
    error "expected ')', but got: ", n

proc trOr(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  if isComplex(n, c.goal):
    # `x or y`  <=> `if x: true else: y` <=> `if x: tmp = true else: tmp = y`
    let info = n.info
    var tmp = declareTempBool(c, dest, info)
    inc n

    var aa = Target(m: IsEmpty)
    trExpr c, dest, n, aa
    copyIntoKind dest, IfS, info:
      copyIntoKind dest, ElifU, info:
        dest.add aa                # if x
        copyIntoKind dest, StmtsS, info:
          copyIntoKind dest, AsgnS, info: # tmp = true
            dest.addSymUse tmp, info
            copyIntoKind dest, TrueX, info: discard
      copyIntoKind dest, ElseU, info:
        copyIntoKind dest, StmtsS, info:
          trExprInto c, dest, n, tmp # tmp = y
    tar.t.addSymUse tmp, info
    skipParRi n
  else:
    copyInto tar.t, n:
      trExpr c, dest, n, tar
      trExpr c, dest, n, tar

proc trAnd(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  if isComplex(n, c.goal):
    # `x and y` <=> `if x: y else: false` <=> `if x: tmp = y else: tmp = false`
    let info = n.info
    var tmp = declareTempBool(c, dest, info)
    inc n

    var aa = Target(m: IsEmpty)
    trExpr c, dest, n, aa
    copyIntoKind dest, IfS, info:
      copyIntoKind dest, ElifU, info:
        dest.add aa                # if x
        copyIntoKind dest, StmtsS, info:
          trExprInto c, dest, n, tmp # tmp = y
      copyIntoKind dest, ElseU, info:
        copyIntoKind dest, StmtsS, info:
          # tmp = false
          copyIntoKind dest, AsgnS, info:
            dest.addSymUse tmp, info
            copyIntoKind dest, FalseX, info: discard
    tar.t.addSymUse tmp, info
    skipParRi n
  else:
    copyInto tar.t, n:
      trExpr c, dest, n, tar
      trExpr c, dest, n, tar

proc trExprLoop(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  if tar.m == IsEmpty:
    tar.m = IsAppend
  else:
    assert tar.m == IsAppend, toString(n, false) & " " & $tar.m
  tar.t.add n
  inc n
  while n.kind != ParRi:
    trExpr c, dest, n, tar
  tar.t.addParRi()
  inc n

proc trExprCall(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  if tar.m == IsAppend and c.goal == TowardsNjvl:
    # bind to a temporary variable:
    let tmp = pool.syms.getOrIncl("`x." & $c.counter)
    inc c.counter
    let info = n.info
    dest.addParLe LetS, info
    dest.addSymDef tmp, info
    dest.addEmpty2 info # no export marker, no pragmas
    let typ = c.typeCache.getType(n)
    dest.copyTree typ

    var callTarget = Target(m: IsAppend)
    trExprLoop c, dest, n, callTarget
    dest.add callTarget
    dest.addParRi()

    tar.t.addSymUse tmp, info
  else:
    trExprLoop c, dest, n, tar

proc trExprToTarget(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  if c.goal == TowardsNjvl and n.exprKind in CallKinds:
    # we know here that the function call will be bound to a location, so do not bind it
    # to a temporary variable!
    trExprLoop c, dest, n, tar
  else:
    trExpr c, dest, n, tar

proc trStmtCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # IMPORTANT: Stores into `tar` helper!
  var tar = Target(m: IsAppend)
  tar.t.copyInto n:
    while n.kind != ParRi:
      trExpr c, dest, n, tar
  dest.add tar

proc trCond(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target; mustUseLabel: bool)

type
  CfVar = object
    v: SymId # as variable

proc makeCfVar(c: var Context; dest: var TokenBuf; tar: var Target; info: PackedLineInfo): CfVar =
  if tar.m == IsEmpty:
    tar.m = IsCfvar
    let s = "`j." & $c.counter & "." & c.thisModuleSuffix
    inc c.counter

    result = CfVar(v: pool.syms.getOrIncl(s))
    dest.add tagToken("cfvar", info)
    dest.addSymDef result.v, info
    dest.addParRi()

    tar.t.addSymUse result.v, info
  else:
    assert tar.m == IsCfvar
    result = CfVar(v: tar.t[0].symId)

proc useCfVar(dest: var TokenBuf; cf: CfVar; info: PackedLineInfo) =
  dest.add tagToken("jtrue", info)
  dest.addSymUse cf.v, info
  dest.addParRi()

proc trCondAnd(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  # `x and y` <=>
  # var tmp = false
  # if x:
  #   if y: jtrue
  let info = n.info
  let cf = makeCfVar(c, dest, tar, info)

  inc n

  var aa = Target(m: IsEmpty)
  trCond c, dest, n, aa, true

  copyIntoKind dest, IfS, info:
    copyIntoKind dest, ElifU, info:
      dest.add aa                # if x
      copyIntoKind dest, StmtsS, info:
        var bb = Target(m: IsEmpty)
        trCond c, dest, n, bb, true
        copyIntoKind dest, IfS, info:
          copyIntoKind dest, ElifU, info:
            dest.add bb                # if y
            copyIntoKind dest, StmtsS, info:
              useCfVar dest, cf, info

  skipParRi n

proc trCondOr(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  # `x or y` <=>
  # var tmp = false
  # if x:
  #   jtrue tmp
  # else:
  #   if y:
  #     jtrue tmp
  let info = n.info
  let cf = makeCfVar(c, dest, tar, info)

  inc n

  var aa = Target(m: IsEmpty)
  trCond c, dest, n, aa, true

  copyIntoKind dest, IfS, info:
    copyIntoKind dest, ElifU, info:
      dest.add aa                # if x
      copyIntoKind dest, StmtsS, info:
        useCfVar dest, cf, info
    # Watch out, we cannot use an ELifU here directly because `bb` can
    # have side effects!
    copyIntoKind dest, ElseU, info:
      copyIntoKind dest, StmtsS, info:
        var bb = Target(m: IsEmpty)
        trCond c, dest, n, bb, true
        copyIntoKind dest, IfS, info:
          copyIntoKind dest, ElifU, info:
            dest.add bb                # if y
            copyIntoKind dest, StmtsS, info:
              useCfVar dest, cf, info

  skipParRi n

proc trCond(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target; mustUseLabel: bool) =
  assert tar.m == IsEmpty
  if c.goal == TowardsNjvl:
    case n.exprKind
    of AndX:
      if isComplex(n, c.goal) or mustUseLabel:
        trCondAnd c, dest, n, tar
      else:
        trAnd c, dest, n, tar
    of OrX:
      if isComplex(n, c.goal) or mustUseLabel:
        trCondOr c, dest, n, tar
      else:
        trOr c, dest, n, tar
    else:
      trExpr c, dest, n, tar
  else:
    trExpr c, dest, n, tar

proc trIf(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  # if cond: a elif condB: b else: c
  # -->
  # if cond: a else: (if condB: b else: c)
  let info = n.info
  let head = n
  var tmp = SymId(0)

  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  var toClose = 0
  var ifs = 0
  inc n
  while n.kind != ParRi:
    if ifs >= 1:
      dest.addParLe ElseU, info
      dest.addParLe StmtsS, info
      inc toClose, 2

    let info = n.info
    case n.substructureKind
    of ElifU:
      var t0 = Target(m: IsEmpty)
      inc n
      trCond c, dest, n, t0, c.goal == TowardsNjvl

      dest.add head
      inc toClose
      inc ifs

      copyIntoKind dest, ElifU, info:
        dest.add t0
        #copyIntoKind dest, StmtsS, info:
        if tar.m != IsIgnored:
          copyIntoKind dest, StmtsS, info:
            trExprInto c, dest, n, tmp
        else:
          trStmt c, dest, n
      skipParRi n
    of ElseU:
      inc n
      if tar.m != IsIgnored:
        copyIntoKind dest, StmtsS, info:
          trExprInto c, dest, n, tmp
      else:
        trStmt c, dest, n
      skipParRi n
    else:
      # Bug: just copy the thing around
      takeTree dest, n
  skipParRi n

  while toClose > 0:
    dest.addParRi()
    dec toClose

  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, info

proc trCase(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  let info = n.info
  var tmp = SymId(0)

  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  var t0 = Target(m: IsEmpty)
  inc n
  trExpr c, dest, n, t0
  dest.addParLe CaseS, info
  dest.add t0
  while n.kind != ParRi:
    case n.substructureKind
    of OfU:
      copyInto(dest, n):
        takeTree dest, n # choices
        if tar.m != IsIgnored:
          copyIntoKind dest, StmtsS, info:
            trExprInto c, dest, n, tmp
        else:
          trStmt c, dest, n
    of ElseU:
      copyInto(dest, n):
        if tar.m != IsIgnored:
          copyIntoKind dest, StmtsS, info:
            trExprInto c, dest, n, tmp
        else:
          trStmt c, dest, n
    else:
      # Bug: just copy the thing around
      takeTree dest, n
  takeParRi dest, n
  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, info

proc trTry(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  let info = n.info
  var tmp = SymId(0)

  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  copyInto(dest, n):
    if tar.m != IsIgnored:
      copyIntoKind dest, StmtsS, info:
        trExprInto c, dest, n, tmp
    else:
      trStmt c, dest, n

    while n.kind != ParRi:
      case n.substructureKind
      of ExceptU:
        copyInto(dest, n):
          takeTree dest, n # declarations
          if tar.m != IsIgnored:
            copyIntoKind dest, StmtsS, info:
              trExprInto c, dest, n, tmp
          else:
            trStmt c, dest, n
      of FinU:
        # The `finally` section never produces a value!
        copyInto(dest, n):
          trStmt c, dest, n
      else:
        # Bug: just copy the thing around
        takeTree dest, n
  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, info

proc trWhile(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.copyInto n:
    if isComplex(n, c.goal):
      dest.copyIntoKind TrueX, info: discard
      copyIntoKind dest, StmtsS, info:
        var tar = Target(m: IsEmpty)
        trCond c, dest, n, tar, c.goal == TowardsNjvl
        dest.copyIntoKind IfS, info:
          dest.copyIntoKind ElifU, info:
            dest.add tar
            trStmt c, dest, n
          dest.copyIntoKind ElseU, info:
            copyIntoKind dest, StmtsS, info:
              dest.copyIntoKind BreakS, info:
                dest.addDotToken()
    else:
      var tar = Target(m: IsEmpty)
      trExpr c, dest, n, tar
      dest.add tar
      trStmt c, dest, n

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var tmp = createTokenBuf(30)
  let kind = n.symKind
  copyInto tmp, n:
    let name = n.symId
    takeTree tmp, n # name
    takeTree tmp, n # export marker
    takeTree tmp, n # pragmas
    c.typeCache.registerLocal(name, kind, n)
    takeTree tmp, n # type
    var v = Target(m: IsEmpty)
    trExprToTarget c, dest, n, v
    tmp.add v
  dest.add tmp

proc trProc(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  let kind = n.symKind
  copyInto dest, n:
    let symId = n.symId
    let isConcrete = takeRoutineHeader(c.typeCache, dest, decl, n)
    if isConcrete:
      if isLocalDecl(symId):
        c.typeCache.registerLocal(symId, kind, decl)
      c.typeCache.openScope()
      trStmt c, dest, n
      c.typeCache.closeScope()
    else:
      takeTree dest, n

proc trBlock(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  var tmp = SymId(0)

  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  copyInto(dest, n):
    takeTree dest, n # label or DotToken
    if tar.m != IsIgnored:
      copyIntoKind dest, StmtsS, n.info:
        trExprInto c, dest, n, tmp
    else:
      trStmt c, dest, n


  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, n.info

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.stmtKind
  of NoStmt:
    assert n.kind != ParRi
    takeTree dest, n
  of IfS, WhenS:
    var tar = Target(m: IsIgnored)
    trIf c, dest, n, tar
  of CaseS:
    var tar = Target(m: IsIgnored)
    trCase c, dest, n, tar
  of TryS:
    var tar = Target(m: IsIgnored)
    trTry c, dest, n, tar

  of RetS, RaiseS, YldS:
    var tar = Target(m: IsEmpty)
    let head = n
    inc n
    trExpr c, dest, n, tar
    dest.add head
    dest.add tar
    dest.addParRi()
    skipParRi n

  of DiscardS:
    if c.goal == TowardsNjvl:
      inc n
      var tar = Target(m: IsAppend)
      trExpr c, dest, n, tar
      # we must bind the result to a temporary variable!
      let tmp = pool.syms.getOrIncl("`x." & $c.counter)
      inc c.counter
      let info = n.info
      dest.addParLe LetS, info
      dest.addSymDef tmp, info
      dest.addEmpty2 info # no export marker, no pragmas
      let typ = c.typeCache.getType(n)
      dest.copyTree typ
      dest.add tar
    else:
      var tar = Target(m: IsEmpty)
      let head = n
      inc n
      trExpr c, dest, n, tar
      dest.add head
      dest.add tar
    dest.addParRi()
    skipParRi n

  of WhileS:
    trWhile c, dest, n
  of CallKindsS, InclS, ExclS:
    trStmtCall c, dest, n
  of AsgnS:
    # IMPORTANT: Stores into `tar` helper!
    var tar = Target(m: IsAppend)
    tar.t.copyInto n:
      trExpr c, dest, n, tar
      # we cannot use `trExprToTarget` here because it is not correct
      # for procs that can raise.
      trExpr c, dest, n, tar
    dest.add tar

  of AsmS, DeferS:
    # IMPORTANT: Stores into `tar` helper!
    var tar = Target(m: IsAppend)
    tar.t.copyInto n:
      while n.kind != ParRi:
        trExpr c, dest, n, tar
    dest.add tar
  of LocalDecls:
    trLocal c, dest, n
  of ProcS, FuncS, MacroS, MethodS, ConverterS:
    trProc c, dest, n
  of BlockS:
    var tar = Target(m: IsIgnored)
    trBlock c, dest, n, tar
  of IteratorS, TemplateS, TypeS, EmitS, BreakS, ContinueS,
     ForS, IncludeS, ImportS, FromimportS, ImportExceptS,
     ExportS, CommentS, AssumeS, AssertS,
     PragmasS, ImportasS, ExportexceptS, BindS, MixinS, UsingS:
    takeTree dest, n
  of ScopeS, StaticstmtS:
    c.typeCache.openScope()
    copyInto(dest, n):
      while n.kind != ParRi:
        trStmt c, dest, n
    c.typeCache.closeScope()
  of StmtsS, UnpackDeclS:
    copyInto(dest, n):
      while n.kind != ParRi:
        trStmt c, dest, n

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  # can have the dangerous `Expr` node which is the whole
  # reason for xelim's existence.
  case n.kind
  of DotToken, UnknownToken, EofToken, Ident, Symbol, SymbolDef, IntLit, UIntLit, FloatLit, CharLit, StringLit:
    takeTree tar.t, n
  of ParLe:
    case n.exprKind
    of ExprX:
      inc n
      while n.kind != ParRi:
        if not isLastSon(n):
          trStmt c, dest, n
        else:
          trExpr c, dest, n, tar
      skipParRi n
    of AndX:
      trAnd c, dest, n, tar
    of OrX:
      trOr c, dest, n, tar
    of CallKinds:
      trExprCall c, dest, n, tar
    else:
      case n.stmtKind
      of IfS:
        trIf c, dest, n, tar
      of CaseS:
        trCase c, dest, n, tar
      of TryS:
        trTry c, dest, n, tar
      of BlockS:
        trBlock c, dest, n, tar
      else:
        copyInto tar.t, n:
          while n.kind != ParRi:
            trExpr c, dest, n, tar
  of ParRi:
    bug "unexpected ')' inside"

proc lowerExprs*(n: Cursor; moduleSuffix: string; goal = ElimExprs): TokenBuf =
  var c = Context(counter: 0, typeCache: createTypeCache(), thisModuleSuffix: moduleSuffix, goal: goal)
  c.typeCache.openScope()
  result = createTokenBuf(300)
  var n = n
  assert n.stmtKind == StmtsS, $n.kind
  result.add n
  inc n
  while n.kind != ParRi:
    trStmt c, result, n
  result.addParRi()
  c.typeCache.closeScope()
  #echo "PRODUCED: ", result.toString(false)

when isMainModule:
  let n = setupProgram("debug.txt", "debug.out")
  let r = lowerExprs(n, "main")
  echo r.toString(false)
