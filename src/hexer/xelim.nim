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

import std / [assertions, syncio]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs, typenav, typeprops, builtintypes]
import passes
include ".." / nimony / nif_annotations

type
  Goal* = enum
    ElimExprs   # normal mode: eliminate expressions
    TowardsNjvl # goal mode: prepare for transformation into njvl
    LowerCasts  # lower cast expressions: bind both source and result to variables

proc isComplex(n: Cursor; goal: Goal): bool =
  var nested = 0
  var n = n
  while true:
    case n.kind
    of IntLit, UIntLit, FloatLit, StringLit, CharLit, UnknownToken, EofToken, Ident, Symbol, SymbolDef, DotToken:
      inc n
    of ParLe:
      if n.stmtKind in {IfS, CaseS, WhileS, AsgnS, LetS, VarS, CursorS, PatternvarS, StmtsS, ResultS, GletS, TletS, GvarS, TvarS}:
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
      elif goal == TowardsNjvl and n.exprKind in (CallKinds+{AndX, OrX}):
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
    IsEmpty, IsAppend, IsBound, IsIgnored, IsCfvar
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
  {.ensuresNif: addedAny(dest).}

proc tempSymName(c: var Context): string {.inline.} =
  result = "`x." & $c.counter
  inc c.counter

proc getType(c: var Context; n: Cursor): Cursor =
  result = getType(c.typeCache, n)
  assert result.typeKind != AutoT, "cannot compute type of: " & toString(n, false)

proc declareTemp(c: var Context; dest: var TokenBuf; n: Cursor): SymId =
  let info = n.info
  let typ = getType(c, n)
  let s = tempSymName(c)
  result = pool.syms.getOrIncl(s)
  copyIntoKind dest, VarS, info:
    dest.addSymDef result, info
    dest.addDotToken() # export, pragmas
    dest.addDotToken()
    copyTree dest, typ # type
    dest.addDotToken() # value

proc declareTempBool(c: var Context; dest: var TokenBuf; info: PackedLineInfo): SymId =
  let s = tempSymName(c)
  result = pool.syms.getOrIncl(s)
  copyIntoKind dest, VarS, info:
    dest.addSymDef result, info
    dest.addDotToken() # export, pragmas
    dest.addDotToken()
    copyTree dest, c.typeCache.builtins.boolType # type
    dest.addDotToken() # value

proc addTarget(dest: var TokenBuf; tar: Target) =
  dest.copyTree tar.t

proc trExprInto(c: var Context; dest: var TokenBuf; n: var Cursor; v: SymId) =
  var tar = Target(m: IsEmpty)
  let typ = getType(c, n)
  trExpr c, dest, n, tar

  if typ.typeKind in {VoidT, AutoT}:
    dest.addTarget tar
  else:
    let info = n.info
    copyIntoKind dest, AsgnS, info:
      dest.addSymUse v, info
      dest.addTarget tar

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
        dest.addTarget aa                # if x
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
        dest.addTarget aa                # if x
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
  if tar.m in {IsEmpty, IsBound}:
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
  if tar.m in {IsAppend, IsEmpty} and c.goal == TowardsNjvl:
    # bind to a temporary variable:
    let info = n.info
    let typ = getType(c, n)

    if isVoidType(typ):
      # can happen for `quit` used inside an expression context.
      trExprLoop c, dest, n, tar
      return

    # Process the call into a temporary buffer so that any nested let
    # declarations are emitted before this one starts:
    var nestedDest = createTokenBuf(30)
    var callTarget = Target(m: IsBound)
    trExprLoop c, nestedDest, n, callTarget

    # Emit nested statements first
    dest.add nestedDest

    # Now create the let binding for this call
    let tmp = pool.syms.getOrIncl(tempSymName(c))
    # `call() = 4` via a `var T` cannot be bound to a let variable
    # as the analysis in constracts_njvl is too simplistic.
    # It would produce: "Cannot reassign a let variable".
    if typ.typeKind == MutT:
      dest.addParLe VarS, info
    else:
      dest.addParLe LetS, info
    dest.addSymDef tmp, info
    dest.addEmpty info # no export marker
    # Mark these temporaries as (inline) so that the analysis
    # in contracts_njvl remembers the value. This is necessary
    # for borrow checking which is defined on the original source
    # code expressions!
    dest.copyIntoKind PragmasS, info:
      dest.copyIntoKind InlineP, info: discard
    dest.copyTree typ
    dest.addTarget callTarget
    dest.addParRi()

    tar.t.addSymUse tmp, info
  else:
    trExprLoop c, dest, n, tar

proc trStmtCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # IMPORTANT: Stores into `tar` helper!
  var tar = Target(m: IsAppend)
  tar.t.copyInto n:
    while n.kind != ParRi:
      trExpr c, dest, n, tar
  dest.addTarget tar

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
    dest.add tagToken("mflag", info)
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
      dest.addTarget aa                # if x
      copyIntoKind dest, StmtsS, info:
        var bb = Target(m: IsEmpty)
        trCond c, dest, n, bb, true
        copyIntoKind dest, IfS, info:
          copyIntoKind dest, ElifU, info:
            dest.addTarget bb                # if y
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
      dest.addTarget aa                # if x
      copyIntoKind dest, StmtsS, info:
        useCfVar dest, cf, info
    # Watch out, we cannot use an ElifU here directly because `bb` can
    # have side effects!
    copyIntoKind dest, ElseU, info:
      copyIntoKind dest, StmtsS, info:
        var bb = Target(m: IsEmpty)
        trCond c, dest, n, bb, true
        copyIntoKind dest, IfS, info:
          copyIntoKind dest, ElifU, info:
            dest.addTarget bb                # if y
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
    of ErrX, SufX, AtX, DerefX, DotX, PatX, ParX, AddrX, NilX,
       InfX, NeginfX, NanX, FalseX, TrueX, XorX, NotX, NegX,
       SizeofX, AlignofX, OffsetofX, OconstrX, AconstrX, BracketX,
       CurlyX, CurlyatX, OvfX, AddX, SubX, MulX, DivX, ModX,
       ShrX, ShlX, BitandX, BitorX, BitxorX, BitnotX, EqX, NeqX,
       LeX, LtX, CastX, ConvX, CallX, CmdX, CchoiceX, OchoiceX,
       PragmaxX, QuotedX, HderefX, DdotX, HaddrX, NewrefX,
       NewobjX, TupX, TupconstrX, SetconstrX, TabconstrX, AshrX,
       BaseobjX, HconvX, DconvX, CallstrlitX, InfixX, PrefixX,
       HcallX, CompilesX, DeclaredX, DefinedX, AstToStrX,
       InstanceofX, ProccallX, HighX, LowX, TypeofX, UnpackX,
       FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX,
       DefaultobjX, DefaulttupX, DefaultdistinctX, DelayX,
       Delay0X, SuspendX, ExprX, DoX, ArratX, TupatX, PlussetX,
       MinussetX, MulsetX, XorsetX, EqsetX, LesetX, LtsetX,
       InsetX, CardX, EmoveX, DestroyX, DupX, CopyX, WasmovedX,
       SinkhX, TraceX, InternalTypeNameX, InternalFieldPairsX,
       FailedX, IsX, EnvpX, KvX, NoExpr:
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
        dest.addTarget t0
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
    of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
       TypevarU, EfldU, FldU, WhenU, TypevarsU, CaseU, OfU,
       StmtsU, ParamsU, PragmasU, EitherU, JoinU, UnpackflatU,
       UnpacktupU, ExceptU, FinU, UncheckedU, NoSub:
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
  dest.addTarget t0
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
    of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
       TypevarU, EfldU, FldU, WhenU, ElifU, TypevarsU, CaseU,
       StmtsU, ParamsU, PragmasU, EitherU, JoinU, UnpackflatU,
       UnpacktupU, ExceptU, FinU, UncheckedU, NoSub:
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
      of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
         TypevarU, EfldU, FldU, WhenU, ElifU, ElseU, TypevarsU,
         CaseU, OfU, StmtsU, ParamsU, PragmasU, EitherU, JoinU,
         UnpackflatU, UnpacktupU, UncheckedU, NoSub:
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
            dest.addTarget tar
            trStmt c, dest, n
          dest.copyIntoKind ElseU, info:
            copyIntoKind dest, StmtsS, info:
              dest.copyIntoKind BreakS, info:
                dest.addDotToken()
    else:
      var tar = Target(m: IsEmpty)
      trExpr c, dest, n, tar
      dest.addTarget tar
      trStmt c, dest, n

proc trFor(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let head = n.load()
  inc n
  var tar = Target(m: IsEmpty)
  trExpr c, dest, n, tar # iterator call
  dest.add head
  dest.addTarget tar
  takeTree dest, n # for loop variables
  trStmt c, dest, n
  dest.takeParRi n

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
    var v = Target(m: IsBound)
    trExpr c, dest, n, v
    tmp.addTarget v
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
    if n.exprKind == ExprX:
      var tar = Target(m: IsEmpty)
      trExpr c, dest, n, tar
      if tar.m == IsAppend:
        dest.addTarget tar
    else:
      takeTree dest, n
  of PragmaxS:
    copyInto(dest, n):
      takeTree dest, n  # pragmas
      trStmt c, dest, n  # body
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
    dest.addTarget tar
    dest.addParRi()
    skipParRi n

  of DiscardS:
    if c.goal == TowardsNjvl:
      inc n
      if n.kind == DotToken:
        dest.takeToken n
      else:
        let typ = getType(c, n)
        var tar = Target(m: IsBound)
        trExpr c, dest, n, tar
        # we must bind the result to a temporary variable!
        let tmp = pool.syms.getOrIncl("`x." & $c.counter)
        inc c.counter
        let info = n.info
        dest.addParLe LetS, info
        dest.addSymDef tmp, info
        dest.addEmpty2 info # no export marker, no pragmas
        dest.copyTree typ
        dest.addTarget tar
        dest.addParRi()
    else:
      var tar = Target(m: IsEmpty)
      let head = n
      inc n
      trExpr c, dest, n, tar
      dest.add head
      dest.addTarget tar
      dest.addParRi()
    skipParRi n

  of WhileS:
    trWhile c, dest, n
  of ForS:
    trFor c, dest, n
  of CallKindsS, InclS, ExclS:
    trStmtCall c, dest, n
  of AsgnS:
    # IMPORTANT: Stores into `tar` helper!
    var tar = Target(m: IsAppend)
    # Peek at the LHS: if it is the `result` variable, do not extract a
    # call on the RHS to a temporary.  nj.nim's trAsgn handles the call
    # directly via trBoundExpr and emits the "was successful?" branching
    # after the store, which is both simpler and avoids borrow-checking
    # trouble caused by the extra temporary.
    var lhsIsResult = false
    if c.goal == TowardsNjvl:
      let peek = n.firstSon
      lhsIsResult = peek.kind == Symbol
    tar.t.copyInto n:
      trExpr c, dest, n, tar
      if c.goal == TowardsNjvl:
        if lhsIsResult: tar.m = IsBound  # keep call in-place, no temp
        trExpr c, dest, n, tar
      else:
        tar.m = IsBound
        trExpr c, dest, n, tar
    dest.addTarget tar

  of AsmS, DeferS:
    # IMPORTANT: Stores into `tar` helper!
    var tar = Target(m: IsAppend)
    tar.t.copyInto n:
      while n.kind != ParRi:
        trExpr c, dest, n, tar
    dest.addTarget tar
  of LocalDecls:
    trLocal c, dest, n
  of ProcS, FuncS, MacroS, MethodS, ConverterS, IteratorS:
    trProc c, dest, n
  of BlockS:
    var tar = Target(m: IsIgnored)
    trBlock c, dest, n, tar
  of TemplateS, TypeS, EmitS, BreakS, ContinueS,
     IncludeS, ImportS, FromimportS, ImportexceptS,
     ExportS, CommentS, AssumeS, AssertS,
     PragmasS, ImportasS, ExportexceptS, BindS, MixinS, UsingS:
    takeTree dest, n
  of ScopeS, StaticstmtS:
    c.typeCache.openScope()
    copyInto(dest, n):
      while n.kind != ParRi:
        trStmt c, dest, n
    c.typeCache.closeScope()
  of StmtsS, UnpackdeclS:
    copyInto(dest, n):
      while n.kind != ParRi:
        trStmt c, dest, n

proc isIntLike(tk: TypeKind): bool {.inline.} =
  tk in {IntT, UIntT, CharT, BoolT}

proc needsBitCast(destType: Cursor; srcType: Cursor): bool =
  ## Returns true when the cast requires memcpy for bit reinterpretation.
  ## Integer-to-integer and float-to-float casts can use a plain C cast.
  ## Integer-to-float (and vice versa) needs memcpy.
  let dtk = typeKind(destType)
  let stk = typeKind(srcType)
  if dtk == FloatT and stk == FloatT: return false
  if isIntLike(dtk) and isIntLike(stk): return false
  # One is float, the other is integer-like (or both are value types of
  # different families): need memcpy for correct bit reinterpretation.
  result = dtk in {IntT, UIntT, FloatT, CharT, BoolT} and
           stk in {IntT, UIntT, FloatT, CharT, BoolT}

proc trCast(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  let info = n.info
  inc n # skip "cast" tag

  var destTypeBuf = createTokenBuf(8)
  takeTree destTypeBuf, n # copy dest type, n now at srcExpr
  let destType = beginRead(destTypeBuf)

  let dtk = typeKind(destType)
  # Quick check: if dest is not a value type, skip getType on source entirely
  if dtk notin {IntT, UIntT, FloatT, CharT, BoolT}:
    var srcTarget = Target(m: IsEmpty)
    trExpr c, dest, n, srcTarget
    skipParRi n
    tar.t.addParLe CastX, info
    tar.t.addSubtree destType
    tar.t.addTarget srcTarget
    tar.t.addParRi()
    return

  let srcType = getType(c, n)
  if not needsBitCast(destType, srcType):
    # Same-family cast (e.g. int-to-int) - use plain C cast
    var srcTarget = Target(m: IsEmpty)
    trExpr c, dest, n, srcTarget
    skipParRi n
    tar.t.addParLe CastX, info
    tar.t.addSubtree destType
    tar.t.addTarget srcTarget
    tar.t.addParRi()
    return

  # Cross-family value type cast (e.g. int↔float):
  # lower to copyMem(addr dest, addr src, sizeof(DstType))
  var srcTarget = Target(m: IsEmpty)
  trExpr c, dest, n, srcTarget
  skipParRi n

  # Ensure source is a variable
  var srcSym: SymId
  var srcCur = beginRead(srcTarget.t)
  if srcCur.kind == Symbol:
    srcSym = srcCur.symId
  else:
    srcSym = pool.syms.getOrIncl(tempSymName(c))
    copyIntoKind dest, VarS, info:
      dest.addSymDef srcSym, info
      dest.addDotToken() # export marker
      dest.copyIntoKind PragmasS, info:
        dest.copyIntoKind InlineP, info: discard
      copyTree dest, srcType
      dest.addTarget srcTarget # value

  # Create dest variable (uninitialized)
  let dstSym = pool.syms.getOrIncl(tempSymName(c))
  copyIntoKind dest, VarS, info:
    dest.addSymDef dstSym, info
    dest.addDotToken() # export marker
    dest.copyIntoKind PragmasS, info:
      dest.copyIntoKind InlineP, info: discard
    dest.addSubtree destType
    dest.addDotToken() # no initializer

  # Emit: copyMem(addr dstSym, addr srcSym, sizeof(DstType))
  let copyMemSym = pool.syms.getOrIncl("copyMem.0." & SystemModuleSuffix)
  copyIntoKind dest, CallX, info:
    dest.addSymUse copyMemSym, info
    dest.copyIntoKind AddrX, info:
      dest.addSymUse dstSym, info
    dest.copyIntoKind AddrX, info:
      dest.addSymUse srcSym, info
    dest.copyIntoKind SizeofX, info:
      dest.addSubtree destType

  tar.t.addSymUse dstSym, info

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
    of CastX:
      if c.goal == LowerCasts:
        trCast c, dest, n, tar
      else:
        trExprLoop c, dest, n, tar
    of ErrX, SufX, AtX, DerefX, DotX, PatX, ParX, AddrX, NilX,
       InfX, NeginfX, NanX, FalseX, TrueX, XorX, NotX, NegX,
       SizeofX, AlignofX, OffsetofX, OconstrX, AconstrX,
       BracketX, CurlyX, CurlyatX, OvfX, AddX, SubX, MulX,
       DivX, ModX, ShrX, ShlX, BitandX, BitorX, BitxorX,
       BitnotX, EqX, NeqX, LeX, LtX, ConvX, CchoiceX,
       OchoiceX, PragmaxX, QuotedX, HderefX, DdotX, HaddrX,
       NewrefX, NewobjX, TupX, TupconstrX, SetconstrX,
       TabconstrX, AshrX, BaseobjX, HconvX, DconvX, CompilesX,
       DeclaredX, DefinedX, AstToStrX, InstanceofX, HighX, LowX,
       TypeofX, UnpackX, FieldsX, FieldpairsX, EnumtostrX,
       IsmainmoduleX, DefaultobjX, DefaulttupX,
       DefaultdistinctX, Delay0X, SuspendX, DoX, ArratX, TupatX,
       PlussetX, MinussetX, MulsetX, XorsetX, EqsetX, LesetX,
       LtsetX, InsetX, CardX, EmoveX, DestroyX, DupX, CopyX,
       WasmovedX, SinkhX, TraceX, InternalTypeNameX,
       InternalFieldPairsX, FailedX, IsX, EnvpX, KvX, NoExpr:
      case n.stmtKind
      of IfS:
        trIf c, dest, n, tar
      of CaseS:
        trCase c, dest, n, tar
      of TryS:
        trTry c, dest, n, tar
      of BlockS:
        trBlock c, dest, n, tar
      of CallS, CmdS, GvarS, TvarS, VarS, ConstS, ResultS,
         GletS, TletS, LetS, CursorS, PatternvarS, ProcS, FuncS,
         IteratorS, ConverterS, MethodS, MacroS, TemplateS,
         TypeS, EmitS, AsgnS, ScopeS, WhenS, BreakS, ContinueS,
         ForS, WhileS, RetS, YldS, StmtsS, PragmasS, PragmaxS,
         InclS, ExclS, IncludeS, ImportS, ImportasS,
         FromimportS, ImportexceptS, ExportS, ExportexceptS,
         CommentS, DiscardS, RaiseS, UnpackdeclS, AssumeS,
         AssertS, CallstrlitS, InfixS, PrefixS, HcallS,
         StaticstmtS, BindS, MixinS, UsingS, AsmS, DeferS,
         NoStmt:
        trExprLoop c, dest, n, tar
  of ParRi:
    bug "unexpected ')' inside"

proc lowerExprs*(pass: var Pass; goal = ElimExprs) =
  var n = pass.n  # Extract cursor locally
  var c = Context(counter: 0, typeCache: createTypeCache(), thisModuleSuffix: pass.moduleSuffix, goal: goal)
  c.typeCache.openScope()
  assert n.stmtKind == StmtsS, $n.kind
  pass.dest.add n
  inc n
  while n.kind != ParRi:
    trStmt c, pass.dest, n
  pass.dest.addParRi()
  c.typeCache.closeScope()
  #echo "PRODUCED: ", pass.dest.toString(false)

when isMainModule:
  var owningBuf = createTokenBuf(300)
  let n = setupProgram("debug.txt", "debug.out", owningBuf)
  var pass = initPass(move owningBuf, "main", "xelim", 64)
  lowerExprs(pass)
  echo pass.dest.toString(false)
