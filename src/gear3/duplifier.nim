#
#
#           Gear3 Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

##[

The duplifier runs before `to_stmts` as it makes heavy use
of `StmtListExpr`. After `to_stmts` destructor injections
and assignment rewrites are performed. This is done in `destroyer`.

Expressions
===========

There are 4 cases:

1. Owner to owner transfer (`var x = f()`): Nothing to do.
2. Owner to unowner transfer (`f g()`): Transform to `var tmp = g(); f(tmp)`.
3. Unowner to owner transfer (`f_sink(x)`): Transform to `f_sink(=dup(x))`.
   Or to `f_sink(x); =wasMoved(x)`.
4. Unowner to unowner transfer (`f(x)`): Nothing to do.

It follows that we're only interested in Call expressions here, or similar
(object constructors etc).

]##

import std / [assertions]
include nifprelude
import nifindexes, symparser, treemangler, lifter
import ".." / nimony / [nimony_model, programs, decls, typenav]

type
  Context = object
    dest: TokenBuf
    lifter: ref LiftingCtx
    reportLastUse: bool
    typeCache: TypeCache
    tmpCounter: int

  Expects = enum
    DontCare,
    WillBeOwned,
    WantNonOwner,
    WantOwner

# -------------- helpers ----------------------------------------

proc isLastRead(c: Context; n: Cursor): bool =
  # XXX We don't have a move analyser yet.
  false

const
  ConstructingExprs = {CallX, CallStrLitX, InfixX, PrefixX, CmdX, OconstrX, AconstrX, TupleConstrX}

proc constructsValue*(n: Cursor): bool =
  var n = n
  while true:
    case n.exprKind
    of CastX, ConvX, HconvX, DconvX, OconvX:
      inc n
      skip n
    of ExprX:
      inc n
      while not isLastSon(n): skip n
    else: break
  result = n.exprKind in ConstructingExprs

proc rootOf(n: Cursor): SymId =
  var n = n
  while n.exprKind in {DotX, TupAtX, AtX}:
    n = n.firstSon
  if n.kind == Symbol:
    result = n.symId
  else:
    result = NoSymId

proc potentialSelfAsgn(dest, src: Cursor): bool =
  # if `x.fieldA` and `*.fieldB` it is not a self assignment.
  # if `x` and `y` and `x != y` it is not a self assignment.
  if src.exprKind in ConstructingExprs:
    result = false
  else:
    result = true # true until proven otherwise
    let d = rootOf(dest)
    let s = rootOf(src)
    if d != NoSymId or s != NoSymId:
      # one of the expressions was analysable
      if d == s:
        # see if we can distinguish between `x.fieldA` and `x.fieldB` which
        # cannot alias. We do know here that both expressions are free of
        # pointer derefs, so we can simply use `sameValues` here.
        result = sameTrees(dest, src)
      else:
        # different roots while we know that at least one expression has
        # no harmful pointer deref:
        result = false

# -----------------------------------------------------------

when not defined(nimony):
  proc tr(c: var Context; n: var Cursor; e: Expects)

proc trSons(c: var Context; n: var Cursor; e: Expects) =
  while n.kind != ParRi:
    tr c, n, e

proc isResultUsage(n: Cursor): bool {.inline.} =
  result = false
  if n.kind == Symbol:
    let res = tryLoadSym(n.symId)
    if res.status == LacksNothing:
      let r = asLocal(res.decl)
      result = r.kind == ResultY

proc wantParRi(dest: var TokenBuf; n: var Cursor) =
  if n.kind == ParRi:
    dest.add n
    inc n
  else:
    error "expected ')', but got: ", n

template copyInto*(dest: var TokenBuf; n: var Cursor; body: untyped) =
  assert n.kind == ParLe
  dest.add n
  inc n
  body
  wantParRi dest, n

proc trReturn(c: var Context; n: var Cursor) =
  copyInto c.dest, n:
    if isResultUsage(n):
      copyTree c.dest, n
    else:
      tr c, n, WantOwner

proc evalLeftHandSide(c: var Context; le: var Cursor): TokenBuf =
  result = createTokenBuf(10)
  if le.kind == Symbol:
    # simple enough:
    takeTree result, le
  else:
    let typ = getType(c.typeCache, le)
    let info = le.info
    let tmp = pool.syms.getOrIncl("`lhs." & $c.tmpCounter)
    inc c.tmpCounter
    copyIntoKind c.dest, VarS, info:
      addSymDef c.dest, tmp, info
      c.dest.addEmpty2 info # export marker, pragma
      copyIntoKind c.dest, PtrT, info: # type
        copyTree c.dest, typ
      copyIntoKind c.dest, AddrX, info:
        tr c, le, DontCare

    copyIntoKind result, DerefX, info:
      copyIntoSymUse result, tmp, info

proc callDestroy(c: var Context; destroyProc: SymId; arg: TokenBuf) =
  let info = arg[0].info
  copyIntoKind c.dest, CallS, info:
    copyIntoSymUse c.dest, destroyProc, info
    copyTree c.dest, arg

proc callDestroy(c: var Context; destroyProc: SymId; arg: SymId; info: PackedLineInfo) =
  copyIntoKind c.dest, CallS, info:
    copyIntoSymUse c.dest, destroyProc, info
    copyIntoSymUse c.dest, arg, info

proc tempOfTrArg(c: var Context; n: Cursor; typ: Cursor): SymId =
  var n = n
  let info = n.info
  result = pool.syms.getOrIncl("`lhs." & $c.tmpCounter)
  inc c.tmpCounter
  copyIntoKind c.dest, VarS, info:
    addSymDef c.dest, result, info
    c.dest.addEmpty2 info # export marker, pragma
    copyTree c.dest, typ
    tr c, n, WillBeOwned

proc callDup(c: var Context; arg: var Cursor) =
  let typ = getType(c.typeCache, arg)
  let info = arg.info
  let hookProc = getHook(c.lifter[], attachedDup, typ, info)
  if hookProc != NoSymId and arg.kind != StringLit:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, hookProc, info
      tr c, arg, WillBeOwned
  else:
    tr c, arg, WillBeOwned

proc callWasMoved(c: var Context; arg: Cursor) =
  let typ = getType(c.typeCache, arg)
  let info = arg.info
  let hookProc = getHook(c.lifter[], attachedWasMoved, typ, info)
  if hookProc != NoSymId:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, hookProc, info
      copyIntoKind c.dest, HaddrX, info:
        copyTree c.dest, arg

proc trAsgn(c: var Context; n: var Cursor) =
  #[
  `x = f()` is turned into `=destroy(x); x =bitcopy f()`.
  `x = lastUse y` is turned into either

    `=destroy(x); x =bitcopy y; =wasMoved(y)` # no self assignments possible

  or

    `let tmp = x; x =bitcopy y; =wasMoved(y); =destroy(tmp)`  # safe for self assignments

  `x = someUse y` is turned into either

    `=destroy(x); x =bitcopy =dup(y)` # no self assignments possible

  or

    `let tmp = x; x =bitcopy =dup(y); =destroy(tmp)` # safe for self assignments

  But we should really prefer to call `=copy(x, y)` here for some of these cases.
  ]#
  var n2 = n
  inc n2 # AsgnS
  let le = n2
  skip n2
  let ri = n2
  let riType = getType(c.typeCache, ri)
  let destructor = getDestructor(c.lifter[], riType, n.info)
  if destructor == NoSymId:
    # the type has no destructor, there is nothing interesting to do:
    trSons c, n, DontCare

  else:
    let isNotFirstAsgn = true # XXX Adapt this once we have "isFirstAsgn" analysis
    var leCopy = le
    var lhs = evalLeftHandSide(c, leCopy)
    if constructsValue(ri):
      # `x = f()` is turned into `=destroy(x); x =bitcopy f()`.
      if isNotFirstAsgn:
        callDestroy(c, destructor, lhs)
      copyInto c.dest, n:
        copyTree c.dest, lhs
        n = ri
        tr c, n, WillBeOwned
    elif isLastRead(c, ri):
      if isNotFirstAsgn and potentialSelfAsgn(le, ri):
        # `let tmp = y; =wasMoved(y); =destroy(x); x =bitcopy tmp`
        let tmp = tempOfTrArg(c, ri, riType)
        callWasMoved c, ri
        callDestroy(c, destructor, lhs)
        copyInto c.dest, n:
          var lhsAsCursor = cursorAt(lhs, 0)
          tr c, lhsAsCursor, DontCare
          copyIntoSymUse c.dest, tmp, ri.info
          n = n2
          skip n # skip right hand side
      else:
        if isNotFirstAsgn:
          callDestroy(c, destructor, lhs)
        copyInto c.dest, n:
          copyTree c.dest, lhs
          var n = ri
          tr c, n, WillBeOwned
        callWasMoved c, ri
    else:
      # XXX We should really prefer to simply call `=copy(x, y)` here.
      if isNotFirstAsgn and potentialSelfAsgn(le, ri):
        # `let tmp = x; x =bitcopy =dup(y); =destroy(tmp)`
        let tmp = tempOfTrArg(c, ri, riType)
        copyInto c.dest, n:
          var lhsAsCursor = cursorAt(lhs, 0)
          tr c, lhsAsCursor, DontCare
          var n = ri
          callDup c, n
        callDestroy(c, destructor, tmp, le.info)
      else:
        if isNotFirstAsgn:
          callDestroy(c, destructor, lhs)
        copyInto c.dest, n:
          var lhsAsCursor = cursorAt(lhs, 0)
          tr c, lhsAsCursor, DontCare
          var n = ri
          callDup c, n

proc skipParRi*(n: var Cursor) =
  if n.kind == ParRi:
    inc n
  else:
    error "expected ')', but got: ", n

proc trExplicitDestroy(c: var Context; n: var Cursor) =
  let typ = getType(c.typeCache, n.firstSon)
  let info = n.info
  let destructor = getDestructor(c.lifter[], typ, info)
  if destructor == NoSymId:
    # the type has no destructor, there is nothing interesting to do:
    c.dest.addEmpty info
    inc n
    skip n
  else:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, destructor, info
      inc n
      tr c, n, DontCare
  skipParRi n

proc trExplicitDup(c: var Context; n: var Cursor; e: Expects) =
  let typ = getType(c.typeCache, n)
  let info = n.info
  let hookProc = getHook(c.lifter[], attachedDup, typ, info)
  if hookProc != NoSymId:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, hookProc, info
      inc n
      tr c, n, DontCare
  else:
    let e2 = if e == WillBeOwned: WantOwner else: e
    inc n
    tr c, n, e2
  skipParRi n

proc trOnlyEssentials(c: var Context; n: var Cursor) =
  var nested = 0
  while true:
    case n.kind
    of Symbol, UIntLit, StringLit, IntLit, FloatLit, CharLit, SymbolDef, UnknownToken, EofToken, DotToken, Ident:
      c.dest.add n
      inc n
    of ParLe:
      var handled = false
      if n.exprKind in CallKinds:
        var n = firstSon n
        if n.kind == Symbol:
          var fn = pool.syms[n.symId]
          extractBasename fn
          case fn
          of "=dup":
            trExplicitDup c, n, DontCare
            handled = true
          of "=destroy":
            trExplicitDestroy c, n
            handled = true
      if not handled:
        c.dest.add n
        inc n
        inc nested
    of ParRi:
      c.dest.add n
      inc n
      dec nested
    if nested == 0: break

proc hasBuiltinPragma(n: Cursor; kind: PragmaKind): bool =
  result = false
  var n = n
  if n.kind == DotToken:
    discard
  else:
    inc n
    while n.kind != ParRi:
      if pragmaKind(n) == kind:
        result = true
        break
      skip n

proc trProcDecl(c: var Context; n: var Cursor) =
  c.dest.add n
  var r = takeRoutine(n)
  copyTree c.dest, r.name
  copyTree c.dest, r.exported
  copyTree c.dest, r.pattern
  copyTree c.dest, r.typevars
  copyTree c.dest, r.params
  copyTree c.dest, r.pragmas
  copyTree c.dest, r.effects
  if r.body.stmtKind == StmtsS and not isGeneric(r):
    if hasBuiltinPragma(r.pragmas, NoDestroy):
      trOnlyEssentials c, r.body
    else:
      tr c, r.body, DontCare
  else:
    copyTree c.dest, r.body
  c.dest.addParRi()

proc hasDestructor(c: Context; typ: Cursor): bool {.inline.} =
  not isTrivial(c.lifter[], typ)

type
  OwningTemp = object
    active: bool
    s: SymId
    info: PackedLineInfo

template owningTempDefault(): OwningTemp =
  OwningTemp(active: false, s: NoSymId, info: NoLineInfo)

proc bindToTemp(c: var Context; typ: Cursor; info: PackedLineInfo; kind = VarS): OwningTemp =
  let s = pool.syms.getOrIncl("`tmp." & $c.tmpCounter)
  inc c.tmpCounter

  c.dest.addParLe ExprX, info
  c.dest.addParLe StmtsS, info

  c.dest.addParLe kind, info
  addSymDef c.dest, s, info
  c.dest.addEmpty2 info # export marker, pragmas
  copyTree c.dest, typ
  # value is filled in by the caller!
  result = OwningTemp(active: true, s: s, info: info)

proc finishOwningTemp(dest: var TokenBuf; ow: OwningTemp) =
  if ow.active:
    dest.addParRi() # finish the VarS
    dest.addParRi()  # finish the StmtsS
    dest.copyIntoSymUse ow.s, ow.info
    dest.addParRi()  # finish the StmtListExpr

proc trCall(c: var Context; n: var Cursor; e: Expects) =
  var ow = owningTempDefault()
  let retType = getType(c.typeCache, n)
  if hasDestructor(c, retType) and e == WantNonOwner:
    ow = bindToTemp(c, retType, n.info)

  c.dest.add n
  inc n # skip `(call)`
  var fnType = getType(c.typeCache, n)
  takeTree c.dest, n # skip `fn`
  assert fnType == "params"
  inc fnType
  while n.kind != ParRi:
    let previousFormalParam = fnType
    let param = takeLocal(fnType)
    let pk = param.typ.typeKind
    var e2 = WantNonOwner
    if pk == SinkT:
      e2 = WantOwner
    elif pk == VarargsT:
      # do not advance formal parameter:
      fnType = previousFormalParam
    else:
      skipParRi(fnType)
    tr c, n, e2
  wantParRi c.dest, n
  finishOwningTemp c.dest, ow

proc trRawConstructor(c: var Context; n: var Cursor; e: Expects) =
  # Idioms like `echo ["ab", myvar, "xyz"]` are important to translate well.
  let e2 = if e == WillBeOwned: WantOwner else: e
  c.dest.add n
  inc n
  while n.kind != ParRi:
    tr c, n, e2
  wantParRi c.dest, n

proc trConvExpr(c: var Context; n: var Cursor; e: Expects) =
  copyInto c.dest, n:
    takeTree c.dest, n # type
    tr c, n, e

proc trObjConstr(c: var Context; n: var Cursor; e: Expects) =
  var ow = owningTempDefault()
  let typ = n.firstSon
  if hasDestructor(c, typ) and e == WantNonOwner:
    ow = bindToTemp(c, typ, n.info)
  copyInto c.dest, n:
    takeTree c.dest, n
    while n.kind != ParRi:
      assert n.exprKind == KvX
      copyInto c.dest, n:
        takeTree c.dest, n
        tr c, n, WantOwner
  finishOwningTemp c.dest, ow

proc genLastRead(c: var Context; n: var Cursor; typ: Cursor) =
  let ex = n
  let info = n.info
  # translate it to: `(var tmp = location; wasMoved(location); tmp)`
  var ow = bindToTemp(c, typ, info, CursorS)
  takeTree c.dest, n

  c.dest.addParRi() # finish the VarDecl

  let hookProc = getHook(c.lifter[], attachedWasMoved, typ, info)
  if hookProc != NoSymId:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, hookProc, info
      copyIntoKind c.dest, HaddrX, info:
        copyTree c.dest, ex

  c.dest.addParRi() # finish the StmtList
  c.dest.copyIntoSymUse ow.s, ow.info
  c.dest.addParRi() # finish the StmtListExpr

proc trLocation(c: var Context; n: Cursor; e: Expects) =
  # `x` does not own its value as it can be read multiple times.
  let typ = getType(c.p, n)
  if e == WantOwner and hasDestructor(c, typ):
    if isLastRead(c, n):
      genLastRead(c, dest, n, typ)
    else:
      let info = n.info
      # translate `x` to `=dup(x)`:
      let hookProc = getHook(c.lifter[], attachedDup, typ, info)
      if hookProc.s != SymId(-1):
        copyIntoKind dest, Call, info:
          copyIntoSymUse c.p, dest, hookProc, info
          if isAtom(tree, n):
            copyTree dest, n
          else:
            trSons c, dest, n, DontCare
      elif isAtom(tree, n):
        copyTree dest, n
      else:
        trSons c, dest, n, DontCare
  elif isAtom(tree, n):
    copyTree dest, n
  else:
    trSons c, dest, n, DontCare

proc trLocal(c: var Context; n: Cursor) =
  let r = asLocal(tree, n)
  var wasMovedArg = noPos
  copyInto(dest, n):
    copyTree dest, r.name
    copyTree dest, r.ex
    copyTree dest, r.pragmas
    copyTree dest, r.typ

    let localType = getType(c.p, r.name)
    let destructor = getDestructor(c.lifter[], localType, n.info)
    if destructor.s != SymId(-1):
      if constructsValue(c.p, r.value):
        tr c, dest, r.value, WillBeOwned
      elif isLastRead(c, r.value):
        tr c, dest, r.value, WillBeOwned
        wasMovedArg = r.value
      else:
        callDup c, dest, r.value
    else:
      tr c, dest, r.value, WillBeOwned
  if wasMovedArg != noPos:
    callWasMoved c, wasMovedArg

proc trStmtListExpr(c: var Context; n: Cursor; e: Expects) =
  let (t, s, x) = sons3(tree, n)
  copyInto dest, n:
    copyTree dest, t
    tr(c, dest, s, WantNonOwner)
    tr(c, dest, x, e)

proc trEnsureMove(c: var Context; n: Cursor; e: Expects) =
  let typ = getType(c.p, n)
  if isLastRead(c, n.firstSon):
    if e == WantOwner and hasDestructor(c, typ):
      copyInto dest, n:
        genLastRead(c, dest, n, typ)
    else:
      copyInto dest, n:
        tr c, dest, n.firstSon, e
  elif constructsValue(c.p, n.firstSon):
    # we allow rather silly code like `ensureMove(234)`.
    # Seems very useful for generic programming as this can come up
    # from template expansions:
    copyInto dest, n:
      tr c, dest, n.firstSon, e
  else:
    produceError dest, n, c.p[tree.m], ["not the last usage of: $1"]

proc tr(c: var Context; n: Cursor; e: Expects) =
  case n.kind
  of Call:
    trCall c, dest, n, e
  of TypedExpr:
    let (typ, ex) = sons2(tree, n)
    if constructsValue(c.p, n):
      trConstructor c, dest, typ, ex, e
    else:
      trSons c, dest, n, DontCare
  of ConvExpr, CastExpr:
    trConvExpr c, dest, n, e
  of ObjConstr:
    trObjConstr c, dest, n, e
  of SymUse, ModuleSymUse, FieldAccess, VarFieldAccess, TupleFieldAccess, PtrFieldAccess,
     PtrVarFieldAccess, RefFieldAccess, RefVarFieldAccess, ArrayAt, ArrayPtrAt:
    trLocation c, dest, n, e
  of ReturnStmt:
    trReturn(c, dest, n)
  of Asgn, FirstAsgn:
    trAsgn c, dest, n
  of VarDecl, LetDecl, ConstDecl, ResultDecl:
    trLocal c, dest, n
  of DeclarativeNodes, AtomsExceptSymUse, Pragmas, TemplateDecl, IteratorDecl,
     UsingStmt, CommentStmt, BindStmt, MixinStmt, ContinueStmt, BreakStmt:
    copyTree dest, n
  of ProcDecl, FuncDecl, MacroDecl, MethodDecl, ConverterDecl:
    trProcDecl c, dest, n
  of Par:
    trSons(c, dest, n, e)
  of StmtListExpr:
    trStmtListExpr c, dest, n, e
  of TupleConstr, BracketConstr:
    trRawConstructor c, dest, n, e
  of EqDup:
    trExplicitDup c, dest, n, e
  of EqDestroy:
    trExplicitDestroy c, dest, n
  of EnsureMove:
    trEnsureMove c, dest, n, e
  else:
    takeToken c.dest, n
    while n.kind != ParRi:
      tr(c, n, WantNonOwner)
    wantParRi c.dest, n

proc injectDups*(n: Cursor; lifter: ref LiftingCtx): TokenBuf =
  var c = Context(lifter: lifter, typeCache: createTypeCache())

  result = createTree(p, thisModule)
  tr(c, n, WantNonOwner)
  genMissingHooks lifter[]

  result = ensureMove(c.dest)
