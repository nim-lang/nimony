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

include nifprelude
import nifindexes, symparser, treemangler
import ".." / nimony / [nimony_model, programs, typenav]

type
  Context = object
    dest: TokenBuf
    lifter: ref LiftingCtx
    procStart: Cursor
    localTypes: TreeId
    reportLastUse: bool

  Expects = enum
    DontCare,
    WillBeOwned,
    WantNonOwner,
    WantOwner

proc isLastRead(c: Context; n: Cursor): bool =
  let r = rootOf(c.p, tree, n)
  if r == SymId(-1):
    result = false
  else:
    var canAnalyse = true
    let s = c.p[tree.m].syms[r]
    if s.kind == ParamDecl:
      let typ = getType(c.p, tree, n)
      canAnalyse = c.p[typ].kind == SinkTy
    result = canAnalyse and isLastRead(c.p, tree, c.procStart, n, r)

  if c.reportLastUse:
    echo infoToStr(n.info, c.p[tree.m]), " LastUse: ", result

when not defined(nimony):
  proc tr(c: var Context; n: Cursor; e: Expects)

proc trSons(c: var Context; n: Cursor; e: Expects) =
  for ch in sons(dest, tree, n):
    tr c, dest, tree, ch, e

proc isResultUsage(p: Program; n: Cursor): bool {.inline.} =
  if n.kind == SymUse:
    let x = accessModuleSym(p, tree, n)
    result = p[x].kind == ResultDecl
  else:
    result = false

proc trReturn(c: var Context; n: Cursor) =
  copyInto dest, n:
    let retVal = n.firstSon
    if isResultUsage(c.p, tree, retVal):
      copyTree dest, tree, retVal
    else:
      tr c, dest, tree, retVal, WantOwner

proc evalLeftHandSide(c: var Context; le: Cursor): TokenBuf =
  result = createTokenBuf(10)
  if le.kind == Symbol:
    # simple enough:
    copyTree result, tree, le
  else:
    let typ = getType(c.p, tree, le)
    let info = le.info
    let tmp = declareSym(c.p[dest.m], VarDecl, c.p[dest.m].strings.getOrIncl("temp"))
    let d = takePos(dest)
    copyIntoKind dest, VarDecl, info:
      addSymDef dest, tmp, info
      dest.addEmpty2 info # export marker, pragma
      copyIntoKind dest, PtrTy, info: # type
        copyTreeX dest, c.p[typ.m], typ.t, c.p
      copyIntoKind dest, HiddenAddr, info:
        #copyTree dest, tree, le
        tr c, dest, tree, le, DontCare
    let d2 = takePos(c.p[c.localTypes])
    copyTree c.p[c.localTypes], dest, d
    setDeclPosRaw c.p, c.p[dest.m].syms[tmp], FullTypeId(m: c.localTypes, t: d2)

    copyIntoKind result, HiddenDeref, info:
      copyIntoSymUse result, tmp, info

proc callDestroy(c: var Context; destroyProc: SymId; t: TokenBuf; arg: Cursor) =
  let info = t[arg].info
  copyIntoKind dest, Call, info:
    copyIntoSymUse c.p, dest, destroyProc, info
    copyTree dest, t, arg

proc callDestroy(c: var Context; destroyProc: SymId; arg: SymId; info: PackedLineInfo) =
  copyIntoKind dest, Call, info:
    copyIntoSymUse c.p, dest, destroyProc, info
    copyIntoSymUse dest, arg, info

proc tempOfTrArg(c: var Context; n: Cursor; typ: FullTypeId): SymId =
  let info = n.info
  result = declareSym(c.p[dest.m], CursorDecl, c.p[dest.m].strings.getOrIncl("temp"))
  let d = takePos(dest)
  copyIntoKind dest, VarDecl, info:
    addSymDef dest, result, info
    dest.addEmpty2 info # export marker, pragma
    copyTreeX dest, c.p[typ.m], typ.t, c.p
    tr c, dest, tree, n, WillBeOwned
  let d2 = takePos(c.p[c.localTypes])
  copyTree c.p[c.localTypes], dest, d
  setDeclPosRaw c.p, c.p[dest.m].syms[result], FullTypeId(m: c.localTypes, t: d2)

proc callDup(c: var Context; arg: Cursor) =
  let typ = getType(c.p, t, arg)
  let info = t[arg].info
  let hookProc = getHook(c.lifter[], attachedDup, typ, info)
  if hookProc.s != SymId(-1) and t[arg].kind != StrLit:
    copyIntoKind dest, Call, info:
      copyIntoSymUse c.p, dest, hookProc, info
      tr c, dest, t, arg, WillBeOwned
  else:
    tr c, dest, t, arg, WillBeOwned

proc callWasMoved(c: var Context; arg: Cursor) =
  let typ = getType(c.p, t, arg)
  let info = t[arg].info
  let hookProc = getHook(c.lifter[], attachedWasMoved, typ, info)
  if hookProc.s != SymId(-1):
    copyIntoKind dest, Call, info:
      copyIntoSymUse c.p, dest, hookProc, info
      copyIntoKind dest, HiddenAddr, info:
        copyTree dest, t, arg

proc trAsgn(c: var Context; n: Cursor) =
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
  ]#
  let (le, ri) = sons2(tree, n)
  let riType = getType(c.p, tree, ri)
  let destructor = getDestructor(c.lifter[], riType, n.info)
  if destructor.s == SymId(-1):
    # the type has no destructor, there is nothing interesting to do:
    trSons c, dest, tree, n, DontCare

  else:
    let isNotFirstAsgn = n.kind == Asgn
    let lhs = evalLeftHandSide(c, dest, tree, le)
    if constructsValue(c.p, tree, ri):
      # `x = f()` is turned into `=destroy(x); x =bitcopy f()`.
      if isNotFirstAsgn:
        callDestroy(c, dest, destructor, lhs, StartPos)
      copyInto dest, n:
        copyTree dest, lhs, StartPos
        tr c, dest, tree, ri, WillBeOwned
    elif isLastRead(c, tree, ri):
      if isNotFirstAsgn and potentialSelfAsgn(c.p, tree, le, ri):
        # `let tmp = y; =wasMoved(y); =destroy(x); x =bitcopy tmp`
        let tmp = tempOfTrArg(c, dest, tree, ri, riType)
        callWasMoved c, dest, tree, ri
        callDestroy(c, dest, destructor, lhs, StartPos)
        copyInto dest, n:
          #tr c, dest, lhs, StartPos, DontCare
          # XXX Fixme
          copyTree dest, lhs, StartPos
          copyIntoSymUse dest, tmp, ri.info
      else:
        if isNotFirstAsgn:
          callDestroy(c, dest, destructor, lhs, StartPos)
        copyInto dest, n:
          copyTree dest, lhs, StartPos
          tr c, dest, tree, ri, WillBeOwned
        callWasMoved c, dest, tree, ri
    else:
      if isNotFirstAsgn and potentialSelfAsgn(c.p, tree, le, ri):
        # `let tmp = x; x =bitcopy =dup(y); =destroy(tmp)`
        let tmp = tempOfTrArg(c, dest, lhs, StartPos, riType)
        copyInto dest, n:
          tr c, dest, lhs, StartPos, DontCare
          callDup c, dest, tree, ri
        callDestroy(c, dest, destructor, tmp, tree[le].info)
      else:
        if isNotFirstAsgn:
          callDestroy(c, dest, destructor, lhs, StartPos)
        copyInto dest, n:
          #tr c, dest, lhs, StartPos, DontCare
          # XXX Fixme
          copyTree dest, lhs, StartPos
          #callDup c, dest, tree, ri
          tr c, dest, tree, ri, WantOwner

proc trExplicitDestroy(c: var Context; n: Cursor) =
  let typ = getType(c.p, tree, n.firstSon)
  let info = n.info
  let destructor = getDestructor(c.lifter[], typ, info)
  if destructor.s == SymId(-1):
    # the type has no destructor, there is nothing interesting to do:
    dest.addEmpty info
  else:
    copyIntoKind dest, Call, info:
      copyIntoSymUse c.p, dest, destructor, info
      tr c, dest, tree, n.firstSon, DontCare

proc trExplicitDup(c: var Context; n: Cursor; e: Expects) =
  let typ = getType(c.p, tree, n)
  let info = n.info
  let hookProc = getHook(c.lifter[], attachedDup, typ, info)
  if hookProc.s != SymId(-1):
    copyIntoKind dest, Call, info:
      copyIntoSymUse c.p, dest, hookProc, info
      tr c, dest, tree, n.firstSon, DontCare
  else:
    let e2 = if e == WillBeOwned: WantOwner else: e
    tr c, dest, tree, n.firstSon, e2

proc trOnlyEssentials(c: var Context; n: Cursor) =
  if isAtom(tree, n):
    copyTree dest, tree, n
  else:
    case n.kind
    of EqDestroy: trExplicitDestroy c, dest, tree, n
    of EqDup: trExplicitDup c, dest, tree, n, DontCare
    else:
      for ch in sons(dest, tree, n): trOnlyEssentials c, dest, tree, ch

proc trProcDecl(c: var Context; n: Cursor) =
  let r = asRoutine(tree, n)
  var c2 = Context(p: c.p, lifter: c.lifter, procStart: r.body, localTypes: c.localTypes)
  c2.reportLastUse = compilerShouldReport(c.p, tree, r.pragmas, "lastUse")

  copyInto(dest, n):
    copyTree dest, tree, r.name
    copyTree dest, tree, r.ex
    copyTree dest, tree, r.pat
    copyTree dest, tree, r.generics
    copyTree dest, tree, r.params
    copyTree dest, tree, r.pragmas
    copyTree dest, tree, r.exc
    if r.body.kind == StmtList and r.generics.kind != GenericParams:
      if hasBuiltinPragma(c.p, tree, r.pragmas, "nodestroy"):
        trOnlyEssentials c2, dest, tree, r.body
      else:
        tr c2, dest, tree, r.body, DontCare
    else:
      copyTree dest, tree, r.body

proc hasDestructor(c: Context; typ: Cursor): bool {.inline.} =
  not isTrivial(c.lifter[], FullTypeId(t: typ, m: tree.id))

proc hasDestructor(c: Context; typ: FullTypeId): bool {.inline.} =
  not isTrivial(c.lifter[], typ)

type
  OwningTemp = object
    ex, st, vr: PatchPos
    s: SymId
    info: PackedLineInfo

template owningTempDefault(): OwningTemp =
  OwningTemp(ex: PatchPos(-1), st: PatchPos(-1), vr: PatchPos(-1), s: SymId(-1), info: UnknownLineInfo)

proc bindToTemp(c: var Context; typ: FullTypeId; info: PackedLineInfo; kind = VarDecl): OwningTemp =
  let s = declareSym(c.p[dest.m], kind, c.p[dest.m].strings.getOrIncl("temp"))

  let ex = prepare(dest, StmtListExpr, info)
  copyTreeX dest, c.p[typ.m], typ.t, c.p
  let st = prepare(dest, StmtList, info)
  let vr = prepare(dest, VarDecl, info)
  addSymDef dest, s, info
  dest.addEmpty2 info # export marker, pragmas
  copyTreeX dest, c.p[typ.m], typ.t, c.p # type
  #  copyTree dest, tree, ex # value
  # value is filled in by the caller!
  result = OwningTemp(ex: ex, st: st, vr: vr, s: s, info: info)

proc finishOwningTemp(dest: var Tree; ow: OwningTemp) =
  if ow.s != SymId(-1):
    dest.patch ow.vr  # finish the VarDecl
    dest.patch ow.st  # finish the StmtList
    dest.copyIntoSymUse ow.s, ow.info
    dest.patch ow.ex  # finish the StmtListExpr

proc trCall(c: var Context; n: Cursor; e: Expects) =
  var ow = owningTempDefault()
  let retType = getType(c.p, tree, n)
  if hasDestructor(c, retType) and e == WantNonOwner:
    ow = bindToTemp(c, dest, retType, n.info)

  var fnType = getType(c.p, tree, n.firstSon)
  fnType = skipGenericInsts(c.p, fnType, {SkipNilTy, SkipObjectInstantiations})
  assert c.p[fnType].kind in ProcTyNodes
  var paramIter = initSonsIter(c.p, ithSon(c.p, fnType, routineParamsPos))
  var i = 0
  for ch in sons(dest, tree, n):
    var e2 = WantNonOwner
    if hasCurrent(paramIter):
      if i > 0 and c.p[ithSon(c.p, paramIter.current, localTypePos)].kind == SinkTy: e2 = WantOwner
      next paramIter, c.p
    tr c, dest, tree, ch, e2
    inc i
  finishOwningTemp dest, ow

proc trRawConstructor(c: var Context; n: Cursor; e: Expects) =
  # Idioms like `echo ["ab", myvar, "xyz"]` are important to translate well.
  let e2 = if e == WillBeOwned: WantOwner else: e
  for ch in sons(dest, tree, n):
    tr c, dest, tree, ch, e2

proc trConstructor(c: var Context; typ, ex: Cursor; e: Expects) =
  # Idioms like `echo ["ab", myvar, "xyz"]` are important to translate well.
  let e2 = if e == WillBeOwned: WantOwner else: e
  copyIntoKind dest, TypedExpr, ex.info:
    copyTree dest, tree, typ
    for ch in sons(dest, tree, ex):
      tr c, dest, tree, ch, e2
  when false:
    var ow = owningTempDefault()
    if hasDestructor(c, tree, typ) and e == WantNonOwner:
      ow = bindToTemp(c, dest, FullTypeId(m: tree.id, t: typ), ex.info)
    copyIntoKind dest, TypedExpr, ex.info:
      copyTree dest, tree, typ
      for ch in sons(dest, tree, ex):
        tr c, dest, tree, ch, WantOwner
    finishOwningTemp dest, ow

proc trConvExpr(c: var Context; n: Cursor; e: Expects) =
  let (typ, ex) = sons2(tree, n)
  copyInto dest, n:
    copyTree dest, tree, typ
    tr c, dest, tree, ex, e

proc trObjConstr(c: var Context; n: Cursor; e: Expects) =
  var ow = owningTempDefault()
  let typ = n.firstSon
  if hasDestructor(c, tree, typ) and e == WantNonOwner:
    ow = bindToTemp(c, dest, FullTypeId(m: tree.id, t: typ), n.info)
  copyIntoKind dest, ObjConstr, n.info:
    copyTree dest, tree, typ
    for ch in sonsFrom1(tree, n):
      let (first, second) = sons2(tree, ch)
      copyIntoKind dest, ch.kind, ch.info:
        copyTree dest, tree, first
        tr c, dest, tree, second, WantOwner
  finishOwningTemp dest, ow

proc genLastRead(c: var Context; n: Cursor; typ: FullTypeId) =
  let info = n.info
  # translate it to: `(var tmp = location; wasMoved(location); tmp)`
  var ow = bindToTemp(c, dest, typ, info, CursorDecl)
  copyTree dest, tree, n

  dest.patch ow.vr # finish the VarDecl

  let hookProc = getHook(c.lifter[], attachedWasMoved, typ, info)
  if hookProc.s != SymId(-1):
    copyIntoKind dest, Call, info:
      copyIntoSymUse c.p, dest, hookProc, info
      copyIntoKind dest, HiddenAddr, info:
        copyTree dest, tree, n

  dest.patch ow.st # finish the StmtList
  dest.copyIntoSymUse ow.s, ow.info
  dest.patch ow.ex # finish the StmtListExpr

proc trLocation(c: var Context; n: Cursor; e: Expects) =
  # `x` does not own its value as it can be read multiple times.
  let typ = getType(c.p, tree, n)
  if e == WantOwner and hasDestructor(c, typ):
    if isLastRead(c, tree, n):
      genLastRead(c, dest, tree, n, typ)
    else:
      let info = n.info
      # translate `x` to `=dup(x)`:
      let hookProc = getHook(c.lifter[], attachedDup, typ, info)
      if hookProc.s != SymId(-1):
        copyIntoKind dest, Call, info:
          copyIntoSymUse c.p, dest, hookProc, info
          if isAtom(tree, n):
            copyTree dest, tree, n
          else:
            trSons c, dest, tree, n, DontCare
      elif isAtom(tree, n):
        copyTree dest, tree, n
      else:
        trSons c, dest, tree, n, DontCare
  elif isAtom(tree, n):
    copyTree dest, tree, n
  else:
    trSons c, dest, tree, n, DontCare

proc trLocal(c: var Context; n: Cursor) =
  let r = asLocal(tree, n)
  var wasMovedArg = noPos
  copyInto(dest, n):
    copyTree dest, tree, r.name
    copyTree dest, tree, r.ex
    copyTree dest, tree, r.pragmas
    copyTree dest, tree, r.typ

    let localType = getType(c.p, tree, r.name)
    let destructor = getDestructor(c.lifter[], localType, n.info)
    if destructor.s != SymId(-1):
      if constructsValue(c.p, tree, r.value):
        tr c, dest, tree, r.value, WillBeOwned
      elif isLastRead(c, tree, r.value):
        tr c, dest, tree, r.value, WillBeOwned
        wasMovedArg = r.value
      else:
        #var v = createTree(c.p, c.thisModule)
        #tr c, c.p[v], tree, val, WillBeOwned
        #callDup c, dest, c.p[v], StartPos
        #enforceFreeTree c.p, v
        callDup c, dest, tree, r.value
    else:
      tr c, dest, tree, r.value, WillBeOwned
  if wasMovedArg != noPos:
    callWasMoved c, dest, tree, wasMovedArg

proc trStmtListExpr(c: var Context; n: Cursor; e: Expects) =
  let (t, s, x) = sons3(tree, n)
  copyInto dest, n:
    copyTree dest, tree, t
    tr(c, dest, tree, s, WantNonOwner)
    tr(c, dest, tree, x, e)

proc trEnsureMove(c: var Context; n: Cursor; e: Expects) =
  let typ = getType(c.p, tree, n)
  if isLastRead(c, tree, n.firstSon):
    if e == WantOwner and hasDestructor(c, typ):
      copyInto dest, n:
        genLastRead(c, dest, tree, n, typ)
    else:
      copyInto dest, n:
        tr c, dest, tree, n.firstSon, e
  elif constructsValue(c.p, tree, n.firstSon):
    # we allow rather silly code like `ensureMove(234)`.
    # Seems very useful for generic programming as this can come up
    # from template expansions:
    copyInto dest, n:
      tr c, dest, tree, n.firstSon, e
  else:
    produceError dest, tree, n, c.p[tree.m], ["not the last usage of: $1"]

proc tr(c: var Context; n: Cursor; e: Expects) =
  case n.kind
  of Call:
    trCall c, dest, tree, n, e
  of TypedExpr:
    let (typ, ex) = sons2(tree, n)
    if constructsValue(c.p, tree, n):
      trConstructor c, dest, tree, typ, ex, e
    else:
      trSons c, dest, tree, n, DontCare
  of ConvExpr, CastExpr:
    trConvExpr c, dest, tree, n, e
  of ObjConstr:
    trObjConstr c, dest, tree, n, e
  of SymUse, ModuleSymUse, FieldAccess, VarFieldAccess, TupleFieldAccess, PtrFieldAccess,
     PtrVarFieldAccess, RefFieldAccess, RefVarFieldAccess, ArrayAt, ArrayPtrAt:
    trLocation c, dest, tree, n, e
  of ReturnStmt:
    trReturn(c, dest, tree, n)
  of Asgn, FirstAsgn:
    trAsgn c, dest, tree, n
  of VarDecl, LetDecl, ConstDecl, ResultDecl:
    trLocal c, dest, tree, n
  of DeclarativeNodes, AtomsExceptSymUse, Pragmas, TemplateDecl, IteratorDecl,
     UsingStmt, CommentStmt, BindStmt, MixinStmt, ContinueStmt, BreakStmt:
    copyTree dest, tree, n
  of ProcDecl, FuncDecl, MacroDecl, MethodDecl, ConverterDecl:
    trProcDecl c, dest, tree, n
  of Par:
    trSons(c, dest, tree, n, e)
  of StmtListExpr:
    trStmtListExpr c, dest, tree, n, e
  of TupleConstr, BracketConstr:
    trRawConstructor c, dest, tree, n, e
  of EqDup:
    trExplicitDup c, dest, tree, n, e
  of EqDestroy:
    trExplicitDestroy c, dest, tree, n
  of EnsureMove:
    trEnsureMove c, dest, tree, n, e
  else:
    for ch in sons(dest, tree, n):
      tr(c, dest, tree, ch, WantNonOwner)

proc injectDups*(p: Program; t: TreeId; lifter: ref LiftingCtx): TreeId =
  let thisModule = p[t].m
  var c = Context(p: p, lifter: lifter, procStart: StartPos, localTypes: createTokenBuf(4))
  assert c.localTypes.int > 0

  result = createTree(p, thisModule)
  p[result].flags.incl dontTouch
  tr(c, p[result], p[t], StartPos, WantNonOwner)
  genMissingHooks lifter[], p[result]
  patch p[result], PatchPos(0)
  p[result].flags.excl dontTouch
  enforceFreeTree p, c.localTypes
