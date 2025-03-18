#
#
#           Hexer Compiler
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
import nifindexes, symparser, treemangler, lifter, mover, hexer_context, typekeys
import ".." / nimony / [nimony_model, programs, decls, typenav, renderer, reporters, builtintypes]

type
  Context = object
    dest: TokenBuf
    lifter: ref LiftingCtx
    reportLastUse: bool
    typeCache: TypeCache
    tmpCounter: int
    resultSym: SymId
    source: ptr TokenBuf

  Expects = enum
    DontCare,
    WillBeOwned,
    WantNonOwner,
    WantOwner

# -------------- helpers ----------------------------------------

proc isLastRead(c: var Context; n: Cursor): bool =
  # This is a hack to make sure that the type cache is populated for the
  # expression we are analysing:
  discard getType(c.typeCache, n)
  var n = n
  while n.exprKind == ExprX:
    inc n
    while n.kind != ParRi and not isLastSon(n): skip n

  if n.exprKind == EmoveX: inc n

  let r = rootOf(n)
  result = false
  if r != NoSymId:
    var canAnalyse = false
    let v = c.typeCache.getLocalInfo(r)
    if v.kind == ParamY:
      canAnalyse = v.typ.typeKind == SinkT
    elif v.kind in {VarY, LetY}:
      # CursorY omitted here on purpose as we cannot steal ownership from a cursor
      # as it doesn't have any.
      canAnalyse = true
    else:
      assert v.kind != NoSym
      canAnalyse = false
    if canAnalyse:
      var otherUsage = NoLineInfo
      result = isLastUse(n, c.source[], otherUsage)

const
  ConstructingExprs = CallKinds + {OconstrX, NewobjX, AconstrX, TupX, NewrefX}

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
  result = n.exprKind in ConstructingExprs or n.kind in {IntLit, FloatLit, StringLit, CharLit}

proc lvalueRoot(n: Cursor; hdrefs: var bool): SymId =
  var n = n
  while true:
    case n.exprKind
    of DotX, TupatX, AtX, ArrAtX: inc n
    of HderefX:
      hdrefs = true
      inc n
    else: break
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
    var destHdrefs = false
    var srcHdrefs = false
    let d = lvalueRoot(dest, destHdrefs)
    let s = lvalueRoot(src, srcHdrefs)
    if d != NoSymId or s != NoSymId:
      # one of the expressions was analysable
      if destHdrefs and srcHdrefs:
        # two pointer derefs? can alias:
        result = true
      elif d == s:
        # see if we can distinguish between `x.fieldA` and `x.fieldB` which
        # cannot alias. We do know here that at least one expressions is free of
        # pointer derefs, so we can simply use `sameValues` here.
        result = sameTreesIgnoreArrayIndexes(dest, src)
      else:
        # different roots while we know that at least one expression has
        # no harmful pointer deref:
        result = false

# -----------------------------------------------------------

when not defined(nimony):
  proc tr(c: var Context; n: var Cursor; e: Expects)

proc trSons(c: var Context; n: var Cursor; e: Expects) =
  assert n.kind == ParLe
  c.dest.add n
  inc n
  while n.kind != ParRi:
    tr(c, n, e)
  takeParRi c.dest, n

proc isResultUsage(c: Context; n: Cursor): bool {.inline.} =
  result = false
  if n.kind == Symbol:
    result = n.symId == c.resultSym

proc trReturn(c: var Context; n: var Cursor) =
  copyInto c.dest, n:
    if isResultUsage(c, n):
      takeTree c.dest, n
    else:
      tr c, n, WantOwner

proc evalLeftHandSide(c: var Context; le: var Cursor): TokenBuf =
  result = createTokenBuf(10)
  if le.kind == Symbol or (le.exprKind in {DerefX, HderefX} and le.firstSon.kind == Symbol):
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
    c.typeCache.registerLocalPtrOf(tmp, VarY, typ)

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
  copyIntoKind c.dest, CursorS, info:
    addSymDef c.dest, result, info
    c.dest.addEmpty2 info # export marker, pragma
    copyTree c.dest, typ
    tr c, n, WillBeOwned
  c.typeCache.registerLocal(result, CursorY, typ)

proc callDup(c: var Context; arg: var Cursor) =
  let typ = getType(c.typeCache, arg)
  if typ.typeKind == NiltT:
    tr c, arg, DontCare
  else:
    let info = arg.info
    let hookProc = getHook(c.lifter[], attachedDup, typ, info)
    if hookProc != NoSymId and arg.kind != StringLit:
      copyIntoKind c.dest, CallS, info:
        copyIntoSymUse c.dest, hookProc, info
        tr c, arg, WillBeOwned
    else:
      tr c, arg, WillBeOwned

proc callWasMoved(c: var Context; arg: Cursor; typ: Cursor) =
  var n = arg
  if n.exprKind == ExprX:
    inc n
    while n.kind != ParRi:
      if isLastSon(n):
        break
      else:
        skip n
  if n.exprKind == EmoveX: inc n

  let info = n.info
  let hookProc = getHook(c.lifter[], attachedWasMoved, typ, info)
  if hookProc != NoSymId:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, hookProc, info
      copyIntoKind c.dest, HaddrX, info:
        copyTree c.dest, n

proc callWasMoved(c: var Context; sym: SymId; info: PackedLineInfo; typ: Cursor) =
  let hookProc = getHook(c.lifter[], attachedWasMoved, typ, info)
  if hookProc != NoSymId:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, hookProc, info
      copyIntoKind c.dest, HaddrX, info:
        copyIntoSymUse c.dest, sym, info

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
  let leType = getType(c.typeCache, le)
  assert leType.typeKind != AutoT, "could not compute type of: " & toString(le, false)

  let destructor = getDestructor(c.lifter[], leType, n.info)
  if destructor == NoSymId:
    # the type has no destructor, there is nothing interesting to do:
    trSons c, n, DontCare

  else:
    #let isNotFirstAsgn = not isResultUsage(c, le) # YYY Adapt this once we have "isFirstAsgn" analysis
    const isNotFirstAsgn = true
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
        let tmp = tempOfTrArg(c, ri, leType)
        callWasMoved c, ri, leType
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
          n = ri
          tr c, n, WillBeOwned
        callWasMoved c, ri, leType
    else:
      # XXX We should really prefer to simply call `=copy(x, y)` here.
      if isNotFirstAsgn and potentialSelfAsgn(le, ri):
        # `let tmp = x; x =bitcopy =dup(y); =destroy(tmp)`
        let tmp = tempOfTrArg(c, le, leType)
        copyInto c.dest, n:
          var lhsAsCursor = cursorAt(lhs, 0)
          tr c, lhsAsCursor, DontCare
          n = ri
          callDup c, n
        callDestroy(c, destructor, tmp, le.info)
      else:
        if isNotFirstAsgn:
          callDestroy(c, destructor, lhs)
        copyInto c.dest, n:
          var lhsAsCursor = cursorAt(lhs, 0)
          tr c, lhsAsCursor, DontCare
          n = ri
          callDup c, n

proc getHookType(c: var Context; n: Cursor): Cursor =
  result = skipModifier(getType(c.typeCache, n.firstSon))

proc trExplicitDestroy(c: var Context; n: var Cursor) =
  let typ = getHookType(c, n)
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
  let typ = getHookType(c, n)
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

proc trExplicitCopy(c: var Context; n: var Cursor; op: AttachedOp) =
  let typ = getHookType(c, n)
  let info = n.info
  let hookProc = getHook(c.lifter[], op, typ, info)
  if hookProc != NoSymId:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, hookProc, info
      inc n
      while n.kind != ParRi:
        tr c, n, DontCare
      takeParRi c.dest, n
  else:
    c.dest.addParLe AsgnS, info
    inc n
    tr c, n, DontCare
    tr c, n, DontCare
    takeParRi c.dest, n

proc trExplicitWasMoved(c: var Context; n: var Cursor) =
  let typ = getHookType(c, n)
  let info = n.info
  let hookProc = getHook(c.lifter[], attachedWasMoved, typ, info)
  if hookProc != NoSymId:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, hookProc, info
      inc n
      tr c, n, DontCare
  else:
    inc n
    skip n
  skipParRi n

proc trExplicitTrace(c: var Context; n: var Cursor) =
  let typ = getHookType(c, n)
  let info = n.info
  let hookProc = getHook(c.lifter[], attachedTrace, typ, info)
  if hookProc != NoSymId:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, hookProc, info
      inc n
      tr c, n, DontCare
      tr c, n, DontCare
  else:
    inc n
    skip n
    skip n
  skipParRi n

when not defined(nimony):
  proc trProcDecl(c: var Context; n: var Cursor; parentNodestroy = false)

proc trOnlyEssentials(c: var Context; n: var Cursor) =
  var nested = 0
  while true:
    case n.kind
    of Symbol, UIntLit, StringLit, IntLit, FloatLit, CharLit, SymbolDef, UnknownToken, EofToken, DotToken, Ident:
      c.dest.add n
      inc n
    of ParLe:
      case n.exprKind
      of DestroyX:
        trExplicitDestroy c, n
      of DupX:
        trExplicitDup c, n, DontCare
      of CopyX:
        trExplicitCopy c, n, attachedCopy
      of SinkhX:
        trExplicitCopy c, n, attachedSink
      of WasMovedX:
        trExplicitWasMoved c, n
      of TraceX:
        trExplicitTrace c, n
      of NoExpr:
        case n.stmtKind
        of LocalDecls:
          let kind = n.symKind
          c.dest.add n
          inc n
          c.typeCache.takeLocalHeader(c.dest, n, kind)
          inc nested
        of ProcS, FuncS, ConverterS, MethodS, MacroS:
          trProcDecl c, n, parentNodestroy = true
        of ScopeS:
          c.typeCache.openScope()
          c.dest.add n
          inc n
          while n.kind != ParRi:
            trOnlyEssentials c, n
          takeParRi c.dest, n
          c.typeCache.closeScope()
        else:
          c.dest.add n
          inc n
          inc nested
      else:
        c.dest.add n
        inc n
        inc nested
    of ParRi:
      c.dest.add n
      inc n
      dec nested
    if nested == 0: break

proc trProcDecl(c: var Context; n: var Cursor; parentNodestroy = false) =
  c.dest.add n
  let oldResultSym = c.resultSym
  c.resultSym = NoSymId
  var r = takeRoutine(n, SkipFinalParRi)
  let symId = r.name.symId
  if isLocalProcDecl(symId):
    c.typeCache.registerLocal(symId, r.kind, r.params)
  copyTree c.dest, r.name
  copyTree c.dest, r.exported
  copyTree c.dest, r.pattern
  copyTree c.dest, r.typevars
  copyTree c.dest, r.params
  copyTree c.dest, r.retType
  copyTree c.dest, r.pragmas
  copyTree c.dest, r.effects
  if r.body.stmtKind == StmtsS and not isGeneric(r):
    c.typeCache.openScope()
    c.typeCache.registerParams(r.name.symId, r.params)
    if parentNodestroy or hasPragma(r.pragmas, NodestroyP):
      trOnlyEssentials c, r.body
    else:
      tr c, r.body, DontCare
    c.typeCache.closeScope()
  else:
    copyTree c.dest, r.body
  c.dest.addParRi()
  c.resultSym = oldResultSym

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
  var fnType = skipProcTypeToParams(getType(c.typeCache, n))
  takeTree c.dest, n # skip `fn`
  assert fnType.typeKind == ParamsT
  inc fnType
  while n.kind != ParRi:
    let previousFormalParam = fnType
    let param = takeLocal(fnType, SkipFinalParRi)
    let pk = param.typ.typeKind
    var e2 = WantNonOwner
    if pk == SinkT:
      e2 = WantOwner
    elif pk == VarargsT:
      # do not advance formal parameter:
      fnType = previousFormalParam
    tr c, n, e2
  takeParRi c.dest, n
  finishOwningTemp c.dest, ow

proc trRawConstructor(c: var Context; n: var Cursor; e: Expects) =
  # Idioms like `echo ["ab", myvar, "xyz"]` are important to translate well.
  let e2 = if e == WillBeOwned: WantOwner else: e
  c.dest.add n
  inc n
  while n.kind != ParRi:
    tr c, n, e2
  takeParRi c.dest, n

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
      assert n.substructureKind == KvU
      copyInto c.dest, n:
        takeTree c.dest, n
        tr c, n, WantOwner
  finishOwningTemp c.dest, ow

proc trNewobjFields(c: var Context; n: var Cursor) =
  while n.kind != ParRi:
    if n.substructureKind == KvU:
      copyInto c.dest, n:
        takeTree c.dest, n # keep field name
        tr(c, n, WantOwner)
    else:
      tr(c, n, WantOwner)
  inc n # skip ParRi

proc trNewobj(c: var Context; n: var Cursor; e: Expects; kind: ExprKind) =
  let info = n.info
  inc n
  let refType = n
  assert refType.typeKind == RefT

  var ow = bindToTemp(c, refType, info, if e == WantNonOwner: VarS else: CursorS)

  let baseType = refType.firstSon
  var refTypeCopy = refType
  let typeKey = takeMangle refTypeCopy
  let typeSym = pool.syms.getOrIncl(typeKey & GeneratedTypeSuffix)

  copyIntoKind c.dest, CastX, info:
    c.dest.addSubtree refType
    copyIntoKind c.dest, CallX, info:
      c.dest.add symToken(pool.syms.getOrIncl("allocFixed.0." & SystemModuleSuffix), info)
      copyIntoKind c.dest, SizeofX, info:
        c.dest.add symToken(typeSym, info)
  c.dest.addParRi() # finish temp declaration

  copyIntoKind c.dest, AsgnS, info:
    copyIntoKind c.dest, DerefX, info:
      c.dest.add symToken(ow.s, info)
    copyIntoKind c.dest, OconstrX, info:
      c.dest.add symToken(typeSym, info)
      copyIntoKind c.dest, KvU, info:
        let rcField = pool.syms.getOrIncl(RcField)
        c.dest.add symToken(rcField, info)
        c.dest.addIntLit(0, info)
      copyIntoKind c.dest, KvU, info:
        let dataField = pool.syms.getOrIncl(DataField)
        c.dest.add symToken(dataField, info)
        if kind == NewobjX:
          copyIntoKind c.dest, OconstrX, info:
            c.dest.addSubtree baseType
            skip n # skip old base type (which is a ref)
            trNewobjFields(c, n)
        else:
          skip n # skip type
          tr c, n, WantOwner # process default(T) call

  c.dest.addParRi()  # finish the StmtsS
  c.dest.copyIntoSymUse ow.s, ow.info
  c.dest.addParRi()  # finish the StmtListExpr

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

proc trLocation(c: var Context; n: var Cursor; e: Expects) =
  # `x` does not own its value as it can be read multiple times.
  let typ = getType(c.typeCache, n)
  if e == WantOwner and hasDestructor(c, typ):
    if isLastRead(c, n):
      genLastRead(c, n, typ)
    else:
      let info = n.info
      # translate `x` to `=dup(x)`:
      let hookProc = getHook(c.lifter[], attachedDup, typ, info)
      if hookProc != NoSymId:
        copyIntoKind c.dest, CallS, info:
          copyIntoSymUse c.dest, hookProc, info
          if isAtom(n):
            takeTree c.dest, n
          else:
            trSons c, n, DontCare
      elif isAtom(n):
        takeTree c.dest, n
      else:
        trSons c, n, DontCare
  elif isAtom(n):
    takeTree c.dest, n
  else:
    trSons c, n, DontCare

proc trValue(c: var Context; n: Cursor; e: Expects) =
  var n = n
  tr c, n, e

proc trLocal(c: var Context; n: var Cursor; k: StmtKind) =
  let kind = n.symKind
  c.dest.add n
  let r = takeLocal(n, SkipFinalParRi)
  if k == ResultS and r.name.kind == SymbolDef:
    c.resultSym = r.name.symId
  copyTree c.dest, r.name
  copyTree c.dest, r.exported
  copyTree c.dest, r.pragmas
  copyTree c.dest, r.typ
  c.typeCache.registerLocal(r.name.symId, kind, r.typ)

  if r.val.kind == DotToken:
    copyTree c.dest, r.val
    c.dest.addParRi()
    callWasMoved c, r.name.symId, r.name.info, r.typ
  else:
    let destructor = getDestructor(c.lifter[], r.typ, n.info)
    if destructor != NoSymId:
      if k == CursorS:
        trValue c, r.val, DontCare
        c.dest.addParRi()
      elif constructsValue(r.val):
        trValue c, r.val, WillBeOwned
        c.dest.addParRi()

      elif isLastRead(c, r.val):
        trValue c, r.val, WillBeOwned
        c.dest.addParRi()
        callWasMoved c, r.val, r.typ
      else:
        var rval = r.val
        callDup c, rval
        c.dest.addParRi()
    else:
      trValue c, r.val, WillBeOwned
      c.dest.addParRi()

proc trStmtListExpr(c: var Context; n: var Cursor; e: Expects) =
  c.dest.add n
  inc n
  while n.kind != ParRi:
    if isLastSon(n):
      tr(c, n, e)
    else:
      tr(c, n, WantNonOwner)
  takeParRi c.dest, n

proc trEnsureMove(c: var Context; n: var Cursor; e: Expects) =
  let typ = getType(c.typeCache, n)
  let arg = n.firstSon
  let info = n.info
  if constructsValue(arg):
    # we allow rather silly code like `ensureMove(234)`.
    # Seems very useful for generic programming as this can come up
    # from template expansions:
    inc n
    tr c, n, e
    skipParRi n
  elif isLastRead(c, arg):
    if e == WantOwner and hasDestructor(c, typ):
      inc n
      genLastRead(c, n, typ)
      skipParRi n
    else:
      inc n
      tr c, n, e
      skipParRi n
  else:
    let m = "not the last usage of: " & asNimCode(n)
    c.dest.buildTree ErrT, info:
      c.dest.addSubtree n
      c.dest.add strToken(pool.strings.getOrIncl(m), info)
    skip n

proc trDeref(c: var Context; n: var Cursor) =
  let info = n.info
  inc n
  let typ = getType(c.typeCache, n, {SkipAliases})
  let isRef = not cursorIsNil(typ) and typ.typeKind == RefT
  if isRef:
    c.dest.addParLe DotX, info
  c.dest.addParLe DerefX, info
  tr c, n, WantNonOwner
  if isRef:
    c.dest.addParRi()
    let dataField = pool.syms.getOrIncl(DataField)
    c.dest.add symToken(dataField, info)
    c.dest.addIntLit(0, info) # inheritance
  takeParRi c.dest, n

proc tr(c: var Context; n: var Cursor; e: Expects) =
  if n.kind == Symbol:
    trLocation c, n, e
  elif n.kind in {Ident, SymbolDef, IntLit, UIntLit, CharLit, StringLit, FloatLit, DotToken} or isDeclarative(n):
    takeTree c.dest, n
  else:
    case n.exprKind
    of CallKinds:
      trCall c, n, e
    of DestroyX:
      trExplicitDestroy c, n
    of DupX:
      trExplicitDup c, n, e
    of CopyX:
      trExplicitCopy c, n, attachedCopy
    of WasMovedX:
      trExplicitWasMoved c, n
    of SinkhX:
      trExplicitCopy c, n, attachedSink
    of TraceX:
      trExplicitTrace c, n
    of ConvKinds, SufX:
      trConvExpr c, n, e
    of OconstrX:
      trObjConstr c, n, e
    of NewobjX:
      trNewobj c, n, e, NewobjX
    of NewrefX:
      trNewobj c, n, e, NewrefX
    of DotX, AtX, ArrAtX, PatX, TupatX:
      trLocation c, n, e
    of ParX:
      trSons c, n, e
    of ExprX:
      trStmtListExpr c, n, e
    of EmoveX:
      trEnsureMove c, n, e
    of AconstrX, TupConstrX:
      trRawConstructor c, n, e
    of NilX, FalseX, TrueX, AndX, OrX, NotX, NegX, SizeofX, SetConstrX,
       OchoiceX, CchoiceX, XorX,
       AddX, SubX, MulX, DivX, ModX, ShrX, ShlX, AshrX, BitandX, BitorX, BitxorX, BitnotX,
       PlusSetX, MinusSetX, MulSetX, XorSetX, EqSetX, LeSetX, LtSetX, InSetX, CardX,
       EqX, NeqX, LeX, LtX, InfX, NegInfX, NanX, CompilesX, DeclaredX,
       DefinedX, HighX, LowX, TypeofX, UnpackX, EnumtostrX, IsmainmoduleX, QuotedX,
       AddrX, HaddrX, AlignofX, OffsetofX, ErrX:
      trSons c, n, WantNonOwner
    of DerefX, HderefX:
      trDeref c, n
    of DdotX:
      raiseAssert "nodekind should have been eliminated in desugar.nim"
    of DefaultobjX, DefaulttupX, BracketX, CurlyX, TupX:
      raiseAssert "nodekind should have been eliminated in sem.nim"
    of PragmaxX, CurlyatX, TabconstrX, DoX:
      trSons c, n, e
    of NoExpr:
      let k = n.stmtKind
      case k
      of RetS:
        trReturn c, n
      of AsgnS:
        trAsgn c, n
      of LocalDecls:
        trLocal c, n, k
      of ProcS, FuncS, ConverterS, MethodS, MacroS:
        trProcDecl c, n
      of ScopeS:
        c.typeCache.openScope()
        trSons c, n, WantNonOwner
        c.typeCache.closeScope()
      of BreakS, ContinueS, IteratorS:
        takeTree c.dest, n
      else:
        trSons c, n, WantNonOwner

proc readableHookname(s: string): string =
  result = s
  extractBasename(result)
  if result.len > 2 and result[0] == '=' and result[1] in {'a'..'z'}:
    var i = 2
    while i < result.len and result[i] != '_':
      inc i
    setLen result, i

proc checkForErrorRoutine(r: var Reporter; fn: SymId; info: PackedLineInfo): int =
  let res = tryLoadSym(fn)
  result = 0
  if res.status == LacksNothing:
    let routine = asRoutine(res.decl)
    if routine.kind.isRoutine and hasPragma(routine.pragmas, ErrorP):
      let fnName = readableHookname(pool.syms[fn])
      var m = "'" & fnName & "' is not available"
      var arg = routine.params
      if arg.substructureKind == ParamsU:
        inc arg
        if arg.kind != ParRi:
          let param = asLocal(arg)
          m.add " for type <" & asNimCode(param.typ) & ">"
      r.error infoToStr(info), m
      inc result

proc checkForMoveTypes(c: var Context; n: Cursor): int =
  var nested = 0
  var r = Reporter(verbosity: 2, noColors: not useColors())
  var n = n
  result = 0
  while true:
    case n.kind
    of ParLe:
      inc nested
      let ek = n.exprKind
      if ek in CallKinds:
        let fn = n.firstSon
        if fn.kind == Symbol:
          result += checkForErrorRoutine(r, fn.symId, n.info)
      elif ek == ErrX:
        let info = n.info
        inc n
        skip n
        while n.kind == DotToken: inc n
        if n.kind == StringLit:
          r.error infoToStr(info), pool.strings[n.litId]
          inc result
    of ParRi:
      dec nested
    else:
      discard
    if nested == 0: break
    inc n

proc injectDups*(n: Cursor; source: var TokenBuf; lifter: ref LiftingCtx): TokenBuf =
  var c = Context(lifter: lifter, typeCache: createTypeCache(),
    dest: createTokenBuf(400), source: addr source)
  c.typeCache.openScope()
  var n = n
  tr(c, n, WantNonOwner)
  genMissingHooks lifter[]

  var ndest = beginRead(c.dest)
  let errorCount = checkForMoveTypes(c, ndest)
  endRead(c.dest)
  c.typeCache.closeScope()

  if errorCount > 0:
    quit 1

  result = ensureMove(c.dest)
