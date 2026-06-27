#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
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

import std / [assertions, tables, hashes, sets, syncio]
when defined(nimony):
  {.feature: "lenientnils".}
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / [nifindexes, symparser, treemangler]
import lifter, mover, hexer_context, passes
import ".." / nimony / [nimony_model, programs, decls, typenav, renderer, reporters, builtintypes, typekeys]
include ".." / nimony / nif_annotations

type
  ContextFlag = enum
    ReportLastUse, CanRaise

  Context = object
    dest: TokenBuf
    lifter: ref LiftingCtx
    flags: set[ContextFlag]
    typeCache: TypeCache
    tmpCounter: int
    resultSym: SymId
    source: ptr TokenBuf
    moduleSuffix: string
    mover: MoverContext

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
    while n.hasMore and not isLastSon(n): skip n

  if n.exprKind == EmoveX: inc n

  let r = rootOf(n, CannotFollowDerefs)
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
      # Globals (GvarY/TvarY/GletY/TletY), cursors, params (non-sink), etc.
      # `getLocalInfo` returns `NoSym` for globals because they live outside
      # any local scope; treat them all as unanalyzable so the caller emits a
      # copy rather than a move.
      canAnalyse = false
    if canAnalyse:
      var otherUsage = NoLineInfo
      result = isLastUse(n, c.source[], otherUsage, c.mover)
      if ReportLastUse in c.flags:
        echo infoToStr(n.info), " LastUse: ", result

const
  ConstructingExprs = CallKinds + {OconstrX, NewobjX, AconstrX, TupconstrX, TupX, NewrefX}

proc constructsValue*(n: Cursor; derefConstructs = true): bool =
  var n = n
  while true:
    case n.exprKind
    of CastX, ConvX, HconvX, DconvX:
      inc n
      skip n
    of DerefX, HderefX:
      if not derefConstructs:
        return false
      inc n
    of BaseobjX:
      inc n
      skip n
      skip n # skip intlit
    of ExprX:
      inc n
      while not isLastSon(n): skip n
    of ErrX, SufX, AtX, DotX, PatX, ParX, AddrX, NilX, InfX,
        NeginfX, NanX, FalseX, TrueX, AndX, OrX, XorX, NotX, NegX,
        SizeofX, AlignofX, OffsetofX, OconstrX, AconstrX, BracketX,
        CurlyX, CurlyatX, OvfX, AddX, SubX, MulX, DivX, ModX,
        ShrX, ShlX, BitandX, BitorX, BitxorX, BitnotX, EqX, NeqX,
        LeX, LtX, CallX, CmdX, CchoiceX, OchoiceX, PragmaxX,
        QuotedX, DdotX, HaddrX, NewrefX, NewobjX, TupX, TupconstrX,
        SetconstrX, TabconstrX, AshrX, CallstrlitX, InfixX, PrefixX,
        HcallX, CompilesX, DeclaredX, DefinedX, AstToStrX, BindSymX, BindSymNameX,
        InstanceofX, ProccallX, HighX, LowX, TypeofX, UnpackX,
        FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX,
        DefaultobjX, DefaulttupX, DefaultdistinctX, DelayX, Delay0X,
        SuspendX, DoX, ArratX, TupatX, PlussetX, MinussetX, MulsetX,
        XorsetX, EqsetX, LesetX, LtsetX, InsetX, CardX, EmoveX,
        DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX,
        InternalTypeNameX, InternalFieldPairsX, FailedX, IsX, EnvpX,
        KvX, NoExpr: break
  result = n.exprKind in ConstructingExprs or n.kind in {IntLit, FloatLit, StringLit, CharLit}

proc lvalueRoot(n: Cursor; hdrefs: var bool): SymId =
  var n = n
  while true:
    case n.exprKind
    of DotX, TupatX, AtX, ArratX: inc n
    of HderefX:
      hdrefs = true
      inc n
    of ErrX, SufX, DerefX, PatX, ParX, AddrX, NilX, InfX, NeginfX,
        NanX, FalseX, TrueX, AndX, OrX, XorX, NotX, NegX, SizeofX,
        AlignofX, OffsetofX, OconstrX, AconstrX, BracketX, CurlyX,
        CurlyatX, OvfX, AddX, SubX, MulX, DivX, ModX, ShrX, ShlX,
        BitandX, BitorX, BitxorX, BitnotX, EqX, NeqX, LeX, LtX,
        CastX, ConvX, CallX, CmdX, CchoiceX, OchoiceX, PragmaxX,
        QuotedX, DdotX, HaddrX, NewrefX, NewobjX, TupX, TupconstrX,
        SetconstrX, TabconstrX, AshrX, BaseobjX, HconvX, DconvX,
        CallstrlitX, InfixX, PrefixX, HcallX, CompilesX, DeclaredX,
        DefinedX, AstToStrX, BindSymX, BindSymNameX, InstanceofX, ProccallX, HighX, LowX,
        TypeofX, UnpackX, FieldsX, FieldpairsX, EnumtostrX,
        IsmainmoduleX, DefaultobjX, DefaulttupX, DefaultdistinctX,
        DelayX, Delay0X, SuspendX, ExprX, DoX, PlussetX, MinussetX,
        MulsetX, XorsetX, EqsetX, LesetX, LtsetX, InsetX, CardX,
        EmoveX, DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX,
        InternalTypeNameX, InternalFieldPairsX, FailedX, IsX, EnvpX,
        KvX, NoExpr: break
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

proc potentialAliasing(le, ri: Cursor): bool =
  var destHdrefs = false
  let d = lvalueRoot(le, destHdrefs)
  if d == NoSymId:
    result = true # too bad, cannot analyse
  else:
    result = false
    var nested = 0
    var n = ri
    while true:
      case n.kind
      of Symbol:
        if n.symId == d:
          result = true
          break
        inc n
      of ParLe:
        inc nested
        inc n
      of ParRi:
        dec nested
        inc n
      else:
        inc n
      if nested == 0: break

# -----------------------------------------------------------

when not defined(nimony):
  proc tr(c: var Context; n: var Cursor; e: Expects)
    {.ensuresNif: addedAny(c.dest).}

proc trSons(c: var Context; n: var Cursor; e: Expects)
    {.ensuresNif: addedAny(c.dest).} =
  assert n.kind == ParLe
  takeToken c.dest, n
  while n.hasMore:
    tr(c, n, e)
  takeParRi c.dest, n

proc isResultUsage(c: Context; n: Cursor): bool {.inline.} =
  result = false
  if n.kind == Symbol:
    result = n.symId == c.resultSym

proc isSimpleExpression(n: var Cursor): bool =
  ## expressions that can be returned safely
  case n.kind
  of Symbol, UIntLit, StringLit, IntLit, FloatLit, CharLit, DotToken, Ident:
    result = true
    inc n
  of ParLe:
    case n.exprKind
    of FalseX, TrueX, InfX, NeginfX, NanX, NilX, SufX:
      result = true
      skip n
    of CastX, ConvX, HconvX, DconvX:
      result = true
      inc n
      skip n # type
      while n.hasMore:
        if not isSimpleExpression(n): return false
      skipParRi n
    of ExprX:
      inc n
      var inner = n
      skip n
      if n.kind == ParRi:
        result = isSimpleExpression(inner)
        skipParRi n
      else:
        result = false
    of ErrX, AtX, DerefX, DotX, PatX, ParX, AddrX, AndX, OrX, XorX,
        NotX, NegX, SizeofX, AlignofX, OffsetofX, OconstrX, AconstrX,
        BracketX, CurlyX, CurlyatX, OvfX, AddX, SubX, MulX, DivX,
        ModX, ShrX, ShlX, BitandX, BitorX, BitxorX, BitnotX, EqX,
        NeqX, LeX, LtX, CallX, CmdX, CchoiceX, OchoiceX, PragmaxX,
        QuotedX, HderefX, DdotX, HaddrX, NewrefX, NewobjX, TupX,
        TupconstrX, SetconstrX, TabconstrX, AshrX, BaseobjX,
        CallstrlitX, InfixX, PrefixX, HcallX, CompilesX, DeclaredX,
        DefinedX, AstToStrX, BindSymX, BindSymNameX, InstanceofX, ProccallX, HighX, LowX,
        TypeofX, UnpackX, FieldsX, FieldpairsX, EnumtostrX,
        IsmainmoduleX, DefaultobjX, DefaulttupX, DefaultdistinctX,
        DelayX, Delay0X, SuspendX, DoX, ArratX, TupatX, PlussetX,
        MinussetX, MulsetX, XorsetX, EqsetX, LesetX, LtsetX, InsetX,
        CardX, EmoveX, DestroyX, DupX, CopyX, WasmovedX, SinkhX,
        TraceX, InternalTypeNameX, InternalFieldPairsX, FailedX, IsX,
        EnvpX, KvX, NoExpr:
      result = false
      skip n
  of ParRi, SymbolDef, UnknownToken, EofToken:
    result = false
    inc n

proc trReturn(c: var Context; n: var Cursor) =
  let retVal = n.firstSon
  var exp = retVal
  if isResultUsage(c, retVal):
    takeTree c.dest, n
  elif isSimpleExpression(exp) or c.resultSym == NoSymId:
    # simple enough:
    copyInto(c.dest, n):
      tr c, n, WantOwner
  else:
    let info = n.info
    inc n, SkipTag
    c.dest.addParLe StmtsS, info
    c.dest.addParLe AsgnS, info
    c.dest.add symToken(c.resultSym, info)
    tr c, n, WantOwner
    c.dest.addParRi() # end of AsgnS
    c.dest.addParLe(RetS, info)
    c.dest.add symToken(c.resultSym, info)
    c.dest.addParRi() # end of RetS
    c.dest.addParRi() # end of StmtsS
    skipParRi(n) # skip ParRi

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

proc callDestroy(c: var Context; destroyProc: SymId; arg: TokenBuf; typ: Cursor) =
  let info = arg[0].info
  let staticCall = typ.typeKind notin {RefT, PtrT}
  template emitArgs(dest: var TokenBuf) =
    copyIntoSymUse dest, destroyProc, info
    if isMutFirstParam(destroyProc):
      copyIntoKind dest, HaddrX, info:
        copyTree dest, arg
    else:
      copyTree dest, arg
  if staticCall:
    copyIntoKind c.dest, ProccallX, info: emitArgs(c.dest)
  else:
    copyIntoKind c.dest, CallS, info: emitArgs(c.dest)

proc callDestroy(c: var Context; destroyProc: SymId; arg: SymId; info: PackedLineInfo; typ: Cursor) =
  let staticCall = typ.typeKind notin {RefT, PtrT}
  template emitArgs(dest: var TokenBuf) =
    copyIntoSymUse dest, destroyProc, info
    if isMutFirstParam(destroyProc):
      copyIntoKind dest, HaddrX, info:
        copyIntoSymUse dest, arg, info
    else:
      copyIntoSymUse dest, arg, info
  if staticCall:
    copyIntoKind c.dest, ProccallX, info: emitArgs(c.dest)
  else:
    copyIntoKind c.dest, CallS, info: emitArgs(c.dest)

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

proc callDup(c: var Context; arg: var Cursor)
    {.ensuresNif: addedAny(c.dest).} =
  if arg.exprKind == EmoveX:
    # `=dup` on an `ensureMove(x)` is always wrong: the user has asserted
    # this is a move, so calling `=dup` on top would emit a bogus
    # `=dup_T(ensureMove …)` that the destroyer would later pair with
    # one too many `=destroy`. `trEnsureMove` enforces the proof
    # obligation: if the inner expression isn't a constructing rvalue or
    # a provable last-read, it produces an `(err …)` node here.
    tr c, arg, WillBeOwned
    return
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
    while n.hasMore:
      if isLastSon(n):
        break
      else:
        skip n
  if n.exprKind == EmoveX: inc n
  # `=wasMoved` mutates its argument in place, so we have to take its address.
  # Peel any conversion wrappers — `(hconv T x)` / `(conv T x)` / `(cast T x)`
  # — so we land on the underlying lvalue. The destination type that was
  # passed in is no longer the right one to look up the hook with, since the
  # location we are zeroing is the (pre-conversion) source: re-derive the type
  # from the peeled cursor.
  var hookTyp = typ
  if n.exprKind in ConvKinds:
    while n.exprKind in ConvKinds:
      inc n
      skip n  # skip the target type
    hookTyp = getType(c.typeCache, n)

  let info = n.info
  let hookProc = getHook(c.lifter[], attachedWasMoved, hookTyp, info)
  if hookProc != NoSymId:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, hookProc, info
      copyIntoKind c.dest, HaddrX, info:
        tr c, n, WillBeOwned

proc callWasMoved(c: var Context; sym: SymId; info: PackedLineInfo; typ: Cursor) =
  let hookProc = getHook(c.lifter[], attachedWasMoved, typ, info)
  if hookProc != NoSymId:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, hookProc, info
      copyIntoKind c.dest, HaddrX, info:
        copyIntoSymUse c.dest, sym, info

proc trAsgn(c: var Context; n: var Cursor) =
  #[
  `x = f()` is turned into `let tmp = f(); =destroy(x); x =bitcopy tmp` #`f()` can read `x`
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
  # A `{.cursor.}` variable is non-owning: trLocal already skipped the `=dup`
  # at its declaration, so per-asgn `=destroy(old) … =dup(rhs)` would
  # over-decrement the rc on a value the cursor never claimed ownership of
  # (and matching =destroy at end of scope was suppressed for the same
  # reason). Treat cursor lhs's like the no-destructor case — raw bitcopy.
  let lhsIsCursor = le.kind == Symbol and
                    c.typeCache.getLocalInfo(le.symId).kind == CursorY
  if destructor == NoSymId or lhsIsCursor:
    # the type has no destructor, there is nothing interesting to do:
    trSons c, n, DontCare

  else:
    #let isNotFirstAsgn = not isResultUsage(c, le) # YYY Adapt this once we have "isFirstAsgn" analysis
    const isNotFirstAsgn = true
    var leCopy = le
    var lhs = evalLeftHandSide(c, leCopy)
    if constructsValue(ri, derefConstructs = false):
      if not potentialAliasing(le, ri):
        # `x = f()` is turned into `=destroy(x); x =bitcopy f()`.
        if isNotFirstAsgn:
          callDestroy(c, destructor, lhs, leType)
        copyInto c.dest, n:
          copyTree c.dest, lhs
          n = ri
          tr c, n, WillBeOwned
      else:
        # `x = f()` is turned into `let tmp = f(); =destroy(x); x =bitcopy tmp`.
        let tmp = tempOfTrArg(c, ri, leType)
        if isNotFirstAsgn:
          callDestroy(c, destructor, lhs, leType)
        copyInto c.dest, n:
          copyTree c.dest, lhs
          copyIntoSymUse c.dest, tmp, ri.info
          n = ri
          skip n, SkipFull
    elif isLastRead(c, ri):
      if isNotFirstAsgn and (potentialSelfAsgn(le, ri) or potentialAliasing(le, ri)):
        # `let tmp = y; =wasMoved(y); =destroy(x); x =bitcopy tmp`
        let tmp = tempOfTrArg(c, ri, leType)
        callWasMoved c, ri, leType
        callDestroy(c, destructor, lhs, leType)
        copyInto c.dest, n:
          var lhsAsCursor = cursorAt(lhs, 0)
          tr c, lhsAsCursor, DontCare
          copyIntoSymUse c.dest, tmp, ri.info
          n = n2
          skip n, SkipFull
      else:
        if isNotFirstAsgn:
          callDestroy(c, destructor, lhs, leType)
        copyInto c.dest, n:
          copyTree c.dest, lhs
          n = ri
          tr c, n, WillBeOwned
        callWasMoved c, ri, leType
    else:
      # XXX We should really prefer to simply call `=copy(x, y)` here.
      if isNotFirstAsgn and (potentialSelfAsgn(le, ri) or potentialAliasing(le, ri)):
        # `let tmp = x; x =bitcopy =dup(y); =destroy(tmp)`
        let tmp = tempOfTrArg(c, le, leType)
        copyInto c.dest, n:
          var lhsAsCursor = cursorAt(lhs, 0)
          tr c, lhsAsCursor, DontCare
          n = ri
          callDup c, n
        callDestroy(c, destructor, tmp, le.info, leType)
      else:
        if isNotFirstAsgn:
          callDestroy(c, destructor, lhs, leType)
        copyInto c.dest, n:
          var lhsAsCursor = cursorAt(lhs, 0)
          tr c, lhsAsCursor, DontCare
          n = ri
          callDup c, n

proc getHookType(c: var Context; n: Cursor): Cursor =
  var n = n
  inc n
  if n.exprKind == HaddrX:
    # skips `HaddrX` to get the correct type; otherwise
    # we get a `ptr` type
    inc n
  result = skipModifier(getType(c.typeCache, n))

proc trExplicitDestroy(c: var Context; n: var Cursor) =
  let typ = getHookType(c, n)
  let info = n.info
  let destructor = getDestructor(c.lifter[], typ, info)
  if destructor == NoSymId:
    c.dest.addEmpty info
    inc n, SkipTag
    skip n, SkipFull
  else:
    let needsAddr = isMutFirstParam(destructor)
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, destructor, info
      inc n, SkipTag
      if needsAddr:
        copyIntoKind c.dest, HaddrX, info:
          tr c, n, DontCare
      else:
        tr c, n, DontCare
  skipParRi n

proc trExplicitDup(c: var Context; n: var Cursor; e: Expects) =
  let typ = getHookType(c, n)
  let info = n.info
  let hookProc = getHook(c.lifter[], attachedDup, typ, info)
  if hookProc != NoSymId:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, hookProc, info
      inc n, SkipTag
      tr c, n, DontCare
  else:
    let e2 = if e == WillBeOwned: WantOwner else: e
    inc n, SkipTag
    tr c, n, e2
  skipParRi n

proc trExplicitCopy(c: var Context; n: var Cursor; op: AttachedOp) =
  let typ = getHookType(c, n)
  let info = n.info
  let hookProc = getHook(c.lifter[], op, typ, info)
  if hookProc != NoSymId:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, hookProc, info
      inc n, SkipTag
      while n.hasMore:
        tr c, n, DontCare
      takeParRi c.dest, n
  else:
    c.dest.addParLe AsgnS, info
    inc n, SkipTag
    if n.exprKind == HaddrX:
      inc n, SkipTag
      tr c, n, DontCare
      skipParRi(n)
    else:
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
      inc n, SkipTag
      tr c, n, DontCare
  else:
    inc n, SkipTag
    skip n, SkipFull
  skipParRi n

proc trExplicitTrace(c: var Context; n: var Cursor) =
  let typ = getHookType(c, n)
  let info = n.info
  let hookProc = getHook(c.lifter[], attachedTrace, typ, info)
  if hookProc != NoSymId:
    copyIntoKind c.dest, CallS, info:
      copyIntoSymUse c.dest, hookProc, info
      inc n, SkipTag
      tr c, n, DontCare
      tr c, n, DontCare
  else:
    inc n, SkipTag
    skip n, SkipFull
    skip n, SkipFull
  skipParRi n

when not defined(nimony):
  proc trProcDecl(c: var Context; n: var Cursor; parentNodestroy = false)

proc trOnlyEssentials(c: var Context; n: var Cursor)
    {.ensuresNif: addedAny(c.dest).} =
  case n.kind
  of Symbol, UIntLit, StringLit, IntLit, FloatLit, CharLit, SymbolDef, UnknownToken, EofToken, DotToken, Ident:
    takeToken c.dest, n
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
    of WasmovedX:
      trExplicitWasMoved c, n
    of TraceX:
      trExplicitTrace c, n
    of NoExpr:
      case n.stmtKind
      of LocalDecls:
        let kind = n.symKind
        copyInto c.dest, n:
          c.typeCache.takeLocalHeader(c.dest, n, kind)
          while n.hasMore:
            trOnlyEssentials c, n
      of ProcS, FuncS, ConverterS, MethodS:
        trProcDecl c, n, parentNodestroy = true
      of MacroS:
        # Macro bodies live in the out-of-process plugin binary; pass
        # the whole decl through opaquely.
        takeTree c.dest, n
      of ScopeS:
        c.typeCache.openScope()
        copyInto c.dest, n:
          while n.hasMore:
            trOnlyEssentials c, n
        c.typeCache.closeScope()
      of CallS, CmdS, IteratorS, TemplateS, TypeS, BlockS,
          EmitS, AsgnS, IfS, WhenS, BreakS, ContinueS, ForS,
          WhileS, CoroforS, CaseS, RetS, YldS, StmtsS, PragmasS,
          PragmaxS, InclS, ExclS, IncludeS, ImportS, ImportasS,
          FromimportS, ImportexceptS, ExportS, ExportexceptS,
          CommentS, DiscardS, TryS, RaiseS, UnpackdeclS, AssumeS,
          AssertS, CallstrlitS, InfixS, PrefixS, HcallS,
          StaticstmtS, BindS, MixinS, UsingS, AsmS, DeferS,
          NoStmt:
        # generic statement: copy the head and recurse into the children
        copyInto c.dest, n:
          while n.hasMore: trOnlyEssentials c, n
    of ErrX, SufX, AtX, DerefX, DotX, PatX, ParX, AddrX, NilX,
        InfX, NeginfX, NanX, FalseX, TrueX, AndX, OrX, XorX,
        NotX, NegX, SizeofX, AlignofX, OffsetofX, OconstrX,
        AconstrX, BracketX, CurlyX, CurlyatX, OvfX, AddX, SubX,
        MulX, DivX, ModX, ShrX, ShlX, BitandX, BitorX, BitxorX,
        BitnotX, EqX, NeqX, LeX, LtX, CastX, ConvX, CallX, CmdX,
        CchoiceX, OchoiceX, PragmaxX, QuotedX, HderefX, DdotX,
        HaddrX, NewrefX, NewobjX, TupX, TupconstrX, SetconstrX,
        TabconstrX, AshrX, BaseobjX, HconvX, DconvX, CallstrlitX,
        InfixX, PrefixX, HcallX, CompilesX, DeclaredX, DefinedX,
        AstToStrX, BindSymX, BindSymNameX, InstanceofX, ProccallX, HighX, LowX, TypeofX,
        UnpackX, FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX,
        DefaultobjX, DefaulttupX, DefaultdistinctX, DelayX,
        Delay0X, SuspendX, ExprX, DoX, ArratX, TupatX, PlussetX,
        MinussetX, MulsetX, XorsetX, EqsetX, LesetX, LtsetX,
        InsetX, CardX, EmoveX, InternalTypeNameX,
        InternalFieldPairsX, FailedX, IsX, EnvpX, KvX:
      # all other expression kinds: copy the head and recurse into the children
      copyInto c.dest, n:
        while n.hasMore: trOnlyEssentials c, n
  of ParRi:
    raiseAssert "BUG: unexpected ParRi in duplifier.trOnlyEssentials"

proc trProcDecl(c: var Context; n: var Cursor; parentNodestroy = false) =
  c.dest.add n
  let oldResultSym = c.resultSym
  let oldFlags = c.flags
  c.resultSym = NoSymId
  c.flags = {}
  let decl = n
  var r = takeRoutine(n, SkipFinalParRi)
  let symId = r.name.symId
  if isLocalDecl(symId):
    c.typeCache.registerLocal(symId, r.kind, decl)
  if hasPragmaOfValue(r.pragmas, ReportP, "lastuse"):
    c.flags.incl ReportLastUse
  if hasPragma(r.pragmas, RaisesP):
    c.flags.incl CanRaise
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
    c.typeCache.registerParams(r.name.symId, decl, r.params)
    if parentNodestroy or hasPragma(r.pragmas, NodestroyP):
      trOnlyEssentials c, r.body
    else:
      tr c, r.body, DontCare
    c.typeCache.closeScope()
  else:
    copyTree c.dest, r.body
  c.dest.addParRi()
  c.resultSym = oldResultSym
  c.flags = oldFlags

proc hasDestructor(c: Context; typ: Cursor): bool {.inline.} =
  # `isTrivial(c.lifter[], typ)` consults `c.lifter[].op`, which floats
  # with each `getHook(...)` call (e.g. a sibling sink-arg's `=dup`
  # lookup mid-trCall): a follow-up call asks "does `T` have an op?" and
  # gets answered against the OTHER op, misclassifying types that define
  # `=destroy` but no `=dup` (e.g. TokenBuf) as trivial. `hasDestroyHook`
  # pins the query to `attachedDestroy` regardless of context.
  hasDestroyHook(c.lifter[], typ)

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

proc trCall(c: var Context; n: var Cursor; e: Expects)
    {.ensuresNif: addedAny(c.dest).} =
  var ow = owningTempDefault()
  let retType = getType(c.typeCache, n)
  if hasDestructor(c, retType) and e == WantNonOwner:
    ow = bindToTemp(c, retType, n.info)

  takeToken c.dest, n # take `(call)`
  var fnType = skipProcTypeToParams(getType(c.typeCache, n))

  tr c, n, DontCare # transforms `fn` because it may be an expression that requires further handling
  assert fnType.substructureKind == ParamsU
  inc fnType
  while n.hasMore:
    let previousFormalParam = fnType
    var e2 = WantNonOwner
    if fnType.kind == ParRi:
      discard "this can happen for closure parameters"
    else:
      let param = takeLocal(fnType, SkipFinalParRi)
      let pk = param.typ.typeKind
      if pk == SinkT:
        e2 = WantOwner
      elif pk == VarargsT:
        # do not advance formal parameter:
        fnType = previousFormalParam
    tr c, n, e2
  takeParRi c.dest, n
  finishOwningTemp c.dest, ow

proc trRawConstructor(c: var Context; n: var Cursor; e: Expects)
    {.ensuresNif: addedAny(c.dest).} =
  # Idioms like `echo ["ab", myvar, "xyz"]` are important to translate well.
  let e2 = if e == WillBeOwned: WantOwner else: e
  takeToken c.dest, n
  while n.hasMore:
    tr c, n, e2
  takeParRi c.dest, n

proc trConvExpr(c: var Context; n: var Cursor; e: Expects) =
  copyInto c.dest, n:
    takeTree c.dest, n # type
    while n.hasMore:
      tr c, n, e

proc isCursorField(fieldKey: Cursor): bool =
  ## A field referenced in a `(kv fieldSym value …)` constructor entry.
  ## Loads the field's decl and checks for the `.cursor` pragma so the
  ## value can be moved/bit-copied without an `=dup` — matching the
  ## non-owning semantics the lifter uses inside auto-derived hooks
  ## (see `unravelObjField` in `src/hexer/lifter.nim`).
  if fieldKey.kind != Symbol: return false
  let res = tryLoadSym(fieldKey.symId)
  if res.status != LacksNothing: return false
  let local = asLocal(res.decl)
  if local.kind notin {FldY, GfldY}: return false
  result = hasPragma(local.pragmas, CursorP)

proc trObjConstr(c: var Context; n: var Cursor; e: Expects) =
  var ow = owningTempDefault()
  let typ = n.firstSon
  if hasDestructor(c, typ) and e == WantNonOwner:
    ow = bindToTemp(c, typ, n.info)
  copyInto c.dest, n:
    takeTree c.dest, n
    while n.hasMore:
      assert n.substructureKind == KvU
      copyInto c.dest, n:
        let fieldKey = n
        takeTree c.dest, n
        # `{.cursor.}` fields are non-owning aliases — they don't bump
        # rc on assignment, and the auto-derived `=destroy` doesn't
        # recurse into them. Pass `WantNonOwner` so `tr → trLocation`
        # emits a plain read instead of splicing an `=dup` hook around
        # the value. Without this, `LocalInfo(typ: someCursor)` would
        # `=dup_Cursor(someCursor)` (rc+1) and the matching
        # `=destroy(typ)` at scope exit would `rc-` — net rc+0, but a
        # mismatched =wasMoved on the source would over-count, exactly
        # the shape of the open stage-2 TypeCache UAF.
        let kind = if isCursorField(fieldKey): WantNonOwner else: WantOwner
        tr c, n, kind
        if n.hasMore:
          # optional inheritance
          takeTree c.dest, n
  finishOwningTemp c.dest, ow

proc trNewobjFields(c: var Context; n: var Cursor) =
  while n.hasMore:
    if n.substructureKind == KvU:
      copyInto c.dest, n:
        let fieldKey = n
        takeTree c.dest, n # keep field name
        let kind = if isCursorField(fieldKey): WantNonOwner else: WantOwner
        tr(c, n, kind)
        if n.hasMore:
          # optional inheritance
          takeTree c.dest, n
    else:
      tr(c, n, WantOwner)
  skipParRi n

proc genOutOfMemCheck(c: var Context; ow: OwningTemp; info: PackedLineInfo) =
  copyIntoKind c.dest, IfS, info:
    copyIntoKind c.dest, ElifU, info:
      copyIntoKind c.dest, EqX, info:
        copyIntoKind c.dest, PointerT, info: discard
        c.dest.add symToken(ow.s, info)
        copyIntoKind c.dest, NilX, info: discard
      copyIntoKind c.dest, StmtsS, info:
        copyIntoKind c.dest, RaiseS, info:
          c.dest.add symToken(pool.syms.getOrIncl("OutOfMemError.0." & SystemModuleSuffix), info)

proc trNewobj(c: var Context; n: var Cursor; e: Expects; kind: ExprKind)
    {.ensuresNif: addedAny(c.dest).} =
  let info = n.info
  inc n, SkipTag
  let refType = n
  assert refType.typeKind == RefT

  var ow = bindToTemp(c, refType, info, if e == WantNonOwner: VarS else: CursorS)

  let baseType = refType.firstSon
  var refTypeCopy = refType
  let typeKey = takeMangle(refTypeCopy, Frontend, c.lifter.bits)
  let typeSym = pool.syms.getOrIncl(genericTypeName(typeKey, c.moduleSuffix))

  copyIntoKind c.dest, CastX, info:
    c.dest.addSubtree refType
    copyIntoKind c.dest, CallX, info:
      c.dest.add symToken(pool.syms.getOrIncl("allocFixed.0." & SystemModuleSuffix), info)
      copyIntoKind c.dest, SizeofX, info:
        c.dest.add symToken(typeSym, info)
  c.dest.addParRi() # finish temp declaration

  # map to OOM if the proc can raise an error:
  if CanRaise in c.flags:
    genOutOfMemCheck(c, ow, info)

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
            skip n, SkipType
            trNewobjFields(c, n)
        else:
          skip n, SkipType
          tr c, n, WantOwner # process default(T) call

  c.dest.addParRi()  # finish the StmtsS
  c.dest.copyIntoSymUse ow.s, ow.info
  c.dest.addParRi()  # finish the StmtListExpr

proc genLastRead(c: var Context; n: var Cursor; typ: Cursor)
    {.ensuresNif: addedAny(c.dest).} =
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

proc trLocationNonOwner(c: var Context; n: var Cursor) =
  if n.kind == ParLe and n.exprKind == DotX:
    takeToken c.dest, n
    tr c, n, WantNonOwner
    while n.hasMore:
      takeTree c.dest, n
    takeParRi c.dest, n
  else:
    takeToken c.dest, n
    tr c, n, WantNonOwner
    while n.hasMore:
      tr(c, n, DontCare)
    takeParRi c.dest, n

proc trLocation(c: var Context; n: var Cursor; e: Expects)
    {.ensuresNif: addedAny(c.dest).} =
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
            trLocationNonOwner c, n
      elif isAtom(n):
        takeTree c.dest, n
      else:
        trLocationNonOwner c, n
  elif isAtom(n):
    takeTree c.dest, n
  else:
    trLocationNonOwner c, n

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
      elif constructsValue(r.val, derefConstructs = false):
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

proc trStmtListExpr(c: var Context; n: var Cursor; e: Expects)
    {.ensuresNif: addedAny(c.dest).} =
  takeToken c.dest, n
  while n.hasMore:
    if isLastSon(n):
      tr(c, n, e)
    else:
      tr(c, n, WantNonOwner)
  takeParRi c.dest, n

proc trEnsureMove(c: var Context; n: var Cursor; e: Expects)
    {.ensuresNif: addedAny(c.dest).} =
  let typ = getType(c.typeCache, n)
  let arg = n.firstSon
  let info = n.info
  # `ensureMove(hderef (call subscript x i))` reads an lvalue through a
  # `var V`-returning accessor. With the old `derefConstructs=true` check
  # this fell into the "constructor" branch and was just copied (no
  # `=wasMoved` on the source) — fine for non-destructor element types
  # but a guaranteed double-free for destructor types: the seq slot and
  # the temp both end up owning the same payload. Force the genLastRead
  # (bitcopy + `=wasMoved(addr inner)`) path when (a) the argument goes
  # through a deref and (b) the result owns resources we'd otherwise
  # alias.
  if arg.exprKind in {DerefX, HderefX} and
      e in {WantOwner, WillBeOwned} and hasDestructor(c, typ):
    inc n, SkipTag
    genLastRead(c, n, typ)
    skipParRi n
  elif constructsValue(arg, derefConstructs = true):
    # we allow rather silly code like `ensureMove(234)`.
    # Seems very useful for generic programming as this can come up
    # from template expansions:
    inc n, SkipTag
    tr c, n, e
    skipParRi n
  elif isLastRead(c, arg):
    if e == WantOwner and hasDestructor(c, typ):
      inc n, SkipTag
      genLastRead(c, n, typ)
      skipParRi n
    else:
      inc n, SkipTag
      tr c, n, e
      skipParRi n
  else:
    let m = "not the last usage of: " & asNimCode(arg)
    c.dest.buildTree nifstreams.ErrT, info:
      c.dest.addSubtree n
      c.dest.add strToken(pool.strings.getOrIncl(m), info)
    skip n, SkipFull

proc trDeref(c: var Context; n: var Cursor; e: Expects)
    {.ensuresNif: addedAny(c.dest).} =
  # `(deref ptr)` produces a value of the dereffed type. When the
  # caller wants to own it (e.g. an `Item(field: deref ptr)` field
  # value, or any other `WantOwner` slot), we have to splice in the
  # type's `=dup` hook the same way `trLocation` does for symbol/dot
  # reads — otherwise the slot is initialized via raw struct copy with
  # no rc bump, and the matching `=destroy` later under-counts the
  # owner's refcount and frees the underlying buffer prematurely.
  let info = n.info
  let derefedTyp = getType(c.typeCache, n) # type of the whole `(deref …)` expr
  let needsDup = e == WantOwner and not cursorIsNil(derefedTyp) and
                 hasDestructor(c, derefedTyp) and not isLastRead(c, n)
  var dupHook = NoSymId
  if needsDup:
    dupHook = getHook(c.lifter[], attachedDup, derefedTyp, info)
  let wrapDup = needsDup and dupHook != NoSymId
  if wrapDup:
    c.dest.addParLe CallS, info
    c.dest.add symToken(dupHook, info)
  inc n, SkipTag
  var typ = getType(c.typeCache, n, {SkipAliases})
  if typ.kind == ParLe and typ.typeKind == SinkT:
    inc typ
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
  if wrapDup:
    c.dest.addParRi()

proc trCoroFor(c: var Context; n: var Cursor)

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
    of WasmovedX:
      trExplicitWasMoved c, n
    of SinkhX:
      trExplicitCopy c, n, attachedSink
    of TraceX:
      trExplicitTrace c, n
    of ConvKinds, BaseobjX, SufX:
      trConvExpr c, n, e
    of OconstrX:
      trObjConstr c, n, e
    of NewobjX:
      trNewobj c, n, e, NewobjX
    of NewrefX:
      trNewobj c, n, e, NewrefX
    of DotX, AtX, ArratX, PatX, TupatX:
      trLocation c, n, e
    of ParX:
      trSons c, n, e
    of ExprX:
      trStmtListExpr c, n, e
    of EmoveX:
      trEnsureMove c, n, e
    of AconstrX, TupconstrX:
      trRawConstructor c, n, e
    of NilX, FalseX, TrueX, AndX, OrX, NotX, NegX, SizeofX, SetconstrX,
       OchoiceX, CchoiceX, XorX,
       AddX, SubX, MulX, DivX, ModX, ShrX, ShlX, AshrX, BitandX, BitorX, BitxorX, BitnotX,
       PlussetX, MinussetX, MulsetX, XorsetX, EqsetX, LesetX, LtsetX, InsetX, CardX,
       EqX, NeqX, LeX, LtX, InfX, NeginfX, NanX, CompilesX, DeclaredX,
       DefinedX, AstToStrX, BindSymX, BindSymNameX, HighX, LowX, TypeofX, UnpackX, FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX, QuotedX,
       AddrX, HaddrX, AlignofX, OffsetofX, ErrX, OvfX, InstanceofX, InternalTypeNameX,
       InternalFieldPairsX, IsX:
      trSons c, n, WantNonOwner
    of DerefX, HderefX:
      trDeref c, n, e
    of DdotX:
      bug "nodekind should have been eliminated in desugar.nim"
    of EnvpX:
      bug "envp should have been eliminated in lambdalifting.nim"
    of DefaultobjX, DefaulttupX, DefaultdistinctX, BracketX, CurlyX, TupX:
      bug "nodekind should have been eliminated in sem.nim"
    of PragmaxX, CurlyatX, TabconstrX, DoX, FailedX, Delay0X, SuspendX:
      trSons c, n, e
    of KvX:
      copyInto c.dest, n:
        takeTree c.dest, n
        tr c, n, e
        if n.hasMore:
          takeTree c.dest, n
    of NoExpr:
      let k = n.stmtKind
      case k
      of RetS:
        trReturn c, n
      of AsgnS:
        trAsgn c, n
      of LocalDecls:
        trLocal c, n, k
      of ProcS, FuncS, ConverterS, MethodS:
        trProcDecl c, n
      of ScopeS:
        c.typeCache.openScope()
        trSons c, n, WantNonOwner
        c.typeCache.closeScope()
      of BreakS, ContinueS, MacroS, TemplateS:
        # Macros are compiled into out-of-process plugins by `nimony`
        # itself; templates are expanded at call-sites. Neither has a
        # body that participates in the regular lowering pipeline, so
        # pass the decl through verbatim.
        takeTree c.dest, n
      of IteratorS:
        # iterinliner passes only `.closure` iterators through to hexer.
        # When the closure flag is set, their bodies need duplifier's hook
        # injection on every asgn (especially the synthesized `result = v`
        # at each yield). Non-closure iters that slip through should be
        # passed verbatim.
        var probe = n
        let routine = asRoutine(probe, SkipExclBody)
        if hasPragma(routine.pragmas, ClosureP):
          trProcDecl c, n
        else:
          takeTree c.dest, n
      of CoroforS:
        trCoroFor c, n
      else:
        trSons c, n, WantNonOwner

proc trCoroFor(c: var Context; n: var Cursor) =
  ## The iter call is consumed by cps.nim's trCoroFor (rewritten to the
  ## init wrapper call). Don't =dup/extract it here. Just descend into the
  ## body.
  c.dest.add n # corofor tag
  inc n
  takeTree c.dest, n # iter call verbatim
  tr c, n, WantNonOwner # body
  c.dest.takeParRi n

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
        if arg.hasMore:
          let param = asLocal(arg)
          m.add " for type <" & typeToString(param.typ) & ">"
      try:
        r.error infoToStr(info), m
      except:
        quit 1
      inc result

proc checkForMoveTypes(c: var Context; n: Cursor): int =
  var nested = 0
  var r = Reporter(verbosity: 2, noColors: not useColors())
  var n = n
  var skipDepth = -1
    ## -1 = walking normally; otherwise the `nested` depth we resume walking at.
    ## We skip the bodies of `(nodestroy)` routines: those are auto-derived hooks
    ## or library hook implementations (e.g. `seq.=dup`), whose bodies may
    ## reference a `.error` sub-hook for dead-code reasons. The lifter has
    ## already propagated `.error` to the enclosing hook itself via
    ## `siblingHookIsError` + `c.calledErrorHook`, so a user-visible call to
    ## the *enclosing* hook is what we want to surface, not the inner call.
  result = 0
  while true:
    case n.kind
    of ParLe:
      inc nested
      if skipDepth < 0:
        let sk = symKind n
        if isRoutine(sk):
          var probe = n
          let routine = asRoutine(probe)
          if hasPragma(routine.pragmas, NodestroyP):
            skipDepth = nested - 1
        if skipDepth < 0:
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
              try:
                r.error infoToStr(info), pool.strings[n.litId]
              except:
                quit 1
              inc result
    of ParRi:
      dec nested
      if skipDepth >= 0 and nested == skipDepth:
        skipDepth = -1
    else:
      discard
    if nested == 0: break
    inc n

proc injectDups*(pass: var Pass; lifter: ref LiftingCtx) =
  var n = pass.n  # Extract cursor locally
  var c = Context(lifter: lifter, typeCache: createTypeCache(),
    dest: move(pass.dest), source: addr pass.buf, moduleSuffix: pass.moduleSuffix)
  c.typeCache.openScope()
  tr(c, n, WantNonOwner)
  genMissingHooks lifter[]

  var ndest = beginRead(c.dest)
  let errorCount = checkForMoveTypes(c, ndest)
  endRead(c.dest)
  c.typeCache.closeScope()

  if errorCount > 0:
    quit 1

  pass.dest = ensureMove(c.dest)
