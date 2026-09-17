# removes abstractions like set ops and ref object constructors

when defined(nimony):
  {.feature: "untyped".}
else:
  {.pragma: untyped.}

import std / [assertions, tables, hashes, sets, syncio]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, expreval, xints, builtintypes, langmodes, renderer, reporters]
import hexer_context, passes
import ".." / finalir / finalir_model
include ".." / nimony / nif_annotations

type
  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    tempUseBufStack: seq[TokenBuf]
    activeChecks: set[CheckMode]
    pending: TokenBuf
    constDecls: TokenBuf
      ## module-level `const`s minted for set literals; emitted before the
      ## module body so the C backend sees them declared before their uses
    bits: int  ## target `int` width, handed to the const evaluator
    fir: bool
      ## The input is the Final IR (`hexerSpeaksFir`) and nothing lowers this
      ## pass's output again: control flow is spelled `loop`/`ite`/`jmp`, and
      ## what an expansion needs to run first goes to `pre` instead of into an
      ## `(expr …)`.
    pre: TokenBuf
      ## `fir`: statements for in front of the statement being translated;
      ## `trStmt` splices them in.

proc freshLabel(c: var Context): SymId =
  result = pool.symId("`desugarL." & $c.counter)
  inc c.counter

proc openLoop(c: var Context; dest: var TokenBuf; info: NifLineInfo): SymId =
  ## `while <cond>: <body>`, part one; the caller emits `<cond>` next, then
  ## `openLoopBody`, the body, and `closeLoop`. Returns the exit label (`fir`
  ## only), for `emitBreak`.
  if c.fir:
    result = freshLabel(c)
    dest.addParLe LoopV, info
    dest.addParLe ScopeS, info
    dest.addParLe IteV, info
  else:
    result = SymId(0)
    dest.addParLe WhileS, info

proc openLoopBody(dest: var TokenBuf; info: NifLineInfo) =
  dest.addParLe StmtsS, info

proc emitBreak(c: var Context; dest: var TokenBuf; exitLab: SymId; info: NifLineInfo) =
  if c.fir:
    copyIntoKind dest, JmpS, info:
      dest.addSymUse exitLab, info
  else:
    copyIntoKind dest, BreakS, info:
      dest.addDotToken()

proc closeLoop(c: var Context; dest: var TokenBuf; exitLab: SymId; info: NifLineInfo) =
  dest.addParRi() # body
  if c.fir:
    copyIntoKind dest, StmtsS, info:
      emitBreak c, dest, exitLab, info
    dest.addParRi() # ite
    copyIntoKind dest, ContinueV, info:
      dest.addDotToken()
    dest.addParRi() # scope
    dest.addParRi() # loop
    copyIntoKind dest, LabS, info:
      dest.addSymDef exitLab, info
  else:
    dest.addParRi() # while

proc openIf(c: var Context; dest: var TokenBuf; info: NifLineInfo) =
  ## `if <cond>: <body>`: `openIf`, `<cond>`, `openIfBody`, `<body>`, `closeIf`.
  if c.fir:
    dest.addParLe IteV, info
  else:
    dest.addParLe IfS, info
    dest.addParLe ElifU, info

proc openIfBody(dest: var TokenBuf; info: NifLineInfo) =
  dest.addParLe StmtsS, info

proc closeIf(c: var Context; dest: var TokenBuf) =
  dest.addParRi() # body
  if c.fir:
    dest.addDotToken() # no else
    dest.addParRi() # ite
  else:
    dest.addParRi() # elif
    dest.addParRi() # if

proc emitValue(c: var Context; dest: var TokenBuf; pre, val: var TokenBuf;
               info: NifLineInfo) =
  ## An expansion's result: the statements it needs first, and its value.
  if pre.len == 0:
    dest.add val
  elif c.fir:
    c.pre.add pre
    dest.add val
  else:
    dest.addParLe(ExprX, info)
    dest.add pre
    dest.add val
    dest.addParRi()

proc declareTemp(c: var Context; dest: var TokenBuf; typ: Cursor; info: NifLineInfo): SymId =
  let s = "`desugar." & $c.counter
  inc c.counter
  result = pool.symId(s)
  dest.addParLe("var", info)
  dest.addSymDef result, info
  dest.addDotToken() # export, pragmas
  dest.addDotToken()
  copyTree dest, typ # type

proc needsTemp(n: Cursor): bool =
  # Pre-initialise: the contract analyser drops the `IfFalse cf s`
  # implication for the leaving-path cfvar raised inside the inner
  # while-loop, so it cannot prove `result` is set on the normal exit of
  # the AtX branch. `result = false` here is the bool default anyway —
  # run `bin/nimony c --verbose src/hexer/desugar.nim` (with this line
  # removed) to see the Final IR that trips the checker.
  result = false
  case n.kind
  of Symbol, IntLit, UIntLit, FloatLit, CharLit, StrLit:
    result = false
  of TagLit:
    var n = n
    case n.exprKind
    of NilX, FalseX, TrueX, InfX, NeginfX, NanX, SizeofX:
      result = false
    of ExprX:
      n = sub(n)  # throwaway copy; bounds the probe under vpr
      let first = n
      skip n
      if not n.hasMore:
        # single element expr
        result = needsTemp(first)
      else:
        result = true
    of SufX:
      inc n
      result = needsTemp(n)
    of DconvX:
      inc n
      skip n
      result = needsTemp(n)
    of AtX, PatX, ArratX, TupatX, DotX, DdotX, ParX, AddrX, HaddrX:
      result = false
      n = sub(n)  # throwaway copy; bounds the walk under vpr
      while n.hasMore:
        if needsTemp(n):
          return true
        skip n
    of ErrX, DerefX, AndX, OrX, XorX, NotX, NegX, AlignofX,
        OffsetofX, OconstrX, AconstrX, BracketX, CurlyX, CurlyatX,
        OvfX, AddX, SubX, MulX, DivX, ModX, ShrX, ShlX, BitandX,
        BitorX, BitxorX, BitnotX, EqX, NeqX, LeX, LtX, CastX,
        ConvX, CallX, CmdX, CchoiceX, OchoiceX, PragmaxX, QuotedX,
        HderefX, NewrefX, NewobjX, TupX, TupconstrX, SetconstrX,
        TabconstrX, AshrX, BaseobjX, HconvX, CallstrlitX, InfixX,
        PrefixX, HcallX, CompilesX, DeclaredX, DefinedX, AstToStrX, BindSymX, BindSymNameX,
        InstanceofX, ProccallX, HighX, LowX, TypeofX, UnpackX,
        FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX, InstantiationinfoX,
        DefaultobjX, DefaulttupX, DefaultdistinctX, DelayX,
        Delay0X, SuspendX, DoX, PlussetX, MinussetX, MulsetX,
        XorsetX, EqsetX, LesetX, LtsetX, InsetX, CardX, EmoveX,
        DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX,
        InternalTypeNameX, InternalFieldPairsX, FailedX, IsX,
        EnvpX, KvX, ToClosureX, PluginCallX, NoExpr:
      result = true
  else:
    result = true

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor; isTopScope = false)
  {.ensuresNif: addedAny(dest).}

proc trSons(c: var Context; dest: var TokenBuf; n: var Cursor; isTopScope = false) =
  if n.substructureKind == KvU:
    takeInto dest, n:
      dest.takeTree n # key
      while n.hasMore:
        tr(c, dest, n, isTopScope)
  elif n.exprKind in {DotX, DdotX}:
    takeInto dest, n:
      tr(c, dest, n, isTopScope)
      while n.hasMore:
        dest.takeTree n
  else:
    copyInto dest, n:
      while n.hasMore:
        tr(c, dest, n, isTopScope)

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    c.typeCache.takeLocalHeader(dest, n, kind)
    tr(c, dest, n)

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor; isTopScope = false) =
  ## One statement, preceded by whatever its translation put in `pre`. The
  ## enclosing statement's `pre` is parked across the descent.
  var outer = createTokenBuf(0)
  swap outer, c.pre
  let start = dest.len
  tr(c, dest, n, isTopScope)
  if c.pre.len > 0:
    dest.insert(c.pre, start)
  swap outer, c.pre

proc trStmtList(c: var Context; dest: var TokenBuf; n: var Cursor; isTopScope = false) =
  copyInto dest, n:
    while n.hasMore:
      trStmt(c, dest, n, isTopScope)

proc trProcBody(c: var Context; dest: var TokenBuf; n: var Cursor) =
  n.into:
    while n.hasMore:
      trStmt(c, dest, n)

proc trRoutineHeader(c: var Context; dest: var TokenBuf; decl: Cursor; n: var Cursor; pragmas: var Cursor): bool =
  # returns false if the routine is generic
  result = true # assume it is concrete
  let sym = n.symId
  for i in 0..<BodyPos:
    if i == ParamsPos:
      c.typeCache.registerParams(sym, decl, n)
    elif i == TypevarsPos:
      result = n.substructureKind != TypevarsU
    elif i == ProcPragmasPos:
      pragmas = n
    takeTree dest, n

proc emitRequiresGuard(c: var Context; dest: var TokenBuf; cond: Cursor;
                      msg: string; info: NifLineInfo) =
  openIf c, dest, info
  dest.copyIntoKind NotX, info:
    var n = cond
    tr(c, dest, n)
  openIfBody dest, info
  dest.copyIntoKind CallS, info:
    dest.addSymUse pool.symId("panic.0." & SystemModuleSuffix), info
    dest.addStrLit msg, info
  closeIf c, dest

proc emitRequires(c: var Context; dest: var TokenBuf; cond: Cursor;
                  where: string; info: NifLineInfo) =
  ## `.requires: a and b` becomes TWO guards, not one guard on a conjunction.
  ##
  ## `panic` does not return, so `if not a: panic` followed by `if not b: panic`
  ## is exactly equivalent — but it costs a compare and a branch each, where the
  ## conjunction costs a *materialized boolean*: the short-circuit lowering
  ## builds `x` in a diamond (`setle`, `and 1`, `jmp`, `mov 0`) and then
  ## re-tests it (`cmp x, 0; jne`). Measured on the x64 back end that is 11
  ## instructions on the fast path against gcc's 2, and it is on the fast path
  ## of EVERY `seq[int]` index check — the single biggest item in the 44 % of
  ## the hot loop these checks cost
  ## ([[destination_measured_bounds_checks_not_inlining]]).
  ##
  ## Splitting also lets the redundant-guard pass (`shoggoth/bce`) match the
  ## conjuncts independently, which one opaque bool temp never allowed.
  if cond.exprKind == AndX:
    var a = cond
    inc a
    var b = a
    skip b
    emitRequires(c, dest, a, where, info)
    emitRequires(c, dest, b, where, info)
  else:
    # Each half reports only the half that failed, which is strictly better
    # diagnostics than naming the whole conjunction.
    emitRequiresGuard(c, dest, cond,
                      where & ": " & asNimCode(cond) & " [AssertionDefect]\n", info)

proc trRequires(c: var Context; dest: var TokenBuf; pragmas: Cursor) =
  if not cursorIsNil(pragmas) and BoundCheck in c.activeChecks:
    let req = extractPragma(pragmas, RequiresP)
    if not cursorIsNil(req):
      let info = req.info
      emitRequires(c, dest, req, infoToStr(pragmas.info), info)

proc isCoroutine(n: Cursor): bool =
  ## Does this `(iterator …)` decl survive as a real routine? `.closure` and
  ## `.passive` become state machines (`coro_transform`); everything else is
  ## inlined away by `elimForLoops` before this pass. The test mirrors the one
  ## `transformCoroutineDecl` makes.
  let r = asRoutine(n)
  result = r.kind == IteratorY and
           (hasPragma(r.pragmas, ClosureP) or hasPragma(r.pragmas, PassiveP))

proc trProc(c: var Context; dest: var TokenBuf; n: var Cursor) =
  c.typeCache.openScope()
  let decl = n
  copyInto dest, n:
    var pragmas = default(Cursor)
    let isConcrete = c.trRoutineHeader(dest, decl, n, pragmas)
    if isConcrete and n.stmtKind == StmtsS:
      dest.addParLe(n.cursorTagId, n.info) # (stmts)
      # the guards are statements of the body: what they need first goes in
      # front of them, not in front of the routine
      var outer = createTokenBuf(0)
      swap outer, c.pre
      let guardStart = dest.len
      trRequires(c, dest, pragmas)
      if c.pre.len > 0:
        dest.insert(c.pre, guardStart)
      swap outer, c.pre
      trProcBody(c, dest, n)
      dest.addParRi()
    else:
      takeTree dest, n
  c.typeCache.closeScope()

proc addUIntType(buf: var TokenBuf; bits: int; info: NifLineInfo) =
  buf.addParLe("u", info)
  buf.addIntLit(bits, info)
  buf.addParRi()

proc addIntType(buf: var TokenBuf; bits: int; info: NifLineInfo) =
  buf.addParLe("i", info)
  buf.addIntLit(bits, info)
  buf.addParRi()

proc addSetType(buf: var TokenBuf; size: int; info: NifLineInfo) =
  case size
  of 1, 2, 4, 8:
    buf.addUIntType(size * 8, info)
  else:
    buf.addParLe("array", info)
    buf.addUIntType(8, info)
    buf.addIntLit(size, info)
    buf.addParRi()

proc trSetType(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  n.into:
    let sizeOrig = bitsetSizeInBytes(n)
    var err = false
    let size = asSigned(sizeOrig, err)
    if err:
      error "invalid set element type: ", n
    else:
      addSetType dest, int size, info
    skip n

proc liftTemp(c: var Context; dest: var TokenBuf; n: Cursor; typ: Cursor; info: NifLineInfo): Cursor =
  let tmp = declareTemp(c, dest, typ, n.info)
  dest.addSubtree n
  dest.addParRi()
  c.tempUseBufStack.add createTokenBuf(4)
  c.tempUseBufStack[^1].addSymUse(tmp, n.info)
  result = beginRead(c.tempUseBufStack[^1])

proc liftTempAddr(c: var Context; dest: var TokenBuf; n: Cursor; typ: Cursor; info: NifLineInfo): Cursor =
  var ptrTypeBuf = createTokenBuf(8)
  copyIntoKind ptrTypeBuf, PtrT, typ.info:
    ptrTypeBuf.addSubtree typ
  let ptrType = beginRead(ptrTypeBuf)
  let tmp = declareTemp(c, dest, ptrType, n.info)
  copyIntoKind dest, AddrX, n.info:
    dest.addSubtree n
  dest.addParRi()
  c.tempUseBufStack.add createTokenBuf(4)
  copyIntoKind c.tempUseBufStack[^1], DerefX, n.info:
    c.tempUseBufStack[^1].addSymUse(tmp, n.info)
  result = beginRead(c.tempUseBufStack[^1])

template addTypedOp(dest: var TokenBuf; kind: ExprKind|StmtKind; typ: Cursor; info: NifLineInfo; body: typed) {.untyped.} =
  copyIntoKind dest, kind, info:
    dest.addSubtree typ
    body

template addUIntTypedOp(dest: var TokenBuf; kind: ExprKind|StmtKind; bits: int; info: NifLineInfo; body: typed) {.untyped.} =
  copyIntoKind dest, kind, info:
    dest.addUIntType(bits, info)
    body

template addIntTypedOp(dest: var TokenBuf; kind: ExprKind|StmtKind; bits: int; info: NifLineInfo; body: typed) {.untyped.} =
  copyIntoKind dest, kind, info:
    dest.addIntType(bits, info)
    body

proc openRange(c: var Context; dest: var TokenBuf; i: Cursor; bound: int;
               info: NifLineInfo): SymId =
  ## `while i < bound:` — the body follows, then `closeRange`.
  result = openLoop(c, dest, info)
  addIntTypedOp dest, LtX, -1, info:
    dest.addSubtree i
    dest.addIntLit(bound, info)
  openLoopBody dest, info

proc closeRange(c: var Context; dest: var TokenBuf; i: Cursor; exitLab: SymId;
                info: NifLineInfo) =
  copyIntoKind dest, AsgnS, info:
    dest.addSubtree i
    addIntTypedOp dest, AddX, -1, info:
      dest.addSubtree i
      dest.addIntLit(1, info)
  closeLoop c, dest, exitLab, info

proc arrayToPointer(dest: var TokenBuf; arr: Cursor; info: NifLineInfo) =
  copyIntoKind dest, AddrX, info:
    copyIntoKind dest, ArratX, info:
      dest.addSubtree arr
      dest.addIntLit(0, info)

proc genSetElem(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # XXX could implement offset here
  addUIntTypedOp dest, CastX, -1, n.info:
    tr(c, dest, n)

proc isConstSym(n: Cursor): bool =
  ## Does `n` name a `const`? Such a symbol survives any side effect the other
  ## operand of a set op might have, so it never needs to be snapshotted.
  result = false
  if n.kind == Symbol:
    let res = tryLoadSym(n.symId)
    if res.status == LacksNothing:
      result = asLocal(res.decl).kind == ConstY

proc isLiteralSet(n: Cursor): bool =
  ## Is `n` the `(aconstr <type> <lit>...)` that `genSetConstr` emits for a set
  ## literal whose elements were all known at compile time?
  result = false
  if n.kind == TagLit and n.exprKind == AconstrX:
    var it = n
    it = sub(it)  # throwaway copy; bounds the walk under vpr
    skip it       # the type
    result = true
    while it.hasMore:
      if it.kind notin {IntLit, UIntLit, CharLit}:
        return false
      inc it

proc hoistConstSet(c: var Context; n: Cursor; info: NifLineInfo): SymId =
  ## Turn a constant set literal into a module-level `const` and return its
  ## symbol. Left inline the literal is bound to a local, so every evaluation of
  ## the enclosing set op rebuilds all of its bytes on the stack — 32 stores for
  ## a `set[char]` — only to read a single one of them back.
  # `<name>.<disambiguator>.<module-suffix>`, the shape nif-spec.md gives every
  # global symbol — same as `lengcgen`'s `Dl.<lib>.<n>.<main>`. Spelling the
  # suffix out is not optional just because the writer elides it again for the
  # module that owns the symbol: a reader expands a trailing-dot name with the
  # suffix of the file IT is reading, so the elided form is only ever a
  # serialization of a name that was complete in the pool. Interning the elided
  # form instead would collide with any other module's `setlit.0.` that a later
  # `tryLoadSym` pulls into the same pool.
  let s = "`setlit." & $c.counter & "." & c.thisModuleSuffix
  inc c.counter
  result = pool.symId(s)
  var typ = n
  typ = sub(typ)  # throwaway copy; bounds the peek under vpr
  c.constDecls.addParLe("const", info)
  c.constDecls.addSymDef result, info
  c.constDecls.addDotToken() # export
  c.constDecls.addDotToken() # pragmas
  c.constDecls.addSubtree typ
  c.constDecls.addSubtree n
  c.constDecls.addParRi()

proc genSetOp(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let kind = n.exprKind
  let opStart = n
  n = sub(n)
  let typ = n
  if typ.typeKind != SetT:
    error "expected set type for set op", n
  var baseType = typ
  inc baseType
  var argsBuf = createTokenBuf(16)
  swap dest, argsBuf
  let typeStart = dest.len
  trSetType(c, dest, n)
  let aStart = dest.len
  tr(c, dest, n)
  let bStart = dest.len
  if kind == InsetX:
    genSetElem(c, dest, n)
  else:
    tr(c, dest, n)
  swap dest, argsBuf
  n = opStart; skip n
  let cType = cursorAt(argsBuf, typeStart)
  let aOrig = cursorAt(argsBuf, aStart)
  let bOrig = cursorAt(argsBuf, bStart)
  # `b` is a scalar, but `a` is a whole set, so lifting it copies every byte —
  # 32 of them for a `set[char]`, which the expansion below then reads at a
  # single index. A constant set needs no copy at all: it cannot change under
  # `b`'s evaluation and re-reading it is free, so keep it in read-only data,
  # hoisting a bare literal to a `const` to get it there.
  let aIsConst = isConstSym(aOrig) or isLiteralSet(aOrig)
  let liftB = needsTemp(bOrig) or (needsTemp(aOrig) and not aIsConst)
  let liftA = not aIsConst and (needsTemp(aOrig) or needsTemp(bOrig))
  let useTemp = liftA or liftB
  let oldBufStackLen = c.tempUseBufStack.len
  var a: Cursor
  var b = bOrig
  if aIsConst and isLiteralSet(aOrig):
    let s = hoistConstSet(c, aOrig, info)
    c.tempUseBufStack.add createTokenBuf(4)
    c.tempUseBufStack[^1].addSymUse(s, info)
    a = beginRead(c.tempUseBufStack[^1])
  else:
    a = aOrig
  # `pre`: what has to run first; `val`: the value (see `emitValue`).
  var pre = createTokenBuf(16)
  var val = createTokenBuf(16)
  if useTemp:
    # lift both so (n, (n = 123; n)) works
    if liftA:
      a = liftTemp(c, pre, aOrig, typ, info)
    if liftB:
      b = liftTemp(c, pre, bOrig, if kind == InsetX: c.typeCache.builtins.uintType else: typ, info)
  var err = false
  let size = int asSigned(bitsetSizeInBytes(baseType), err)
  assert not err
  case size
  of 1, 2, 4, 8:
    case kind
    of LtsetX:
      copyIntoKind val, AndX, info:
        addTypedOp val, EqX, cType, info:
          addTypedOp val, BitandX, cType, info:
            val.addSubtree a
            addTypedOp val, BitnotX, cType, info:
              val.addSubtree b
          val.addIntLit(0, info)
        addTypedOp val, NeqX, cType, info:
          val.addSubtree a
          val.addSubtree b
    of LesetX:
      addTypedOp val, EqX, cType, info:
        addTypedOp val, BitandX, cType, info:
          val.addSubtree a
          addTypedOp val, BitnotX, cType, info:
            val.addSubtree b
        val.addIntLit(0, info)
    of EqsetX:
      addTypedOp val, EqX, cType, info:
        val.addSubtree a
        val.addSubtree b
    of MulsetX:
      addTypedOp val, BitandX, cType, info:
        val.addSubtree a
        val.addSubtree b
    of PlussetX:
      addTypedOp val, BitorX, cType, info:
        val.addSubtree a
        val.addSubtree b
    of MinussetX:
      addTypedOp val, BitandX, cType, info:
        val.addSubtree a
        addTypedOp val, BitnotX, cType, info:
          val.addSubtree b
    of XorsetX:
      addTypedOp val, BitxorX, cType, info:
        val.addSubtree a
        val.addSubtree b
    of InsetX:
      let mask = size * 8 - 1
      addTypedOp val, NeqX, cType, info:
        addTypedOp val, BitandX, cType, info:
          val.addSubtree a
          addTypedOp val, ShlX, cType, info:
            addTypedOp val, CastX, cType, info:
              val.addIntLit(1, info)
            addUIntTypedOp val, BitandX, -1, info:
              val.addSubtree b
              val.addUIntLit(uint64(mask), info)
        val.addUIntLit(0, info)
    else:
      bug("unreachable")
  else:
    case kind
    of LtsetX, LesetX:
      var resValueBuf = createTokenBuf(2)
      resValueBuf.addParLe(TrueX, info)
      resValueBuf.addParRi()
      let res = liftTemp(c, pre, beginRead(resValueBuf), c.typeCache.builtins.boolType, info)
      var iValueBuf = createTokenBuf(2)
      iValueBuf.addIntLit(0, info)
      let i = liftTemp(c, pre, beginRead(iValueBuf), c.typeCache.builtins.intType, info)
      let exitLab = openRange(c, pre, i, size, info)
      copyIntoKind pre, AsgnS, info:
        pre.addSubtree res
        addUIntTypedOp pre, EqX, 8, info:
          addUIntTypedOp pre, BitandX, 8, info:
            copyIntoKind pre, ArratX, info:
              pre.addSubtree a
              pre.addSubtree i
            addUIntTypedOp pre, BitnotX, 8, info:
              copyIntoKind pre, ArratX, info:
                pre.addSubtree b
                pre.addSubtree i
          pre.addIntLit(0, info)
      openIf c, pre, info
      copyIntoKind pre, NotX, info:
        pre.addSubtree res
      openIfBody pre, info
      emitBreak c, pre, exitLab, info
      closeIf c, pre
      closeRange c, pre, i, exitLab, info
      if kind == LtsetX:
        openIf c, pre, info
        pre.addSubtree res
        openIfBody pre, info
        copyIntoKind pre, AsgnS, info:
          pre.addSubtree res
          addIntTypedOp pre, NeqX, -1, info:
            copyIntoKind pre, CallX, info:
              pre.addSymUse(pool.symId("cmpMem.0." & SystemModuleSuffix), info)
              pre.arrayToPointer(a, info)
              pre.arrayToPointer(b, info)
              pre.addIntLit(size, info)
            pre.addIntLit(0, info)
        closeIf c, pre
      val.addSubtree res
    of EqsetX:
      addIntTypedOp val, EqX, -1, info:
        copyIntoKind val, CallX, info:
          val.addSymUse(pool.symId("cmpMem.0." & SystemModuleSuffix), info)
          val.arrayToPointer(a, info)
          val.arrayToPointer(b, info)
          val.addIntLit(size, info)
        val.addIntLit(0, info)
    of MulsetX, PlussetX, MinussetX, XorsetX:
      var resValueBuf = createTokenBuf(2)
      resValueBuf.addDotToken(info)
      let res = liftTemp(c, pre, beginRead(resValueBuf), cType, info)
      var iValueBuf = createTokenBuf(2)
      iValueBuf.addIntLit(0, info)
      let i = liftTemp(c, pre, beginRead(iValueBuf), c.typeCache.builtins.intType, info)
      let exitLab = openRange(c, pre, i, size, info)
      copyIntoKind pre, AsgnS, info:
        copyIntoKind pre, ArratX, info:
          pre.addSubtree res
          pre.addSubtree i
        let op =
          case kind
          of PlussetX: BitorX
          of XorsetX: BitxorX
          of MulsetX, MinussetX: BitandX
          else: bug("unreachable")
        addUIntTypedOp pre, op, 8, info:
          copyIntoKind pre, ArratX, info:
            pre.addSubtree a
            pre.addSubtree i
          if kind == MinussetX:
            addUIntTypedOp pre, BitnotX, 8, info:
              copyIntoKind pre, ArratX, info:
                pre.addSubtree b
                pre.addSubtree i
          else:
            copyIntoKind pre, ArratX, info:
              pre.addSubtree b
              pre.addSubtree i
      closeRange c, pre, i, exitLab, info
      val.addSubtree res
    of InsetX:
      addUIntTypedOp val, NeqX, 8, info:
        addUIntTypedOp val, BitandX, 8, info:
          copyIntoKind val, ArratX, info:
            val.addSubtree a
            addUIntTypedOp val, ShrX, -1, info:
              val.addSubtree b
              val.addUIntLit(3, info)
          addUIntTypedOp val, ShlX, 8, info:
            val.addUIntLit(1, info)
            addUIntTypedOp val, BitandX, -1, info:
              val.addSubtree b
              val.addUIntLit(7, info)
        val.addUIntLit(0, info)
    else:
      bug("unreachable")
  emitValue c, dest, pre, val, info
  # unconditional: a hoisted set literal parks its symbol use on this stack even
  # when nothing was lifted
  c.tempUseBufStack.shrink(oldBufStackLen)

proc genCard(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let cardStart = n
  n = sub(n)
  let typ = n
  if typ.typeKind != SetT:
    error "expected set type for set op", n
  var baseType = typ
  inc baseType
  var argsBuf = createTokenBuf(16)
  swap dest, argsBuf
  skip n # nothing to do with set type
  let aStart = dest.len
  tr(c, dest, n)
  swap dest, argsBuf
  n = cardStart; skip n
  let a = cursorAt(argsBuf, aStart) # no temp needed
  var err = false
  let size = asSigned(bitsetSizeInBytes(baseType), err)
  assert not err
  case size
  of 1, 2:
    copyIntoKind dest, CallX, info:
      dest.addSymUse(pool.symId("countBits32.0." & SystemModuleSuffix), info)
      addUIntTypedOp dest, CastX, 32, info:
        dest.addSubtree a
  of 4:
    copyIntoKind dest, CallX, info:
      dest.addSymUse(pool.symId("countBits32.0." & SystemModuleSuffix), info)
      dest.addSubtree a
  of 8:
    copyIntoKind dest, CallX, info:
      dest.addSymUse(pool.symId("countBits64.0." & SystemModuleSuffix), info)
      dest.addSubtree a
  else:
    copyIntoKind dest, CallX, info:
      dest.addSymUse(pool.symId("cardSet.0." & SystemModuleSuffix), info)
      dest.arrayToPointer(a, info)
      dest.addIntLit(size, info)

proc genSingleInclSmall(dest: var TokenBuf; s, elem: Cursor; size: int; info: NifLineInfo) =
  let bits = size * 8
  copyIntoKind dest, AsgnS, info:
    dest.addSubtree s
    addUIntTypedOp dest, BitorX, bits, info:
      dest.addSubtree s
      addUIntTypedOp dest, ShlX, bits, info:
        addUIntTypedOp dest, CastX, bits, info:
          dest.addIntLit(1, info)
        addUIntTypedOp dest, ModX, bits, info:
          dest.addSubtree elem
          dest.addUIntLit(uint64(bits), info)

proc genSingleInclBig(dest: var TokenBuf; s, elem: Cursor; info: NifLineInfo) =
  template addLhs() =
    copyIntoKind dest, ArratX, info:
      dest.addSubtree s
      addUIntTypedOp dest, ShrX, -1, info:
        addUIntTypedOp dest, CastX, -1, info:
          dest.addSubtree elem
        dest.addUIntLit(3, info)
  copyIntoKind dest, AsgnS, info:
    addLhs()
    addUIntTypedOp dest, BitorX, 8, info:
      addLhs()
      addUIntTypedOp dest, ShlX, 8, info:
        dest.addUIntLit(1, info)
        addUIntTypedOp dest, BitandX, -1, info:
          addUIntTypedOp dest, CastX, -1, info:
            dest.addSubtree elem
          dest.addUIntLit(7, info)

proc genSetConstrRuntime(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  var pre = createTokenBuf(32) # see `emitValue`
  let constrStart = n # tag
  n = sub(n)
  let typ = n
  skip n
  var elemTyp = typ
  inc elemTyp
  var err = false
  let size = int asSigned(bitsetSizeInBytes(elemTyp), err)
  assert not err
  var typBuf = createTokenBuf(16)
  addSetType typBuf, size, info
  let cType = beginRead(typBuf)
  let big = size > 8
  var resValueBuf = createTokenBuf(2)
  if big: resValueBuf.addDotToken(info)
  else: resValueBuf.addUIntLit(0, info)
  let res = liftTemp(c, pre, beginRead(resValueBuf), cType, info)
  if big:
    copyIntoKind pre, CallS, info:
      pre.addSymUse(pool.symId("zeroMem.0." & SystemModuleSuffix), info)
      pre.arrayToPointer(res, info)
      pre.addIntLit(size, info)
  while n.hasMore:
    let elemInfo = n.info
    if n.substructureKind == RangeU:
      let rangeStart = n
      n = sub(n)
      var argsBuf = createTokenBuf(16)
      let aStart = argsBuf.len
      genSetElem(c, argsBuf, n)
      let bStart = argsBuf.len
      genSetElem(c, argsBuf, n)
      n = rangeStart; skip n
      # a is used once, no need for temp:
      let a = cursorAt(argsBuf, aStart)
      let bOrig = cursorAt(argsBuf, bStart)
      let useTemp = needsTemp(bOrig)
      let b: Cursor
      if useTemp:
        b = liftTemp(c, pre, bOrig, c.typeCache.builtins.uintType, elemInfo)
      else:
        b = bOrig
      let i = liftTemp(c, pre, a, c.typeCache.builtins.uintType, elemInfo)
      let exitLab = openLoop(c, pre, elemInfo)
      addUIntTypedOp pre, LeX, -1, elemInfo:
        pre.addSubtree i
        pre.addSubtree b
      openLoopBody pre, elemInfo
      if big:
        genSingleInclBig(pre, res, i, elemInfo)
      else:
        genSingleInclSmall(pre, res, i, size, elemInfo)
      copyIntoKind pre, AsgnS, elemInfo:
        pre.addSubtree i
        addUIntTypedOp pre, AddX, -1, elemInfo:
          pre.addSubtree i
          pre.addUIntLit(1, elemInfo)
      closeLoop c, pre, exitLab, elemInfo
    else:
      var argsBuf = createTokenBuf(16)
      let aStart = argsBuf.len
      genSetElem(c, argsBuf, n)
      let aOrig = cursorAt(argsBuf, aStart)
      let useTemp = needsTemp(aOrig)
      let a: Cursor
      if useTemp:
        a = liftTemp(c, pre, aOrig, c.typeCache.builtins.uintType, elemInfo)
      else:
        a = aOrig
      if big:
        genSingleInclBig(pre, res, a, elemInfo)
      else:
        genSingleInclSmall(pre, res, a, size, elemInfo)
  n = constrStart; skip n
  var val = createTokenBuf(4)
  val.addSubtree res
  emitValue c, dest, pre, val, info

proc genSetConstr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  var typ = c.typeCache.getType(n)
  var bytes = evalBitSet(n, typ, c.bits)
  case bytes.len
  of 0:
    # not constant
    genSetConstrRuntime(c, dest, n)
  of 1, 2, 4, 8:
    # A word-sized set IS its bit pattern, so the constant folds to one unsigned
    # literal — SUFFIXED with the set's own width. Unsuffixed, the literal is
    # polymorphic and everything downstream has to guess: `xelim`'s `declareTemp`
    # asks `getType` for the type of the `if` expression it is hoisting, gets `u64`
    # off a bare `2u`, and declares a `u64` temp for a `set[RoutineProp]` that is one
    # byte wide. C narrows that on assignment without a word, so the C backend never
    # saw it; nifasm types its registers and rejected the `(mov props.0 (u 8) <-
    # x.16 (u 64))` it became — `derefs.trProcDecl`'s
    # `if …: {IsNoSideEffect} else: {}`, which is what kept `nimony.nim` off the
    # native bootstrap ladder. The suffix is how sem writes literals in
    # `defaults.nim`, and how `hexer/defaultvalues` writes its zeros.
    let width = bytes.len * 8
    bytes.setLen(8)
    dest.addParLe(SufX, info)
    dest.addUIntLit(cast[ptr uint64](addr bytes[0])[], info)
    dest.addStrLit("u" & $width, info)
    dest.addParRi()
    skip n
  else:
    dest.addParLe(AconstrX, info)
    trSetType(c, dest, typ)
    for b in bytes:
      dest.addUIntLit(b, info)
    dest.addParRi()
    skip n

proc genInclExcl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let kind = n.stmtKind
  let inclStart = n
  n = sub(n)
  let typ = n
  if typ.typeKind != SetT:
    error "expected set type for incl/excl", n
  var baseType = typ
  inc baseType
  var argsBuf = createTokenBuf(16)
  swap dest, argsBuf
  let typeStart = dest.len
  trSetType(c, dest, n)
  let aStart = dest.len
  tr(c, dest, n)
  let bStart = dest.len
  tr(c, dest, n)
  swap dest, argsBuf
  n = inclStart; skip n
  let cType = cursorAt(argsBuf, typeStart)
  let aOrig = cursorAt(argsBuf, aStart)
  let bOrig = cursorAt(argsBuf, bStart)
  let useTemp = needsTemp(aOrig) or needsTemp(bOrig)
  let oldBufStackLen = c.tempUseBufStack.len
  let a: Cursor
  let b: Cursor
  if useTemp:
    dest.addParLe(StmtsS, info)
    # lift both so (n, (n = 123; n)) works
    a = liftTempAddr(c, dest, aOrig, typ, info)
    b = liftTemp(c, dest, bOrig, typ.childCursor, info)
  else:
    a = aOrig
    b = bOrig
  var err = false
  let size = asSigned(bitsetSizeInBytes(baseType), err)
  assert not err
  case size
  of 1, 2, 4, 8:
    let mask = size * 8 - 1
    copyIntoKind dest, AsgnS, info:
      dest.addSubtree a
      if kind == InclS:
        dest.addParLe(BitorX, info)
        dest.addSubtree cType
        dest.addSubtree a
      else:
        dest.addParLe(BitandX, info)
        dest.addSubtree cType
        dest.addSubtree a
        dest.addParLe(BitnotX, info)
        dest.addSubtree cType
      addTypedOp dest, ShlX, cType, info:
        addTypedOp dest, CastX, cType, info:
          dest.addIntLit(1, info)
        addUIntTypedOp dest, BitandX, -1, info:
          dest.addSubtree b
          dest.addIntLit(mask, info)
      if kind == InclS:
        dest.addParRi() # bitor
      else:
        dest.addParRi() # bitand
        dest.addParRi() # bitnot
  else:
    template addLhs() =
      copyIntoKind dest, ArratX, info:
        dest.addSubtree a
        addUIntTypedOp dest, ShrX, -1, info:
          addUIntTypedOp dest, CastX, -1, info:
            dest.addSubtree b
          dest.addUIntLit(3)
    copyIntoKind dest, AsgnS, info:
      addLhs()
      addUIntTypedOp dest, if kind == InclS: BitorX else: BitandX, 8, info:
        addLhs()
        if kind == ExclS:
          dest.addParLe BitnotX, info
          dest.addUIntType(8, info)
        addUIntTypedOp dest, ShlX, 8, info:
          dest.addUIntLit(1, info)
          addUIntTypedOp dest, BitandX, -1, info:
            dest.addSubtree b
            dest.addUIntLit(7, info)
        if kind == ExclS:
          dest.addParRi()
  if useTemp:
    dest.addParRi()
    c.tempUseBufStack.shrink(oldBufStackLen)

proc isChainedStringConcatCall(n: Cursor): bool =
  ## True iff the outer call is `string.&` *and* at least one operand is
  ## itself a `string.&` call — i.e. the chain length is at least 2 calls
  ## (>= 3 leaves). A single `a & b` is left for the runtime to handle.
  result = false
  if isStringConcatCall(n):
    var c = n
    inc c                       # past call tag
    skip c                      # past callee
    if isStringConcatCall(c):
      result = true
    else:
      skip c                    # past first arg
      result = isStringConcatCall(c)

proc collectConcatLeaves(c: var Context; leavesBuf: var TokenBuf;
                         leafStarts: var seq[int]; n: var Cursor) =
  ## Walks an arbitrarily-nested chain of `string.&` calls rooted at `n`
  ## and records each non-`&` operand into `leavesBuf`, in left-to-right
  ## order, with `leafStarts` indexing each leaf's beginning. Each leaf is
  ## desugared in-place (full `tr` recursion).
  into n:
    skip n              # past fn symbol
    for _ in 0..1:
      if isStringConcatCall(n):
        collectConcatLeaves(c, leavesBuf, leafStarts, n)
      else:
        leafStarts.add leavesBuf.len
        tr(c, leavesBuf, n)

proc emitLenSum(dest: var TokenBuf; lenSym: SymId;
                leafCursors: openArray[Cursor]; lo, hi: int;
                info: NifLineInfo) =
  ## Emit `len(leaf[lo]) + len(leaf[lo+1]) + ... + len(leaf[hi])`,
  ## left-associated, as a single `int` expression.
  if lo == hi:
    copyIntoKind dest, CallX, info:
      dest.addSymUse(lenSym, info)
      dest.addSubtree leafCursors[lo]
  else:
    addIntTypedOp dest, AddX, -1, info:
      emitLenSum(dest, lenSym, leafCursors, lo, hi-1, info)
      copyIntoKind dest, CallX, info:
        dest.addSymUse(lenSym, info)
        dest.addSubtree leafCursors[hi]

proc genStringConcatChain(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Rewrites `a & b & c & d` (chain of `string.&` calls) into
  ##   (expr
  ##     (var :t0 . . string a)?  ...        # only for side-effectful leaves
  ##     (var :tmp . . string (call newStringOfCap (add (i -1)
  ##                              (call len leaf0) ... (call len leafN))))
  ##     (call add tmp leaf0)
  ##     ...
  ##     (call add tmp leafN)
  ##     tmp)
  ## Side-effectful leaves are lifted to a local first so that `.len` and
  ## the matching `.add` see the same value (no double evaluation).
  let info = n.info
  var leavesBuf = createTokenBuf(64)
  var leafStarts: seq[int] = @[]
  collectConcatLeaves(c, leavesBuf, leafStarts, n)

  let stringType = c.typeCache.builtins.stringType
  let oldBufStackLen = c.tempUseBufStack.len

  var pre = createTokenBuf(32) # see `emitValue`
  var leafCursors = newSeqOfCap[Cursor](leafStarts.len)
  for st in leafStarts:
    let leafOrig = cursorAt(leavesBuf, st)
    if needsTemp(leafOrig):
      leafCursors.add liftTemp(c, pre, leafOrig, stringType, info)
    else:
      leafCursors.add leafOrig

  # Forged symbol names — indices match declaration order across the
  # system module's includes (setops/seqimpl/stringimpl/openarrays). If
  # an overload with the same identifier is inserted earlier in system,
  # these numbers must shift. (`len(string)` is `len.4`, not `.5`: object
  # fields no longer share the global per-name counter, so the `len` field
  # of `seq`/`openArray` no longer pushes the `len` overloads up by one.)
  let newStrSym = pool.symId("newStringOfCap.0." & SystemModuleSuffix)
  let lenSym    = pool.symId("len.4."           & SystemModuleSuffix)
  let addSym    = pool.symId("add.2."           & SystemModuleSuffix)

  let tmp = declareTemp(c, pre, stringType, info)
  copyIntoKind pre, CallX, info:
    pre.addSymUse(newStrSym, info)
    emitLenSum(pre, lenSym, leafCursors, 0, leafCursors.len-1, info)
  pre.addParRi()  # close (var :tmp . . string ...)

  for lc in leafCursors:
    copyIntoKind pre, CallS, info:
      pre.addSymUse(addSym, info)
      # `add.2`'s first parameter is `var string`, so the call site must
      # take the address of `tmp` — `derefs` (in sem) won't see this
      # rewrite, so the wrap has to happen here.
      copyIntoKind pre, HaddrX, info:
        pre.addSymUse(tmp, info)
      pre.addSubtree lc

  var val = createTokenBuf(2)
  val.addSymUse(tmp, info)
  emitValue c, dest, pre, val, info

  c.tempUseBufStack.shrink(oldBufStackLen)

const FoldableFloatExprs = {AddX, SubX, MulX, DivX, NegX, EqX, LeX, LtX}

proc floatOpBits(n: Cursor): int =
  ## `AddX` & co. carry their type as the first child, so one look decides
  ## whether this is a float op and at which width. Returns 0 for anything
  ## that is not one, which is the "do not fold" answer.
  result = 0
  if n.kind == TagLit and n.exprKind in FoldableFloatExprs:
    var ty = sub(n)
    if ty.typeKind == FloatT:
      inc ty
      result = typebits(ty.load)

proc tryFoldFloatExpr(dest: var TokenBuf; exprStart: int; targetBits: int) =
  ## fixes nim-lang/nimony#1626: folds a float expression over compile-time
  ## operands, computing at maximum (float64) precision like `const`
  ## evaluation does. This keeps runtime and `const` results consistent with
  ## doc/language.md's "maximum precision" rule, e.g.
  ## `0.09'f32 + 0.01'f32 == 0.09'f64 + 0.01'f64` folds to `true`.
  ## Runs here in hexer (not in nimsem) so sem only const-evals on demand.
  ## `targetBits` is the target `int` width (for any `shl`/`not` nested in
  ## the expression), distinct from the float width returned below.
  var probe = cursorAt(dest, exprStart)
  let floatBits = floatOpBits(probe)
  if floatBits == 0:
    endRead probe
    return
  # `eval` decides what is constant; it already reports "cannot evaluate"
  # instead of failing, so no structural pre-check is needed here. The
  # SemContext is nil (hexer has none) and `noExecute` keeps it from
  # reaching for one via a sub-compile.
  var ec = initEvalContext(nil, noExecute = true, bits = targetBits)
  var n = probe
  var val = eval(ec, n)
  var isFloat = false
  var f = 0.0
  var isBool = false
  var truthy = false
  if val.kind == FloatLit:
    isFloat = true
    f = val.floatVal
  elif val.kind == TagLit and val.exprKind in {TrueX, FalseX}:
    isBool = true
    truthy = val.exprKind == TrueX
  let info = probe.info
  # release every cursor into `dest` before mutating it:
  endRead n
  endRead val
  endRead probe
  if isFloat and (f != f or f == Inf or f == -Inf):
    # non-finite results stay unfolded; the runtime computes the same
    # Inf/NaN and we avoid encoding them as raw FloatLits
    return
  if not (isFloat or isBool): return
  expectUnique dest
  shrink dest, exprStart
  if isFloat:
    dest.addParLe(SufX, info)
    dest.addFloatLit(f, info)
    dest.addStrLit("f" & $floatBits, info)
    dest.addParRi()
  else:
    dest.addParLe(if truthy: TrueX else: FalseX, info)
    dest.addParRi()

proc isClosureValueType(typ: Cursor): bool =
  ## The (fn, env) pair: the lowered `ClosureTupleT`, a `.closure` proctype whose
  ## decl has not been rewritten yet, or a `.closure` iterator. A `.passive`
  ## iterator is NOT one — it lowers to a bare wrapper proctype.
  typ.typeKind == ClosureTupleT or
    (typ.typeKind == ProctypeT and procHasPragma(typ, ClosureP)) or
    (typ.typeKind == ItertypeT and not procHasPragma(typ, PassiveP))


proc tryClosureCompare(c: var Context; dest: var TokenBuf; n: var Cursor): bool =
  ## `(eq|neq <closure> a b)` -> compare the fn slots AND the env slots.
  ##
  ## Both halves, because two closures are equal when they are the same
  ## function over the same environment — the same `proc` captured twice is
  ## not the same closure. Against `nil` this still does the right thing: a
  ## nil closure is nil in both slots, so the added env test is true for the
  ## nil literal and cannot make a non-nil closure compare equal to it.
  let head = n
  var probe = n
  probe = sub(probe)
  let typ = probe
  if not isClosureValueType(typ): return false

  let info = head.info
  let isNeq = head.exprKind == NeqX
  skip probe                     # past the type
  var lhs = probe; skip probe
  var rhs = probe

  # (and (eq fn fn) (eq env env))   /   (or (neq …) (neq …)) for `!=`
  let outer = if isNeq: OrX else: AndX
  let inner = if isNeq: NeqX else: EqX
  copyIntoKind dest, outer, info:
    for slot in 0..1:
      # `(pointer)` for both halves: the fn slot is a function pointer and the
      # env slot a `ref RootObj`, and identity is all either comparison means.
      copyIntoKind dest, inner, info:
        dest.addParPair PointerT, info
        for side in [lhs, rhs]:
          var e = side
          dest.copyIntoKind TupatX, info:
            tr(c, dest, e)
            dest.addIntLit slot, info
  n = head; skip n
  result = true

proc trFloatArith(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Emits an arithmetic/comparison node normally, then attempts a
  ## max-precision constant fold over the finished subtree. Children fold
  ## first (inside `trSons`), so nesting works bottom-up.
  let start = dest.len
  trSons(c, dest, n)
  tryFoldFloatExpr(dest, start, c.bits)

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # Simplify (expr (expr ...)) to (expr (...)) so that our
  # controlflow graph can handle them easily:
  dest.addParLe(n.cursorTagId, n.info)
  var scopes: seq[Cursor] = @[]
  scopes.add n; n = sub(n)
  while n.exprKind == ExprX:
    scopes.add n; n = sub(n)
  while n.hasMore:
    tr(c, dest, n)
  dest.addParRi()
  while scopes.len > 0:
    n = scopes.pop(); skip n, SkipFull

proc trTupleAsgn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Lower `(a, b, ...) = rhs` (LHS is `tup`/`tupconstr`) into:
  ##   (stmts (var :tmp <tupleType> <rhs>)
  ##          (asgn a (tupat tmp 0))
  ##          (asgn b (tupat tmp 1))
  ##          ...)
  ## NIFC rejects the naive `(asgn (oconstr ...) rhs)` because an oconstr
  ## is not a valid L-value, so the destructuring assignment must be
  ## broken apart before codegen sees it.
  let info = n.info
  let asgnStart = n # past `asgn` tag
  n = sub(n)
  let lhsTagInfo = n.info
  let lhsStart = n # past LHS `tup`/`tupconstr` tag
  n = sub(n)
  # The tuple constructor's first child is the type (a `(tuple ...)`
  # subtree); the remaining children are the actual element expressions.
  let tupleType = n
  skip n
  var lhsItems: seq[Cursor] = @[]
  while n.hasMore:
    lhsItems.add n
    skip n, SkipExpr
  n = lhsStart; skip n # past LHS close

  dest.addParLe StmtsS, info

  let tmp = declareTemp(c, dest, tupleType, lhsTagInfo)
  tr c, dest, n       # the RHS is the var's initial value (a bound temp's
                      # symbol, too, under the Final IR)
  dest.addParRi()     # close `(var ...)`

  n = asgnStart; skip n # close original `(asgn ...)`

  for i in 0 ..< lhsItems.len:
    var lhsLocal = lhsItems[i]
    dest.addParLe AsgnS, info
    tr c, dest, lhsLocal
    dest.addParLe TupatX, info
    dest.addSymUse(tmp, info)
    dest.addIntLit(i, info)
    dest.addParRi() # close tupat
    dest.addParRi() # close asgn

  dest.addParRi()     # close stmts

proc emitCheckedIndex(c: var Context; dest: var TokenBuf; chk: var TokenBuf;
                      isUnsigned: bool; info: NifLineInfo) =
  ## The check call `chk` as an index. Under the Final IR it is bound to an
  ## `{.inline.}` temp in front of the statement — the shape `xelim` used to
  ## give it, and the one the intra-module inliner can splice. (An `and`/`or`
  ## operand keeps its statements to itself: `trShortCircuit`.)
  if c.fir:
    let tmp = pool.symId("`desugar." & $c.counter)
    inc c.counter
    copyIntoKind c.pre, LetS, info:
      c.pre.addSymDef tmp, info
      c.pre.addDotToken() # no export marker
      copyIntoKind c.pre, PragmasS, info:
        copyIntoKind c.pre, InlineP, info: discard
      if isUnsigned:
        c.pre.addUIntType(-1, info)
      else:
        c.pre.addIntType(-1, info)
      c.pre.add chk
    dest.addSymUse tmp, info
  else:
    dest.add chk

proc trArrAt(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Lower the array-index bound check here rather than in `nifcgen`. Sem
  ## attaches the (compile-time) bounds to `(arrat arr idx [hi [lo]])`; we
  ## rewrite that to `(arrat arr <checkedIndex>)` where `<checkedIndex>` is
  ## either a `(call nimIcheckAB …)`/`(call nimIcheckB …)` (bound checks on)
  ## or the bare/`(sub …)`-adjusted index (checks off). Doing it in desugar —
  ## before the `xelim` passes — lets `xelim` hoist the check call into a
  ## `(var :tmp … (call …))`, which is the only shape the intra-module
  ## inliner can splice; emitted late in `nifcgen` the call stays buried in
  ## the `(at …)` index expression and never gets inlined.
  let info = n.info
  dest.addParLe(ArratX, info)
  n.into:
    tr(c, dest, n)  # array operand
    # `isUnsigned` is decided from the index's type, exactly as nifcgen did.
    let isUnsigned = getType(c.typeCache, n).typeKind in {UIntT, CharT}
    var idxBuf = createTokenBuf(8)
    tr(c, idxBuf, n)
    if n.hasMore:
      # `(arrat arr idx hi [lo])` — `hi` is the inclusive upper bound, `lo`
      # the optional lower bound. nimIcheckAB(i, a, b) wants (i, lo, hi).
      var hiBuf = createTokenBuf(8)
      tr(c, hiBuf, n)
      if n.hasMore:
        var loBuf = createTokenBuf(8)
        tr(c, loBuf, n)
        if BoundCheck in c.activeChecks:
          let p = pool.symId(
            (if isUnsigned: "nimUcheckAB" else: "nimIcheckAB") & ".0." & SystemModuleSuffix)
          var chk = createTokenBuf(16)
          copyIntoKind chk, CallX, info:
            chk.addSymUse p, info
            chk.add idxBuf
            chk.add loBuf
            chk.add hiBuf
          emitCheckedIndex c, dest, chk, isUnsigned, info
        else:
          # The subtraction is needed regardless of checks: NIFC arrays are
          # zero-based, so a `lo..hi` Nim array indexes at `i - lo`.
          if isUnsigned:
            addUIntTypedOp dest, SubX, -1, info:
              dest.add idxBuf
              dest.add loBuf
          else:
            addIntTypedOp dest, SubX, -1, info:
              dest.add idxBuf
              dest.add loBuf
      else:
        if BoundCheck in c.activeChecks:
          let p = pool.symId(
            (if isUnsigned: "nimUcheckB" else: "nimIcheckB") & ".0." & SystemModuleSuffix)
          var chk = createTokenBuf(16)
          copyIntoKind chk, CallX, info:
            chk.addSymUse p, info
            chk.add idxBuf
            chk.add hiBuf
          emitCheckedIndex c, dest, chk, isUnsigned, info
        else:
          dest.add idxBuf
    else:
      dest.add idxBuf
    dest.addParRi(n.endInfo)

proc trShortCircuit(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## `a and b` / `a or b`. The right operand may not run, so whatever its
  ## translation wants to run first (`pre`) cannot go in front of the whole
  ## statement. When there is any, the operator is materialized:
  ##
  ##   var t = a
  ##   if t: <b's pre>; t = b          # `or`: if not t
  ##   ... t ...
  if not c.fir:
    trSons(c, dest, n)
    return
  let info = n.info
  let tag = n.cursorTagId
  let isAnd = n.exprKind == AndX
  var a = createTokenBuf(8)
  var b = createTokenBuf(8)
  var bPre = createTokenBuf(0)
  n.into:
    tr(c, a, n) # always runs: its `pre` stays where it is
    swap bPre, c.pre
    tr(c, b, n)
    swap bPre, c.pre
  if bPre.len == 0:
    dest.addParLe(tag, info)
    dest.add a
    dest.add b
    dest.addParRi()
  else:
    var stmts = createTokenBuf(32)
    let t = declareTemp(c, stmts, c.typeCache.builtins.boolType, info)
    stmts.add a
    stmts.addParRi()
    openIf c, stmts, info
    if isAnd:
      stmts.addSymUse t, info
    else:
      copyIntoKind stmts, NotX, info:
        stmts.addSymUse t, info
    openIfBody stmts, info
    stmts.add bPre
    copyIntoKind stmts, AsgnS, info:
      stmts.addSymUse t, info
      stmts.add b
    closeIf c, stmts
    c.pre.add stmts
    dest.addSymUse t, info

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor; isTopScope = false) =
  case n.kind
  of DotToken, UnknownToken, EofToken, ParLe, ParRi, ExtendedSuffix, LineInfoLit, Ident, Symbol, SymbolDef, IntLit, UIntLit, FloatLit, CharLit, StrLit:
    takeTree dest, n
  of TagLit:
    case n.exprKind
    of NoExpr:
      case n.stmtKind
      of NoStmt:
        case n.typeKind
        of SetT:
          #trSetType(c, dest, n)
          # leave this to nifcgen
          trSons(c, dest, n)
        of ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT,
            ConverterT, MethodT, MacroT, TemplateT, ObjectT,
            EnumT, ProctypeT, IT, UT, FT, CT, BoolT, VoidT,
            PtrT, ArrayT, VarargsT, StaticT, TupleT, ClosureTupleT, OnumT,
            AnumT, RefT, MutT, OutT, LentT, SinkT, NiltT,
            ConceptT, DistinctT, ItertypeT, RangetypeT, UarrayT,
            AutoT, SymkindT, TypekindT, TypedescT, UntypedT,
            TypedT, CstringT, PointerT, OrdinalT, PluginCallT, NoType:
          trSons(c, dest, n)
      of InclS, ExclS:
        genInclExcl(c, dest, n)
      of CaseS:
        copyInto dest, n:
          while n.hasMore:
            case n.substructureKind
            of OfU:
              copyInto dest, n:
                takeTree dest, n # keep set constructor
                tr(c, dest, n)
            of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
                TypevarU, StaticTypevarU, EfldU, FldU, WhenU, ElifU, ElseU,
                TypevarsU, CaseU, StmtsU, ParamsU, PragmasU,
                EitherU, JoinU, UnpackflatU, UnpacktupU, ExceptU,
                FinU, UncheckedU, GfldU, CallargsU, ForcallU, DeferexpansionU, NeedtypesU, NoSub:
              tr(c, dest, n)
      of LocalDecls:
        trLocal c, dest, n
      of ProcS, FuncS, MethodS, ConverterS:
        trProc c, dest, n
      of IteratorS:
        # An INLINE iterator's decl is dead by the time desugar runs —
        # `elimForLoops` spliced its body into each caller before this pass, and
        # what is left reaches no back end — so lowering it would be wasted work,
        # which is why iterators sat with the macros and templates below.
        #
        # A COROUTINE iterator is not that. `.closure` (and `.passive`) survive as
        # real routines: lambdalifting and cps turn the body into a state machine
        # and it goes all the way to `lengcgen`. Skipping it meant nothing ever
        # lowered the constructs desugar owns, so a `set` literal in a closure
        # iterator reached the back end as a live `(setconstr …)` and died there
        # ("BUG: not eliminated"). The same held for every other form this pass is
        # responsible for — set operations, `card`, `incl`/`excl`.
        if isCoroutine(n):
          trProc c, dest, n
        else:
          takeTree dest, n
      of MacroS, TemplateS, EmitS, BreakS, ContinueS,
        ForS, IncludeS, ImportS, FromimportS, ImportexceptS,
        ExportS, CommentS,
        PragmasS, LabS, JmpS:
        takeTree dest, n
      of TypeS:
        if isTopScope:
          takeTree dest, n
        else:
          takeTree c.pending, n
      of ScopeS:
        c.typeCache.openScope()
        trStmtList(c, dest, n)
        c.typeCache.closeScope()
      of StmtsS:
        trStmtList(c, dest, n, isTopScope = isTopScope)
      of AsgnS:
        # Tuple-LHS assignments need to be split into per-field stores;
        # otherwise NIFC chokes on `(asgn (tupconstr ...) ...)`.
        var peek = n
        inc peek
        if peek.exprKind in {TupX, TupconstrX}:
          trTupleAsgn(c, dest, n)
        else:
          trSons(c, dest, n)
      of CallS, CmdS, BlockS, IfS, WhenS, WhileS, CoroforS, RetS,
          YldS, PragmaxS, ImportasS, ExportexceptS, DiscardS,
          TryS, RaiseS, UnpackdeclS, AssumeS, AssertS,
          CallstrlitS, InfixS, PrefixS, HcallS, StaticstmtS,
          BindS, MixinS, UsingS, AsmS, DeferS:
        trSons(c, dest, n)
    of SetconstrX:
      genSetConstr(c, dest, n)
    of PlussetX, MinussetX, MulsetX, XorsetX, EqsetX, LesetX, LtsetX, InsetX:
      genSetOp(c, dest, n)
    of CardX:
      genCard(c, dest, n)
    of TypeofX:
      takeTree dest, n
    of ArratX:
      trArrAt(c, dest, n)
    of DdotX:
      dest.addParLe("dot", n.info)
      dest.addParLe("deref", n.info)
      n.into: # skip tag
        tr c, dest, n
        dest.addParRi() # deref
        tr c, dest, n
        tr c, dest, n # inheritance depth
        if n.isStringLit:
          # drop optional access-token marker; no visibility in NIFC.
          skip n
        dest.addParRi(n.endInfo)
    of ExprX:
      trExpr c, dest, n
    of CallX, CallstrlitX, CmdX, PrefixX, InfixX, HcallX:
      # CallKinds — check for a foldable chain of `string.&` before
      # falling back to the generic son-recursion path.
      if isChainedStringConcatCall(n):
        genStringConcatChain(c, dest, n)
      else:
        trSons(c, dest, n)
    of EqX, NeqX:
      # A `.closure` value is an (fn, env) pair, so C's `==` cannot compare it:
      # the operands are structs. Project the halves and compare those instead.
      #
      # This belongs HERE, on the typed `(eq <type> a b)` node, because the type
      # is right there — no need to guess from the shape of the operand
      # expression. An earlier attempt did exactly that, in coro_transform,
      # by intercepting the `(hconv (pointer …) g)` that sem used to emit for
      # `g == nil` and pattern-matching the inner expression against a list of
      # value shapes (symbol, field, tupat…). Every new shape that could hold a
      # closure — a seq element read through an inlined `for`, say — had to be
      # added to that list. Comparing is the comparator's job.
      if not tryClosureCompare(c, dest, n):
        trFloatArith(c, dest, n)
    of AddX, SubX, MulX, DivX, NegX, LeX, LtX:
      trFloatArith(c, dest, n)
    of AndX, OrX:
      trShortCircuit(c, dest, n)
    of ErrX, SufX, AtX, DerefX, DotX, PatX, ParX, AddrX, NilX,
        InfX, NeginfX, NanX, FalseX, TrueX, XorX,
        NotX, SizeofX, AlignofX, OffsetofX, OconstrX,
        AconstrX, BracketX, CurlyX, CurlyatX, OvfX,
        ModX, ShrX, ShlX, BitandX, BitorX, BitxorX,
        BitnotX, CastX,
        CchoiceX, OchoiceX, PragmaxX, QuotedX, HderefX,
        HaddrX, NewrefX, NewobjX, TupX, TupconstrX, TabconstrX,
        AshrX, BaseobjX, DconvX, HconvX, ConvX,
        CompilesX, DeclaredX, DefinedX, ProccallX, DelayX,
        AstToStrX, BindSymX, BindSymNameX, InstanceofX, HighX, LowX, UnpackX,
        FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX, InstantiationinfoX,
        DefaultobjX, DefaulttupX, DefaultdistinctX,
        Delay0X, SuspendX, DoX, TupatX, EmoveX,
        DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX,
        InternalTypeNameX, InternalFieldPairsX, FailedX, IsX,
        EnvpX, KvX, ToClosureX, PluginCallX:
      trSons(c, dest, n)
  else:
    bug "unexpected ')' inside"

proc desugar*(pass: var Pass; activeChecks: set[CheckMode]) =
  var n = pass.n  # Extract cursor locally
  var c = Context(counter: 0, typeCache: createTypeCache(pass.bits), thisModuleSuffix: pass.moduleSuffix, activeChecks: activeChecks, pending: createTokenBuf(), constDecls: createTokenBuf(), bits: pass.bits,
                  fir: hexerSpeaksFir(), pre: createTokenBuf())
  c.typeCache.openScope()
  # Process the root `(stmts` manually (mirroring trSons' copyInto) but
  # keep it OPEN until `pending` has been appended: an emitted close
  # cannot be rolled back under `-d:virtualParRi` (it seals the tag and
  # is elided), so the old "close, shrink away, re-close" dance is
  # impossible.
  assert n.stmtKind == StmtsS
  let rootTag = n.cursorTagId
  let rootInfo = n.info
  # The body is built aside so the `const`s hoisted out of it can be emitted
  # ahead of it: a `const` the C backend meets only after the function reading
  # it is a use before declaration.
  var body = createTokenBuf()
  n.into:
    while n.hasMore:
      trStmt c, body, n, isTopScope = true

  pass.dest.addParLe(rootTag, rootInfo)
  pass.dest.add c.constDecls
  pass.dest.add body
  pass.dest.add c.pending
  pass.dest.addParRi()

  c.typeCache.closeScope()
