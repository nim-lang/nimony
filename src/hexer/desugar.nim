# removes abstractions like set ops and ref object constructors

when defined(nimony):
  {.feature: "untyped".}
else:
  {.pragma: untyped.}

import std / [assertions, tables, hashes, sets, syncio]
from std / strutils import startsWith
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, expreval, xints, builtintypes, langmodes, renderer, reporters]
import hexer_context, passes
include ".." / nimony / nif_annotations

type
  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    tempUseBufStack: seq[TokenBuf]
    activeChecks: set[CheckMode]
    pending: TokenBuf
    hoisted: TokenBuf
      ## module-level `const`s minted for set literals; emitted before the
      ## module body so the C backend sees them declared before their uses
    bits: int  ## target `int` width, handed to the const evaluator

proc declareTemp(c: var Context; dest: var TokenBuf; typ: Cursor; info: NifLineInfo): SymId =
  let s = "`desugar." & $c.counter
  inc c.counter
  result = pool.syms.getOrIncl(s)
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
        FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX,
        DefaultobjX, DefaulttupX, DefaultdistinctX, DelayX,
        Delay0X, SuspendX, DoX, PlussetX, MinussetX, MulsetX,
        XorsetX, EqsetX, LesetX, LtsetX, InsetX, CardX, EmoveX,
        DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX,
        InternalTypeNameX, InternalFieldPairsX, FailedX, IsX,
        EnvpX, KvX, ToClosureX, NoExpr:
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

proc trProcBody(c: var Context; dest: var TokenBuf; n: var Cursor) =
  n.into:
    while n.hasMore:
      tr(c, dest, n)

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
  dest.copyIntoKind IfS, info:
    dest.copyIntoKind ElifU, info:
      dest.copyIntoKind NotX, info:
        var n = cond
        tr(c, dest, n)
      dest.copyIntoKind StmtsS, info:
        dest.copyIntoKind CallS, info:
          dest.addSymUse pool.syms.getOrIncl("panic.0." & SystemModuleSuffix), info
          dest.addStrLit msg, info

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
      trRequires(c, dest, pragmas)
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

template forRangeExclusive(c: var Context; dest: var TokenBuf; i: Cursor; bound: int; info: NifLineInfo; body: typed) {.untyped.} =
  copyIntoKind dest, WhileS, info:
    addIntTypedOp dest, LtX, -1, info:
      dest.addSubtree i
      dest.addIntLit(bound, info)
    copyIntoKind dest, StmtsS, info:
      body
      copyIntoKind dest, AsgnS, info:
        dest.addSubtree i
        addIntTypedOp dest, AddX, -1, info:
          dest.addSubtree i
          dest.addIntLit(1, info)

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
  result = pool.syms.getOrIncl(s)
  var typ = n
  typ = sub(typ)  # throwaway copy; bounds the peek under vpr
  c.hoisted.addParLe("const", info)
  c.hoisted.addSymDef result, info
  c.hoisted.addDotToken() # export
  c.hoisted.addDotToken() # pragmas
  c.hoisted.addSubtree typ
  c.hoisted.addSubtree n
  c.hoisted.addParRi()

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
  if useTemp:
    dest.addParLe(ExprX, info)
    # lift both so (n, (n = 123; n)) works
    if liftA:
      a = liftTemp(c, dest, aOrig, typ, info)
    if liftB:
      b = liftTemp(c, dest, bOrig, if kind == InsetX: c.typeCache.builtins.uintType else: typ, info)
  var err = false
  let size = int asSigned(bitsetSizeInBytes(baseType), err)
  assert not err
  case size
  of 1, 2, 4, 8:
    case kind
    of LtsetX:
      copyIntoKind dest, AndX, info:
        addTypedOp dest, EqX, cType, info:
          addTypedOp dest, BitandX, cType, info:
            dest.addSubtree a
            addTypedOp dest, BitnotX, cType, info:
              dest.addSubtree b
          dest.addIntLit(0, info)
        addTypedOp dest, NeqX, cType, info:
          dest.addSubtree a
          dest.addSubtree b
    of LesetX:
      addTypedOp dest, EqX, cType, info:
        addTypedOp dest, BitandX, cType, info:
          dest.addSubtree a
          addTypedOp dest, BitnotX, cType, info:
            dest.addSubtree b
        dest.addIntLit(0, info)
    of EqsetX:
      addTypedOp dest, EqX, cType, info:
        dest.addSubtree a
        dest.addSubtree b
    of MulsetX:
      addTypedOp dest, BitandX, cType, info:
        dest.addSubtree a
        dest.addSubtree b
    of PlussetX:
      addTypedOp dest, BitorX, cType, info:
        dest.addSubtree a
        dest.addSubtree b
    of MinussetX:
      addTypedOp dest, BitandX, cType, info:
        dest.addSubtree a
        addTypedOp dest, BitnotX, cType, info:
          dest.addSubtree b
    of XorsetX:
      addTypedOp dest, BitxorX, cType, info:
        dest.addSubtree a
        dest.addSubtree b
    of InsetX:
      let mask = size * 8 - 1
      addTypedOp dest, NeqX, cType, info:
        addTypedOp dest, BitandX, cType, info:
          dest.addSubtree a
          addTypedOp dest, ShlX, cType, info:
            addTypedOp dest, CastX, cType, info:
              dest.addIntLit(1, info)
            addUIntTypedOp dest, BitandX, -1, info:
              dest.addSubtree b
              dest.addUIntLit(uint64(mask), info)
        dest.addUIntLit(0, info)
    else:
      bug("unreachable")
  else:
    case kind
    of LtsetX, LesetX:
      dest.addParLe(ExprX, info)
      var resValueBuf = createTokenBuf(2)
      resValueBuf.addParLe(TrueX, info)
      resValueBuf.addParRi()
      let res = liftTemp(c, dest, beginRead(resValueBuf), c.typeCache.builtins.boolType, info)
      var iValueBuf = createTokenBuf(2)
      iValueBuf.addIntLit(0, info)
      let i = liftTemp(c, dest, beginRead(iValueBuf), c.typeCache.builtins.intType, info)
      forRangeExclusive c, dest, i, size, info:
        copyIntoKind dest, AsgnS, info:
          dest.addSubtree res
          addUIntTypedOp dest, EqX, 8, info:
            addUIntTypedOp dest, BitandX, 8, info:
              copyIntoKind dest, ArratX, info:
                dest.addSubtree a
                dest.addSubtree i
              addUIntTypedOp dest, BitnotX, 8, info:
                copyIntoKind dest, ArratX, info:
                  dest.addSubtree b
                  dest.addSubtree i
            dest.addIntLit(0, info)
        copyIntoKind dest, IfS, info:
          copyIntoKind dest, ElifU, info:
            copyIntoKind dest, NotX, info:
              dest.addSubtree res
            copyIntoKind dest, StmtsS, info:
              copyIntoKind dest, BreakS, info:
                dest.addDotToken()
      if kind == LtsetX:
        copyIntoKind dest, IfS, info:
          copyIntoKind dest, ElifU, info:
            dest.addSubtree res
            copyIntoKind dest, StmtsS, info:
              copyIntoKind dest, AsgnS, info:
                dest.addSubtree res
                addIntTypedOp dest, NeqX, -1, info:
                  copyIntoKind dest, CallX, info:
                    dest.addSymUse(pool.syms.getOrIncl("cmpMem.0." & SystemModuleSuffix), info)
                    dest.arrayToPointer(a, info)
                    dest.arrayToPointer(b, info)
                    dest.addIntLit(size, info)
                  dest.addIntLit(0, info)
      dest.addSubtree res
      dest.addParRi()
    of EqsetX:
      addIntTypedOp dest, EqX, -1, info:
        copyIntoKind dest, CallX, info:
          dest.addSymUse(pool.syms.getOrIncl("cmpMem.0." & SystemModuleSuffix), info)
          dest.arrayToPointer(a, info)
          dest.arrayToPointer(b, info)
          dest.addIntLit(size, info)
        dest.addIntLit(0, info)
    of MulsetX, PlussetX, MinussetX, XorsetX:
      dest.addParLe(ExprX, info)
      var resValueBuf = createTokenBuf(2)
      resValueBuf.addDotToken(info)
      let res = liftTemp(c, dest, beginRead(resValueBuf), cType, info)
      var iValueBuf = createTokenBuf(2)
      iValueBuf.addIntLit(0, info)
      let i = liftTemp(c, dest, beginRead(iValueBuf), c.typeCache.builtins.intType, info)
      forRangeExclusive c, dest, i, size, info:
        copyIntoKind dest, AsgnS, info:
          copyIntoKind dest, ArratX, info:
            dest.addSubtree res
            dest.addSubtree i
          let op =
            case kind
            of PlussetX: BitorX
            of XorsetX: BitxorX
            of MulsetX, MinussetX: BitandX
            else: bug("unreachable")
          addUIntTypedOp dest, op, 8, info:
            copyIntoKind dest, ArratX, info:
              dest.addSubtree a
              dest.addSubtree i
            if kind == MinussetX:
              addUIntTypedOp dest, BitnotX, 8, info:
                copyIntoKind dest, ArratX, info:
                  dest.addSubtree b
                  dest.addSubtree i
            else:
              copyIntoKind dest, ArratX, info:
                dest.addSubtree b
                dest.addSubtree i
      dest.addSubtree res
      dest.addParRi()
    of InsetX:
      addUIntTypedOp dest, NeqX, 8, info:
        addUIntTypedOp dest, BitandX, 8, info:
          copyIntoKind dest, ArratX, info:
            dest.addSubtree a
            addUIntTypedOp dest, ShrX, -1, info:
              dest.addSubtree b
              dest.addUIntLit(3, info)
          addUIntTypedOp dest, ShlX, 8, info:
            dest.addUIntLit(1, info)
            addUIntTypedOp dest, BitandX, -1, info:
              dest.addSubtree b
              dest.addUIntLit(7, info)
        dest.addUIntLit(0, info)
    else:
      bug("unreachable")
  if useTemp:
    dest.addParRi()
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
      dest.addSymUse(pool.syms.getOrIncl("countBits32.0." & SystemModuleSuffix), info)
      addUIntTypedOp dest, CastX, 32, info:
        dest.addSubtree a
  of 4:
    copyIntoKind dest, CallX, info:
      dest.addSymUse(pool.syms.getOrIncl("countBits32.0." & SystemModuleSuffix), info)
      dest.addSubtree a
  of 8:
    copyIntoKind dest, CallX, info:
      dest.addSymUse(pool.syms.getOrIncl("countBits64.0." & SystemModuleSuffix), info)
      dest.addSubtree a
  else:
    copyIntoKind dest, CallX, info:
      dest.addSymUse(pool.syms.getOrIncl("cardSet.0." & SystemModuleSuffix), info)
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
  dest.addParLe(ExprX, info)
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
  let res = liftTemp(c, dest, beginRead(resValueBuf), cType, info)
  if big:
    copyIntoKind dest, CallX, info:
      dest.addSymUse(pool.syms.getOrIncl("zeroMem.0." & SystemModuleSuffix), info)
      dest.arrayToPointer(res, info)
      dest.addIntLit(size, info)
  while n.hasMore:
    let elemInfo = n.info
    if n.substructureKind == RangeU:
      let rangeStart = n
      n = sub(n)
      var argsBuf = createTokenBuf(16)
      swap dest, argsBuf
      let aStart = dest.len
      genSetElem(c, dest, n)
      let bStart = dest.len
      genSetElem(c, dest, n)
      swap dest, argsBuf
      n = rangeStart; skip n
      # a is used once, no need for temp:
      let a = cursorAt(argsBuf, aStart)
      let bOrig = cursorAt(argsBuf, bStart)
      let useTemp = needsTemp(bOrig)
      let b: Cursor
      if useTemp:
        b = liftTemp(c, dest, bOrig, c.typeCache.builtins.uintType, elemInfo)
      else:
        b = bOrig
      let i = liftTemp(c, dest, a, c.typeCache.builtins.uintType, elemInfo)
      copyIntoKind dest, WhileS, elemInfo:
        addUIntTypedOp dest, LeX, -1, elemInfo:
          dest.addSubtree i
          dest.addSubtree b
        copyIntoKind dest, StmtsS, elemInfo:
          if big:
            genSingleInclBig(dest, res, i, elemInfo)
          else:
            genSingleInclSmall(dest, res, i, size, elemInfo)
          copyIntoKind dest, AsgnS, elemInfo:
            dest.addSubtree i
            addUIntTypedOp dest, AddX, -1, elemInfo:
              dest.addSubtree i
              dest.addUIntLit(1, elemInfo)
    else:
      var argsBuf = createTokenBuf(16)
      swap dest, argsBuf
      let aStart = dest.len
      genSetElem(c, dest, n)
      swap dest, argsBuf
      let aOrig = cursorAt(argsBuf, aStart)
      let useTemp = needsTemp(aOrig)
      let a: Cursor
      if useTemp:
        a = liftTemp(c, dest, aOrig, c.typeCache.builtins.uintType, elemInfo)
      else:
        a = aOrig
      if big:
        genSingleInclBig(dest, res, a, elemInfo)
      else:
        genSingleInclSmall(dest, res, a, size, elemInfo)
  n = constrStart; skip n
  dest.addSubtree res
  dest.addParRi()

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

proc isConcat(s: SymId): bool =
  let res = tryLoadSym(s)
  if res.status != LacksNothing or not isRoutine(res.decl.symKind):
    return false
  let routine = asRoutine(res.decl)
  result = hasPragmaOfValue(routine.pragmas, SemanticsP, "string.&")

proc isStringConcatCall(n: Cursor): bool =
  # Non-mutating peek: cannot use `into` here because the body would have
  # to consume every child (the callee plus both args) just to satisfy the
  # closing-ParRi assertion — wasteful for a one-token check.
  result = false
  if n.exprKind in CallKinds:
    var c = n
    inc c                       # past call tag
    if c.kind == Symbol and startsWith(pool.syms[c.symId], "&."):
      result = isConcat(c.symId)

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

  dest.addParLe(ExprX, info)

  var leafCursors = newSeqOfCap[Cursor](leafStarts.len)
  for st in leafStarts:
    let leafOrig = cursorAt(leavesBuf, st)
    if needsTemp(leafOrig):
      leafCursors.add liftTemp(c, dest, leafOrig, stringType, info)
    else:
      leafCursors.add leafOrig

  # Forged symbol names — indices match declaration order across the
  # system module's includes (setops/seqimpl/stringimpl/openarrays). If
  # an overload with the same identifier is inserted earlier in system,
  # these numbers must shift. (`len(string)` is `len.4`, not `.5`: object
  # fields no longer share the global per-name counter, so the `len` field
  # of `seq`/`openArray` no longer pushes the `len` overloads up by one.)
  let newStrSym = pool.syms.getOrIncl("newStringOfCap.0." & SystemModuleSuffix)
  let lenSym    = pool.syms.getOrIncl("len.4."           & SystemModuleSuffix)
  let addSym    = pool.syms.getOrIncl("add.2."           & SystemModuleSuffix)

  let tmp = declareTemp(c, dest, stringType, info)
  copyIntoKind dest, CallX, info:
    dest.addSymUse(newStrSym, info)
    emitLenSum(dest, lenSym, leafCursors, 0, leafCursors.len-1, info)
  dest.addParRi()  # close (var :tmp . . string ...)

  for lc in leafCursors:
    copyIntoKind dest, CallS, info:
      dest.addSymUse(addSym, info)
      # `add.2`'s first parameter is `var string`, so the call site must
      # take the address of `tmp` — `derefs` (in sem) won't see this
      # rewrite, so the wrap has to happen here.
      copyIntoKind dest, HaddrX, info:
        dest.addSymUse(tmp, info)
      dest.addSubtree lc

  dest.addSymUse(tmp, info)
  dest.addParRi()  # close (expr ...)

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
  trExpr c, dest, n   # serialise the RHS as the var's initial value
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
          let p = pool.syms.getOrIncl(
            (if isUnsigned: "nimUcheckAB" else: "nimIcheckAB") & ".0." & SystemModuleSuffix)
          copyIntoKind dest, CallX, info:
            dest.addSymUse p, info
            dest.add idxBuf
            dest.add loBuf
            dest.add hiBuf
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
          let p = pool.syms.getOrIncl(
            (if isUnsigned: "nimUcheckB" else: "nimIcheckB") & ".0." & SystemModuleSuffix)
          copyIntoKind dest, CallX, info:
            dest.addSymUse p, info
            dest.add idxBuf
            dest.add hiBuf
        else:
          dest.add idxBuf
    else:
      dest.add idxBuf
    dest.addParRi(n.endInfo)

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
            TypedT, CstringT, PointerT, OrdinalT, NoType:
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
        trSons(c, dest, n)
        c.typeCache.closeScope()
      of StmtsS:
        trSons(c, dest, n, isTopScope = isTopScope)
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
    of AddX, SubX, MulX, DivX, NegX, EqX, LeX, LtX:
      trFloatArith(c, dest, n)
    of ErrX, SufX, AtX, DerefX, DotX, PatX, ParX, AddrX, NilX,
        InfX, NeginfX, NanX, FalseX, TrueX, AndX, OrX, XorX,
        NotX, SizeofX, AlignofX, OffsetofX, OconstrX,
        AconstrX, BracketX, CurlyX, CurlyatX, OvfX,
        ModX, ShrX, ShlX, BitandX, BitorX, BitxorX,
        BitnotX, NeqX, CastX, ConvX,
        CchoiceX, OchoiceX, PragmaxX, QuotedX, HderefX,
        HaddrX, NewrefX, NewobjX, TupX, TupconstrX, TabconstrX,
        AshrX, BaseobjX, HconvX, DconvX,
        CompilesX, DeclaredX, DefinedX, ProccallX, DelayX,
        AstToStrX, BindSymX, BindSymNameX, InstanceofX, HighX, LowX, UnpackX,
        FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX,
        DefaultobjX, DefaulttupX, DefaultdistinctX,
        Delay0X, SuspendX, DoX, TupatX, EmoveX,
        DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX,
        InternalTypeNameX, InternalFieldPairsX, FailedX, IsX,
        EnvpX, KvX, ToClosureX:
      trSons(c, dest, n)
  else:
    bug "unexpected ')' inside"

proc desugar*(pass: var Pass; activeChecks: set[CheckMode]) =
  var n = pass.n  # Extract cursor locally
  var c = Context(counter: 0, typeCache: createTypeCache(pass.bits), thisModuleSuffix: pass.moduleSuffix, activeChecks: activeChecks, pending: createTokenBuf(), hoisted: createTokenBuf(), bits: pass.bits)
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
      tr c, body, n, isTopScope = true

  pass.dest.addParLe(rootTag, rootInfo)
  pass.dest.add c.hoisted
  pass.dest.add body
  pass.dest.add c.pending
  pass.dest.addParRi()

  c.typeCache.closeScope()
