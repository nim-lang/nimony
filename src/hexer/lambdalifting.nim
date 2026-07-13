#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[
Lambda lifting uses multiple passes:

- Determine which local variables cross proc boundaries. Map these to an environment.
  An environment is a scope that is allocated on the heap.
- Each usage of such a local becomes `env.local` but the `env` is not always the same:
  The outer env is itself a local variable, the inner env is a proc parameter.
- Procs that use a local variable that crosses a proc boundary are marked as "closure"
  ("uses environment").
- **Usages** of closure procs are turned from `fn` to `(fn, env)` and these tuple calls are
  turned from `(fn, env)(args)` to `fn(args, env)`. The tuple creation/unpacking can
  be optimized further.
- If all usages of closure procs do not escape, the environment can be allocated on
  the stack. As an approximation, closure procs do not escape if they are only used
  as the `fn` value in a function call `fn(args)`.
- A single indirection might not be enough. Consider:

```nim
  proc outerA =
    var a, b: int
    proc outerB =
      proc innerA = use(a)
      proc innerB = use(b); innerA()
      innerB()
```

Here `outerB` is also a closure.

]##

import std / [assertions, sets, tables, hashes, syncio]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / symparser
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, expreval, xints, builtintypes, langmodes, renderer, reporters, typeprops]
import hexer_context, passes
include ".." / nimony / nif_annotations
import coro_transform
# Bring the iter-value tuple constants/helpers into scope as
# unqualified names. `ResultParamName` / `CallerParamName` are the
# canonical names; lambdalifting's old aliases (`IterResultParamName`,
# `IterCallerParamName`) are gone — they were the same strings.
# `BareRootObjName` is the real `RootObj` (the env-slot type); the
# misleadingly-named `RootObjName` over in coro_transform is
# `CoroutineBase` and is NOT the right thing for the env slot. Don't
# confuse them.

type
  EnvMode = enum
    EnvIsLocal, EnvIsParam
  CurrentEnv = object
    s: SymId
    typ: SymId
    mode: EnvMode
    needsHeap: bool

  EnvField = object
    objType: SymId
    field: SymId
    typ: Cursor

  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    procStack: seq[SymId]
    dest: TokenBuf
    closureProcs, createsEnv, escapes: HashSet[SymId]
    localToEnv: Table[SymId, EnvField]
    env: CurrentEnv
    hasClosures: bool
    coroCtx: coro_transform.Context
      ## Shadow `coro_transform.Context` used to drive `.closure` iter
      ## state-machine generation. We loan our `typeCache` to it via
      ## `swap` while `transformCoroutineDecl` runs, then swap back.
      ## `coroTypes` and `shouldPublish` accumulate here across all
      ## iters in the module and get flushed in `elimLambdas`.

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)
  {.ensuresNif: addedAny(dest).}

proc isClosureIterDecl(n: Cursor): bool =
  ## True if `n` is at an `(iterator :sym …)` whose pragmas carry
  ## `.closure`. Used by pass 1 (set `hasClosures`) and pass 2 (route
  ## to `transformClosureIter`).
  if n.stmtKind != IteratorS: return false
  var m = n
  inc m   # past iterator tag
  for _ in 0..<ProcPragmasPos:
    skip m
  hasPragma(m, ClosureP)

proc trSons(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    while n.hasMore:
      tr(c, dest, n)

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    c.typeCache.takeLocalHeader(dest, n, kind)
    tr(c, dest, n)

proc trProc(c: var Context; dest: var TokenBuf; n: var Cursor) =
  #c.typeCache.openScope(ProcScope)
  let decl = n
  copyInto dest, n:
    let symId = n.symId
    c.procStack.add(symId)
    var isConcrete = true # assume it is concrete
    for i in 0..<BodyPos:
      if i == ParamsPos:
        c.typeCache.openProcScope(symId, decl, n)
        c.typeCache.registerParams(symId, decl, n)
      elif i == TypevarsPos:
        isConcrete = n.substructureKind != TypevarsU
      elif i == ProcPragmasPos:
        if hasPragma(n, ClosureP):
          c.closureProcs.incl symId
          c.escapes.incl symId
          c.hasClosures = true
      takeTree dest, n
    if isConcrete:
      tr(c, dest, n)
    else:
      takeTree dest, n
    discard c.procStack.pop()
  c.typeCache.closeScope()

proc envTypeForProc(c: var Context; procId: SymId): SymId =
  let s = extractVersionedBasename(pool.syms[procId])
  result = pool.syms.getOrIncl(s & ".env." & c.thisModuleSuffix)

proc localToField(c: var Context; n: Cursor; local, typ: SymId): SymId =
  if c.localToEnv.hasKey(local):
    result = c.localToEnv.getOrQuit(local).field
  else:
    var name = pool.syms[local]
    extractBasename name
    name.add "`f."
    name.add $c.counter
    inc c.counter
    name.add "."
    name.add c.thisModuleSuffix
    result = pool.syms.getOrIncl(name)
    c.localToEnv[local] = EnvField(objType: typ, field: result, typ: c.typeCache.getType(n))

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  takeInto dest, n:
    if n.kind == Symbol:
      # if a closure proc is called, we don't want to see it as "escaping".
      dest.add n
      inc n
    while n.hasMore:
      tr(c, dest, n)

proc itertypeNeedsTuple(n: Cursor): bool {.inline.} =
  ## True when an itertype's pragmas tag it as a `.closure` iter (Nim-style
  ## resumable, ref-based shared env). `.passive` iters keep the existing
  ## non-ref CPS lowering: cps's `trProctype` rewrites their itertype to a
  ## plain function pointer with the wrapper signature, no tuple wrap.
  n.typeKind == ItertypeT and procHasPragma(n, ClosureP)

proc trNil(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let nilScope = enterScope(n)
  # `(nil <Type>)` for a closure proctype OR itertype (.closure / .passive)
  # lowers to a `{fnptr, env}` tuple constructor — that's the runtime shape
  # of Nim closures and of our wrapper-signature iter values.
  let isIter = n.hasMore and itertypeNeedsTuple(n)
  let isCloseable = n.hasMore and (isIter or procHasPragma(n, ClosureP))
  if isIter:
    c.hasClosures = true
    dest.copyIntoKind TupconstrX, info:
      # rewrite the inner itertype to its wrapper-shape tuple
      emitIterTupleTypeFromParams(dest, n, info)
      if n.hasMore: skip n # might have another nil value
      dest.addParPair NilX, info
      dest.addParPair NilX, info
  elif isCloseable:
    # nil closure must be a tuple:
    c.hasClosures = true
    dest.copyIntoKind TupconstrX, info:
      dest.takeTree n # type
      if n.hasMore: skip n # might have another nil value
      dest.addParPair NilX, info
      dest.addParPair NilX, info
  else:
    dest.addParLe NilX, (if n.hasMore: n.info else: n.endInfo)
    while n.hasMore: takeTree dest, n
    dest.addParRi(n.endInfo)
  leaveScope(n, nilScope)

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of DotToken, UnknownToken, EofToken, Ident, SymbolDef,
     IntLit, UIntLit, FloatLit, CharLit, StringLit:
    takeTree dest, n
  of Symbol:
    let loc = c.typeCache.getLocalInfo(n.symId)
    if loc.kind in {ParamY, LetY, VarY, ResultY}:
      let cross = loc.crossedProc.int
      if cross > 0:
        for i in c.procStack.len - cross ..< c.procStack.len:
          c.closureProcs.incl(c.procStack[i])
        let destEnv = c.procStack[0] #c.procStack.len - cross]
        c.createsEnv.incl destEnv
        let envType = c.envTypeForProc(destEnv)
        let fld = c.localToField(n, n.symId, envType)
        #[

        Problem:

          proc outerA =
            var a: int
            proc outerB =
              var b: int
              proc inner =
                use a # uses env from outerA
                use b # uses env from outerB

        But there is only one environment parameter that `inner` can use
        for the accesses to `a` and `b`. Luckily, we analyse the entire
        `outerA` in one go, so we can use `outerA`'s environment for `outerB`
        too.
        ]#
        dest.copyIntoKind EnvpX, n.info:
          dest.addSymUse envType, n.info
          dest.addSymUse fld, n.info
        inc n
      else:
        takeTree dest, n
    elif loc.kind in {ProcY, FuncY, IteratorY, ConverterY, MethodY}:
      # usage of a closure proc not within a call? --> The closure does escape:
      if c.procStack.len > 0:
        #c.escapes.incl n.symId
        c.escapes.incl c.procStack[0]
      takeTree dest, n
    else:
      takeTree dest, n
  of ParLe:
    case n.stmtKind
    of LocalDecls:
      trLocal c, dest, n
    of ProcS, FuncS, MethodS, ConverterS:
      trProc c, dest, n
    of TypeS:
      # Type alias body for an itertype must be rewritten to the wrapper-shape
      # tuple BEFORE the lifter runs (lifter is in duplifier/destroyer, which
      # is after lambdalifting and before cps). Otherwise the lifter follows
      # the alias, sees ItertypeT, marks trivial (RoutineTypes branch in
      # lifter.isTrivial), and never generates destroy/copy hooks for the
      # iter-value env slot.
      let typeStart = dest.len
      var typeSym = SymId(0)
      var publishIt = false
      takeInto dest, n:       # TypeS tag
        if n.kind == SymbolDef:
          typeSym = n.symId
        takeTree dest, n        # name
        takeTree dest, n        # exported
        takeTree dest, n        # typevars
        takeTree dest, n        # pragmas
        if typeSym != SymId(0) and itertypeNeedsTuple(n):
          c.hasClosures = true
          emitIterTupleTypeFromParams(dest, n, n.info)
          publishIt = true
        while n.hasMore: takeTree dest, n
      if publishIt:
        programs.publish(typeSym, dest, typeStart)
    of IteratorS:
      # `.closure` iter decls are owned by lambdalifting now (pass 2
      # generates the state machine via coro_transform). Set
      # `hasClosures` so pass 2 fires for this module even when
      # there's no other closure pressure.
      if isClosureIterDecl(n):
        c.hasClosures = true
      takeTree dest, n
    of MacroS, TemplateS, EmitS, BreakS, ContinueS,
      ForS, IncludeS, ImportS, FromimportS, ImportexceptS,
      ExportS, CommentS,
      PragmasS:
      takeTree dest, n
    of ScopeS:
      c.typeCache.openScope()
      trSons(c, dest, n)
      c.typeCache.closeScope()
    of CallS, CmdS, BlockS, AsgnS, IfS, WhenS, WhileS, CoroforS,
      CaseS, RetS, YldS, StmtsS, PragmaxS, InclS, ExclS, ImportasS,
      ExportexceptS, DiscardS, TryS, RaiseS, UnpackdeclS,
      AssumeS, AssertS, CallstrlitS, InfixS, PrefixS, HcallS,
      StaticstmtS, BindS, MixinS, UsingS, AsmS, DeferS,
      NoStmt:
      case n.exprKind
      of CallKinds:
        trCall c, dest, n
      of TypeofX:
        takeTree dest, n
      of NilX:
        trNil c, dest, n
      of ToClosureX:
        c.hasClosures = true
        trSons(c, dest, n)
      of ErrX, SufX, AtX, DerefX, DotX, PatX, ParX, AddrX,
        InfX, NeginfX, NanX, FalseX, TrueX, AndX, OrX, XorX,
        NotX, NegX, SizeofX, AlignofX, OffsetofX, OconstrX,
        AconstrX, BracketX, CurlyX, CurlyatX, OvfX, AddX,
        SubX, MulX, DivX, ModX, ShrX, ShlX, BitandX, BitorX,
        BitxorX, BitnotX, EqX, NeqX, LeX, LtX, CastX, ConvX,
        CchoiceX, OchoiceX, PragmaxX, QuotedX, HderefX, DdotX,
        HaddrX, NewrefX, NewobjX, TupX, TupconstrX, SetconstrX,
        TabconstrX, AshrX, BaseobjX, HconvX, DconvX, CompilesX,
        DeclaredX, DefinedX, AstToStrX, BindSymX, BindSymNameX, InstanceofX, HighX,
        LowX, UnpackX, FieldsX, FieldpairsX, EnumtostrX,
        IsmainmoduleX, DefaultobjX, DefaulttupX,
        DefaultdistinctX, Delay0X, SuspendX, ExprX, DoX,
        ArratX, TupatX, PlussetX, MinussetX, MulsetX, XorsetX,
        EqsetX, LesetX, LtsetX, InsetX, CardX, EmoveX,
        DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX,
        InternalTypeNameX, InternalFieldPairsX, FailedX, IsX,
        EnvpX, KvX, NoExpr:
        trSons(c, dest, n)
  of ParRi:
    bug "unexpected ')' inside"

proc isClosure(typ: Cursor): bool {.inline.} = procHasPragma(typ, ClosureP)

when false:
  proc paramsWithClosurePragma(typ: Cursor): bool =
    var typ = typ
    skip typ
    skip typ # return type
    result = hasPragma(typ, ClosureP)

const
  # Lambdalifting-specific names (not in coro_transform).
  EnvParamName = "`ep.0"
  EnvLocalName = "`el.0"

# `RootObjName` / `coroWrapperProcName` / `emitIterTupleType*` /
# `isClosureIterSym` / `isLiftedClosureTuple` now live in
# `coro_transform`. The wrapper-signature shape is owned there too, so
# both passes stay in lock-step automatically.

proc addRootRef(dest: var TokenBuf; info: PackedLineInfo)
  {.ensuresNif: addedType(dest).} =
  dest.copyIntoKind RefT, info:
    dest.addSymUse pool.syms.getOrIncl(BareRootObjName), info

type
  UntypedEnvMode = enum
    WantValue, WantAddr

proc untypedEnv(dest: var TokenBuf; info: PackedLineInfo; env: CurrentEnv; mode=WantValue)
  {.ensuresNif: addedExpr(dest).} =
  assert env.s != SymId(0)
  case env.mode
  of EnvIsLocal:
    dest.copyIntoKind CastX, info:
      if env.needsHeap:
        dest.addRootRef info
      else:
        dest.copyIntoKind PointerT, info: discard
      if mode == WantAddr:
        dest.copyIntoKind AddrX, info:
          dest.addSymUse env.s, info
      else:
        dest.addSymUse env.s, info
  of EnvIsParam:
    # the parameter already has the erased type:
    if mode == WantAddr:
      dest.copyIntoKind AddrX, info:
        dest.addSymUse env.s, info
    else:
      dest.addSymUse env.s, info

proc typedEnv(dest: var TokenBuf; info: PackedLineInfo; env: CurrentEnv)
  {.ensuresNif: addedExpr(dest).} =
  assert env.s != SymId(0)
  case env.mode
  of EnvIsLocal:
    # the local already has the full type:
    dest.addSymUse env.s, info
  of EnvIsParam:
    # the parameter has the erased type:
    dest.copyIntoKind CastX, info:
      dest.copyIntoKind (if env.needsHeap: RefT else: PtrT), info:
        dest.addSymUse env.typ, info
      dest.addSymUse env.s, info

proc tre(c: var Context; dest: var TokenBuf; n: var Cursor)
  {.ensuresNif: addedAny(dest).}

# ---------------------------------------------------------------------
# Hooks installed on `coroCtx`. Lambdalifting drives the coro-transform
# pipeline for `.closure` iters only — there is no `.passive` here,
# nested procs have been lifted out of the iter body before
# coro_transform sees it, and the type-slot rewrites have already been
# done at the lambdalifting level. So most hooks are pass-through; the
# `.passive`-flavour ones are bug-guards.
# ---------------------------------------------------------------------

proc llIsPassiveProc(c: var coro_transform.Context; s: SymId): bool = false
proc llIsPassiveCall(c: var coro_transform.Context; n: Cursor): bool = false

proc llTrPassiveCall(c: var coro_transform.Context; dest: var TokenBuf;
                     n: var Cursor; target: Cursor) =
  bug "`.passive` call lowering invoked inside a `.closure` iter body"

proc llTrBug(c: var coro_transform.Context; dest: var TokenBuf; n: var Cursor) =
  bug "delay/suspend lowering invoked inside a `.closure` iter body"

proc llTakeTree(c: var coro_transform.Context; dest: var TokenBuf; n: var Cursor) =
  takeTree dest, n

proc lambdaHooks(): coro_transform.Hooks =
  coro_transform.Hooks(
    isPassiveProc: llIsPassiveProc,
    isPassiveCall: llIsPassiveCall,
    trPassiveCall: llTrPassiveCall,
    trDelay: llTrBug,
    trDelay0: llTrBug,
    trSuspend: llTrBug,
    trProctype: llTakeTree,
    trCoroutine: llTakeTree
  )

proc transformClosureIter(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Run coro_transform's full pipeline on a `.closure` iter decl:
  ## state procs, coro frame type, wrapper proc, signature patch.
  ##
  ## State transfer: loan our `typeCache` to `coroCtx` for the
  ## duration of the call (so type lookups against locals we already
  ## registered keep working) and reclaim it after. `coroTypes` /
  ## `shouldPublish` stay on `coroCtx`; flushed by `elimLambdas`.
  swap c.coroCtx.typeCache, c.typeCache
  coro_transform.transformCoroutineDecl(c.coroCtx, dest, n)
  swap c.coroCtx.typeCache, c.typeCache

proc isClosureCoroFor(c: var Context; n: Cursor): bool =
  ## Peek at a `(corofor (call <target> …) …)` to decide whether this
  ## corofor is for a `.closure` iter (lambdalifting handles) or a
  ## `.passive` iter (cps handles, lambdalifting passes through).
  ##
  ## Three target shapes reach here:
  ##   1. Symbol of an `.closure` iter decl — direct iter call.
  ##   2. Symbol of a local of iter-value type — iter VALUE call.
  ##   3. Non-Symbol expression (e.g. `(tupat g 0)`) emitted by
  ##      lambdalifting's genCall pre-extraction — iter VALUE call.
  ## `.passive` iter direct calls leave the target as Symbol of a
  ## non-`.closure` iter decl, which falls through to cps.
  assert n.stmtKind == CoroforS
  var m = n
  inc m  # past corofor tag
  if m.exprKind notin CallKinds: return false
  inc m  # past call tag
  if m.kind == Symbol and isClosureIterSym(m.symId):
    return true
  # Inspect the target's TYPE — covers iter-value locals (case 2)
  # and any non-Symbol target that nonetheless has iter-shaped type.
  let typ = c.typeCache.getType(m, {SkipAliases})
  if typ.typeKind == ItertypeT and procHasPragma(typ, ClosureP):
    return true
  if typ.typeKind == TupleT:
    var t = typ
    inc t  # past tuple tag
    if t.kind == ParLe and t.typeKind == ProctypeT and procHasPragma(t, ClosureP):
      return true
  return false

proc trClosureCoroFor(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Expand `(corofor (call closure-iter args... (haddr forLoopVar)) (block ...))`
  ## into the trampoline. Mirrors `coro_transform.trCoroFor` but walks
  ## the body via lambdalifting's `tre` (so closure captures inside
  ## the for-loop body get rewritten to env accesses).
  let info = n.info
  let coroforScope = enterScope(n) # skip (corofor

  # ---- first child: (call iter-or-tupat args... (haddr forLoopVar)) ----
  assert n.exprKind in CallKinds, "corofor: expected iter call as first child"
  let callScope = enterScope(n) # past CallS tag
  # Extract the call target. Three shapes:
  #   1. Symbol of an iter DECL — direct call routed through its
  #      init wrapper.
  #   2. Symbol of an iter-VALUE local — pull fn-slot and env-slot
  #      out via `(tupat g 0)` / `(tupat g 1)`. The env-slot becomes
  #      the wrapper-call's `caller.env`, triggering the wrapper's
  #      reuse branch so iter state persists across loops.
  #   3. Pre-extracted expression (e.g. a `(tupat g 0)` already
  #      emitted by upstream genCall) — use verbatim and look for
  #      the trailing tupat env-arg further down.
  # Track which branch we took for the target — this is the ONLY
  # reliable signal for "does the arg list have an upstream env-arg?".
  # Probing the last arg for TupatX is unsound: a regular arg like
  # `(tupat someTuple 0)` would falsely match.
  var targetBuf = createTokenBuf(4)
  var valSymForEnv: SymId = SymId(0)  # case 2: synthesize env-arg from this
  var valInfoForEnv: PackedLineInfo = default(PackedLineInfo)
  var upstreamEnvArg = false           # case 3: env-arg is penultimate arg
  if n.kind == Symbol and isClosureIterSym(n.symId):
    targetBuf.addSymUse coro_transform.coroWrapperForExternIter(n.symId), n.info
    coro_transform.publishWrapperSignature(n.symId, c.thisModuleSuffix)
    inc n
  elif n.kind == Symbol:
    valSymForEnv = n.symId
    valInfoForEnv = n.info
    targetBuf.copyIntoKind TupatX, valInfoForEnv:
      targetBuf.addSymUse valSymForEnv, valInfoForEnv
      targetBuf.addIntLit 0, valInfoForEnv
    inc n
  else:
    upstreamEnvArg = true
    targetBuf.takeTree n

  # Cursors are stable into the source buffer — walk once to count args
  # and remember the cursor at each arg's start position; emit later
  # via `addSubtree` from those cursor copies.
  let argsStart = n
  var lastArgPos = default(Cursor)
  var penultimateArgPos = default(Cursor)
  var argCount = 0
  while n.hasMore:
    penultimateArgPos = lastArgPos
    lastArgPos = n
    skip n
    inc argCount
  leaveScope(n, callScope) # close iter call

  # Structural invariant maintained by the corofor producer (sem/hexer
  # genCall): `(haddr forLoopVar)` is the trailing arg, optionally
  # preceded by an env-arg when the target was pre-extracted. We
  # don't probe `lastArgPos.exprKind == HaddrX` — a regular iter arg
  # of `addr` shape would falsely match.
  let trailingCount = if upstreamEnvArg: 2 else: 1
  assert argCount >= trailingCount, "corofor: iter call missing args"
  let realArgCount = argCount - trailingCount

  # ---- emit `var it: Continuation = wrapper(args..., addr forLoopVar, callerCont)` ----
  # For iter-VALUE calls the callerCont's env is the iter value's
  # env-slot ref (so `caller.env != nil` → wrapper reuse branch).
  # For direct iter-sym calls the callerCont is the Stop sentinel
  # (`caller.env == nil` → wrapper fresh-frame branch).
  let itSym = pool.syms.getOrIncl("`coroIt." & $c.counter & "." & c.thisModuleSuffix)
  inc c.counter
  c.typeCache.registerLocal(itSym, VarY, default(Cursor))
  dest.copyIntoKind VarS, info:
    dest.addSymDef itSym, info
    dest.addDotToken() # exported
    dest.addDotToken() # pragmas
    dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
    dest.copyIntoKind CallS, info:
      dest.add targetBuf
      var w = argsStart
      for i in 0 ..< realArgCount:
        dest.takeTree w
      var addrW = lastArgPos
      dest.takeTree addrW
      if upstreamEnvArg or valSymForEnv != SymId(0):
        # iter-value call: caller = `Continuation(fn: nil, env: addr(envSlot[]))`.
        # A bare `(cast (ptr CoroutineBase) ref)` would be a raw
        # bit-cast giving the ref-struct ptr (where the rc lives),
        # NOT the data ptr — so the wrapper would read garbage when
        # it accesses `caller.env.callee` etc. `(haddr (hderef ref))`
        # peels the ARC header and yields the underlying object's
        # address, which is the right shape for `ptr CoroutineBase`.
        dest.copyIntoKind OconstrX, info:
          dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
          dest.copyIntoKind KvU, info:
            dest.addSymUse pool.syms.getOrIncl(FnFieldName), info
            dest.addParPair NilX, info
          dest.copyIntoKind KvU, info:
            dest.addSymUse pool.syms.getOrIncl(EnvFieldName), info
            dest.copyIntoKind CastX, info:
              dest.copyIntoKind PtrT, info:
                dest.addSymUse pool.syms.getOrIncl(coro_transform.RootObjName), info
              dest.copyIntoKind HaddrX, info:
                dest.copyIntoKind HderefX, info:
                  if upstreamEnvArg:
                    var envW = penultimateArgPos
                    dest.takeTree envW
                  else:
                    dest.copyIntoKind TupatX, valInfoForEnv:
                      dest.addSymUse valSymForEnv, valInfoForEnv
                      dest.addIntLit 1, valInfoForEnv
      else:
        coro_transform.emitStopContinuation(dest, info)

  # `myEnv` snapshot + try/while/finally — shared with cps's
  # `.passive` expansion via `coro_transform.emitWhileBegin`/
  # `emitWhileEnd`. The body walk uses `tre` (capture-rewriting), as
  # opposed to cps's `tr` (passive-state-machine emission); that's the
  # only behavioural difference between the two corofor expansions.
  let myEnvSym = pool.syms.getOrIncl("`coroEnv." & $c.counter & "." & c.thisModuleSuffix)
  inc c.counter
  c.typeCache.registerLocal(myEnvSym, LetY, default(Cursor))

  coro_transform.emitWhileBegin(dest, info, itSym, myEnvSym)
  while n.hasMore:
    tre(c, dest, n)
  coro_transform.emitWhileEnd(dest, info, itSym)

  leaveScope(n, coroforScope) # close (corofor

proc treSons(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    while n.hasMore:
      tre(c, dest, n)

proc addEnvParam(dest: var TokenBuf; info: PackedLineInfo; envTyp: SymId) =
  dest.copyIntoKind ParamU, info:
    dest.addSymDef pool.syms.getOrIncl(EnvParamName), info
    dest.addDotToken() # no export marker
    dest.addDotToken() # no pragmas
    if envTyp == SymId(0):
      dest.copyIntoKind RefT, info:
        dest.addSymUse pool.syms.getOrIncl(BareRootObjName), info
    else:
      # to keep NIFC's type system happy we need a ptr type here
      # and then a cast in the body!
      dest.copyIntoKind PointerT, info: discard
    dest.addDotToken() # no default value

proc treParamsWithEnv(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    while n.hasMore:
      tre(c, dest, n)
    addEnvParam dest, NoLineInfo, SymId(0)

proc treProcType(c: var Context; dest: var TokenBuf; n: var Cursor) =
  if itertypeNeedsTuple(n):
    # Closure / passive iterators get the wrapper-signature tuple shape
    # rather than the closure-proc shape. The wrapper signature lives in cps;
    # we mirror it here so the lifter sees the final tuple shape at
    # duplifier/destroyer time and hooks line up with cps's wrapper-proc
    # emission. `.closure` and `.passive` iters share the same tuple shape;
    # they differ only at the cps trampoline level.
    c.hasClosures = true
    emitIterTupleTypeFromParams(dest, n, n.info)
  elif isClosure(n):
    # type is really a tuple:
    let info = n.info
    copyIntoKind dest, TupleT, info:
      copyIntoKind dest, ProctypeT, info:
        dest.addDotToken() # nilability tag
        let inputKind = n.typeKind
        let isProctypeInput = inputKind == ProctypeT
        # the callers guarantee `inputKind in RoutineTypes` here
        let rtScope = enterScope(n)
        if inputKind in {ProctypeT, ItertypeT}:
          skip n # nilability tag
        else:
          skipRoutineDeclPrefix(n, inputKind)
        if n.substructureKind == ParamsU:
          treParamsWithEnv(c, dest, n)
        else:
          assert n.kind == DotToken
          inc n
          dest.addParLe ParamsU, info
          addEnvParam dest, info, SymId(0)
          dest.addParRi()
        tre c, dest, n # return type
        # pragmas:
        tre c, dest, n
        if not isProctypeInput:
          # effects and body, deliberately made flexible here for future changes
          # as it's messy to work with.
          if n.hasMore:
            skip n
            if n.hasMore: skip n
        leaveScope(n, rtScope)
      copyIntoKind dest, RefT, info:
        dest.addSymUse pool.syms.getOrIncl(BareRootObjName), info
  else:
    let isProctypeInput = n.typeKind == ProctypeT
    takeInto dest, n:
      if isProctypeInput:
        # new layout: nilability, params, retType, pragmas
        for i in 0..3:
          if not n.hasMore: break
          tre c, dest, n
      else:
        for i in 0..<BodyPos:
          tre c, dest, n
        if n.hasMore:
          dest.takeTree n # don't transform the potential proc body here

proc treType(c: var Context; dest: var TokenBuf; n: var Cursor)
  {.ensuresNif: addedType(dest).} =
  # Like `tre` but prefer the type interpretation. (Matters for ProcS etc.)
  if n.typeKind in RoutineTypes:
    treProcType(c, dest, n)
  else:
    tre(c, dest, n)

proc treLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let s = n.firstSon.symId
  let fld = c.localToEnv.getOrDefault(s)
  let kind = n.symKind
  if fld.field != SymId(0):
    # the local is already a field of an environment object
    let info = n.info
    let declScope = enterScope(n) # into the decl
    let name = n.symId
    for i in 1..3: skip n
    # register the local anyway to keep the type navigator happy:
    c.typeCache.registerLocal(name, kind, n)
    skip n # type
    if n.kind != DotToken:
      # generate an assignment:
      dest.copyIntoKind AsgnS, info:
        dest.copyIntoKind DotX, info:
          if c.env.needsHeap:
            dest.copyIntoKind DerefX, info:
              dest.typedEnv info, c.env
          else:
            dest.typedEnv info, c.env
          dest.addSymUse fld.field, info
        tre c, dest, n # value
    else:
      inc n # the dot value
    leaveScope(n, declScope)
  else:
    copyInto dest, n:
      let name = n.symId
      takeTree dest, n # name
      takeTree dest, n # export marker
      takeTree dest, n # pragmas
      c.typeCache.registerLocal(name, kind, n)
      let beforeType = dest.len
      treType c, dest, n # type (might grow an environment parameter)
      tre c, dest, n # value

proc treParams(c: var Context; dest, init: var TokenBuf; n: var Cursor; doAddEnvParam: bool; envTyp: SymId) =
  copyInto dest, n:
    while n.hasMore:
      assert n.substructureKind == ParamU
      copyInto dest, n:
        let name = n.symId
        let paramInfo = n.info # `n` sits at the scope's end below
        takeTree dest, n # name
        takeTree dest, n # export marker
        takeTree dest, n # pragmas
        c.typeCache.registerLocal(name, ParamY, n)
        treType c, dest, n # type (might grow an environment parameter)
        tre c, dest, n # value

        # parameter might have been captured:
        let fld = c.localToEnv.getOrDefault(name)
        if fld.field != SymId(0):
          # XXX Check here for memory safety violations: Cannot capture a `var T` parameter
          # We're emitting `outer_env.<field> = <param>` into the
          # body-prologue (treProcBody splices `init` after the
          # env-local decl). `c.env` isn't usable yet — it'll be set
          # up by treProcBody AFTER this — so reference the env-local
          # by name (`EnvLocalName`) directly.
          #
          # `envTyp == SymId(0)` means "heap env" (the caller sets it
          # that way when the closureOwner escapes): the env-local is
          # `ref EnvT` and we need to deref before `.field`. For stack
          # env (`envTyp != 0`), the env-local IS the object and no
          # deref is needed. (The previous code had this inverted —
          # never tripped because the path was also broken by the
          # `typedEnv c.env` call with `c.env.s == 0`.)
          init.copyIntoKind AsgnS, paramInfo:
            init.copyIntoKind DotX, paramInfo:
              if envTyp == SymId(0):
                init.copyIntoKind DerefX, paramInfo:
                  init.addSymUse pool.syms.getOrIncl(EnvLocalName), paramInfo
              else:
                init.addSymUse pool.syms.getOrIncl(EnvLocalName), paramInfo
              init.addSymUse fld.field, paramInfo
            init.addSymUse name, paramInfo

    if doAddEnvParam:
      addEnvParam dest, n.endInfo, envTyp

proc treProcBody(c: var Context; dest, init: var TokenBuf; n: var Cursor; sym: SymId; needsHeap: bool) =
  if n.stmtKind == StmtsS:
    copyInto dest, n:
      let oldEnv = c.env
      if c.createsEnv.contains(sym):
        let envTyp = c.envTypeForProc(sym)
        c.env = CurrentEnv(s: pool.syms.getOrIncl(EnvLocalName), mode: EnvIsLocal, typ: envTyp, needsHeap: needsHeap)
        dest.copyIntoKind VarS, NoLineInfo:
          dest.addSymDef c.env.s, NoLineInfo
          dest.addDotToken() # no export marker
          dest.addDotToken() # no pragmas
          if needsHeap:
            dest.copyIntoKind RefT, NoLineInfo:
              dest.addSymUse c.env.typ, NoLineInfo
            dest.copyIntoKind NewobjX, NoLineInfo:
              dest.copyIntoKind RefT, NoLineInfo:
                dest.addSymUse c.env.typ, NoLineInfo
          else:
            dest.addSymUse c.env.typ, NoLineInfo
            dest.addDotToken() # no default value
        if needsHeap:
          # Note: If the environment is on the stack, a single `wasMoved`
          # hook will be generated for it so we don't need to do anything here.
          # Otherwise, we need to init the environment via the `=wasMoved` hooks:
          for _, field in c.localToEnv:
            if field.objType == c.env.typ:
              dest.copyIntoKind WasmovedX, NoLineInfo:
                dest.copyIntoKind HaddrX, NoLineInfo:
                  dest.copyIntoKind DotX, NoLineInfo:
                    if needsHeap:
                      dest.copyIntoKind DerefX, NoLineInfo:
                        dest.addSymUse c.env.s, NoLineInfo
                    else:
                      dest.addSymUse c.env.s, NoLineInfo
                    dest.addSymUse field.field, NoLineInfo

      elif c.closureProcs.contains(sym):
        c.env = CurrentEnv(s: pool.syms.getOrIncl(EnvParamName), mode: EnvIsParam, typ: c.envTypeForProc(sym), needsHeap: needsHeap)
      else:
        c.env = CurrentEnv(s: SymId(0), mode: EnvIsParam, typ: SymId(0), needsHeap: needsHeap)
      dest.add init
      while n.hasMore:
        tre(c, dest, n)
      var needsHeapB = c.env.needsHeap
      c.env = oldEnv
      c.env.needsHeap = c.env.needsHeap or needsHeapB
  else:
    tre(c, dest, n)

proc treProc(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var init = createTokenBuf(10)
  let decl = n
  copyInto dest, n:
    var isConcrete = true # assume it is concrete
    let sym = n.symId
    c.procStack.add(sym)
    let closureOwner = c.procStack[0]
    let needsHeap = c.escapes.contains(closureOwner)
    for i in 0..<BodyPos:
      if i == ParamsPos:
        c.typeCache.openProcScope(sym, decl, n)
        let envType = if needsHeap: SymId(0) else: c.envTypeForProc(closureOwner)
        treParams c, dest, init, n, c.closureProcs.contains(sym), envType
      else:
        if i == TypevarsPos:
          isConcrete = n.substructureKind != TypevarsU
        if i == ReturnTypePos and isConcrete:
          treType c, dest, n
        else:
          takeTree dest, n

    if isConcrete:
      treProcBody(c, dest, init, n, sym, needsHeap)
    else:
      takeTree dest, n
    discard c.procStack.pop()
  c.typeCache.closeScope()

proc treProcLift(c: var Context; dest: var TokenBuf; n: var Cursor) =
  if c.procStack.len == 0:
    swap c.dest, dest
  var lift = createTokenBuf(16)
  treProc c, lift, n
  c.dest.add lift
  if c.procStack.len == 0:
    swap c.dest, dest

proc isStaticCall(c: var Context;s: SymId): bool =
  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    let fn = asRoutine(res.decl)
    result = isRoutine(fn.kind)
  else:
    let local = c.typeCache.getLocalInfo(s)
    result = isRoutine(local.kind)

proc toNonClosureProcType(c: var Context; dest: var TokenBuf; n: Cursor) =
  # just remove closure pragma from proctype
  var n = n
  assert n.typeKind in {ProctypeT, ProcT}
  takeInto dest, n:
    while n.hasMore:
      if n.substructureKind == PragmasU:
        takeInto dest, n:
          while n.hasMore:
            if n.pragmaKind == ClosureP:
              skip n
            else:
              takeTree dest, n
      else:
        takeTree dest, n

proc genCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let callNode = n  # the call node itself
  let callScope = enterScope(n)
  let fn = n
  let typ = c.typeCache.getType(n, {SkipAliases})
  # A closure iter-value call target type can appear here in two guises:
  #   - raw `(itertype … (pragmas (closure)))` — `isClosure` matches.
  #   - lifted `(tuple <proctype> (ref RootObj))` — `isLiftedClosureTuple`
  #     matches (when getType already follows the alias to the rewritten
  #     body). `.passive` iter values are NOT iter-value tuples; cps's
  #     `trProctype` lowers them to plain function pointers, and they
  #     reach genCall as ordinary proctype calls — not wrapped.
  #
  # Iter SYM in static call position (`countup(1, 5)` inside a corofor)
  # is NOT a closure-value call — coro_transform.trCoroFor rewrites
  # this to a wrapper-proc call with explicit `StopContinuation`. We
  # must not append an env-arg here, otherwise the trampoline expects
  # the env-arg to be the addr-of-result and bails.
  let wantsEnv = isClosure(typ) or isLiftedClosureTuple(typ) or
                 (n.kind == Symbol and c.closureProcs.contains(n.symId))
  var isStatic = false
  var tmp = SymId(0)
  var needNilCheck = false
  if wantsEnv:
    isStatic = n.kind == Symbol and isStaticCall(c, n.symId)
    if isStatic:
      dest.add callNode
      # do not produce a tuple:
      dest.add n
      inc n
    elif n.kind == Symbol:
      tmp = n.symId
      needNilCheck = true
      dest.addParLe IfS, info
      dest.addParLe ElifU, info
      # env == nil means calls the non closure procedure that was converted to a closure procedure
      copyIntoKind dest, NeqX, info:
        if c.env.needsHeap:
          dest.addRootRef info
        else:
          dest.copyIntoKind PointerT, info: discard
        copyIntoKind dest, TupatX, info:
          dest.addSymUse tmp, info
          dest.addIntLit 1, info
        dest.addParPair NilX, info
      dest.add callNode
      copyIntoKind dest, TupatX, info:
        #tre c, dest, n
        takeToken dest, n
        dest.addIntLit 0, info
    else:
      dest.add callNode
      dest.addParLe(ExprX, info)
      copyIntoKind dest, StmtsS, info:
        tmp = pool.syms.getOrIncl("`llTemp." & $c.counter)
        inc c.counter
        copyIntoKind dest, VarS, info:
          dest.addSymDef tmp, info
          dest.addDotToken() # no export marker
          dest.addDotToken() # no pragmas
          var t = typ
          tre c, dest, t
          tre c, dest, n # value
      dest.addSymUse tmp, info
      dest.addParRi() # ExprX
  else:
    dest.add callNode
  while n.hasMore:
    tre(c, dest, n)
  if wantsEnv:
    if isStatic:
      if c.env.s != SymId(0):
        let mode = if c.env.needsHeap: WantValue else: WantAddr
        # use the current environment as the last parameter:
        untypedEnv dest, info, c.env, mode
      else:
        # can happen for toplevel closures that have been declared .closure for interop
        # We have no environment here, so pass `nil` instead:
        dest.copyIntoKind NilX, info: discard
    else:
      # unpack the tuple:
      assert tmp != SymId(0)
      copyIntoKind dest, TupatX, info:
        dest.addSymUse tmp, info
        dest.addIntLit 1, info
  dest.addParRi()
  leaveScope(n, callScope)

  if needNilCheck:
    dest.addParRi() # end of ElifU
    copyIntoKind dest, ElseU, info:
      dest.add callNode
      var n2 = fn
      copyIntoKind dest, CastX, info:
        c.toNonClosureProcType dest, typ
        copyIntoKind dest, TupatX, info:
          takeToken dest, n2
          dest.addIntLit 0, info
      while n2.hasMore:
        tre(c, dest, n2)
      dest.addParRi() # end of call
    dest.addParRi() # end of IfS

proc toProcType(c: var Context; dest: var TokenBuf; n: Cursor) =
  var n = n
  let info = n.info
  copyIntoKind dest, ProctypeT, info:
    dest.addDotToken() # nilability tag
    let inputKind = n.typeKind
    let rtScope = enterScope(n)
    if inputKind in {ProctypeT, ItertypeT}:
      skip n # nilability tag
    elif inputKind in RoutineTypes:
      skipRoutineDeclPrefix(n, inputKind)
    copyIntoKind dest, ParamsU, n.info:
      if n.kind == DotToken:
        inc n
      else:
        n.into:
          while n.hasMore:
            tre c, dest, n # params
      addEnvParam dest, info, SymId(0)
    tre c, dest, n # return type
    # pragmas:
    tre c, dest, n
    while n.hasMore: skip n
    leaveScope(n, rtScope)

proc treKv(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    dest.takeTree n # key
    while n.hasMore:
      tre(c, dest, n)

proc treToClosure(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let origTyp = c.typeCache.getType(n, {SkipAliases})
  let closureScope = enterScope(n)
  dest.copyIntoKind TupconstrX, info:
    dest.copyIntoKind TupleT, info:
      c.toProcType(dest, origTyp)
      dest.addRootRef info
    dest.copyIntoKind CastX, info:
      c.toProcType(dest, origTyp)
      tr c, dest, n
    dest.addParPair NilX, info
  leaveScope(n, closureScope)

proc tre(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of Symbol:
    # is this the usage of a proc symbol that is a closure? If so,
    # turn it into a `(fn, env)` tuple and generate the environment.
    let origTyp = c.typeCache.getType(n, {SkipAliases})
    let info = n.info
    if isClosureIterSym(n.symId):
      # Closure iter sym used as a VALUE — emit the wrapper-shape
      # iter-value tuple. Lambdalifting OWNS `.closure` iter
      # transformation (state machine + wrapper generated in
      # `transformClosureIter`), so the iter sym refers to the
      # state-machine entry (5 params: a, b, this, result, caller),
      # NOT a callable matching the tuple's declared type. The
      # WRAPPER sym (`iter.init.<mod>`) matches the declared 4-param
      # wrapper proctype; emit that directly.
      #
      # Pre-publish a placeholder signature for the wrapper so
      # downstream passes (eraiser / duplifier / destroyer) can
      # resolve its type before cps would have generated it.
      #
      # Env slot: eagerly allocate `(newobj (ref CoroType))` so the
      # iter VALUE owns its frame. Each evaluation of an iter-sym at
      # a value position creates an independent frame. The wrapper's
      # `caller.env != nil` reuse branch detects on first call (via
      # `this.callee == nil`) that this is a fresh frame and inits
      # it; subsequent calls dispatch via the resume slot
      # (`caller.fn`). This is Nim's shared-state semantics:
      # `let g = countup(1, 5); for x in g(): if x == 3: break; for x
      # in g(): echo x` resumes at 4 instead of restarting at 1.
      # The frame is wrapped in `(cast (ref RootObj) …)` because the
      # tuple slot's declared type is `(ref RootObj)` — Nim's real
      # `RootObj`, the base of the `CoroutineBase` hierarchy
      # `CoroType` inherits from.
      #
      # Important: this MUST come before the generic closure-proc
      # branch because iter decls match `RoutineKinds`/`isClosure`
      # too, but the env-injection path below would feed them the
      # wrong shape.
      let iterSym = n.symId
      coro_transform.publishWrapperSignature(iterSym, c.thisModuleSuffix)
      dest.copyIntoKind TupconstrX, info:
        emitIterTupleTypeFromSym(dest, iterSym, info)
        dest.addSymUse coro_transform.coroWrapperForExternIter(iterSym), info
        dest.copyIntoKind CastX, info:
          dest.copyIntoKind RefT, info:
            dest.addSymUse pool.syms.getOrIncl(BareRootObjName), info
          dest.copyIntoKind NewobjX, info:
            dest.copyIntoKind RefT, info:
              dest.addSymUse coro_transform.coroTypeForExternIter(iterSym), info
      inc n
    elif origTyp.typeKind in RoutineTypes and isClosure(origTyp) and c.typeCache.fetchSymKind(n.symId) in RoutineKinds:
      dest.copyIntoKind TupconstrX, info:
        dest.copyIntoKind TupleT, info:
          c.toProcType(dest, origTyp)
          dest.addRootRef info
        dest.addSymUse n.symId, info
        dest.untypedEnv info, c.env
      inc n
    else:
      let repWith = c.localToEnv.getOrDefault(n.symId)
      if repWith.field != SymId(0):
        # For stack-env local (`!needsHeap` + `EnvIsLocal`), `typedEnv`
        # returns the env OBJECT directly — deref-ing it is a type
        # error NIFC rejects. Mirror `treLocal`'s branch: only deref
        # when the env is heap-ref-shaped.
        dest.copyIntoKind DotX, info:
          if c.env.needsHeap:
            dest.copyIntoKind DerefX, info:
              dest.typedEnv info, c.env
          else:
            dest.typedEnv info, c.env
          dest.addSymUse repWith.field, info
        inc n
      else:
        takeTree dest, n
  of DotToken, UnknownToken, EofToken, Ident, SymbolDef,
     IntLit, UIntLit, FloatLit, CharLit, StringLit:
    takeTree dest, n
  of ParLe:
    case n.stmtKind
    of LocalDecls:
      treLocal c, dest, n
    of ProcS, FuncS, MethodS, ConverterS:
      treProcLift c, dest, n
    of IteratorS:
      # `.closure` iter decls: run the coro-transform pipeline here
      # so the state machine + frame type + wrapper are emitted at
      # lambdalifting time. The decl is retagged ProcS in the
      # process, so cps's `(IteratorY and ClosureP)` gate no longer
      # fires on it. `.passive` iter decls pass through to cps as
      # before.
      if isClosureIterDecl(n):
        transformClosureIter c, dest, n
      else:
        takeTree dest, n
    of MacroS, TemplateS, TypeS, EmitS, BreakS, ContinueS,
      ForS, IncludeS, ImportS, FromimportS, ImportexceptS,
      ExportS, CommentS,
      PragmasS:
      takeTree dest, n
    of ScopeS:
      c.typeCache.openScope()
      treSons(c, dest, n)
      c.typeCache.closeScope()
    of CoroforS:
      # `.closure` iter corofors are owned by lambdalifting — we expand
      # them into the trampoline here so the body walk goes through
      # `tre` (capture rewriting). `.passive` iter corofors pass
      # through to cps's `coro_transform.trCoroFor`.
      if isClosureCoroFor(c, n):
        trClosureCoroFor c, dest, n
      else:
        treSons(c, dest, n)
    of CallS, CmdS, BlockS, AsgnS, IfS, WhenS, WhileS,
      CaseS, RetS, YldS, StmtsS, PragmaxS, InclS, ExclS, ImportasS,
      ExportexceptS, DiscardS, TryS, RaiseS, UnpackdeclS,
      AssumeS, AssertS, CallstrlitS, InfixS, PrefixS, HcallS,
      StaticstmtS, BindS, MixinS, UsingS, AsmS, DeferS,
      NoStmt:
      case n.exprKind
      of CallKinds:
        genCall(c, dest, n)
      of DotX:
        takeInto dest, n:
          tre c, dest, n
          takeTree dest, n # don't look up field names here
          if n.hasMore: takeTree dest, n # optional inheritance depth
          if n.hasMore: takeTree dest, n # optional access-token string lit
      of CastX, ConvX:
        takeInto dest, n:
          treType c, dest, n
          while n.hasMore:
            tre c, dest, n
      of EnvpX:
        let info = n.info
        let envpScope = enterScope(n)
        dest.copyIntoKind DotX, info:
          dest.copyIntoKind DerefX, info:
            dest.copyIntoKind CastX, info:
              dest.copyIntoKind (if c.env.needsHeap: RefT else: PtrT), info:
                dest.takeTree n # type
              dest.addSymUse c.env.s, info
          assert n.kind == Symbol
          dest.takeTree n # the symbol
        leaveScope(n, envpScope)
      of TypeofX:
        takeTree dest, n
      of ToClosureX:
        treToClosure c, dest, n
      of ErrX, SufX, AtX, DerefX, PatX, ParX, AddrX, NilX,
        InfX, NeginfX, NanX, FalseX, TrueX, AndX, OrX, XorX,
        NotX, NegX, SizeofX, AlignofX, OffsetofX, OconstrX,
        AconstrX, BracketX, CurlyX, CurlyatX, OvfX, AddX,
        SubX, MulX, DivX, ModX, ShrX, ShlX, BitandX, BitorX,
        BitxorX, BitnotX, EqX, NeqX, LeX, LtX, CchoiceX,
        OchoiceX, PragmaxX, QuotedX, HderefX, DdotX, HaddrX,
        NewrefX, NewobjX, TupX, TupconstrX, SetconstrX,
        TabconstrX, AshrX, BaseobjX, HconvX, DconvX,
        CompilesX, DeclaredX, DefinedX, AstToStrX, BindSymX, BindSymNameX, InstanceofX,
        HighX, LowX, UnpackX, FieldsX, FieldpairsX,
        EnumtostrX, IsmainmoduleX, DefaultobjX, DefaulttupX,
        DefaultdistinctX, Delay0X, SuspendX, ExprX, DoX,
        ArratX, TupatX, PlussetX, MinussetX, MulsetX, XorsetX,
        EqsetX, LesetX, LtsetX, InsetX, CardX, EmoveX,
        DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX,
        InternalTypeNameX, InternalFieldPairsX, FailedX, IsX,
        KvX, NoExpr:
        if n.typeKind == TupleT and isLiftedClosureTuple(n):
          # An iter-value tuple or closure-proc tuple emitted by an earlier
          # pass — don't recurse into it, otherwise treProcType would fire
          # again on the inner ProctypeT and wrap it in ANOTHER tuple. The
          # shape is stable already; take it verbatim.
          takeTree dest, n
        elif n.typeKind in RoutineTypes:
          treProcType(c, dest, n)
        elif n.substructureKind == KvU:
          treKv(c, dest, n)
        else:
          treSons(c, dest, n)
  of ParRi:
    bug "unexpected ')' inside"

proc genObjectTypes(c: var Context; dest: var TokenBuf) =
  var objectTypes = initTable[SymId, seq[EnvField]]()
  for local, field in c.localToEnv:
    objectTypes.mgetOrPut(field.objType, @[]).add(field)
  for objType, fields in objectTypes:
    let beforeType = dest.len
    dest.copyIntoKind TypeS, NoLineInfo:
      dest.addSymDef objType, NoLineInfo
      dest.addDotToken() # no export marker
      dest.addDotToken() # no generic params
      dest.addDotToken() # no pragmas
      dest.copyIntoKind ObjectT, NoLineInfo:
        # inherits from RootObj:
        dest.addSymUse pool.syms.getOrIncl(BareRootObjName), NoLineInfo
        for field in items fields:
          let beforeField = dest.len
          dest.copyIntoKind FldY, NoLineInfo:
            dest.addSymDef field.field, NoLineInfo
            dest.addDotToken() # no export marker
            dest.addDotToken() # no pragmas
            var n = field.typ
            tre(c, dest, n) # type might need an environment parameter
            dest.addDotToken() # no default value
          programs.publish(field.field, dest, beforeField)
    programs.publish(objType, dest, beforeType)

proc elimLambdas*(pass: var Pass) =
  var n = pass.n  # Extract cursor locally
  var c = Context(counter: 0, typeCache: createTypeCache(), thisModuleSuffix: pass.moduleSuffix)
  c.coroCtx = coro_transform.Context(
    thisModuleSuffix: pass.moduleSuffix,
    typeCache: createTypeCache(),   # placeholder; swapped with c.typeCache per call
    coroTypes: createTokenBuf(10),
    continuationProcImpl: coro_transform.generateContinuationProcImpl(),
    hooks: lambdaHooks()
  )
  c.typeCache.openScope()
  tr c, pass.dest, n
  c.typeCache.closeScope()

  # second pass: generate environments and rewrite closure types/symbols.
  # Triggered by captures OR any closure-proc declaration / closure-typed nil:
  # pass 1's `trNil` wraps `(nil ClosureProc)` in a `tupconstr`, and `trProc`
  # marks `.closure` procs whose usages need tuple wrappers — both require
  # pass 2 to rewrite the surrounding proctypes/calls so types and values agree.
  if c.localToEnv.len > 0 or c.hasClosures:
    # some closure usage has been found, so we need to generate environments
    c.typeCache.openScope()
    let cap = pass.dest.len
    var oldDest = move pass.dest
    pass.dest = createTokenBuf(cap)
    var n2 = beginRead(oldDest)
    assert n2.stmtKind == StmtsS
    pass.dest.add n2 # stmts opener
    let stmtsScope = enterScope(n2)
    genObjectTypes(c, pass.dest)
    # Walk statements into a side buffer so we can prepend any
    # `.closure` iter coro frame types that `transformClosureIter`
    # accumulates during the walk. The state procs and wrappers
    # reference those types, so they must appear FIRST in the output
    # (alongside the env types).
    var stmtsBuf = createTokenBuf(cap)
    while n2.hasMore:
      tre(c, stmtsBuf, n2)
    # Publish the rewritten iter signatures NOW — `shouldPublish`
    # `start` offsets index into stmtsBuf. Doing this earlier would
    # change `tryLoadSym(iterSym)` mid-pass and confuse the
    # iter-sym-as-value check; deferring until after the walk keeps
    # the pass internally consistent. Order also matters: flush
    # BEFORE concatenating stmtsBuf into pass.dest so the indices
    # stay valid.
    for entry in c.coroCtx.shouldPublish:
      var buf = createTokenBuf(16)
      buf.copyTree stmtsBuf.cursorAt(entry.start)
      endRead(stmtsBuf)
      publishSignature buf, entry.sym, 0
    pass.dest.add c.coroCtx.coroTypes
    pass.dest.add stmtsBuf
    pass.dest.addParRi(n2.endInfo)
    leaveScope(n2, stmtsScope)
    endRead(oldDest)
    c.typeCache.closeScope()

  #echo "PRODUCED ", toString(pass.dest, false)
