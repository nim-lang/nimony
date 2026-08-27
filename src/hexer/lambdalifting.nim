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
    isCursor: bool
      ## the captured local was `{.cursor.}` — the env field must be a
      ## non-owning `.cursor` field too, or hoisting it into the heap env
      ## re-forms the very ref cycle the cursor was written to break
      ## (env strongly owns the object that owns the closure -> leak).

  ProcContext = object
    ## Per-LIFTING-ROOT state: created when either pass enters a top-level
    ## routine, carried from pass 1 to pass 2 via `Context.procEnvs`, and
    ## never consulted for any other root — so a local SymId reused by an
    ## unrelated routine (sem restarts numbering per proc; iterinliner's
    ## `ii temps reset per proc) can never observe another root's captures.
    ## The key keeps the env OBJECT type sym alongside the local sym; within
    ## one root every capture targets `procStack[0]`'s single shared env, so
    ## the type half is constant here — kept for shape-compatibility with
    ## `envTypeForProc` lookups.
    localToEnv: Table[(SymId, SymId), EnvField]
    env: CurrentEnv

  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    procStack: seq[SymId]
    dest: TokenBuf
    closureProcs, createsEnv, escapes: HashSet[SymId]
    currentProc: ProcContext
    procEnvs: Table[SymId, ProcContext]
      ## finished roots, keyed by root sym: pass 1 deposits each root's
      ## ProcContext here; pass 2 re-installs it while walking that root;
      ## `genObjectTypes` emits every root's env object types from it.
    envFieldType: Table[SymId, Cursor] ## env FIELD sym -> captured local's type
      ## (typenav cannot type `(envp ...)` nodes, so `genCall` resolves a
      ## capture-rewritten callee's type through this instead — field syms
      ## are counter-minted per module, so this table is safely module-wide)
    coroCtx: coro_transform.Context
      ## Shadow `coro_transform.Context` used to drive `.closure` iter
      ## state-machine generation. We loan our `typeCache` to it via
      ## `swap` while `transformCoroutineDecl` runs, then swap back.
      ## `coroTypes` and `shouldPublish` accumulate here across all
      ## iters in the module and get flushed in `elimLambdas`.
    pendingIterSigs: seq[(SymId, TokenBuf)]
      ## Rewritten `.closure` iter signatures, snapshotted by
      ## `transformClosureIter` while `shouldPublish` offsets still
      ## index the buffer the iter was written into (for a NESTED
      ## iter that is treProcLift's local lift buffer, not stmtsBuf).
      ## Published at the end of pass 2 — publishing earlier would
      ## flip `tryLoadSym(iterSym)` mid-pass and confuse the
      ## iter-sym-as-value check.

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)
  {.ensuresNif: addedAny(dest).}

proc isClosureIterDecl(n: Cursor): bool =
  ## True if `n` is at an `(iterator :sym …)` whose pragmas carry
  ## `.closure`. Used by pass 1 and pass 2 (route
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

proc isClosure(typ: Cursor): bool {.inline.} = procHasPragma(typ, ClosureP)

proc trKv(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## `(kv FIELD value)` object-constructor pairs: the key is a field
  ## identity, not a value use. It can share a SymId with a captured
  ## local/param of the same spelling; recursing would rewrite it into an
  ## `(envp …)` access and corrupt the constructor (lengc then reports
  ## "expected field name but got (envp …)"). Take the key verbatim and
  ## only rewrite the value(s). Mirrors pass 2's `treKv` and the DotX/DdotX
  ## selector guard; same class as iterinliner's field-identity guard.
  copyInto dest, n:
    dest.takeTree n # key (field identity — never rewrite)
    while n.hasMore:
      tr(c, dest, n)

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    let name = n.symId
    takeTree dest, n # name
    takeTree dest, n # export marker
    takeTree dest, n # pragmas
    let typ = n
    tr(c, dest, n)
    c.typeCache.registerLocal(name, kind, typ, n)
    tr(c, dest, n)  # value

proc trProc(c: var Context; dest: var TokenBuf; n: var Cursor) =
  #c.typeCache.openScope(ProcScope)
  let decl = n
  copyInto dest, n:
    let symId = n.symId
    if c.procStack.len == 0:
      c.currentProc = ProcContext()   # fresh per lifting root
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
          c.escapes.incl symId
      if i == ParamsPos:
        takeInto dest, n:
          while n.hasMore:
            trLocal c, dest, n
      else:
        takeTree dest, n
    if isConcrete:
      tr(c, dest, n)
    else:
      takeTree dest, n
    discard c.procStack.pop()
    if c.procStack.len == 0:
      c.procEnvs[symId] = move c.currentProc   # hand the root's state to pass 2
  c.typeCache.closeScope()

proc envTypeForProc(c: var Context; procId: SymId): SymId =
  let s = extractVersionedBasename(pool.syms[procId])
  result = pool.syms.getOrIncl(derivedName(s, "env") & "." & c.thisModuleSuffix)

proc localToField(c: var Context; n: Cursor; local, typ: SymId; isCursor = false): SymId =
  if c.currentProc.localToEnv.hasKey((typ, local)):
    result = c.currentProc.localToEnv.getOrQuit((typ, local)).field
  else:
    var name = pool.syms[local]
    extractBasename name
    name.add "`f."
    name.add $c.counter
    inc c.counter
    name.add "."
    name.add c.thisModuleSuffix
    result = pool.syms.getOrIncl(name)
    let localTyp = c.typeCache.getType(n)
    c.currentProc.localToEnv[(typ, local)] = EnvField(objType: typ, field: result, typ: localTyp, isCursor: isCursor)
    c.envFieldType[result] = localTyp

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  takeInto dest, n:
    if n.kind == Symbol and
        c.typeCache.getLocalInfo(n.symId).kind notin {ParamY, LetY, VarY, ResultY}:
      # if a closure proc is called, we don't want to see it as "escaping".
      # But when the callee is a LOCAL holding a closure value, it must still
      # go through `tr`: a cross-proc use in call position is a capture like
      # any other and needs the envp rewrite, otherwise the enclosing proc
      # never creates an environment for it.
      dest.addSubtree n
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
  n.into:
    # `(nil <Type>)` for a closure proctype OR itertype (.closure / .passive)
    # lowers to a `{fnptr, env}` tuple constructor — that's the runtime shape
    # of Nim closures and of our wrapper-signature iter values.
    let isIter = n.hasMore and itertypeNeedsTuple(n)
    let isCloseable = n.hasMore and (isIter or procHasPragma(n, ClosureP))
    if isIter:
      dest.copyIntoKind TupconstrX, info:
        # rewrite the inner itertype to its wrapper-shape tuple
        emitIterTupleTypeFromParams(dest, n, info)
        if n.hasMore: skip n # might have another nil value
        dest.addParPair NilX, info
        dest.addParPair NilX, info
    elif isCloseable:
      # nil closure must be a tuple:
      dest.copyIntoKind TupconstrX, info:
        dest.takeTree n # type
        if n.hasMore: skip n # might have another nil value
        dest.addParPair NilX, info
        dest.addParPair NilX, info
    else:
      dest.addParLe NilX, (if n.hasMore: n.info else: n.endInfo)
      while n.hasMore: takeTree dest, n
      dest.addParRi(n.endInfo)

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of DotToken, UnknownToken, EofToken, ParLe, ParRi, ExtendedSuffix, LineInfoLit, Ident, SymbolDef,
     IntLit, UIntLit, FloatLit, CharLit, StrLit:
    takeTree dest, n
  of Symbol:
    let loc = c.typeCache.getLocalInfo(n.symId)
    # CursorY included: a captured `{.cursor.}` local must ALSO be hoisted into
    # the environment. Omitting it left the closure body referencing the outer
    # local symbol directly (never rewritten to an env access); the following
    # duplifier pass then hit "could not find symbol" doing getType on a symbol
    # that lives in no scope it can see. The env field is marked `.cursor` (see
    # localToField / genObjectTypes) so hoisting preserves the non-owning
    # semantics rather than re-forming the ref cycle.
    if loc.kind in {ParamY, LetY, VarY, ResultY, CursorY}:
      let cross = loc.crossedProc.int
      if cross > 0:
        for i in c.procStack.len - cross ..< c.procStack.len:
          c.closureProcs.incl(c.procStack[i])
        let destEnv = c.procStack[0] #c.procStack.len - cross]
        c.createsEnv.incl destEnv
        let envType = c.envTypeForProc(destEnv)
        let fld = c.localToField(n, n.symId, envType, isCursor = loc.kind == CursorY)
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
  of TagLit:
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
          emitIterTupleTypeFromParams(dest, n, n.info)
          publishIt = true
        while n.hasMore: takeTree dest, n
      if publishIt:
        programs.publish(typeSym, dest, typeStart)
    of IteratorS:
      # `.closure` iter decls are owned by lambdalifting (pass 2
      # generates the state machine via coro_transform).
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
      LabS, JmpS, NoStmt:
      case n.exprKind
      of CallKinds:
        trCall c, dest, n
      of TypeofX:
        takeTree dest, n
      of NilX:
        trNil c, dest, n
      of DotX, DdotX:
        # The selector is a field identity, not a value use. It can
        # share a SymId with a captured local/param of the same
        # spelling; recursing would rewrite it into an `(envp …)`
        # access and corrupt the dot. (Same guard as pass 2's tre and
        # coro_transform's coroTr.)
        takeInto dest, n:
          tr c, dest, n            # object expression
          takeTree dest, n         # field selector
          while n.hasMore:
            takeTree dest, n       # optional inheritance depth / access token
      of ToClosureX:
        trSons(c, dest, n)
      of ErrX, SufX, AtX, DerefX, PatX, ParX, AddrX,
        InfX, NeginfX, NanX, FalseX, TrueX, AndX, OrX, XorX,
        NotX, NegX, SizeofX, AlignofX, OffsetofX, OconstrX,
        AconstrX, BracketX, CurlyX, CurlyatX, OvfX, AddX,
        SubX, MulX, DivX, ModX, ShrX, ShlX, BitandX, BitorX,
        BitxorX, BitnotX, EqX, NeqX, LeX, LtX, CastX, ConvX,
        CchoiceX, OchoiceX, PragmaxX, QuotedX, HderefX,
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
        if n.substructureKind == KvU:
          # `(kv FIELD value)` object-constructor pair: guard the
          # field-identity key against the capture rewrite (KvX table
          # keys are real value uses and stay in trSons).
          trKv(c, dest, n)
        else:
          trSons(c, dest, n)
  else:
    bug "unexpected ')' inside" # classic: a physical ParRi; nifcore: suffix kinds (never heads)

when false:
  proc paramsWithClosurePragma(typ: Cursor): bool =
    var typ = typ
    skip typ
    skip typ # return type
    result = hasPragma(typ, ClosureP)

const
  # Lambdalifting-specific names (not in coro_transform). The lowered closure
  # env *param* (`ep.0`) and its emitter now live in `coro_transform` as
  # `ClosureEnvParamName` / `addClosureEnvParam`, shared with any pass that must
  # emit the identical env slot; only the env *local* stays local here.
  EnvLocalName = "`el.0"

# `RootObjName` / `coroWrapperProcName` / `emitIterTupleType*` /
# `isClosureIterSym` / `isLiftedClosureTuple` now live in
# `coro_transform`. The wrapper-signature shape is owned there too, so
# both passes stay in lock-step automatically.

proc addRootRef(dest: var TokenBuf; info: NifLineInfo)
  {.ensuresNif: addedType(dest).} =
  dest.copyIntoKind RefT, info:
    dest.addSymUse pool.syms.getOrIncl(BareRootObjName), info

type
  UntypedEnvMode = enum
    WantValue, WantAddr

proc untypedEnv(dest: var TokenBuf; info: NifLineInfo; env: CurrentEnv; mode=WantValue)
  {.ensuresNif: addedExpr(dest).} =
  if env.s == SymId(0):
    bug "lambdalifting untypedEnv: no environment in scope at " & infoToStr(info)
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
    # The parameter already has the erased type AND is already the
    # env's address: for a stack env `ep.0 is `pointer` (the caller
    # passed `addr el.0`), for a heap env it's `(ref RootObj)`. Either
    # way FORWARD IT AS-IS regardless of `mode` — WantAddr here would
    # take the address of the parameter SLOT (ptr-to-ptr), so a nested
    # closure calling a deeper closure handed it garbage.
    dest.addSymUse env.s, info

proc typedEnv(dest: var TokenBuf; info: NifLineInfo; env: CurrentEnv)
  {.ensuresNif: addedExpr(dest).} =
  if env.s == SymId(0):
    bug "lambdalifting typedEnv: no environment in scope at " & infoToStr(info)
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
  let publishedBefore = c.coroCtx.shouldPublish.len
  coro_transform.transformCoroutineDecl(c.coroCtx, dest, n)
  swap c.coroCtx.typeCache, c.typeCache
  # Snapshot the rewritten signature NOW, while `shouldPublish.start`
  # still indexes `dest`. For a nested iter `dest` is treProcLift's
  # local lift buffer which is concatenated (at a shifted offset) into
  # the outer buffer afterwards — flushing against stmtsBuf at the end
  # of the pass would read a random subtree and publish garbage.
  for i in publishedBefore ..< c.coroCtx.shouldPublish.len:
    let entry = c.coroCtx.shouldPublish[i]
    var buf = createTokenBuf(16)
    buf.copyTree dest.cursorAt(entry.start)
    c.pendingIterSigs.add (entry.sym, ensureMove buf)
  c.coroCtx.shouldPublish.setLen publishedBefore

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
  return typ.typeKind == ClosureTupleT

proc trClosureCoroFor(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Expand `(corofor (call closure-iter args... (haddr forLoopVar)) (block ...))`
  ## into the trampoline. Mirrors `coro_transform.trCoroFor` but walks
  ## the body via lambdalifting's `tre` (so closure captures inside
  ## the for-loop body get rewritten to env accesses).
  let info = n.info
  n.into: # skip (corofor

    # ---- first child: (call iter-or-tupat args... (haddr forLoopVar)) ----
    assert n.exprKind in CallKinds, "corofor: expected iter call as first child"
    let callStart = n # past CallS tag
    n = sub(n)
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
    var valInfoForEnv: NifLineInfo = default(NifLineInfo)
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
    n = callStart; skip n # close iter call

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


proc treSons(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    while n.hasMore:
      tre(c, dest, n)

proc treParamsWithEnv(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    while n.hasMore:
      tre(c, dest, n)
    addClosureEnvParam dest, NoLineInfo, SymId(0)

proc treProcType(c: var Context; dest: var TokenBuf; n: var Cursor) =
  if itertypeNeedsTuple(n):
    # Closure / passive iterators get the wrapper-signature tuple shape
    # rather than the closure-proc shape. The wrapper signature lives in cps;
    # we mirror it here so the lifter sees the final tuple shape at
    # duplifier/destroyer time and hooks line up with cps's wrapper-proc
    # emission. `.closure` and `.passive` iters share the same tuple shape;
    # they differ only at the cps trampoline level.
    emitIterTupleTypeFromParams(dest, n, n.info)
  elif isClosure(n):
    # type is really a `(closureTuple fn env)`:
    let info = n.info
    copyIntoKind dest, ClosureTupleT, info:
      copyIntoKind dest, ProctypeT, info:
        dest.addDotToken() # nilability tag
        let inputKind = n.typeKind
        let isProctypeInput = inputKind == ProctypeT
        # the callers guarantee `inputKind in RoutineTypes` here
        n.into:
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
            addClosureEnvParam dest, info, SymId(0)
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
      addRootRef dest, info
  else:
    # `itertype` (a first-class closure-iterator value) has the SAME compact
    # 4-field shape as `proctype`; treating it as an 8-field routine decl walks
    # `BodyPos` fields past the end of the tree. Under nifcore there is no
    # `ParRi` token to stop on, so that overrun reads off the end of the buffer
    # and trips nifcore's cursor assert instead (issue #2177).
    let isCompactRoutine = n.typeKind in {ProctypeT, ItertypeT}
    takeInto dest, n:
      if isCompactRoutine:
        # compact layout: nilability, params, retType, pragmas
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
  let s = n.childCursor.symId
  let fld = c.currentProc.localToEnv.getOrDefault((c.currentProc.env.typ, s))
  let kind = n.symKind
  if fld.field != SymId(0):
    # the local is already a field of an environment object
    let info = n.info
    n.into: # into the decl
      let name = n.symId
      for i in 1..3: skip n
      # register the local anyway to keep the type navigator happy:
      c.typeCache.registerLocal(name, kind, n)
      skip n # type
      if n.kind != DotToken:
        # generate an assignment:
        dest.copyIntoKind AsgnS, info:
          dest.copyIntoKind DotX, info:
            # Deref for a heap env (ref) but ALSO for EnvIsParam with a
            # stack env: typedEnv then yields `(cast (ptr EnvT) ep)` — a
            # pointer either way. Only an EnvIsLocal stack env-local IS
            # the object directly.
            if c.currentProc.env.needsHeap or c.currentProc.env.mode == EnvIsParam:
              dest.copyIntoKind DerefX, info:
                dest.typedEnv info, c.currentProc.env
            else:
              dest.typedEnv info, c.currentProc.env
            dest.addSymUse fld.field, info
          tre c, dest, n # value
      else:
        inc n # the dot value
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

proc treParams(c: var Context; dest, init: var TokenBuf; n: var Cursor; doAddEnvParam: bool; envTyp: SymId; ownsEnvLocal: bool) =
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
        let fld = c.currentProc.localToEnv.getOrDefault((c.envTypeForProc(c.procStack[0]), name))
        if fld.field != SymId(0):
          # XXX Check here for memory safety violations: Cannot capture a `var T` parameter
          # We're emitting `<env>.<field> = <param>` into the
          # body-prologue (treProcBody splices `init` after the env-local
          # decl). `c.currentProc.env` isn't usable yet — it'll be set up by
          # treProcBody AFTER this — so build the env access directly.
          # Which `<env>` depends on who OWNS the environment object:
          #
          # - this proc creates it (`createsEnv`): the `el.0 env-local.
          #   `envTyp == SymId(0)` means "heap env" (the caller sets it
          #   that way when the closureOwner escapes): the env-local is
          #   `ref EnvT` and needs a deref before `.field`; for stack
          #   env the env-local IS the object.
          # - this proc is a closure RECEIVING the owner's env through
          #   the `ep.0 param (its own param captured by a deeper
          #   closure): cast the erased param to the field's env object
          #   type and deref — mirrors pass 2's EnvpX lowering.
          init.copyIntoKind AsgnS, paramInfo:
            init.copyIntoKind DotX, paramInfo:
              if ownsEnvLocal:
                if envTyp == SymId(0):
                  init.copyIntoKind DerefX, paramInfo:
                    init.addSymUse pool.syms.getOrIncl(EnvLocalName), paramInfo
                else:
                  init.addSymUse pool.syms.getOrIncl(EnvLocalName), paramInfo
              elif doAddEnvParam:
                init.copyIntoKind DerefX, paramInfo:
                  init.copyIntoKind CastX, paramInfo:
                    init.copyIntoKind (if envTyp == SymId(0): RefT else: PtrT), paramInfo:
                      init.addSymUse fld.objType, paramInfo
                    init.addSymUse pool.syms.getOrIncl(ClosureEnvParamName), paramInfo
              else:
                bug "lambdalifting treParams: captured param but no environment access at " & infoToStr(paramInfo)
              init.addSymUse fld.field, paramInfo
            init.addSymUse name, paramInfo

    if doAddEnvParam:
      addClosureEnvParam dest, n.endInfo, envTyp

proc treProcBody(c: var Context; dest, init: var TokenBuf; n: var Cursor; sym: SymId; needsHeap: bool) =
  if n.stmtKind == StmtsS:
    copyInto dest, n:
      let oldEnv = c.currentProc.env
      if c.createsEnv.contains(sym):
        let envTyp = c.envTypeForProc(sym)
        c.currentProc.env = CurrentEnv(s: pool.syms.getOrIncl(EnvLocalName), mode: EnvIsLocal, typ: envTyp, needsHeap: needsHeap)
        dest.copyIntoKind VarS, NoLineInfo:
          dest.addSymDef c.currentProc.env.s, NoLineInfo
          dest.addDotToken() # no export marker
          dest.addDotToken() # no pragmas
          if needsHeap:
            dest.copyIntoKind RefT, NoLineInfo:
              dest.addSymUse c.currentProc.env.typ, NoLineInfo
            dest.copyIntoKind NewobjX, NoLineInfo:
              dest.copyIntoKind RefT, NoLineInfo:
                dest.addSymUse c.currentProc.env.typ, NoLineInfo
          else:
            dest.addSymUse c.currentProc.env.typ, NoLineInfo
            dest.addDotToken() # no default value
        if needsHeap:
          # Note: If the environment is on the stack, a single `wasMoved`
          # hook will be generated for it so we don't need to do anything here.
          # Otherwise, we need to init the environment via the `=wasMoved` hooks:
          for _, field in c.currentProc.localToEnv:
            if field.objType == c.currentProc.env.typ:
              dest.copyIntoKind WasmovedX, NoLineInfo:
                dest.copyIntoKind HaddrX, NoLineInfo:
                  dest.copyIntoKind DotX, NoLineInfo:
                    if needsHeap:
                      dest.copyIntoKind DerefX, NoLineInfo:
                        dest.addSymUse c.currentProc.env.s, NoLineInfo
                    else:
                      dest.addSymUse c.currentProc.env.s, NoLineInfo
                    dest.addSymUse field.field, NoLineInfo

      elif c.closureProcs.contains(sym):
        # The `ep.0 param carries the OUTERMOST proc's environment —
        # captures always target `procStack[0]`'s env (one shared env,
        # see the outerA/outerB note in pass 1) — so type it as that,
        # not as this closure's own (never materialized) env type.
        c.currentProc.env = CurrentEnv(s: pool.syms.getOrIncl(ClosureEnvParamName), mode: EnvIsParam, typ: c.envTypeForProc(c.procStack[0]), needsHeap: needsHeap)
      else:
        c.currentProc.env = CurrentEnv(s: SymId(0), mode: EnvIsParam, typ: SymId(0), needsHeap: needsHeap)
      dest.add init
      while n.hasMore:
        tre(c, dest, n)
      var needsHeapB = c.currentProc.env.needsHeap
      c.currentProc.env = oldEnv
      c.currentProc.env.needsHeap = c.currentProc.env.needsHeap or needsHeapB
  else:
    tre(c, dest, n)

proc treProc(c: var Context; dest: var TokenBuf; n: var Cursor): SymId =
  ## Returns the routine's symbol when the decl is concrete (so the
  ## caller can schedule the rewritten decl for republishing), else 0.
  result = SymId(0)
  var init = createTokenBuf(10)
  let decl = n
  copyInto dest, n:
    var isConcrete = true # assume it is concrete
    let sym = n.symId
    if c.procStack.len == 0:
      c.currentProc = c.procEnvs.getOrDefault(sym)   # pass 1's state for this root
    c.procStack.add(sym)
    let closureOwner = c.procStack[0]
    let needsHeap = c.escapes.contains(closureOwner)
    for i in 0..<BodyPos:
      if i == ParamsPos:
        c.typeCache.openProcScope(sym, decl, n)
        let envType = if needsHeap: SymId(0) else: c.envTypeForProc(closureOwner)
        treParams c, dest, init, n, c.closureProcs.contains(sym), envType,
                  c.createsEnv.contains(sym)
      else:
        if i == TypevarsPos:
          isConcrete = n.substructureKind != TypevarsU
        if i == ReturnTypePos and isConcrete:
          treType c, dest, n
        else:
          takeTree dest, n

    if isConcrete:
      treProcBody(c, dest, init, n, sym, needsHeap)
      result = sym
    else:
      takeTree dest, n
    discard c.procStack.pop()
  c.typeCache.closeScope()

proc treProcLift(c: var Context; dest: var TokenBuf; n: var Cursor) =
  if c.procStack.len == 0:
    swap c.dest, dest
  var lift = createTokenBuf(16)
  discard treProc(c, lift, n)
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

proc capturedBaseType(c: var Context; o: Cursor): Cursor =
  ## Type an object expression that may bottom out at a pass-1 capture
  ## rewrite `(envp EnvType field)`. typenav cannot type `envp` (the env
  ## field's type only lives in `c.envFieldType`, not the global symtab), so
  ## `getType` on any dot chain rooted at one falls back to `auto`. Walk the
  ## chain here, resolving the envp leaf through `envFieldType` and every
  ## intermediate field through `lookupField` (which handles ref/ptr auto-
  ## deref). `hderef`/`deref` wrappers are transparent — `lookupField` derefs
  ## the ref itself. Returns nil for any shape we don't recognize.
  # Shape probe, not a dispatch: recognize the few wrapper forms and
  # delegate EVERY other expr kind to typenav (an if/elif chain, like
  # duplifier's probes — a `case` here would need to enumerate the whole
  # NimonyExpr enum only to say "getType" for all of it).
  let k = o.exprKind
  if k == EnvpX:
    var f = o
    inc f # past env type symbol
    inc f # at the field symbol
    result =
      if f.kind == Symbol and c.envFieldType.hasKey(f.symId):
        c.envFieldType.getOrQuit(f.symId)
      else:
        default(Cursor)
  elif k in {HderefX, DerefX}:
    var inner = o
    inc inner
    result = capturedBaseType(c, inner)
  elif k in {DotX, DdotX}:
    var obj = o
    inc obj # past dot / ddot tag
    var fld = obj
    skip fld # past object expression -> field symbol
    let objTyp = capturedBaseType(c, obj)
    result =
      if fld.kind == Symbol and not cursorIsNil(objTyp):
        lookupField(c.typeCache, objTyp, fld.symId)
      else:
        default(Cursor)
  else:
    result = c.typeCache.getType(o, {SkipAliases})

proc toNonClosureProcType(c: var Context; dest: var TokenBuf; n: Cursor) =
  # just remove closure pragma from proctype
  var n = n
  # ItertypeT: genCall runs before cps rewrites itertypes to wrapper
  # proctypes, so a first-class closure-iter value lands here too.
  assert n.typeKind in {ProctypeT, ProcT, ItertypeT}
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

proc calleeHasClosureParam(typ: Cursor): bool =
  ## True if a callee proctype has any closure/lifted-closure-tuple parameter.
  ## The env==nil else-branch de-closures the callee via `toNonClosureProcType`
  ## and calls it bare; that reconstruction is only ABI/type-sound for simple
  ## parameter types. A parameter that is itself a closure
  ## (`proc(fn: proc() {.closure.})`) lowers to a `{fn,env}` tuple whose element
  ## shape the bare cast cannot match, so the (for a genuine closure, dead)
  ## else-branch emits a type-invalid call. Such a callee is never a plausible
  ## ToClosureX target anyway — nothing passes a bare non-closure proc to a
  ## closure-of-closure parameter — so skip the nil-check and use the plain
  ## tuple-unpack call, exactly as before upstream #2074.
  var t = typ
  if t.typeKind != ProctypeT: return false
  skipToParams t
  if t.substructureKind != ParamsU: return false
  inc t # into (params …), at the first param
  while t.hasMore:
    if t.substructureKind == ParamU:
      var pt = t
      inc pt   # (param -> name
      skip pt  # name -> export
      skip pt  # export -> pragmas
      skip pt  # pragmas -> type
      if isClosure(pt) or isLiftedClosureTuple(pt):
        return true
    skip t
  result = false

proc genCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let callNode = n  # the call node itself
  let callStart = n
  n = sub(n)
  var typ = c.typeCache.getType(n, {SkipAliases})
  if n.exprKind == EnvpX:
    # capture-rewritten callee `(envp EnvType field)` from pass 1: typenav
    # cannot type envp nodes, so resolve the captured local's type through
    # the field instead — otherwise a captured-closure call is emitted as
    # a plain call of the tuple value.
    var fieldSym = n
    inc fieldSym # at the env type symbol
    inc fieldSym # at the field symbol
    if fieldSym.kind == Symbol and c.envFieldType.hasKey(fieldSym.symId):
      typ = c.envFieldType.getOrQuit(fieldSym.symId)
  elif not (isClosure(typ) or isLiftedClosureTuple(typ)) and
       n.exprKind in {DotX, DdotX}:
    # A closure-typed OBJECT FIELD reached THROUGH a captured env — the
    # call-position sibling of the direct-envp case above (and of the
    # nil-compare shape generalization in coro_transform, 7bbe47b0). Pass 1
    # rewrote the object base of `state.onDrawImageRecreated()` to
    # `(envp …)`, which typenav cannot type, so `getType` on the whole dot
    # fell back to `auto`; the closure call would then be emitted as a
    # direct call of the tuple value (clang: "called object type '…tuple…'
    # is not a function"). Re-resolve the callee's type through the env-aware
    # `capturedBaseType` walk and take the closure field type. Only overrides
    # when the field itself is a closure, so a non-captured dot (getType
    # already succeeds) and a plain-proc field (a nimcall proctype) are left
    # untouched.
    var obj = n
    inc obj  # past the dot / ddot tag
    var fld = obj
    skip fld # past the object expression -> field symbol
    let objTyp = capturedBaseType(c, obj)
    if fld.kind == Symbol and not cursorIsNil(objTyp):
      let ftyp = lookupField(c.typeCache, objTyp, fld.symId)
      if isClosure(ftyp) or isLiftedClosureTuple(ftyp):
        typ = ftyp
  # Upstream #2142: `isStatic` is computed up-front and drives `wantsEnv` below
  # (a static call to a proc that captures nothing is no longer a closure). Our
  # `var typ` re-resolution above (envp / captured-dot callees) feeds the
  # non-static `else` branch of `wantsEnv`, so both survive.
  let isStatic = n.kind == Symbol and isStaticCall(c, n.symId)
  # A closure iter-value call target type can appear here in two guises:
  #   - raw `(itertype … (pragmas (closure)))` — `isClosure` matches.
  #   - lifted `(closureTuple <proctype> (ref RootObj))` — `isLiftedClosureTuple`
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
  let wantsEnv = if isStatic:
                   c.closureProcs.contains(n.symId)
                 else:
                   isClosure(typ) or isLiftedClosureTuple(typ)
  var tmp = SymId(0)
  var needNilCheck = false
  var addTmpVar = false
  if wantsEnv:
    if isStatic:
      dest.addParLe(callNode.cursorTagId, callNode.info)
      # do not produce a tuple:
      dest.addSubtree n
      inc n
    else:
      # The env==nil runtime dispatch (upstream d4435b37) exists for a raw
      # `.closure` PROCTYPE callee — a param or proctype-typed var that may hold
      # a non-closure proc converted via ToClosureX: when its env slot is nil we
      # must call the bare fn WITHOUT an env arg. Gate strictly on `ProctypeT`:
      #  - A *lifted closure tuple* callee (house's cross-module canonicalized
      #    closure vars/values, canonForeignDecl 86774db7) is never such a
      #    conversion — its fn slot is an env-taking lifted proc — and a tuple
      #    would also assert in toNonClosureProcType.
      #  - A `ProcT` callee — a local closure var whose type resolved to the
      #    concrete lifted lambda decl (`let enqueue = proc() {.closure.} = …`) —
      #    is likewise always a genuine closure with a live env, never a
      #    ToClosureX bare proc. Letting the nil-check fire there fed the lambda's
      #    whole DECL (body and all) to toNonClosureProcType, which copied the
      #    still-`ddot` field accesses in that body verbatim into the cast type;
      #    those raw ddots then reached the duplifier as an eliminated-in-desugar
      #    node. Both defects only surface on real closure code, never the tiny
      #    upstream tests.
      needNilCheck = typ.typeKind == ProctypeT and isClosure(typ) and
                     not calleeHasClosureParam(typ)
      if n.kind == Symbol:
        tmp = n.symId
        inc n
      else:
        # Expression callee: bind the (fn, env) tuple to a temp first. The
        # wrapper spans the whole call (and the nil-dispatch `if`, when it
        # fires) and is closed at the end of genCall (`addTmpVar`).
        # A VOID call sits in statement position, so wrap in `(stmts …)`:
        # an ExprX there makes njvl materialize a `void` result temp
        # (invalid C — upstream's shape only ever saw `(): int` callees).
        # A value-returning call needs the ExprX to stay an expression.
        addTmpVar = true
        var rt = typ
        skipToParams rt
        skip rt # params -> return type
        if isVoidType(rt):
          dest.addParLe(StmtsS, info)
        else:
          dest.addParLe(ExprX, info)
        copyIntoKind dest, StmtsS, info:
          tmp = pool.syms.getOrIncl("`llTemp." & $c.counter)
          inc c.counter
          copyIntoKind dest, VarS, info:
            dest.addSymDef tmp, info
            dest.addDotToken() # no export marker
            dest.addDotToken() # no pragmas
            var t = typ
            # treType, not tre: a captured lambda's type can be decl-shaped
            # (`(proc ...)`), which `tre` would lift as a declaration.
            treType c, dest, t
            tre c, dest, n # value
      if needNilCheck:
        # Bare-call branch bodies, matching upstream d4435b37/#2150: the
        # njvl/xelim line at this tip lowers them correctly for both void
        # and value-returning calls (the pre-#2153 engines needed explicit
        # `(stmts …)`/`(expr …)` wrappers here; those now MIS-lower).
        dest.addParLe IfS, info
        dest.addParLe ElifU, info
        # env == nil means calls the non closure procedure that was converted to a closure procedure
        copyIntoKind dest, NeqX, info:
          if c.currentProc.env.needsHeap:
            dest.addRootRef info
          else:
            dest.copyIntoKind PointerT, info: discard
          copyIntoKind dest, TupatX, info:
            dest.addSymUse tmp, info
            dest.addIntLit 1, info
          dest.addParPair NilX, info
      dest.addParLe(callNode.cursorTagId, callNode.info)
      # the temp/local holds the (fn, env) tuple — the callee is its fn slot:
      copyIntoKind dest, TupatX, info:
        dest.addSymUse tmp, info
        dest.addIntLit 0, info
  else:
    dest.addParLe(callNode.cursorTagId, callNode.info)
    if isStatic:
      takeTree dest, n
  let firstArg = n
  while n.hasMore:
    tre(c, dest, n)
  if wantsEnv:
    if isStatic:
      if c.currentProc.env.s != SymId(0):
        let mode = if c.currentProc.env.needsHeap: WantValue else: WantAddr
        # use the current environment as the last parameter:
        untypedEnv dest, info, c.currentProc.env, mode
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
  n = callStart; skip n

  if needNilCheck:
    dest.addParRi() # end of ElifU
    copyIntoKind dest, ElseU, info:
      dest.addParLe(callNode.cursorTagId, callNode.info)
      copyIntoKind dest, CastX, info:
        c.toNonClosureProcType dest, typ
        copyIntoKind dest, TupatX, info:
          dest.addSymUse tmp, info
          dest.addIntLit 0, info
      var n2 = firstArg
      while n2.hasMore:
        tre(c, dest, n2)
      dest.addParRi() # end of call
    dest.addParRi() # end of IfS
  if addTmpVar:
    # Not tied to needNilCheck: our gate can skip the nil-dispatch while the
    # expression-callee temp (and its ExprX wrapper) is still open.
    dest.addParRi() # end of ExprX

proc toProcType(c: var Context; dest: var TokenBuf; n: Cursor) =
  var n = n
  let info = n.info
  copyIntoKind dest, ProctypeT, info:
    dest.addDotToken() # nilability tag
    let inputKind = n.typeKind
    n.into:
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
        addClosureEnvParam dest, info, SymId(0)
      tre c, dest, n # return type
      # pragmas:
      tre c, dest, n
      while n.hasMore: skip n

proc treKv(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    dest.takeTree n # key
    while n.hasMore:
      tre(c, dest, n)

proc nonClosureToClosure(c: var Context; dest: var TokenBuf; n: var Cursor; origTyp: Cursor; info: NifLineInfo) =
  dest.copyIntoKind TupconstrX, info:
    dest.copyIntoKind ClosureTupleT, info:
      c.toProcType(dest, origTyp)
      dest.addRootRef info
    dest.copyIntoKind CastX, info:
      c.toProcType(dest, origTyp)
      if n.isTagLit:
        treSons c, dest, n
      else:
        dest.takeTree n
    dest.addParPair NilX, info

proc treToClosure(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let origTyp = c.typeCache.getType(n, {SkipAliases})
  n.into:
    nonClosureToClosure c, dest, n, origTyp, info

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
      if c.closureProcs.contains(n.symId):
        dest.copyIntoKind TupconstrX, info:
          dest.copyIntoKind ClosureTupleT, info:
            c.toProcType(dest, origTyp)
            dest.addRootRef info
          dest.addSymUse n.symId, info
          if c.currentProc.env.s == SymId(0):
            # A capturing closure value referenced where no enclosing
            # environment exists: there is nothing to pass — nil, mirroring the
            # static-call lowering in `treCall`. (Upstream #2142 routes the
            # *non*-capturing case to the `else` branch below; this guard remains
            # for the genuine-capturer-without-current-env case we hit in real
            # closure code, and is a strict superset of upstream's unconditional
            # `untypedEnv`.)
            dest.copyIntoKind NilX, info: discard
          else:
            dest.untypedEnv info, c.currentProc.env
        inc n
      else:
        # proc with closure pragma but doesn't capture any variables.
        # so it is actually not closure.
        nonClosureToClosure c, dest, n, origTyp, info
    else:
      let repWith = c.currentProc.localToEnv.getOrDefault((c.currentProc.env.typ, n.symId))
      if repWith.field != SymId(0):
        # For an EnvIsLocal STACK env, `typedEnv` returns the env
        # OBJECT directly — deref-ing it is a type error NIFC rejects.
        # A heap env (ref) needs the deref, and so does EnvIsParam with
        # a stack env: there typedEnv yields `(cast (ptr EnvT) ep)` — a
        # pointer either way (mirrors `treLocal`'s branch).
        dest.copyIntoKind DotX, info:
          if c.currentProc.env.needsHeap or c.currentProc.env.mode == EnvIsParam:
            dest.copyIntoKind DerefX, info:
              dest.typedEnv info, c.currentProc.env
          else:
            dest.typedEnv info, c.currentProc.env
          dest.addSymUse repWith.field, info
        inc n
      else:
        takeTree dest, n
  of DotToken, UnknownToken, EofToken, ParLe, ParRi, ExtendedSuffix, LineInfoLit, Ident, SymbolDef,
     IntLit, UIntLit, FloatLit, CharLit, StrLit:
    takeTree dest, n
  of TagLit:
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
    of TypeS:
      # Rewrite closure proctypes inside type-declaration BODIES to the
      # `(tuple <proctype> (ref RootObj))` shape: object fields
      # (`Handler.handler`), type aliases, and generic instances
      # (`seq[proc()]`'s payload). Upstream #2251 (issue #2244) lifts the body
      # via `treType`; we ADDITIONALLY (a) skip an already-lowered pass-1
      # itertype tuple, and (b) republish the rewritten type via
      # `programs.publish` so cross-module consumers see the lowered shape
      # (our `86774db7e`/`309a7d455` — upstream candidate: this republish is
      # the only remaining fork delta at this site).
      let typeStart = dest.len
      var typeSym = SymId(0)
      takeInto dest, n:       # TypeS tag
        if n.kind == SymbolDef:
          typeSym = n.symId
        takeTree dest, n        # name
        takeTree dest, n        # exported
        takeTree dest, n        # typevars
        takeTree dest, n        # pragmas
        if isLiftedClosureTuple(n):
          # already the stable lowered shape (pass 1 itertype rewrite)
          takeTree dest, n
        elif n.hasMore:
          treType c, dest, n    # body (upstream's empty-body guard)
        while n.hasMore: takeTree dest, n
      if typeSym != SymId(0):
        programs.publish(typeSym, dest, typeStart)
    of MacroS, TemplateS, EmitS, BreakS, ContinueS,
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
      LabS, JmpS, NoStmt:
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
        n.into:
          dest.copyIntoKind DotX, info:
            dest.copyIntoKind DerefX, info:
              dest.copyIntoKind CastX, info:
                dest.copyIntoKind (if c.currentProc.env.needsHeap: RefT else: PtrT), info:
                  dest.takeTree n # type
                dest.addSymUse c.currentProc.env.s, info
            assert n.kind == Symbol
            dest.takeTree n # the symbol
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
        if isLiftedClosureTuple(n):
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
  else:
    bug "unexpected ')' inside" # classic: a physical ParRi; nifcore: suffix kinds (never heads)

proc genObjectTypes(c: var Context; dest: var TokenBuf) =
  var objectTypes = initTable[SymId, seq[EnvField]]()
  for _, procCtx in c.procEnvs:
    for local, field in procCtx.localToEnv:
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
            if field.isCursor:
              # non-owning field: the lifter skips it in the env's =destroy/
              # =dup/=copy hooks, so the captured object keeps its original
              # owner and the closure->env->object cycle stays broken.
              dest.copyIntoKind PragmasU, NoLineInfo:
                dest.addParPair CursorP, NoLineInfo
            else:
              dest.addDotToken() # no pragmas
            var n = field.typ
            # treType, not tre: a captured lambda's type is decl-shaped
            # (`(proc ...)`), which `tre`'s stmtKind dispatch would treat
            # as a proc DECLARATION and lift — the field must get the
            # lowered closure-tuple type instead.
            treType(c, dest, n) # type might need an environment parameter
            dest.addDotToken() # no default value
          programs.publish(field.field, dest, beforeField)
    programs.publish(objType, dest, beforeType)

proc elimLambdas*(pass: var Pass) =
  var n = pass.n  # Extract cursor locally
  var c = Context(counter: 0, typeCache: createTypeCache(pass.bits), thisModuleSuffix: pass.moduleSuffix)
  c.coroCtx = coro_transform.Context(
    thisModuleSuffix: pass.moduleSuffix,
    typeCache: createTypeCache(pass.bits),   # placeholder; swapped with c.typeCache per call
    coroTypes: createTokenBuf(10),
    continuationProcImpl: coro_transform.generateContinuationProcImpl(),
    hooks: lambdaHooks(),
    nextTemp: pass.nextTemp,        # nested njvl runs continue the xelim counter
    ptrSize: pass.bits div 8
  )
  c.typeCache.openScope()
  tr c, pass.dest, n
  c.typeCache.closeScope()

  # second pass: generate environments and rewrite closure types/symbols.
  # Runs UNCONDITIONALLY: closure pressure can arrive from anywhere — a decl
  # body alias, a foreign closure value called through a field, a
  # closure-typed nil — and every detection scheme tried here missed one of
  # them. On a closure-free module the pass is a near-identity walk.
  if true:
    c.typeCache.openScope()
    let cap = pass.dest.len
    var oldDest = move pass.dest
    pass.dest = createTokenBuf(cap)
    var n2 = beginRead(oldDest)
    assert n2.stmtKind == StmtsS
    pass.dest.addParLe(n2.cursorTagId, n2.info)  # stmts opener
    n2.into:
      genObjectTypes(c, pass.dest)
      # Walk statements into a side buffer so we can prepend any
      # `.closure` iter coro frame types that `transformClosureIter`
      # accumulates during the walk. The state procs and wrappers
      # reference those types, so they must appear FIRST in the output
      # (alongside the env types).
      var stmtsBuf = createTokenBuf(cap)
      while n2.hasMore:
        tre(c, stmtsBuf, n2)
      # Publish the rewritten iter signatures NOW — the snapshots were
      # taken by `transformClosureIter` while the offsets were valid
      # (nested iters land in treProcLift's lift buffer, not stmtsBuf,
      # so buffer-relative offsets can't be replayed here). Publishing
      # earlier would change `tryLoadSym(iterSym)` mid-pass and
      # confuse the iter-sym-as-value check.
      for (sym, sig) in c.pendingIterSigs.mitems:
        publishSignature sig, sym, 0
      pass.dest.add c.coroCtx.coroTypes
      pass.dest.add stmtsBuf
      pass.dest.addParRi(n2.endInfo)
    pass.nextTemp = c.coroCtx.nextTemp
    c.typeCache.closeScope()

  #echo "PRODUCED ", toString(pass.dest, false)
