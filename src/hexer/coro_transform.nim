#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[
Shared coroutine state-machine transform.

This module contains the body-walking dispatcher, state-machine
generators, frame-type generator, wrapper-proc generator, and for-loop
trampoline for coroutine-shaped routines. Both flavours of coroutines
share it:

  - `.passive` procs / `.passive` iters (driven from `complete()`,
    factory-allocated frame, owned by the trampoline).
  - `.closure` iters (Nim-compatible resumable iter values; eager
    value-owned frame is planned for a follow-up).

The flavour-specific bits — recognising a `.passive` call, emitting the
call itself, lowering `delay`/`delay0`/`suspend`, the
proctype-to-wrapper-signature rewrite, the top-level coroutine
entrypoint — live behind a `Hooks` proc-field record on `Context`.
Consumers (cps.nim for `.passive`, eventually lambdalifting.nim for
`.closure` iters) install their own hooks before invoking `tr`.

Default hook implementations behave as "no `.passive` in scope". A
consumer can install only the hooks it actually needs; defaults answer
"no" / "pass through" for the rest.
]##

import std / [assertions, sets, tables, hashes, syncio]
when defined(nimony):
  {.feature: "lenientnils".}
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / symparser
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, expreval, xints, builtintypes, langmodes, renderer, reporters, typeprops]
import ".." / finalir / [finalir, finalir_model]
import passes, defaultvalues, constparams, duplifier, eraiser
include ".." / nimony / nif_annotations

## Note: `ContinuationName` lives in `builtintypes` (re-imported via the
## `nimony / [..., builtintypes, ...]` line above); we don't redefine it
## here.

const
  ContinuationProcName* = "ContinuationProc.0." & SystemModuleSuffix
  RootObjName* = "CoroutineBase.0." & SystemModuleSuffix
    ## Misleadingly named: this is `CoroutineBase`, used for
    ## `(ptr CoroutineBase)` throughout the coroutine internals. Kept
    ## for source compatibility — the iter-value env slot uses
    ## `BareRootObjName` (real RootObj) instead.
  BareRootObjName* = "RootObj.0." & SystemModuleSuffix
    ## The system's real `RootObj`. Used as the type of the iter-value
    ## tuple's env slot so iter values have the same `(ref RootObj)`
    ## shape as closure procs.
  EnvParamName* = "`this.0"
  ClosureEnvParamName* = "`ep.0"
    ## The env param appended to a lowered closure signature (distinct from the
    ## coroutine's `this.0` env above). Lives here — like `RootObjName` and the
    ## wrapper-signature shape — so lambdalifting's pass-2 lowering and any other
    ## pass that must emit the identical env slot (e.g. a cross-module foreign-decl
    ## canonicalizer) stay in lock-step off one definition.
  FnFieldName* = "fn.0"
  EnvFieldName* = "env.0"
  CallerFieldName* = "caller.0"
  CalleeFieldName* = "callee.0"
  ResultParamName* = "`result.0"
  ResultFieldName* = "`result.0"
  CallerParamName* = "`caller.0"
  AllocFrameProcName* = "allocFrame.0." & SystemModuleSuffix
  DeallocFrameProcName* = "deallocFrame.0." & SystemModuleSuffix

proc addClosureEnvParam*(dest: var TokenBuf; info: NifLineInfo; envTyp: SymId) =
  ## Emit the trailing env `(param)` of a lowered closure signature. `envTyp == 0`
  ## uses the generic `(ref RootObj)` slot shared with iter values; a concrete env
  ## type uses a `(ptr)` (NIFC needs the pointer type here, with a cast in the body).
  dest.copyIntoKind ParamU, info:
    dest.addSymDef pool.syms.getOrIncl(ClosureEnvParamName), info
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

proc addPragmasWithoutRaises*(dest: var TokenBuf; pragmas: Cursor; info: NifLineInfo) =
  ## Copy a coroutine's pragmas onto one of the procs generated FOR it, minus
  ## `(raises)`. See `ProcContext.canRaise`: the generated procs return a
  ## `Continuation` and never fail, and leaving the pragma on would have
  ## `lengcgen` rewrite that return type into `(ErrorCode, Continuation)` and
  ## `constparams` wrap every call to them in a success tuple.
  if pragmas.isDotToken or not pragmas.isTagLit:
    dest.addDotToken()
    return
  var n = pragmas
  dest.addParLe(n.cursorTagId, n.info)
  n = sub(n)
  while n.hasMore:
    if n.pragmaKind == RaisesP: skip n
    else: dest.takeTree n
  dest.addParRi()

type
  EnvField* = object
    objType*: SymId
    field*: SymId
    typeAsSym*: SymId
    pragmas*, typ*: Cursor
    def*: int
    use*: int

  RoutineKind* = enum
    IsNormal, IsIterator, IsPassive

  TryTarget* = object
    tracker*: SymId   ## the `ErrorCode` local this `try` accumulates into
    label*: SymId     ## the `(lab ...)` its handler dispatch starts at

  ProcContext* = object
    localToEnv*: Table[SymId, EnvField]
    constrFields*: HashSet[SymId]
      ## The frame fields the frame constructor already mentions, filled
      ## in by `patchParamList`. `completeFrameConstr` defaults every
      ## *other* field of the frame type against this set, which is what
      ## keeps the constructor total.
    cf*: TokenBuf
    resultSym*: SymId
    counter*: int
    labelCounter*: int = 1
    tryTargets*: seq[TryTarget]
      ## The enclosing `try` regions whose BODY we are inside, innermost last.
      ## A `raise` there is not a proc exit: it stores into that try's error
      ## tracker and jumps to its handler dispatch label.
    inFlight*: SymId
      ## Inside an `except`/`fin` body, the tracker holding the exception being
      ## handled — what a bare `(raise .)` re-raises.
    loopHeads*: seq[int]
      ## One entry per enclosing loop, innermost last: the state label of a
      ## SUSPENDING loop, or `KeptLoop` for one that stays a single
      ## `(loop ...)` construct. A Final IR `(continue .)` is the back-edge of
      ## the INNERMOST loop only, so it lowers against the top of this stack:
      ## a `jmp` to that state label, or — for a kept loop — the marker copied
      ## through untouched, because `coroTr` is what translates that one. A
      ## kept loop has to push too: without an entry of its own its back-edge
      ## would lower against the enclosing suspending loop and turn the inner
      ## loop into a single iteration per outer round.
    canRaise*: bool
      ## The coroutine was written `.raises`. Its error does NOT travel in the
      ## state procs' return type — that slot belongs to the `Continuation` —
      ## it travels in the frame's result, whose type becomes the success
      ## tuple. So the generated procs are not raising procs at all, and must
      ## not carry the pragma into `lengcgen`, which would wrap their
      ## `Continuation` in a tuple of its own.
    resultIsTuple*: bool
      ## `canRaise` and the routine returns a value, so the result slot is
      ## `(ErrorCode, T)` and the code lives at index 0. A `void` raising
      ## coroutine's slot is a bare `ErrorCode` and is written whole.
    tupleVars*: HashSet[SymId]
      ## The locals holding a raising call's result — `eraiser`'s `canRaise`
      ## temps, and any local a raising call was assigned to. They are declared
      ## with the type the routine was WRITTEN with, and only become
      ## `(ErrorCode, T)` in `lengcgen`; a frame field cannot wait that long.
      ## See `frameFieldType`.
    synthTypes*: seq[TokenBuf]
      ## Types this transform had to build rather than borrow from the body.
      ## One buffer each, kept alive for as long as the `ProcContext` is: an
      ## `EnvField.typ` is a Cursor, so the tokens behind it have to outlive
      ## the walk that produced them.
    kind*: RoutineKind
    isClosureIter*: bool
      ## True for `.closure` iters specifically. Drives the resume-slot
      ## writeback at yield sites and the two-branch wrapper body.
      ## `.passive` iters use the factory model and don't set this.
    capturedEnvField*: SymId
      ## Set for a `.closure` iter whose body captures locals of the
      ## ENCLOSING proc: the coro frame grows one extra field holding the
      ## erased `(ref RootObj)` pointer to that proc's lambdalifting
      ## environment. Whoever creates the frame (lambdalifting, at the
      ## point where the iter VALUE is built) fills it in; every
      ## `(envp EnvType field)` in the body reads the capture back
      ## through it. `SymId(0)` = this iter captures nothing.

  TrHook* = proc (c: var Context; dest: var TokenBuf; n: var Cursor) {.nimcall.}
  TrPassiveCallHook* = proc (c: var Context; dest: var TokenBuf; n: var Cursor; target: Cursor) {.nimcall.}
  SymPredHook* = proc (c: var Context; s: SymId): bool {.nimcall.}
  CursorPredHook* = proc (c: var Context; n: Cursor): bool {.nimcall.}

  Hooks* = object
    ## Flavour-specific call-backs. Installed by the consumer before
    ## any shared transform proc runs.
    isPassiveProc*: SymPredHook
                               ## True if `s` denotes a routine with
                               ## `{.passive.}` (used by `tr`'s
                               ## Symbol path to rewrite the sym to
                               ## its `init.` wrapper).
    isPassiveCall*: CursorPredHook
                               ## True if `n` is a call whose target
                               ## has `{.passive.}` (i.e. a
                               ## suspension point).
    trPassiveCall*: TrPassiveCallHook
                               ## Emit a `.passive` call.  `target`
                               ## is the lvalue receiving the call's
                               ## result, or `default(Cursor)` for a
                               ## void call.
    trDelay*: TrHook           ## handles `(delay …)`
    trDelay0*: TrHook          ## handles `(delay0)`
    trSuspend*: TrHook         ## handles `(suspend)`
    trProctype*: TrHook        ## handles ProctypeT / ItertypeT bodies
    trCoroutine*: TrHook       ## handles ProcS/FuncS/MethodS/
                               ## ConverterS/IteratorS — decides
                               ## whether the routine is a coroutine
                               ## and emits the state machine if so.

  Context* = object
    counter*: int
    ptrSize*: int
      ## Target pointer size in bytes. Needed to give `int`/`uint`/`float`
      ## a concrete width when a default value for one is synthesized.
    nextTemp*: int
      ## Continues the outer pipeline's xelim temp counter through the
      ## nested per-coroutine Final-IR runs (treIteratorBody) — restarting at
      ## 0 re-mints `x.N SymIds that collide with still-live outer temps.
    typeCache*: TypeCache
    sizeofCache*: SizeofCache
      ## Memoizes the type sizes `nextArgRole` asks about while `trGoto`
      ## decides which actuals have to arrive as an address.
    thisModuleSuffix*: string
    procStack*: seq[SymId]
    currentProc*: ProcContext
    continuationProcImpl*: Cursor
    shouldPublish*: seq[tuple[sym: SymId, start: int]]
    coroTypes*: TokenBuf
    hooks*: Hooks
    awaitingSuspendPark*: bool
      ## Set by `(delay0)`; consumed by the following `(suspend)` to
      ## decide between real parking and a synchronous state transition.
    pendingCapturedEnvField*: SymId
      ## Inbox for the NEXT `transformCoroutineDecl` call: the consumer
      ## (lambdalifting) knows whether the iter it is about to hand us
      ## captures, we don't. Moved into `currentProc` on entry and
      ## cleared, so a following non-capturing iter can't inherit it.

proc generateContinuationProcImpl*(): Cursor =
  ## Load the `ContinuationProc` typedef body from system, returned as
  ## a Cursor pointing at the proctype literal. Cps and lambdalifting
  ## both feed this into `Context.continuationProcImpl`; the value is
  ## used by `contNextState` / `stashResumeFn` / wrapper emission as
  ## the cast target for state-proc symbols.
  let symId = pool.syms.getOrIncl(ContinuationProcName)
  let impl = programs.tryLoadSym(symId)
  if impl.status == LacksNothing:
    let t = asTypeDecl(impl.decl)
    if t.kind == TypeY:
      return t.body
  return default(Cursor)

proc coroTr*(c: var Context; dest: var TokenBuf; n: var Cursor)
  {.ensuresNif: addedAny(dest).}
proc coroTrSons*(c: var Context; dest: var TokenBuf; n: var Cursor)
  ## `coroTr` / `coroTrSons` (rather than the unqualified `tr` / `trSons`)
  ## so they don't overload-collide with the `tr` / `trSons` that
  ## `lambdalifting` and other consumers naturally name their local
  ## body walkers.

# ---------------------------------------------------------------------
# Naming helpers
# ---------------------------------------------------------------------

proc coroHelperName*(routineSym: SymId; tag, fallbackSuffix: string): SymId =
  ## Mint the name of a coroutine helper (`coro` frame type, `init` wrapper,
  ## `s<state>` state proc) derived from `routineSym`.
  ##
  ## The suffix is the DEFINING module's, never the transforming module's:
  ## the helpers are generated once, by the module that declares the routine,
  ## so a caller in another module has to arrive at the same name for the two
  ## to link up. `fallbackSuffix` covers a bare symbol (no module segment),
  ## which is what a symbol this pass minted itself looks like.
  ##
  ## `splitSymName(...).name` — not `extractVersionedBasename` — because it
  ## preserves an intermediate `I<hash>` segment: two instantiations of one
  ## generic (`gen.12.Iaaaa.mod`, `gen.12.Ibbbb.mod`) would otherwise share
  ## the stem `gen.12` and collide on every helper name.
  let split = splitSymName(pool.syms[routineSym])
  let module = if split.module.len > 0: split.module else: fallbackSuffix
  result = pool.syms.getOrIncl(derivedName(split.name, tag) & "." & module)

proc coroTypeForProc*(c: Context; procId: SymId): SymId =
  coroHelperName(procId, "coro", c.thisModuleSuffix)

proc coroWrapperProc*(c: Context; procId: SymId): SymId =
  coroHelperName(procId, "init", c.thisModuleSuffix)

proc stateToProcName*(c: Context; sym: SymId; state: int): SymId =
  coroHelperName(sym, "s" & $state, c.thisModuleSuffix)

proc localToFieldname*(c: var Context; local: SymId): SymId =
  var name = pool.syms[local]
  extractBasename name
  name.add "`f."
  name.add $c.counter
  inc c.counter
  name.add "."
  name.add c.thisModuleSuffix
  result = pool.syms.getOrIncl(name)

proc coroWrapperForExternIter*(iterSym: SymId): SymId =
  ## Context-free spelling of `coroWrapperProc` for lambdalifting, which
  ## holds its own `Context` type and so cannot pass ours. Same name, same
  ## rule — the iterator's own module suffix.
  coroHelperName(iterSym, "init", "")

proc coroTypeForExternIter*(iterSym: SymId): SymId =
  ## Context-free spelling of `coroTypeForProc`; see above.
  coroHelperName(iterSym, "coro", "")

proc coroEnvFieldForIter*(iterSym: SymId): SymId =
  ## The frame field holding a capturing `.closure` iter's env pointer.
  ## Derived from the iter sym exactly like the frame type and the
  ## wrapper, so the frame-CREATION site (lambdalifting) and the
  ## frame-READING sites (the state machine, generated here) arrive at
  ## the same name without having to agree on an order.
  coroHelperName(iterSym, "cenv", "")

proc publishWrapperSignature*(routineSym: SymId; moduleSuffix: string) =
  ## Publish a placeholder signature for a coroutine's `init` wrapper so
  ## downstream passes (eraiser / duplifier / destroyer / constparams) can
  ## resolve its type via `tryLoadSym` even though no wrapper DECL exists in
  ## this process. Hexer-generated symbols never enter a module's sem index,
  ## which is the one thing both callers are up against:
  ##
  ##  * lambdalifting expands a same-module `.closure` iter corofor into a
  ##    trampoline that names the wrapper before cps has emitted it;
  ##  * cps compiles a call to a FOREIGN `.passive` proc, whose wrapper the
  ##    DEFINING module's hexer run emits — there is no later point in *this*
  ##    run at which it becomes loadable.
  ##
  ## `tryLoadSym` on the wrapper is the whole guard. A module-suffix test
  ## would be wrong in both directions: the foreign case is exactly the one
  ## that needs publishing, and the same-module case is already covered by
  ## the wrapper resolving once cps has emitted it.
  ##
  ## The shape mirrors what `generateCoroutineHelpers` emits: original params,
  ## then `(param result (ptr T))` if non-void, then
  ## `(param caller Continuation)`, return type `Continuation`, and the
  ## routine's OWN pragmas — copied, not synthesized, because a foreign
  ## wrapper's placeholder is never replaced by the real signature in this
  ## process and `constparams.trCall` reads `raises` off it to decide whether
  ## the call returns an `(ErrorCode, T)` tuple. Body is empty (`.`); for a
  ## same-module routine cps's `publishSignature` overwrites this entry.
  let wrapperSym = coroHelperName(routineSym, "init", moduleSuffix)
  if tryLoadSym(wrapperSym).status == LacksNothing:
    return  # already published: an earlier call, or cps's own emission

  let res = tryLoadSym(routineSym)
  if res.status != LacksNothing:
    return  # signature unrecoverable; let the downstream lookup fail loudly
  let fn = asRoutine(res.decl)
  let info = NoLineInfo

  var buf = createTokenBuf(40)
  buf.addParLe ProcS, info
  buf.addSymDef wrapperSym, info
  buf.addDotToken() # exported
  buf.addDotToken() # pattern
  buf.addDotToken() # typevars
  buf.copyIntoKind ParamsU, info:
    var p = fn.params
    if p.kind != DotToken:
      p = sub(p) # peek walk, never left
      while p.hasMore:
        assert p.substructureKind == ParamU
        takeInto buf, p:
          buf.takeTree p # name
          buf.takeTree p # exported
          buf.takeTree p # pragmas
          buf.takeTree p # type
          buf.takeTree p # default value
    let raises = hasPragma(fn.pragmas, RaisesP)
    var ret = fn.retType
    if raises or not isVoidType(ret):
      buf.copyIntoKind ParamU, info:
        buf.addSymDef pool.syms.getOrIncl(ResultParamName), info
        buf.addDotToken() # export
        buf.addDotToken() # pragmas
        buf.copyIntoKind PtrT, info:
          if raises: addSuccessTupleType(buf, ret, info)
          else: buf.takeTree ret
        buf.addDotToken() # default value
    buf.copyIntoKind ParamU, info:
      buf.addSymDef pool.syms.getOrIncl(CallerParamName), info
      buf.addDotToken() # export
      buf.addDotToken() # pragmas
      buf.addSymUse pool.syms.getOrIncl(ContinuationName), info
      buf.addDotToken() # default value
  buf.addSymUse pool.syms.getOrIncl(ContinuationName), info
  addPragmasWithoutRaises(buf, fn.pragmas, info)
  buf.addDotToken() # effects
  buf.addDotToken() # body — empty, cps replaces with the real body
  buf.addParRi() # close proc
  programs.publish(wrapperSym, buf, SemcheckSignatures)

# ---------------------------------------------------------------------
# Iter-value tuple-type emitters
#
# `.closure` iter values are lowered to `(tuple <wrapper-proctype>
# (ref RootObj))` — structurally identical to closure procs, so the
# lifter handles destroy/copy/sink hooks for iter values uniformly.
#
# Two entry points: one consumes an `(itertype …)` cursor in-place (used
# in type slots), the other re-builds the shape from an iterator sym's
# decl (used at iter-sym-as-value sites and iter-nil tupconstrs).
# Lambdalifting and cps both call these to keep the wrapper-signature
# shape in lock-step with `generateCoroutineHelpers`.
# ---------------------------------------------------------------------

proc emitIterTupleTypeFromParams*(dest: var TokenBuf; n: var Cursor; info: NifLineInfo) =
  ## Consume an (itertype ...) tree at `n` and emit
  ##   `(closureTuple (proctype . (params <orig>... (param result ptr T) (param caller Continuation)) Continuation <pragmas>) (ref RootObj))`
  ## Cursor is left past the closing ParRi of the input itertype.
  ##
  ## NOTE: parameter types are copied verbatim (`takeTree`) on the
  ## assumption that iter param types are scalar. If we ever support
  ## nested itertypes in param positions we'll need to recurse via a
  ## proctype-walker here.
  assert n.typeKind == ItertypeT
  n.into: # past itertype tag
    if n.hasMore:
      skip n               # past nilability tag
    dest.copyIntoKind ClosureTupleT, info:
      dest.copyIntoKind ProctypeT, info:
        dest.addDotToken() # nilability tag
        dest.copyIntoKind ParamsU, info:
          if n.substructureKind == ParamsU:
            n.into:
              while n.hasMore:
                assert n.substructureKind == ParamU
                takeInto dest, n:     # param tag
                  dest.takeTree n       # name
                  dest.takeTree n       # exported
                  dest.takeTree n       # pragmas
                  dest.takeTree n       # type (assumed scalar)
                  dest.takeTree n       # default value
          elif n.kind == DotToken:
            inc n
          # result becomes a ptr parameter (skipped when return type is void):
          let isVoid = isVoidType(n)
          if not isVoid:
            dest.copyIntoKind ParamU, info:
              dest.addSymDef pool.syms.getOrIncl(ResultParamName), info
              dest.addDotToken() # export
              dest.addDotToken() # pragmas
              dest.copyIntoKind PtrT, info:
                dest.takeTree n
              dest.addDotToken() # default value
          else:
            skip n
          # caller parameter is always last:
          dest.copyIntoKind ParamU, info:
            dest.addSymDef pool.syms.getOrIncl(CallerParamName), info
            dest.addDotToken() # export
            dest.addDotToken() # pragmas
            dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
            dest.addDotToken() # default value
        dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
        # Pragmas: ALWAYS emit `(pragmas (closure))` regardless of whether
        # the source itertype was `.closure` or `.passive`. Two reasons:
        #  (a) cps's `trProctype` re-walks types and treats ProctypeT with
        #      `(pragmas (passive))` as an unlifted passive proctype — it
        #      would wrap our already-lifted proctype in another result-ptr
        #      + caller-Continuation param pair, corrupting the type sym.
        #  (b) `(closure)` is the canonical "this is a closure-shaped fn
        #      pointer" marker used by `isClosure`, cps's `HconvX` path,
        #      etc. Both `.closure` and `.passive` iter values share the
        #      SAME tuple ABI, so they share the same lifted-tuple shape.
        # Skip the source pragmas; emit normalized closure marker.
        if n.hasMore: skip n
        dest.copyIntoKind PragmasU, info:
          dest.copyIntoKind ClosureP, info: discard
        # drop anything else (effects/body slots)
        while n.hasMore: skip n
      dest.copyIntoKind RefT, info:
        dest.addSymUse pool.syms.getOrIncl(BareRootObjName), info

proc emitIterTupleTypeFromSym*(dest: var TokenBuf; iterSym: SymId; info: NifLineInfo) =
  ## Build the iter-value tuple type from an iterator sym's decl. Used
  ## at iter-sym-as-value and iter-nil sites where we don't have an
  ## itertype tree on hand.
  let res = tryLoadSym(iterSym)
  assert res.status == LacksNothing, "iter sym not loaded: " & pool.syms[iterSym]
  let fn = asRoutine(res.decl)
  dest.copyIntoKind ClosureTupleT, info:
    dest.copyIntoKind ProctypeT, info:
      dest.addDotToken() # nilability tag
      dest.copyIntoKind ParamsU, info:
        var p = fn.params
        if p.kind != DotToken:
          p = sub(p) # peek walk, never left
          while p.hasMore:
            assert p.substructureKind == ParamU
            takeInto dest, p:
              dest.takeTree p # name
              dest.takeTree p # exported
              dest.takeTree p # pragmas
              dest.takeTree p # type
              dest.takeTree p # default value
        var ret = fn.retType
        if not isVoidType(ret):
          dest.copyIntoKind ParamU, info:
            dest.addSymDef pool.syms.getOrIncl(ResultParamName), info
            dest.addDotToken() # export
            dest.addDotToken() # pragmas
            dest.copyIntoKind PtrT, info:
              dest.takeTree ret
            dest.addDotToken() # default value
        dest.copyIntoKind ParamU, info:
          dest.addSymDef pool.syms.getOrIncl(CallerParamName), info
          dest.addDotToken() # export
          dest.addDotToken() # pragmas
          dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
          dest.addDotToken() # default value
      dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
      # See emitIterTupleTypeFromParams for why we always emit
      # `(pragmas (closure))` regardless of the source pragma.
      dest.copyIntoKind PragmasU, info:
        dest.copyIntoKind ClosureP, info: discard
    dest.copyIntoKind RefT, info:
      dest.addSymUse pool.syms.getOrIncl(BareRootObjName), info

proc isClosureIterSym*(s: SymId): bool =
  ## True for `.closure` iter decls only — those are the ones that lower
  ## to the iter-value tuple (Nim-compatible ref-based env). `.passive`
  ## iters stay as plain function pointers via cps's `trProctype`, so
  ## they don't go through the tupconstr emission in lambdalifting's
  ## tre Symbol path.
  let res = tryLoadSym(s)
  if res.status == LacksNothing and res.decl.symKind == IteratorY:
    let routine = asRoutine(res.decl)
    return hasPragma(routine.pragmas, ClosureP)
  return false

proc isLiftedClosureTuple*(n: Cursor): bool {.inline.} =
  ## `(closureTuple <proctype …> (ref RootObj))` is the shape both closure
  ## procs and closure-iter values get lifted to. If we encounter one
  ## while walking, it's already lifted — recursing into it would
  ## re-trigger the proctype rewrite and produce nested tuples.
  ##
  ## The tag alone answers this, because every producer — `emitIterTupleType*`
  ## here, lambdalifting's `treProcType` / `nonClosureToClosure` / closure-sym
  ## path, and cps's `trProctype` — emits `ClosureTupleT`. This used to probe a
  ## plain `(tuple …)` for "exactly a proctype then a ref", which answered a
  ## question about *layout* where the callers all ask about *provenance*: an
  ## ordinary `(proc (), ref RootObj)` tuple written by the user answered yes,
  ## and any drift in the lifted element shape would silently answer no.
  n.typeKind == ClosureTupleT

# ---------------------------------------------------------------------
# Predicates
# ---------------------------------------------------------------------

proc isProc*(c: var Context; s: SymId): bool =
  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    result = res.decl.symKind == ProcY
  else:
    let info = getLocalInfo(c.typeCache, s)
    result = info.kind == ProcY

proc isClosureIter*(s: SymId): bool =
  ## True for any coroutine-shaped iter decl — `.closure` or `.passive`.
  ## `tr`'s Symbol path uses this to rewrite the iter sym (as it
  ## appears in value positions) to its wrapper sym.
  let res = tryLoadSym(s)
  if res.status == LacksNothing and res.decl.symKind == IteratorY:
    let routine = asRoutine(res.decl)
    return hasPragma(routine.pragmas, ClosureP) or
           hasPragma(routine.pragmas, PassiveP)
  return false

proc getNextState*(buf: TokenBuf; n: Cursor): int =
  var pos = cursorToPosition(buf, n)
  while pos < buf.len:
    # raw linear scan: only TagLit tokens carry a tagId (suffix/literal bits
    # alias it), and the head may carry a line-info suffix before its child
    if buf[pos].kind == TagLit and buf[pos].tagId == TagId(LabS):
      let operand = readonlyCursorAt(buf, pos + tokenWidth(readonlyCursorAt(buf, pos)))
      # Skip `xelim`'s structured merge labels: only the CPS state machine's
      # own integer-labelled `lab` names a state (`doc/final_ir.md`).
      if operand.kind == IntLit:
        return int(operand.intVal)
    inc pos
  return -1

proc coroTrSons*(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    while n.hasMore:
      coroTr(c, dest, n)

# ---------------------------------------------------------------------
# IR emitters — operate on (ptr CoroutineBase) frames via `this.0`
# ---------------------------------------------------------------------

proc contNextState*(c: var Context; dest: var TokenBuf; state: int; info: NifLineInfo) =
  assert state >= 0
  if cursorIsNil(c.continuationProcImpl):
    bug "could not load system.ContinuationProc"
  dest.copyIntoKind OconstrX, info:
    dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
    dest.copyIntoKind KvU, info:
      dest.addSymUse pool.syms.getOrIncl(FnFieldName), info
      dest.copyIntoKind CastX, info:
        dest.copyTree c.continuationProcImpl
        dest.addSymUse stateToProcName(c, c.procStack[^1], state), info
    dest.copyIntoKind KvU, info:
      dest.addSymUse pool.syms.getOrIncl(EnvFieldName), info
      dest.copyIntoKind CastX, info:
        dest.copyIntoKind PtrT, info:
          dest.addSymUse pool.syms.getOrIncl(RootObjName), info
        dest.addSymUse pool.syms.getOrIncl(EnvParamName), info

proc stashResumeFn*(c: var Context; dest: var TokenBuf; state: int; info: NifLineInfo) =
  ## For `.closure` iters: emit
  ##   `this.caller.fn = cast[ContinuationProc](next_state)`
  ## so the wrapper, on a subsequent iter-value call, reads this slot
  ## to find where to resume. `caller.env` doubles as the ownership
  ## marker:
  ##   - nil   → wrapper-allocated, frame deallocated at final state.
  ##   - !nil  → iter-value-owned, final state just returns (nil, nil);
  ##     the ref destructor handles dealloc via finalizeCoroutine.
  if not c.currentProc.isClosureIter: return
  if cursorIsNil(c.continuationProcImpl):
    bug "could not load system.ContinuationProc"
  dest.copyIntoKind AsgnS, info:
    dest.copyIntoKind DotX, info:
      dest.copyIntoKind DotX, info:
        dest.copyIntoKind DerefX, info:
          dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
        dest.addSymUse pool.syms.getOrIncl(CallerFieldName), info
        dest.addIntLit 1, info # CallerFieldName lives on the CoroutineBase super
      dest.addSymUse pool.syms.getOrIncl(FnFieldName), info
      dest.addIntLit 0, info # FnFieldName is a direct field of Continuation
    if state < 0:
      dest.addParPair NilX, info
    else:
      dest.copyIntoKind CastX, info:
        dest.copyTree c.continuationProcImpl
        dest.addSymUse stateToProcName(c, c.procStack[^1], state), info

proc emitAllocFrame*(c: var Context; dest: var TokenBuf; calleeSym: SymId; info: NifLineInfo) =
  ## Emit: cast[ptr CalleeCoroutine](allocFrame(sizeof(CalleeCoroutine)))
  dest.copyIntoKind CastX, info:
    dest.copyIntoKind PtrT, info:
      dest.addSymUse coroTypeForProc(c, calleeSym), info
    dest.copyIntoKind CallX, info:
      dest.addSymUse pool.syms.getOrIncl(AllocFrameProcName), info
      dest.copyIntoKind SizeofX, info:
        dest.addSymUse coroTypeForProc(c, calleeSym), info

proc emitDeallocFrame*(c: var Context; dest: var TokenBuf; info: NifLineInfo) =
  ## Emit: deallocFrame(cast[ptr CoroutineBase](this))
  dest.copyIntoKind CallS, info:
    dest.addSymUse pool.syms.getOrIncl(DeallocFrameProcName), info
    dest.copyIntoKind CastX, info:
      dest.copyIntoKind PtrT, info:
        dest.addSymUse pool.syms.getOrIncl(RootObjName), info
      dest.addSymUse pool.syms.getOrIncl(EnvParamName), info

proc emitStopContinuation*(dest: var TokenBuf; info: NifLineInfo) =
  ## Emit `Continuation(fn: nil, env: nil)` — the sentinel "no caller"
  ## continuation passed to closure-iterator init wrappers.
  dest.copyIntoKind OconstrX, info:
    dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
    dest.copyIntoKind KvU, info:
      dest.addSymUse pool.syms.getOrIncl(FnFieldName), info
      dest.addParPair NilX, info
    dest.copyIntoKind KvU, info:
      dest.addSymUse pool.syms.getOrIncl(EnvFieldName), info
      dest.addParPair NilX, info

proc emitFinalReturn*(c: var Context; dest: var TokenBuf; info: NifLineInfo) =
  ## Emit the terminating return for a coroutine state machine.
  ##
  ## `.passive` procs / `.passive` iters: save `this.caller`,
  ## deallocFrame, return the saved caller — control flows back to the
  ## passive caller's continuation.
  ##
  ## `.closure` iters: `caller.fn` is the *resume slot* (overwritten at
  ## every yield), so we cannot read it back for the final return — it
  ## points at the last-yielded state proc, not at Stop. Instead emit a
  ## literal `Continuation(fn: nil, env: nil)` directly. `caller.env`
  ## is the ownership marker:
  ##   - `caller.env == nil`  → wrapper-allocated frame; deallocFrame
  ##     here.
  ##   - `caller.env != nil`  → iter-value-owned; the ref destructor
  ##     deallocs via `finalizeCoroutine`, so we just return Stop
  ##     without freeing.
  if c.currentProc.isClosureIter:
    let envSym = pool.syms.getOrIncl(EnvParamName)
    let callerFld = pool.syms.getOrIncl(CallerFieldName)
    let envFld = pool.syms.getOrIncl(EnvFieldName)
    # if (*this).caller.env == nil: deallocFrame
    dest.copyIntoKind IfS, info:
      dest.copyIntoKind ElifU, info:
        dest.copyIntoKind EqX, info:
          dest.addParPair PointerT, info
          dest.copyIntoKind DotX, info:
            dest.copyIntoKind DotX, info:
              dest.copyIntoKind DerefX, info:
                dest.addSymUse envSym, info
              dest.addSymUse callerFld, info
              dest.addIntLit 1, info # CallerFieldName lives on the super
            dest.addSymUse envFld, info
            dest.addIntLit 0, info # EnvFieldName is direct field of Continuation
          dest.addParPair NilX, info
        dest.copyIntoKind StmtsS, info:
          emitDeallocFrame(c, dest, info)
    dest.copyIntoKind RetS, info:
      emitStopContinuation(dest, info)
    return
  let tmpVar = pool.syms.getOrIncl("`tmpCaller." & $c.currentProc.counter)
  inc c.currentProc.counter
  dest.copyIntoKind VarS, info:
    dest.addSymDef tmpVar, info
    dest.addDotToken() # exported
    dest.addDotToken() # pragmas
    dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
    dest.copyIntoKind DotX, info:
      dest.copyIntoKind DerefX, info:
        dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
      dest.addSymUse pool.syms.getOrIncl(CallerFieldName), info
      dest.addIntLit 1, info # field is in superclass
  emitDeallocFrame(c, dest, info)
  dest.copyIntoKind RetS, info:
    dest.addSymUse tmpVar, info

proc emitStackFrameTag*(c: var Context; dest: var TokenBuf; coroVar: SymId; info: NifLineInfo) =
  ## Emit: coroVar.callee = nil
  ## Marks the frame as stack-allocated so deallocFrame is a nop.
  dest.copyIntoKind AsgnS, info:
    dest.copyIntoKind DotX, info:
      dest.addSymUse coroVar, info
      dest.addSymUse pool.syms.getOrIncl(CalleeFieldName), info
      dest.addIntLit 1, info # field is in superclass
    dest.addParPair NilX, info

proc emitItEnv(dest: var TokenBuf; info: NifLineInfo;
               itSym, envFieldSym: SymId) =
  dest.copyIntoKind DotX, info:
    dest.addSymUse itSym, info
    dest.addSymUse envFieldSym, info
    dest.addIntLit 0, info # direct field of Continuation

proc emitWhileBegin*(dest: var TokenBuf; info: NifLineInfo;
                     itSym, myEnvSym: SymId) =
  ## Open half of the corofor trampoline (shared by cps's `.passive`
  ## and lambdalifting's `.closure` expansions). Emits:
  ##
  ##   let myEnv = it.env
  ##   try:
  ##     while true:
  ##       it = advance(it)
  ##       if stopping(it): break
  ##       if it.env == myEnv:
  ##         <body-stmts goes here — emit between begin and end>
  ##
  ## The caller follows with body emission, then `emitWhileEnd`.
  let envFieldSym = pool.syms.getOrIncl(EnvFieldName)
  let advanceSym = pool.syms.getOrIncl("advance.0." & SystemModuleSuffix)
  let stoppingSym = pool.syms.getOrIncl("stopping.0." & SystemModuleSuffix)

  dest.copyIntoKind LetS, info:
    dest.addSymDef myEnvSym, info
    dest.addDotToken() # exported
    dest.addDotToken() # pragmas
    dest.copyIntoKind PtrT, info:
      dest.addSymUse pool.syms.getOrIncl(RootObjName), info
    emitItEnv(dest, info, itSym, envFieldSym)

  dest.addParLe TryS, info
  dest.addParLe StmtsS, info     # outer try-body stmts
  dest.addParLe WhileS, info
  dest.addParPair TrueX, info
  dest.addParLe StmtsS, info     # while-body stmts
  dest.copyIntoKind AsgnS, info:
    dest.addSymUse itSym, info
    dest.copyIntoKind CallS, info:
      dest.addSymUse advanceSym, info
      dest.addSymUse itSym, info
  dest.copyIntoKind IfS, info:
    dest.copyIntoKind ElifU, info:
      dest.copyIntoKind CallS, info:
        dest.addSymUse stoppingSym, info
        dest.addSymUse itSym, info
      dest.copyIntoKind StmtsS, info:
        dest.copyIntoKind BreakS, info:
          dest.addDotToken()
  dest.addParLe IfS, info
  dest.addParLe ElifU, info
  dest.copyIntoKind EqX, info:
    dest.addParPair PointerT, info
    emitItEnv(dest, info, itSym, envFieldSym)
    dest.addSymUse myEnvSym, info
  dest.addParLe StmtsS, info     # body-stmts open

proc emitWhileEnd*(dest: var TokenBuf; info: NifLineInfo; itSym: SymId) =
  ## Close half of the corofor trampoline. Balances `emitWhileBegin`'s
  ## opens and emits `finally: finalizeCoroutine(addr it)`.
  let finalizeSym = pool.syms.getOrIncl("finalizeCoroutine.0." & SystemModuleSuffix)
  dest.addParRi()  # close body StmtsS
  dest.addParRi()  # close ElifU
  dest.addParRi()  # close IfS
  dest.addParRi()  # close while-body StmtsS
  dest.addParRi()  # close WhileS
  dest.addParRi()  # close outer try-body StmtsS
  dest.copyIntoKind FinU, info:
    dest.copyIntoKind StmtsS, info:
      dest.copyIntoKind CallS, info:
        dest.addSymUse finalizeSym, info
        dest.copyIntoKind HaddrX, info:
          dest.addSymUse itSym, info
  dest.addParRi()  # close try

# ---------------------------------------------------------------------
# trCoroFor — expand a `(corofor ...)` into the trampoline
# ---------------------------------------------------------------------

proc trCoroFor*(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Expand `(corofor (call iter args... (haddr forLoopVar)) (block ...))`
  ## into the trampoline:
  ##
  ##   var it: Continuation = `iter.init.<suffix>`(args..., addr forLoopVar,
  ##                                                StopContinuation)
  ##   try:
  ##     while true:
  ##       it = advance(it)
  ##       if finished(it): break
  ##       <body>
  ##   finally:
  ##     finalizeCoroutine(addr it)
  let info = n.info
  n.into: # skip (corofor

    # ---- first child: (call iter-or-tupat args... (haddr forLoopVar)) ----
    assert n.exprKind in CallKinds, "corofor: expected iter call as first child"
    let callStart = n # past CallS tag
    n = sub(n)
    # The branch we take here is the ONLY reliable signal for whether
    # the arg list has an upstream env-arg (case 3, non-Symbol target).
    # Probing the last arg for TupatX is unsound: a regular `(tupat
    # someTuple 0)` arg would falsely match.
    var targetBuf = createTokenBuf(4)
    var upstreamEnvArg = false
    if n.kind == Symbol and isClosureIter(n.symId):
      # Direct `.passive` (or `.closure`) iter DECL call — route through the
      # iter's init wrapper.
      targetBuf.addSymUse coroWrapperProc(c, n.symId), n.info
      inc n
    elif n.kind == Symbol:
      # Iter-VALUE local: a `.passive` iter value is a bare function pointer
      # to the wrapper (no env tuple — see cps's `trProctype`), so call it
      # directly. The wrapper always allocates a fresh frame, so no caller
      # env is needed; `emitStopContinuation` below supplies the sentinel.
      targetBuf.addSymUse n.symId, n.info
      inc n
    else:
      upstreamEnvArg = true
      targetBuf.takeTree n

    # Cursors are stable — walk once to count args and remember the
    # cursor at the last (haddr) position; emit later via `addSubtree`.
    let argsStart = n
    var lastArgPos = default(Cursor)
    var argCount = 0
    while n.hasMore:
      lastArgPos = n
      skip n
      inc argCount
    n = callStart; skip n # close iter call

    # Structural invariant from the corofor producer: trailing arg is
    # `(haddr forLoopVar)`, optionally preceded by an env-arg when the
    # target was pre-extracted. Don't probe `HaddrX` — a regular iter
    # arg of `addr` shape would falsely match.
    let trailingCount = if upstreamEnvArg: 2 else: 1
    assert argCount >= trailingCount, "corofor: iter call missing args"
    let realArgCount = argCount - trailingCount

    let itSym = pool.syms.getOrIncl("`coroIt." & $c.currentProc.counter)
    inc c.currentProc.counter
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
        emitStopContinuation(dest, info)

    let myEnvSym = pool.syms.getOrIncl("`coroEnv." & $c.currentProc.counter)
    inc c.currentProc.counter
    c.typeCache.registerLocal(myEnvSym, LetY, default(Cursor))

    emitWhileBegin(dest, info, itSym, myEnvSym)
    while n.hasMore:
      coroTr(c, dest, n)
    emitWhileEnd(dest, info, itSym)



# ---------------------------------------------------------------------
# Shared call / asgn / local dispatchers — route to .passive hooks
# when the call/asgn rhs is a passive call.
# ---------------------------------------------------------------------

proc trCall*(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let fn = n.childCursor
  let typ = c.typeCache.getType(fn, {SkipAliases})
  if procHasPragma(typ, PassiveP):
    var retType = getType(c.typeCache, n)
    let hasResult = not isVoidType(retType)
    if hasResult:
      let info = n.info
      dest.copyIntoKind ExprX, info:
        let tmpVar = pool.syms.getOrIncl("`tmpCpsResult." & $c.currentProc.counter)
        inc c.currentProc.counter
        var target = createTokenBuf(1)
        target.addSymUse tmpVar, info
        dest.copyIntoKind VarS, info:
          dest.addSymDef tmpVar, info
          dest.addDotToken() # exported
          dest.addDotToken() # pragmas
          coroTr c, dest, retType # type
          dest.addDotToken()
        c.hooks.trPassiveCall(c, dest, n, beginRead target)
        dest.addSymUse tmpVar, info
    else:
      c.hooks.trPassiveCall(c, dest, n, default(Cursor))
  else:
    coroTrSons(c, dest, n)

proc trLocalValue*(c: var Context; dest: var TokenBuf; n: var Cursor; lhs: Cursor) =
  if c.hooks.isPassiveCall(c, n):
    c.hooks.trPassiveCall(c, dest, n, lhs)
  else:
    dest.copyIntoKind AsgnS, n.info:
      dest.copyTree lhs
      coroTr c, dest, n

proc trAsgn*(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var rhs = n.childCursor
  skip rhs
  if c.hooks.isPassiveCall(c, rhs):
    var lhsTransformed = createTokenBuf(6)
    n.into:
      coroTr c, lhsTransformed, n
      c.hooks.trPassiveCall(c, dest, n, beginRead lhsTransformed)
  else:
    copyInto dest, n:
      coroTr c, dest, n
      coroTr c, dest, n

proc trLocal*(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let sym = n.childCursor.symId
  let kind = n.symKind
  let info = n.info

  let field = c.currentProc.localToEnv.getOrDefault(sym)
  if field.def != field.use:
    n.into:
      skip n, SkipName # name
      skip n, SkipExport # exported
      skip n, SkipPragmas # pragmas
      c.typeCache.registerLocal(sym, kind, n)
      skip n, SkipType # type
      if n.kind == DotToken:
        inc n
      else:
        var lhs = createTokenBuf(6)
        lhs.copyIntoKind DotX, info:
          lhs.copyIntoKind DerefX, info:
            lhs.addSymUse pool.syms.getOrIncl(EnvParamName), info
          lhs.addSymUse field.field, info
        trLocalValue(c, dest, n, beginRead lhs)
  else:
    var pcall = false
    var callExpr = default(Cursor)
    copyInto dest, n:
      let target = n
      takeTree dest, n # name
      takeTree dest, n # export marker
      takeTree dest, n # pragmas
      c.typeCache.registerLocal(sym, kind, n)
      let isPassive = procHasPragma(n, PassiveP)
      # Use trProctype hook for the type slot so inline itertypes /
      # passive proctypes get the wrapper-signature rewrite — same
      # coverage as the TypeS path. sem often inlines named iter types
      # into use-site type slots, so without this the let's type
      # disagrees with what `consume(g: MyIter)`-style param types
      # get.
      c.hooks.trProctype(c, dest, n) # type
      pcall = c.hooks.isPassiveCall(c, n)
      if pcall:
        callExpr = n
        dest.addDotToken()
        skip n
      elif isPassive and n.kind == Symbol:
        # rhs is a `.passive` proc/iter sym used as a value — rewrite to
        # its init wrapper, NOT `target.symId` (that's the local var).
        # A non-Symbol rhs (e.g. `nil`, or another value of the same
        # type) is left to `coroTr`: the lowered passive type is a bare
        # wrapper proctype, so a plain `(nil)` is already well-typed.
        dest.addSymUse coroWrapperProc(c, n.symId), info
        inc n
      else:
        coroTr(c, dest, n)
    if pcall:
      var symBuf = createTokenBuf(1)
      symBuf.addSymUse target.symId, info
      c.hooks.trPassiveCall(c, dest, callExpr, beginRead symBuf)

# ---------------------------------------------------------------------
# State-machine entries — body-level structural lowering of yield/return
# ---------------------------------------------------------------------

proc declareContinuationResult*(c: var Context; dest: var TokenBuf; info: NifLineInfo) =
  dest.copyIntoKind ResultS, info:
    dest.addSymDef pool.syms.getOrIncl("result.0"), info
    dest.addDotToken() # exported
    dest.addDotToken() # pragmas
    dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
    dest.addDotToken() # default value

proc newLocalProc*(c: var Context; dest: var TokenBuf; state: int; sym: SymId) =
  c.awaitingSuspendPark = false
  const info = NoLineInfo
  let procBegin = dest.len
  dest.addParLe ProcS, info
  let name = stateToProcName(c, sym, state)
  dest.addSymDef name, info
  for i in 0..<3:
    dest.addDotToken() # exported, pattern, typevars
  dest.copyIntoKind ParamsU, info:
    dest.copyIntoKind ParamY, info:
      dest.addSymDef pool.syms.getOrIncl(EnvParamName), info
      dest.addDotToken() # export
      dest.addDotToken() # pragmas
      dest.copyIntoKind PtrT, info:
        dest.addSymUse coroTypeForProc(c, sym), info
      dest.addDotToken() # default value

  dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
  dest.addDotToken() # pragmas
  dest.addDotToken() # effects

  publishSignature dest, name, procBegin

  dest.addParLe StmtsS, info # body
  declareContinuationResult c, dest, info
  when defined(cpsDebugStates):
    dest.copyIntoKind CmdS, info:
      dest.addSymUse pool.syms.getOrIncl("write.0.syn1lfpjv"), info
      dest.addSymUse pool.syms.getOrIncl("stdout.0.syn1lfpjv"), info
      dest.addStrLit extractVersionedBasename(pool.syms[sym]) & ".s" & $state & "\n"

proc gotoNextState*(c: var Context; dest: var TokenBuf; state: int; info: NifLineInfo) =
  # generate: `return state(this)`
  dest.copyIntoKind RetS, info:
    dest.copyIntoKind CallS, info:
      dest.addSymUse stateToProcName(c, c.procStack[^1], state), info
      dest.addSymUse pool.syms.getOrIncl(EnvParamName), info

proc emitResultSlot(c: var Context; dest: var TokenBuf; info: NifLineInfo) =
  ## `(*this).result[]` — the caller's storage for what this coroutine
  ## produces, which for a `.raises` coroutine is where its ErrorCode goes too.
  dest.copyIntoKind DerefX, info:
    dest.copyIntoKind DotX, info:
      dest.copyIntoKind DerefX, info:
        dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
      dest.addSymUse pool.syms.getOrIncl(ResultFieldName), info

proc emitRaisingResultInit*(c: var Context; dest: var TokenBuf; info: NifLineInfo) =
  ## `result[] = Success` (or `result[0] = Success`) at coroutine entry. The
  ## ordinary lowering does this in `constparams.trResultDecl`; a coroutine has
  ## no `(result)` declaration left by then, so it happens here — and it has to
  ## happen, because the caller reads the code out of this slot whether or not
  ## the coroutine ever assigned it.
  if not c.currentProc.canRaise: return
  dest.copyIntoKind AsgnS, info:
    if c.currentProc.resultIsTuple:
      dest.copyIntoKind TupatX, info:
        emitResultSlot(c, dest, info)
        dest.addIntLit 0, info
    else:
      emitResultSlot(c, dest, info)
    dest.addSymUse pool.syms.getOrIncl(SuccessName), info

proc returnValue*(c: var Context; dest: var TokenBuf; n: var Cursor;
                  info: NifLineInfo; isRaise = false) =
  n.into: # yield/return/raise
    if n.kind == DotToken or (n.kind == Symbol and n.symId == c.currentProc.resultSym):
      inc n
    elif isVoidType(getType(c.typeCache, n)) and n.kind != Symbol:
      # void type for Symbol can happen for `raise` statements:
      coroTr c, dest, n
    elif isRaise and c.currentProc.resultIsTuple:
      # The operand is the ErrorCode; the value half of the slot keeps
      # whatever it held, exactly as `constparams.trRaise` leaves it for an
      # ordinary routine.
      dest.copyIntoKind AsgnS, info:
        dest.copyIntoKind TupatX, info:
          emitResultSlot(c, dest, info)
          dest.addIntLit 0, info
        coroTr c, dest, n
    elif not isRaise and c.currentProc.resultIsTuple:
      dest.copyIntoKind AsgnS, info:
        dest.copyIntoKind TupatX, info:
          emitResultSlot(c, dest, info)
          dest.addIntLit 1, info
        coroTr c, dest, n
    else:
      dest.copyIntoKind AsgnS, info:
        emitResultSlot(c, dest, info)
        coroTr c, dest, n

proc trYield*(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # yield ex
  # -->
  # this.res[] = ex
  # this.caller.fn = cast[ContinuationProc](nextState)  # .closure iters only
  # return Continuation(fn: stateToProcName(c, sym, nextState), env: this)
  #
  # The returned Continuation does NOT hand control to `this.caller` —
  # for `.closure` iters that slot is not a return target. State procs
  # are driven directly by the for-loop trampoline's `advance(it) =
  # it.fn(it.env)`; the returned `(nextState, this)` is what the next
  # `advance` invokes to resume. `this.caller.fn = nextState` is a
  # separate cache used only by the wrapper's reuse branch when the
  # iter VALUE is called as `g()` outside the loop (see
  # `tclosure_iter_shared_state.nim`); `this.caller.env` is the
  # ownership marker read by `emitFinalReturn`.
  let state = getNextState(c.currentProc.cf, n)
  assert state != -1
  let info = n.info
  returnValue(c, dest, n, info)
  stashResumeFn(c, dest, state, info)
  dest.copyIntoKind RetS, info:
    contNextState(c, dest, state, info)

proc trReturn*(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # return/raise x -->
  # this.res[] = x
  # var tmpCaller = this.caller; deallocFrame(this); return tmpCaller
  #
  # A `raise` leaves as a RETURN. A state proc's return type is the
  # `Continuation` the trampoline drives next, and it is not an error channel:
  # the code went into the result slot above, which is where the caller's
  # `(failed tmp)` reads it. Emitting the source's own tag here instead would
  # hand `(raise <Continuation>)` to a proc that does not raise.
  let isRaise = n.stmtKind == RaiseS
  let info = n.info
  returnValue(c, dest, n, info, isRaise)
  if c.currentProc.isClosureIter:
    # `.closure` iters: caller.fn holds the resume slot, not a return
    # target. Emit the same shape as emitFinalReturn.
    emitFinalReturn(c, dest, info)
    return
  let tmpVar = pool.syms.getOrIncl("`tmpCaller." & $c.currentProc.counter)
  inc c.currentProc.counter
  dest.copyIntoKind VarS, info:
    dest.addSymDef tmpVar, info
    dest.addDotToken() # exported
    dest.addDotToken() # pragmas
    dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
    dest.copyIntoKind DotX, info:
      dest.copyIntoKind DerefX, info:
        dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
      dest.addSymUse pool.syms.getOrIncl(CallerFieldName), info
      dest.addIntLit 1, info # field is in superclass
  emitDeallocFrame(c, dest, info)
  dest.copyIntoKind RetS, info:
    dest.addSymUse tmpVar, info

# ---------------------------------------------------------------------
# Lifetime + state analysis
# ---------------------------------------------------------------------

const
  AddressTaken = -1
    ## A `use` state no `def` can equal — `currentState` only ever comes from a
    ## non-negative `lab` literal — so a local marked with it always becomes a
    ## frame field.

proc markAddressTaken(c: var Context; n: Cursor) =
  ## A local whose address is taken has to live in the FRAME, not in whichever
  ## state proc happens to contain the `addr`. The pointer can be stored
  ## anywhere and read in a later state, i.e. after that proc returned, and the
  ## `def != use` rule below cannot see it: taking an address is not a use in a
  ## different state. It is also the rule the lifetime extension below rests
  ## on: the local it gives an escaping temporary is worth nothing unless the
  ## `(haddr)` next to it pins that local to the frame.
  ##
  ## Derefs are not followed: `addr p[]` is an address of what `p` points at,
  ## which is not `p`'s own storage and says nothing about where `p` must live.
  var n = n
  while true:
    case n.exprKind
    of DotX, TupatX, AtX, ArratX, AddrX, HaddrX:
      inc n
    of ConvKinds:
      inc n
      skip n # type part
    of BaseobjX:
      inc n
      skip n # type part
      skip n # skip intlit
    else:
      break
  if n.kind == Symbol:
    let known = c.currentProc.localToEnv.getOrDefault(n.symId, EnvField(def: -2))
    if known.def != -2:
      c.currentProc.localToEnv.getOrQuit(n.symId).use = AddressTaken

proc frameFieldType(c: var Context; local: SymId; declared: Cursor): Cursor =
  ## The type the FRAME has to give this local, which is not always the type it
  ## was declared with. A local holding a raising call's result is declared as
  ## the callee's written return type and only becomes `(ErrorCode, T)` in
  ## `lengcgen` — `constparams` bridges the gap for an ordinary local by
  ## retyping its declaration and projecting its uses, and it cannot do that
  ## for a frame field, because by the time it runs the declaration is an
  ## assignment and the uses are `(dot (deref this) fld)`. So the frame states
  ## the final type here, where the field is minted.
  if local notin c.currentProc.tupleVars:
    return declared
  var buf = createTokenBuf(8)
  addSuccessTupleType(buf, declared, declared.info)
  c.currentProc.synthTypes.add ensureMove(buf)
  result = beginRead(c.currentProc.synthTypes[c.currentProc.synthTypes.len-1])

proc escapingLocalsImpl(c: var Context; n: var Cursor; currentState: var int) =
  ## Processes the single tree/token at `n`, advancing past it.
  if n.stmtKind == LabS and n.childCursor.kind == IntLit:
    # Only the CPS state machine's own integer-labelled `lab` marks a state
    # boundary. A symbol-labelled one is `xelim`'s structured merge label
    # (`doc/final_ir.md`) and is opaque here.
    currentState = int(n.childCursor.intVal)

  let sk = n.stmtKind
  case sk
  of LocalDecls:
    n.into:
      let mine = n.symId
      if sk == ResultS:
        c.currentProc.resultSym = mine
      skip n # name
      skip n # exported
      let pragmas = n
      skip n # pragmas
      c.currentProc.localToEnv[mine] = EnvField(
        objType: coroTypeForProc(c, c.procStack[^1]),
        field: if sk == ResultS: pool.syms.getOrIncl(ResultFieldName) else: localToFieldname(c, mine),
        pragmas: pragmas,
        typ: frameFieldType(c, mine, n),
        def: currentState,
        use: currentState)
      skip n # type
      while n.hasMore:
        escapingLocalsImpl c, n, currentState # the value
  else:
    case n.kind
    of TagLit:
      if n.exprKind in {AddrX, HaddrX}:
        markAddressTaken c, n.childCursor
      n.loopInto:
        escapingLocalsImpl c, n, currentState
    of Symbol:
      let def = c.currentProc.localToEnv.getOrDefault(n.symId, EnvField(def: -2)).def
      if def != -2:
        if def != currentState:
          c.currentProc.localToEnv.getOrQuit(n.symId).use = currentState
      inc n
    else:
      inc n

proc escapingLocals*(c: var Context; n: Cursor) =
  if n.isDotToken: return
  var currentState = 0
  var n = n
  escapingLocalsImpl c, n, currentState

proc containsSuspensionPoint*(c: var Context; n: Cursor): bool =
  var n = n
  if n.stmtKind == YldS or c.hooks.isPassiveCall(c, n) or n.exprKind == SuspendX:
    return true
  # `linearScan` visits the nested nodes; the root was checked above
  linearScan n:
    if n.stmtKind == YldS or c.hooks.isPassiveCall(c, n) or n.exprKind == SuspendX:
      return true
  return false

const
  KeptLoop* = -1
    ## `ProcContext.loopHeads` entry for a loop kept as a `(loop ...)`
    ## construct: it names no state, so it is no jump target.

proc emitJump*(dest: var TokenBuf; label: int; info: NifLineInfo) =
  dest.addParLe(JmpS, info)
  dest.addIntLit label, info
  dest.addParRi()

proc emitLabel*(dest: var TokenBuf; label: int; info: NifLineInfo) =
  dest.addParLe(LabS, info)
  dest.addIntLit label, info
  dest.addParRi()

proc trGoto*(c: var Context; dest: var TokenBuf; n: var Cursor)

# ---------------------------------------------------------------------
# Lifetime extension for escaping data
# ---------------------------------------------------------------------
#
# A state proc is not the coroutine's activation record. It runs, it returns,
# and its stack is gone — while the FRAME, and everything the frame points at,
# has to survive until the coroutine is resumed and, for a `.passive` callee,
# until that callee has run to completion. So the coroutine has exactly one
# kind of storage that outlives a state: the frame.
#
# `escapingLocals` below decides which NAMED locals go there — the `def != use`
# rule, plus `markAddressTaken` for a local a pointer is kept to. Neither rule
# can see data that has no name at all, and a value that is only ever an
# argument does not get one until much later: `constparams.trConstRef` gives a
# literal or a constructor a temporary to take the address of, and by then the
# body has been cut and that temporary is a local of ONE state proc. The
# pointer taken from it lives on — in an `openArray` in the frame, in the
# `.passive` callee's own frame — so the length arrives intact and the data
# pointer points into a dead stack slot.
#
# The fix is to name that data here, while there is still one body to name it
# in: an actual that will reach its callee as an address gets a local of the
# coroutine, `markAddressTaken` pins the local to the frame, and the later
# `trConstRef` finds an argument that already has an address and leaves it
# alone. `nextArgRole` is the shared classifier, so the two passes cannot
# disagree about which actual goes with which formal.
#
# This rides on `trGoto`'s walk rather than adding a pass of its own: `trGoto`
# already rewrites the whole body, already maintains the type cache this needs,
# and runs right before `escapingLocals` — which is precisely the window in
# which "give it a name" is still enough to mean "put it in the frame".

proc trGotoValue(c: var Context; dest: var TokenBuf; n: var Cursor)

proc extendLifetime(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## `n` is an actual the callee receives as an ADDRESS. If it has no storage
  ## of its own to take an address of, give it some — as a local, which the
  ## `(haddr)` below then pins to the frame.
  if not constructsValue(n):
    # An lvalue already has storage of its own, and whether THAT storage is
    # durable enough is the ordinary escape question `escapingLocals` answers:
    # a borrow which outlives the call is one the caller kept, and keeping it
    # means an `addr` that `markAddressTaken` can see — `toOpenArray` is
    # `.inline`, so the `(haddr local)` inside it is in this body already.
    trGotoValue c, dest, n
    return
  let info = n.info
  let argType = getType(c.typeCache, n)
  let symId = pool.syms.getOrIncl("`coroTemp." & $c.currentProc.counter)
  inc c.currentProc.counter
  # The `(expr (stmts ...) v)` shape rather than a statement hoisted in front
  # of the enclosing one: it keeps the value's evaluation exactly where it was,
  # which matters as soon as a second argument of the same call has side
  # effects. `xelim_final` flattens it at the end of the pipeline, the same way
  # it flattens the temporaries `constparams` and `vtables` emit.
  copyIntoKind dest, ExprX, info:
    copyIntoKind dest, StmtsS, info:
      copyIntoKind dest, VarS, info:
        addSymDef dest, symId, info
        dest.addEmpty2 info # export marker, pragmas
        copyTree dest, argType
        c.typeCache.registerLocal(symId, VarY, argType)
        trGotoValue c, dest, n # the value
    copyIntoKind dest, HaddrX, info:
      dest.addSymUse symId, info

proc trGotoCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var fnType = skipProcTypeToParams(getType(c.typeCache, n.childCursor))
  if not fnType.isParamsTag:
    # Nothing with formal parameters to walk against; just recurse.
    copyInto dest, n:
      while n.hasMore: trGotoValue c, dest, n
    return
  dest.addParLe(n.cursorTagId, n.info)
  n.into:
    trGotoValue c, dest, n # the `fn`
    fnType = sub(fnType)   # peek only, never left
    while n.hasMore:
      case nextArgRole(fnType, c.ptrSize, c.sizeofCache)
      of argConstRef: extendLifetime c, dest, n
      of argPlain, argCompileTime: trGotoValue c, dest, n
  dest.addParRi(n.endInfo)

proc trGotoValue(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Copy an expression, extending the lifetime of every escaping temporary
  ## inside it. Used wherever `trGoto` would otherwise `takeTree` a value.
  case n.kind
  of TagLit:
    if n.exprKind in CallKinds:
      trGotoCall c, dest, n
    else:
      copyInto dest, n:
        while n.hasMore: trGotoValue c, dest, n
  else:
    takeTree dest, n

proc emitSymJump(dest: var TokenBuf; label: SymId; info: NifLineInfo) =
  dest.addParLe(JmpS, info)
  dest.addSymUse label, info
  dest.addParRi()

proc emitSymLabel(dest: var TokenBuf; label: SymId; info: NifLineInfo) =
  dest.addParLe(LabS, info)
  dest.addSymDef label, info
  dest.addParRi()

proc containsRaise(n: Cursor): bool =
  ## Can this subtree leave with an exception? After the eraiser every raising
  ## call is already `(var tmp (call f)) (ite (failed tmp) (raise tmp))`, so a
  ## literal `raise` node is the only way out. Conservative in the safe
  ## direction: a `try` whose body cannot raise needs no tracker and no
  ## handlers, which is what keeps `try: echo x finally: echo y` from
  ## fabricating an error path a void coroutine has no frame slot for.
  var n = n
  if n.stmtKind == RaiseS: return true
  linearScan n:
    if n.stmtKind == RaiseS: return true
  return false

proc isCatchAll(exceptNode: Cursor): bool =
  ## `except:` and `except e:` catch everything; `except SomeType:` does not.
  ## Only a catch-all lets us drop the trailing re-raise.
  var n = exceptNode
  n = sub(n)
  if not n.hasMore: return true
  if n.isDotToken: return true
  result = isLocal(n.symKind)

proc freshTrySym(c: var Context; prefix: string): SymId =
  result = pool.syms.getOrIncl(prefix & $c.currentProc.counter & "." & c.thisModuleSuffix)
  inc c.currentProc.counter

proc trGotoScoped(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## A *body slot* of a construct we are keeping (`ite` arm, `loop` body, a
  ## `case` branch). Exactly one `(stmts ...)` node has to come out — the
  ## flattening `trGoto` does for the goto stream would splice several
  ## statements into a slot that holds one.
  if n.stmtKind in {StmtsS, ScopeS}:
    dest.addParLe(n.cursorTagId, n.info)
    n.into:
      while n.hasMore:
        trGoto c, dest, n
    dest.addParRi()
  else:
    dest.copyIntoKind StmtsS, n.info:
      trGoto c, dest, n

proc trGotoBuf(c: var Context; dest: var TokenBuf; buf: var TokenBuf) =
  ## Run the goto conversion over a freshly synthesized statement list so the
  ## synthesized `ite`s get the same suspension-point splitting as the ones
  ## that came out of `finalir`. `buf` may die on return even though `trGoto`
  ## registers a local's type in the `TypeCache` as a Cursor INTO it: a Cursor
  ## holds a reference on the buffer's `CursorOwner`, so the token storage
  ## outlives the `TokenBuf` for exactly as long as something still points at
  ## it.
  var m = beginRead(buf)
  while m.hasMore:
    trGoto c, dest, m

proc emitErrorCodeOf(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## The ERROR CODE of a raise operand. A raising call's temp holds
  ## `(ErrorCode, T)` when the callee returns a value, and the code is its
  ## first half; the temp of a `void` raising call holds the code itself, and
  ## an error tracker is already a bare code. Getting this wrong does not fail
  ## to compile in the ordinary case — every shape here is one word — it puts
  ## the returned VALUE where the code belongs.
  if n.kind == Symbol and n.symId in c.currentProc.tupleVars and
     not isVoidType(getType(c.typeCache, n)):
    let info = n.info
    dest.copyIntoKind TupatX, info:
      dest.takeTree n
      dest.addIntLit 0, info
  else:
    dest.takeTree n

proc trRaiseGoto(c: var Context; dest: var TokenBuf; n: var Cursor): bool =
  ## A `raise` inside a `try` BODY is not a proc exit. Store the error code in
  ## that try's tracker and jump to its handler dispatch. Returns false when no
  ## `try` encloses it — then it really is a proc exit and `coroTr.trReturn`
  ## takes it.
  if c.currentProc.tryTargets.len == 0: return false
  let t = c.currentProc.tryTargets[^1]
  let info = n.info
  let raiseStart = n
  n = sub(n)
  if not n.hasMore or n.isDotToken:
    # a bare re-raise: the in-flight tracker still holds the code
    if c.currentProc.inFlight != NoSymId and c.currentProc.inFlight != t.tracker:
      dest.copyIntoKind AsgnS, info:
        dest.addSymUse t.tracker, info
        dest.addSymUse c.currentProc.inFlight, info
  else:
    dest.copyIntoKind AsgnS, info:
      dest.addSymUse t.tracker, info
      emitErrorCodeOf c, dest, n
  emitSymJump dest, t.label, info
  n = raiseStart; skip n
  result = true

proc trTryGoto(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Lower `try`/`except`/`fin` to the Final IR's own vocabulary — an error
  ## tracker plus one `(lab)` the body's `raise`s jump to:
  ##
  ##     (var err ErrorCode Success)
  ##     <body>                          # `raise x` -> `err = x; jmp Lexc`
  ##     (lab Lexc)
  ##     (ite (neq ErrorCode err Success)         # once per `except`
  ##          (stmts <handler> (asgn err Success)) .)
  ##     <finally>
  ##     (ite (neq ErrorCode err Success) (stmts (raise err)) .)   # if uncaught
  ##
  ## `nj.nim` expressed the same thing with a monotone guard cfvar per try and
  ## a `jtrue` that set every guard between the `raise` and its handler, because
  ## it had no jump to offer. With `lab`/`jmp` the "did we already leave"
  ## question is answered positionally, so the guards — and the `(ite (not g) ...)`
  ## wrap around every following statement that came with them — are gone.
  ##
  ## An `except` with a TYPE pattern is not dispatched on, exactly as `nj.nim`
  ## did not dispatch on it: the handler runs for any error and consumes it.
  ## Typed `except` is unfinished in the front end too (`except A, B:` does not
  ## get past sem), so this pass is not where that gets decided.
  let info = n.info
  let tryStart = n
  var body = sub(n)                     # the try body
  var probe = body
  skip probe                            # -> first `except` / `fin`, if any
  let needsTracker = containsRaise(body)
  var catchAll = false
  var q = probe
  while q.hasMore and q.substructureKind == ExceptU:
    if isCatchAll(q): catchAll = true
    skip q
  let tracker = if needsTracker: freshTrySym(c, "´err.") else: NoSymId
  let excLab = if needsTracker: freshTrySym(c, "´exc.") else: NoSymId
  if needsTracker:
    dest.copyIntoKind VarS, info:
      dest.addSymDef tracker, info
      dest.addDotToken()                # exported
      dest.addDotToken()                # pragmas
      dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), info
      dest.addSymUse pool.syms.getOrIncl(SuccessName), info

  # --- the guarded body ------------------------------------------------
  if needsTracker:
    c.currentProc.tryTargets.add TryTarget(tracker: tracker, label: excLab)
  var b = body
  trGoto c, dest, b
  if needsTracker:
    discard c.currentProc.tryTargets.pop()
    emitSymLabel dest, excLab, info

  # --- the handlers ----------------------------------------------------
  let savedInFlight = c.currentProc.inFlight
  if needsTracker: c.currentProc.inFlight = tracker
  var m = probe
  while m.hasMore and m.substructureKind == ExceptU:
    let exceptStart = m
    if needsTracker:
      var h = createTokenBuf(64)
      h.addParLe IteV, info
      # NOT `(failed t)`: that tag is the eraiser's contract for a raising
      # call's own result temp, and `constparams.localsThatBecomeTuples`
      # retypes every symbol it sees under one to `(tuple ErrorCode T)`. The
      # tracker is a plain `ErrorCode`, so it asks the plain question.
      h.copyIntoKind NeqX, info:
        h.addSymUse pool.syms.getOrIncl(ErrorCodeName), info
        h.addSymUse tracker, info
        h.addSymUse pool.syms.getOrIncl(SuccessName), info
      h.addParLe StmtsS, info
      var e = sub(m)
      while e.hasMore:
        if e.stmtKind in {StmtsS, ScopeS}:
          e.into:                       # inline the handler body
            while e.hasMore: h.takeTree e
        elif isLocal(e.symKind):
          # `except err:` — bind the exception variable to the tracker
          var d = e
          h.addParLe LetS, e.info
          d = sub(d)
          h.takeTree d                  # name
          h.takeTree d                  # export marker
          h.takeTree d                  # pragmas
          h.takeTree d                  # type
          h.addSymUse tracker, e.info   # value: the code we caught
          h.addParRi()
          skip e
        else:
          skip e                        # an exception TYPE pattern: unmodelled
      h.copyIntoKind AsgnS, info:       # handled: the error is consumed
        h.addSymUse tracker, info
        h.addSymUse pool.syms.getOrIncl(SuccessName), info
      h.addParRi()                      # close `stmts`
      h.addDotToken()                   # no else
      h.addParRi()                      # close `ite`
      trGotoBuf c, dest, h
    m = exceptStart; skip m

  # --- the finally, which runs on every path ---------------------------
  if m.hasMore and m.substructureKind == FinU:
    var f = sub(m)
    trGoto c, dest, f
  c.currentProc.inFlight = savedInFlight

  # --- whatever no handler consumed keeps travelling -------------------
  if needsTracker and not catchAll:
    var r = createTokenBuf(24)
    r.addParLe IteV, info
    r.copyIntoKind NeqX, info:
      r.addSymUse pool.syms.getOrIncl(ErrorCodeName), info
      r.addSymUse tracker, info
      r.addSymUse pool.syms.getOrIncl(SuccessName), info
    r.copyIntoKind StmtsS, info:
      r.copyIntoKind RaiseS, info:
        r.addSymUse tracker, info
    r.addDotToken()                     # no else
    r.addParRi()
    trGotoBuf c, dest, r

  n = tryStart; skip n

proc trGoto*(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var info = n.info
  case n.finalIrKind
  of ContinueV:
    # The back-edge of the INNERMOST enclosing loop. For a suspending one that
    # is the jump to its head state, emitted by the `LoopV` case below. For a
    # loop kept as a construct the marker stays as it is: `coroTr`'s `LoopV`
    # case reads it to tell the loop's own tail from a source-level `continue`.
    # At the top level of a body (which the Final IR never produces) there is
    # nothing to jump to.
    if c.currentProc.loopHeads.len == 0:
      skip n
    elif c.currentProc.loopHeads[^1] == KeptLoop:
      dest.takeTree n
    else:
      emitJump dest, c.currentProc.loopHeads[^1], info
      skip n
  of StoreV:
    var addLabel = false
    takeInto dest, n:
      addLabel = c.hooks.isPassiveCall(c, n)
      trGotoValue c, dest, n
      trGotoValue c, dest, n
    if addLabel:
      emitLabel dest, c.currentProc.labelCounter, info
      inc c.currentProc.labelCounter
  of LoopV:
    if containsSuspensionPoint(c, n):
      # A Final IR `(loop (stmts BODY (continue .)))` is unconditional: there
      # is no condition slot to rotate and no prelude to split off, because
      # `finalir.trWhile` already turned `while cond` into a leading guard
      # whose `else` is a `(jmp loopExit)`. So the whole construct is one
      # state label plus a back-edge to it, and every way OUT is already a
      # `jmp` to the `(lab loopExit)` the Final IR emitted after the loop.
      let head = c.currentProc.labelCounter
      inc c.currentProc.labelCounter
      emitJump dest, head, info                     # fall into the head state
      emitLabel dest, head, info
      c.currentProc.loopHeads.add head
      n.into:                                       # (loop ...)
        assert n.stmtKind == StmtsS, $n.kind
        n.into:                                     # the body
          while n.hasMore:
            trGoto c, dest, n                       # `(continue .)` -> jmp head
      discard c.currentProc.loopHeads.pop()
    else:
      # Kept as a construct, but still WALKED: a `raise` in here belongs to an
      # enclosing `try` and has to be redirected even though no state boundary
      # falls inside. Copying the subtree verbatim would leave it a proc exit.
      dest.addParLe(n.cursorTagId, info)
      c.currentProc.loopHeads.add KeptLoop
      n.into:
        trGotoScoped c, dest, n
      discard c.currentProc.loopHeads.pop()
      dest.addParRi()
  of IteV, ItecV:
    if containsSuspensionPoint(c, n):
      n.into:
        var lthen = c.currentProc.labelCounter
        inc c.currentProc.labelCounter
        var lelse = c.currentProc.labelCounter
        inc c.currentProc.labelCounter
        var lend = c.currentProc.labelCounter
        inc c.currentProc.labelCounter
        dest.copyIntoKind IfS, info:
          dest.copyIntoKind ElifU, info:
            trGotoValue c, dest, n # cond
            dest.copyIntoKind StmtsS, info:
              emitJump dest, lthen, info
        var thenCur = n
        skip n
        var elseCur = n
        if n.hasMore: skip n
        # Else-branch presence: the missing-else case presents as a `DotToken`
        # placeholder (`finalir.trIf`) or, for an `ite` whose else slot was
        # elided altogether, as the scope's close. Treating the scope end as
        # "no else" prevents `elseCur.into:` from asserting on a non-ParLe
        # cursor.
        if elseCur.hasMore and elseCur.isTagLit:
          emitJump dest, lelse, info
          emitLabel dest, lelse, info
          elseCur.into:
            while elseCur.hasMore:
              trGoto c, dest, elseCur
        emitJump dest, lend, info
        emitLabel dest, lthen, info
        thenCur.into:
          while thenCur.hasMore:
            trGoto c, dest, thenCur
        emitJump dest, lend, info
        emitLabel dest, lend, info
    else:
      dest.addParLe(n.cursorTagId, info)
      n.into:
        trGotoValue c, dest, n         # condition
        trGotoScoped c, dest, n        # then
        if n.hasMore:
          if n.isTagLit: trGotoScoped c, dest, n   # else
          else: dest.takeTree n                    # `.`: no else
        while n.hasMore: skip n
      dest.addParRi()
  else:
    let sk = n.stmtKind
    let ek = n.exprKind
    if sk == YldS or c.hooks.isPassiveCall(c, n) or ek == SuspendX:
      trGotoValue c, dest, n
      emitLabel dest, c.currentProc.labelCounter, info
      inc c.currentProc.labelCounter
    else:
      case n.kind
      of TagLit:
        if n.exprKind in CallKinds:
          # Also reached for a `(call ...)` in STATEMENT position: same tree,
          # same actuals, and its arguments escape the same way.
          trGotoCall c, dest, n
          return
        case n.stmtKind
        of LocalDecls - {ResultS}:
          let sym = n.childCursor.symId
          let kind = n.symKind
          var addLabel = false
          takeInto dest, n:
            dest.takeTree n
            dest.takeTree n
            dest.takeTree n
            c.typeCache.registerLocal(sym, kind, n)
            # dont change type, tr will traverse it again later
            dest.takeTree n
            addLabel = c.hooks.isPassiveCall(c, n)
            trGotoValue c, dest, n
          if addLabel:
            emitLabel dest, c.currentProc.labelCounter, info
            inc c.currentProc.labelCounter
        of CallS, CmdS, ResultS, ProcS, FuncS, IteratorS,
            ConverterS, MethodS, MacroS, TemplateS, TypeS,
            BlockS, EmitS, AsgnS, IfS, WhenS,
            BreakS, ContinueS, ForS, WhileS, CoroforS,
            RetS, YldS, PragmasS, PragmaxS, InclS, ExclS,
            IncludeS, ImportS, ImportasS, FromimportS,
            ImportexceptS, ExportS, ExportexceptS, CommentS,
            DiscardS, UnpackdeclS, AssumeS,
            AssertS, CallstrlitS, InfixS, PrefixS, HcallS,
            StaticstmtS, BindS, MixinS, UsingS, AsmS,
            DeferS, LabS, JmpS, NoStmt:
          dest.addParLe(n.cursorTagId, n.info)
          n.into:
            while n.hasMore:
              trGoto c, dest, n
          dest.addParRi()
        of CaseS:
          # `finalir` keeps `case` as a construct ("Format as existing"), so
          # unlike `nj.nim` — which lowered every branch to an `ite` chain —
          # the state machine meets it head on. A branch that suspends gets
          # the same treatment as an `ite` arm: the dispatch keeps only the
          # jumps, and each body is laid out flat behind its own label.
          if containsSuspensionPoint(c, n):
            let caseStart = n
            let lend = c.currentProc.labelCounter
            inc c.currentProc.labelCounter
            var labels: seq[int] = @[]
            var hasElse = false
            dest.addParLe(n.cursorTagId, info)      # the dispatch: jumps only
            n.into:
              trGotoValue c, dest, n                # selector
              while n.hasMore:
                let subK = n.substructureKind
                if subK in {OfU, ElseU}:
                  let l = c.currentProc.labelCounter
                  inc c.currentProc.labelCounter
                  labels.add l
                  if subK == ElseU: hasElse = true
                  dest.addParLe(n.cursorTagId, n.info)
                  n.into:
                    if subK == OfU: dest.takeTree n # ranges
                    dest.copyIntoKind StmtsS, n.info:
                      emitJump dest, l, n.info
                    while n.hasMore: skip n
                  dest.addParRi()
                else:
                  dest.takeTree n
            dest.addParRi()
            if not hasElse:
              emitJump dest, lend, info             # no branch matched
            var b = caseStart
            b = sub(b)
            skip b                                  # past the selector
            var i = 0
            while b.hasMore:
              if b.substructureKind in {OfU, ElseU}:
                emitLabel dest, labels[i], info
                var body = sub(b)
                if b.substructureKind == OfU: skip body   # ranges
                body.into:
                  while body.hasMore:
                    trGoto c, dest, body
                emitJump dest, lend, info
                inc i
              skip b
            emitLabel dest, lend, info
            n = caseStart; skip n
          else:
            dest.addParLe(n.cursorTagId, info)
            n.into:
              trGotoValue c, dest, n                # selector
              while n.hasMore:
                let subK = n.substructureKind
                if subK in {OfU, ElseU}:
                  dest.addParLe(n.cursorTagId, n.info)
                  n.into:
                    if subK == OfU: dest.takeTree n # ranges
                    trGotoScoped c, dest, n         # branch body
                    while n.hasMore: skip n
                  dest.addParRi()
                else:
                  dest.takeTree n
            dest.addParRi()
        of TryS:
          trTryGoto c, dest, n
        of RaiseS:
          if not trRaiseGoto(c, dest, n):
            # no enclosing `try`: a proc exit, taken by `coroTr.trReturn`,
            # which writes the code into the frame's result slot
            dest.addParLe(n.cursorTagId, n.info)
            n.into:
              while n.hasMore:
                emitErrorCodeOf c, dest, n
            dest.addParRi()
        of StmtsS, ScopeS:
          # FLATTEN. The Final IR wraps every body — an `ite` arm, a loop body,
          # a `block` — in its own `(stmts ...)`, and the scope ends are already
          # spelled out as `kill` instructions. The state machine's `(lab k)`
          # closes the current state proc and opens the next one, so it can only
          # live at the TOP level of the body: a label emitted inside a nested
          # list would close that list's parens instead of the proc's. `nj.nim`
          # never produced the nesting (its guard lowering flattens by
          # construction), which is why this had no counterpart before.
          n.into:
            while n.hasMore:
              trGoto c, dest, n
      else:
        dest.takeTree n

proc toGoto*(c: var Context; n: Cursor): TokenBuf =
  result = createTokenBuf(300)
  assert n.stmtKind == StmtsS, $n.kind
  var n = n
  # `trGoto` flattens `(stmts ...)`, so the body's own wrapper is emitted here.
  result.addParLe(n.cursorTagId, n.info)
  n.into:
    while n.hasMore:
      trGoto(c, result, n)
  result.addParRi()

proc rewriteCrossStateJumps(n: var Cursor; dest: var TokenBuf; states: Table[SymId, int]) =
  if n.kind == TagLit:
    let sk = n.stmtKind
    if sk in {LabS, JmpS}:
      let op = n.childCursor
      if op.kind in {Symbol, SymbolDef} and states.hasKey(op.symId):
        let state = states.getOrDefault(op.symId)
        if sk == LabS:
          emitJump dest, state, n.info # preserve the fall-through edge
          emitLabel dest, state, n.info
        else:
          emitJump dest, state, n.info
        skip n
        return
    dest.addParLe(n.cursorTagId, n.info)
    n.into:
      while n.hasMore:
        rewriteCrossStateJumps(n, dest, states)
    dest.addParRi()
  else:
    takeTree dest, n

proc repairCrossStateJumps(c: var Context) =
  ## The Final IR's own control flow is `(jmp L)` to a later `(lab :L)`: a loop
  ## exit, a `break`, an `if`/`elif` merge, a short-circuit `and`/`or`. Those
  ## are *structured* transfers — they stay inside one function, and `lengcgen`
  ## maps them straight to a C label and `goto`.
  ##
  ## The state machine cuts the body into one proc per `(lab k)`, so a symbolic
  ## jump whose label ends up in a *different* state proc has no target left. A
  ## state transition is what a cross-proc jump has to be, so convert exactly
  ## those pairs: the jump becomes `(jmp k)` and the label gets a fall-through
  ## `(jmp k)` in front of it, mirroring `trGoto`'s own emitJump/emitLabel
  ## pattern so the edge that used to fall into the label still reaches it.
  ##
  ## Pairs that stay within one state are left alone — a `goto` is cheaper than
  ## a trampoline bounce, and every state label that survives here is a local
  ## the state proc must otherwise hand to the frame.
  ##
  ## Must run before `escapingLocals` so locals that now cross a state boundary
  ## are moved into the frame.
  ##
  ## ITERATED to a fixed point, and that is not a detail: converting a pair
  ## PLANTS a new `(lab k)` where its label was, which is itself a new state
  ## boundary and can split a pair that was intra-state a moment ago. #2366 is
  ## exactly that shape — the `and` of a `while` condition and the loop's own
  ## exit share a segment until the exit is converted, and the surviving
  ## `goto` then names a label that moved to the next state proc.
  template buf: TokenBuf = c.currentProc.cf
  while true:
    var seg = 0
    var labSeg = initTable[SymId, int]()
    var jmps: seq[(SymId, int)] = @[]
    var pos = 0
    while pos < buf.len:
      if buf[pos].kind == TagLit and
         (buf[pos].tagId == TagId(LabS) or buf[pos].tagId == TagId(JmpS)):
        let isLab = buf[pos].tagId == TagId(LabS)
        let operand = readonlyCursorAt(buf, pos + tokenWidth(readonlyCursorAt(buf, pos)))
        if operand.kind == IntLit:
          if isLab: inc seg
        elif operand.kind in {Symbol, SymbolDef}:
          if isLab:
            labSeg[operand.symId] = seg
          else:
            jmps.add (operand.symId, seg)
      inc pos
    var states = initTable[SymId, int]()
    for (s, jseg) in jmps:
      if labSeg.getOrDefault(s, jseg) != jseg and not states.hasKey(s):
        states[s] = c.currentProc.labelCounter
        inc c.currentProc.labelCounter
    # Every round retires at least one symbolic label, so this terminates.
    if states.len == 0: break
    var dest = createTokenBuf(buf.len + 16)
    var n = beginRead(buf)
    while n.hasMore:
      rewriteCrossStateJumps(n, dest, states)
    c.currentProc.cf = ensureMove dest

# ---------------------------------------------------------------------
# Body lowering — produce the state-machine procs for a single
# coroutine routine
# ---------------------------------------------------------------------

proc completeFrameConstr(c: var Context; init: var TokenBuf) =
  ## Close the frame constructor `patchParamList` left open, defaulting
  ## every field it did not mention.
  ##
  ## `oconstr` is total — it names every field of its type — and this is
  ## where the coroutine frame's constructor earns that. The C back end
  ## forgives less than totality, since a designated initializer zeroes
  ## whatever it does not mention, but the native one stores exactly the
  ## fields the constructor lists, so a field left out here holds
  ## whatever the frame's storage held: stack garbage for the
  ## stack-allocated frames `cps` emits. A garbage `string` field is then
  ## a `=destroy` reading a wild length and freeing a wild pointer.
  ##
  ## The fields at stake are the iterator's own locals, hoisted into the
  ## frame — so `escapingLocals` must have run, and the predicate here is
  ## the one `generateCoroutineType` uses to decide which locals become
  ## fields at all. The two must agree exactly: a field the type has and
  ## the constructor lacks is the bug above, and a field the constructor
  ## has and the type lacks does not compile.
  const info = NoLineInfo
  for key, value in c.currentProc.localToEnv.pairs:
    if (value.def != value.use or key == c.currentProc.resultSym) and
       value.field notin c.currentProc.constrFields:
      # The default's type has to be the field's own, and the field got
      # its type from `coroTr` (`generateCoroutineType`) — which is not a
      # copy: it rewrites an iterator proctype into its wrapper's
      # signature, turning a closure tuple into a plain proc pointer. Run
      # the same rewrite here or the constructor hands a two-word closure
      # to a one-word field.
      var typBuf = createTokenBuf(16)
      var typ = value.typ
      coroTr(c, typBuf, typ)
      let fieldType = beginRead(typBuf)
      init.copyIntoKind KvU, info:
        init.addSymUse value.field, info
        if key == c.currentProc.resultSym:
          # The result field is a `(ptr T)`: the frame does not own the
          # result, it points at the caller's location for it.
          init.copyIntoKind NilX, info:
            init.copyIntoKind PtrT, info:
              init.addSubtree fieldType
        else:
          addDefaultValue(init, fieldType, info, c.ptrSize)
  init.addParRi() # object constructor
  init.addParRi() # assignment

proc treIteratorBody*(c: var Context; dest: var TokenBuf; init: var TokenBuf; iter: Cursor; sym: SymId) =
  # Lower the proc body to the FINAL IR (`doc/final_ir.md`) — `loop`/`ite`/
  # `case`/`lab`/`jmp`, control flow entirely statement-based — and keep it in
  # `c.currentProc.cf`. The state machine wants a body it can CUT, and the
  # Final IR's `lab`/`jmp` is already the cut: `toGoto` only has to decide
  # which of those transfers has to become a state transition.
  #
  # This used to run `nj.nim`, whose whole job is the opposite one: it
  # ELIMINATES jumps, materialising a monotone `mflag` guard per construct and
  # wrapping every following statement in `(ite (not g) ...)`. The state
  # machine then had `toGoto` put the jumps back. Two inverse rewrites in a
  # row, and the guard machinery's else-exploitation is what produced the
  # unbalanced `ite` of #2362's sibling (see `tests/nimony/cps/treturn_or_guard.nim`).
  var wrapper = createTokenBuf(10)
  wrapper.addParLe StmtsS, NoLineInfo
  wrapper.copyTree iter
  wrapper.addParRi()
  # `c.ptrSize` IS the target width; the 0 that stood here was invisible only
  # while the type cache ignored what it was handed.
  var pass = initPass(ensureMove wrapper, c.thisModuleSuffix, "finalir",
                      c.ptrSize * 8, nextTemp = c.nextTemp)
  toFinalIr(pass)
  c.nextTemp = pass.nextTemp
  block extractBody:
    var wholeResult = ensureMove(pass.dest)
    var nExt = beginRead(wholeResult)
    inc nExt  # skip outer StmtsS, now at first child
    let procKind = iter.stmtKind
    while nExt.hasMore and nExt.stmtKind != procKind:
      skip nExt
    inc nExt  # skip ProcS/IteratorS tag, now at first header subtree
    for i in 0..<BodyPos:
      skip nExt
    var bodyBuf = createTokenBuf(wholeResult.len)
    bodyBuf.copyTree nExt
    c.currentProc.cf = ensureMove bodyBuf

  when defined(logPasses):
    echo "========= FINAL IR ======="
    echo c.currentProc.cf.toString(false)
    echo ""
  # Which locals hold a raising call's result. Both halves of the transform
  # need it: `toGoto` to take the error CODE out of one when a `raise` feeds a
  # try's tracker, and `frameFieldType` to give the frame field the type the
  # local will actually hold.
  c.currentProc.tupleVars = localsThatBecomeTuples(beginRead(c.currentProc.cf))
  c.currentProc.cf = toGoto(c, beginRead(c.currentProc.cf))
  repairCrossStateJumps(c)
  when defined(logPasses):
    echo "========= GOTO ======="
    echo c.currentProc.cf.toString(false)
    echo ""

  var n = beginRead(c.currentProc.cf)
  escapingLocals(c, n)
  completeFrameConstr(c, init)

  assert n.stmtKind == StmtsS
  dest.addParLe(n.cursorTagId, n.info)
  n.into:
    dest.add init
    emitRaisingResultInit c, dest, NoLineInfo
    declareContinuationResult c, dest, NoLineInfo
    dest.copyIntoKind RetS, n.info:
      contNextState(c, dest, 0, n.info)
    dest.addParRi() # close stmts
    dest.addParRi() # close proc decl
    newLocalProc c, dest, 0, c.procStack[^1]
    while n.hasMore:
      coroTr c, dest, n

proc generateCoroutineType*(c: var Context; dest: var TokenBuf; sym: SymId) =
  const info = NoLineInfo
  let beforeType = dest.len
  let objType = coroTypeForProc(c, sym)
  copyIntoKind dest, TypeS, info:
    dest.addSymDef objType, info
    dest.addDotToken() # exported
    dest.addDotToken() # typevars
    dest.addDotToken() # pragmas
    copyIntoKind dest, ObjectT, info:
      # we inherit from CoroutineBase:
      dest.addSymUse pool.syms.getOrIncl(RootObjName), info
      for key, value in c.currentProc.localToEnv.pairs:
        if value.def != value.use or key == c.currentProc.resultSym:
          let beforeField = dest.len
          copyIntoKind dest, FldU, info:
            dest.addSymDef value.field, info
            dest.addDotToken() # exported
            var typ = value.typ
            if cursorIsNil(value.pragmas):
              dest.addDotToken()
            else:
              dest.copyTree value.pragmas
            if key == c.currentProc.resultSym:
              dest.copyIntoKind PtrT, info:
                if c.currentProc.canRaise:
                  # what `patchParamList` made the result PARAM; the field
                  # holds that pointer, so the two have to say the same thing
                  addSuccessTupleType(dest, typ, info)
                else:
                  coroTr c, dest, typ
            elif value.typeAsSym != SymId(0):
              dest.addSymUse value.typeAsSym, info
            else:
              coroTr c, dest, typ
            dest.addDotToken() # default value
          programs.publish(value.field, dest, beforeField)
      if c.currentProc.canRaise and c.currentProc.resultSym == SymId(0):
        # A `void` raising coroutine has no `result` local for `escapingLocals`
        # to lift, and still has an `ErrorCode` to hand back, so its slot is a
        # field in its own right. `patchParamList` has already put the pointer
        # into the constructor.
        let beforeField = dest.len
        copyIntoKind dest, FldU, info:
          dest.addSymDef pool.syms.getOrIncl(ResultFieldName), info
          dest.addDotToken() # exported
          dest.addDotToken() # pragmas
          dest.copyIntoKind PtrT, info:
            dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), info
          dest.addDotToken() # default value
        programs.publish(pool.syms.getOrIncl(ResultFieldName), dest, beforeField)
      if c.currentProc.capturedEnvField != SymId(0):
        # The capture slot: erased to `(ref RootObj)` like a closure
        # proc's env, so the frame type doesn't depend on the enclosing
        # proc's env object and the lifter treats it as an ordinary
        # owning ref field (the iter value keeps the env alive).
        let beforeField = dest.len
        copyIntoKind dest, FldU, info:
          dest.addSymDef c.currentProc.capturedEnvField, info
          dest.addDotToken() # exported
          dest.addDotToken() # pragmas
          copyIntoKind dest, RefT, info:
            dest.addSymUse pool.syms.getOrIncl(BareRootObjName), info
          dest.addDotToken() # default value
        programs.publish(c.currentProc.capturedEnvField, dest, beforeField)
  programs.publish(objType, dest, beforeType)

proc emitFreshFrameCall(c: var Context; d: var TokenBuf; sym: SymId; params: Cursor; hasResult: bool; info: NifLineInfo) =
  ## Identical to the original single-branch wrapper body: alloc a
  ## fresh frame, delegate to the iter entry.
  d.copyIntoKind RetS, info:
    d.copyIntoKind ProccallX, info:
      d.addSymUse sym, info
      var p = params
      if p.kind != DotToken:
        p = sub(p) # peek walk, never left
        while p.hasMore:
          assert p.substructureKind == ParamU
          p.into:
            d.addSymUse p.symId, info
            skip p, SkipName # name
            skip p, SkipExport # exported
            skip p, SkipPragmas # pragmas
            skip p, SkipType # type
            skip p, SkipValue # default value
      emitAllocFrame(c, d, sym, info)
      if hasResult:
        d.addSymUse pool.syms.getOrIncl(ResultParamName), info
      d.addSymUse pool.syms.getOrIncl(CallerParamName), info

proc generateCoroutineHelpers*(c: var Context; dest: var TokenBuf; sym: SymId; iter: Cursor) =
  let newSym = coroWrapperProc(c, sym)
  let info = iter.info
  var hasResult = false
  var n: Cursor = iter
  var params: Cursor

  var start = dest.len

  let srcKind = symKind(n)
  if srcKind == IteratorY:
    dest.addParLe ProcS, info
  else:
    # copy the decl head only (classic `takeToken`), then descend
    dest.addParLe(n.cursorTagId, n.info)
  inc n
  skip n         # skip original name
  dest.addSymDef newSym, info
  dest.takeTree n # exported
  dest.takeTree n # pattern
  dest.takeTree n # TypevarsU

  dest.copyIntoKind ParamsU, info:
    params = n
    c.typeCache.openProcScope(newSym, iter, n)
    if n.kind == DotToken:
      inc n
    else:
      n.into:
        while n.hasMore:
          assert n.substructureKind == ParamU
          takeInto dest, n:
            let paramSym = n.symId
            dest.takeTree n # name
            dest.takeTree n # exported
            dest.takeTree n # pragmas
            c.typeCache.registerLocal(paramSym, ParamY, n)
            coroTr c, dest, n # type
            dest.takeTree n # default value
    hasResult = c.currentProc.canRaise or not isVoidType(n)
    if hasResult:
      dest.copyIntoKind ParamU, info:
        dest.addSymDef pool.syms.getOrIncl(ResultParamName), info
        dest.addDotToken() # export
        dest.addDotToken() # pragmas
        dest.copyIntoKind PtrT, info:
          if c.currentProc.canRaise: addSuccessTupleType(dest, n, info)
          else: dest.takeTree n
        dest.addDotToken() # default value
      if c.currentProc.canRaise: skip n
    else:
      skip n
    dest.copyIntoKind ParamU, info:
      dest.addSymDef pool.syms.getOrIncl(CallerParamName), info
      dest.addDotToken() # export
      dest.addDotToken() # pragmas
      dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
      dest.addDotToken() # default value
  dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
  addPragmasWithoutRaises(dest, n, info)
  skip n          # the original pragmas, filtered above
  dest.takeTree n # effects

  publishSignature dest, newSym, start

  let isClosureIter = srcKind == IteratorY and hasPragma(asRoutine(iter).pragmas, ClosureP)

  dest.copyIntoKind StmtsS, info:
    if not isClosureIter:
      emitFreshFrameCall(c, dest, sym, params, hasResult, info)
    else:
      let callerParam = pool.syms.getOrIncl(CallerParamName)
      let envFld = pool.syms.getOrIncl(EnvFieldName)
      let callerFld = pool.syms.getOrIncl(CallerFieldName)
      let fnFld = pool.syms.getOrIncl(FnFieldName)
      let coroSym = coroTypeForProc(c, sym)
      dest.copyIntoKind IfS, info:
        dest.copyIntoKind ElifU, info:
          dest.copyIntoKind EqX, info:
            dest.addParPair PointerT, info
            dest.copyIntoKind DotX, info:
              dest.addSymUse callerParam, info
              dest.addSymUse envFld, info
              dest.addIntLit 0, info # direct field of Continuation
            dest.addParPair NilX, info
          dest.copyIntoKind StmtsS, info:
            emitFreshFrameCall(c, dest, sym, params, hasResult, info)
        dest.copyIntoKind ElseU, info:
          dest.copyIntoKind StmtsS, info:
            let thisLocal = pool.syms.getOrIncl("`thisReuse." & $c.currentProc.counter & "." & c.thisModuleSuffix)
            inc c.currentProc.counter
            dest.copyIntoKind LetS, info:
              dest.addSymDef thisLocal, info
              dest.addDotToken() # exported
              dest.addDotToken() # pragmas
              dest.copyIntoKind PtrT, info:
                dest.addSymUse coroSym, info
              dest.copyIntoKind CastX, info:
                dest.copyIntoKind PtrT, info:
                  dest.addSymUse coroSym, info
                dest.copyIntoKind DotX, info:
                  dest.addSymUse callerParam, info
                  dest.addSymUse envFld, info
                  dest.addIntLit 0, info
            var p = params
            if p.isTagLit:
              p = sub(p)  # throwaway copy; bounds the walk under vpr
              while p.hasMore:
                assert p.substructureKind == ParamU
                p.into:
                  let paramSym = p.symId
                  let field = c.currentProc.localToEnv.getOrDefault(paramSym)
                  if field.field != SymId(0):
                    dest.copyIntoKind AsgnS, info:
                      dest.copyIntoKind DotX, info:
                        dest.copyIntoKind DerefX, info:
                          dest.addSymUse thisLocal, info
                        dest.addSymUse field.field, info
                        dest.addIntLit 0, info
                      dest.addSymUse paramSym, info
                  skip p, SkipName
                  skip p, SkipExport
                  skip p, SkipPragmas
                  skip p, SkipType
                  skip p, SkipValue
            if hasResult:
              dest.copyIntoKind AsgnS, info:
                dest.copyIntoKind DotX, info:
                  dest.copyIntoKind DerefX, info:
                    dest.addSymUse thisLocal, info
                  dest.addSymUse pool.syms.getOrIncl(ResultFieldName), info
                  dest.addIntLit 0, info
                dest.addSymUse pool.syms.getOrIncl(ResultParamName), info
            let calleeFld = pool.syms.getOrIncl(CalleeFieldName)
            dest.copyIntoKind IfS, info:
              dest.copyIntoKind ElifU, info:
                dest.copyIntoKind EqX, info:
                  dest.addParPair PointerT, info
                  dest.copyIntoKind DotX, info:
                    dest.copyIntoKind DerefX, info:
                      dest.addSymUse thisLocal, info
                    dest.addSymUse calleeFld, info
                    dest.addIntLit 1, info # super
                  dest.addParPair NilX, info
                dest.copyIntoKind StmtsS, info:
                  dest.copyIntoKind AsgnS, info:
                    dest.copyIntoKind DotX, info:
                      dest.copyIntoKind DerefX, info:
                        dest.addSymUse thisLocal, info
                      dest.addSymUse calleeFld, info
                      dest.addIntLit 1, info
                    dest.copyIntoKind CastX, info:
                      dest.copyIntoKind PtrT, info:
                        dest.addSymUse pool.syms.getOrIncl(RootObjName), info
                      dest.addSymUse thisLocal, info
                  dest.copyIntoKind AsgnS, info:
                    dest.copyIntoKind DotX, info:
                      dest.copyIntoKind DotX, info:
                        dest.copyIntoKind DerefX, info:
                          dest.addSymUse thisLocal, info
                        dest.addSymUse callerFld, info
                        dest.addIntLit 1, info
                      dest.addSymUse envFld, info
                      dest.addIntLit 0, info
                    dest.copyIntoKind CastX, info:
                      dest.copyIntoKind PtrT, info:
                        dest.addSymUse pool.syms.getOrIncl(RootObjName), info
                      dest.addSymUse thisLocal, info
                  dest.copyIntoKind RetS, info:
                    dest.copyIntoKind OconstrX, info:
                      dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
                      dest.copyIntoKind KvU, info:
                        dest.addSymUse fnFld, info
                        dest.copyIntoKind CastX, info:
                          dest.copyTree c.continuationProcImpl
                          dest.addSymUse stateToProcName(c, sym, 0), info
                      dest.copyIntoKind KvU, info:
                        dest.addSymUse envFld, info
                        dest.copyIntoKind CastX, info:
                          dest.copyIntoKind PtrT, info:
                            dest.addSymUse pool.syms.getOrIncl(RootObjName), info
                          dest.addSymUse thisLocal, info
            dest.copyIntoKind RetS, info:
              dest.copyIntoKind OconstrX, info:
                dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
                dest.copyIntoKind KvU, info:
                  dest.addSymUse fnFld, info
                  dest.copyIntoKind DotX, info:
                    dest.copyIntoKind DotX, info:
                      dest.copyIntoKind DerefX, info:
                        dest.addSymUse thisLocal, info
                      dest.addSymUse callerFld, info
                      dest.addIntLit 1, info # super
                    dest.addSymUse fnFld, info
                    dest.addIntLit 0, info
                dest.copyIntoKind KvU, info:
                  dest.addSymUse envFld, info
                  dest.copyIntoKind CastX, info:
                    dest.copyIntoKind PtrT, info:
                      dest.addSymUse pool.syms.getOrIncl(RootObjName), info
                    dest.addSymUse thisLocal, info
  dest.addParRi() # ProcS

  c.typeCache.closeScope()

proc registerParamsInTypecache*(c: var Context; sym: SymId; origParams: Cursor) =
  var n = origParams
  if n.isTagLit:
    n = sub(n)  # throwaway copy; bounds the walk under vpr
    while n.hasMore:
      assert n.substructureKind == ParamU
      n.into:
        let paramSym = n.symId
        skip n, SkipName # name
        skip n, SkipExport # exported
        skip n, SkipPragmas # pragmas
        c.typeCache.registerLocal(paramSym, ParamY, n)
        skip n, SkipType # type
        skip n, SkipValue # default value

proc patchParamList*(c: var Context; dest, init: var TokenBuf; sym: SymId;
                     paramsBegin, paramsEnd: int; origParams: Cursor) =
  let info = readonlyCursorAt(dest, paramsBegin).info
  var retType = createTokenBuf(4)
  # balanced span: raw copy keeps its seals
  for i in paramsEnd..<dest.len: retType.add dest[i]
  c.currentProc.resultIsTuple = c.currentProc.canRaise and
                                not isVoidType(beginRead(retType))
  if c.currentProc.canRaise:
    # The error leaves through the RESULT slot, not through a return value, so
    # even a `void` raising coroutine gets one — it has an `ErrorCode` to hand
    # back. This is also what makes the caller's temp and this slot the same
    # type; see `frameFieldType`.
    var t = createTokenBuf(8)
    addSuccessTupleType(t, beginRead(retType), info)
    retType = ensureMove t

  dest.shrink paramsBegin
  let thisParam = pool.syms.getOrIncl(EnvParamName)
  dest.copyIntoKind ParamsU, info:
    init.addParLe AsgnS, info
    init.copyIntoKind DerefX, info:
      init.addSymUse thisParam, info
    init.addParLe OconstrX, info
    init.addSymUse coroTypeForProc(c, sym), info
    var n = origParams
    if n.kind != DotToken:
      n = sub(n) # peek walk, never left
      while n.hasMore:
        assert n.substructureKind == ParamU
        var field = SymId(0)
        var paramSym = SymId(0)
        takeInto dest, n:
          paramSym = n.symId
          dest.takeTree n # name
          dest.takeTree n # exported
          let pragmas = n
          dest.takeTree n # pragmas
          field = localToFieldname(c, paramSym)
          c.currentProc.localToEnv[paramSym] = EnvField(
            objType: coroTypeForProc(c, sym),
            field: field,
            pragmas: pragmas,
            typ: n,
            def: -1,
            use: 0)
          c.typeCache.registerLocal(paramSym, ParamY, n)
          c.hooks.trProctype(c, dest, n) # type
          dest.takeTree n # default value

        c.currentProc.constrFields.incl field
        init.copyIntoKind KvU, info:
          init.addSymUse field, info
          init.addSymUse paramSym, info

    dest.copyIntoKind ParamU, info:
      dest.addSymDef thisParam, info
      dest.addDotToken() # export
      dest.addDotToken() # pragmas
      dest.copyIntoKind PtrT, info:
        dest.addSymUse coroTypeForProc(c, sym), info
      dest.addDotToken() # default value

    n = beginRead(retType)
    if not isVoidType(n):
      dest.copyIntoKind ParamU, info:
        dest.addSymDef pool.syms.getOrIncl(ResultParamName), info
        dest.addDotToken() # export
        dest.addDotToken() # pragmas
        dest.copyIntoKind PtrT, info:
          dest.copyTree retType
        dest.addDotToken() # default value
      c.currentProc.constrFields.incl pool.syms.getOrIncl(ResultFieldName)
      init.copyIntoKind KvU, info:
        init.addSymUse pool.syms.getOrIncl(ResultFieldName), info
        init.addSymUse pool.syms.getOrIncl(ResultParamName), info
    dest.copyIntoKind ParamU, info:
      dest.addSymDef pool.syms.getOrIncl(CallerParamName), info
      dest.addDotToken() # export
      dest.addDotToken() # pragmas
      dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
      dest.addDotToken() # default value
    init.copyIntoKind KvU, info:
      init.addSymUse pool.syms.getOrIncl(CallerFieldName), info
      init.addSymUse pool.syms.getOrIncl(CallerParamName), info
      init.addIntLit 1, info # field is in superclass
    init.copyIntoKind KvU, info:
      init.addSymUse pool.syms.getOrIncl(CalleeFieldName), info
      init.copyIntoKind CastX, info:
        init.copyIntoKind PtrT, info:
          init.addSymUse pool.syms.getOrIncl(RootObjName), info
        init.addSymUse thisParam, info
      init.addIntLit 1, info # field is in superclass

  # The `oconstr` and the `asgn` around it stay OPEN: the frame's local
  # fields are not known yet — `escapingLocals` discovers them while the
  # body is transformed — and an `oconstr` may not be closed before every
  # field of the type is in it. `completeFrameConstr` finishes both.
  dest.addSymUse pool.syms.getOrIncl(ContinuationName), info

# ---------------------------------------------------------------------
# Top-level coroutine decl transformer.
#
# Walks a single `(proc|iterator :sym ...)` decl. If the pragmas mark it
# as a coroutine (`.passive` proc, `.passive` iter, or `.closure` iter),
# runs the full state-machine pipeline:
#
#   1. patchParamList   — rewrite signature (this, result, caller params).
#   2. treIteratorBody  — NJ-eliminate jumps, split body into state procs.
#   3. emitFinalReturn  — terminating return after the body.
#   4. generateCoroutineType    — the coro frame type.
#   5. generateCoroutineHelpers — the init wrapper.
#
# Otherwise: passes through unchanged.
#
# Both `cps.nim` (for `.passive`) and `lambdalifting.nim` (for
# `.closure` iters) drive this proc — installed on `Hooks.trCoroutine`,
# or called directly with the appropriate hooks.
# ---------------------------------------------------------------------

proc transformCoroutineDecl*(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind =
    if n.stmtKind == IteratorS: IteratorY
    else: NoSym
  var currentProc = ProcContext(kind: IsNormal)
  swap(c.currentProc, currentProc)
  # Take delivery of the capture slot our consumer announced for exactly
  # this decl and clear the inbox — a following non-capturing coroutine
  # must not inherit it.
  c.currentProc.capturedEnvField = c.pendingCapturedEnvField
  c.pendingCapturedEnvField = SymId(0)
  var init = createTokenBuf(20)
  let iter = n
  var paramsEnd = -1
  var paramsBegin = -1
  var origParams = default(Cursor)
  let procStart = dest.len # position of the head (addParLe may also emit a
                           # line-info suffix, so `dest.len - 1` would be off)
  dest.addParLe(n.cursorTagId, n.info) # ProcS etc.
  let procScopeStart = n
  n = sub(n)
  var isConcrete = true # assume it is concrete
  let sym = n.symId
  c.procStack.add(sym)
  var isCoroutine = false
  for i in 0..<BodyPos:
    if i == ParamsPos:
      origParams = n
      c.typeCache.openProcScope(sym, iter, n)
      paramsBegin = dest.len
    elif i == ReturnTypePos:
      paramsEnd = dest.len
    elif i == ProcPragmasPos:
      if (kind == IteratorY and hasPragma(n, ClosureP)) or hasPragma(n, PassiveP):
        isCoroutine = true
        c.currentProc.canRaise = hasPragma(n, RaisesP)
        c.currentProc.kind = (if kind == IteratorY: IsIterator else: IsPassive)
        c.currentProc.isClosureIter = kind == IteratorY and hasPragma(n, ClosureP)
        # Only concrete coroutines get lowered: their signature gets
        # the state-machine wrapper params and their tag becomes
        # `proc`. Generic templates pass through unchanged — only
        # their instances are coroutine-transformed.
        if isConcrete:
          if kind == IteratorY:
            # retag in place: `parLeToken` would reset an already-set jump
            setTagAt(dest, procStart, cast[TagId](ProcS))
          patchParamList c, dest, init, sym, paramsBegin, paramsEnd, origParams
      if isCoroutine and isConcrete and c.currentProc.canRaise:
        addPragmasWithoutRaises(dest, n, n.info)
        skip n
        continue
    elif i == TypevarsPos:
      isConcrete = n.substructureKind != TypevarsU
    # function declaration can have (delay) tag inside but it just
    # needs to change proctypes
    c.hooks.trProctype(c, dest, n)

  if isConcrete and isCoroutine:
    c.shouldPublish.add (sym: sym, start: procStart)
    treIteratorBody(c, dest, init, iter, sym)
    skip n # we used the body from the control flow graph
    # Emit implicit final return: deallocFrame + return caller
    emitFinalReturn(c, dest, NoLineInfo)
    dest.addParRi() # stmts
  elif isConcrete:
    registerParamsInTypecache(c, sym, origParams)
    if not n.isTagLit:
      dest.addSubtree n
      inc n
    else:
      coroTrSons(c, dest, n)
  else:
    takeTree dest, n
  dest.addParRi(n.endInfo) # ProcS
  n = procScopeStart; skip n
  discard c.procStack.pop()
  c.typeCache.closeScope()
  if isCoroutine and isConcrete:
    var coroTypes = move c.coroTypes
    generateCoroutineType(c, coroTypes, sym)
    c.coroTypes = move coroTypes
    generateCoroutineHelpers(c, dest, sym, iter)
  swap(c.currentProc, currentProc)

# ---------------------------------------------------------------------
# Body-walking dispatcher — recursive `tr`
# ---------------------------------------------------------------------

proc coroTr*(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of DotToken, EofToken, Ident, SymbolDef,
     IntLit, UIntLit, FloatLit, CharLit, StrLit:
    takeTree dest, n
  of Symbol:
    if isProc(c, n.symId) and c.hooks.isPassiveProc(c, n.symId):
      dest.addSymUse coroWrapperProc(c, n.symId), n.info
      inc n
    elif isClosureIter(n.symId):
      # Post-lambdalifting an iter sym only ever appears in the fn slot
      # of a lambdalifting-emitted iter-value tupconstr. Rewrite to the
      # wrapper sym now that cps is about to generate it.
      dest.addSymUse coroWrapperProc(c, n.symId), n.info
      inc n
    else:
      let field = c.currentProc.localToEnv.getOrDefault(n.symId)
      if field.def != field.use or n.symId == c.currentProc.resultSym:
        let info = n.info
        let isResult = n.symId == c.currentProc.resultSym
        # A raising coroutine's result slot is `(ErrorCode, T)`, so `result`
        # names its second half — the same projection `constparams` gives the
        # `result` of an ordinary `.raises` routine.
        # A raising call's temp is a `(ErrorCode, T)` once it is a frame
        # field, and an ordinary use of it means the value. `constparams` does
        # this for a local it can still see declared; it cannot reach a field.
        let projectValue = (isResult and c.currentProc.resultIsTuple) or
          (not isResult and n.symId in c.currentProc.tupleVars and
           not isVoidType(getType(c.typeCache, n)))
        if projectValue:
          dest.addParLe TupatX, info
        if isResult:
          dest.addParLe DerefX, info
        dest.copyIntoKind DotX, info:
          dest.copyIntoKind DerefX, info:
            dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
          dest.addSymUse field.field, info
        if isResult:
          dest.addParRi()
        if projectValue:
          dest.addIntLit 1, info
          dest.addParRi()
        inc n
      else:
        takeTree dest, n
  of UnknownToken:
    takeTree dest, n
  of TagLit:
    if n.exprKind in {FailedX, TupatX}:
      # Both address a raising temp's halves directly — `(failed t)` asks for
      # its code, `(tupat t i)` was put there by `emitErrorCodeOf`. Walking
      # into them normally would apply the value projection above on top.
      dest.addParLe(n.cursorTagId, n.info)
      n.into:
        if n.kind == Symbol and n.symId in c.currentProc.tupleVars:
          let field = c.currentProc.localToEnv.getOrDefault(n.symId, EnvField(def: -2))
          if field.def != -2 and field.def != field.use:
            dest.copyIntoKind DotX, n.info:
              dest.copyIntoKind DerefX, n.info:
                dest.addSymUse pool.syms.getOrIncl(EnvParamName), n.info
              dest.addSymUse field.field, n.info
            inc n
          else:
            takeTree dest, n
        else:
          coroTr c, dest, n
        while n.hasMore: coroTr c, dest, n
      dest.addParRi()
      return
    case n.stmtKind
    of LocalDecls - {ResultS}:
      trLocal c, dest, n
    of ResultS:
      if c.currentProc.kind == IsNormal:
        trLocal c, dest, n
      else:
        skip n
    of ProcS, FuncS, MethodS, ConverterS, IteratorS:
      c.hooks.trCoroutine(c, dest, n)
    of TypeS:
      let typeStart = dest.len
      var typeSym = SymId(0)
      takeInto dest, n: # TypeS tag
        if n.kind == SymbolDef:
          typeSym = n.symId
        takeTree dest, n # name
        takeTree dest, n # exported
        takeTree dest, n # typevars
        takeTree dest, n # pragmas
        c.hooks.trProctype(c, dest, n) # body
      if typeSym != SymId(0):
        programs.publish(typeSym, dest, typeStart)
    of MacroS, TemplateS, EmitS, BreakS, ContinueS,
      ForS, IncludeS, ImportS, FromimportS, ImportexceptS,
      ExportS, CommentS,
      PragmasS:
      takeTree dest, n
    of YldS:
      trYield c, dest, n
    of RetS, RaiseS:
      if c.currentProc.kind == IsNormal:
        coroTrSons(c, dest, n)
      else:
        trReturn c, dest, n
    of AsgnS:
      trAsgn c, dest, n
    of ScopeS:
      c.typeCache.openScope()
      coroTrSons(c, dest, n)
      c.typeCache.closeScope()
    of CoroforS:
      trCoroFor c, dest, n
    of CallS, CmdS, BlockS, IfS, WhenS, WhileS, CaseS,
        StmtsS, PragmaxS, InclS, ExclS, ImportasS,
        ExportexceptS, DiscardS, TryS, UnpackdeclS,
        AssumeS, AssertS, CallstrlitS, InfixS, PrefixS,
        HcallS, StaticstmtS, BindS, MixinS, UsingS,
        AsmS, DeferS, LabS, JmpS, NoStmt:
      case n.exprKind
      of CallKinds - {DelayX}:
        trCall c, dest, n
      of DelayX:
        c.hooks.trDelay(c, dest, n)
      of Delay0X:
        c.hooks.trDelay0(c, dest, n)
      of SuspendX:
        c.hooks.trSuspend(c, dest, n)
      of TypeofX:
        takeTree dest, n
      of HconvX, ConvX:
        # `g == nil` / `g != nil` over a closure / iter value: sem
        # emits `(hconv (pointer (nil)) g)` so NIFC can compare via a
        # pointer cast. For a `.closure` iter / closure proc the value is
        # a `(closureTuple <proctype> (ref RootObj))`, so cast-to-pointer no
        # longer typechecks — peel off the fn-slot via tupat and convert
        # that scalar instead. A `.passive` iter value is a bare wrapper
        # proctype (no tuple, see `trProctype`), so it converts to
        # pointer directly and must NOT be field-extracted.
        let info = n.info
        let tag = n.exprKind
        var inner = n
        let convStart = inner # past tag
        inner = sub(inner)
        var dstType = inner
        skip inner           # past target type
        if inner.kind == Symbol or inner.exprKind in {TupatX, DotX}:
          let srcTyp = c.typeCache.getType(inner, {SkipAliases})
          let isFnEnvTuple = srcTyp.typeKind == ClosureTupleT
          let isClosureIterType = srcTyp.typeKind == ItertypeT and
              not procHasPragma(srcTyp, PassiveP)
          if (isClosureIterType or isFnEnvTuple) and
              dstType.typeKind in {PtrT, PointerT}:
            dest.addParLe tag, info
            dest.takeTree dstType
            dest.copyIntoKind TupatX, info:
              takeTree dest, inner
              dest.addIntLit 0, info
            dest.addParRi()
            n = inner
            n = convStart; skip n
          else:
            coroTrSons c, dest, n
        else:
          coroTrSons c, dest, n
      of DotX, DdotX:
        # The selector is a field identity, not a value use. It can share a
        # SymId with a lifted local or parameter of the same spelling; running
        # it through `coroTr` would replace the field name with an environment
        # access and produce malformed `(dot obj (dot ...))` NIF.
        takeInto dest, n:
          coroTr c, dest, n
          takeTree dest, n # field
          if n.hasMore: takeTree dest, n # optional inheritance depth
          if n.hasMore: takeTree dest, n # optional private-access token
      of ErrX, SufX, AtX, DerefX, PatX, ParX,
          AddrX, NilX, InfX, NeginfX, NanX, FalseX,
          TrueX, AndX, OrX, XorX, NotX, NegX, SizeofX,
          AlignofX, OffsetofX, OconstrX, AconstrX,
          BracketX, CurlyX, CurlyatX, OvfX, AddX, SubX,
          MulX, DivX, ModX, ShrX, ShlX, BitandX, BitorX,
          BitxorX, BitnotX, EqX, NeqX, LeX, LtX, CastX,
          CchoiceX, OchoiceX, PragmaxX, QuotedX,
          HderefX, HaddrX, NewrefX, NewobjX,
          TupX, TupconstrX, SetconstrX, TabconstrX,
          AshrX, BaseobjX, DconvX, CompilesX,
          DeclaredX, DefinedX, AstToStrX, BindSymX, BindSymNameX, InstanceofX,
          HighX, LowX, UnpackX, FieldsX, FieldpairsX,
          EnumtostrX, IsmainmoduleX, DefaultobjX,
          DefaulttupX, DefaultdistinctX, ExprX, DoX,
          ArratX, TupatX, PlussetX, MinussetX, MulsetX,
          XorsetX, EqsetX, LesetX, LtsetX, InsetX,
          CardX, EmoveX, DestroyX, DupX, CopyX,
          WasmovedX, SinkhX, TraceX,
          InternalTypeNameX, InternalFieldPairsX,
          FailedX, IsX, EnvpX, KvX, ToClosureX, NoExpr:
        case n.finalIrKind
        of LoopV:
          # A suspension-free Final IR `(loop (stmts BODY (continue .)))`
          # survives into the state proc as an ordinary `while true`. The
          # back-edge marker is the loop's own tail, so the LAST `(continue .)`
          # is simply dropped; an earlier one is a source-level `continue` and
          # becomes a forward `jmp` to a `(lab)` closing the body — `lengcgen`
          # rejects `ContinueS`, and the Final IR's own label/jump pair is
          # exactly the construct that expresses it.
          var bodyBuf = createTokenBuf(64)
          var info = n.info
          let contLab = pool.syms.getOrIncl("´cont." & $c.currentProc.counter &
                                            "." & c.thisModuleSuffix)
          inc c.currentProc.counter
          var lastJmp = -1
          var jumps = 0
          n.into:                                 # (loop ...)
            assert n.stmtKind == StmtsS, $n.kind
            n.into:                               # the body
              while n.hasMore:
                if n.finalIrKind == ContinueV:
                  lastJmp = bodyBuf.len
                  inc jumps
                  bodyBuf.copyIntoKind JmpS, n.info:
                    bodyBuf.addSymUse contLab, n.info
                  skip n
                else:
                  lastJmp = -1
                  coroTr c, bodyBuf, n
          if lastJmp >= 0:
            # the trailing back-edge marker: falling off the body does it
            bodyBuf.shrink lastJmp
            dec jumps
          dest.addParLe WhileS, info
          dest.addParPair TrueX, info
          dest.copyIntoKind StmtsS, info:
            dest.add bodyBuf
            if jumps > 0:
              dest.copyIntoKind LabS, info:
                dest.addSymDef contLab, info
          dest.addParRi()
        of IteV, ItecV:
          # `(ite cond then else)`, where `else` may be a bare `.`. Any further
          # child is drained rather than left for the outer loop, which would
          # otherwise stop at the stray token and drop the following siblings.
          var info = n.info
          n.into:
            dest.copyIntoKind IfS, info:
              dest.copyIntoKind ElifU, info:
                coroTr c, dest, n
                coroTr c, dest, n
              dest.copyIntoKind ElseU, info:
                coroTr c, dest, n
            while n.hasMore:
              skip n
        of MflagV, VflagV:
          # NJVL control-flow flags; nothing produces them since `xelim`'s
          # cfvar lowering went out with `nj.nim`. See `finalir.trStmt`.
          bug "cfvar in Final IR input"
        of StoreV:
          # (store value dest) -> (asgn dest value)
          let info = n.info
          n.into: # skip 'store' tag
            var value = n
            if c.hooks.isPassiveCall(c, value):
              skip n
              var lhsTransformed = createTokenBuf(6)
              coroTr c, lhsTransformed, n
              c.hooks.trPassiveCall(c, dest, value, beginRead lhsTransformed)
            else:
              var valueBuf = createTokenBuf(16)
              coroTr c, valueBuf, n # value (first operand)
              dest.copyIntoKind AsgnS, info:
                coroTr c, dest, n   # dest (second operand)
                dest.add valueBuf
        of KillV, UnknownV:
          skip n  # NJ bookkeeping, not needed in CPS output
        else:
          if n.typeKind == ProctypeT:
            c.hooks.trProctype(c, dest, n)
          else:
            case n.stmtKind
            of JmpS:
              # Two different `jmp`s meet here. The CPS state machine's own
              # carries an INTEGER state id (this pass and `togoto` produce
              # it); the structured Nimony one carries a label SYMBOL and is
              # `xelim`'s short-circuit lowering, which must survive to Leng
              # untouched (`doc/final_ir.md`).
              if n.childCursor.kind == IntLit:
                n.into:
                  gotoNextState(c, dest, int(n.intVal), n.info)
                  inc n
              else:
                takeTree dest, n
            of LabS:
              if n.childCursor.kind == IntLit:
                dest.addParRi() # close stmts
                dest.addParRi() # close proc decl
                n.into:
                  newLocalProc c, dest, int(n.intVal), c.procStack[^1]
                  inc n
              else:
                takeTree dest, n
            else:
              # NOT an `elif` branch of the `case` above: nimony's own `case`
              # takes `of` and `else` and nothing else, so an `elif` here is
              # dropped on the floor — which is exactly how this guard, written
              # as one, silently unmade `coroTr` in the self-hosted hexer. See
              # `semCaseImpl`, which now rejects the form instead.
              if n.substructureKind == KvU:
                # `(kv FIELD value)` object-constructor pair: the key is a
                # field identity, not a value use. It can share a SymId with a
                # lifted local of the same spelling — sem's `name.N` numbering
                # makes that ordinary — and running it through `coroTr` would
                # replace the field name with a frame access, leaving lengc to
                # report "expected field name but got: (dot …)". Take the key
                # verbatim and rewrite only the value(s). This is the `DotX` /
                # `DdotX` selector guard above in its other position, and the
                # twin of `lambdalifting`'s `trKv` / `treKv`. A `KvX` table key
                # IS a real value use, so it stays in `coroTrSons`.
                copyInto dest, n:
                  dest.takeTree n # key (field identity — never rewrite)
                  while n.hasMore:
                    coroTr(c, dest, n)
              else:
                coroTrSons(c, dest, n)
  else:
    bug "unexpected ')' inside"
