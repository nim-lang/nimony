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
import ".." / njvl / [nj, njvl_model]
import passes
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
  FnFieldName* = "fn.0"
  EnvFieldName* = "env.0"
  CallerFieldName* = "caller.0"
  CalleeFieldName* = "callee.0"
  ResultParamName* = "`result.0"
  ResultFieldName* = "`result.0"
  CallerParamName* = "`caller.0"
  AllocFrameProcName* = "allocFrame.0." & SystemModuleSuffix
  DeallocFrameProcName* = "deallocFrame.0." & SystemModuleSuffix

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

  ProcContext* = object
    localToEnv*: Table[SymId, EnvField]
    cf*: TokenBuf
    resultSym*: SymId
    counter*: int
    labelCounter*: int = 1
    kind*: RoutineKind
    isClosureIter*: bool
      ## True for `.closure` iters specifically. Drives the resume-slot
      ## writeback at yield sites and the two-branch wrapper body.
      ## `.passive` iters use the factory model and don't set this.

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
    nextTemp*: int
      ## Continues the outer pipeline's xelim temp counter through the
      ## nested per-coroutine njvl runs (treIteratorBody) — restarting at
      ## 0 re-mints `x.N SymIds that collide with still-live outer temps.
    typeCache*: TypeCache
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

proc coroNameStem*(procId: SymId): string =
  ## Returns the symbol's full name minus its trailing module suffix.
  ## Unlike `extractVersionedBasename`, this preserves any intermediate
  ## `I<hash>` segment for generic instances — necessary because two
  ## different instantiations (e.g. `gen.12.Iaaaa.mod` and
  ## `gen.12.Ibbbb.mod`) would otherwise produce identical stem
  ## `"gen.12"` and collide on every synthesised wrapper / coro-type /
  ## state-proc / field name.
  splitSymName(pool.syms[procId]).name

proc coroTypeForProc*(c: Context; procId: SymId): SymId =
  result = pool.syms.getOrIncl(coroNameStem(procId) & ".coro." & c.thisModuleSuffix)

proc coroWrapperProc*(c: Context; procId: SymId): SymId =
  result = pool.syms.getOrIncl(coroNameStem(procId) & ".init." & c.thisModuleSuffix)

proc stateToProcName*(c: Context; sym: SymId; state: int): SymId =
  result = pool.syms.getOrIncl(coroNameStem(sym) & ".s" & $state & "." & c.thisModuleSuffix)

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
  ## Mangle the closure iterator's init-wrapper symbol using the
  ## iterator's OWN module suffix (which may differ from the current
  ## module's). The wrapper is defined in the same module that declared
  ## the iterator. `splitSymName` preserves the `I<hash>` segment for
  ## generic instances so two instantiations don't collide on a single
  ## wrapper.
  let split = splitSymName(pool.syms[iterSym])
  result = pool.syms.getOrIncl(split.name & ".init." & split.module)

proc coroTypeForExternIter*(iterSym: SymId): SymId =
  ## Same idea as `coroWrapperForExternIter` but for the coroutine
  ## frame type — uses the iter's OWN module suffix so cross-module
  ## iter values reference the right `.coro` type.
  let split = splitSymName(pool.syms[iterSym])
  result = pool.syms.getOrIncl(split.name & ".coro." & split.module)

proc publishWrapperSignature*(iterSym: SymId; moduleSuffix: string) =
  ## Publish a placeholder signature for an iter's init wrapper so
  ## downstream passes (eraiser / duplifier / destroyer) can resolve
  ## the wrapper's type via `tryLoadSym` BEFORE cps generates the
  ## actual wrapper body.
  ##
  ## Lambdalifting calls this when it expands a `.closure` iter
  ## corofor into a trampoline that references the wrapper sym. Only
  ## same-module iters need this — cross-module iters already have
  ## their wrapper published by the iter's owning module's compile.
  ##
  ## The published signature shape is the same as what
  ## `generateCoroutineHelpers` emits: original iter params, then
  ## `(param result (ptr T))` if non-void, then
  ## `(param caller Continuation)`, return type `Continuation`,
  ## pragmas `(closure)`. Body is empty (`.`); cps's emission later
  ## overrides this with the real body via `publishSignature` again.
  let split = splitSymName(pool.syms[iterSym])
  if split.module != moduleSuffix:
    return  # foreign iter — wrapper published by its own module
  let wrapperSym = pool.syms.getOrIncl(split.name & ".init." & split.module)
  if tryLoadSym(wrapperSym).status == LacksNothing:
    return  # already published (e.g. by an earlier corofor for the
            # same iter, or by a previous compile)

  let res = tryLoadSym(iterSym)
  assert res.status == LacksNothing, "iter sym not loaded: " & pool.syms[iterSym]
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
    var ret = fn.retType
    if not isVoidType(ret):
      buf.copyIntoKind ParamU, info:
        buf.addSymDef pool.syms.getOrIncl(ResultParamName), info
        buf.addDotToken() # export
        buf.addDotToken() # pragmas
        buf.copyIntoKind PtrT, info:
          buf.takeTree ret
        buf.addDotToken() # default value
    buf.copyIntoKind ParamU, info:
      buf.addSymDef pool.syms.getOrIncl(CallerParamName), info
      buf.addDotToken() # export
      buf.addDotToken() # pragmas
      buf.addSymUse pool.syms.getOrIncl(ContinuationName), info
      buf.addDotToken() # default value
  buf.addSymUse pool.syms.getOrIncl(ContinuationName), info
  buf.copyIntoKind PragmasU, info:
    buf.copyIntoKind ClosureP, info: discard
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

proc emitIterTupleTypeFromParams*(dest: var TokenBuf; n: var Cursor; info: PackedLineInfo) =
  ## Consume an (itertype ...) tree at `n` and emit
  ##   `(tuple (proctype . (params <orig>... (param result ptr T) (param caller Continuation)) Continuation <pragmas>) (ref RootObj))`
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
    dest.copyIntoKind TupleT, info:
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

proc emitIterTupleTypeFromSym*(dest: var TokenBuf; iterSym: SymId; info: PackedLineInfo) =
  ## Build the iter-value tuple type from an iterator sym's decl. Used
  ## at iter-sym-as-value and iter-nil sites where we don't have an
  ## itertype tree on hand.
  let res = tryLoadSym(iterSym)
  assert res.status == LacksNothing, "iter sym not loaded: " & pool.syms[iterSym]
  let fn = asRoutine(res.decl)
  dest.copyIntoKind TupleT, info:
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

proc isLiftedClosureTuple*(n: Cursor): bool =
  ## A `(tuple <proctype …> (ref RootObj))` is the shape both closure
  ## procs and closure-iter values get lifted to. If we encounter one
  ## while walking, it's already lifted — recursing into it would
  ## re-trigger the proctype rewrite and produce nested tuples.
  if n.typeKind != TupleT: return false
  var t = n
  t = sub(t)  # throwaway copy; bounds the probe under vpr
  if t.kind != ParLe or t.typeKind != ProctypeT: return false
  skip t
  if t.kind != ParLe or t.typeKind != RefT: return false
  skip t
  result = not t.hasMore

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
    if pool.tags[buf[pos].tag] == "lab":
      return int(pool.integers[buf[pos+1].intId])
    inc pos
  return -1

proc coroTrSons*(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    while n.hasMore:
      coroTr(c, dest, n)

# ---------------------------------------------------------------------
# IR emitters — operate on (ptr CoroutineBase) frames via `this.0`
# ---------------------------------------------------------------------

proc contNextState*(c: var Context; dest: var TokenBuf; state: int; info: PackedLineInfo) =
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

proc stashResumeFn*(c: var Context; dest: var TokenBuf; state: int; info: PackedLineInfo) =
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

proc emitAllocFrame*(c: var Context; dest: var TokenBuf; calleeSym: SymId; info: PackedLineInfo) =
  ## Emit: cast[ptr CalleeCoroutine](allocFrame(sizeof(CalleeCoroutine)))
  dest.copyIntoKind CastX, info:
    dest.copyIntoKind PtrT, info:
      dest.addSymUse coroTypeForProc(c, calleeSym), info
    dest.copyIntoKind CallX, info:
      dest.addSymUse pool.syms.getOrIncl(AllocFrameProcName), info
      dest.copyIntoKind SizeofX, info:
        dest.addSymUse coroTypeForProc(c, calleeSym), info

proc emitDeallocFrame*(c: var Context; dest: var TokenBuf; info: PackedLineInfo) =
  ## Emit: deallocFrame(cast[ptr CoroutineBase](this))
  dest.copyIntoKind CallS, info:
    dest.addSymUse pool.syms.getOrIncl(DeallocFrameProcName), info
    dest.copyIntoKind CastX, info:
      dest.copyIntoKind PtrT, info:
        dest.addSymUse pool.syms.getOrIncl(RootObjName), info
      dest.addSymUse pool.syms.getOrIncl(EnvParamName), info

proc emitStopContinuation*(dest: var TokenBuf; info: PackedLineInfo) =
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

proc emitFinalReturn*(c: var Context; dest: var TokenBuf; info: PackedLineInfo) =
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

proc emitStackFrameTag*(c: var Context; dest: var TokenBuf; coroVar: SymId; info: PackedLineInfo) =
  ## Emit: coroVar.callee = nil
  ## Marks the frame as stack-allocated so deallocFrame is a nop.
  dest.copyIntoKind AsgnS, info:
    dest.copyIntoKind DotX, info:
      dest.addSymUse coroVar, info
      dest.addSymUse pool.syms.getOrIncl(CalleeFieldName), info
      dest.addIntLit 1, info # field is in superclass
    dest.addParPair NilX, info

proc emitItEnv(dest: var TokenBuf; info: PackedLineInfo;
               itSym, envFieldSym: SymId) =
  dest.copyIntoKind DotX, info:
    dest.addSymUse itSym, info
    dest.addSymUse envFieldSym, info
    dest.addIntLit 0, info # direct field of Continuation

proc emitWhileBegin*(dest: var TokenBuf; info: PackedLineInfo;
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

proc emitWhileEnd*(dest: var TokenBuf; info: PackedLineInfo; itSym: SymId) =
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
  let fn = n.firstSon
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
  var rhs = n.firstSon
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
  let sym = n.firstSon.symId
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

proc declareContinuationResult*(c: var Context; dest: var TokenBuf; info: PackedLineInfo) =
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

proc gotoNextState*(c: var Context; dest: var TokenBuf; state: int; info: PackedLineInfo) =
  # generate: `return state(this)`
  dest.copyIntoKind RetS, info:
    dest.copyIntoKind CallS, info:
      dest.addSymUse stateToProcName(c, c.procStack[^1], state), info
      dest.addSymUse pool.syms.getOrIncl(EnvParamName), info

proc returnValue*(c: var Context; dest: var TokenBuf; n: var Cursor; info: PackedLineInfo) =
  n.into: # yield/return
    if n.kind == DotToken or (n.kind == Symbol and n.symId == c.currentProc.resultSym):
      inc n
    elif isVoidType(getType(c.typeCache, n)) and n.kind != Symbol:
      # void type for Symbol can happen for `raise` statements:
      coroTr c, dest, n
    else:
      dest.copyIntoKind AsgnS, info:
        dest.copyIntoKind DerefX, info:
          dest.copyIntoKind DotX, info:
            dest.copyIntoKind DerefX, info:
              dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
            dest.addSymUse pool.syms.getOrIncl(ResultFieldName), info
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
  # var tmpCaller = this.caller; deallocFrame(this); return/raise tmpCaller
  let head = n.load()
  let info = head.info
  returnValue(c, dest, n, info)
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
  dest.add head
  dest.addSymUse tmpVar, info
  dest.addParRi()

# ---------------------------------------------------------------------
# Lifetime + state analysis
# ---------------------------------------------------------------------

proc escapingLocalsImpl(c: var Context; n: var Cursor; currentState: var int) =
  ## Processes the single tree/token at `n`, advancing past it.
  if n.kind == ParLe and pool.tags[n.tag] == "lab":
    currentState = int(pool.integers[n.firstSon.intId])

  let sk = n.stmtKind
  let nk = n.njvlKind
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
        typ: n,
        def: currentState,
        use: currentState)
      skip n # type
      while n.hasMore:
        escapingLocalsImpl c, n, currentState # the value
  else:
    if nk in {MflagV, VflagV}:
      # NJ guard flags are bool variables that may cross state boundaries
      n.into: # mflag/vflag tag
        let mine = n.symId
        c.currentProc.localToEnv[mine] = EnvField(
          objType: coroTypeForProc(c, c.procStack[^1]),
          field: localToFieldname(c, mine),
          typ: c.typeCache.builtins.boolType,
          def: currentState,
          use: currentState)
        skip n # symdef
    else:
      case n.kind
      of ParLe:
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
  if n.kind == DotToken: return
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

proc trMflag*(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Convert NJ `(mflag symdef)` / `(vflag symdef)` to a bool var
  ## initialized to false. If the flag is lifted to the environment,
  ## emit an assignment to the env field instead.
  let info = n.info
  let flagStart = n  # past mflag/vflag tag
  n = sub(n)
  let symDef = n
  let symId = n.symId
  inc n  # skip symdef
  n = flagStart; skip n  # past the (elided) ParRi
  let field = c.currentProc.localToEnv.getOrDefault(symId)
  if field.def != field.use:
    dest.copyIntoKind AsgnS, info:
      dest.copyIntoKind DotX, info:
        dest.copyIntoKind DerefX, info:
          dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
        dest.addSymUse field.field, info
      dest.addParPair FalseX, info
  else:
    dest.addParLe VarS, info
    dest.add symDef
    dest.addDotToken()  # exported
    dest.addDotToken()  # pragmas
    dest.copyTree c.typeCache.builtins.boolType
    dest.addParPair FalseX, info  # initialized to false
    dest.addParRi()

proc trJtrue*(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Convert NJ `(jtrue sym...)` to `(asgn sym true)` for each symbol.
  let info = n.info
  n.into:
    while n.hasMore:
      let symId = n.symId
      let field = c.currentProc.localToEnv.getOrDefault(symId)
      dest.addParLe AsgnS, info
      if field.def != field.use:
        dest.copyIntoKind DotX, info:
          dest.copyIntoKind DerefX, info:
            dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
          dest.addSymUse field.field, info
      else:
        dest.add n
      dest.addParPair TrueX, info
      dest.addParRi()
      skip n

proc emitJump*(dest: var TokenBuf; label: int; info: PackedLineInfo) =
  dest.add tagToken("jmp", info)
  dest.addIntLit label, info
  dest.addParRi()

proc emitLabel*(dest: var TokenBuf; label: int; info: PackedLineInfo) =
  dest.add tagToken("lab", info)
  dest.addIntLit label, info
  dest.addParRi()

proc trGoto*(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var info = n.info
  case n.njvlKind
  of ContinueV:
    skip n
  of StoreV:
    var addLabel = false
    takeInto dest, n:
      addLabel = c.hooks.isPassiveCall(c, n)
      dest.takeTree n
      dest.takeTree n
    if addLabel:
      emitLabel dest, c.currentProc.labelCounter, info
      inc c.currentProc.labelCounter
  of LoopV:
    if containsSuspensionPoint(c, n):
      var beforeLoopState = c.currentProc.labelCounter
      inc c.currentProc.labelCounter
      var afterLoopState = c.currentProc.labelCounter
      inc c.currentProc.labelCounter
      n.into:                                       # (loop ...)
        assert n.stmtKind == StmtsS
        var preludeBuf = createTokenBuf(16)
        n.into:                                     # stmts_before
          while n.hasMore and n.njvlKind in {MflagV, VflagV}:
            dest.takeTree n                         # flag decls: once, above the loop
          while n.hasMore:                          # rotated prelude: lowered, spliced below
            trGoto c, preludeBuf, n
        emitJump dest, beforeLoopState, info
        emitLabel dest, beforeLoopState, info
        dest.copyIntoKind IfS, info:                # loop-continue test (cond first)
          dest.copyIntoKind ElifU, info:
            dest.copyIntoKind NotX, info:
              dest.takeTree n
            dest.copyIntoKind StmtsS, info:
              emitJump dest, afterLoopState, info
        dest.add preludeBuf                         # prelude: each iteration, after the test
        assert n.stmtKind == StmtsS
        n.into:                                     # stmts_body
          while n.hasMore:
            trGoto c, dest, n
        emitJump dest, beforeLoopState, info
        emitLabel dest, afterLoopState, info
    else:
      dest.takeTree n
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
            dest.takeTree n # cond
            dest.copyIntoKind StmtsS, info:
              emitJump dest, lthen, info
        var thenCur = n
        skip n
        var elseCur = n
        if n.hasMore: skip n
        # Else-branch presence: in NJVL the missing-else case can present as
        # either a `DotToken` placeholder *or* the scope's close (when the
        # `ite` was emitted with the else slot elided rather than explicitly
        # filled with `.`). Treating the scope end as "no else" prevents
        # `elseCur.into:` from asserting on a non-ParLe cursor.
        if elseCur.hasMore and elseCur.kind == ParLe:
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
      dest.takeTree n
  else:
    let sk = n.stmtKind
    let ek = n.exprKind
    if sk == YldS or c.hooks.isPassiveCall(c, n) or ek == SuspendX:
      takeTree dest, n
      emitLabel dest, c.currentProc.labelCounter, info
      inc c.currentProc.labelCounter
    else:
      case n.kind
      of ParLe:
        case n.stmtKind
        of LocalDecls - {ResultS}:
          let sym = n.firstSon.symId
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
            dest.takeTree n
          if addLabel:
            emitLabel dest, c.currentProc.labelCounter, info
            inc c.currentProc.labelCounter
        of CallS, CmdS, ResultS, ProcS, FuncS, IteratorS,
            ConverterS, MethodS, MacroS, TemplateS, TypeS,
            BlockS, EmitS, AsgnS, ScopeS, IfS, WhenS,
            BreakS, ContinueS, ForS, WhileS, CoroforS, CaseS,
            RetS, YldS, StmtsS, PragmasS, PragmaxS, InclS, ExclS,
            IncludeS, ImportS, ImportasS, FromimportS,
            ImportexceptS, ExportS, ExportexceptS, CommentS,
            DiscardS, TryS, RaiseS, UnpackdeclS, AssumeS,
            AssertS, CallstrlitS, InfixS, PrefixS, HcallS,
            StaticstmtS, BindS, MixinS, UsingS, AsmS,
            DeferS, NoStmt:
          dest.add n.load()
          n.into:
            while n.hasMore:
              trGoto c, dest, n
          dest.addParRi()
      else:
        dest.takeToken n

proc toGoto*(c: var Context; n: Cursor): TokenBuf =
  result = createTokenBuf(300)
  assert n.stmtKind == StmtsS, $n.kind
  var n = n
  trGoto(c, result, n)

# ---------------------------------------------------------------------
# Body lowering — produce the state-machine procs for a single
# coroutine routine
# ---------------------------------------------------------------------

proc treIteratorBody*(c: var Context; dest: var TokenBuf; init: TokenBuf; iter: Cursor; sym: SymId) =
  # Transform the proc body via the NJ pass to get structured code
  # without break/continue/goto, then store just the body (without NJ
  # bookkeeping variables like mflag/jtrue/kill) in c.currentProc.cf.
  var wrapper = createTokenBuf(10)
  wrapper.addParLe StmtsS, NoLineInfo
  wrapper.copyTree iter
  wrapper.addParRi()
  var pass = initPass(ensureMove wrapper, c.thisModuleSuffix, "eliminateJumps", 0,
                      nextTemp = c.nextTemp)
  eliminateJumps(pass, raisesResolved = true)
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

  c.currentProc.cf = toGoto(c, beginRead(c.currentProc.cf))
  when defined(logPasses):
    echo "========= GOTO ======="
    echo c.currentProc.cf.toString(false)
    echo ""

  var n = beginRead(c.currentProc.cf)
  escapingLocals(c, n)

  assert n.stmtKind == StmtsS
  dest.add n
  n.into:
    dest.add init
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
                coroTr c, dest, typ
            elif value.typeAsSym != SymId(0):
              dest.addSymUse value.typeAsSym, info
            else:
              coroTr c, dest, typ
            dest.addDotToken() # default value
          programs.publish(value.field, dest, beforeField)
  programs.publish(objType, dest, beforeType)

proc emitFreshFrameCall(c: var Context; d: var TokenBuf; sym: SymId; params: Cursor; hasResult: bool; info: PackedLineInfo) =
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
    inc n
  else:
    dest.takeToken n
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
    hasResult = not isVoidType(n)
    if hasResult:
      dest.copyIntoKind ParamU, info:
        dest.addSymDef pool.syms.getOrIncl(ResultParamName), info
        dest.addDotToken() # export
        dest.addDotToken() # pragmas
        dest.copyIntoKind PtrT, info:
          dest.takeTree n
        dest.addDotToken() # default value
    else:
      skip n
    dest.copyIntoKind ParamU, info:
      dest.addSymDef pool.syms.getOrIncl(CallerParamName), info
      dest.addDotToken() # export
      dest.addDotToken() # pragmas
      dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
      dest.addDotToken() # default value
  dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
  dest.takeTree n
  dest.takeTree n

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
            if p.kind == ParLe:
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
  if n.kind == ParLe:
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
  let info = dest[paramsBegin].info
  var retType = createTokenBuf(4)
  # balanced span: raw copy keeps its seals
  for i in paramsEnd..<dest.len: retType.addRaw dest[i]

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

  init.addParRi() # object constructor
  init.addParRi() # assignment

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
  var init = createTokenBuf(20)
  let iter = n
  var paramsEnd = -1
  var paramsBegin = -1
  var origParams = default(Cursor)
  dest.add n # ProcS etc.
  let procScopeStart = n
  n = sub(n)
  let procStart = dest.len - 1
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
        c.currentProc.kind = (if kind == IteratorY: IsIterator else: IsPassive)
        c.currentProc.isClosureIter = kind == IteratorY and hasPragma(n, ClosureP)
        # Only concrete coroutines get lowered: their signature gets
        # the state-machine wrapper params and their tag becomes
        # `proc`. Generic templates pass through unchanged — only
        # their instances are coroutine-transformed.
        if isConcrete:
          if kind == IteratorY:
            # retag in place: `parLeToken` would reset an already-set jump
            dest[procStart].setTag cast[TagId](ProcS)
          patchParamList c, dest, init, sym, paramsBegin, paramsEnd, origParams
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
    if n.kind != ParLe:
      dest.add n
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
     IntLit, UIntLit, FloatLit, CharLit, StringLit:
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
        if isResult:
          dest.addParLe DerefX, info
        dest.copyIntoKind DotX, info:
          dest.copyIntoKind DerefX, info:
            dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
          dest.addSymUse field.field, info
        if isResult:
          dest.addParRi()
        inc n
      else:
        takeTree dest, n
  of UnknownToken:
    takeTree dest, n
  of ParLe:
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
        AsmS, DeferS, NoStmt:
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
        # a `(tuple <proctype> (ref RootObj))`, so cast-to-pointer no
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
          var isFnEnvTuple = false
          if srcTyp.typeKind == TupleT:
            var t = srcTyp
            inc t
            if t.typeKind == ProctypeT and procHasPragma(t, ClosureP):
              isFnEnvTuple = true
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
        case n.njvlKind
        of LoopV:
          var beforeBuf = createTokenBuf(32)
          var condBuf = createTokenBuf(16)
          var bodyBuf = createTokenBuf(64)
          var info = n.info
          n.into:                                 # (loop ...)
            assert n.stmtKind == StmtsS
            n.into:                               # stmts_before
              while n.hasMore:
                if n.njvlKind in {MflagV, VflagV}:
                  trMflag c, dest, n   # hoisted outside while
                else:
                  coroTr c, beforeBuf, n
            coroTr c, condBuf, n
            assert n.stmtKind == StmtsS
            n.into:                               # stmts_body
              while n.hasMore:
                if n.stmtKind == ContinueS:
                  skip n
                else:
                  coroTr c, bodyBuf, n
          dest.addParLe WhileS, info
          dest.add condBuf
          dest.addParLe StmtsS, info
          dest.add beforeBuf
          dest.add bodyBuf
          dest.addParRi()
          dest.addParRi()
        of IteV, ItecV:
          # `nj.nim` can emit a trailing 4th slot (a leftover DotToken from
          # guard-closing); drain any extra children so the closing `)` isn't
          # left for the outer loop, which would drop the following siblings.
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
          trMflag c, dest, n
        of JtrueV:
          trJtrue c, dest, n
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
            case pool.tags[n.tagId]
            of "jmp":
              n.into:
                gotoNextState(c, dest, int(pool.integers[n.intId]), n.info)
                inc n
            of "lab":
              dest.addParRi() # close stmts
              dest.addParRi() # close proc decl
              n.into:
                newLocalProc c, dest, int(pool.integers[n.intId]), c.procStack[^1]
                inc n
            else:
              coroTrSons(c, dest, n)
  of ParRi:
    bug "unexpected ')' inside"
