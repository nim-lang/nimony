#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[
Thin `.passive`-flavour adapter for the shared coroutine transform.

The bulk of the CPS state-machine machinery moved into
`src/hexer/coro_transform.nim`. This module:

  - Implements the `.passive`-specific hooks the shared transform calls
    back into (passive-call recognition, passive-call emission, delay /
    suspend / delay0 lowering, the proctype/itertype shape rewrite, and
    the top-level `proc`/`iterator` dispatcher).
  - Installs those hooks on `Context` and runs the top-level pass via
    `transformToCps`.

The example below summarises what the shared transform produces. See
`coro_transform.nim` for the full design notes; this file only narrates
the `.passive` flavour.

  iterator it(x: int): int {.closure.} =
    for i in 0..<x:
      yield i

becomes (sketch):

  type
    ItCoroutine* = object of CoroutineBase
      x, i: int
      dest: ptr int

  proc itNext(coro: ptr ItCoroutine): Continuation =
    coro.i += 1
    result = Continuation(fn: itStart, env: coro)

  proc itStart(coro: ptr ItCoroutine): Continuation =
    if coro.i < coro.x:
      result = Continuation(fn: itNext, env: coro)
    else:
      result = Continuation(fn: nil, env: nil)

  proc createItCoroutine(this: ptr ItCoroutine; x: int; dest: ptr int; caller: Continuation): Continuation =
    this[] = ItCoroutine(x: x, i: 0, dest: dest, caller: caller)
    return itStart(this)

For a passive proc:

  proc foo(x: int) {.passive.} =
    bar()
    baz()

the transformation produces a similar state-procs trio plus an init
wrapper. See `coro_transform.nim` for the full lowering rules.
]##

import std / [assertions, sets, tables]
when defined(nimony):
  {.feature: "lenientnils".}
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs, typenav, builtintypes, typeprops]
import passes
include ".." / nimony / nif_annotations
import coro_transform

# ---------------------------------------------------------------------
# `.passive`-specific predicates
# ---------------------------------------------------------------------

proc isPassive*(c: var Context; s: SymId): bool =
  let typ = c.typeCache.lookupSymbol(s)
  if not cursorIsNil(typ) and procHasPragma(typ, PassiveP):
    return true
  return false

proc passiveProcHook(c: var Context; s: SymId): bool =
  ## `Hooks.isPassiveProc`: only ProcY symbols can carry `{.passive.}`.
  isProc(c, s) and isPassive(c, s)

proc passiveCallHook(c: var Context; n: Cursor): bool =
  ## `Hooks.isPassiveCall`: a call whose statically known target type
  ## carries `{.passive.}`. `delay`/`delay0`/`suspend` are NOT passive
  ## calls — they're their own lowering shape.
  if n.exprKind in CallKinds - {DelayX}:
    let typ = c.typeCache.getType(n.firstSon, {SkipAliases})
    return procHasPragma(typ, PassiveP)
  return false

# ---------------------------------------------------------------------
# Passive call / delay / suspend emitters
# ---------------------------------------------------------------------

proc emitCompleteFromNormal(c: var Context; dest: var TokenBuf;
                            contVar: SymId; info: PackedLineInfo) =
  dest.copyIntoKind CallS, info:
    dest.addSymUse pool.syms.getOrIncl("complete.0." & SystemModuleSuffix), info
    dest.addSymUse contVar, info

proc trPassiveCall(c: var Context; dest: var TokenBuf; n: var Cursor; target: Cursor) =
  let typ = c.typeCache.getType(n.firstSon, {SkipAliases})
  let retType = getType(c.typeCache, n)
  let hasResult = not isVoidType(retType)
  if hasResult:
    assert not cursorIsNil(target), "passive call without target"
  case c.currentProc.kind
  of IsNormal:
    # passive call from within a normal proc:
    let info = n.info
    # fallback to init wrapper call for methods, closures, proctype
    # calls because we cant restore its coroTypeForProc
    if typ.typeKind == MethodT or procHasPragma(typ, ClosureP) or typ.firstSon.kind == DotToken or n.firstSon.symKind notin RoutineKinds:
      let contVar = pool.syms.getOrIncl("`contVar." & $c.currentProc.counter)
      inc c.currentProc.counter
      copyIntoKind dest, VarS, info:
        dest.addSymDef contVar, info
        dest.addDotToken() # exported
        dest.addDotToken() # pragmas
        dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
        # constructor call as initializer:
        copyIntoKind dest, CallS, info:
          let callStart = n
          n = sub(n)
          if n.kind == Symbol and typ.firstSon.kind == SymbolDef:
            dest.addSymUse coroWrapperProc(c, n.symId), info
            inc n
          else:
            coroTr(c, dest, n)
          while n.hasMore:
            coroTr(c, dest, n)
          n = callStart; skip n
          if hasResult:
            dest.copyIntoKind AddrX, info:
              dest.copyTree target
          # add StopContinuation:
          dest.copyIntoKind OconstrX, info:
            dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
            dest.copyIntoKind KvU, info:
              dest.addSymUse pool.syms.getOrIncl(FnFieldName), info
              dest.addParPair NilX, info
            dest.copyIntoKind KvU, info:
              dest.addSymUse pool.syms.getOrIncl(EnvFieldName), info
              dest.addParPair NilX, info
      emitCompleteFromNormal(c, dest, contVar, info)
    else:
      # Stack-allocate the callee's frame (statically known callee).
      # Null callee.callee (see emitStackFrameTag) so deallocFrame is a
      # nop — `callee == nil` marks the frame as stack-allocated.
      let coroVar = pool.syms.getOrIncl("`coroVar." & $c.currentProc.counter)
      inc c.currentProc.counter
      var sym = n.firstSon.symId
      copyIntoKind dest, VarS, info:
        dest.addSymDef coroVar, info
        dest.addDotToken() # exported
        dest.addDotToken() # pragmas
        dest.addSymUse coroTypeForProc(c, sym), info
        dest.addDotToken() # default value
      let contVar = pool.syms.getOrIncl("`contVar." & $c.currentProc.counter)
      inc c.currentProc.counter
      copyIntoKind dest, VarS, info:
        dest.addSymDef contVar, info
        dest.addDotToken() # exported
        dest.addDotToken() # pragmas
        dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
        # constructor call as initializer:
        copyIntoKind dest, CallS, info:
          dest.addSymUse sym, info
          let callStart = n
          n = sub(n)
          skip n # fn already handled
          while n.hasMore:
            coroTr(c, dest, n)
          n = callStart; skip n
          dest.copyIntoKind AddrX, info:
            dest.addSymUse coroVar, info
          if hasResult:
            dest.copyIntoKind AddrX, info:
              dest.copyTree target
          # add StopContinuation:
          dest.copyIntoKind OconstrX, info:
            dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
            dest.copyIntoKind KvU, info:
              dest.addSymUse pool.syms.getOrIncl(FnFieldName), info
              dest.addParPair NilX, info
            dest.copyIntoKind KvU, info:
              dest.addSymUse pool.syms.getOrIncl(EnvFieldName), info
              dest.addParPair NilX, info
      # Tag as stack-allocated:
      emitStackFrameTag(c, dest, coroVar, info)
      emitCompleteFromNormal(c, dest, contVar, info)
  of IsIterator, IsPassive:
    # passive call from within a passive proc:
    # The callee's frame is heap-allocated via allocFrame; the callee
    # frees it via deallocFrame before returning. This supports
    # recursion.  target = call fn, args -->
    # return fnConstructor(addr this.frame, args, addr target, Continuation(nextState, this))
    let state = getNextState(c.currentProc.cf, n)
    assert state != -1
    let info = n.info

    # We cannot generate `return fnConstruct(args)` directly here
    # because the rest of the pipeline already assumes code like
    # `let tmp = fnConstruct(args); return tmp` so that is what we
    # generate here:
    let contVar = pool.syms.getOrIncl("`contVar." & $c.currentProc.counter)
    inc c.currentProc.counter
    dest.addParLe VarS, info
    dest.addSymDef contVar, info
    dest.addDotToken() # exported
    dest.addDotToken() # pragmas
    dest.addSymUse pool.syms.getOrIncl(ContinuationName), info

    # value: emit constructor call with heap-allocated frame:
    copyIntoKind dest, CallS, info:
      let callStart = n
      n = sub(n)
      if n.kind == Symbol and typ.firstSon.kind == SymbolDef:
        dest.addSymUse coroWrapperProc(c, n.symId), info
        inc n
      else:
        coroTr(c, dest, n)
      while n.hasMore:
        coroTr(c, dest, n)
      n = callStart; skip n

      if hasResult:
        dest.copyIntoKind AddrX, info:
          dest.copyTree target
      contNextState c, dest, state, info

    dest.addParRi() # VarS

    copyIntoKind dest, RetS, info:
      dest.addSymUse contVar, info

proc trDelay0(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## `(delay0)` — no-arg form: jump to the NEXT suspension point.
  c.awaitingSuspendPark = true
  let info = n.info
  n.into: # delay0
    let state = getNextState(c.currentProc.cf, n)
    assert state != -1, "delay() no-arg must precede suspension point"
    contNextState(c, dest, state, info)

proc trSuspend(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## `(suspend)` — parks only after a preceding `(delay0)` in the same
  ## state (`Continuation(fn: nil, env: this)`). A bare `suspend()` is a
  ## synchronous transition to the next state so `complete()` can drive on.
  let info = n.info
  var state = -1
  n.into: # suspend
    state = getNextState(c.currentProc.cf, n)
  let park = c.awaitingSuspendPark
  c.awaitingSuspendPark = false
  dest.copyIntoKind RetS, info:
    if park:
      dest.copyIntoKind OconstrX, info:
        dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
        dest.copyIntoKind KvU, info:
          dest.addSymUse pool.syms.getOrIncl(FnFieldName), info
          dest.addParPair NilX, info
        dest.copyIntoKind KvU, info:
          dest.addSymUse pool.syms.getOrIncl(EnvFieldName), info
          dest.copyIntoKind CastX, info:
            dest.copyIntoKind PtrT, info:
              dest.addSymUse pool.syms.getOrIncl(RootObjName), info
            dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
    else:
      assert state != -1
      contNextState(c, dest, state, info)

proc trDelay(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## `(delay fn args)` — fn-args form; typenav returns
  ## `Continuation` for `DelayX`.
  let info = n.info
  let delayStart = n # skip delay tag
  n = sub(n)
  if n.kind == Symbol:
    let sym = n.symId
    inc n    # skip fn symbol
    # Create a child coroutine and return it as a Continuation without
    # yielding. The callee's frame is heap-allocated via allocFrame.
    copyIntoKind dest, CallS, info:
      dest.addSymUse sym, info
      while n.hasMore:
        coroTr(c, dest, n)
      emitAllocFrame(c, dest, sym, info)
      # Pass StopContinuation as the caller so the child doesn't
      # resume anyone on finish.
      dest.copyIntoKind OconstrX, info:
        dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
        dest.copyIntoKind KvU, info:
          dest.addSymUse pool.syms.getOrIncl(FnFieldName), info
          dest.addParPair NilX, info
        dest.copyIntoKind KvU, info:
          dest.addSymUse pool.syms.getOrIncl(EnvFieldName), info
          dest.addParPair NilX, info
    n = delayStart; skip n # skip close of delay
  else:
    dest.copyIntoKind ErrT, info:
      dest.addStrLit "`delay` expects a call expression"
    while n.hasMore: skip n # skip rest
    n = delayStart; skip n

# ---------------------------------------------------------------------
# Proctype / itertype shape rewrite — coroutine-shaped types get the
# wrapper signature `(args..., result: ptr T, caller: Continuation):
# Continuation`. Itertype lowers to a `(tuple <proctype> (ref RootObj))`
# matching closure procs.
# ---------------------------------------------------------------------

proc trProctype(c: var Context; dest: var TokenBuf; n: var Cursor) =
  if n.kind == ParLe:
    let nk = n.typeKind
    let isPassiveProc = nk == ProctypeT and procHasPragma(n, PassiveP)
    # An itertype is a coroutine-shaped value type. `.closure` iter values
    # carry a captured environment, so they lower to a
    # `(tuple <wrapper-proctype> (ref RootObj))` — same runtime shape as
    # closure procs. `.passive` iters have NO environment (their wrapper
    # always allocates a fresh frame, see `generateCoroutineHelpers`), so
    # — exactly like a `.passive` proc — they lower to a bare wrapper
    # proctype function pointer, NOT a tuple.
    let isClosureIterType = nk == ItertypeT and not procHasPragma(n, PassiveP)
    let isPassiveIterType = nk == ItertypeT and procHasPragma(n, PassiveP)
    if isPassiveProc or isClosureIterType or isPassiveIterType:
      var info = n.info
      if isClosureIterType:
        # open the (tuple … (ref RootObj)) wrapper
        dest.addParLe TupleT, info
      var ptStart = default(Cursor)
      if isPassiveProc:
        dest.add n                      # proctype tag (passive proc)
        ptStart = n; n = sub(n)
      else:
        # itertype → wrapper proctype (both `.closure` and `.passive`)
        dest.addParLe ProctypeT, info
        ptStart = n; n = sub(n)          # consume original itertype tag
      dest.takeTree n         # nilability tag
      dest.add n              # params tag
      n.into:
        while n.hasMore:
          trProctype(c, dest, n)
      # return type becomes a ptr parameter:
      if not isVoidType(n):
        dest.copyIntoKind ParamU, info:
          dest.addSymDef pool.syms.getOrIncl(ResultParamName), info
          dest.addDotToken() # export
          dest.addDotToken() # pragmas
          dest.copyIntoKind PtrT, info:
            trProctype(c, dest, n)
          dest.addDotToken() # default value
      else:
        skip n
      # here we add caller param
      dest.copyIntoKind ParamU, info:
        dest.addSymDef pool.syms.getOrIncl(CallerParamName), info
        dest.addDotToken() # export
        dest.addDotToken() # pragmas
        dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
        dest.addDotToken() # default value
      dest.addParRi()
      dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
      while n.hasMore:
        trProctype(c, dest, n)
      dest.addParRi(n.endInfo)
      n = ptStart; skip n
      if isClosureIterType:
        # proctype is already closed; add the env slot and close the tuple
        dest.copyIntoKind RefT, info:
          dest.addSymUse pool.syms.getOrIncl(BareRootObjName), info
        dest.addParRi() # close tuple
    else:
      copyInto dest, n:
        while n.hasMore:
          trProctype(c, dest, n)
  else:
    dest.takeToken n

# ---------------------------------------------------------------------
# Top-level pass + hook wiring.
#
# The coroutine decl dispatcher itself (`transformCoroutineDecl`) is in
# coro_transform.nim — both `.passive` (here) and `.closure` iter
# (lambdalifting.nim) consumers point their `trCoroutine` hook at it.
# ---------------------------------------------------------------------

proc passiveHooks(): Hooks =
  Hooks(
    isPassiveProc: passiveProcHook,
    isPassiveCall: passiveCallHook,
    trPassiveCall: trPassiveCall,
    trDelay: trDelay,
    trDelay0: trDelay0,
    trSuspend: trSuspend,
    trProctype: trProctype,
    trCoroutine: transformCoroutineDecl
  )

proc transformToCps*(pass: var Pass) =
  var n = pass.n  # Extract cursor locally
  var c = Context(thisModuleSuffix: pass.moduleSuffix,
    typeCache: createTypeCache(), coroTypes: createTokenBuf(10),
    continuationProcImpl: generateContinuationProcImpl(),
    hooks: passiveHooks())
  c.typeCache.openScope()
  assert n.stmtKind == StmtsS
  c.coroTypes.add n.load() # the `(stmts` open tag
  n.into:
    while n.hasMore:
      coroTr(c, pass.dest, n)
    for (sym, start) in c.shouldPublish:
      var buf = createTokenBuf(16)
      buf.copyTree pass.dest.cursorAt(start)
      endRead(pass.dest)
      publishSignature buf, sym, 0
    c.coroTypes.add pass.dest # concat coroTypes and other statements
    c.coroTypes.addParRi() # close the root; its source ParRi may be elided
  swap c.coroTypes, pass.dest
  c.typeCache.closeScope()

when isMainModule:
  const
    inp = """ (stmts
 (proc :pa.0.slaldpees1 . . .
  (params
   (param :inp.0 . . string.0.sysvq0asl .)) .
  (pragmas
   (passive)) .
  (stmts
   (if
    (elif
     (true)
     (stmts
      (stmts
       (stmts
        (cmd ignore))
       (cmd ignore)))))))
 (cmd pa.0.slaldpees1 "abcdef")

 (proc :other.0.slaldpees1 . . .
  (params
   (param :inp.0 . . string.0.sysvq0asl .)) .
  (pragmas) .
  (stmts)
  )

 )"""
  var buf = parseFromBuffer(inp, "slaldpees1")
  var pass = Pass(n: beginRead(buf), dest: createTokenBuf(10), moduleSuffix: "slaldpees1")
  transformToCps(pass)
