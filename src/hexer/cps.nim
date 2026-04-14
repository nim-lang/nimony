#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[
We need to transform:

  iterator it(x: int): int {.closure.} =
    for i in 0..<x:
      yield i

or:

  proc foo(x: int) {.passive.} =
    bar()
    baz()

into:

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

Callee frames are heap-allocated via `allocFrame(sizeof(ItCoroutine))` and freed by the
callee via `deallocFrame` before returning. This supports recursion since each call gets
its own heap frame rather than being inlined into the caller's frame.

1. Compile the AST/NIF to a CFG with goto instructions (src/nimony/controlflow does that already). Pay special
   attention that no implicit fall-through remains for code like (if cond: a); b().
2. Optimize the CFG so that unused labels are not generated.
3. Turn the target **labels** of the gotos into functions.
4. Move these inner functions to the top level ("lambda lifting"), compute the required environment objects.
5. The return value `result` remains and is passed out of the iterator like it is done for regular procs.
6. Turn `goto label` into function calls. Turn `yield` into `env.cont = nextState; return`.

Usage of the closure iterator in a for loop than becomes an easy trampoline:

  for forLoopVar in it(10): echo forLoopVar

Becomes:

  const StopContinuation = Continuation(fn: nil, env: nil)

  var forLoopVar: int
  var it = createItCoroutine(cast[ptr ItCoroutine](allocFrame(sizeof(ItCoroutine))), 10, addr forLoopVar, StopContinuation)
  while it.fn != nil:
    echo forLoopVar
    it = scheduler.tick it

For a passive proc:

  proc foo(x: int) {.passive.} =
    bar()
    baz()

The transformation produces:

  type
    FooCoroutine* = object of CoroutineBase
      x: int

  # Helper that allocates frame and delegates to entry
  proc foo_init(x: int; result: ptr int; caller: Continuation): Continuation =
    let this = cast[ptr FooCoroutine](allocFrame(sizeof(FooCoroutine)))
    return foo(x, this, result, caller)

  # Entry: takes pre-allocated frame, initializes environment, runs first state
  proc foo(x, this: ptr FooCoroutine; result: ptr int; caller: Continuation): Continuation =
    this[] = FooCoroutine(x: x, caller: caller, callee: cast[ptr CoroutineBase](this))
    return foo_s0(this)

  # State s0: runs up to first suspension point
  proc foo_s0(this: ptr FooCoroutine): Continuation =
    bar()        # suspension point -> state transition
    return foo_s1(this)

  # State s1: runs until completion
  proc foo_s1(this: ptr FooCoroutine): Continuation =
    baz()
    let tmpCaller = this.caller
    deallocFrame(cast[ptr CoroutineBase](this))
    return tmpCaller  # return to caller

]##

import std / [assertions, sets, tables]
include ".." / lib / nifprelude
import ".." / lib / symparser
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, expreval, xints,
  builtintypes, langmodes, renderer, reporters, typeprops]
import ".." / njvl / [nj, njvl_model]
import hexer_context, passes

# TODO:
# - transform `for` loops into trampoline code
# - the control flow graph must duplicate `finally` blocks
# - the control flow graph must model procs that can raise

#[

Compatibility with Nim is not easy. We need to produce full-fledged adapter objects:

var it = (iter)
while not finished(it):
  it(args)

becomes:

type
  IterAdapter = object of IterCoroutine
    emulate: proc (this: ptr IterAdapter; args)
    real: Continuation

proc emulate(this: ptr IterAdapter; args) =
  # or maybe ignore this:
  this.args = args # expand all args
  this.real = this.real.fn(this.real.env)

proc emulateBegin(this: ptr IterAdapter; args) =
  initIterCoroutine(this, args)
  this.emulate = emulate

]#

const
  ContinuationProcName = "ContinuationProc.0." & SystemModuleSuffix
  ContinuationName = "Continuation.0." & SystemModuleSuffix
  RootObjName = "CoroutineBase.0." & SystemModuleSuffix
  EnvParamName = "`this.0"
  FnFieldName = "fn.0"
  EnvFieldName = "env.0"
  CallerFieldName = "caller.0"
  CalleeFieldName = "callee.0"
  ResultParamName = "`result.0"
  ResultFieldName = "`result.0"
  CallerParamName = "`caller.0"
  AllocFrameProcName = "allocFrame.0." & SystemModuleSuffix
  DeallocFrameProcName = "deallocFrame.0." & SystemModuleSuffix

type
  EnvField = object
    objType: SymId
    field: SymId
    typeAsSym: SymId
    pragmas, typ: Cursor
    def: int
    use: int

  RoutineKind = enum
    IsNormal, IsIterator, IsPassive

  ProcContext = object
    localToEnv: Table[SymId, EnvField]
    labels: Table[int, int]
    cf: TokenBuf
    resultSym: SymId
    counter: int
    labelCounter: int = 1
    kind: RoutineKind

  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    procStack: seq[SymId]
    currentProc: ProcContext
    continuationProcImpl: Cursor
    shouldPublish: seq[tuple[sym: SymId, start: int]]
    coroTypes: TokenBuf

proc coroTypeForProc(c: Context; procId: SymId): SymId =
  let s = extractVersionedBasename(pool.syms[procId])
  result = pool.syms.getOrIncl(s & ".coro." & c.thisModuleSuffix)

proc coroWrapperProc(c: Context; procId: SymId): SymId =
  let s = extractVersionedBasename(pool.syms[procId])
  result = pool.syms.getOrIncl(s & ".init." & c.thisModuleSuffix)

proc stateToProcName(c: Context; sym: SymId; state: int): SymId =
  let s = extractVersionedBasename(pool.syms[sym])
  result = pool.syms.getOrIncl(s & ".s" & $state & "." & c.thisModuleSuffix)

proc localToFieldname(c: var Context; local: SymId): SymId =
  var name = pool.syms[local]
  extractBasename name
  name.add "`f."
  name.add $c.counter
  inc c.counter
  name.add "."
  name.add c.thisModuleSuffix
  result = pool.syms.getOrIncl(name)

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc trSons(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    while n.kind != ParRi:
      tr(c, dest, n)

proc contNextState(c: var Context; dest: var TokenBuf; state: int; info: PackedLineInfo) =
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

proc emitAllocFrame(c: var Context; dest: var TokenBuf; calleeSym: SymId; info: PackedLineInfo) =
  ## Emit: cast[ptr CalleeCoroutine](allocFrame(sizeof(CalleeCoroutine)))
  dest.copyIntoKind CastX, info:
    dest.copyIntoKind PtrT, info:
      dest.addSymUse coroTypeForProc(c, calleeSym), info
    dest.copyIntoKind CallX, info:
      dest.addSymUse pool.syms.getOrIncl(AllocFrameProcName), info
      dest.copyIntoKind SizeofX, info:
        dest.addSymUse coroTypeForProc(c, calleeSym), info

proc emitDeallocFrame(c: var Context; dest: var TokenBuf; info: PackedLineInfo) =
  ## Emit: deallocFrame(cast[ptr CoroutineBase](this))
  dest.copyIntoKind CallS, info:
    dest.addSymUse pool.syms.getOrIncl(DeallocFrameProcName), info
    dest.copyIntoKind CastX, info:
      dest.copyIntoKind PtrT, info:
        dest.addSymUse pool.syms.getOrIncl(RootObjName), info
      dest.addSymUse pool.syms.getOrIncl(EnvParamName), info

proc emitFinalReturn(c: var Context; dest: var TokenBuf; info: PackedLineInfo) =
  ## Emit: save caller, deallocFrame, return saved caller.
  ## Used when the coroutine completes (not for intermediate state transitions).
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

proc emitStackFrameTag(c: var Context; dest: var TokenBuf; coroVar: SymId; info: PackedLineInfo) =
  ## Emit: coroVar.callee = nil
  ## Marks the frame as stack-allocated so deallocFrame is a nop.
  ## (The constructor sets callee = this; overriding it to nil signals "don't free".)
  dest.copyIntoKind AsgnS, info:
    dest.copyIntoKind DotX, info:
      dest.addSymUse coroVar, info
      dest.addSymUse pool.syms.getOrIncl(CalleeFieldName), info
      dest.addIntLit 1, info # field is in superclass
    dest.addParPair NilX, info

proc isMethod*(c: var Context; s: SymId): bool =
  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    result = res.decl.symKind == MethodY
  else:
    let info = getLocalInfo(c.typeCache, s)
    result = info.kind == MethodY

proc getNextState(buf: TokenBuf; n: Cursor): int =
  var pos = cursorToPosition(buf, n)
  while pos < buf.len:
    if pool.tags[buf[pos].tag] == "lab":
      return int(pool.integers[buf[pos+1].intId])
    inc pos
  return -1

proc trPassiveCall(c: var Context; dest: var TokenBuf; n: var Cursor; sym: SymId; target: Cursor;
                   inhibitComplete = false) =
  let retType = getType(c.typeCache, n)
  let hasResult = not isVoidType(retType)
  if hasResult:
    assert not cursorIsNil(target), "passive call without target"
  case c.currentProc.kind
  of IsNormal:
    # passive call from within a normal proc:
    let info = n.info
    if inhibitComplete:
      # Use heap allocation (callee deallocates):
      dest.addParLe ExprX, info
      dest.addParLe StmtsS, info
      dest.addParRi() # StmtsS
      copyIntoKind dest, CallS, info:
        dest.addSymUse coroWrapperProc(c, sym), info
        inc n
        skip n # fn already handled
        while n.kind != ParRi:
          tr(c, dest, n)
        inc n
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
      dest.addParRi() # ExprX
    elif isMethod(c, sym):
      let contVar = pool.syms.getOrIncl("`contVar." & $c.currentProc.counter)
      inc c.currentProc.counter
      copyIntoKind dest, VarS, info:
        dest.addSymDef contVar, info
        dest.addDotToken() # exported
        dest.addDotToken() # pragmas
        dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
        # constructor call as initializer:
        copyIntoKind dest, CallS, info:
          dest.addSymUse coroWrapperProc(c, sym), info
          inc n
          skip n # fn already handled
          while n.kind != ParRi:
            tr(c, dest, n)
          inc n
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
      # complete(contVar):
      dest.copyIntoKind CallS, info:
        dest.addSymUse pool.syms.getOrIncl("complete.0." & SystemModuleSuffix), info
        dest.addSymUse contVar, info
    else:
      # Stack-allocate the callee's frame (statically known callee).
      # Tag callee.callee with bit 0 so deallocFrame is a nop.
      let coroVar = pool.syms.getOrIncl("`coroVar." & $c.currentProc.counter)
      inc c.currentProc.counter
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
          inc n
          skip n # fn already handled
          while n.kind != ParRi:
            tr(c, dest, n)
          inc n
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
      # complete(contVar):
      dest.copyIntoKind CallS, info:
        dest.addSymUse pool.syms.getOrIncl("complete.0." & SystemModuleSuffix), info
        dest.addSymUse contVar, info
  of IsIterator, IsPassive:
    # passive call from within a passive proc:
    # The callee's frame is heap-allocated via allocFrame; the callee
    # frees it via deallocFrame before returning. This supports recursion.
    # target = call fn, args
    # -->
    # return fnConstructor(addr this.frame, args, addr target, Continuation(nextState, this))
    let state = getNextState(c.currentProc.cf, n)
    assert state != -1
    let info = n.info

    var contVar = SymId(0)
    if not inhibitComplete:
      # We cannot generate `return fnConstruct(args)` directly here because
      # the rest of the pipeline already assumes code
      # like `let tmp = fnConstruct(args); return tmp` so that is what we
      # generate here:
      contVar = pool.syms.getOrIncl("`contVar." & $c.currentProc.counter)
      inc c.currentProc.counter
      dest.addParLe VarS, info
      dest.addSymDef contVar, info
      dest.addDotToken() # exported
      dest.addDotToken() # pragmas
      dest.addSymUse pool.syms.getOrIncl(ContinuationName), info

    # value: emit constructor call with heap-allocated frame:
    copyIntoKind dest, CallS, info:
      dest.addSymUse coroWrapperProc(c, sym), info

      inc n
      skip n # fn already handled
      while n.kind != ParRi:
        tr(c, dest, n)
      inc n

      if hasResult:
        dest.copyIntoKind AddrX, info:
          dest.copyTree target
      contNextState c, dest, state, info

    if not inhibitComplete:
      dest.addParRi() # VarS

      copyIntoKind dest, RetS, info:
        dest.addSymUse contVar, info

proc trDelay0(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # Handles (delay0) — no-arg form: to the NEXT suspension point.
  let info = n.info
  inc n      # skip delay0 tag
  # Find the next suspension point (if any) - delay captures continuation to resume, not stop
  var state = getNextState(c.currentProc.cf, n)
  assert state != -1, "delay() no-arg must precede suspension point"
  contNextState(c, dest, state, info)
  skipParRi n  # skip ParRi of delay0

proc trSuspend(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # Handles (suspend) — suspends the coroutine by returning Continuation(nil, nil).
  # This stops the trampoline. To resume, use delay() to capture the continuation.
  let info = n.info
  inc n      # skip suspend tag
  skipParRi n  # skip ParRi of suspend
  dest.copyIntoKind RetS, info:
    dest.copyIntoKind OconstrX, info:
      dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
      dest.copyIntoKind KvU, info:
        dest.addSymUse pool.syms.getOrIncl(FnFieldName), info
        dest.addParPair NilX, info
      dest.copyIntoKind KvU, info:
        dest.addSymUse pool.syms.getOrIncl(EnvFieldName), info
        dest.addParPair NilX, info

proc trDelay(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # Handles (delay fn args) — fn-args form; typenav returns Continuation for DelayX.
  let info = n.info
  inc n      # skip delay tag
  if n.kind == Symbol:
    let sym = n.symId
    inc n    # skip fn symbol
    # Create a child coroutine and return it as a Continuation without yielding.
    # The callee's frame is heap-allocated via allocFrame.
    copyIntoKind dest, CallS, info:
      dest.addSymUse sym, info
      while n.kind != ParRi:
        tr(c, dest, n)
      emitAllocFrame(c, dest, sym, info)
      # Pass StopContinuation as the caller so the child doesn't resume anyone on finish.
      dest.copyIntoKind OconstrX, info:
        dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
        dest.copyIntoKind KvU, info:
          dest.addSymUse pool.syms.getOrIncl(FnFieldName), info
          dest.addParPair NilX, info
        dest.copyIntoKind KvU, info:
          dest.addSymUse pool.syms.getOrIncl(EnvFieldName), info
          dest.addParPair NilX, info
    skipParRi n  # skip ParRi of delay
  else:
    dest.copyIntoKind ErrT, info:
      dest.addStrLit "`delay` expects a call expression"
    skip n   # skip rest
    skipParRi n

proc passiveCallFn(c: var Context; n: Cursor): SymId =
  if n.exprKind notin CallKinds - {DelayX}: return SymId(0)
  let fn = n.firstSon
  if fn.kind == Symbol:
    let typ = c.typeCache.getType(fn, {SkipAliases})
    if procHasPragma(typ, PassiveP):
      return fn.symId
  return SymId(0)

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let fn = n.firstSon
  if fn.kind == Symbol:
    let sym = fn.symId
    let typ = c.typeCache.getType(fn, {SkipAliases})
    if procHasPragma(typ, PassiveP):
      var retType = getType(c.typeCache, n)
      let hasResult = not isVoidType(retType)
      if hasResult:
        let info = n.info
        var s = dest.len
        dest.copyIntoKind ExprX, info:
          let tmpVar = pool.syms.getOrIncl("`tmpCpsResult." & $c.currentProc.counter)
          inc c.currentProc.counter
          var target = createTokenBuf(1)
          target.addSymUse tmpVar, info
          dest.copyIntoKind VarS, info:
            dest.addSymDef tmpVar, info
            dest.addDotToken() # exported
            dest.addDotToken() # pragmas
            dest.takeTree retType
            dest.addDotToken()
          trPassiveCall(c, dest, n, sym, beginRead target)
          dest.addSymUse tmpVar, info
      else:
        trPassiveCall(c, dest, n, sym, default(Cursor))
    else:
      trSons(c, dest, n)
  else:
    trSons(c, dest, n)

proc trLocalValue(c: var Context; dest: var TokenBuf; n: var Cursor; lhs: Cursor) =
  let fn = passiveCallFn(c, n)
  if fn != SymId(0):
    trPassiveCall(c, dest, n, fn, lhs)
  else:
    dest.copyIntoKind AsgnS, n.info:
      dest.copyTree lhs
      tr(c, dest, n)


proc trAsgn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var rhs = n.firstSon
  skip rhs
  let fn = passiveCallFn(c, rhs)
  if fn == SymId(0):
    copyInto dest, n:
      tr c, dest, n
      tr c, dest, n
  else:
    var lhsTransformed = createTokenBuf(6)
    inc n
    tr c, lhsTransformed, n
    trPassiveCall(c, dest, rhs, fn, beginRead lhsTransformed)
    skipParRi n

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let sym = n.firstSon.symId
  let kind = n.symKind
  let info = n.info

  let field = c.currentProc.localToEnv.getOrDefault(sym)
  if field.def != field.use:
    inc n
    skip n # name
    skip n # exported
    skip n # pragmas
    c.typeCache.registerLocal(sym, kind, n)
    skip n # type
    if n.kind == DotToken:
      inc n
    else:
      var lhs = createTokenBuf(6)
      lhs.copyIntoKind DotX, info:
        lhs.copyIntoKind DerefX, info:
          lhs.addSymUse pool.syms.getOrIncl(EnvParamName), info
        lhs.addSymUse field.field, info
      trLocalValue(c, dest, n, beginRead lhs)
    skipParRi n
  else:
    var pcall = SymId(0)
    var callExpr = default(Cursor)
    copyInto dest, n:
      let target = n
      c.typeCache.takeLocalHeader(dest, n, kind)
      pcall = passiveCallFn(c, n)
      if pcall != SymId(0):
        callExpr = n
        dest.addDotToken()
        skip n
      else:
        tr(c, dest, n)
    if pcall != SymId(0):
      var sym = createTokenBuf(1)
      sym.addSymUse target.symId, info
      trPassiveCall c, dest, callExpr, pcall, beginRead sym

proc declareContinuationResult(c: var Context; dest: var TokenBuf; info: PackedLineInfo) =
  dest.copyIntoKind ResultS, info:
    dest.addSymDef pool.syms.getOrIncl("result.0"), info
    dest.addDotToken() # exported
    dest.addDotToken() # pragmas
    dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
    dest.addDotToken() # default value

proc newLocalProc(c: var Context; dest: var TokenBuf; state: int; sym: SymId) =
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

  # return type is always `Continuation`:
  dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
  dest.addDotToken() # pragmas
  dest.addDotToken() # effects

  publishSignature dest, name, procBegin

  dest.addParLe StmtsS, info # body
  declareContinuationResult c, dest, info

proc gotoNextState(c: var Context; dest: var TokenBuf; state: int; info: PackedLineInfo) =
  # generate: `return state(this)`
  dest.copyIntoKind RetS, info:
    dest.copyIntoKind CallS, info:
      dest.addSymUse stateToProcName(c, c.procStack[^1], state), info
      dest.addSymUse pool.syms.getOrIncl(EnvParamName), info

proc returnValue(c: var Context; dest: var TokenBuf; n: var Cursor; info: PackedLineInfo) =
  inc n # yield/return
  if n.kind == DotToken or (n.kind == Symbol and n.symId == c.currentProc.resultSym):
    inc n
  elif isVoidType(getType(c.typeCache, n)) and n.kind != Symbol:
    # void type for Symbol can happen for `raise` statements:
    tr c, dest, n
  else:
    dest.copyIntoKind AsgnS, info:
      dest.copyIntoKind DerefX, info:
        dest.copyIntoKind DotX, info:
          dest.copyIntoKind DerefX, info:
            dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
          dest.addSymUse pool.syms.getOrIncl(ResultFieldName), info
      tr c, dest, n
  skipParRi n

proc trYield(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # yield ex
  # -->
  # this.res[] = ex
  # return Continuation(fn: stateToProcName(c, sym, nextState), env: this)
  let state = getNextState(c.currentProc.cf, n)
  assert state != -1
  let info = n.info
  returnValue(c, dest, n, info)
  dest.copyIntoKind RetS, info:
    contNextState(c, dest, state, info)

proc emitReturnCaller(c: var Context; dest: var TokenBuf; info: PackedLineInfo) =
  ## Emit `return this.caller` — returns the caller continuation.
  dest.copyIntoKind RetS, info:
    dest.copyIntoKind DotX, info:
      dest.copyIntoKind DerefX, info:
        dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
      dest.addSymUse pool.syms.getOrIncl(CallerFieldName), info
      dest.addIntLit 1, info # field is in superclass

proc trReturn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # return/raise x -->
  # this.res[] = x
  # var tmpCaller = this.caller; deallocFrame(this); return/raise tmpCaller
  let head = n.load()
  let info = head.info
  returnValue(c, dest, n, info)
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

proc escapingLocals(c: var Context; n: Cursor) =
  if n.kind == DotToken: return
  var currentState = 0
  var n = n
  var nested = 0
  while true:
    if pool.tags[n.tag] == "lab":
      currentState = int(pool.integers[n.firstSon.intId])

    let sk = n.stmtKind
    let nk = n.njvlKind
    case sk
    of LocalDecls:
      inc n
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
      inc nested
    else:
     if nk in {MflagV, VflagV}:
      # NJ guard flags are bool variables that may cross state boundaries
      inc n # skip mflag/vflag tag
      let mine = n.symId
      c.currentProc.localToEnv[mine] = EnvField(
        objType: coroTypeForProc(c, c.procStack[^1]),
        field: localToFieldname(c, mine),
        typ: c.typeCache.builtins.boolType,
        def: currentState,
        use: currentState)
      skip n # symdef
      inc n # ParRi
     else:
      case n.kind
      of ParRi:
        dec nested
        if nested == 0: break
      of ParLe:
        inc nested
      of Symbol:
        let def = c.currentProc.localToEnv.getOrDefault(n.symId, EnvField(def: -2)).def
        if def != -2:
          if def != currentState:
            c.currentProc.localToEnv[n.symId].use = currentState
      else:
        discard
      inc n

proc isPassiveCall(c: var Context; n: PackedToken): bool =
  if n.kind == Symbol:
    let typ = c.typeCache.lookupSymbol(n.symId)
    if not cursorIsNil(typ) and procHasPragma(typ, PassiveP):
      return true
  return false

proc containsSuspensionPoint(c: var Context; n: Cursor): bool =
  var nested = 0
  var n = n
  while true:
    let sk = n.stmtKind
    let ek = n.exprKind
    if sk == YldS or (ek in CallKinds - {DelayX} and isPassiveCall(c, n.firstSon.load)) or ek == SuspendX:
      return true
    inc n
    if n.kind == ParRi:
      if nested == 0: break
      dec nested
    elif n.kind == ParLe: inc nested
  return false

proc trMflag(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Convert NJ `(mflag symdef)` / `(vflag symdef)` to a bool var initialized to false.
  ## If the flag is lifted to the environment, emit an assignment to the env field instead.
  let info = n.info
  inc n  # skip mflag/vflag tag
  let symDef = n
  let symId = n.symId
  inc n  # skip symdef
  inc n  # skip ParRi
  let field = c.currentProc.localToEnv.getOrDefault(symId)
  if field.def != field.use:
    # Lifted to environment: assign false to the env field
    dest.copyIntoKind AsgnS, info:
      dest.copyIntoKind DotX, info:
        dest.copyIntoKind DerefX, info:
          dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
        dest.addSymUse field.field, info
      dest.addParPair FalseX, info
  else:
    # Local: emit a regular var declaration
    dest.addParLe VarS, info
    dest.add symDef
    dest.addDotToken()  # exported
    dest.addDotToken()  # pragmas
    dest.copyTree c.typeCache.builtins.boolType
    dest.addParPair FalseX, info  # initialized to false
    dest.addParRi()

proc trJtrue(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Convert NJ `(jtrue sym...)` to `(asgn sym true)` for each symbol.
  ## If the symbol is lifted to the environment, assign to the env field.
  let info = n.info
  inc n  # skip jtrue tag
  while n.kind != ParRi:
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
    inc n
  inc n  # skip ParRi

proc emitJump(dest: var TokenBuf; label: int; info: PackedLineInfo) =
  dest.add tagToken("jmp", info)
  dest.addIntLit label, info
  dest.addParRi()

proc emitLabel(dest: var TokenBuf; label: int; info: PackedLineInfo) =
  dest.add tagToken("lab", info)
  dest.addIntLit label, info
  dest.addParRi()

proc trGoto(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var info = n.info
  case n.njvlKind
  of ContinueV:
    skip n
  of StoreV:
    dest.takeToken n
    var addLabel = n.exprKind in CallKinds - {DelayX} and isPassiveCall(c, n.firstSon.load)
    dest.takeTree n
    dest.takeTree n
    dest.takeParRi n
    if addLabel:
      emitLabel dest, c.currentProc.labelCounter, info
      inc c.currentProc.labelCounter
  of LoopV:
    let hasSuspension = containsSuspensionPoint(c, n)
    if not hasSuspension:
      dest.takeTree n
    else:
      inc n
      assert n.stmtKind == StmtsS
      inc n # enter stmts_before
      while n.kind != ParRi:
        dest.takeTree n # copy all mflags, it will be handled later
      inc n # skip stmts_before ParRi
      var beforeLoopState = c.currentProc.labelCounter
      inc c.currentProc.labelCounter
      var afterLoopState = c.currentProc.labelCounter
      inc c.currentProc.labelCounter
      emitJump dest, beforeLoopState, info
      emitLabel dest, beforeLoopState, info
      dest.copyIntoKind IfS, info:
        dest.copyIntoKind ElifU, info:
          dest.copyIntoKind NotX, info:
            dest.takeTree n
          dest.copyIntoKind StmtsS, info:
            emitJump dest, afterLoopState, info
      assert n.stmtKind == StmtsS
      inc n  # enter stmts_body (past StmtsS tag)
      while n.kind != ParRi:
        trGoto c, dest, n
      emitJump dest, beforeLoopState, info
      emitLabel dest, afterLoopState, info
      skipParRi n  # skip loop ParRi
  of IteV, ItecV:
    let hasSuspension = containsSuspensionPoint(c, n)
    if not hasSuspension:
      dest.takeTree n
    else:
      inc n
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
      skip n
      if elseCur.kind != DotToken:
        emitJump dest, lelse, info
        emitLabel dest, lelse, info
        inc elseCur
        while elseCur.kind != ParRi:
          trGoto c, dest, elseCur
      emitJump dest, lend, info
      emitLabel dest, lthen, info
      inc thenCur
      while thenCur.kind != ParRi:
        trGoto c, dest, thenCur
      emitJump dest, lend, info
      emitLabel dest, lend, info
      skipParRi n
  else:
    let sk = n.stmtKind
    let ek = n.exprKind
    if sk == YldS or (ek in CallKinds - {DelayX} and isPassiveCall(c, n.firstSon.load)) or
        ek == SuspendX:
      takeTree dest, n
      emitLabel dest, c.currentProc.labelCounter, info
      inc c.currentProc.labelCounter
    else:
      case n.kind
      of ParLe:
        case n.stmtKind
        of LocalDecls - {ResultS}:
          dest.takeToken n
          dest.takeTree n
          dest.takeTree n
          dest.takeTree n
          dest.takeTree n
          var addLabel = n.exprKind in CallKinds - {DelayX} and isPassiveCall(c, n.firstSon.load)
          dest.takeTree n
          dest.takeParRi n
          if addLabel:
            emitLabel dest, c.currentProc.labelCounter, info
            inc c.currentProc.labelCounter
        else:
          dest.takeToken n
          while n.kind != ParRi:
            trGoto c, dest, n
          dest.takeToken n
      else:
        dest.takeToken n

proc toGoto(c: var Context; n: Cursor): TokenBuf =
  result = createTokenBuf(300)
  assert n.stmtKind == StmtsS, $n.kind
  var n = n
  trGoto(c, result, n)

proc treIteratorBody(c: var Context; dest: var TokenBuf; init: TokenBuf; iter: Cursor; sym: SymId) =
  # Transform the proc body via the NJ pass to get structured code without
  # break/continue/goto, then store just the body (without NJ bookkeeping
  # variables like mflag/jtrue/kill) in c.currentProc.cf.
  # Wrap the single proc into a temporary `(stmts ...)` so NJ can process it:
  var wrapper = createTokenBuf(10)
  wrapper.addParLe StmtsS, NoLineInfo
  wrapper.copyTree iter
  wrapper.addParRi()
  var pass = initPass(ensureMove wrapper, c.thisModuleSuffix, "eliminateJumps", 0)
  eliminateJumps(pass, raisesResolved = true)
  when defined(logPasses):
    echo ""
    echo "========= NJ OUTPUT ======"
    echo pass.dest.toString(false)
  # pass.dest is (stmts cfvar_decls... (proc header body_stmts) ...).
  # Navigate into the proc body; then copy it while stripping NJ bookkeeping
  # (mflag/vflag/jtrue/kill) so c.currentProc.cf has no versionized variables.
  block extractBody:
    var wholeResult = ensureMove(pass.dest)
    var nExt = beginRead(wholeResult)
    inc nExt  # skip outer StmtsS, now at first child
    # Skip cfvar decls and other non-proc statements until we find the proc
    let procKind = iter.stmtKind
    while nExt.kind != ParRi and nExt.stmtKind != procKind:
      skip nExt
    # Now at the proc node
    inc nExt  # skip ProcS/IteratorS tag, now at first header subtree
    for i in 0..<BodyPos:
      skip nExt  # skip each of the BodyPos header subtrees
    # nExt now points to the body (stmts ...) with NJ-transformed code
    # Copy the body as-is; tr handles NJ constructs (mflag→var, jtrue→asgn, loop→while, etc.)
    var bodyBuf = createTokenBuf(wholeResult.len)
    bodyBuf.copyTree nExt
    c.currentProc.cf = ensureMove bodyBuf
  
  # Analyze which locals escape across suspension points using the same label map
  c.currentProc.cf = toGoto(c, beginRead(c.currentProc.cf))
  when defined(logPasses):
    echo "========= GOTO ======="
    echo c.currentProc.cf.toString(false)
    echo ""

  var n = beginRead(c.currentProc.cf)
  escapingLocals(c, n)

  # Compile the state machine by splitting at label positions
  assert n.stmtKind == StmtsS
  dest.takeToken n
  dest.add init
  declareContinuationResult c, dest, NoLineInfo
  dest.copyIntoKind RetS, n.info:
    contNextState(c, dest, 0, n.info)
  dest.addParRi() # close stmts
  dest.addParRi() # close proc decl
  newLocalProc c, dest, 0, c.procStack[^1]
  while n.kind != ParRi:
    tr c, dest, n
  skipParRi n

proc generateCoroutineType(c: var Context; dest: var TokenBuf; sym: SymId) =
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
            if cursorIsNil(value.pragmas):
              dest.addDotToken()
            else:
              dest.copyTree value.pragmas
            if key == c.currentProc.resultSym:
              dest.copyIntoKind PtrT, info:
                dest.copyTree value.typ
            elif value.typeAsSym != SymId(0):
              dest.addSymUse value.typeAsSym, info
            else:
              dest.copyTree value.typ
            dest.addDotToken() # default value
          programs.publish(value.field, dest, beforeField)
  programs.publish(objType, dest, beforeType)

proc generateCoroutineHelpers(c: var Context; dest: var TokenBuf; sym: SymId; iter: Cursor) =
  let newSym = coroWrapperProc(c, sym)
  let info = iter.info
  var hasResult = false
  var n: Cursor = iter
  var params: Cursor

  var start = dest.len

  dest.takeToken n # ProcS
  skip n
  dest.addSymDef newSym, info
  dest.takeTree n # 
  dest.takeTree n # 
  dest.takeTree n # TypevarsU

  dest.copyIntoKind ParamsU, info:
    params = n
    c.typeCache.openProcScope(newSym, iter, n)
    if n.kind != DotToken:
      inc n
      while n.kind != ParRi:
        assert n.substructureKind == ParamU
        dest.takeToken n
        let paramSym = n.symId
        dest.takeTree n # name
        dest.takeTree n # exported
        dest.takeTree n # pragmas
        c.typeCache.registerLocal(paramSym, ParamY, n)
        dest.takeTree n # type
        dest.takeTree n # default value
        dest.takeParRi n # ParRi
    inc n
    # return type becomes a ptr parameter:
    hasResult = not isVoidType(n)
    if hasResult:
      dest.copyIntoKind ParamU, info:
        dest.addSymDef pool.syms.getOrIncl(ResultParamName), info
        dest.addDotToken() # export
        dest.addDotToken() # pragmas
        dest.copyIntoKind PtrT, info:
          dest.takeTree n
        dest.addDotToken() # default value
      # final parameter is always the `caller` continuation:
    else:
      skip n
    dest.copyIntoKind ParamU, info:
      dest.addSymDef pool.syms.getOrIncl(CallerParamName), info
      dest.addDotToken() # export
      dest.addDotToken() # pragmas
      dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
      dest.addDotToken() # default value
  # the return type is always `Continuation` too:
  dest.addSymUse pool.syms.getOrIncl(ContinuationName), info
  dest.takeTree n
  dest.takeTree n

  publishSignature dest, newSym, start

  dest.copyIntoKind StmtsS, info:
    dest.copyIntoKind RetS, info:
      dest.copyIntoKind ProccallX, info:
        dest.addSymUse sym, info
        if params.kind != DotToken:
          inc params
          while params.kind != ParRi:
            assert params.substructureKind == ParamU
            inc params
            dest.addSymUse params.symId, info
            skip params # name
            skip params # exported
            skip params # pragmas
            skip params # type
            skip params # default value
            inc params # ParRi
        emitAllocFrame(c, dest, sym, info)
        if hasResult:
          dest.addSymUse pool.syms.getOrIncl(ResultParamName), info
        dest.addSymUse pool.syms.getOrIncl(CallerParamName), info
  dest.addParRi() # ProcS

  c.typeCache.closeScope()

proc registerParamsInTypecache(c: var Context; sym: SymId; origParams: Cursor) =
  var n = origParams
  if n.kind != DotToken:
    inc n
    while n.kind != ParRi:
      assert n.substructureKind == ParamU
      inc n
      let paramSym = n.symId
      skip n # name
      skip n # exported
      skip n # pragmas
      c.typeCache.registerLocal(paramSym, ParamY, n)
      skip n # type
      skip n # default value
      inc n # ParRi

proc patchParamList(c: var Context; dest, init: var TokenBuf; sym: SymId;
                    paramsBegin, paramsEnd: int; origParams: Cursor) =
  let info = dest[paramsBegin].info
  var retType = createTokenBuf(4)
  for i in paramsEnd..<dest.len: retType.add dest[i]

  dest.shrink paramsBegin
  let thisParam = pool.syms.getOrIncl(EnvParamName)
  dest.copyIntoKind ParamsU, info:
    # generate `this[] = ObjConstructor(params)`:
    init.addParLe AsgnS, info
    init.copyIntoKind DerefX, info:
      init.addSymUse thisParam, info
    init.addParLe OconstrX, info
    init.addSymUse coroTypeForProc(c, sym), info
    # First: copy original parameters (these come from the caller, before coro-addr):
    var n = origParams
    if n.kind != DotToken:
      inc n
      while n.kind != ParRi:
        assert n.substructureKind == ParamU
        dest.takeToken n
        let paramSym = n.symId
        dest.takeTree n # name
        dest.takeTree n # exported
        let pragmas = n
        dest.takeTree n # pragmas
        let field = localToFieldname(c, paramSym)
        c.currentProc.localToEnv[paramSym] = EnvField(
          objType: coroTypeForProc(c, sym),
          field: field,
          pragmas: pragmas,
          typ: n,
          def: -1,
          use: 0)
        c.typeCache.registerLocal(paramSym, ParamY, n)
        dest.takeTree n # type
        dest.takeTree n # default value
        dest.takeParRi n # ParRi

        init.copyIntoKind KvU, info:
          init.addSymUse field, info
          init.addSymUse paramSym, info

    # Second: `this` pointer (coroutine type - comes after original args):
    dest.copyIntoKind ParamU, info:
      dest.addSymDef thisParam, info
      dest.addDotToken() # export
      dest.addDotToken() # pragmas
      dest.copyIntoKind PtrT, info:
        dest.addSymUse coroTypeForProc(c, sym), info
      dest.addDotToken() # default value

    # return type becomes a ptr parameter:
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
    # final parameter is always the `caller` continuation:
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
    # Set callee = this so deallocFrame knows this is a heap-allocated frame.
    # Stack-allocated frames override this to nil after construction.
    init.copyIntoKind KvU, info:
      init.addSymUse pool.syms.getOrIncl(CalleeFieldName), info
      init.copyIntoKind CastX, info:
        init.copyIntoKind PtrT, info:
          init.addSymUse pool.syms.getOrIncl(RootObjName), info
        init.addSymUse thisParam, info
      init.addIntLit 1, info # field is in superclass

  init.addParRi() # object constructor
  init.addParRi() # assignment

  # the return type is always `Continuation` too:
  dest.addSymUse pool.syms.getOrIncl(ContinuationName), info


proc trCoroutine(c: var Context; dest: var TokenBuf; n: var Cursor; kind: SymKind) =
  var currentProc = ProcContext(kind: IsNormal)
  swap(c.currentProc, currentProc)
  var init = createTokenBuf(20)
  let iter = n
  var paramsEnd = -1
  var paramsBegin = -1
  var origParams = default(Cursor)
  dest.takeToken n # ProcS etc.
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
        patchParamList c, dest, init, sym, paramsBegin, paramsEnd, origParams
    elif i == TypevarsPos:
      isConcrete = n.substructureKind != TypevarsU
    takeTree dest, n

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
      trSons(c, dest, n)
  else:
    takeTree dest, n
  dest.takeParRi n # ProcS
  discard c.procStack.pop()
  c.typeCache.closeScope()
  if isCoroutine:
    generateCoroutineType(c, c.coroTypes, sym)
    generateCoroutineHelpers(c, dest, sym, iter)
  swap(c.currentProc, currentProc)

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of DotToken, EofToken, Ident, SymbolDef,
     IntLit, UIntLit, FloatLit, CharLit, StringLit:
    takeTree dest, n
  of Symbol:
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
    # Pass through unknown tokens conservatively
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
    of ProcS, FuncS, MacroS, MethodS, ConverterS:
      trCoroutine c, dest, n, NoSym
    of IteratorS:
      trCoroutine c, dest, n, IteratorY
    of TemplateS, TypeS, EmitS, BreakS, ContinueS,
      ForS, IncludeS, ImportS, FromimportS, ImportExceptS,
      ExportS, CommentS,
      PragmasS:
      takeTree dest, n
    of YldS:
      trYield c, dest, n
    of RetS, RaiseS:
      if c.currentProc.kind == IsNormal:
        trSons(c, dest, n)
      else:
        trReturn c, dest, n
    of AsgnS:
      trAsgn c, dest, n
    of ScopeS:
      c.typeCache.openScope()
      trSons(c, dest, n)
      c.typeCache.closeScope()
    else:
      case n.exprKind
      of CallKinds - {DelayX}:
        trCall c, dest, n
      of DelayX:
        trDelay c, dest, n
      of Delay0X:
        trDelay0 c, dest, n
      of SuspendX:
        trSuspend c, dest, n
      of TypeofX:
        takeTree dest, n
      else:
        case n.njvlKind
        of LoopV:
          # No suspension points inside this loop → simple while loop
          var beforeBuf = createTokenBuf(32)
          var info = n.info
          inc n
          assert n.stmtKind == StmtsS
          inc n  # enter stmts_before
          while n.kind != ParRi:
            if n.njvlKind in {MflagV, VflagV}:
              trMflag c, dest, n   # hoisted outside while
            else:
              tr c, beforeBuf, n
          inc n  # skip stmts_before ParRi
          var condBuf = createTokenBuf(16)
          tr c, condBuf, n
          var bodyBuf = createTokenBuf(64)
          assert n.stmtKind == StmtsS
          inc n  # enter stmts_body
          while n.kind != ParRi:
            if n.stmtKind == ContinueS:
              skip n
            else:
              tr c, bodyBuf, n
          inc n  # skip stmts_body ParRi
          skipParRi n  # skip loop ParRi
          dest.addParLe WhileS, info
          dest.add condBuf
          dest.addParLe StmtsS, info
          dest.add beforeBuf
          dest.add bodyBuf
          dest.addParRi()
          dest.addParRi()
        of IteV, ItecV:
          var info = n.info
          inc n
          dest.copyIntoKind IfS, info:
            dest.copyIntoKind ElifU, info:
              tr c, dest, n
              tr c, dest, n
            dest.copyIntoKind ElseU, info:
              tr c, dest, n
          inc n
        of MflagV, VflagV:
          trMflag c, dest, n
        of JtrueV:
          trJtrue c, dest, n
        of StoreV:
          # (store value dest) -> (asgn dest value)
          let info = n.info
          inc n # skip 'store' tag
          var valueBuf = createTokenBuf(16)
          tr c, valueBuf, n # value (first operand)
          dest.copyIntoKind AsgnS, info:
            tr c, dest, n   # dest (second operand)
            dest.add valueBuf
          skipParRi n
        of KillV, UnknownV:
          skip n  # NJ bookkeeping, not needed in CPS output
        else:
          case pool.tags[n.tagId]
          of "jmp":
            inc n
            gotoNextState(c, dest, int(pool.integers[n.intId]), n.info)
            inc n
            skipParRi n
          of "lab":
            dest.addParRi() # close stmts
            dest.addParRi() # close proc decl
            inc n
            newLocalProc c, dest, int(pool.integers[n.intId]), c.procStack[^1]
            inc n
            skipParRi n
          else:
            trSons(c, dest, n)
  of ParRi:
    bug "unexpected ')' inside"

proc generateContinuationProcImpl(): Cursor =
  let symId = pool.syms.getOrIncl(ContinuationProcName)
  let impl = programs.tryLoadSym(symId)
  if impl.status == LacksNothing:
    let t = asTypeDecl(impl.decl)
    if t.kind == TypeY:
      return t.body
  return default(Cursor)

proc transformToCps*(pass: var Pass) =
  var n = pass.n  # Extract cursor locally
  var c = Context(thisModuleSuffix: pass.moduleSuffix,
    typeCache: createTypeCache(), coroTypes: createTokenBuf(10),
    continuationProcImpl: generateContinuationProcImpl())
  c.typeCache.openScope()
  assert n.stmtKind == StmtsS
  c.coroTypes.takeToken n
  while n.kind != ParRi:
    tr(c, pass.dest, n)
  for (sym, start) in c.shouldPublish:
    var buf = createTokenBuf(16)
    buf.copyTree pass.dest.cursorAt(start)
    endRead(pass.dest)
    publishSignature buf, sym, 0
  c.coroTypes.add pass.dest # concat coroTypes and other statements
  c.coroTypes.takeToken n # ParRi
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
  var n = beginRead(buf)
  discard transformToCps(n, "slaldpees1")
