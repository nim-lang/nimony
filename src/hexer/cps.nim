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
    yieldConts: Table[int, int]
    labels: Table[int, int]
    cf: TokenBuf
    resultSym: SymId
    upcomingState: int
    counter: int
    subProcs: int  ## number of sub-state procs opened so far
    kind: RoutineKind

  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    procStack: seq[SymId]
    currentProc: ProcContext
    continuationProcImpl: Cursor
    inlineContState: int   ## >= 0 when trIte detected a split inside a branch
    inlineContCursor: Cursor ## cursor to rest-of-branch code after the split point

proc coroTypeForProc(c: Context; procId: SymId): SymId =
  let s = extractVersionedBasename(pool.syms[procId])
  result = pool.syms.getOrIncl(s & ".coro." & c.thisModuleSuffix)

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
        dest.addSymUse sym, info
        emitAllocFrame(c, dest, sym, info)
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
          dest.copyIntoKind AddrX, info:
            dest.addSymUse coroVar, info
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
    let pos = cursorToPosition(c.currentProc.cf, n)
    let state = c.currentProc.yieldConts.getOrDefault(pos, -1)
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
      dest.addSymUse sym, info
      emitAllocFrame(c, dest, sym, info)

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
  # Handles (delay0) — no-arg form: capture continuation for the code that follows.
  # Returns continuation to the NEXT suspension point, not the current one.
  let info = n.info
  let pos = cursorToPosition(c.currentProc.cf, n)  # position of (delay0 token
  inc n      # skip delay0 tag
  var state = c.currentProc.yieldConts.getOrDefault(pos, -1)
  assert state != -1, "delay() no-arg must be a suspension point"
  # Find the next suspension point (if any) - delay captures continuation to resume, not stop
  var searchPos = pos + 1
  while searchPos < c.currentProc.cf.len:
    let nextState = c.currentProc.yieldConts.getOrDefault(searchPos, -1)
    if nextState != -1 and nextState != state:
      state = nextState
      break
    inc searchPos
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
      emitAllocFrame(c, dest, sym, info)
      while n.kind != ParRi:
        tr(c, dest, n)
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

  let field = c.currentProc.localToEnv.getOrDefault(sym)
  if field.def != field.use:
    let info = n.info
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
      else:
        tr(c, dest, n)
    if pcall != SymId(0):
      trPassiveCall c, dest, callExpr, pcall, target

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
  let pos = cursorToPosition(c.currentProc.cf, n)
  let state = c.currentProc.yieldConts.getOrDefault(pos, -1)
  assert state != -1
  let oldState = c.currentProc.upcomingState
  c.currentProc.upcomingState = state
  let info = n.info
  returnValue(c, dest, n, info)
  dest.copyIntoKind RetS, info:
    contNextState(c, dest, state, info)
  c.currentProc.upcomingState = oldState

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
    let pos = cursorToPosition(c.currentProc.cf, n)
    let stateAtPos = c.currentProc.labels.getOrDefault(pos, -1)
    if stateAtPos != -1:
      currentState = stateAtPos

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

proc compileStmtSeq(c: var Context; dest: var TokenBuf; n: var Cursor; continueState: int)

proc trLoop(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Translate NJ `(loop stmts_before cond stmts_body)`.
  ## If the loop has suspension points, it becomes a state-machine loop:
  ##   the loop head gets its own state, and `(continue .)` jumps back to it.
  ## Otherwise it's converted to a simple `(while cond body)`.
  let loopPos = cursorToPosition(c.currentProc.cf, n)
  let loopState = c.currentProc.labels.getOrDefault(loopPos, -1)
  let info = n.info
  inc n  # skip loop tag

  if loopState == -1:
    # No suspension points inside this loop → simple while loop
    var beforeBuf = createTokenBuf(32)
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
  else:
    # Loop has suspension points → compile as state machine.
    # compileStmtSeq already opened the loop head state proc (loopState).
    # We generate: stmts_before inline, condition check, then stmts_body with splits.

    # stmts_before: mflag decls. Lifted mflags (def != use) must NOT be
    # initialized here — this state is re-entered on loop-back and would
    # reset the guard after jtrue set it to true. The aggregate zero-init
    # in the entry function already sets them to false.
    assert n.stmtKind == StmtsS
    inc n  # enter stmts_before
    while n.kind != ParRi:
      if n.njvlKind in {MflagV, VflagV}:
        var flagSym = n
        inc flagSym  # peek past mflag/vflag tag to symdef
        let symId = flagSym.symId
        let field = c.currentProc.localToEnv.getOrDefault(symId)
        if field.def != field.use:
          # Lifted: skip init here; zero-init in entry function handles it
          skip n
        else:
          trMflag c, dest, n
      else:
        tr c, dest, n
    inc n  # skip stmts_before ParRi

    # Condition: (not :g) means loop runs while guard is false.
    # Find the after-loop state to jump to when the loop exits.
    var afterLoopCursor = n
    skip afterLoopCursor  # skip condition
    # afterLoopCursor is now at stmts_body
    skip afterLoopCursor  # skip stmts_body
    # afterLoopCursor is now at loop's ParRi
    let afterLoopParRiPos = cursorToPosition(c.currentProc.cf, afterLoopCursor)
    let afterLoopState = c.currentProc.labels.getOrDefault(afterLoopParRiPos + 1, -1)

    # Emit condition check: the loop condition is (not :g).
    # The loop exits when (not :g) is false = when :g is true.
    # Translate the condition cursor to get the guard sym.
    var condBuf = createTokenBuf(16)
    tr c, condBuf, n  # translates (not :g) into condBuf

    # Emit: if NOT (loop condition): exit loop
    # NOT (not :g) = :g. We emit: if NOT condBuf: exit
    dest.copyIntoKind IfS, info:
      dest.copyIntoKind ElifU, info:
        dest.copyIntoKind NotX, info:
          dest.add condBuf
        dest.copyIntoKind StmtsS, info:
          if afterLoopState != -1:
            gotoNextState c, dest, afterLoopState, info
          else:
            # Loop is the last statement; end iterator/passive proc
            emitReturnCaller(c, dest, info)

    # stmts_body: compile with state splits, passing loopState as continueState
    assert n.stmtKind == StmtsS
    inc n  # enter stmts_body (past StmtsS tag)
    compileStmtSeq c, dest, n, loopState
    # n is now at the ParRi of stmts_body
    inc n  # skip stmts_body ParRi
    skipParRi n  # skip loop ParRi

proc emitTailStmts(c: var Context; dest: var TokenBuf; n: var Cursor; continueState: int) =
  ## Emit statements from n up to ParRi (or a labeled split boundary),
  ## handling continue-as-goto. Updates n. Used to duplicate tail code.
  while n.kind != ParRi:
    let p = cursorToPosition(c.currentProc.cf, n)
    if c.currentProc.labels.getOrDefault(p, -1) >= 0: break
    if n.stmtKind == ContinueS:
      if continueState != -1: gotoNextState c, dest, continueState, n.info
      skip n
    else:
      c.inlineContState = -1
      tr c, dest, n
      c.inlineContState = -1  # ignore nested splits in tail (handled separately)

proc compileStmtSeq(c: var Context; dest: var TokenBuf; n: var Cursor; continueState: int) =
  ## Process a sequence of statements (n is already past the StmtsS opening tag,
  ## stopping when n.kind == ParRi). Handles state splits at label positions and
  ## translates (continue .) to gotoNextState(continueState) when inside a loop.
  let sym = c.procStack[^1]
  while n.kind != ParRi:
    let p = cursorToPosition(c.currentProc.cf, n)
    let state = c.currentProc.labels.getOrDefault(p, -1)
    if state != -1:
      if c.currentProc.subProcs == 0:
        gotoNextState(c, dest, state, n.info)
      else:
        # State procs that fall through to the next state need a return caller
        emitReturnCaller(c, dest, n.info)
      dest.addParRi() # close stmts
      dest.addParRi() # close proc decl
      newLocalProc c, dest, state, sym
      inc c.currentProc.subProcs
    # Handle (continue .) as a loop back-edge
    if n.stmtKind == ContinueS:
      if continueState != -1:
        gotoNextState c, dest, continueState, n.info
      skip n
    else:
      c.inlineContState = -1
      tr c, dest, n
      if c.inlineContState >= 0:
        # A passive call split happened inside a nested ite branch:
        # c.inlineContState = the new continuation state
        # c.inlineContCursor = cursor to rest-of-then code in the branch
        let splitState = c.inlineContState
        let restOfThen = c.inlineContCursor
        c.inlineContState = -1
        # Emit tail stmts (after-ite code) into the current proc (s1 else-path)
        let tailStart = n
        emitTailStmts c, dest, n, continueState
        # Close current proc and open the split continuation state
        emitReturnCaller(c, dest, n.info)
        dest.addParRi() # close stmts
        dest.addParRi() # close proc decl
        newLocalProc c, dest, splitState, sym
        inc c.currentProc.subProcs
        # Emit rest-of-then into new state proc.
        # The label at restOfThen's starting position equals splitState (already consumed
        # by newLocalProc above). Delete it so the recursive compileStmtSeq call doesn't
        # re-open the same state proc.
        var thenN = restOfThen
        c.currentProc.labels.del(cursorToPosition(c.currentProc.cf, thenN))
        compileStmtSeq c, dest, thenN, continueState
        # Emit tail again into the last state proc opened during thenN processing
        var tailN = tailStart
        emitTailStmts c, dest, tailN, continueState
        # Advance n past the tail (already emitted above)
        n = tailN
        break

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
  eliminateJumps(pass)
  when defined(logPasses):
    echo "NJ OUTPUT: ", pass.dest.toString(false)
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

  # Compute suspension points (labels) and mapping from suspension sites to next state.
  c.currentProc.labels = initTable[int, int]()
  c.currentProc.yieldConts = initTable[int, int]()
  var nextLabel = 1  # 0 is the entry function, state labels start at 1
  block gather:
    type LoopEntry = object
      pos: int          ## position of the (loop ...) token
      depth: int        ## depth when the loop was entered (after inc depth)
      containsSusp: bool

    var loopStack: seq[LoopEntry] = @[]
    var scan = beginRead(c.currentProc.cf)
    var depth = 0
    while true:
      let pos = cursorToPosition(c.currentProc.cf, scan)
      if scan.kind == ParLe:
        inc depth
        let nk = scan.njvlKind
        if nk == LoopV:
          loopStack.add(LoopEntry(pos: pos, depth: depth))
        let sk = scan.stmtKind
        let ek = scan.exprKind
        if sk == YldS or (ek in CallKinds - {DelayX} and isPassiveCall(c, c.currentProc.cf[pos+1])) or
            ek == Delay0X or ek == SuspendX:
          # Mark all enclosing loops as containing a suspension point
          for i in 0..<loopStack.len:
            loopStack[i].containsSusp = true
          # Record mapping from the start of this construct to its continuation state
          c.currentProc.yieldConts[pos] = nextLabel
          # Skip to matching ParRi to find the "after statement" label position.
          # If the call is nested inside a local declaration (let/var/result),
          # we need to skip past the enclosing declaration, not just the call.
          var nested = 1
          var j = pos + 1
          while j < c.currentProc.cf.len and nested > 0:
            case c.currentProc.cf[j].kind
            of ParLe: inc nested
            of ParRi: dec nested
            else: discard
            inc j
          # j now points to the token after the call's matching ParRi.
          # If the call was nested inside a local decl (let/var/result),
          # j is still inside the parent stmt. Skip trailing tokens + ParRi
          # to reach the position after the enclosing statement.
          if j < c.currentProc.cf.len and c.currentProc.cf[j].kind == ParRi:
            inc j  # skip the enclosing statement's ParRi
          if j <= c.currentProc.cf.len:
            c.currentProc.labels[j] = nextLabel
            inc nextLabel
        inc scan
      elif scan.kind == ParRi:
        # Check if we're exiting a loop (depth about to drop to below loop's entry depth)
        if loopStack.len > 0 and depth == loopStack[^1].depth:
          let loop = loopStack.pop()
          if loop.containsSusp:
            # Assign a state for the loop head
            c.currentProc.labels[loop.pos] = nextLabel
            let loopState = nextLabel
            inc nextLabel
            # Assign a state for the code after the loop (position after this ParRi)
            c.currentProc.labels[pos + 1] = nextLabel
            inc nextLabel
        dec depth
        if depth == 0: break gather
        inc scan
      else:
        inc scan

  # Analyze which locals escape across suspension points using the same label map
  var n = beginRead(c.currentProc.cf)
  escapingLocals(c, n)

  # Compile the state machine by splitting at label positions
  assert n.stmtKind == StmtsS
  dest.takeToken n
  dest.add init
  declareContinuationResult c, dest, NoLineInfo
  compileStmtSeq c, dest, n, -1

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

proc patchParamList(c: var Context; dest, init: var TokenBuf; sym: SymId;
                    paramsBegin, paramsEnd: int; origParams: Cursor) =
  let info = dest[paramsBegin].info
  var retType = createTokenBuf(4)
  for i in paramsEnd..<dest.len: retType.add dest[i]

  dest.shrink paramsBegin
  let thisParam = pool.syms.getOrIncl(EnvParamName)
  dest.copyIntoKind ParamsU, info:
    # first parameter is always the `this` pointer:
    dest.copyIntoKind ParamU, info:
      dest.addSymDef thisParam, info
      dest.addDotToken() # export
      dest.addDotToken() # pragmas
      dest.copyIntoKind PtrT, info:
        dest.addSymUse coroTypeForProc(c, sym), info
      dest.addDotToken() # default value
    # generate `this[] = ObjConstructor(params)`:
    init.addParLe AsgnS, info
    init.copyIntoKind DerefX, info:
      init.addSymUse thisParam, info
    init.addParLe OconstrX, info
    init.addSymUse coroTypeForProc(c, sym), info

    var n = origParams
    # copy original parameters:
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
  var currentProc = ProcContext(upcomingState: -1, kind: IsNormal)
  swap(c.currentProc, currentProc)
  var init = createTokenBuf(20)
  let iter = n
  var paramsEnd = -1
  var paramsBegin = -1
  var origParams = default(Cursor)
  dest.takeToken n # ProcS etc.
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
    treIteratorBody(c, dest, init, iter, sym)
    skip n # we used the body from the control flow graph
    # Emit implicit final return: deallocFrame + return caller
    emitFinalReturn(c, dest, NoLineInfo)
    dest.addParRi() # stmts
  else:
    takeTree dest, n
  dest.takeParRi n # ProcS
  discard c.procStack.pop()
  c.typeCache.closeScope()
  if isCoroutine:
    generateCoroutineType(c, dest, sym)
  swap(c.currentProc, currentProc)

proc trIteStmts(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## Process a stmts sequence inside an ite branch.
  ## Detects label splits (from passive calls) and sets c.inlineContState/Cursor.
  if n.kind == DotToken:
    # Empty else-branch (no stmts, just a dot token)
    inc n
    return
  assert n.stmtKind == StmtsS
  inc n  # enter stmts
  while n.kind != ParRi:
    let p = cursorToPosition(c.currentProc.cf, n)
    let state = c.currentProc.labels.getOrDefault(p, -1)
    if state != -1:
      # split inside branch — save position and stop emitting into this branch
      c.inlineContState = state
      c.inlineContCursor = n
      while n.kind != ParRi: skip n  # skip rest of branch
      break
    tr c, dest, n
  inc n  # skip stmts ParRi

proc trIte(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n
  var condBuf = createTokenBuf(16)
  tr c, condBuf, n  # condition
  var thenBuf = createTokenBuf(64)
  trIteStmts c, thenBuf, n  # then-branch (may set c.inlineContState)
  let splitState = c.inlineContState
  let splitCursor = c.inlineContCursor
  c.inlineContState = -1
  var elseBuf = createTokenBuf(32)
  trIteStmts c, elseBuf, n  # else-branch
  skipParRi n  # skip ite ParRi
  dest.copyIntoKind IfS, info:
    dest.copyIntoKind ElifU, info:
      dest.add condBuf
      dest.copyIntoKind StmtsS, info:
        dest.add thenBuf
    dest.copyIntoKind ElseU, info:
      dest.copyIntoKind StmtsS, info:
        dest.add elseBuf
  # If a split was detected inside the then-branch, signal it to the caller
  if splitState >= 0:
    c.inlineContState = splitState
    c.inlineContCursor = splitCursor

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
        if n.cfKind == IteF:
          trIte c, dest, n
        else:
          case n.njvlKind
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
          of LoopV:
            trLoop c, dest, n
          of ItecV:
            trIte c, dest, n  # same structure as ite
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
    typeCache: createTypeCache(),
    continuationProcImpl: generateContinuationProcImpl(),
    inlineContState: -1)
  c.typeCache.openScope()
  assert n.stmtKind == StmtsS
  pass.dest.takeToken n
  while n.kind != ParRi:
    tr(c, pass.dest, n)
  pass.dest.takeToken n # ParRi
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
