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
  var itCoroutine: ItCoroutine
  var it = createItCoroutine(addr itCoroutine, 10, addr forLoopVar, StopContinuation)
  while it.fn != nil:
    if it.env == addr itCoroutine:
      echo forLoopVar
    it = scheduler.tick it
    #it.fn(it.env)

]##

import std / [assertions, sets, tables]
include ".." / lib / nifprelude
import ".." / lib / symparser
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, expreval, xints,
  builtintypes, langmodes, renderer, reporters, controlflow, typeprops]
import hexer_context

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
  FnFieldName = "fn.0." & SystemModuleSuffix
  EnvFieldName = "env.0." & SystemModuleSuffix
  CallerFieldName = "caller.0." & SystemModuleSuffix
  ResultParamName = "`result.0"
  ResultFieldNamePrefix = "`result.0."
  CallerParamName = "`caller.0"

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
    reachable: seq[bool]
    resultSym: SymId
    upcomingState: int
    counter: int
    kind: RoutineKind

  Context = object
    afterYieldSym: SymId
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    procStack: seq[SymId]
    currentProc: ProcContext
    continuationProcImpl: Cursor

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
      dest.addSymUse pool.syms.getOrIncl(EnvParamName), info

proc trPassiveCall(c: var Context; dest: var TokenBuf; n: var Cursor; sym: SymId; target: Cursor;
                   inhibitComplete = false) =
  let retType = getType(c.typeCache, n)
  let hasResult = not isVoidType(retType)
  if hasResult:
    assert not cursorIsNil(target), "passive call without target"
  case c.currentProc.kind
  of IsNormal:
    # passive call from within a normal proc:
    #[
    var res: int
    var itCoroutine: ItCoroutine
    complete createItCoroutine(addr itCoroutine, 10, addr res, StopContinuation)
    # we know afterwards the result is available
    ]#
    let info = n.info
    if inhibitComplete:
      dest.addParLe ExprX, info
      dest.addParLe StmtsS, info
    # declare coroutine variable:
    let coroVar = pool.syms.getOrIncl("`coroVar." & $c.currentProc.counter)
    inc c.currentProc.counter
    copyIntoKind dest, VarS, info:
      dest.addSymDef coroVar, info
      dest.addDotToken() # exported
      dest.addDotToken() # pragmas
      dest.addSymUse coroTypeForProc(c, sym), info
      dest.addDotToken() # default value

    if inhibitComplete:
      dest.addParRi() # StmtsS
    else:
      dest.addParLe CallS, info
      dest.addSymUse pool.syms.getOrIncl("complete.0." & SystemModuleSuffix), info

    # emit constructor call:
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
    dest.addParRi() # ExprX or CallS
  of IsIterator, IsPassive:
    # passive call from within a passive proc:
    # We use a single stackframe variable that acts as a union storage
    # for all the different passive calls that might happen.
    # We need to generate code that is very close to a `yield` statement:
    # target = call fn, args
    # -->
    # return fnConstructor(addr this.frame, args, addr target, Continuation(nextState, this))
    let pos = cursorToPosition(c.currentProc.cf, n)
    let state = c.currentProc.yieldConts.getOrDefault(pos, -1)
    assert state != -1
    let info = n.info

    let field: SymId
    # We use the caller `fn` as the key here so that at least the storage is
    # shared between all passive calls to the same function.
    # TODO: We should use some untyped storage here!
    if not c.currentProc.localToEnv.hasKey(sym):
      let coroVar = pool.syms.getOrIncl("`coroVar." & $c.currentProc.counter)
      inc c.currentProc.counter
      field = localToFieldname(c, coroVar)
      c.currentProc.localToEnv[sym] = EnvField(
        objType: coroTypeForProc(c, c.procStack[^1]),
        field: field,
        typeAsSym: coroTypeForProc(c, sym),
        def: -1,
        use: 0)
    else:
      field = c.currentProc.localToEnv[sym].field

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

    # value: emit constructor call:
    copyIntoKind dest, CallS, info:
      dest.addSymUse sym, info

      dest.copyIntoKind AddrX, info:
        dest.copyIntoKind DotX, info:
          dest.copyIntoKind DerefX, info:
            dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
          dest.addSymUse field, info

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

proc trDelay(c: var Context; dest: var TokenBuf; n: var Cursor) =
  inc n
  skip n # skip type; it is `Continuation` and uninteresting here
  const nested = 1

  if n.exprKind in CallKinds and n.firstSon.kind == Symbol:
    let fn = n.firstSon.symId
    trPassiveCall c, dest, n, fn, default(Cursor), inhibitComplete = true
  else:
    dest.copyIntoKind ErrT, n.info:
      dest.addStrLit "`delay` takes a call expression"
    skip n
  for i in 0..<nested:
    skipParRi n

proc passiveCallFn(c: var Context; n: Cursor): SymId =
  if n.exprKind notin CallKinds: return SymId(0)
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
    if sym == c.afterYieldSym:
      contNextState(c, dest, c.currentProc.upcomingState, n.info)
      skip n
    else:
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
  elif isVoidType(getType(c.typeCache, n)):
    tr c, dest, n
  else:
    dest.copyIntoKind AsgnS, info:
      dest.copyIntoKind DerefX, info:
        dest.copyIntoKind DotX, info:
          dest.copyIntoKind DerefX, info:
            dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
          dest.addSymUse pool.syms.getOrIncl(ResultFieldNamePrefix & c.thisModuleSuffix), info
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

proc trReturn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # return x -->
  # this.res[] = x
  # return this.caller
  let info = n.info
  returnValue(c, dest, n, info)
  dest.copyIntoKind RetS, info:
    dest.copyIntoKind DotX, info:
      dest.copyIntoKind DerefX, info:
        dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
      dest.addSymUse pool.syms.getOrIncl(CallerFieldName), info
      dest.addIntLit 1, info # field is in superclass

proc escapingLocals(c: var Context; n: Cursor) =
  if n.kind == DotToken: return
  var currentState = 0
  var n = n
  var nested = 0
  while true:
    if nested <= 1:
      let pos = cursorToPosition(c.currentProc.cf, n)
      let state = c.currentProc.labels.getOrDefault(pos, -1)
      if state != -1:
        currentState = state

    let sk = n.stmtKind
    case sk
    of LocalDecls:
      if sk == ResultS:
        c.currentProc.resultSym = n.symId

      inc n
      let mine = n.symId
      skip n # name
      skip n # exported
      let pragmas = n
      skip n # pragmas
      c.currentProc.localToEnv[mine] = EnvField(
        objType: coroTypeForProc(c, c.procStack[^1]),
        field: if sk == ResultS: pool.syms.getOrIncl(ResultFieldNamePrefix & c.thisModuleSuffix) else: localToFieldname(c, mine),
        pragmas: pragmas,
        typ: n,
        def: currentState,
        use: currentState)
      skip n # type
      inc nested
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

proc treIteratorBody(c: var Context; dest: var TokenBuf; init: TokenBuf; iter: Cursor; sym: SymId) =
  c.currentProc.cf = toControlflow(iter, keepReturns = true)
  c.currentProc.reachable = eliminateDeadInstructions(c.currentProc.cf)

  # Now compute basic blocks considering only reachable instructions
  c.currentProc.labels = initTable[int, int]()
  var nextLabel = 0
  for i in 0..<c.currentProc.cf.len:
    if c.currentProc.reachable[i]:
      if c.currentProc.cf[i].kind == GotoInstr:
        let diff = c.currentProc.cf[i].getInt28
        if i+diff > 0 and i+diff < c.currentProc.cf.len and c.currentProc.reachable[i+diff]:
          c.currentProc.labels[i+diff] = nextLabel
          inc nextLabel
      elif c.currentProc.cf[i].stmtKind == YldS or
          (c.currentProc.cf[i].exprKind in CallKinds and isPassiveCall(c, c.currentProc.cf[i+1])):
        # after a yield we also have a suspension point (a label):
        var nested = 1
        c.currentProc.yieldConts[i] = nextLabel
        for j in i+1..<c.currentProc.cf.len:
          case c.currentProc.cf[j].kind
          of ParLe: inc nested
          of ParRi:
            dec nested
            if nested == 0:
              c.currentProc.labels[j+1] = nextLabel
              inc nextLabel
              break
          else:
            discard

  # analyze which locals are used across basic blocks:
  var n = beginRead(c.currentProc.cf)
  inc n # ProcS
  for i in 0..<BodyPos: skip n
  escapingLocals(c, n)

  # compile the state machine:
  assert n.stmtKind == StmtsS
  dest.takeToken n
  dest.add init
  declareContinuationResult c, dest, NoLineInfo
  var subProcs = 0
  while n.kind != ParRi:
    let pos = cursorToPosition(c.currentProc.cf, n)
    let state = c.currentProc.labels.getOrDefault(pos, -1)
    if state != -1:
      if subProcs == 0:
        gotoNextState(c, dest, state, n.info)
      dest.addParRi() # stmts
      dest.addParRi() # proc decl
      newLocalProc c, dest, state, sym
      inc subProcs
    tr c, dest, n

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
        if value.def != value.use:
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
        init.addSymUse pool.syms.getOrIncl(ResultFieldNamePrefix & c.thisModuleSuffix), info
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
    # treIteratorBody already added the required 2 ParRi tokens
    dest.addParRi() # stmts
  else:
    takeTree dest, n
  dest.takeParRi n # ProcS
  discard c.procStack.pop()
  c.typeCache.closeScope()
  if isCoroutine:
    generateCoroutineType(c, dest, sym)
  swap(c.currentProc, currentProc)

proc trIte(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n
  dest.copyIntoKind IfS, info:
    dest.copyIntoKind ElifU, info:
      tr c, dest, n
      tr c, dest, n
    dest.copyIntoKind ElseU, info:
      tr c, dest, n
  skipParRi n

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of DotToken, EofToken, Ident, SymbolDef,
     IntLit, UIntLit, FloatLit, CharLit, StringLit:
    takeTree dest, n
  of Symbol:
    let field = c.currentProc.localToEnv.getOrDefault(n.symId)
    if field.def != field.use:
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
  of GotoInstr:
    let pos = cursorToPosition(c.currentProc.cf, n)
    # XXX Find a better solution for this.
    if not c.currentProc.reachable[pos]:
      skip n
      return
    let diff = n.getInt28
    let target = pos + diff
    let state = c.currentProc.labels.getOrDefault(target, -1)
    if state != -1:
      gotoNextState(c, dest, state, n.info)
    else:
      bug "goto target not found"
    inc n
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
    of RetS:
      trReturn c, dest, n
    of AsgnS:
      trAsgn c, dest, n
    of ScopeS:
      c.typeCache.openScope()
      trSons(c, dest, n)
      c.typeCache.closeScope()
    else:
      case n.exprKind
      of CallKinds:
        trCall c, dest, n
      of TypeofX:
        takeTree dest, n
      of DelayX:
        trDelay c, dest, n
      else:
        if n.cfKind == IteF:
          trIte c, dest, n
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

proc transformToCps*(n: var Cursor; moduleSuffix: string): TokenBuf =
  var c = Context(thisModuleSuffix: moduleSuffix,
    afterYieldSym: pool.syms.getOrIncl("afterYield.0." & SystemModuleSuffix),
    continuationProcImpl: generateContinuationProcImpl())
  c.typeCache.openScope()
  result = createTokenBuf()
  assert n.stmtKind == StmtsS
  result.takeToken n
  while n.kind != ParRi:
    tr(c, result, n)
  result.takeToken n # ParRi
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
  var buf = parseFromBuffer(inp)
  var n = beginRead(buf)
  discard transformToCps(n, "slaldpees1")
