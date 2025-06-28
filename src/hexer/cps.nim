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
    this = ItCoroutine(x: x, i: 0, caller: caller, dest: dest)
    result = Continuation(fn: itStart, env: this)


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
    echo forLoopVar
    it = it.fn(it.env)

]##

import std / [assertions, sets, tables]
include ".." / lib / nifprelude
import ".." / lib / symparser
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, expreval, xints,
  builtintypes, langmodes, renderer, reporters, controlflow]
import hexer_context

const
  ContinuationProcName = "ContinuationProc.0." & SystemModuleSuffix
  RootObjName = "CoroutineBase.0." & SystemModuleSuffix
  EnvParamName = "`this.0"
  ResultFieldName = "`result.0"
  FnFieldName = "fn.0." & SystemModuleSuffix
  EnvFieldName = "env.0." & SystemModuleSuffix
  CallerFieldName = "caller.0." & SystemModuleSuffix

type
  EnvField = object
    objType: SymId
    field: SymId
    pragmas, typ: Cursor
    def: int
    use: int

  ProcContext = object
    localToEnv: Table[SymId, EnvField]
    yieldConts: Table[int, int]
    labels: Table[int, int]
    cf: TokenBuf
    resultSym: SymId

  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    procStack: seq[SymId]
    currentProc: ProcContext

proc coroTypeForProc(c: Context; procId: SymId): SymId =
  let s = extractVersionedBasename(pool.syms[procId])
  result = pool.syms.getOrIncl(s & ".coro." & c.thisModuleSuffix)

proc stateToProcName(c: Context; sym: SymId; state: int): SymId =
  let s = extractVersionedBasename(pool.syms[sym])
  result = pool.syms.getOrIncl(s & "." & $state & "." & c.thisModuleSuffix)

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

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # XXX To implement
  trSons(c, dest, n)

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    if kind == ResultY:
      c.currentProc.resultSym = n.symId
    c.typeCache.takeLocalHeader(dest, n, kind)
    tr(c, dest, n)

proc newLocalProc(c: var Context; dest: var TokenBuf; state: int; sym: SymId) =
  const info = NoLineInfo
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
  dest.addSymUse pool.syms.getOrIncl("Continuation.0." & SystemModuleSuffix), info
  dest.addDotToken() # pragmas
  dest.addDotToken() # effects
  dest.addParLe StmtsS, info # body

proc returnNextState(c: var Context; dest: var TokenBuf; state: int; info: PackedLineInfo) =
  dest.copyIntoKind RetS, info:
    dest.copyIntoKind OconstrX, info:
      dest.copyIntoKind KvU, info:
        dest.addSymUse pool.syms.getOrIncl(FnFieldName), info
        dest.copyIntoKind CastX, info:
          dest.addSymUse pool.syms.getOrIncl(ContinuationProcName), info
          dest.addSymUse stateToProcName(c, c.procStack[^1], state), info
      dest.copyIntoKind KvU, info:
        dest.addSymUse pool.syms.getOrIncl(EnvFieldName), info
        dest.addSymUse pool.syms.getOrIncl(EnvParamName), info

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
  let info = n.info
  returnValue(c, dest, n, info)
  returnNextState(c, dest, state, info)

proc trReturn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # return x -->
  # this.res[] = x
  # return this.caller
  let info = n.info
  returnValue(c, dest, n, info)
  dest.copyIntoKind RetS, info:
      dest.copyIntoKind DerefX, info:
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

    case n.stmtKind
    of LocalDecls:
      inc n
      let mine = n.symId
      skip n # name
      skip n # exported
      let pragmas = n
      skip n # pragmas
      c.currentProc.localToEnv[mine] = EnvField(
        objType: coroTypeForProc(c, c.procStack[^1]),
        field: localToFieldname(c, mine),
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
        if c.currentProc.localToEnv.hasKey(n.symId):
          c.currentProc.localToEnv[n.symId].use = currentState
      else:
        discard
      inc n

proc treIteratorBody(c: var Context; dest: var TokenBuf; init: TokenBuf; iter: Cursor; sym: SymId) =
  c.currentProc.cf = toControlflow(iter, keepReturns = true)
  let reachable = eliminateDeadInstructions(c.currentProc.cf)

  # Now compute basic blocks considering only reachable instructions
  c.currentProc.labels = initTable[int, int]()
  var nextLabel = 0
  for i in 0..<c.currentProc.cf.len:
    if reachable[i]:
      if c.currentProc.cf[i].kind == GotoInstr:
        let diff = c.currentProc.cf[i].getInt28
        if diff > 0 and i+diff < c.currentProc.cf.len and reachable[i+diff]:
          c.currentProc.labels[i+diff] = nextLabel
          inc nextLabel
      elif c.currentProc.cf[i].stmtKind == YldS:
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
          else:
            discard

  # analyze which locals are used across basic blocks:
  var n = beginRead(c.currentProc.cf)
  escapingLocals(c, n)

  # compile the state machine:
  assert n.stmtKind == StmtsS
  dest.takeToken n
  while n.kind != ParRi:
    let pos = cursorToPosition(c.currentProc.cf, n)
    let state = c.currentProc.labels.getOrDefault(pos, -1)
    if state != -1:
      dest.addParRi() # stmts
      dest.addParRi() # proc decl
      newLocalProc c, dest, state, sym
    tr c, dest, n
  dest.takeToken n # ParRi

proc trCoroutine(c: var Context; dest: var TokenBuf; n: var Cursor; kind: SymKind) =
  var currentProc = ProcContext()
  swap(c.currentProc, currentProc)
  var init = createTokenBuf(10)
  let iter = n
  copyInto dest, n:
    var isConcrete = true # assume it is concrete
    let sym = n.symId
    c.procStack.add(sym)
    let closureOwner = c.procStack[0]
    var isClosure = false
    for i in 0..<BodyPos:
      if i == ParamsPos:
        c.typeCache.openProcScope(sym, n)
      elif i == ProcPragmasPos:
        if kind == IteratorY and hasPragma(n, ClosureP):
          isClosure = true
      elif i == TypevarsPos:
        isConcrete = n.substructureKind != TypevarsU
      takeTree dest, n

    if isConcrete and isClosure:
      treIteratorBody(c, dest, init, iter, sym)
    else:
      takeTree dest, n
    discard c.procStack.pop()
  c.typeCache.closeScope()
  swap(c.currentProc, currentProc)

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of DotToken, EofToken, Ident, SymbolDef,
     IntLit, UIntLit, FloatLit, CharLit, StringLit:
    takeTree dest, n
  of Symbol:
    let field = c.currentProc.localToEnv.getOrDefault(n.symId)
    if field.def != field.use:
      let info = n.info
      dest.copyIntoKind DotX, info:
        dest.copyIntoKind DerefX, info:
          dest.addSymUse pool.syms.getOrIncl(EnvParamName), info
        dest.addSymUse field.field, info
    else:
      takeTree dest, n
  of GotoInstr:
    let diff = n.getInt28
    let target = cursorToPosition(c.currentProc.cf, n) + diff
    let state = c.currentProc.labels.getOrDefault(target, -1)
    if state != -1:
      gotoNextState(c, dest, state, n.info)
    else:
      bug "goto target not found"
  of ParLe:
    case n.stmtKind
    of LocalDecls:
      trLocal c, dest, n
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
      else:
        trSons(c, dest, n)
  of ParRi:
    bug "unexpected ')' inside"

proc transformToCps*(n: var Cursor; moduleSuffix: string): TokenBuf =
  var c = Context(thisModuleSuffix: moduleSuffix)
  c.typeCache.openScope()
  result = createTokenBuf()
  assert n.stmtKind == StmtsS
  result.takeToken n
  while n.kind != ParRi:
    tr(c, result, n)
  result.takeToken n # ParRi
  c.typeCache.closeScope()
