#
#
#           Nimony Jump Elimination Pass
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Compiles Nimony IR to a simpler IR called [NJVL](doc/njvl.md) that does not contain jumps.

import std / [tables, sets, assertions]
include ".." / lib / nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav, typeprops, builtintypes]
import ".." / hexer / [xelim, mover, passes]
import njvl_model

#[
Introducing cfvars is more complex than it looks.

Consider:

while true:
  if cond:
    if condB:
      break
    more()
  code()

# --> not only does `while` have to understand `break`, also the outer `if` is affected!

while true:
  if cond:
    if condB:
      jtrue loopGuard
    if not loopGuard:
      more()
  if not loopGuard:
    code()

It gets slightly worse:

while true:
  if cond:
    if condB:
      break
    else:
      return
    more()
  code()

# --->

while true:
  if cond:
    if condB:
      jtrue loopGuard
    else:
      jtrue loopGuard, retFlag
    if not loopGuard:
      more()
  if not loopGuard:
    code()

We solve this problem in the `trGuardedStmts` proc. The `jtrue` instruction
can now set multiple guards at once and we ensure that leaving an outer block
also always implies leaving all inner blocks! This way we can always use a single guard
condition and do not have to synthesize one with `or`!

# Example Analysis

while true:           # loopGuard
  if cond:            # ifGuard
    if condB:         # innerGuard
      break           # Sets: loopGuard=true, ifGuard=true, innerGuard=true
    more()            # This should be guarded by ifGuard (not innerGuard)
  code()              # This should be guarded by loopGuard (not ifGuard)

# Missing else exploit

If the `else` is missing or empty, it's often preferable to generate one
so that:

if cond:
  break
code()

Becomes:

if cond:
  break
else:
  code()

This is especiall important for exception handling which always has an empty `else`
branch otherwise:

f()
f2()

Is turned directly into:

f()
if failed:
  raise
else:
  f2()

This avoids most of the overhead of control flow variables by construction and keeps
our dominator trees more precise. In order to do this reliably we pass the
"current basic block" around as a parameter.

]#

type
  Guard = object
    cond: SymId
    blockName: SymId # used for named `block` statements
    active: bool
    isTryGuard: bool

  ExceptionMode* = enum
    NoRaise     # proc/call cannot raise
    VoidRaise   # proc/call can raise, but is void
    TupleRaise  # proc/call can raise, and has a return value so it becomes a tuple

  BasicBlock = object
    ## Tracks the else-exploitation state within a statement list.
    ##
    ## Invariants:
    ## - `openElseBranches` counts how many `(stmts` tokens have been emitted
    ##   as else branches of ites that were left open for subsequent statements.
    ##   `closeBasicBlock` must close exactly this many before leaving the block.
    ## - `leavesWith` is set by break/return/raise to the innermost guard they
    ##   activated. It tells the parent (e.g. trIf) that the then-branch ended
    ##   with a leaving statement, enabling else exploitation for the no-else case.
    ## - `reenableOnLeave` records guards that were deactivated during else
    ##   exploitation and must be re-enabled when the basic block closes.
    ##   Each entry `(idx, cond)` is validated: the guard at `idx` must still
    ##   have `cond` as its condition (guards are a stack, so this can go stale
    ##   if guards were removed).
    openElseBranches: int
    leavesWith: int # index to the innermost guard that we activated or -1
    hasParLe: bool
    reenableOnLeave: seq[(int, SymId)]

  CurrentProc = object
    mode: ExceptionMode
    resultSym: SymId
    errorTracker: SymId # tracks error codes in try blocks (can differ from resultSym)
    guards: seq[Guard]
    tmpCounter: int
    returnType: Cursor
    tupleVars: HashSet[SymId] # variables that have been expanded to a tuple due to exception handling

  Context* = object
    typeCache: TypeCache
    counter: int
    thisModuleSuffix: string
    raisesResolved: bool  ## True when eraiser has already run; prevents double-injecting raise checks
    current: CurrentProc
    callFirstArgs: Table[SymId, TokenBuf] ## Maps local syms to the first argument of their init call (for borrow tracking)

proc addParLe*(dest: var TokenBuf; kind: NjvlKind;
               info = NoLineInfo) =
  dest.add parLeToken(cast[TagId](kind), info)

proc openScope(c: var Context) =
  c.typeCache.openScope()

proc closeScope(c: var Context; dest: var TokenBuf; info: PackedLineInfo) =
  # insert kill instructions:
  var i = 0
  for s in c.typeCache.currentScopeLocals:
    if i == 0:
      dest.add tagToken("kill", info)
    dest.addSymUse s, info
    inc i
  if i > 0:
    dest.addParRi()
  c.typeCache.closeScope()

proc closeBasicBlock(c: var Context; b: var BasicBlock; dest: var TokenBuf) =
  ## Close all else branches opened by else exploitation within this block.
  ## Post-condition: openElseBranches == 0 and all borrowed guards are returned.
  while b.openElseBranches > 0:
    dest.addParRi() # close `else` branch (stmts)
    dest.addParRi() # close ite
    dec b.openElseBranches
  assert b.openElseBranches == 0
  for (idx, cond) in b.reenableOnLeave:
    if idx < c.current.guards.len and
        cond == c.current.guards[idx].cond:
      # Re-enable the guard that was deactivated during else exploitation.
      # The guard was "borrowed" by openElseBranch — we return it here.
      c.current.guards[idx].active = true

proc openElseBranch(b: var BasicBlock; dest: var TokenBuf; info: PackedLineInfo) =
  ## Open an else branch on the current ite, deferring its closure to `closeBasicBlock`.
  ## This is the core of else exploitation: subsequent statements in the same block
  ## are emitted inside this else branch rather than after the ite.
  ## Pre-condition: an ite's then-branch has just been emitted (ending with jtrue).
  dest.addParLe StmtsS, info # begin of else branch
  inc b.openElseBranches

proc maybeEmitGuard(c: var Context; dest: var TokenBuf; info: PackedLineInfo): (int, SymId) =
  ## If any guard is active, emit `(ite (not guard) (stmts` and temporarily
  ## disable the guard. Returns `(index, sym)` of the disabled guard, or
  ## `(-1, NoSymId)` if no guard was active.
  ##
  ## The guard is "borrowed": disabled here, re-enabled by `maybeCloseGuard`.
  ## Between emit and close, nested code runs without seeing this guard,
  ## preventing double-wrapping.
  result = (-1, NoSymId)
  for i in countdown(c.current.guards.len - 1, 0):
    let g = addr c.current.guards[i]
    if g.active:
      dest.add tagToken("ite", info)
      dest.copyIntoKind NotX, info:
        dest.addSymUse g.cond, info
      result = (i, g.cond)
      g.active = false # borrow: disable until maybeCloseGuard returns it
      dest.addParLe StmtsS, info # then section
      break

proc maybeCloseGuard(c: var Context; dest: var TokenBuf; g: (int, SymId); mustCloseScope: bool) =
  ## Close the guard opened by `maybeEmitGuard`, re-enabling it.
  ## Post-condition: if a guard was emitted (g[0] >= 0), it is active again.
  let idx = g[0]
  if idx >= 0:
    if mustCloseScope:
      closeScope c, dest, NoLineInfo
    dest.addParRi() # close then section (stmts)
    dest.addDotToken() # no else section
    dest.addParRi() # close ite
    if idx < c.current.guards.len and
        g[1] == c.current.guards[idx].cond:
      # Return the borrowed guard. It may have been re-activated by raiseGuards
      # or jtrue inside the child statement — that's expected and harmless.
      c.current.guards[idx].active = true
  else:
    if mustCloseScope:
      closeScope c, dest, NoLineInfo

proc trGuardedStmts(c: var Context; b: var BasicBlock; dest: var TokenBuf; n: var Cursor; mustCloseScope: bool)
proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc declareCfVar(c: var Context; dest: var TokenBuf; s: SymId) =
  dest.add tagToken("mflag", NoLineInfo)
  dest.addSymDef s, NoLineInfo
  dest.addParRi()
  c.typeCache.registerLocal(s, VarY, c.typeCache.builtins.boolType)

proc useErrorTracker(c: Context; dest: var TokenBuf; errorTracker: SymId; info: PackedLineInfo) =
  ## Emit the correct expression to read the error code from errorTracker.
  ## In TupleRaise mode, errorTracker is a tuple and we need (tupat errorTracker +0).
  ## In VoidRaise/NoRaise mode, errorTracker is a plain ErrorCode variable.
  assert errorTracker != NoSymId
  if c.current.mode == TupleRaise:
    dest.addParLe EtupatV, info
    dest.addSymUse errorTracker, info
    dest.addIntLit 0, info
    dest.addParRi()
  else:
    dest.addSymUse errorTracker, info

proc storeToErrorTracker(c: var Context; dest: var TokenBuf; value: var Cursor; info: PackedLineInfo) =
  ## Emit the correct store to set the error code in errorTracker from a source expression.
  ## In TupleRaise mode, store to (tupat errorTracker +0).
  ## In VoidRaise/NoRaise mode, store directly to errorTracker.
  assert c.current.errorTracker != NoSymId
  dest.copyIntoKind StoreV, info:
    trExpr c, dest, value
    if c.current.mode == TupleRaise:
      dest.addParLe EtupatV, info
      dest.addSymUse c.current.errorTracker, info
      dest.addIntLit 0, info
      dest.addParRi()
    else:
      dest.addSymUse c.current.errorTracker, info

proc storeConstToErrorTracker(c: Context; dest: var TokenBuf; tracker, constSym: SymId; info: PackedLineInfo) =
  ## Store a constant (like Success) to errorTracker.
  assert constSym != NoSymId
  assert tracker != NoSymId
  dest.copyIntoKind StoreV, info:
    dest.addSymUse constSym, info
    if c.current.mode == TupleRaise:
      dest.addParLe EtupatV, info
      dest.addSymUse tracker, info
      dest.addIntLit 0, info
      dest.addParRi()
    else:
      dest.addSymUse tracker, info

proc declareResultVar(dest: var TokenBuf; s: SymId; info: PackedLineInfo) =
  copyIntoKind dest, ResultS, info:
    dest.addSymDef s, info
    dest.addDotToken() # export marker
    dest.addDotToken() # pragmas
    dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), info # type
    dest.addDotToken() # no value
  # start with: `result -> success` assignment/store instruction
  copyIntoKind dest, StoreV, info:
    dest.addSymUse pool.syms.getOrIncl(SuccessName), info
    dest.addSymUse s, info

proc trResultExpr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # we know it will be bound to `result` here:
  let info = n.info
  case c.current.mode
  of VoidRaise:
    if n.kind == DotToken:
      inc n
      dest.addSymUse pool.syms.getOrIncl(SuccessName), info
    else:
      trExpr c, dest, n
  of TupleRaise:
    # wrap it a tuple constructor:
    copyIntoKind dest, TupconstrX, info:
      copyIntoKind dest, TupleT, info:
        dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), info
        dest.copyTree c.current.returnType
      dest.addSymUse pool.syms.getOrIncl(SuccessName), info
      trExpr c, dest, n
  of NoRaise:
    if n.kind == DotToken:
      inc n
    else:
      trExpr c, dest, n

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  var r = asRoutine(n)
  let oldProc = move c.current
  c.current = CurrentProc(tmpCounter: 1, returnType: r.retType)
  if hasPragma(r.pragmas, RaisesP):
    # Iterators don't have a `result` variable (semdecls.declareResult skips them)
    # and their bodies are not tuple-transformed by eraiser/constparams.
    # Use VoidRaise so we create a synthetic error-tracking variable instead.
    if isVoidType(r.retType) or (r.kind == IteratorY and not hasPragma(r.pragmas, ClosureP)):
      c.current.mode = VoidRaise
    else:
      c.current.mode = TupleRaise
  else:
    c.current.mode = NoRaise

  let retFlag = pool.syms.getOrIncl("´r.0")
  c.current.guards.add Guard(cond: retFlag, active: false)

  copyInto(dest, n):
    let isConcrete = c.typeCache.takeRoutineHeader(dest, decl, n)
    if isConcrete:
      let symId = r.name.symId
      if isLocalDecl(symId):
        c.typeCache.registerLocal(symId, r.kind, decl)
      c.typeCache.openScope()
      let info = n.info
      copyIntoKind dest, StmtsS, info:
        var b = BasicBlock(openElseBranches: 0, hasParLe: true, leavesWith: -1)
        openScope c
        # if this is a void proc that `.raises` we add a `result` variable as we actually need to return something
        if c.current.mode == VoidRaise:
          c.current.resultSym = pool.syms.getOrIncl("`result." & $c.current.tmpCounter)
          inc c.current.tmpCounter
          declareResultVar dest, c.current.resultSym, info
          # By default, errorTracker is the same as resultSym
          c.current.errorTracker = c.current.resultSym

        declareCfVar c, dest, retFlag
        trGuardedStmts c, b, dest, n, false
        closeBasicBlock c, b, dest
        closeScope c, dest, info
      c.typeCache.closeScope()
    else:
      takeTree dest, n
  c.current = ensureMove oldProc

type
  CallInfo = object
    isNoReturn: bool
    mode: ExceptionMode
    mutates: seq[SymId]
    info: PackedLineInfo

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor): CallInfo =
  let info = n.info
  var fnType = skipProcTypeToParams(getType(c.typeCache, n.firstSon))
  assert isParamsTag(fnType)
  var pragmas = fnType
  skip pragmas
  let retType = pragmas
  skip pragmas
  let canRaise = hasPragma(pragmas, RaisesP)
  let isNoReturn = hasPragma(pragmas, NoreturnP)

  result = CallInfo(isNoReturn: isNoReturn, mode:
    if canRaise:
      (if isVoidType(retType): VoidRaise else: TupleRaise)
    else: NoRaise,
    info: info
  )
  dest.add n
  inc n # skip `(call)`
  trExpr c, dest, n # handle `fn`
  while n.kind != ParRi:
    if n.exprKind == HaddrX:
      let r = rootOf(n, CanFollowCalls)
      if r != NoSymId:
        result.mutates.add r
    trExpr c, dest, n
  dest.takeParRi n

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of Symbol:
    if c.current.tupleVars.contains(n.symId):
      let info = n.info
      copyIntoKind dest, EtupatV, info:
        dest.addSymUse n.symId, info
        dest.addIntLit 1, info
      inc n
    else:
      dest.takeToken n
  of UnknownToken, EofToken, DotToken, Ident, SymbolDef, StringLit, CharLit, IntLit, UIntLit, FloatLit:
    dest.takeToken n
  of ParLe:
    case n.exprKind
    of CallKinds:
      bug "call must have been bound to a location"
    of AndX, OrX:
      bug "and/or should have been handled by the expression elimination pass xelim.nim"
    else:
      dest.takeToken n
      while n.kind != ParRi:
        trExpr c, dest, n
      dest.takeToken n
  of ParRi: bug "Unmatched ParRi"

proc emitReturnGuards(c: var Context; dest: var TokenBuf; info: PackedLineInfo) =
  dest.add tagToken("jtrue", info)
  # we also need to break out of everything:
  for i in countdown(c.current.guards.len - 1, 0):
    let cond = c.current.guards[i].cond
    assert cond != NoSymId
    c.current.guards[i].active = true
    dest.addSymUse cond, info
  dest.addParRi()

proc callIsOver(c: var Context; dest: var TokenBuf; callInfo: CallInfo) =
  # we make `unknown` part of the `call` for now. This will be cleaned up
  # in the `versionizer` pass!
  for s in callInfo.mutates:
    dest.add tagToken("unknown", callInfo.info)
    dest.addSymUse s, callInfo.info
    dest.addParRi() # unknown
  if callInfo.isNoReturn:
    emitReturnGuards(c, dest, callInfo.info)

proc trBoundExpr(c: var Context; dest: var TokenBuf; n: var Cursor): CallInfo =
  # Indicates that the expression is about to be bound to a location.
  # Hence a `call` expression is valid here.
  if n.exprKind in CallKinds:
    result = trCall(c, dest, n)
  else:
    trExpr c, dest, n
    result = CallInfo(isNoReturn: false, mode: NoRaise, mutates: @[], info: n.info)

proc raiseGuards(c: var Context; dest: var TokenBuf; info: PackedLineInfo) =
  let before = dest.len
  dest.add tagToken("jtrue", info)
  var produced = 0
  # Break out of everything up to and INCLUDING the innermost try guard.
  # The try guard itself must be set so the except handler fires.
  for i in countdown(c.current.guards.len - 1, 0):
    let cond = c.current.guards[i].cond
    assert cond != NoSymId
    c.current.guards[i].active = true
    dest.addSymUse cond, info
    inc produced
    if c.current.guards[i].isTryGuard:
      break  # include the try guard, then stop (don't propagate further up)
  dest.addParRi()
  if produced == 0: dest.shrink before

proc trStmtCall(c: var Context; b: var BasicBlock; dest: var TokenBuf; n: var Cursor) =
  let before = dest.len
  let info = n.info
  let callInfo = trCall(c, dest, n)
  case callInfo.mode
  of NoRaise:
    discard "nothing to do"
  of VoidRaise:
    # we need to bind the call to a temporary!
    var target = createTokenBuf(10)
    target.copyTree cursorAt(dest, before)
    endRead dest
    dest.shrink before

    let s = pool.syms.getOrIncl("`g." & $c.current.tmpCounter)
    inc c.current.tmpCounter
    copyIntoKind dest, LetS, info:
      dest.addSymDef s, info
      dest.addDotToken() # export marker
      dest.addDotToken() # pragmas
      dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), info # type
      dest.add target

    # Manually emit ite to control whether we close it (for optimization)
    dest.add tagToken("ite", info)
    dest.addSymUse s, info # XXX write that later as `e != Success`
    dest.copyIntoKind StmtsS, info: # then-branch
      raiseGuards(c, dest, info)
    openElseBranch b, dest, info

  of TupleRaise:
    bug "value should have been discarded"
  callIsOver(c, dest, callInfo)

proc replayLocalHeader(c: var Context; dest: var TokenBuf; n: Cursor) =
  var n = n
  dest.takeToken n
  takeTree dest, n # name
  takeTree dest, n # export marker
  takeTree dest, n # pragmas
  dest.copyIntoKind TupleT, n.info:
    dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), n.info
    takeTree dest, n # type
  takeTree dest, n # value
  dest.takeParRi n

proc trLocal(c: var Context; b: var BasicBlock; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  let beforeHead = dest.len
  dest.takeToken n

  let symId = n.symId
  if kind == ResultY:
    c.current.resultSym = symId
    # For TupleRaise mode, errorTracker should also point to result
    if c.current.mode == TupleRaise:
      c.current.errorTracker = symId

  takeTree dest, n # name
  takeTree dest, n # export marker
  takeTree dest, n # pragmas
  c.typeCache.registerLocal(symId, kind, n)
  takeTree dest, n # type

  # Record first argument of call inits for borrow tracking (used by trFor):
  if n.kind == ParLe and n.exprKind in CallKinds:
    var tmp = n
    inc tmp # skip call tag
    skip tmp # skip callee
    if tmp.kind != ParRi: # has at least one argument
      var argBuf = createTokenBuf(8)
      argBuf.addSubtree tmp
      c.callFirstArgs[symId] = argBuf

  let info = n.info
  let callInfo = trBoundExpr(c, dest, n)
  skipParRi n
  # After eraiser, every raiseable local init already has an explicit
  # `if (failed x): raise x` injected after it, so NJ must not add another check.
  let effectiveMode = if c.raisesResolved: NoRaise else: callInfo.mode
  case effectiveMode
  of NoRaise:
    dest.addParRi()
    callIsOver(c, dest, callInfo)
  of VoidRaise:
    dest.addParRi()
    callIsOver(c, dest, callInfo)
  of TupleRaise:
    # we also need to patch the type!
    dest.addParRi()
    callIsOver(c, dest, callInfo)
    var decl = createTokenBuf(10)
    decl.copyTree cursorAt(dest, beforeHead)
    endRead dest
    dest.shrink beforeHead
    replayLocalHeader(c, dest, beginRead(decl))

    # Track guard state before emitting the error-check ite
    #let guardBefore = if c.optimizeIte: c.current.iteOpt.getLastActivated() else: InvalidGuardRef

    c.current.tupleVars.incl(symId)

    # Manually emit ite to control whether we close it (for optimization)
    dest.add tagToken("ite", info)
    dest.addParLe EtupatV, info # condition
    dest.addSymUse symId, info # XXX write that later as `e != Success`
    dest.addIntLit 0, info
    dest.addParRi() # close tupat
    dest.copyIntoKind StmtsS, info:
      # then-branch
      raiseGuards(c, dest, info)

    openElseBranch b, dest, info

proc trAsgn(c: var Context; b: var BasicBlock; dest: var TokenBuf; n: var Cursor) =
  # we translate `(asgn X Y)` to `(store Y X)` as it's easier to analyze,
  # it reflects the actual evaluation order.
  let info = n.info
  dest.add tagToken("store", info)
  inc n
  if n.kind == Symbol:
    let symId = n.symId
    inc n # skip `result`:
    if n.exprKind in CallKinds:
      # result = f() where xelim preserved the call without a temp:
      # Use trBoundExpr so we store the whole (ErrorCode, T) tuple into result,
      # then emit the "was successful?" branching afterwards.
      let callInfo = trBoundExpr(c, dest, n)
      dest.addSymUse symId, info
      skipParRi n
      dest.addParRi()
      callIsOver(c, dest, callInfo)
      case callInfo.mode
      of NoRaise:
        discard
      of TupleRaise:
        # result now holds (ErrorCode, T); check the error code and propagate.
        dest.add tagToken("ite", info)
        dest.addParLe EtupatV, info
        dest.addSymUse symId, info
        dest.addIntLit 0, info
        dest.addParRi()
        dest.copyIntoKind StmtsS, info:
          raiseGuards(c, dest, info)
        openElseBranch b, dest, info
      of VoidRaise:
        bug "void-raise call on right-hand side of result assignment"
      return
    elif symId == c.current.resultSym:
      trResultExpr c, dest, n
    else:
      trExpr c, dest, n
    # add `result` later due to the changed order for `(store X result)`
    dest.addSymUse symId, info
  else:
    var rhs = n
    skip rhs
    trExpr c, dest, rhs
    trExpr c, dest, n # lhs
    n = rhs
  skipParRi n
  dest.addParRi()

proc countSons(dest: var TokenBuf; d: int): int =
  var n = cursorAt(dest, d)
  result = 0
  assert n.kind == ParLe
  inc n
  while n.kind != ParRi:
    skip n
    inc result
  endRead(dest)

proc trIf(c: var Context; outerB: var BasicBlock; dest: var TokenBuf; n: var Cursor) =
  # Precondition: xelim already produced a single elif-else construct here
  let info = n.info
  dest.add tagToken("ite", info)
  inc n
  assert n.substructureKind == ElifU
  inc n
  trExpr c, dest, n

  openScope c
  var b = BasicBlock(openElseBranches: 0, hasParLe: false, leavesWith: -1)
  trGuardedStmts c, b, dest, n, true
  closeBasicBlock c, b, dest
  skipParRi n # end of `elif`

  if n.kind != ParRi:
    # --- Case 1: Explicit else branch ---
    assert n.substructureKind == ElseU
    inc n
    openScope c
    var oldActive = false
    if b.leavesWith >= 0:
      # The then-branch ended with a leaving statement (break/return/raise)
      # that activated guard `b.leavesWith`. Disable it during else processing
      # because the else branch only executes when the then-branch was NOT taken,
      # so the jtrue didn't fire and the guard isn't set at runtime.
      assert b.leavesWith < c.current.guards.len, "leavesWith out of range"
      oldActive = c.current.guards[b.leavesWith].active
      c.current.guards[b.leavesWith].active = false

    var thenB = BasicBlock(openElseBranches: 0, hasParLe: false, leavesWith: -1)
    trGuardedStmts c, thenB, dest, n, true
    closeBasicBlock c, thenB, dest
    skipParRi n
    dest.takeParRi n # "ite"

    if b.leavesWith >= 0:
      # Restore the guard to its pre-else state.
      c.current.guards[b.leavesWith].active = oldActive

  elif b.leavesWith >= 0:
    # --- Case 2: No else branch, then-branch ended with a leaving statement ---
    # Else exploitation: the code AFTER this if becomes the else branch.
    # This is correct because if the then-branch was taken, jtrue fired and
    # subsequent guarded code is skipped. If the then-branch was NOT taken,
    # we fall through to the else which runs the subsequent code.
    skipParRi n
    assert b.leavesWith < c.current.guards.len, "leavesWith out of range"
    c.current.guards[b.leavesWith].active = false
    outerB.reenableOnLeave.add ((b.leavesWith, c.current.guards[b.leavesWith].cond))
    openElseBranch outerB, dest, info
  else:
    # --- Case 3: No else branch, normal completion ---
    dest.addDotToken() # no else section
    dest.takeParRi n # "ite"

proc trBreak(c: var Context; b: var BasicBlock; dest: var TokenBuf; n: var Cursor) =
  ## Emit `(jtrue guard1 guard2 ...)` and activate the guards.
  ## Sets `b.leavesWith` to the innermost guard so `trIf` knows the
  ## then-branch ended with a leaving statement.
  assert c.current.guards.len > 0, "break outside any guarded scope"

  var entries = 0 # only care about the inner most
  inc n
  if n.kind == ParRi:
    entries = 1
  elif n.kind == DotToken:
    inc n
    inc entries
  elif n.kind == Symbol:
    for i in countdown(c.current.guards.len - 1, 0):
      inc entries
      if c.current.guards[i].blockName == n.symId: break
    inc n
  else:
    bug "invalid `break` structure"

  assert entries > 0, "break resolved to zero guard entries"
  b.leavesWith = c.current.guards.len-1
  dest.add tagToken("jtrue", n.info)
  for i in 1..entries:
    let guardIdx = c.current.guards.len - i
    assert guardIdx >= 0, "guard index underflow in break"
    let g = addr c.current.guards[guardIdx]
    dest.addSymUse g.cond, n.info
    g.active = true
  dest.takeParRi n

type
  GuardUndoState = object
    at: int

proc addGuard(c: var Context; g: Guard): GuardUndoState =
  result = GuardUndoState(at: c.current.guards.len)
  c.current.guards.add g

proc removeGuard(c: var Context; s: GuardUndoState) =
  c.current.guards.shrink(s.at)

proc trBlock(c: var Context; outerB: BasicBlock; dest: var TokenBuf; n: var Cursor) =
  let guard = pool.syms.getOrIncl("´g." & $c.current.tmpCounter)
  inc c.current.tmpCounter

  declareCfVar c, dest, guard
  inc n # "block"
  let blockName = if n.kind == SymbolDef: n.symId else: NoSymId
  inc n # name or empty
  openScope c
  let s = addGuard(c, Guard(cond: guard, active: false, blockName: blockName))

  var b = BasicBlock(openElseBranches: 0, hasParLe: outerB.hasParLe, leavesWith: -1)
  trGuardedStmts c, b, dest, n, true
  closeBasicBlock c, b, dest
  removeGuard c, s
  skipParRi n

proc findBreakSplitPoint(n: Cursor): int =
  # search for pattern `if cond: break` as all statements before that
  # can be considered to be part of the pre-condition of the loop.
  var n = n
  assert n.stmtKind == StmtsS
  inc n # stmtList
  result = 0
  while n.kind != ParRi:
    if n.stmtKind == IfS:
      inc n
      assert n.substructureKind == ElifU
      inc n
      skip n
      if n.stmtKind == StmtsS:
        inc n
        if n.stmtKind == BreakS:
          skip n
          if n.kind == ParRi:
            return result

    inc result
    # skip the statement but if we find any break at this point, we don't understand the structure
    # well enough and bail out:
    if n.kind == ParLe:
      var nested = 0
      while true:
        inc n
        if n.kind == ParRi:
          if nested == 0: break
          dec nested
        elif n.kind == ParLe:
          inc nested
          if n.stmtKind == BreakS: return -1
    inc n
  result = -1

proc trWhileTrue(c: var Context; dest: var TokenBuf; n: var Cursor;
                  forBorrow: TokenBuf = createTokenBuf(0)) =
  let info = n.info
  let guard = pool.syms.getOrIncl("´g." & $c.current.tmpCounter)
  inc c.current.tmpCounter
  openScope c

  dest.copyIntoKind StmtsS, info:
    declareCfVar c, dest, guard
    let s = addGuard(c, Guard(cond: guard, active: false))
    var b = BasicBlock(openElseBranches: 0, hasParLe: true, leavesWith: -1)

    var breakSplitPoint = findBreakSplitPoint(n)
    inc n # into the loop body statement list
    while n.kind != ParRi and breakSplitPoint >= 1:
      trGuardedStmts c, b, dest, n, false
      dec breakSplitPoint

    closeBasicBlock c, b, dest

  dest.copyIntoKind NotX, info:
    dest.addSymUse guard, info # condition is always our artifical guard

  # post loop condition body:
  dest.copyIntoKind StmtsS, info:
    # Inject synthetic borrow local for for-loop iteration borrows:
    if forBorrow.len > 0:
      dest.add forBorrow

    var b2 = BasicBlock(openElseBranches: 0, hasParLe: true, leavesWith: -1)
    var g = (-1, NoSymId)
    while n.kind != ParRi:
      if g[0] < 0: g = maybeEmitGuard(c, dest, n.info)
      elif g[0] < c.current.guards.len and g[1] == c.current.guards[g[0]].cond:
        c.current.guards[g[0]].active = false
      trGuardedStmts c, b2, dest, n, false
    maybeCloseGuard(c, dest, g, false)
    closeBasicBlock c, b2, dest
    closeScope c, dest, NoLineInfo
    skipParRi n # end of body statement list

    # last statement of our loop body is the `continue`:
    dest.copyIntoKind ContinueS, info:
      dest.addDotToken() # no `join` information yet

  removeGuard c, s

proc trWhile(c: var Context; dest: var TokenBuf; n: var Cursor) =
  dest.add tagToken("loop", n.info)
  inc n

  # special case `while true` as it plays into our hands:
  if n.exprKind == TrueX:
    inc n
    skipParRi n
    trWhileTrue c, dest, n
    dest.takeParRi n # close "loop"
  else:
    # translate `while cond: body` to `while true: if cond: body else: break`
    # as it's too complex to handle otherwise.
    let info = n.info
    var w = createTokenBuf(10)
    w.copyIntoKind StmtsS, info:
      w.copyIntoKind IfS, info:
        w.copyIntoKind ElifU, info:
          w.takeTree n # condition
          w.takeTree n # body
          skipParRi n
        w.copyIntoKind ElseU, info:
          w.copyIntoKind StmtsS, info:
            w.addParPair BreakS, info
    var ww = beginRead(w)
    trWhileTrue c, dest, ww
    endRead w
    dest.addParRi() # close "loop"

proc addForBorrowDecls(dest: var TokenBuf; vars: Cursor; firstArgBuf: TokenBuf) =
  var vars = vars
  if vars.substructureKind in {UnpackflatU, UnpacktupU}:
    inc vars
    while vars.kind != ParRi:
      addForBorrowDecls dest, vars, firstArgBuf
      skip vars
  elif isLocal(vars.symKind):
    let local = asLocal(vars)
    if local.typ.typeKind in {MutT, LentT}:
      var localDecl = vars
      dest.addParLe(if local.typ.typeKind == MutT: VarS else: LetS, vars.info)
      inc localDecl # skip original local-decl tag
      takeTree dest, localDecl # name
      takeTree dest, localDecl # export marker
      takeTree dest, localDecl # pragmas
      takeTree dest, localDecl # type
      dest.addParLe HaddrX, vars.info
      dest.add firstArgBuf
      dest.addParRi()
      dest.addParRi()

proc extractForBorrow(c: var Context; forStmt: ForStmt; info: PackedLineInfo): TokenBuf =
  ## If the for-loop iterates with a borrowing iterator (yields var T or lent T),
  ## initialize the corresponding loop variables with a fake `(haddr firstArg)`.
  ## This lets contract analysis treat the real loop binders as borrowers so
  ## their borrow lifetime naturally ends with the generated `(kill ...)`.
  result = createTokenBuf(0)

  # Extract the first argument of the iterator call (the collection).
  # After xelim, the iter call may have been extracted to a temporary:
  #   let `x.N = items(s)  =>  forStmt.iter = (hderef `x.N)
  # So we trace back through hderef/temporaries to find the original call's first arg.
  var firstArgBuf = createTokenBuf(0)
  var iterCall = forStmt.iter
  if iterCall.kind == ParLe and iterCall.exprKind in CallKinds:
    inc iterCall
    skip iterCall
    if iterCall.kind != ParRi:
      firstArgBuf = createTokenBuf(8)
      firstArgBuf.addSubtree iterCall
  elif iterCall.kind == ParLe and iterCall.exprKind in {HderefX, HaddrX}:
    inc iterCall
    if iterCall.kind == Symbol:
      let tempSym = iterCall.symId
      if tempSym in c.callFirstArgs:
        firstArgBuf = createTokenBuf(8)
        firstArgBuf.addSubtree beginRead(c.callFirstArgs[tempSym])
  elif iterCall.kind == Symbol:
    let tempSym = iterCall.symId
    if tempSym in c.callFirstArgs:
      firstArgBuf = createTokenBuf(8)
      firstArgBuf.addSubtree beginRead(c.callFirstArgs[tempSym])

  if firstArgBuf.len == 0:
    return

  addForBorrowDecls result, forStmt.vars, firstArgBuf

proc trFor(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # Map `for x in i()` to `(loop ... (stmts (let x type i()) body))` so the
  # loop variable is bound from the iterator at the start of the body.
  let info = n.info
  let forStmt = asForStmt(n) # peek at structure before advancing
  dest.add tagToken("loop", info)
  inc n

  let borrowBuf = extractForBorrow(c, forStmt, info)

  skip n # for loop iterator call
  skip n # for loop variables
  trWhileTrue c, dest, n, borrowBuf
  dest.takeParRi n # close "loop"

proc buildCaseCondition(c: var Context; dest: var TokenBuf; n: var Cursor;
                        selector: SymId; selectorType: Cursor; info: PackedLineInfo) =
  ## Build condition for one of-branch using OrX for multiple ranges/values
  assert n.substructureKind == RangesU
  inc n  # into RangesU
  # Collect all conditions
  var conditions: seq[TokenBuf] = @[]

  while n.kind != ParRi:
    var cond = createTokenBuf(10)
    if n.substructureKind == RangeU:
      # Range: low..high => (low <= selector) and (selector <= high)
      inc n
      cond.copyIntoKind AndX, info:
        cond.copyIntoKind LeX, info:
          cond.copyTree selectorType
          trExpr c, cond, n
          cond.addSymUse selector, info
        cond.copyIntoKind LeX, info:
          cond.copyTree selectorType
          cond.addSymUse selector, info
          trExpr c, cond, n
      skipParRi n
    else:
      # Single value: selector == value
      cond.copyIntoKind EqX, info:
        cond.copyTree selectorType
        cond.addSymUse selector, info
        trExpr c, cond, n

    conditions.add cond

  inc n  # skip closing ParRi of RangesU

  # Combine conditions with OrX
  if conditions.len == 1:
    dest.add conditions[0]
  else:
    dest.addParLe OrX, info
    for cond in conditions:
      dest.add cond
    dest.addParRi()

proc trGuardedStmtsBlock(c: var Context; dest: var TokenBuf; n: var Cursor; hasParLe = false) =
  var b = BasicBlock(openElseBranches: 0, hasParLe: hasParLe, leavesWith: -1)
  trGuardedStmts c, b, dest, n, false
  closeBasicBlock c, b, dest

proc trCase(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n  # skip 'case'

  # Evaluate selector
  let selectorType = c.typeCache.getType(n)
  let isExhaustive = isOrdinalType(selectorType, allowEnumWithHoles=true)

  var selector: SymId
  if n.kind == Symbol:
    selector = n.symId
    inc n
  else:
    # Complex selector - bind to temporary
    selector = pool.syms.getOrIncl("`cs." & $c.current.tmpCounter)
    inc c.current.tmpCounter
    dest.copyIntoKind LetS, info:
      dest.addSymDef selector, info
      dest.addDotToken() # export marker
      dest.addDotToken() # pragmas
      dest.copyTree selectorType
      trExpr c, dest, n

  # Find final branch if exhaustive
  var finalBranch = default(Cursor)
  if isExhaustive:
    var nn = n
    while nn.substructureKind == OfU:
      finalBranch = nn
      skip nn
    if nn.substructureKind == ElseU:
      finalBranch = default(Cursor)

  # Generate nested ite for each of-branch
  var iteCount = 0

  while n.substructureKind == OfU:
    if n == finalBranch:
      inc n  # into OfU
      # Final exhaustive branch - no condition needed, just emit body
      skip n  # skip ranges
      openScope c
      trGuardedStmtsBlock c, dest, n
      closeScope c, dest, info
      skipParRi n
      break

    inc n  # into OfU
    # Emit ite (or itec for first branch to mark case origin)
    if iteCount == 0:
      dest.add tagToken("itec", info)
    else:
      dest.add tagToken("ite", info)
    inc iteCount

    # Emit condition
    buildCaseCondition c, dest, n, selector, selectorType, info

    # Emit then-branch
    dest.addParLe StmtsS, info
    openScope c
    trGuardedStmtsBlock c, dest, n, true
    closeScope c, dest, info
    dest.addParRi()
    skipParRi n  # close OfU

    # Start else-branch (will contain next ite or final else)
    dest.addParLe StmtsS, info

  # Handle explicit else branch
  if n.substructureKind == ElseU:
    inc n
    openScope c
    trGuardedStmtsBlock c, dest, n
    closeScope c, dest, info
    skipParRi n
  else:
    # No explicit else
    dest.addDotToken()

  skipParRi n  # close case

  # Close all the nested ite structures
  for i in 0..<iteCount:
    dest.addParRi()  # close else stmts
    dest.addParRi()  # close ite/itec

proc trTry(c: var Context; outerB: BasicBlock; dest: var TokenBuf; n: var Cursor) =
  let info = n.info


  let guard = pool.syms.getOrIncl("´g." & $c.current.tmpCounter)
  inc c.current.tmpCounter

  # Determine the error tracker variable
  # If the proc can raise, use resultSym; otherwise create a local tracker
  let oldErrorTracker = c.current.errorTracker
  var tracker: SymId
  if c.current.mode != NoRaise and c.current.resultSym != NoSymId:
    tracker = c.current.resultSym
  else:
    # Need a local variable to track errors within this try block
    tracker = pool.syms.getOrIncl("´err." & $c.current.tmpCounter)
    inc c.current.tmpCounter
    # Declare and initialize to Success
    dest.copyIntoKind VarS, info:
      dest.addSymDef tracker, info
      dest.addDotToken() # export marker
      dest.addDotToken() # pragmas
      dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), info # type
      dest.addSymUse pool.syms.getOrIncl(SuccessName), info # initial value
  c.current.errorTracker = tracker

  declareCfVar c, dest, guard
  let s = addGuard(c, Guard(cond: guard, active: false, isTryGuard: true))

  # Temporarily override mode so error handling inside try block works
  let oldMode = c.current.mode
  if c.current.mode == NoRaise:
    # Make the try block think we're in a VoidRaise context
    c.current.mode = VoidRaise

  openScope c
  inc n # into the try
  trGuardedStmtsBlock c, dest, n, outerB.hasParLe

  # Restore original mode and errorTracker
  c.current.mode = oldMode
  c.current.errorTracker = oldErrorTracker

  closeScope c, dest, info

  while n.substructureKind == ExceptU:
    inc n # into ExceptU
    dest.copyIntoKind IteV, info:
      dest.addSymUse guard, info
      dest.copyIntoKind StmtsS, info:
        # Handle exception type pattern (if present)
        if n.stmtKind != LetS:
          dest.takeTree n
        openScope c

        # If there's an exception variable (let e: ErrorCode), declare and initialize it
        if n.stmtKind == LetS:
          inc n
          let excVar = n.symId
          inc n # symbol
          skip n # export marker
          skip n # pragmas
          c.typeCache.registerLocal(excVar, LetY, n)
          skip n # type
          # Initialize: e = errorTracker
          copyIntoKind dest, StoreV, info:
            useErrorTracker(c, dest, tracker, info)
            dest.addSymUse excVar, info
          assert n.kind == DotToken
          inc n # skip value (should be dot)
          skipParRi n # close let

        # The except handler executes only when `guard=true` (established by the outer
        # `(ite guard ...)` we just opened). Deactivate the guard so `maybeEmitGuard`
        # does not wrap the handler body in a spurious `(ite (not guard) ...)` — that
        # inner ite would be dead code, but it buries any `jtrue` calls and prevents
        # contracts analysis from seeing that the handler is a leaving path.
        c.current.guards[s.at].active = false
        trGuardedStmtsBlock c, dest, n, true

        # Mark exception as handled by resetting error tracker to Success
        storeConstToErrorTracker(c, dest, tracker, pool.syms.getOrIncl(SuccessName), info)

        closeScope c, dest, info
        skipParRi n
      dest.addDotToken() # no else
  if n.substructureKind == FinU:
    c.current.errorTracker = tracker
    if c.current.mode == NoRaise:
      c.current.mode = VoidRaise

    inc n # into FinU
    openScope c
    # The finally body must always execute. Deactivate the try guard so `maybeEmitGuard`
    # does not wrap the finally body in `(ite (not guard) ...)`.
    c.current.guards[s.at].active = false
    trGuardedStmtsBlock c, dest, n, true
    closeScope c, dest, info
    skipParRi n

    # Re-raise any unhandled error after finally block executes
    # Check the error tracker, not the guard (guards are monotonic and can't be reset)
    dest.copyIntoKind IteV, info:
      useErrorTracker(c, dest, tracker, info)
      dest.copyIntoKind StmtsS, info:
        raiseGuards(c, dest, info)
      dest.addDotToken() # no else

    c.current.mode = oldMode
    c.current.errorTracker = oldErrorTracker
  removeGuard c, s
  skipParRi n


proc trRet(c: var Context; b: var BasicBlock; dest: var TokenBuf; n: var Cursor) =
  ## Emit the return value store and activate all return guards.
  ## Sets `b.leavesWith` to signal that this block ends with a leaving statement.
  let info = n.info
  inc n

  if n.kind == ParRi:
    inc n
  else:
    if n.kind == DotToken:
      inc n
    else:
      assert c.current.resultSym != NoSymId, "could not find `result` symbol"
      if n.kind == Symbol and n.symId == c.current.resultSym:
        inc n
      else:
        dest.copyIntoKind StoreV, info:
          trResultExpr c, dest, n
          dest.addSymUse c.current.resultSym, info
    skipParRi n

  emitReturnGuards(c, dest, info)
  assert c.current.guards.len > 0, "return outside any guarded scope"
  b.leavesWith = c.current.guards.len-1

proc trRaise(c: var Context; b: var BasicBlock; dest: var TokenBuf; n: var Cursor) =
  ## Emit error tracker store and activate raise guards.
  ## Sets `b.leavesWith` to signal that this block ends with a leaving statement.
  let info = n.info
  inc n

  if n.kind == ParRi:
    inc n
  else:
    if n.kind == DotToken:
      inc n
      bug "reraise not implemented"
    else:
      assert c.current.errorTracker != NoSymId, "raise outside a .raises proc or try section"
      storeToErrorTracker(c, dest, n, info)
    skipParRi n
  raiseGuards(c, dest, info)
  assert c.current.guards.len > 0, "raise outside any guarded scope"
  b.leavesWith = c.current.guards.len-1

proc trCfVarDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # xelim can produce cfvars that we didn't generate so handle them here
  dest.takeToken n # CfVarV
  let s = n.symId
  dest.takeToken n # SymDef
  dest.takeParRi n # ParRi
  c.typeCache.registerLocal(s, VarY, c.typeCache.builtins.boolType)

proc trGuardedStmts(c: var Context; b: var BasicBlock; dest: var TokenBuf; n: var Cursor; mustCloseScope: bool) =
  ## Process one statement, wrapping it in a guard ite if a guard is active.
  ##
  ## The guard protocol:
  ## 1. `maybeEmitGuard` borrows the innermost active guard: emits `(ite (not g) (stmts`
  ##    and disables g so nested statements don't double-wrap.
  ## 2. The statement is processed (which may activate new guards via jtrue).
  ## 3. `maybeCloseGuard` returns the borrowed guard: closes `) . )` and re-enables g.
  ##
  ## For StmtsS, the g2 mechanism merges consecutive statements under the same guard:
  ## the guard is borrowed once (g2) and held open while all children are processed.
  ## This turns `(ite (not g) a .)(ite (not g) b .)` into `(ite (not g) (a b) .)`.
  let g = maybeEmitGuard(c, dest, n.info)

  var takeThisParRi = false
  case n.stmtKind
  of StmtsS, ScopeS:
    # Flatten nested stmts when the output already has a (stmts open.
    if not b.hasParLe and g[0] < 0:
      dest.takeToken n
      b.hasParLe = true
      takeThisParRi = true
    else:
      inc n
    # g2 borrows the innermost active guard for the ENTIRE statement list.
    # All children are emitted inside this single guard, achieving the merge.
    var g2 = (-1, NoSymId)
    while n.kind != ParRi:
      if g2[0] < 0:
        g2 = maybeEmitGuard(c, dest, n.info)
      elif g2[0] < c.current.guards.len and g2[1] == c.current.guards[g2[0]].cond:
        # The guard may have been re-activated by raiseGuards inside a child
        # (e.g. a VoidRaise call). Since we're still inside the g2 scope,
        # suppress the re-activation to prevent sibling ites.
        c.current.guards[g2[0]].active = false
      trGuardedStmts(c, b, dest, n, false)
    inc n # ParRi
    maybeCloseGuard(c, dest, g2, false)

  of AsgnS:
    trAsgn c, b, dest, n
  of IfS:
    trIf c, b, dest, n
  of CaseS:
    trCase c, dest, n
  of WhileS:
    trWhile c, dest, n
  of ForS:
    trFor c, dest, n
  of LocalDecls:
    trLocal c, b, dest, n
  of ProcS, FuncS, MacroS, MethodS, ConverterS, IteratorS:
    trProcDecl c, dest, n
  of RetS:
    trRet c, b, dest, n
  of BreakS:
    trBreak c, b, dest, n
  of BlockS:
    trBlock c, b, dest, n
  of TemplateS, TypeS:
    takeTree dest, n
  of RaiseS:
    trRaise c, b, dest, n
  of TryS:
    trTry c, b, dest, n
  of CallKindsS:
    trStmtCall c, b, dest, n
  else:
    if n.njvlKind in {MflagV, VflagV}:
      trCfVarDecl c, dest, n
    elif n.exprKind == PragmaxX:
      copyInto(dest, n):
        takeTree dest, n  # pragmas
        var innerB = BasicBlock(openElseBranches: 0, hasParLe: false, leavesWith: -1)
        trGuardedStmts c, innerB, dest, n, false  # body
        closeBasicBlock c, innerB, dest
        if innerB.leavesWith >= 0:
          # Propagate leaving status from the pragma body to the outer block.
          assert innerB.leavesWith < c.current.guards.len,
            "pragma body leavesWith out of range"
          b.leavesWith = innerB.leavesWith
    elif n.exprKind == ProccallX:
      trStmtCall c, b, dest, n
    else:
      trExpr c, dest, n

  maybeCloseGuard(c, dest, g, mustCloseScope)
  if takeThisParRi:
    dest.addParRi()

proc eliminateJumps*(pass: var Pass; raisesResolved = false) =
  var c = Context(counter: 0, typeCache: createTypeCache(),
                  thisModuleSuffix: pass.moduleSuffix,
                  raisesResolved: raisesResolved)
  c.openScope()
  lowerExprs(pass, TowardsNjvl)
  pass.prepareForNext("elimjumps")
  var n = pass.n
  #echo "after xelim: ", toString(n, false)
  assert n.stmtKind == StmtsS, $n.kind
  pass.dest.add n
  inc n
  # Add a top-level return guard so that noreturn calls at module level
  # have a mflag to set via jtrue, enabling the mflag-based init tracking:
  let topFlag = pool.syms.getOrIncl("´r.0." & c.thisModuleSuffix)
  c.current.guards.add Guard(cond: topFlag, active: false)
  declareCfVar c, pass.dest, topFlag
  var b = BasicBlock(openElseBranches: 0, hasParLe: true, leavesWith: -1)
  while n.kind != ParRi:
    trGuardedStmts c, b, pass.dest, n, false
  closeScope c, pass.dest, n.info
  closeBasicBlock c, b, pass.dest
  pass.dest.addParRi()
  #echo "PRODUCED: ", pass.dest.toString(false)

when isMainModule:
  from std/os import paramStr, paramCount
  import std/syncio
  import ".." / lib / symparser
  let infile = paramStr(1)
  var owningBuf = createTokenBuf(300)
  let n = setupProgram(infile, infile.changeModuleExt".njvl.nif", owningBuf)
  var initialBuf = createTokenBuf(300)
  initialBuf.addSubtree(n)
  var pass = initPass(move initialBuf, "main", "xelim_njvl", 0)
  eliminateJumps(pass)
  let output = pass.dest.toString(false)
  if paramCount() >= 2:
    # Write to specified output file
    let outfile = paramStr(2)
    var f = syncio.open(outfile, fmWrite)
    try:
      syncio.write(f, output)
    finally:
      syncio.close(f)
  else:
    # Write to stdout
    echo output
