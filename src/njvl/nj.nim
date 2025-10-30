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
import ".." / hexer / [xelim, mover]
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

# If-Then-Else Optimization

When `optimizeIte` is enabled, we perform an optimization to avoid redundant guard checks.
Instead of generating:

  if cond: break
  if not guard: stmt1
  if not guard: stmt2
  if not guard: stmt3

We generate:

  if cond: break
  else:
    stmt1
    stmt2
    stmt3

This works by:
1. `trIf` detects when a guard was activated in the then-branch (by comparing
   `lastActivatedGuard` before and after).
2. If activated and no explicit else exists, `trIf` sets `pendingIte` to the
   activated guard index and leaves the `ite` instruction open.
3. The next call to `trGuardedStmts` detects `pendingIte >= 0` and processes
   the remaining statement(s) as the else-branch with the guard disabled.
4. After processing, it closes the `ite` instruction.

Fields involved:
- `lastActivatedGuard`: Tracks the most recently activated guard index (set by
  `trBreak`, `trRet`; reset when guards are popped in `trBlock`, `trWhileTrue`).
- `pendingIte`: Index of guard for an open `ite` waiting for its else-branch,
  or -1 if none.

]#

type
  Guard = object
    cond: SymId
    blockName: SymId # used for named `block` statements
    active: bool
    isTryGuard: bool

  GuardIndex = object
    idx: int      # index in guards array, or -1 if invalid
    sym: SymId    # the guard's SymId, for validation

  IteOptState = object
    ## State machine for the if-then-else optimization.
    ## States:
    ##   1. Empty: no guard activation
    ##   2. GuardActivated: a guard was activated, available for optimization
    ##   3. PendingIte: an ite is open, waiting for its else-branch
    lastActivated: GuardIndex  # Guard that was activated
    pendingIte: GuardIndex     # Guard for unclosed ite waiting for else-branch

  ExceptionMode* = enum
    NoRaise     # proc/call cannot raise
    VoidRaise   # proc/call can raise, but is void
    TupleRaise  # proc/call can raise, and has a return value so it becomes a tuple

  CurrentProc = object
    mode: ExceptionMode
    resultSym: SymId
    errorTracker: SymId # tracks error codes in try blocks (can differ from resultSym)
    guards: seq[Guard]
    tmpCounter: int
    iteOpt: IteOptState # if-then-else optimization state
    returnType: Cursor
    tupleVars: HashSet[SymId] # variables that have been expanded to a tuple due to exception handling

  Context* = object
    typeCache: TypeCache
    counter: int
    thisModuleSuffix: string
    current: CurrentProc
    optimizeIte: bool # Enable if-then-else optimization

const InvalidGuardRef = GuardIndex(idx: -1, sym: NoSymId)

proc isValid(g: GuardIndex; guards: seq[Guard]): bool =
  ## Check if a guard reference is still valid
  g.idx >= 0 and g.idx < guards.len and guards[g.idx].cond == g.sym

# IteOptState operations - state machine for if-then-else optimization
proc initIteOpt(): IteOptState =
  ## Create empty optimization state
  IteOptState(lastActivated: InvalidGuardRef, pendingIte: InvalidGuardRef)

proc recordActivation(s: var IteOptState; g: GuardIndex) =
  ## Record that a guard was activated
  s.lastActivated = g

proc getLastActivated(s: IteOptState): GuardIndex =
  ## Get the last activated guard
  s.lastActivated

proc transferToPending(s: var IteOptState) =
  ## Transfer lastActivated to pendingIte (opening an unclosed ite)
  s.pendingIte = s.lastActivated
  s.lastActivated = InvalidGuardRef

proc hasPending(s: IteOptState): bool =
  ## Check if there's a pending ite
  s.pendingIte.idx >= 0

proc takePending(s: var IteOptState): GuardIndex =
  ## Consume and return the pending ite's guard
  result = s.pendingIte
  s.pendingIte = InvalidGuardRef

proc clear(s: var IteOptState) =
  ## Clear all optimization state
  s.lastActivated = InvalidGuardRef
  s.pendingIte = InvalidGuardRef

proc addKill(dest: var TokenBuf; s: SymId; info: PackedLineInfo) =
  dest.add tagToken("kill", info)
  dest.addSymUse s, info
  dest.addParRi()

proc openScope(c: var Context) =
  c.typeCache.openScope()

proc closeScope(c: var Context; dest: var TokenBuf; info: PackedLineInfo) =
  # insert kill instructions:
  for s in c.typeCache.currentScopeLocals:
    dest.addKill(s, info)
  c.typeCache.closeScope()

proc trGuardedStmts(c: var Context; dest: var TokenBuf; n: var Cursor; parentIsStmtList=false)
proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc declareCfVar(dest: var TokenBuf; s: SymId) =
  dest.add tagToken("cfvar", NoLineInfo)
  dest.addSymDef s, NoLineInfo
  dest.addParRi()

proc useErrorTracker(c: Context; dest: var TokenBuf; info: PackedLineInfo) =
  ## Emit the correct expression to read the error code from errorTracker.
  ## In TupleRaise mode, errorTracker is a tuple and we need (tupat errorTracker +0).
  ## In VoidRaise/NoRaise mode, errorTracker is a plain ErrorCode variable.
  if c.current.mode == TupleRaise:
    dest.addParLe TupatX, info
    dest.addSymUse c.current.errorTracker, info
    dest.addIntLit 0, info
    dest.addParRi()
  else:
    dest.addSymUse c.current.errorTracker, info

proc storeToErrorTracker(c: var Context; dest: var TokenBuf; value: var Cursor; info: PackedLineInfo) =
  ## Emit the correct store to set the error code in errorTracker from a source expression.
  ## In TupleRaise mode, store to (tupat errorTracker +0).
  ## In VoidRaise/NoRaise mode, store directly to errorTracker.
  dest.copyIntoKind StoreV, info:
    trExpr c, dest, value
    if c.current.mode == TupleRaise:
      dest.addParLe TupatX, info
      dest.addSymUse c.current.errorTracker, info
      dest.addIntLit 0, info
      dest.addParRi()
    else:
      dest.addSymUse c.current.errorTracker, info

proc storeConstToErrorTracker(c: Context; dest: var TokenBuf; constSym: SymId; info: PackedLineInfo) =
  ## Store a constant (like Success) to errorTracker.
  dest.copyIntoKind StoreV, info:
    dest.addSymUse constSym, info
    if c.current.mode == TupleRaise:
      dest.addParLe TupatX, info
      dest.addSymUse c.current.errorTracker, info
      dest.addIntLit 0, info
      dest.addParRi()
    else:
      dest.addSymUse c.current.errorTracker, info

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
    assert n.kind == DotToken
    inc n
    copyIntoKind dest, StoreV, info:
      dest.addSymUse pool.syms.getOrIncl(SuccessName), info
      dest.addSymUse c.current.resultSym, info
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
  c.current = CurrentProc(tmpCounter: 1, iteOpt: initIteOpt(), returnType: r.retType)
  if hasPragma(r.pragmas, RaisesP):
    if isVoidType(r.retType):
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
        openScope c
        # if this is a void proc that `.raises` we add a `result` variable as we actually need to return something
        if c.current.mode == VoidRaise:
          c.current.resultSym = pool.syms.getOrIncl("`result." & $c.current.tmpCounter)
          inc c.current.tmpCounter
          declareResultVar dest, c.current.resultSym, info
          # By default, errorTracker is the same as resultSym
          c.current.errorTracker = c.current.resultSym

        declareCfVar dest, retFlag
        trGuardedStmts c, dest, n, true
        closeScope c, dest, info
      c.typeCache.closeScope()
    else:
      takeTree dest, n
  c.current = ensureMove oldProc

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor): ExceptionMode =
  var fnType = skipProcTypeToParams(getType(c.typeCache, n.firstSon))
  assert isParamsTag(fnType)
  var pragmas = fnType
  skip pragmas
  let retType = pragmas
  skip pragmas
  let canRaise = hasPragma(pragmas, RaisesP)

  result =
    if canRaise:
      (if isVoidType(retType): VoidRaise else: TupleRaise)
    else: NoRaise

  let info = n.info
  dest.add n
  inc n # skip `(call)`
  trExpr c, dest, n # handle `fn`
  var mutates: seq[SymId] = @[]
  while n.kind != ParRi:
    if n.exprKind == HaddrX:
      let r = rootOf(n, CanFollowCalls)
      if r != NoSymId:
        mutates.add r
    trExpr c, dest, n
  # we make `unknown` part of the `call` for now. This will be cleaned up
  # in the `versionizer` pass!
  for s in mutates:
    dest.add tagToken("unknown", info)
    dest.addSymUse s, info
    dest.addParRi() # unknown
  dest.takeParRi n

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of Symbol:
    if c.current.tupleVars.contains(n.symId):
      let info = n.info
      copyIntoKind dest, TupatX, info:
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

proc trBoundExpr(c: var Context; dest: var TokenBuf; n: var Cursor): ExceptionMode =
  # Indicates that the expression is about to be bound to a location.
  # Hence a `call` expression is valid here.
  if n.exprKind in CallKinds:
    result = trCall(c, dest, n)
  else:
    trExpr c, dest, n
    result = NoRaise

proc raiseGuards(c: var Context; dest: var TokenBuf; info: PackedLineInfo) =
  dest.add tagToken("jtrue", info)
  # we also need to break out of everything, until a `try` guard is found
  for i in countdown(c.current.guards.len - 1, 0):
    if c.current.guards[i].isTryGuard:
      break
    let cond = c.current.guards[i].cond
    assert cond != NoSymId
    c.current.guards[i].active = true
    # Track the innermost guard (first in countdown)
    if c.optimizeIte and c.current.iteOpt.getLastActivated().idx < 0:
      c.current.iteOpt.recordActivation(GuardIndex(idx: i, sym: cond))
    dest.addSymUse cond, info
  dest.addParRi()

proc trStmtCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let before = dest.len
  let info = n.info
  let m = trCall(c, dest, n)
  case m
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

    # Track guard state before emitting the error-check ite
    let guardBefore = if c.optimizeIte: c.current.iteOpt.getLastActivated() else: InvalidGuardRef

    # Manually emit ite to control whether we close it (for optimization)
    dest.add tagToken("ite", info)
    dest.addSymUse s, info # XXX write that later as `e != Success`
    dest.addParLe StmtsS, info # then-branch
    raiseGuards(c, dest, info)
    dest.addParRi() # close then-branch stmts

    # Check if raiseGuards activated a guard, and if so, set up pending ite for optimization
    let activatedGuard = if c.optimizeIte: c.current.iteOpt.getLastActivated() else: InvalidGuardRef
    if c.optimizeIte and activatedGuard.idx >= 0 and activatedGuard.idx != guardBefore.idx:
      # Don't close the ite - let next statement become the else-branch
      c.current.iteOpt.transferToPending()
    else:
      # Close the ite normally
      dest.addDotToken() # no else section
      dest.addDotToken() # no join information
      dest.addParRi() # close ite

  of TupleRaise:
    bug "value should have been discarded"

proc replayLocalHeader(c: var Context; dest: var TokenBuf; n: Cursor) =
  var n = n
  takeTree dest, n # name
  takeTree dest, n # export marker
  takeTree dest, n # pragmas
  dest.copyIntoKind TupleT, n.info:
    dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), n.info
    takeTree dest, n # type
  takeTree dest, n # value

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  dest.takeToken n
  let afterHead = dest.len

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

  let info = n.info
  let m = trBoundExpr(c, dest, n)
  skipParRi n
  # the `raise` statement must follow the var declaration!
  case m
  of NoRaise:
    dest.addParRi()
  of VoidRaise:
    dest.addParRi()
    bug "value should have been discarded"
  of TupleRaise:
    # we also need to patch the type!
    var decl = createTokenBuf(10)
    decl.copyTree cursorAt(dest, afterHead)
    endRead dest
    dest.shrink afterHead
    replayLocalHeader(c, dest, beginRead(decl))

    dest.addParRi()

    # Track guard state before emitting the error-check ite
    let guardBefore = if c.optimizeIte: c.current.iteOpt.getLastActivated() else: InvalidGuardRef

    c.current.tupleVars.incl(symId)

    # Manually emit ite to control whether we close it (for optimization)
    dest.add tagToken("ite", info)
    dest.addParLe TupatX, info # condition
    dest.addSymUse symId, info # XXX write that later as `e != Success`
    dest.addIntLit 0, info
    dest.addParRi() # close tupat
    dest.addParLe StmtsS, info # then-branch
    raiseGuards(c, dest, info)
    dest.addParRi() # close then-branch stmts

    # Check if raiseGuards activated a guard, and if so, set up pending ite for optimization
    let activatedGuard = if c.optimizeIte: c.current.iteOpt.getLastActivated() else: InvalidGuardRef
    if c.optimizeIte and activatedGuard.idx >= 0 and activatedGuard.idx != guardBefore.idx:
      # Don't close the ite - let next statement become the else-branch
      c.current.iteOpt.transferToPending()
    else:
      # Close the ite normally
      dest.addDotToken() # no else section
      dest.addDotToken() # no join information
      dest.addParRi() # close ite

proc trAsgn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # we translate `(asgn X Y)` to `(store Y X)` as it's easier to analyze,
  # it reflects the actual evaluation order.
  let info = n.info
  dest.add tagToken("store", info)
  inc n
  if n.kind == Symbol and n.symId == c.current.resultSym:
    inc n # skip `result`:
    trResultExpr c, dest, n
    # add `result` later due to the changed order for `(store X result)`
    dest.addSymUse c.current.resultSym, info
  else:
    var rhs = n
    skip rhs
    trExpr c, dest, rhs
    trExpr c, dest, n # lhs
    n = rhs
  skipParRi n
  dest.addParRi()

proc trIf(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # we assume here that xelim already produced a single elif-else construct here
  let info = n.info
  dest.add tagToken("ite", n.info)
  inc n
  assert n.substructureKind == ElifU
  inc n
  trExpr c, dest, n

  # Track which guard was activated before then-branch
  let guardBefore = if c.optimizeIte: c.current.iteOpt.getLastActivated() else: InvalidGuardRef

  openScope c
  trGuardedStmts c, dest, n
  closeScope c, dest, info
  skipParRi n

  # Check if a guard was activated during then-branch
  let activatedGuard = if c.optimizeIte: c.current.iteOpt.getLastActivated() else: InvalidGuardRef

  if n.kind != ParRi:
    # Has explicit else branch
    assert n.substructureKind == ElseU
    inc n
    openScope c
    trGuardedStmts c, dest, n
    closeScope c, dest, info
    skipParRi n
    skipParRi n
    # join information: not yet available
    dest.addDotToken()
    dest.addParRi() # "ite"
    # Note: keep lastActivated set so statements after this entire if-else
    # remain guarded (the explicit else doesn't consume the guard like our optimization does)
  elif c.optimizeIte and activatedGuard.idx >= 0 and activatedGuard.idx != guardBefore.idx:
    # Guard was activated in then-branch, don't close ite yet
    # Let next statement become the else branch. Transfer the activation to pending state.
    c.current.iteOpt.transferToPending()
    skipParRi n
  else:
    # Normal completion, no new guard activated
    skipParRi n
    dest.addDotToken() # no else section
    # join information: not yet available
    dest.addDotToken()
    dest.addParRi() # "ite"

proc trBreak(c: var Context; dest: var TokenBuf; n: var Cursor) =
  assert c.current.guards.len > 0

  var entries = 0 # only care about the inner most
  inc n
  if n.kind != ParRi:
    if n.kind == DotToken:
      inc n
      inc entries
    elif n.kind == Symbol:
      for i in countdown(c.current.guards.len - 1, 0):
        if c.current.guards[i].blockName == n.symId: break
        inc entries
    else:
      bug "invalid `break` structure"

  dest.add tagToken("jtrue", n.info)
  for i in 1..entries:
    let guardIdx = c.current.guards.len - i
    let g = addr c.current.guards[guardIdx]
    dest.addSymUse g.cond, n.info
    g.active = true
    # Track the innermost (last in loop) guard that was activated
    if c.optimizeIte:
      c.current.iteOpt.recordActivation(GuardIndex(idx: guardIdx, sym: g.cond))
  dest.addParRi()
  skipParRi n

type
  GuardUndoState = object
    at: int
    iteOpt: IteOptState

proc addGuard(c: var Context; g: Guard): GuardUndoState =
  result = GuardUndoState(at: c.current.guards.len, iteOpt: c.current.iteOpt)
  c.current.iteOpt.clear()
  c.current.guards.add g

proc removeGuard(c: var Context; s: GuardUndoState) =
  c.current.guards.shrink(s.at)
  # Restore optimization state only if it refers to guards still in scope
  if s.iteOpt.lastActivated.idx >= 0 and s.iteOpt.lastActivated.idx < s.at:
    c.current.iteOpt.lastActivated = s.iteOpt.lastActivated
  if s.iteOpt.pendingIte.idx >= 0 and s.iteOpt.pendingIte.idx < s.at:
    c.current.iteOpt.pendingIte = s.iteOpt.pendingIte

proc trBlock(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let guard = pool.syms.getOrIncl("´g." & $c.current.tmpCounter)
  inc c.current.tmpCounter

  declareCfVar dest, guard
  inc n # "block"
  let blockName = if n.kind == SymbolDef: n.symId else: NoSymId
  inc n # name or empty
  openScope c
  let s = addGuard(c, Guard(cond: guard, active: false, blockName: blockName))

  trGuardedStmts c, dest, n, false
  removeGuard c, s
  closeScope c, dest, n.info


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

proc trWhileTrue(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let guard = pool.syms.getOrIncl("´g." & $c.current.tmpCounter)
  inc c.current.tmpCounter
  openScope c

  dest.copyIntoKind StmtsS, info:
    declareCfVar dest, guard
    let s = addGuard(c, Guard(cond: guard, active: false))

    var breakSplitPoint = findBreakSplitPoint(n)
    inc n # into the loop body statement list
    while n.kind != ParRi and breakSplitPoint >= 1:
      trGuardedStmts c, dest, n, true
      dec breakSplitPoint

  dest.copyIntoKind NotX, info:
    dest.addSymUse guard, info # condition is always our artifical guard

  # post loop condition body:
  dest.copyIntoKind StmtsS, info:
    while n.kind != ParRi:
      trGuardedStmts c, dest, n, true
    skipParRi n # end of body statement list

    closeScope c, dest, info
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
    inc n  # into OfU

    if n == finalBranch:
      # Final exhaustive branch - no condition needed, just emit body
      skip n  # skip ranges
      openScope c
      trGuardedStmts c, dest, n, false
      closeScope c, dest, info
      skipParRi n
      break

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
    trGuardedStmts c, dest, n, false
    closeScope c, dest, info
    dest.addParRi()
    skipParRi n  # close OfU

    # Start else-branch (will contain next ite or final else)
    dest.addParLe StmtsS, info

  # Handle explicit else branch
  if n.substructureKind == ElseU:
    inc n
    openScope c
    trGuardedStmts c, dest, n, false
    closeScope c, dest, info
    skipParRi n
  else:
    # No explicit else
    dest.addDotToken()

  skipParRi n  # close case

  # Close all the nested ite structures
  for i in 0..<iteCount:
    dest.addParRi()  # close else stmts
    dest.addDotToken()  # join placeholder
    dest.addParRi()  # close ite/itec

proc trTry(c: var Context; dest: var TokenBuf; n: var Cursor; parentIsStmtList: bool) =
  let info = n.info
  openScope c

  let guard = pool.syms.getOrIncl("´g." & $c.current.tmpCounter)
  inc c.current.tmpCounter

  # Determine the error tracker variable
  # If the proc can raise, use resultSym; otherwise create a local tracker
  let oldErrorTracker = c.current.errorTracker
  if c.current.mode != NoRaise:
    c.current.errorTracker = c.current.resultSym
  else:
    # Need a local variable to track errors within this try block
    let tracker = pool.syms.getOrIncl("´err." & $c.current.tmpCounter)
    inc c.current.tmpCounter
    # Declare and initialize to Success
    dest.copyIntoKind LetS, info:
      dest.addSymDef tracker, info
      dest.addDotToken() # export marker
      dest.addDotToken() # pragmas
      dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), info # type
      dest.addSymUse pool.syms.getOrIncl(SuccessName), info # initial value
    c.current.errorTracker = tracker

  declareCfVar dest, guard
  let s = addGuard(c, Guard(cond: guard, active: false, isTryGuard: true))

  # Temporarily override mode so error handling inside try block works
  let oldMode = c.current.mode
  if c.current.mode == NoRaise:
    # Make the try block think we're in a VoidRaise context
    c.current.mode = VoidRaise

  inc n # into the loop body statement list
  trGuardedStmts c, dest, n, parentIsStmtList

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
          let excVar = n.symId
          c.typeCache.takeLocalHeader(dest, n, LetY)
          # Initialize: e = errorTracker
          copyIntoKind dest, StoreV, info:
            useErrorTracker(c, dest, info)
            dest.addSymUse excVar, info
          assert n.kind == DotToken
          inc n # skip value (should be dot)
          skipParRi n # close let

        trGuardedStmts c, dest, n, true

        # Mark exception as handled by resetting error tracker to Success
        storeConstToErrorTracker(c, dest, pool.syms.getOrIncl(SuccessName), info)

        closeScope c, dest, info
        skipParRi n
      dest.addDotToken() # no else
      dest.addDotToken() # no join
  if n.substructureKind == FinU:
    inc n # into FinU
    openScope c
    trGuardedStmts c, dest, n, true
    closeScope c, dest, info
    skipParRi n

    # Re-raise any unhandled error after finally block executes
    # Check the error tracker, not the guard (guards are monotonic and can't be reset)
    dest.copyIntoKind IteV, info:
      dest.copyIntoKind NeqX, info:
        useErrorTracker(c, dest, info)
        dest.addSymUse pool.syms.getOrIncl(SuccessName), info
      dest.copyIntoKind StmtsS, info:
        raiseGuards(c, dest, info)
      dest.addDotToken() # no else
      dest.addDotToken() # no join

  removeGuard c, s


proc trRet(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.add tagToken("jtrue", info)
  # we also need to break out of everything:
  for i in countdown(c.current.guards.len - 1, 0):
    let cond = c.current.guards[i].cond
    assert cond != NoSymId
    c.current.guards[i].active = true
    # Track the innermost guard (first in countdown)
    if c.optimizeIte and c.current.iteOpt.getLastActivated().idx < 0:
      c.current.iteOpt.recordActivation(GuardIndex(idx: i, sym: cond))
    dest.addSymUse cond, info

  dest.addParRi()
  inc n

  if n.kind == ParRi:
    inc n
  else:
    if n.kind == DotToken:
      inc n
    else:
      assert c.current.resultSym != NoSymId, "could not find `result` symbol"
      dest.copyIntoKind StoreV, info:
        trResultExpr c, dest, n
        dest.addSymUse c.current.resultSym, info
    skipParRi n

proc trRaise(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  raiseGuards(c, dest, info)
  inc n

  if n.kind == ParRi:
    inc n
  else:
    if n.kind == DotToken:
      inc n
      bug "reraise not implemented"
    else:
      assert c.current.errorTracker != NoSymId, "could not find error tracker"
      storeToErrorTracker(c, dest, n, info)
    skipParRi n

proc trGuardedStmts(c: var Context; dest: var TokenBuf; n: var Cursor; parentIsStmtList=false) =
  # Check if we have a pending ite from a previous if statement
  if c.current.iteOpt.hasPending():
    let guardRef = c.current.iteOpt.takePending()

    # Validate that the guard is still in scope and is the same guard (not an ABA problem)
    if guardRef.isValid(c.current.guards):
      let guardIdx = guardRef.idx

      # Save state before processing else-branch (stack discipline)
      let oldIteOpt = c.current.iteOpt

      # Disable the guard for the else branch
      c.current.guards[guardIdx].active = false

      if parentIsStmtList:
        # Process all remaining statements in the list as the else branch
        # Wrap multiple statements in a stmts block for proper ite structure
        dest.addParLe StmtsS, n.info
        while n.kind != ParRi:
          trGuardedStmts(c, dest, n, true)
        dest.addParRi() # close stmts
      else:
        # Process this single statement as the else branch
        trGuardedStmts(c, dest, n, false)

      # Re-enable the guard
      c.current.guards[guardIdx].active = true

      # Now close the ite
      dest.addDotToken() # no join information
      dest.addParRi() # "ite"

      # Restore state (stack discipline). Since trIf called transferToPending (which cleared
      # lastActivated), oldIteOpt.lastActivated will be InvalidGuardRef, which prevents the
      # optimization from incorrectly triggering at outer if levels.
      c.current.iteOpt = oldIteOpt
      return

  var usedGuard = -1
  for i in countdown(c.current.guards.len - 1, 0):
    let g = addr c.current.guards[i]
    if g.active:
      dest.add tagToken("ite", n.info)
      dest.copyIntoKind NotX, n.info:
        dest.addSymUse g.cond, n.info
      usedGuard = i
      g.active = false # disable
      dest.addParLe StmtsS, n.info # then section
      break

  case n.stmtKind
  of StmtsS, ScopeS:
    # flat nested statements list:
    if not parentIsStmtList and usedGuard < 0:
      dest.takeToken n
    else:
      inc n
    while n.kind != ParRi:
      trGuardedStmts(c, dest, n, true)
    if not parentIsStmtList and usedGuard < 0:
      dest.takeToken n # ParRi
    else:
      inc n
  of AsgnS:
    trAsgn c, dest, n
  of IfS:
    trIf c, dest, n
  of CaseS:
    trCase c, dest, n
  of WhileS:
    trWhile c, dest, n
  of LocalDecls:
    trLocal c, dest, n
  of ProcS, FuncS, MacroS, MethodS, ConverterS, IteratorS:
    trProcDecl c, dest, n
  of RetS:
    trRet c, dest, n
  of BreakS:
    trBreak c, dest, n
  of BlockS:
    trBlock c, dest, n
  of TemplateS, TypeS:
    takeTree dest, n
  of RaiseS:
    trRaise c, dest, n
  of TryS:
    trTry c, dest, n, parentIsStmtList
  of CallKindsS:
    trStmtCall c, dest, n
  else:
    trExpr c, dest, n

  if usedGuard >= 0:
    # enable again as we are not under the guard anymore:
    c.current.guards[usedGuard].active = true
    dest.addParRi() # then section of ite
    dest.addDotToken() # no else section
    dest.addDotToken() # no join information
    dest.addParRi() # "ite"


proc eliminateJumps*(n: Cursor; moduleSuffix: string; optimizeIte = true): TokenBuf =
  var c = Context(counter: 0, typeCache: createTypeCache(),
                  thisModuleSuffix: moduleSuffix, optimizeIte: optimizeIte)
  c.openScope()
  result = createTokenBuf(300)
  var elimExprs = lowerExprs(n, moduleSuffix, TowardsNjvl)
  var n = beginRead(elimExprs)
  assert n.stmtKind == StmtsS, $n.kind
  result.add n
  inc n
  while n.kind != ParRi:
    trGuardedStmts c, result, n, true
  closeScope c, result, n.info
  result.addParRi()
  endRead elimExprs
  #echo "PRODUCED: ", result.toString(false)

when isMainModule:
  from std/os import paramStr, paramCount
  import std/syncio
  import ".." / lib / symparser
  let infile = paramStr(1)
  let n = setupProgram(infile, infile.changeModuleExt".njvl.nif")
  let r = eliminateJumps(n, "main")
  let output = r.toString(false)
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
