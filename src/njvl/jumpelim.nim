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
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, typenav]
import ".." / hexer / [xelim, mover]
import versiontabs

type
  Guard = object
    cond: SymId
    blockName: SymId # used for named `block` statements
    version: int # version of the guard that is active or -1 if inactive
    negate: bool

  CurrentBlock* {.acyclic.} = ref object
    parent: CurrentBlock

  CurrentProc = object
    addrTaken: HashSet[SymId]
    retFlag: SymId
    guards: seq[Guard]
    tmpCounter: int

  Context* = object
    typeCache: TypeCache
    counter: int
    thisModuleSuffix: string
    current: CurrentProc
    vt: VersionTab

proc setupProc(c: var Context; procBody: Cursor) =
  # detect `addr x` and mark `x` as addrTaken:
  var n = procBody
  var nested = 0
  while true:
    case n.kind
    of ParLe:
      if n.exprKind == AddrX:
        let r = rootOf(n, CanFollowCalls)
        if r != NoSymId:
          c.current.addrTaken.incl r
      inc nested
    of ParRi:
      dec nested
    else:
      discard
    if nested == 0: break

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

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor; parentIsStmtList=false)
proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc trParams(c: var Context; params: Cursor) =
  var n = params
  inc n # skips (params
  while n.kind != ParRi:
    let r = takeLocal(n, SkipFinalParRi)
    if r.name.kind == SymbolDef:
      c.vt.newValueFor r.name.symId # register parameter as known location

proc declareCfVar(dest: var TokenBuf; s: SymId) =
  dest.addParLe VarS, NoLineInfo
  dest.add tagToken("v", NoLineInfo)
  dest.addSymDef s, NoLineInfo
  dest.addIntLit 0, NoLineInfo
  dest.addParRi() # "v"
  dest.addDotToken() # export marker
  dest.addDotToken() # pragmas
  dest.addParPair BoolT, NoLineInfo
  dest.addParPair FalseX, NoLineInfo
  dest.addParRi()

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  var r = asRoutine(n)
  let oldProc = move c.current
  c.current = CurrentProc(tmpCounter: 1)
  c.current.retFlag = pool.syms.getOrIncl("´r.0")
  c.vt.newValueFor c.current.retFlag

  copyInto(dest, n):
    let isConcrete = c.typeCache.takeRoutineHeader(dest, decl, n)
    if isConcrete:
      let symId = r.name.symId
      if isLocalDecl(symId):
        c.typeCache.registerLocal(symId, r.kind, decl)
      c.typeCache.openScope()
      trParams c, r.params
      let info = n.info
      copyIntoKind dest, StmtsS, info:
        openScope c
        declareCfVar dest, c.current.retFlag
        trStmt c, dest, n, true
        # XXX Declare return label here
        closeScope c, dest, info
      c.typeCache.closeScope()
    else:
      takeTree dest, n
  c.current = ensureMove oldProc

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var fnType = skipProcTypeToParams(getType(c.typeCache, n.firstSon))
  assert isParamsTag(fnType)
  var pragmas = fnType
  skip pragmas
  #let retType = pragmas
  skip pragmas
  let canRaise = hasPragma(pragmas, RaisesP)

  if canRaise:
    discard "XXX produce failed flag"

  let info = n.info
  dest.add n
  inc n # skip `(call)`
  trExpr c, dest, n # handle `fn`
  var mutates: seq[SymId] = @[]
  while n.kind != ParRi:
    if n.exprKind == HaddrX:
      let r = rootOf(n, CanFollowCalls)
      if r != NoSymId:
        if not c.current.addrTaken.contains(r):
          mutates.add r
    trExpr c, dest, n
  dest.takeParRi n
  for s in mutates:
    dest.add tagToken("unknown", info)
    newValueFor c.vt, s
    let v = c.vt.getVersion(s)
    dest.add tagToken("v", info)
    dest.addSymUse s, info
    dest.addIntLit v, info
    dest.addParRi()
    dest.addParRi() # unknown

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  case n.kind
  of Symbol:
    let s = n.symId
    let v = c.vt.getVersion(s)
    if v < 0:
      dest.addSymUse s, info
    else:
      dest.add tagToken("v", info)
      dest.addSymUse s, info
      dest.addIntLit v, info
      dest.addParRi()
  of UnknownToken, EofToken, DotToken, Ident, SymbolDef, StringLit, CharLit, IntLit, UIntLit, FloatLit:
    dest.takeToken n
  of ParLe:
    case n.exprKind
    of CallKinds:
      trCall c, dest, n
    else:
      dest.takeToken n
      while n.kind != ParRi:
        trExpr c, dest, n
      dest.takeToken n

  of ParRi: bug "Unmatched ParRi"

proc trAsgn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.add tagToken("asgn", info)
  inc n
  let r = rootOf(n, CanFollowCalls)
  # we have to watch out: For the lhs the new version is
  # active immediately, for the rhs it is not: Thus we process the rhs first.
  var rhs = n
  skip rhs
  var rhsDest = createTokenBuf(10)
  trExpr c, rhsDest, rhs

  if r != NoSymId:
    if not c.current.addrTaken.contains(r):
      newValueFor c.vt, r

  trExpr c, dest, n # lhs
  n = rhs
  skipParRi n
  dest.add rhsDest
  dest.addParRi()

proc trIf(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # we assume here that xelim already produced a single elif-else construct here
  let info = n.info
  dest.add tagToken("ite", n.info)
  inc n
  assert n.substructureKind == ElifU
  inc n
  trExpr c, dest, n
  openSection c.vt
  openScope c
  trStmt c, dest, n
  closeScope c, dest, info
  closeSection c.vt
  skipParRi n
  openSection c.vt
  if n.kind != ParRi:
    assert n.substructureKind == ElseU
    inc n
    openScope c
    trStmt c, dest, n
    closeScope c, dest, info
    skipParRi n
  closeSection c.vt
  skipParRi n
  # join information:
  dest.addParLe StmtsS, info
  let joinData = combineJoin(c.vt, IfJoin)
  for s, j in joinData:
    dest.add tagToken("join", info)
    dest.addSymUse s, info
    dest.addIntLit j.newv, info
    dest.addIntLit j.old1, info
    dest.addIntLit j.old2, info
    dest.addParRi()

  dest.addParRi() # join information
  dest.addParRi() # "ite"

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    let symId = n.symId
    c.typeCache.takeLocalHeader(dest, n, kind)
    trExpr c, dest, n
    if not c.current.addrTaken.contains(symId):
      # initial version for variable!
      newValueFor c.vt, symId

proc trBreak(c: var Context; dest: var TokenBuf; n: var Cursor) =
  assert c.current.guards.len > 0

  var entries = 1 # only care about the inner most
  inc n
  if n.kind != ParRi:
    if n.kind == DotToken:
      inc n
    elif n.kind == Symbol:
      for i in countdown(c.current.guards.len - 1, 0):
        if c.current.guards[i].blockName == n.symId: break
        inc entries

  for i in 1..entries:
    let g = addr c.current.guards[c.current.guards.len - i]
    dest.add tagToken("jtrue", n.info)
    dest.addSymUse g.cond, n.info
    dest.addParRi()
    g.version = c.vt.getVersion(g.cond)

  skipParRi n

proc trBlock(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let guard = pool.syms.getOrIncl("´g." & $c.current.tmpCounter)
  inc c.current.tmpCounter

  declareCfVar dest, guard
  inc n # "block"
  let blockName = if n.kind == SymbolDef: n.symId else: NoSymId
  inc n # name or empty
  openScope c
  c.current.guards.add Guard(cond: guard, version: -1, negate: false, blockName: blockName)
  let myGuardAt = c.current.guards.len - 1

  assert n.stmtKind == StmtsS
  var usedGuard = false
  while n.kind != ParRi:
    let v = c.current.guards[myGuardAt].version
    if v >= 0:
      # the rest of the block body must be protected by the guard:
      dest.add tagToken("ite", n.info)
      dest.addSymUse guard, n.info
      c.current.guards[myGuardAt].version = -1 # but only once
      dest.addParLe StmtsS, n.info # then section
      usedGuard = true
    trStmt c, dest, n

  if usedGuard:
    dest.addParRi() # then section of ite
    dest.addDotToken() # no else section
    dest.addParRi() # "ite"
  c.current.guards.shrink(myGuardAt)
  closeScope c, dest, n.info


proc trWhileTrue(c: var Context; dest: var TokenBuf; n: var Cursor) =
  openSection c.vt

  assert n.stmtKind == StmtsS
  dest.takeToken n

  let guard = pool.syms.getOrIncl("´g." & $c.current.tmpCounter)
  inc c.current.tmpCounter

  declareCfVar dest, guard
  c.current.guards.add Guard(cond: guard, version: -1, negate: false)
  let myGuardAt = c.current.guards.len - 1

  dest.addParRi() # "stmts" that is the pre-condition body
  dest.addSymUse guard, n.info # condition is always our artifical guard

  dest.addParLe StmtsS, n.info # loop body
  openScope c
  var usedGuard = false
  while n.kind != ParRi:
    let v = c.current.guards[myGuardAt].version
    if v >= 0:
      # the rest of the loop body must be protected by the guard:
      dest.add tagToken("ite", n.info)
      dest.addSymUse guard, n.info
      c.current.guards[myGuardAt].version = -1 # but only once
      dest.addParLe StmtsS, n.info # then section
      usedGuard = true
    trStmt c, dest, n

  if usedGuard:
    dest.addParRi() # then section of ite
    dest.addDotToken() # no else section
    dest.addParRi() # "ite"

  closeScope c, dest, n.info
  c.current.guards.shrink(myGuardAt)

  # last statement of our loop body is the `continue`:
  closeSection c.vt
  dest.addParLe ContinueS, n.info
  let joinData = combineJoin(c.vt, LoopEither)
  # `either` seems to be flawed as we need a new version after the loop
  # as we don't know if the loop ran a single time or not!
  for s, j in joinData:
    dest.add tagToken("join", n.info)
    dest.addSymUse s, n.info
    dest.addIntLit j.newv, n.info
    dest.addIntLit j.old1, n.info
    dest.addIntLit j.old2, n.info
    dest.addParRi()
  dest.addParRi() # Continue statement

  dest.addParRi() # close loop body

proc trWhile(c: var Context; dest: var TokenBuf; n: var Cursor) =
  dest.add tagToken("loop", n.info)
  inc n

  # special case `while true` as it plays into our hands:
  if n.exprKind == TrueX:
    inc n
    skipParRi n
    trWhileTrue c, dest, n
    dest.addParRi() # close "loop"
  else:
    bug "unimplemented `while` loop structure"

proc trRet(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.add tagToken("jtrue", info)
  dest.addSymUse c.current.retFlag, info
  dest.addParRi()
  inc n
  # we also need to break out of all loops currently active:
  for i in countdown(c.current.guards.len - 1, 0):
    c.current.guards[i].version = c.vt.getVersion(c.current.guards[i].cond)

  if n.kind == ParRi:
    inc n
  else:
    if n.kind == DotToken:
      inc n
      skipParRi n
    else:
      bug "return should not have a value"

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor; parentIsStmtList=false) =
  case n.stmtKind
  of StmtsS, ScopeS:
    # flat nested statements list:
    if not parentIsStmtList:
      dest.takeToken n
    else:
      inc n
    while n.kind != ParRi:
      trStmt(c, dest, n, true)
    if not parentIsStmtList:
      dest.takeToken n # ParRi
    else:
      inc n
  of AsgnS:
    trAsgn c, dest, n
  of IfS:
    trIf c, dest, n
  of WhileS:
    trWhile c, dest, n
  of LocalDecls - {ResultS}:
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
  else:
    bug "Unhandled stmt kind: " & $n.stmtKind


proc toNjvl*(n: Cursor; moduleSuffix: string): TokenBuf =
  var c = Context(counter: 0, typeCache: createTypeCache(), thisModuleSuffix: moduleSuffix, vt: createVersionTab())
  c.typeCache.openScope()
  result = createTokenBuf(300)
  var elimExprs = lowerExprs(n, moduleSuffix, TowardsNjvl)
  var n = beginRead(elimExprs)
  assert n.stmtKind == StmtsS, $n.kind
  result.add n
  inc n
  while n.kind != ParRi:
    trStmt c, result, n
  result.addParRi()
  c.typeCache.closeScope()
  endRead elimExprs
  #echo "PRODUCED: ", result.toString(false)

when isMainModule:
  let n = setupProgram("debug.txt", "debug.out")
  let r = toNjvl(n, "main")
  echo r.toString(false)
