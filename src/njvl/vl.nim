#
#
#           Nimony add location versions Pass
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Compiles Nimony IR to a simpler IR called [NJVL](doc/njvl.md) that does not contain jumps.
##
## We must add version information to all locations.
## This cannot be done in one step with `jumpelim` because
## the `join` needs to be generated for every `ite` and `jumpelim``
## adds new `ite` statements.

import std / [tables, sets, assertions]
include ".." / lib / nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav]
import ".." / hexer / [mover]

import versiontabs, nj, njvl_model

type
  CurrentProc = object
    addrTaken: HashSet[SymId]
    resultSym: SymId
    tmpCounter: int

  Context* = object
    typeCache: TypeCache
    counter: int
    thisModuleSuffix: string
    current: CurrentProc
    vt: VersionTab

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor)
proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor)

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

proc trParams(c: var Context; params: Cursor) =
  var n = params
  inc n # skips (params
  while n.kind != ParRi:
    let r = takeLocal(n, SkipFinalParRi)
    if r.name.kind == SymbolDef:
      c.vt.newValueFor r.name.symId # register parameter as known location

proc trCfvar(c: var Context; dest: var TokenBuf; n: var Cursor) =
  dest.takeToken n
  assert n.kind == SymbolDef
  let s = n.symId
  # do not versionize cfvars!
  c.current.addrTaken.incl s
  dest.takeParRi n

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  var r = asRoutine(n)
  let oldProc = move c.current
  c.current = CurrentProc(tmpCounter: 1)

  copyInto(dest, n):
    let isConcrete = c.typeCache.takeRoutineHeader(dest, decl, n)
    if isConcrete:
      let symId = r.name.symId
      if isLocalDecl(symId):
        c.typeCache.registerLocal(symId, r.kind, decl)
      c.typeCache.openScope()
      trParams c, r.params
      let info = n.info
      setupProc c, n
      copyIntoKind dest, StmtsS, info:
        trStmt c, dest, n
      c.typeCache.closeScope()
    else:
      takeTree dest, n
  c.current = ensureMove oldProc

proc trUnknown(c: var Context; dest: var TokenBuf; n: var Cursor) =
  dest.takeToken n
  let r = rootOf(n, CanFollowCalls)
  if r != NoSymId:
    if not c.current.addrTaken.contains(r):
      newValueFor c.vt, r
  trExpr c, dest, n
  dest.takeParRi n

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # for now we leave the unknown instructions where they are as they do not hurt us.
  # We can later push them around.
  dest.takeToken n
  while n.kind != ParRi:
    if n.njvlKind == UnknownV:
      trUnknown c, dest, n
    else:
      trExpr c, dest, n
  dest.takeToken n

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

proc trStore(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.add tagToken("store", info)
  inc n
  trExpr c, dest, n # source

  let r = rootOf(n, CanFollowCalls)
  if r != NoSymId:
    if not c.current.addrTaken.contains(r):
      newValueFor c.vt, r

  trExpr c, dest, n # dest
  dest.takeParRi n

proc trIte(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.add tagToken("ite", n.info)
  inc n
  trExpr c, dest, n
  openSection c.vt
  openScope c.typeCache
  trStmt c, dest, n
  closeScope c.typeCache
  closeSection c.vt
  openSection c.vt
  openScope c.typeCache
  trStmt c, dest, n
  closeScope c.typeCache
  closeSection c.vt
  # join information:
  skip n # ignore the currently empty join information
  dest.addParLe StmtsS, info
  let joinData = combineJoin(c.vt, IfJoin)
  for s, j in joinData:
    if isValid(j):
      dest.add tagToken("join", info)
      dest.addSymUse s, info
      dest.addIntLit j.newv, info
      dest.addIntLit j.old1, info
      dest.addIntLit j.old2, info
      dest.addParRi()

  dest.addParRi() # join information
  dest.takeParRi n # "ite"

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    let symId = n.symId
    if kind == ResultY:
      c.current.resultSym = symId
    c.typeCache.takeLocalHeader(dest, n, kind)
    trExpr c, dest, n
    if not c.current.addrTaken.contains(symId):
      # initial version for variable!
      newValueFor c.vt, symId

proc trLoop(c: var Context; dest: var TokenBuf; n: var Cursor) =
  openSection c.vt

  dest.takeToken n # "loop"

  openScope c.typeCache
  trStmt c, dest, n # pre condition
  trExpr c, dest, n # condition
  trStmt c, dest, n # body
  # last statement of our loop body is the `continue`:
  closeSection c.vt
  skip n # ignore the currently empty join information
  dest.addParLe ContinueS, n.info
  let joinData = combineJoin(c.vt, LoopEither)
  # `either` seems to be flawed as we need a new version after the loop
  # as we don't know if the loop ran a single time or not!
  for s, j in joinData:
    if isValid(j):
      dest.add tagToken("join", n.info)
      dest.addSymUse s, n.info
      dest.addIntLit j.newv, n.info
      dest.addIntLit j.old1, n.info
      dest.addIntLit j.old2, n.info
      dest.addParRi()
  dest.addParRi() # Continue statement
  dest.takeParRi n # close loop body

proc trKill(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # Do not version the variables here!
  dest.takeTree n

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.njvlKind
  of IteV:
    trIte c, dest, n
  of LoopV:
    trLoop c, dest, n
  of KillV:
    trKill c, dest, n
  of StoreV:
    trStore c, dest, n
  of CfvarV:
    trCfvar c, dest, n
  of UnknownV:
    trUnknown c, dest, n
  else:
    case n.stmtKind
    of NoStmt:
      trExpr c, dest, n
    of ProcS, FuncS, MacroS, MethodS, ConverterS, IteratorS:
      trProcDecl c, dest, n
    of LocalDecls:
      trLocal c, dest, n
    of AsgnS:
      bug "(asgn) should have been translated to store"
    else:
      dest.takeToken n
      while n.kind != ParRi:
        trStmt c, dest, n
      dest.takeToken n

proc toNjvl*(n: Cursor; moduleSuffix: string): TokenBuf =
  var c = Context(counter: 0, typeCache: createTypeCache(), thisModuleSuffix: moduleSuffix, vt: createVersionTab())
  c.typeCache.openScope()
  result = createTokenBuf(300)
  var elimJumps = eliminateJumps(n, moduleSuffix)
  var n = beginRead(elimJumps)
  assert n.stmtKind == StmtsS, $n.kind
  result.add n
  inc n
  while n.kind != ParRi:
    trStmt c, result, n
  result.addParRi()
  c.typeCache.closeScope()
  endRead elimJumps
