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

import std / [tables, hashes, sets, assertions, syncio]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs, typenav]
import ".." / hexer / [mover, passes]

import versiontabs, nj, njvl_model

type
  CurrentProc = object
    addrTaken: HashSet[SymId]

  Context* = object
    typeCache: TypeCache
    current: CurrentProc
    vt: VersionTab

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor)
proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc setupProc(c: var Context; procBody: Cursor) =
  # detect `addr x` and mark `x` as addrTaken:
  var n = procBody
  linearScan n:
    if n.exprKind == AddrX:
      let r = rootOf(n, CanFollowCalls)
      if r != NoSymId:
        c.current.addrTaken.incl r

proc trParams(c: var Context; params: Cursor) =
  var n = params
  n = sub(n) # skips (params; peek only, never left
  while n.hasMore:
    let r = takeLocal(n, SkipFinalParRi)
    if r.name.isSymbolDef:
      c.vt.newValueFor r.name.symId # register parameter as known location

proc trCfvar(c: var Context; dest: var TokenBuf; n: var Cursor) =
  takeInto dest, n:
    assert n.isSymbolDef
    let s = n.symId
    dest.takeTree n
    # Do versioning for cfvars!
    newValueFor c.vt, s

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  var r = asRoutine(n)
  let oldProc = move c.current

  copyInto(dest, n):
    let isConcrete = c.typeCache.takeRoutineHeader(dest, decl, n)
    if isConcrete:
      let symId = r.name.symId
      if isLocalDecl(symId):
        c.typeCache.registerLocal(symId, r.kind, decl)
      c.typeCache.openScope()
      # Fresh version table per procedure so params/history from other procs don't leak into joins.
      # Save/restore parent vt so nested procs don't clobber the outer procedure's state.
      let parentVt = move c.vt
      c.vt = createVersionTab()
      trParams c, r.params
      let info = n.info
      setupProc c, n
      copyIntoKind dest, StmtsS, info:
        trStmt c, dest, n
      c.typeCache.closeScope()
      c.vt = ensureMove parentVt
    else:
      takeTree dest, n
  c.current = ensureMove oldProc

proc trUnknown(c: var Context; dest: var TokenBuf; n: var Cursor) =
  takeInto dest, n:
    let r = rootOf(n, CanFollowCalls)
    if r != NoSymId:
      if not c.current.addrTaken.contains(r):
        newValueFor c.vt, r
    trExpr c, dest, n

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # for now we leave the unknown instructions where they are as they do not hurt us.
  # We can later push them around.
  takeInto dest, n:
    while n.hasMore:
      trExpr c, dest, n

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  case n.kind
  of Symbol:
    let s = n.symId
    let v = c.vt.getVersion(s)
    if v < 0:
      dest.addSymUse s, info
    else:
      dest.addParLe("v", info)
      dest.addSymUse s, info
      dest.addIntLit v, info
      dest.addParRi()
    inc n
  of UnknownToken, EofToken, ParLe, ParRi, ExtendedSuffix, LineInfoLit, DotToken, Ident, SymbolDef, StrLit, CharLit, IntLit, UIntLit, FloatLit:
    dest.takeTree n
  of TagLit:
    case n.exprKind
    of CallKinds:
      trCall c, dest, n
    of DotX, DdotX:
      takeInto dest, n:
        trExpr c, dest, n
        # field name:
        dest.takeTree n
        if n.hasMore:
          # inheritance depth:
          takeTree dest, n
        if n.hasMore:
          # optional access-token string lit
          takeTree dest, n
    else:
      if n.substructureKind == KvU:
        takeInto dest, n:
          dest.takeTree n # key, don't versionize!
          trExpr c, dest, n
          if n.hasMore:
            # inheritance depth:
            takeTree dest, n
      else:
        takeInto dest, n:
          while n.hasMore:
            trExpr c, dest, n
  else: bug "Unmatched ParRi" # classic: a physical ParRi; nifcore: suffix kinds (never heads)

proc trStore(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.addParLe("store", info)
  let storeStart = n
  n = sub(n)
  trExpr c, dest, n # source

  let r = rootOf(n, CanFollowCalls)
  if r != NoSymId:
    if not c.current.addrTaken.contains(r):
      newValueFor c.vt, r

  trExpr c, dest, n # dest
  dest.addParRi(n.endInfo)
  n = storeStart; skip n

proc trIte(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.addParLe(n.cursorTagId, n.info) # "ite" or "itec"
  let iteStart = n
  n = sub(n)
  trExpr c, dest, n
  openSection c.vt
  openScope c.typeCache
  trStmt c, dest, n
  closeScope c.typeCache
  closeSection c.vt
  openSection c.vt
  openScope c.typeCache
  if n.hasMore:
    trStmt c, dest, n
  else:
    # repair broken ite statements (missing else):
    dest.addDotToken()
  closeScope c.typeCache
  closeSection c.vt
  # join information is optional here:
  if n.hasMore:
    skip n # ignore the currently empty join information
  dest.addParLe StmtsS, info
  let joinData = combineJoin(c.vt, IfJoin)
  for s, j in joinData:
    if isValid(j):
      dest.addParLe("join", info)
      dest.addSymUse s, info
      dest.addIntLit j.newv, info
      dest.addIntLit j.old1, info
      dest.addIntLit j.old2, info
      dest.addParRi()

  dest.addParRi() # join information
  dest.addParRi(n.endInfo) # "ite"
  n = iteStart; skip n

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    let symId = n.symId
    c.typeCache.takeLocalHeader(dest, n, kind)
    trExpr c, dest, n
    if not c.current.addrTaken.contains(symId):
      # initial version for variable!
      newValueFor c.vt, symId

proc trLoop(c: var Context; dest: var TokenBuf; n: var Cursor) =
  openSection c.vt

  dest.addParLe(n.cursorTagId, n.info) # "loop"
  let loopStart = n
  n = sub(n)

  openScope c.typeCache
  trStmt c, dest, n # pre condition
  trExpr c, dest, n # condition
  assert n.stmtKind == StmtsS
  dest.addParLe(n.cursorTagId, n.info)
  let bodyStart = n
  n = sub(n)
  while n.hasMore:
    trStmt c, dest, n # body
  # last statement of our loop body is the `continue`:
  closeSection c.vt
  dest.addParLe ContinueS, n.info
  let joinData = combineJoin(c.vt, LoopEither)
  # `either` seems to be flawed as we need a new version after the loop
  # as we don't know if the loop ran a single time or not!
  for s, j in joinData:
    if isValid(j):
      dest.addParLe("join", n.info)
      dest.addSymUse s, n.info
      dest.addIntLit j.newv, n.info
      dest.addIntLit j.old1, n.info
      dest.addIntLit j.old2, n.info
      dest.addParRi()
  dest.addParRi() # Continue statement
  dest.addParRi(n.endInfo) # close loop body
  n = bodyStart; skip n
  dest.addParRi(n.endInfo) # close loop
  n = loopStart; skip n

proc trKill(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # Do not version the variables here!
  takeInto dest, n:
    while n.hasMore:
      assert n.isSymbol
      let s = n.symId
      killVar c.vt, s
      dest.takeTree n

proc trJtrue(c: var Context; dest: var TokenBuf; n: var Cursor) =
  takeInto dest, n:
    while n.hasMore:
      assert n.isSymbol
      let s = n.symId
      dest.takeTree n
      # Do versioning for cfvars!
      newValueFor c.vt, s

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.njvlKind
  of IteV, ItecV:
    trIte c, dest, n
  of LoopV:
    trLoop c, dest, n
  of KillV:
    trKill c, dest, n
  of StoreV:
    trStore c, dest, n
  of MflagV, VflagV:
    trCfvar c, dest, n
  of UnknownV:
    trUnknown c, dest, n
  of JtrueV:
    trJtrue c, dest, n
  of ContinueV:
    # we produce a filled `continue` statement in trLoop
    skip n
  of AssumeV, AssertV, VV, LabV, JmpV:
    takeTree dest, n
  of NoVTag, EtupatV:
    case n.stmtKind
    of NoStmt:
      trExpr c, dest, n
    of ProcS, FuncS, MethodS, ConverterS, IteratorS:
      trProcDecl c, dest, n
    of LocalDecls:
      trLocal c, dest, n
    of AsgnS, IfS, WhileS, CaseS, TryS, BreakS, RaiseS:
      bug "construct should have been eliminated: " & $n.stmtKind
    of MacroS, TemplateS, TypeS:
      # Macro bodies belong to the plugin compilation, not the user's
      # codegen pipeline. Pass through opaquely.
      takeTree dest, n
    of ContinueS:
      skip n
    else:
      takeInto dest, n:
        while n.hasMore:
          trStmt c, dest, n

proc toNjvl*(n: Cursor; moduleSuffix: string): TokenBuf =
  var c = Context(typeCache: createTypeCache(), vt: createVersionTab())
  c.typeCache.openScope()
  result = createTokenBuf(300)
  var initialBuf = createTokenBuf(300)
  initialBuf.addSubtree(n)
  var pass = initPass(move initialBuf, moduleSuffix, "xelim_njvl", 0)
  eliminateJumps(pass)
  var elimJumps = ensureMove(pass.dest)
  var n = beginRead(elimJumps)
  assert n.stmtKind == StmtsS, $n.kind
  result.addParLe(n.cursorTagId, n.info)
  n.into:
    while n.hasMore:
      trStmt c, result, n
  result.addParRi()
  c.typeCache.closeScope()

when isMainModule:
  from std/os import paramStr, paramCount
  import std/syncio
  import ".." / lib / symparser
  let infile = paramStr(1)
  var owningBuf = createTokenBuf(300)
  let n = setupProgram(infile, infile.changeModuleExt".njvl.nif", owningBuf)
  let r = toNjvl(n, "main")
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
