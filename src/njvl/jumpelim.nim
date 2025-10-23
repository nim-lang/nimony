#
#
#           Nimony Jump Elimination Pass
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Compiles Nimony IR to a simpler IR called [NJVL](doc/njvl.md) that does not contain jumps.

import std / [assertions]
include ".." / lib / nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, typenav]
import ".." / hexer / [xelim]
import versiontabs

type
  Guard = object
    cond: SymId
    negate: bool

  CurrentBlock* {.acyclic.} = ref object
    parent: CurrentBlock

  Context* = object
    typeCache: TypeCache
    counter: int
    thisModuleSuffix: string
    current: CurrentBlock
    vt: VersionTab

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
