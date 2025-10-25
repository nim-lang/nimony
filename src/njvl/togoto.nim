#
#
#           Nimony Goto Insertion Pass
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Undoes the effects of NJVL by turning `(jtrue X)` statements into goto statements, if possible.
import std / [tables, sets, assertions]
include ".." / lib / nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav]
import ".." / hexer / [mover]

import njvl_model

type
  Context* = object
    typeCache: TypeCache

proc trJtrue(c: var Context; dest: var TokenBuf; n: var Cursor) =
  takeTree dest, n

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.njvlKind
  of JtrueV:
    trJtrue c, dest, n
  else:
    dest.takeToken n
    while n.kind != ParRi:
      trStmt c, dest, n
    dest.takeToken n

proc toGoto*(n: Cursor; moduleSuffix: string): TokenBuf =
  var c = Context(typeCache: createTypeCache())
  var n = n
  c.typeCache.openScope()
  result = createTokenBuf(300)
  assert n.stmtKind == StmtsS, $n.kind
  result.add n
  inc n
  while n.kind != ParRi:
    trStmt c, result, n
  result.addParRi()
  c.typeCache.closeScope()
