#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

import std/assertions
include nifprelude

import ".." / nimony / [nimony_model, programs, decls]
import hexer_context, iterinliner, desugar, xelim, duplifier, lifter, destroyer,
  constparams, vtables_backend, eraiser

proc publishHooks*(n: var Cursor) =
  var nested = 0
  while true:
    case n.kind
    of ParLe:
      case n.stmtKind
      of ProcS, FuncS, MacroS, MethodS, ConverterS:
        let decl = asRoutine(n)
        var dest = createTokenBuf()
        takeTree(dest, n)
        let sym = decl.name.symId
        publish sym, dest
      else:
        inc n
        inc nested
    of ParRi:
      inc n
      dec nested
    else:
      inc n
    if nested == 0: break

proc transform*(c: var EContext; n: Cursor; moduleSuffix: string): TokenBuf =
  var n = n
  elimForLoops(c, n)

  var n0 = move c.dest
  var c0 = beginRead(n0)

  var n1 = desugar(c0, moduleSuffix, c.activeChecks)
  endRead(n0)

  var c1 = beginRead(n1)
  var nx = lowerExprs(c1, moduleSuffix)
  endRead(n1)

  var c2 = beginRead(nx)
  let ctx = createLiftingCtx(moduleSuffix, c.bits)
  var n2 = injectDups(c2, nx, ctx)
  endRead(nx)

  var c3 = beginRead(n2)
  var needsXelimIgnored = false
  var withRaises = injectRaisingCalls(c3, c.bits div 8, needsXelimIgnored)
  endRead(n2)
  var withRaisesCursor = beginRead(withRaises)

  var n3 = lowerExprs(withRaisesCursor, moduleSuffix)
  endRead(withRaises)

  var c4 = beginRead(n3)
  var n4 = injectDestructors(c4, ctx)
  endRead(n3)

  assert n4[n4.len-1].kind == ParRi
  shrink(n4, n4.len-1)

  if ctx[].dest.len > 0:
    var hookCursor = beginRead(ctx[].dest)
    #echo "HOOKS: ", toString(hookCursor)
    publishHooks hookCursor
    endRead(ctx[].dest)

  n4.add move(ctx[].dest)
  n4.addParRi()

  var needsXelimAgain = false

  var c5 = beginRead(n4)
  var nwithvtables = transformVTables(c5, moduleSuffix, needsXelimAgain)
  endRead(n4)

  var c6 = beginRead(nwithvtables)
  var n5 = injectConstParamDerefs(c6, c.bits div 8, needsXelimAgain)
  endRead(nwithvtables)

  if needsXelimAgain:
    var c7 = beginRead(n5)
    var n6 = lowerExprs(c7, moduleSuffix)
    endRead(n5)
    result = move n6
  else:
    result = move n5
