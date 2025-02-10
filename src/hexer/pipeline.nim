#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

include nifprelude

import ".." / nimony / [nimony_model, programs, decls]
import basics, iterinliner, desugar, xelim, duplifier, lifter, destroyer, constparams

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

  var n1 = desugar(c0, moduleSuffix)
  endRead(n0)

  var c2 = beginRead(n1)
  let ctx = createLiftingCtx()
  var n2 = injectDups(c2, ctx)
  endRead(n1)

  var c3 = beginRead(n2)
  var n3 = injectDestructors(c3, ctx)
  endRead(n2)

  shrink(n3, n3.len-1)

  if ctx[].dest.len > 0:
    var hookCursor = beginRead(ctx[].dest)
    publishHooks hookCursor
    endRead(ctx[].dest)

  n3.add move(ctx[].dest)
  n3.addParRi()

  var c4 = beginRead(n3)
  var n4 = injectConstParamDerefs(c4, c.bits div 8)
  endRead(n3)

  var c5 = beginRead(n4)
  var n5 = lowerExprs(c5, moduleSuffix)
  endRead(n4)

  result = move n5
