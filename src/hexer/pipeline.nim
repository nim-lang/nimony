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
  constparams, vtables_backend, eraiser, lambdalifting, cps

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

  var initialBuf = move c.dest
  var desugarReader = beginRead(initialBuf)

  var desugaredBuf = desugar(desugarReader, moduleSuffix, c.activeChecks)
  endRead(initialBuf)

  var lambdaLiftingReader = beginRead(desugaredBuf)
  var lambdaLiftedBuf = elimLambdas(lambdaLiftingReader, moduleSuffix)
  endRead(desugaredBuf)

  var lowerExprsReader1 = beginRead(lambdaLiftedBuf)
  var nx = lowerExprs(lowerExprsReader1, moduleSuffix)
  endRead(lambdaLiftedBuf)

  var duplicationReader = beginRead(nx)
  var duplicatedBuf = injectDups(duplicationReader, moduleSuffix, nx, c.liftingCtx)
  endRead(nx)

  var raisesReader = beginRead(duplicatedBuf)
  var needsXelimIgnored = false
  var withRaises = injectRaisingCalls(raisesReader, c.bits div 8, needsXelimIgnored)
  endRead(duplicatedBuf)

  var loweredReader = beginRead(withRaises)
  var loweredBuf = lowerExprs(loweredReader, moduleSuffix)
  endRead(withRaises)

  var destructorReader = beginRead(loweredBuf)
  var destructorBuf = injectDestructors(destructorReader, c.liftingCtx)
  endRead(loweredBuf)

  assert destructorBuf[destructorBuf.len-1].kind == ParRi
  shrink(destructorBuf, destructorBuf.len-1)

  if c.liftingCtx[].dest.len > 0:
    var hookReader = beginRead(c.liftingCtx[].dest)
    #echo "HOOKS: ", toString(hookReader)
    publishHooks hookReader
    endRead(c.liftingCtx[].dest)

  destructorBuf.add move(c.liftingCtx[].dest)
  destructorBuf.addParRi()

  var cpsReader = beginRead(destructorBuf)
  var cpsBuf = transformToCps(cpsReader, moduleSuffix)
  endRead(destructorBuf)

  var needsXelimAgain = false

  var vtableReader = beginRead(cpsBuf)
  var nwithvtables = transformVTables(vtableReader, moduleSuffix, needsXelimAgain)
  endRead(cpsBuf)

  var constParamReader = beginRead(nwithvtables)
  var constParamBuf = injectConstParamDerefs(constParamReader, c.bits div 8, needsXelimAgain)
  endRead(nwithvtables)

  if needsXelimAgain:
    var finalLowerExprsReader = beginRead(constParamBuf)
    var finalBuf = lowerExprs(finalLowerExprsReader, moduleSuffix)
    endRead(constParamBuf)
    result = move finalBuf
  else:
    result = move constParamBuf
