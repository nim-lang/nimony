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
  constparams, vtables_backend, eraiser, lambdalifting, cps, passes

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

proc transform*(c: var EContext; n: Cursor; moduleSuffix: string; bits: int): TokenBuf =
  # Prepare initial buffer from elimForLoops
  var n = n
  elimForLoops(c, n)
  var initialBuf = move c.dest

  # Initialize the Pass pipeline
  var pass = initPass(initialBuf, moduleSuffix, "desugar", bits)

  # Pass 1: Desugar
  desugar(pass, c.activeChecks)

  # Pass 2: CPS Transformation
  pass.prepareForNext("cps")
  transformToCps(pass)

  # Pass 3: Lambda Lifting
  pass.prepareForNext("lambdalift")
  elimLambdas(pass)

  # Pass 6: Inject Raising Calls (Exception Handling)
  pass.prepareForNext("eraiser")
  var needsXelimIgnored = false
  injectRaisingCalls(pass, c.bits div 8, needsXelimIgnored)

  # Pass 4: Lower Expressions (first time)
  pass.prepareForNext("xelim1")
  lowerExprs(pass)

  # Pass 5: Inject Duplication Points
  pass.prepareForNext("duplifier")
  injectDups(pass, c.liftingCtx)


  # Pass 7: Lower Expressions (second time, after raises)
  pass.prepareForNext("xelim2")
  lowerExprs(pass)

  # Pass 8: Inject Destructors (RAII/Cleanup)
  pass.prepareForNext("destroyer")
  injectDestructors(pass, c.liftingCtx)

  # Special handling: Merge generated hooks
  assert pass.dest[pass.dest.len-1].kind == ParRi
  shrink(pass.dest, pass.dest.len-1)

  if c.liftingCtx[].dest.len > 0:
    var hookReader = beginRead(c.liftingCtx[].dest)
    #echo "HOOKS: ", toString(hookReader)
    publishHooks hookReader
    endRead(c.liftingCtx[].dest)

  pass.dest.add move(c.liftingCtx[].dest)
  pass.dest.addParRi()

  # Pass 9: Transform VTables (Virtual Table Backend)
  var needsXelimAgain = false
  pass.prepareForNext("vtables")
  transformVTables(pass, needsXelimAgain)

  # Pass 10: Inject Const Param Dereferences
  pass.prepareForNext("constparams")
  injectConstParamDerefs(pass, c.bits div 8, needsXelimAgain)

  # Pass 11 (Conditional): Final Lower Expressions if needed
  if needsXelimAgain:
    pass.prepareForNext("xelim_final")
    lowerExprs(pass)

  result = ensureMove(pass.dest)
