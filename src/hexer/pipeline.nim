#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

include nifprelude

import basics, iterinliner, desugar, xelim, duplifier, lifter, destroyer

proc transform*(c: var EContext; n: Cursor; moduleSuffix: string): TokenBuf =
  var n = n
  elimForLoops(c, n)

  var n0 = move c.dest
  var c0 = beginRead(n0)

  var n1 = desugar(c0, moduleSuffix)
  endRead(n0)

  var c1 = beginRead(n1)
  let ctx = createLiftingCtx()
  var n2 = injectDups(c1, ctx)
  endRead(n1)

  var c2 = beginRead(n2)

  var n3 = lowerExprs(c2, moduleSuffix)
  endRead(n2)

  var c3 = beginRead(n3)
  result = injectDestructors(c3, ctx)
  endRead(n3)

  shrink(result, result.len-1)
  result.add move(ctx[].dest)
  result.addParRi()
