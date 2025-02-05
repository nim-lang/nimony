#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

include nifprelude

import basics, iterinliner, desugar, xelim, duplifier, lifter, destroyer, constparams

proc transform*(c: var EContext; n: Cursor; moduleSuffix: string): TokenBuf =
  var n = n
  elimForLoops(c, n)

  var n0 = move c.dest
  var c0 = beginRead(n0)

  var n1 = desugar(c0, moduleSuffix)
  endRead(n0)

  var c2 = beginRead(n1)
  var n2 = injectConstParamDerefs(c2, 8)
  endRead(n1)

  var c3 = beginRead(n2)
  let ctx = createLiftingCtx()
  var n3 = injectDups(c3, ctx)
  endRead(n2)

  var c4 = beginRead(n3)
  var n4 = lowerExprs(c4, moduleSuffix)
  endRead(n3)

  var c5 = beginRead(n4)
  var n5 = injectDestructors(c5, ctx)
  endRead(n4)

  shrink(n5, n5.len-1)
  n5.add move(ctx[].dest)
  n5.addParRi()

  result = move n5
