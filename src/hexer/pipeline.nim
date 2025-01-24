#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

include nifprelude

import basics, iterinliner, xelim, duplifier, lifter, destroyer

proc transform*(c: var EContext; n: Cursor; moduleSuffix: string): TokenBuf =
  var n = n
  elimForLoops(c, n)

  var n0 = move c.dest
  var c0 = beginRead(n0)

  let ctx = createLiftingCtx()
  var n1 = injectDups(c0, ctx)
  endRead(n0)

  var c1 = beginRead(n1)

  var n2 = lowerExprs(c1, moduleSuffix)
  endRead(n1)

  var c2 = beginRead(n2)
  result = injectDestructors(c2, ctx)
  endRead(n2)

  shrink(result, result.len-1)
  result.add move(ctx[].dest)
  result.addParRi()
