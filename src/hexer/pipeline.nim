#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

include nifprelude

import basics, iterinliner, xelim, duplifier, lifter

proc transform*(c: var EContext; n: Cursor; moduleSuffix: string): TokenBuf =
  var n = n
  elimForLoops(c, n)

  var n0 = move c.dest
  var c0 = beginRead(n0)

  let ctx = createLiftingCtx()
  var n1 = injectDups(c0, ctx)
  endRead(n0)

  shrink(n1, n1.len-1)
  n1.add ctx[].dest
  n1.addParRi()

  var c = beginRead(n1)

  result = lowerExprs(c, moduleSuffix)
  endRead(n1)
