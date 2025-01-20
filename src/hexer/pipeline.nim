#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

include nifprelude

import basics, iterinliner, xelim

proc transform*(c: var EContext; n: Cursor; moduleSuffix: string): TokenBuf =
  var n = n
  elimForLoops(c, n)

  when false:
    var dest = move c.dest
    var c = beginRead(dest)

    result = lowerExprs(n, moduleSuffix)
  else:
    result = move c.dest
