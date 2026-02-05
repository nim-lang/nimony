#
#
#           Hexer Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

import ../lib/[nifcursors, nifstreams]

type
  Pass* = object
    n*: Cursor         ## Current read position in buf
    buf*: TokenBuf     ## Input buffer for current pass
    dest*: TokenBuf    ## Output buffer being written to
    moduleSuffix*: string  ## Module suffix for symbol generation
    bits*: int         ## number of bits in the target architecture
    nextTemp*: int     ## Counter for temporary variable generation
    passName*: string  ## Current pass name (for debugging/logging)

proc initPass*(initialBuf: sink TokenBuf; moduleSuffix: string;
               firstPassName: string; bits: int): Pass =
  ## Initialize a new Pass pipeline with the given input buffer.
  ## The buffer is moved into the Pass and a cursor is created.
  result = Pass(buf: initialBuf, moduleSuffix: moduleSuffix, bits: bits, nextTemp: 0, passName: firstPassName)
  result.n = beginRead(result.buf)
  result.dest = createTokenBuf(300)

proc prepareForNext*(pass: var Pass; nextPassName: string) =
  ## Transition to the next pass in the pipeline.
  when defined(logPasses):
    echo pass.passName, " produced:"
    echo "  ", toString(pass.dest, false)

  # End reading from old buffer
  endRead(pass.buf)

  # Swap: previous output becomes next input
  swap(pass.buf, pass.dest)
  pass.dest.shrink 0
  pass.n = beginRead(pass.buf)

  pass.passName = nextPassName
