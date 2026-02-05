##[

Pass Pipeline Infrastructure for Hexer
=======================================

This module provides a unified Pass object for chaining compiler transformations.
Each pass reads from `buf` via cursor `n` and writes to `dest`, then `prepareForNext`
swaps buffers for the next stage.

Example usage:
  var pass = initPass(initialBuf, "moduleName", "desugar")

  # Each pass extracts cursor locally and writes to pass.dest
  block desugarPass:
    var n = pass.n  # Extract cursor
    var ctx = initDesugarContext(pass.moduleSuffix)
    tr(ctx, pass.dest, n)  # Transform: read from n, write to pass.dest

  pass.prepareForNext("cps")

  block cpsPass:
    var n = pass.n
    var ctx = initCpsContext(pass.moduleSuffix, pass.nextTemp)
    transformToCps(ctx, pass.dest, n)
    pass.nextTemp = ctx.tempCounter  # Update shared counter if needed

  pass.prepareForNext("lambdalift")
  # ... continue pipeline

  let finalResult = pass.extractResult()

]##

import ../lib/[nifcursors, nifstreams]

type
  Pass* = object
    n*: Cursor         ## Current read position in buf
    buf*: TokenBuf     ## Input buffer for current pass
    dest*: TokenBuf    ## Output buffer being written to
    moduleSuffix*: string  ## Module suffix for symbol generation
    bits*: int         ## Flags/bitset for pass configuration
    nextTemp*: int     ## Counter for temporary variable generation
    passName*: string  ## Current pass name (for debugging/logging)

# Configuration flags for Pass.bits
const
  PassLogOutput* = 1 shl 0      ## Log pass output when prepareForNext is called
  PassValidateOutput* = 1 shl 1 ## Validate output TokenBuf structure
  PassTrackMemory* = 1 shl 2    ## Track memory allocation stats

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

proc resetTempCounter*(pass: var Pass) {.inline.} =
  ## Reset the temporary variable counter.
  ## Useful when starting a fresh scope or function.
  pass.nextTemp = 0

proc genTemp*(pass: var Pass; prefix: string = "tmp"): string =
  ## Generate a unique temporary variable name.
  ## Increments nextTemp counter.
  result = prefix & $pass.nextTemp & pass.moduleSuffix
  inc pass.nextTemp
