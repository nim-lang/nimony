#
#
#           Hexer Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

when not defined(nimony):
  import std/[monotimes, times, syncio, os, strutils]
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
    when not defined(nimony):
      passStart*: MonoTime  ## start time of current pass (only set when timing on)

when not defined(nimony):
  # Per-pass timing log: append one line per (module, pass) when
  # `NIMONY_PASS_TIMING=<file>` is set in the environment. Multiple hexer
  # processes can append to the same file in parallel — lines are short and
  # `O_APPEND` makes single `write(2)` calls atomic on POSIX, so the log
  # stays well-formed without locking. Each line is `<module>\t<pass>\t<sec>`.
  var
    passTimingInited: bool
    passTimingLog: File
    passTimingEnabled: bool

  proc ensurePassTimingInit() =
    if passTimingInited: return
    passTimingInited = true
    let env = getEnv("NIMONY_PASS_TIMING")
    if env.len == 0: return
    try:
      passTimingLog = open(env, fmAppend)
      passTimingEnabled = true
    except IOError:
      discard

  proc logPassTiming(moduleSuffix, passName: string; start: MonoTime) =
    if not passTimingEnabled: return
    let sec = float(inMicroseconds(getMonoTime() - start)) / 1_000_000.0
    # Pre-build the line so a single write(2) covers it — keeps parallel
    # appenders from interleaving fragments.
    let line = moduleSuffix & '\t' & passName & '\t' &
               sec.formatFloat(ffDecimal, 6) & '\n'
    passTimingLog.write line
    passTimingLog.flushFile()

proc initPass*(initialBuf: sink TokenBuf; moduleSuffix: string;
               firstPassName: string; bits: int): Pass =
  ## Initialize a new Pass pipeline with the given input buffer.
  ## The buffer is moved into the Pass and a cursor is created.
  when not defined(nimony):
    ensurePassTimingInit()
  result = Pass(buf: initialBuf, moduleSuffix: moduleSuffix, bits: bits, nextTemp: 0, passName: firstPassName)
  result.n = beginRead(result.buf)
  result.dest = createTokenBuf(300)
  when not defined(nimony):
    if passTimingEnabled:
      result.passStart = getMonoTime()

proc prepareForNext*(pass: var Pass; nextPassName: string) =
  ## Transition to the next pass in the pipeline.
  when defined(logPasses):
    echo pass.passName, " produced:"
    echo "  ", toString(pass.dest, false)

  when not defined(nimony):
    if passTimingEnabled:
      logPassTiming(pass.moduleSuffix, pass.passName, pass.passStart)

  # End reading from old buffer
  endRead(pass.buf)

  # Swap: previous output becomes next input
  swap(pass.buf, pass.dest)
  pass.dest.shrink 0
  pass.n = beginRead(pass.buf)

  pass.passName = nextPassName
  when not defined(nimony):
    if passTimingEnabled:
      pass.passStart = getMonoTime()

proc finishPass*(pass: var Pass) =
  ## Log the final pass's elapsed time. Call once after the last pass body
  ## has finished; no-op when timing is disabled.
  when defined(logPasses):
    echo pass.passName, " produced:"
    echo "  ", toString(pass.dest, false)

  when not defined(nimony):
    if passTimingEnabled:
      logPassTiming(pass.moduleSuffix, pass.passName, pass.passStart)
