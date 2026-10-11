## Running child processes side by side while keeping their output readable:
## each child's stdout+stderr is drained by a thread of its own and printed in
## one piece when the child exits, so concurrent children never interleave.
## The test pool (`parallel.nim`) and the toolchain build (`builders.nim`)
## both run on this.

import std / [syncio, osproc, typedthreads, locks]
when defined(windows):
  import std/winlean
else:
  import std/posix

type
  ReaderArg* = object
    ## Pure value-type passed to a reader thread: just an OS handle and a
    ## pointer to a shared `Lock`. No `ref`, no string transfer across
    ## threads. The worker accumulates output in a thread-local string,
    ## allocated and freed in the same thread it lives in, and prints
    ## under the lock so concurrent slots don't interleave their
    ## per-test output. Earlier designs that handed the string back to
    ## the main thread (via ref, channel, or ptr-string) all hit
    ## `addToSharedFreeListBigChunks` SIGSEGVs in the runtime when ORC
    ## tried to free a worker-allocated big chunk on the main thread —
    ## keeping every alloc and dealloc thread-local sidesteps that.
    handle*: int      # cast of the child's stdout `FileHandle` to int.
    lockPtr*: pointer # ptr Lock guarding stdout.

proc drainStdout*(arg: ReaderArg) {.thread, nimcall.} =
  ## Background reader: pulls bytes off the child's pipe as they arrive so
  ## the child never blocks on a full pipe buffer. The previous one-shot
  ## drain (only after `peekExitCode` reported the child gone) deadlocked
  ## on Windows: clang on the generated C emits enough `-W…-cast`
  ## warnings during a normal compile to fill the ~4KB pipe buffer, the
  ## child then blocks on its next write, the parent's `peekExitCode`
  ## never advances past -1, and the whole `--jobs:auto` run hangs
  ## producing zero output. Streaming as we go fixes that.
  ##
  ## At EOF we flush the accumulated buffer to stdout under
  ## `lockPtr[]` so the per-test block stays atomic relative to other
  ## slots' reads.
  var buf = newStringOfCap(1 shl 12)
  var tmp = newString(4096)
  while true:
    var n: int = 0
    when defined(windows):
      var bytesRead: int32 = 0
      let ok = winlean.readFile(cast[Handle](arg.handle), tmp[0].addr,
                                tmp.len.int32, addr bytesRead, nil)
      # `readFile` returns 0 on error; ERROR_BROKEN_PIPE is the normal EOF
      # when the child closes its stdout, and it's also signaled by
      # `bytesRead == 0` with success. Treat both as EOF.
      if ok == 0'i32 or bytesRead == 0'i32: break
      n = bytesRead.int
    else:
      n = posix.read(arg.handle.cint, tmp[0].addr, tmp.len)
      if n <= 0: break
    let prevLen = buf.len
    buf.setLen(prevLen + n)
    copyMem(addr buf[prevLen], addr tmp[0], n)
  let lock = cast[ptr Lock](arg.lockPtr)
  acquire lock[]
  try:
    stdout.write buf
    stdout.flushFile()
  finally:
    release lock[]

proc runConcurrently*(cmds: openArray[string]): seq[int] =
  ## Run every command of `cmds` at once and wait for all of them; returns
  ## their exit codes in `cmds` order. A command line is handed to the OS as
  ## is (`poEvalCommand`), so it must already be quoted. Each child's output
  ## is printed as one block when it exits.
  result = newSeq[int](cmds.len)
  var lock = default(Lock)
  initLock(lock)
  var procs = newSeq[Process](cmds.len)
  var readers = newSeq[Thread[ReaderArg]](cmds.len)
  for i, cmd in pairs(cmds):
    procs[i] = startProcess(cmd, options = {poEvalCommand, poStdErrToStdOut})
    createThread(readers[i], drainStdout,
                 ReaderArg(handle: procs[i].outputHandle.int,
                           lockPtr: cast[pointer](addr lock)))
  for i in 0 ..< procs.len:
    # Join the reader first: it only returns at EOF, i.e. once the child
    # closed its end of the pipe, and waiting on the exit code before that
    # could leave a child blocked on a full pipe nobody reads.
    joinThread(readers[i])
    result[i] = waitForExit(procs[i])
    close procs[i]
  deinitLock(lock)
