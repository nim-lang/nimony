## Process-termination primitives shared by `memory` (out-of-memory), `panics`
## (range/conversion/dispatch errors) and `syncio` (`quit`).
##
## glibc's `exit`/`abort` flush stdio through a registered cleanup function;
## we mirror that with a single flush hook that `syncio` installs at module
## init (it owns the buffered standard streams). Every normal *and* abnormal
## exit funnels through `cExit`/`cAbort`, so the flush happens in exactly one
## place regardless of who terminates the process.
##
## Under `-d:nimNativeIo` the primitives are raw syscalls (`kill`/`getpid`/
## `_exit`) — all real libc symbols *and* real Linux syscalls, so they link on
## today's C backend and lower to bare `syscall` instructions on arkham. The
## default build defers to libc `exit`/`abort` (which flush stdio themselves).

proc nimNoopFlush() {.nimcall.} = discard

var gExitFlush: proc () {.nimcall.} = nimNoopFlush
  ## Flushes the buffered standard streams on process exit. A no-op by default
  ## (the libc build's stdio flushes itself); `syncio` installs the real one
  ## via `setExitFlush` when `-d:nimNativeIo` is in effect.

proc setExitFlush*(p: proc () {.nimcall.}) {.inline.} =
  ## Registers the std-stream flush callback (called by `syncio`).
  gExitFlush = p

proc nimFlushStdStreams*() {.exportc: "nimFlushStdStreams".} =
  ## Flushes the buffered standard streams. The generated C `main` epilogue
  ## calls this before returning (see `hexer/nifcgen.genMainProc`), so normal
  ## program termination flushes too — not just `quit`/`abort`. Always present
  ## (system is always imported) and a no-op unless `syncio` installed a flush.
  gExitFlush()

when defined(nimNativeIo):
  when defined(windows):
    # Freestanding Windows: terminate through kernel32 `ExitProcess` — no libc
    # and no POSIX signals, the direct counterpart of the Linux syscall path.
    # Declared bare (no `header: "<windows.h>"`): nimony emits the prototype and
    # the linker binds `ExitProcess` from kernel32, keeping the whole `system`
    # translation unit free of the Windows header (see `panics`).
    proc cExitProcess(uExitCode: uint32) {.stdcall, importc: "ExitProcess",
                                           noreturn.}

    proc cExit*(code: int) {.noreturn.} =
      ## Normal process termination: flush the standard streams, then exit.
      gExitFlush()
      cExitProcess(uint32(code))

    proc cAbort*() {.noreturn.} =
      ## Abnormal termination. Windows has no `SIGABRT`, so we flush the
      ## standard streams and exit with a non-zero status — the closest
      ## libc-free analog of glibc `abort()` (minus the signal/core-dump dance).
      gExitFlush()
      cExitProcess(127'u32)
  else:
    const SIGABRT = 6'i32   ## Linux signal number, stable across x86_64/arm64.
    proc cKill(pid, sig: cint): cint {.importc: "kill".}
    proc cGetpid(): cint {.importc: "getpid".}
    proc cExitSys(code: cint) {.importc: "_exit", noreturn.}

    proc cExit*(code: int) {.noreturn.} =
      ## Normal process termination: flush the standard streams, then `_exit`.
      gExitFlush()
      cExitSys(code.int32)

    proc cAbort*() {.noreturn.} =
      ## Abnormal termination, modelled on glibc `abort()`: flush the standard
      ## streams, raise `SIGABRT` (default disposition → core dump / debugger
      ## trap), and `_exit(127)` as a last resort if the signal was ignored.
      ## We omit glibc's unblock / reset-handler / re-raise dance because this
      ## freestanding runtime installs no `SIGABRT` handler and never blocks it.
      gExitFlush()
      discard cKill(cGetpid(), SIGABRT)
      cExitSys(127'i32)
else:
  proc cExitLibc(code: cint) {.importc: "exit", header: "<stdlib.h>", noreturn.}
  proc cAbortLibc() {.importc: "abort", header: "<stdlib.h>", noreturn.}

  proc cExit*(code: int) {.noreturn.} =
    ## Normal process termination: flush the standard streams, then `exit`.
    gExitFlush()
    cExitLibc(code.int32)

  proc cAbort*() {.noreturn.} =
    ## Abnormal termination via libc `abort()` (flushes stdio, raises SIGABRT).
    gExitFlush()
    cAbortLibc()
