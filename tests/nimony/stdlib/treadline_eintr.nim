# nim-lang/nimony#2912: a read that a signal handler (installed without
# `SA_RESTART`) interrupts fails with EINTR. That is neither an error nor the
# end of the file, so `readLine` retries it and reads the rest of the input.
#
# C backend only: the handler is installed through libc `sigaction`, which the
# freestanding native backend does not bind.

when defined(linux) or defined(osx):
  import std/[assertions, syncio]
  import std/posix/posix

  type Sigaction {.importc: "struct sigaction", header: "<signal.h>".} = object
    sa_handler: nil proc (sig: cint) {.cdecl.}
    sa_flags: cint

  proc sigaction(sig: cint; act, old: nil ptr Sigaction): cint {.
    importc, header: "<signal.h>".}
  proc ualarm(usecs, interval: cuint): cuint {.importc, header: "<unistd.h>".}

  var hits = 0   ## the handler ran, so the read really was interrupted
  proc onAlarm(sig: cint) {.cdecl.} =
    {.cast(assumeSync).}:
      inc hits

  proc pause(ms: int) =
    var req = Timespec(tv_sec: Time(0), tv_nsec: clong(ms * 1_000_000))
    var rem = default(Timespec)
    discard nanosleep(req, rem)

  proc send(fd: cint; s: string) =
    var s = s
    discard write(fd, readRawData(s), s.len)

  proc main() =
    var sa = default(Sigaction)
    sa.sa_handler = onAlarm   # sa_flags = 0: no SA_RESTART
    assert sigaction(14, addr sa, nil) == 0'i32   # SIGALRM

    var fds = [cint(-1), cint(-1)]
    assert pipe(addr fds[0]) == 0'i32
    let pid = fork()
    if pid == Pid(0):
      discard close(fds[0])
      send(fds[1], "a\n")
      pause 400     # the parent is blocked in `read` when the alarm fires
      send(fds[1], "b\n")
      exitnow(0)
    discard close(fds[1])

    var f: File = nil
    assert open(f, fds[0], fmRead)
    var s = ""
    assert readLine(f, s)
    assert s == "a"
    discard ualarm(100_000'u32, 0'u32)
    assert readLine(f, s)
    assert s == "b"
    assert not failed(f)
    {.cast(assumeSync).}:
      assert hits == 1
    assert not readLine(f, s)
    assert not failed(f)
    close(f)
    var status: cint = 0
    discard waitpid(pid, status, 0)

  main()
