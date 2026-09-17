when defined(windows):
  import std/syncio
  echo "cancelled=1 op=opPollAdd fd_is_a=true res_is_ECancelled=true"
  echo "still-open read=1 got=x"
  echo "nothing-armed cancelled=0"
  echo "two-ops cancelled=2"
else:
  import std / [ioring, assertions, syncio]
  import std/posix/posix

  const
    AF_UNIX = 1.cint
    SOCK_STREAM = 1.cint
    MSG_NOSIGNAL = 0x4000.cint

  proc socketpair(domain, typ, protocol: cint;
                  sv: ptr UncheckedArray[cint]): cint {.importc: "socketpair".}
  proc send(s: cint; buf: pointer; len: int; flags: cint): int {.importc: "send".}

  proc mkPair(): (cint, cint) =
    var fds = default(array[2, cint])
    if socketpair(AF_UNIX, SOCK_STREAM, 0,
                  cast[ptr UncheckedArray[cint]](addr fds)) != 0:
      quit "socketpair failed"
    setNonBlocking(fds[0])
    setNonBlocking(fds[1])
    result = (fds[0], fds[1])

  var comps = default(array[8, IoCompletion])

  block cancelWithoutClose:
    # `a` is not readable and nobody will make it so, so this poll would park
    # for the life of the process — which is exactly the libcurl abort path
    # that retires a slot. Take it back WITHOUT closing `a`.
    let (a, b) = mkPair()
    discard submitPollAdd(a, never, {evRead})
    let cancelled = submitPollRemove(a)
    let n = waitCompletions(comps)
    assert n == 1
    echo "cancelled=", cancelled, " op=", comps[0].op,
         " fd_is_a=", comps[0].fd == a,
         " res_is_ECancelled=", comps[0].result == ECancelled

    # THE POINT OF THE PROC: `a` is still a live descriptor. A real read on it
    # must still work — if `submitPollRemove` had closed it, or left the
    # backend registration in a broken state, this would fail or hang.
    var msg = "x"
    assert send(b, msg.toCString, 1, MSG_NOSIGNAL) == 1
    var buf = default(array[8, char])
    discard submitRead(a, addr buf[0], 8, never)
    let m = waitCompletions(comps)
    assert m == 1
    echo "still-open read=", comps[0].result, " got=", buf[0]
    closeFd(a); closeFd(b)

  block nothingArmed:
    # The common path: REMOVE arrives when nothing is in flight. Not an error.
    let (a, b) = mkPair()
    echo "nothing-armed cancelled=", submitPollRemove(a)
    closeFd(a); closeFd(b)

  block twoOps:
    # One fd can carry several in-flight ops sharing one registration; all of
    # them come back.
    let (a, b) = mkPair()
    var outb = default(array[8, char])
    discard submitPollAdd(a, never, {evRead})
    discard submitRead(a, addr outb[0], 8, never)
    echo "two-ops cancelled=", submitPollRemove(a)
    var got = 0
    while got < 2:
      got += waitCompletions(comps)
    closeFd(a); closeFd(b)
    while pollCompletions(comps) > 0: discard
