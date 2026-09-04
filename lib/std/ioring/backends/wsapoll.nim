# Windows WSAPoll readiness backend.
#
# Winsock's poll(2) drives the same readiness machinery the epoll and kqueue
# backends use (`processFd`/`armEventsForFd` in ./poll). Each lane owns its
# poller state, and the poll set is rebuilt on every call from the lane's slot
# arena: one entry per fd with in-flight ops, armed for the union of their
# directions. There is no kernel-side registration to keep in sync, so
# `reArmEvent` has nothing to do and `forgetFd` is nil — which also means
# epoll's ADD/MOD race has no counterpart here.
#
# Scale: the set is O(pending fds) per poll and WSAPoll is O(n) per call —
# right for a client and for a single-user local server, and the fallback
# selected with `-d:nimIoringWsaPoll`; the Windows default is the IOCP
# proactor (backends/iocp.nim), which also avoids this backend's one
# scheduler-tick stall on a fresh connection's first request (the listener's
# readiness is a tick late; measured in the IOCP header).
#
# Winsock is bound by `dynlib` (the winlean house style) rather than through
# `<winsock2.h>`, so the generated C never has to order that header against
# winlean's `<Windows.h>`. `WSAPOLLFD` is declared to its ABI (SOCKET + two
# SHORTs, natural alignment). Known caveat: before Windows 10 2004, WSAPoll
# does not report a failed connect() (no POLLERR is raised).

when defined(windows):
  import std/threadpool
  import std/windows/winlean   # sleep

  import ../core/types
  import ../core/slots
  import ../core/backend
  import ./poll

  const
    DrainBatch = 128

  type
    SocketHandle = uint   ## Winsock SOCKET (UINT_PTR); the ring's fd is its cint narrowing
    WsaPollFd = object    ## WSAPOLLFD
      fd: SocketHandle
      events: cshort
      revents: cshort

  const
    POLLRDNORM = 0x0100   ## normal data readable
    POLLWRNORM = 0x0010   ## normal data writable
    POLLERR = 0x0001      ## error condition (revents only)
    POLLHUP = 0x0002      ## hang-up (revents only)
    POLLNVAL = 0x0004     ## invalid socket (revents only)
    PollFailMask = POLLERR or POLLHUP or POLLNVAL

  proc wsaPoll(fdArray: ptr WsaPollFd; nfds: culong; timeout: cint): cint {.
    stdcall, importc: "WSAPoll", dynlib: "ws2_32.dll".}

  var
    pollSets: seq[seq[WsaPollFd]]   ## per lane; reused across polls

  proc wsapollReArm(fd: cint; events: IoEvents; alreadyRegistered: bool): bool {.nimcall.} =
    ## Nothing to register: the poll set is derived from the arena on every poll,
    ## so arming cannot fail here — a dead socket surfaces as POLLNVAL instead.
    result = true

  proc wsapollPoll(timeoutMs: int): bool {.nimcall.} =
    let lane = ioLane()
    var buf {.noinit.}: array[DrainBatch, OpContext]
    var n = gOpQueues[lane].tryBulkDequeue(DrainBatch, buf)
    if n > 0:
      for i in 0..<n:
        let idx = gSlots[lane].allocSlot(buf[i])
        armDeadline(lane, idx)
        case buf[i].kind
        of opTimeout:
          discard            # nothing to arm on: the deadline heap is the wait
        of opNop:
          complete(idx, 0)   # nothing to wait for either
        of opConnect:
          # Start the attempt here, on the polling thread, so the socket is
          # already connecting by the time the set below watches it. A connect
          # that finished at once has completed the slot; the set is rebuilt
          # from the arena, so there is nothing to undo either way.
          discard startConnect(buf[i].fd, idx)
        else:
          discard
    # Build this lane's set from its arena. Collect before dispatching:
    # `processFd` frees slots, which mutates the fd index the set comes from.
    pollSets[lane].setLen(0)
    for fd in gSlots[lane].pendingFds:
      let events = armEventsForFd(fd)
      if events == {}: continue   # nothing to watch: the fd-less bucket (timers)
      var ev = 0
      if evRead in events: ev = ev or POLLRDNORM
      if evWrite in events: ev = ev or POLLWRNORM
      pollSets[lane].add WsaPollFd(fd: SocketHandle(cast[uint32](fd)),
                                   events: cshort(ev), revents: cshort(0))
    # Sleep no longer than the earliest deadline in this lane, so a timer
    # fires on time instead of on the next poll that happens for another
    # reason.
    let waitMs = waitMillis(lane, timeoutMs)
    if pollSets[lane].len == 0:
      # Nothing armed on this lane: honour the idle timeout here, or the worker
      # loop spins (it only sleeps when nothing fired AND it ran no task). A
      # lane holding nothing but timers takes this path on every poll, so the
      # sleep is the deadline-bounded one and the expiry below is what
      # completes them.
      if waitMs > 0: sleep(uint32(waitMs))
      expireDeadlines(lane)
      return false
    discard wsaPoll(addr pollSets[lane][0], culong(pollSets[lane].len),
                    cint(waitMs))
    # The return value is not the gate: WSAPoll marks a closed handle POLLNVAL
    # in revents but does not count it — it returns 0 with live sockets in the
    # set and SOCKET_ERROR/WSAENOTSOCK with only dead ones (measured on a
    # Windows 10 builder). Gating on `ready > 0` therefore parked every op on
    # a socket closed before its op was issued for good. Scan revents always.
    result = false
    for i in 0..<pollSets[lane].len:
      let re = int(pollSets[lane][i].revents)
      if re == 0: continue
      result = true
      if (re and POLLNVAL) != 0:
        # Closed under its ops (closeFd ran before this lane issued them):
        # the ring's cancellation result, same as the other backends report.
        let dead = cint(cast[uint32](pollSets[lane][i].fd))
        for idx in gSlots[lane].slotsForFd(dead):
          complete(idx, ECancelled)
        continue
      var fired: IoEvents = {}
      if (re and POLLRDNORM) != 0: fired.incl evRead
      if (re and POLLWRNORM) != 0: fired.incl evWrite
      if (re and PollFailMask) != 0:
        # Error, hang-up or a dead socket: wake every pending op so its transfer
        # runs and reports the failure (recv returns 0 or -1, send fails) instead
        # of leaving the continuation parked on a socket that will never fire.
        fired = {evRead, evWrite}
      processFd(cint(cast[uint32](pollSets[lane][i].fd)), fired)
    expireDeadlines(lane)

  proc wsapollClose() {.nimcall.} =
    ## No poller descriptors to release. Winsock itself is left initialised:
    ## sockets the process still owns keep working until exit tears it down.
    discard

  proc wsapollForgetFd(fd: cint) {.nimcall.} =
    ## No registration to drop: the next poll simply no longer sees the fd.
    discard

  proc initWsaPollBackendRelays*(): BackendRelays =
    pollSets = newSeq[seq[WsaPollFd]](ioLanes())
    reArmEvent = wsapollReArm
    result = BackendRelays(
      poll: wsapollPoll,
      waits: true,          # `WSAPoll(waitMs)` is a real wait, and so is the
                            # `sleep` on the nothing-armed path above
      close: wsapollClose,
      forgetFd: wsapollForgetFd,
    )
