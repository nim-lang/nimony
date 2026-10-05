## Test: a write on a non-blocking socket parks until it can make progress.
## When the send buffer is full and the peer has half-closed (FIN), io_uring's
## internal retry of an IORING_OP_WRITE found no room and completed the op
## with -EAGAIN. "Not now" is the ring's business: a write op completes with
## progress, a real error, its deadline, or a cancel; never with EAGAIN.

{.feature: "assumeSync".}  # test program: globals shared freely

const expected = [
  "fin-before parked=true progressed=true",
  "fin-while-parked parked=true still-parked=true progressed=true",
  "fin-before deadline res=-110",
  "fin-before closed res=-125"]

when defined(windows) or not defined(linux):
  import std/syncio
  for line in expected: echo line
else:
  import std / [ioring, assertions, syncio]
  import std/posix/posix

  proc cAccept(fd: cint; sa: nil pointer; len: nil pointer): cint {.
    importc: "accept", header: "<sys/socket.h>".}
  proc cConnect(fd: cint; sa: pointer; len: SockLen): cint {.
    importc: "connect", header: "<sys/socket.h>".}
  proc cShutdown(fd: cint; how: cint): cint {.
    importc: "shutdown", header: "<sys/socket.h>".}
  proc cSocket(domain, typ, protocol: cint): cint {.
    importc: "socket", header: "<sys/socket.h>".}
  proc recvfrom(s: cint; buf: pointer; len: int; flags: cint;
                src: nil pointer; srcLen: nil pointer): int {.importc: "recvfrom".}

  const
    ShutWr = 1.cint
    Chunk = 1024 * 1024
    ParkedMs = 300

  var comps = default(array[8, IoCompletion])
  var chunk = default(array[Chunk, char])
  var sink = default(array[65536, char])

  proc waitFor(ms: int; res: var int): bool =
    ## Whether a completion landed within `ms`; its result goes to `res`.
    let until = afterMs(ms)
    result = false
    while not result and monoNow() < until:
      if pollCompletions(comps) > 0:
        res = comps[0].result
        result = true

  proc drainPeer(peer: cint; res: var int): bool =
    ## Read from the peer until the parked write completes.
    let until = afterMs(5000)
    result = false
    while not result and monoNow() < until:
      discard recvfrom(peer, addr sink[0], sink.len, 0, nil, nil)
      if pollCompletions(comps) > 0:
        res = comps[0].result
        result = true

  proc tcpPair(): (cint, cint) =
    ## (server side, non-blocking; peer side, non-blocking).
    let lfd = listenTcp(0'u16)
    var sa = default(Sockaddr_storage)
    var sl = default(SockLen)
    loopbackAddr(sa, sl, boundPort(lfd))
    let peer = cSocket(2.cint, 1.cint, 6.cint)
    assert cConnect(peer, addr sa, sl) == 0
    let srv = cAccept(lfd, nil, nil)
    assert srv >= 0
    setNonBlocking(srv)
    setNonBlocking(peer)
    closeFd(lfd)
    result = (srv, peer)

  proc fillUntilParked(srv: cint; deadlineMs: int; res: var int): bool =
    ## Submit 1 MiB writes one at a time until one does not complete within
    ## `ParkedMs`: that one is left in flight. True if it parked, false if a
    ## write completed with a non-positive result (which is left in `res`).
    var i = 0
    while i < 64:
      inc i
      discard submitWrite(srv, addr chunk[0], Chunk,
                          if deadlineMs > 0: afterMs(deadlineMs) else: never)
      if not waitFor(ParkedMs + deadlineMs, res): return true
      if res <= 0: return false
    result = false

  block finBefore:
    let (srv, peer) = tcpPair()
    var res = 0
    discard cShutdown(peer, ShutWr)
    let parked = fillUntilParked(srv, 0, res)
    var progressed = false
    if parked: progressed = drainPeer(peer, res) and res > 0
    else: echo "fin-before: write completed res=", res
    echo "fin-before parked=", parked, " progressed=", progressed
    closeFd(srv); closeFd(peer)

  block finWhileParked:
    let (srv, peer) = tcpPair()
    var res = 0
    let parked = fillUntilParked(srv, 0, res)
    discard cShutdown(peer, ShutWr)
    let stillParked = not waitFor(ParkedMs, res)
    if not stillParked: echo "fin-while-parked: write completed res=", res
    var progressed = false
    if stillParked: progressed = drainPeer(peer, res) and res > 0
    echo "fin-while-parked parked=", parked, " still-parked=", stillParked,
         " progressed=", progressed
    closeFd(srv); closeFd(peer)

  block deadline:
    # The re-armed write keeps its deadline.
    let (srv, peer) = tcpPair()
    var res = 0
    discard cShutdown(peer, ShutWr)
    discard fillUntilParked(srv, 400, res)
    echo "fin-before deadline res=", res
    closeFd(srv); closeFd(peer)

  block closed:
    # closeFd takes a parked write back.
    let (srv, peer) = tcpPair()
    var res = 0
    discard cShutdown(peer, ShutWr)
    assert fillUntilParked(srv, 0, res)
    closeFd(srv)
    assert waitFor(2000, res)
    echo "fin-before closed res=", res
    closeFd(peer)
