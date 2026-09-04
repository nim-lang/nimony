# std/ioring on Windows: the selected backend (the IOCP proactor by default,
# WSAPoll readiness with -d:nimIoringWsaPoll) driving a loopback TCP round trip through
# the ring's own socket surface — listenTcp, submitAccept, submitRead,
# submitWrite, submitPollAdd, closeFd — with completions drained through
# waitCompletions (no continuations, so the test needs no CPS). The client
# side is raw blocking Winsock so the only thing under test is the ring. The
# output is backend-neutral, so run it under both.
#
# On the other platforms this prints the expected output verbatim, the mirror
# image of tioringrw/tpolladd's Windows stubs, so one .output serves everywhere.
# A broken backend hangs in waitCompletions and is caught by the tester's timeout.

when not defined(windows):
  import std/syncio
  echo "accept ok=true"
  echo "read n=4 buf=ping"
  echo "write n=4 client=pong"
  echo "polladd wr=true rd=false"
  echo "abort read res=-125 op=true"
  echo "abort accept res=-125 op=true"
  echo "abort queued accept res=-125 op=true"
  echo "timer res=0 op=true"
  echo "connect res=0"
  echo "read deadline res=-110 op=true"
  echo "connect refused neg=true"
  echo "close ok"
else:
  import std / [ioring, syncio, assertions]

  type
    SocketHandle = uint
    SockAddrIn = object
      sin_family: int16
      sin_port: uint16
      sin_addr: uint32
      sin_zero: array[8, char]

  const
    AF_INET = 2.cint
    SOCK_STREAM = 1.cint
    IPPROTO_TCP = 6.cint
    InvalidSocket = not 0'u

  proc wsSocket(af, typ, protocol: cint): SocketHandle {.
    stdcall, importc: "socket", dynlib: "ws2_32.dll".}
  proc wsConnect(s: SocketHandle; name: pointer; namelen: cint): cint {.
    stdcall, importc: "connect", dynlib: "ws2_32.dll".}
  proc wsGetsockname(s: SocketHandle; name: pointer; namelen: ptr cint): cint {.
    stdcall, importc: "getsockname", dynlib: "ws2_32.dll".}
  proc wsRecv(s: SocketHandle; buf: pointer; len, flags: cint): cint {.
    stdcall, importc: "recv", dynlib: "ws2_32.dll".}
  proc wsSend(s: SocketHandle; buf: pointer; len, flags: cint): cint {.
    stdcall, importc: "send", dynlib: "ws2_32.dll".}
  proc wsClosesocket(s: SocketHandle): cint {.
    stdcall, importc: "closesocket", dynlib: "ws2_32.dll".}
  proc htons(x: uint16): uint16 {.inline.} = (x shl 8) or (x shr 8)

  var comps = default(array[8, IoCompletion])

  proc waitOne(): IoCompletion =
    ## One completion. `waitCompletions` polls this thread's lane until one
    ## lands; the budget is enforced by the tester's per-test timeout.
    let n = waitCompletions(comps)
    assert n >= 1
    result = comps[0]

  # An ephemeral-port listener; the port comes back from getsockname.
  let lfd = listenTcp(0)
  var bound = default(SockAddrIn)
  var blen = cint(sizeof(bound))
  assert wsGetsockname(SocketHandle(cast[uint32](lfd)), addr bound, addr blen) == 0
  let port = bound.sin_port   # already network order; reuse as-is for connect

  discard submitAccept(lfd, never)

  # Blocking client on the loopback.
  let client = wsSocket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
  assert client != InvalidSocket
  var target = default(SockAddrIn)
  target.sin_family = int16(AF_INET)
  target.sin_port = port
  target.sin_addr = 0x0100007F'u32   # 127.0.0.1 in network byte order
  assert wsConnect(client, addr target, cint(sizeof(target))) == 0, "connect failed"

  let acc = waitOne()
  let cfd = cint(acc.result)
  echo "accept ok=", (acc.op == opAccept and cfd > 0)
  setNonBlocking(cfd)

  var ping = "ping"
  assert wsSend(client, ping.toCString, 4.cint, 0.cint) == 4
  var rbuf = default(array[8, char])
  discard submitRead(cfd, addr rbuf[0], 8, never)
  let rd = waitOne()
  var got = ""
  for i in 0 ..< rd.result: got.add rbuf[i]
  echo "read n=", rd.result, " buf=", got

  var pong = "pong"
  discard submitWrite(cfd, pong.toCString, 4, never)
  let wr = waitOne()
  var cbuf = default(array[8, char])
  let cn = wsRecv(client, addr cbuf[0], 8.cint, 0.cint)
  var cgot = ""
  for i in 0 ..< cn: cgot.add cbuf[i]
  echo "write n=", wr.result, " client=", cgot

  # Readiness probe: the connected socket is writable and (nothing sent) not
  # readable; the mask asks for both and the completion reports what fired.
  discard submitPollAdd(cfd, never, {evRead, evWrite})
  let pa = waitOne()
  echo "polladd wr=", evWrite in pa.readyEvents, " rd=", evRead in pa.readyEvents

  # Close with ops in flight. Every backend must surface the op as an
  # `ECancelled` completion with its slot freed, in both windows:
  #  - issued: `pollCompletions` drove the lane once, so the op is a pending
  #    overlapped WSARecv/AcceptEx (IOCP: closesocket aborts it, the drain
  #    maps STATUS_CANCELLED) or an armed readiness slot (`cancelPendingOps`);
  #  - queued: closeFd ran before the lane ever issued it (IOCP: the port
  #    association fails on the dead handle; readiness: WSAPoll's POLLNVAL).
  discard submitRead(cfd, addr rbuf[0], 8, never)
  discard pollCompletions(comps)
  closeFd(cfd)
  let ab = waitOne()
  echo "abort read res=", ab.result, " op=", ab.op == opRead
  let lfd2 = listenTcp(0)
  discard submitAccept(lfd2, never)
  discard pollCompletions(comps)
  closeFd(lfd2)
  let ac = waitOne()
  echo "abort accept res=", ac.result, " op=", ac.op == opAccept
  discard submitAccept(lfd, never)
  closeFd(lfd)
  let aq = waitOne()
  echo "abort queued accept res=", aq.result, " op=", aq.op == opAccept
  # The deadline machinery, which on Windows has no other test: `ttimers`
  # stubs itself out here because it is built on AF_UNIX socket pairs.

  # A timer reaching its deadline is a success, not a timeout.
  discard submitTimeout(afterMs(20))
  let tm = waitOne()
  echo "timer res=", tm.result, " op=", tm.op == opTimeout

  # A real non-blocking connect through the ring, to a listener we own. Port 0:
  # the kernel picks, so a parallel run cannot collide with us.
  let lfd3 = listenTcp(0'u16)
  let port3 = boundPort(lfd3)
  assert port3 != 0'u16, "no port was bound"
  let cs = socketNonBlocking()
  var sa = default(Sockaddr_storage)
  var slen = SockLen(0)
  loopbackAddr(sa, slen, port3)
  discard submitConnect(cs, sa, slen, afterMs(2000))
  let cn2 = waitOne()
  assert cn2.op == opConnect
  assert cn2.result != IoTimedOut, "a connect to a live listener timed out"
  echo "connect res=", cn2.result

  # `cs` is connected but nobody ever wrote to it, so this read can only end
  # one way: the deadline. On IOCP that is the interesting path — the op is a
  # live overlapped WSARecv the kernel owns, so expiring it locally has to take
  # it back (CancelIoEx) and then drop the completion that still arrives.
  var dbuf = default(array[8, char])
  discard submitRead(cs, addr dbuf[0], 8, afterMs(50))
  let dl = waitOne()
  echo "read deadline res=", dl.result, " op=", dl.op == opRead
  closeFd(cs); closeFd(lfd3)

  # Nothing is listening: bind a port, learn its number, close it. The result
  # is only checked for its shape — a Winsock refusal is -10061 where the POSIX
  # arm reports -ECONNREFUSED, and the WSAPoll backend on a host older than
  # Windows 10 2004 does not see the failure at all and reports the deadline.
  let probe = listenTcp(0'u16)
  let deadPort = boundPort(probe)
  closeFd(probe)
  let rs = socketNonBlocking()
  var sa2 = default(Sockaddr_storage)
  var slen2 = SockLen(0)
  loopbackAddr(sa2, slen2, deadPort)
  discard submitConnect(rs, sa2, slen2, afterMs(1000))
  let rf = waitOne()
  assert rf.op == opConnect
  echo "connect refused neg=", rf.result < 0
  closeFd(rs)

  discard wsClosesocket(client)
  echo "close ok"
