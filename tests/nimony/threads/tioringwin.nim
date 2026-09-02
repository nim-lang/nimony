# std/ioring on Windows: the WSAPoll readiness backend driving a loopback TCP
# round trip through the ring's own socket surface — listenTcp, submitAccept,
# submitRead, submitWrite, submitPollAdd, closeFd — with completions drained
# through waitCompletions (no continuations, so the test needs no CPS). The
# client side is raw blocking Winsock so the only thing under test is the ring.
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

  discard submitAccept(lfd)

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
  discard submitRead(cfd, addr rbuf[0], 8)
  let rd = waitOne()
  var got = ""
  for i in 0 ..< rd.result: got.add rbuf[i]
  echo "read n=", rd.result, " buf=", got

  var pong = "pong"
  discard submitWrite(cfd, pong.toCString, 4)
  let wr = waitOne()
  var cbuf = default(array[8, char])
  let cn = wsRecv(client, addr cbuf[0], 8.cint, 0.cint)
  var cgot = ""
  for i in 0 ..< cn: cgot.add cbuf[i]
  echo "write n=", wr.result, " client=", cgot

  # Readiness probe: the connected socket is writable and (nothing sent) not
  # readable; the mask asks for both and the completion reports what fired.
  discard submitPollAdd(cfd, {evRead, evWrite})
  let pa = waitOne()
  echo "polladd wr=", evWrite in pa.readyEvents, " rd=", evRead in pa.readyEvents

  closeFd(cfd)
  closeFd(lfd)
  discard wsClosesocket(client)
  echo "close ok"
