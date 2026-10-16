# The readiness backend (epoll here, forced with -d:nimIoringNoUring by this
# directory's nimony.args) never completes a read, write or accept with
# -EAGAIN. The descriptors are O_NONBLOCK, as the backend requires; a wake that
# finds nothing to transfer leaves the op pending and re-armed.
#
# "Spurious" wakes are provoked deterministically: two ops of the same kind
# wait on one fd and ONE event arrives. The loop in `processFd` serves the
# first; the second gets EAGAIN, must stay pending, and completes when the
# next event arrives.
#
# Linux only (raw socket constants); elsewhere the expected output is printed.

when not defined(linux):
  import std/syncio
  echo "readiness backend: true"
  echo "two reads, one byte: completions=1 res=1"
  echo "two reads, second byte: completions=1 res=1"
  echo "two writes, drained: completions=1 partial=true"
  echo "two writes, drained again: completions=1 partial=true"
  echo "two accepts, one connection: completions=1 ok=true"
  echo "two accepts, second connection: completions=1 ok=true"
  echo "pipe read: n=2 buf=hi"
  echo "pipe write: n=3 buf=abc"
else:
  import std / [ioring, syncio, assertions]
  import std/ioring/platform
  import std/posix/posix

  const
    AF_INET = 2.cint
    AF_UNIX = 1.cint
    SOCK_STREAM = 1.cint
    MSG_DONTWAIT = 0x40.cint
    MSG_NOSIGNAL = 0x4000.cint

  proc socketpair(domain, typ, protocol: cint;
                  sv: ptr UncheckedArray[cint]): cint {.importc: "socketpair".}
  proc rawSocket(domain, typ, protocol: cint): cint {.importc: "socket".}
  proc rawConnect(s: cint; name: pointer; namelen: cint): cint {.importc: "connect".}
  proc sendto(s: cint; buf: pointer; len: int; flags: cint;
              dest: nil pointer; destLen: cint): int {.importc: "sendto".}
  proc send(s: cint; buf: pointer; len: int; flags: cint): int {.inline.} =
    sendto(s, buf, len, flags, nil, 0)
  proc recvfrom(s: cint; buf: pointer; len: int; flags: cint;
                src: nil pointer; srcLen: nil pointer): int {.importc: "recvfrom".}
  proc recv(s: cint; buf: pointer; len: int; flags: cint): int {.inline.} =
    recvfrom(s, buf, len, flags, nil, nil)

  proc mkPair(): (cint, cint) =
    var fds = default(array[2, cint])
    if socketpair(AF_UNIX, SOCK_STREAM, 0,
                  cast[ptr UncheckedArray[cint]](addr fds)) != 0:
      quit "socketpair failed"
    setNonBlocking(fds[0])
    setNonBlocking(fds[1])
    result = (fds[0], fds[1])

  proc connectLocal(port: uint16): cint =
    ## A client connected to 127.0.0.1:port.
    result = rawSocket(AF_INET, SOCK_STREAM, 0)
    var sa = default(array[16, uint8])
    sa[0] = 2'u8                       # sin_family = AF_INET (little endian)
    sa[2] = uint8(port shr 8)
    sa[3] = uint8(port and 0xFF)
    sa[4] = 127'u8
    sa[7] = 1'u8
    if rawConnect(result, addr sa, 16) != 0: quit "connect failed"

  var comps = default(array[16, IoCompletion])
  var chunk = default(array[65536, char])

  proc fill(fd: cint) =
    ## Fill `fd`'s send buffer until it would block.
    while send(fd, addr chunk[0], chunk.len, MSG_DONTWAIT or MSG_NOSIGNAL) > 0: discard

  proc drain(fd: cint): int =
    ## Read everything the peer has buffered. A second sweep catches what the
    ## kernel moved forward as the first one opened the window.
    result = 0
    for sweep in 0 ..< 2:
      while true:
        let n = recv(fd, addr chunk[0], chunk.len, MSG_DONTWAIT)
        if n <= 0: break
        result += n

  proc settle(ms: int): seq[IoCompletion] =
    ## Everything that completes in the next `ms` milliseconds.
    result = @[]
    discard submitTimeout(afterMs(ms))
    var done = false
    while not done:
      let n = waitCompletions(comps)
      for i in 0 ..< n:
        if comps[i].op == opTimeout: done = true
        else: result.add comps[i]

  echo "readiness backend: ", not hasIouring

  block twoReads:
    let (a, b) = mkPair()
    var r1 = default(array[8, char])
    var r2 = default(array[8, char])
    discard submitRead(a, addr r1[0], 8, never)
    discard submitRead(a, addr r2[0], 8, never)
    discard settle(50)
    var x = "x"
    assert send(b, x.toCString, 1, MSG_NOSIGNAL) == 1
    var got = settle(150)
    echo "two reads, one byte: completions=", got.len,
         " res=", (if got.len > 0: got[0].result else: 0)
    var y = "y"
    assert send(b, y.toCString, 1, MSG_NOSIGNAL) == 1
    got = settle(150)
    echo "two reads, second byte: completions=", got.len,
         " res=", (if got.len > 0: got[0].result else: 0)
    closeFd(a); closeFd(b)
    while pollCompletions(comps) > 0: discard

  block twoWrites:
    let (a, b) = mkPair()
    fill(a)
    var big1 = newString(1024 * 1024)
    var big2 = newString(1024 * 1024)
    discard submitWrite(a, big1.toCString, big1.len, never)
    discard submitWrite(a, big2.toCString, big2.len, never)
    discard settle(50)
    discard drain(b)
    var got = settle(150)
    echo "two writes, drained: completions=", got.len,
         " partial=", got.len > 0 and got[0].result > 0
    discard drain(b)
    got = settle(150)
    echo "two writes, drained again: completions=", got.len,
         " partial=", got.len > 0 and got[0].result > 0
    closeFd(a); closeFd(b)
    while pollCompletions(comps) > 0: discard

  block twoAccepts:
    let label = "two accepts"
    let lfd = listenTcp(0)
    let port = boundPort(lfd)
    discard submitAccept(lfd, never)
    discard submitAccept(lfd, never)
    discard settle(50)
    let c1 = connectLocal(port)
    var got = settle(150)
    var ok = got.len > 0 and got[0].result >= 0
    echo label, ", one connection: completions=", got.len, " ok=", ok
    for g in got:
      if g.result >= 0: closeFd(cint(g.result))
    let c2 = connectLocal(port)
    got = settle(150)
    ok = got.len > 0 and got[0].result >= 0
    echo label, ", second connection: completions=", got.len, " ok=", ok
    for g in got:
      if g.result >= 0: closeFd(cint(g.result))
    closeFd(c1); closeFd(c2); closeFd(lfd)
    while pollCompletions(comps) > 0: discard

  block pipes:
    var fds = default(array[2, cint])
    assert pipe(addr fds[0]) == 0
    setNonBlocking(fds[0])
    setNonBlocking(fds[1])
    var inb = default(array[8, char])
    discard submitRead(fds[0], addr inb[0], 8, never)
    discard settle(50)
    var hi = "hi"
    assert write(fds[1], hi.toCString, 2) == 2
    var got = settle(150)
    echo "pipe read: n=", (if got.len > 0: got[0].result else: -1),
         " buf=", inb[0], inb[1]
    var abc = "abc"
    discard submitWrite(fds[1], abc.toCString, 3, never)
    got = settle(150)
    var outb = default(array[8, char])
    let n = read(fds[0], addr outb[0], 8)
    echo "pipe write: n=", (if got.len > 0: got[0].result else: -1),
         " buf=", outb[0], outb[1], outb[2], (if n == 3: "" else: " short")
    closeFd(fds[0]); closeFd(fds[1])
    while pollCompletions(comps) > 0: discard

{.feature: "assumeSync".}  # test program: globals shared freely
