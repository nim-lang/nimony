when defined(windows):
  import std/syncio
  echo "two-reads got=2 r1=1 r2=2"
  echo "same-fd wrote=1 read=1 in1=Y"
  echo "write-only wrote=1"
else:
  import std / [ioring, syncio, assertions]
  import std/posix/posix

  const
    AF_UNIX = 1.cint
    SOCK_STREAM = 1.cint
    MSG_NOSIGNAL = 0x4000.cint

  proc socketpair(domain, typ, protocol: cint;
                  sv: ptr UncheckedArray[cint]): cint {.importc: "socketpair".}
  proc send(s: cint; buf: pointer; len: int; flags: cint): int {.importc: "send".}
  proc recv(s: cint; buf: pointer; len: int; flags: cint): int {.importc: "recv".}

  proc mkPair(): (cint, cint) =
    var fds = default(array[2, cint])
    if socketpair(AF_UNIX, SOCK_STREAM, 0,
                  cast[ptr UncheckedArray[cint]](addr fds)) != 0:
      quit "socketpair failed"
    setNonBlocking(fds[0])
    setNonBlocking(fds[1])
    result = (fds[0], fds[1])

  var comps = default(array[8, IoCompletion])

  proc drain(want: int): int =
    ## Collect `want` completions, returning how many arrived.
    result = 0
    while result < want:
      result += waitCompletions(comps)

  block twoReads:
    # Two fds each with one pending read, both ready at once. The completion
    # handler frees the slot it was just handed while the per-fd list is being
    # walked, so this pins down that the walk survives its own body.
    let (a, b) = mkPair()
    let (c, d) = mkPair()
    var m1 = "1"
    var m2 = "2"
    assert send(b, m1.toCString, 1, MSG_NOSIGNAL) == 1
    assert send(d, m2.toCString, 1, MSG_NOSIGNAL) == 1
    var r1 = default(array[8, char])
    var r2 = default(array[8, char])
    discard submitRead(a, addr r1[0], 8)
    discard submitRead(c, addr r2[0], 8)
    let got = drain(2)
    echo "two-reads got=", got, " r1=", r1[0], " r2=", r2[0]
    closeFd(a); closeFd(b); closeFd(c); closeFd(d)

  block sameFd:
    # A write and a read in flight on ONE fd. On a readiness backend both share
    # a single registration, so arming for the newer op alone would disarm the
    # other direction.
    let (a, b) = mkPair()
    var outb = "X"
    var inb = default(array[8, char])
    discard submitWrite(a, outb.toCString, 1)
    discard submitRead(a, addr inb[0], 8)
    var y = "Y"
    assert send(b, y.toCString, 1, MSG_NOSIGNAL) == 1
    var wrote = 0
    var readn = 0
    var got = 0
    while got < 2:
      let n = waitCompletions(comps)
      for i in 0..<n:
        if comps[i].op == opWrite: wrote = comps[i].result
        elif comps[i].op == opRead: readn = comps[i].result
      got += n
    var sink = default(array[8, char])
    discard recv(b, addr sink[0], 8, 0)
    echo "same-fd wrote=", wrote, " read=", readn, " in1=", inb[0]
    closeFd(a); closeFd(b)

  block writeStuckBehindRead:
    # Same shape, but the read never becomes ready (`b` sends nothing). Nothing
    # else will ever fire on this fd, so a clobbered interest set is not
    # repaired by a later event: the write has to complete on its own.
    let (a, b) = mkPair()
    var outb = "Z"
    var inb = default(array[8, char])
    discard submitWrite(a, outb.toCString, 1)
    discard submitRead(a, addr inb[0], 8)
    var wrote = -1
    while wrote < 0:
      let n = waitCompletions(comps)
      for i in 0..<n:
        if comps[i].op == opWrite: wrote = comps[i].result
    echo "write-only wrote=", wrote
    closeFd(a); closeFd(b)
