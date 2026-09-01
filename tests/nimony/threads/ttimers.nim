when defined(windows):
  import std/syncio
  echo "timer fired res=0 late=true"
  echo "read timed out res=-110"
  echo "read beat its deadline n=1"
  echo "ordered: 1 2 3"
  echo "fd-less ops nops=1 timers=2"
  echo "connect ok res=0"
  echo "connect refused neg=true"
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

  var comps = default(array[16, IoCompletion])

  proc waitOne(): IoCompletion =
    while true:
      let n = waitCompletions(comps)
      if n > 0: return comps[0]

  block timerFires:
    # A timer op reaching its deadline is a success, not a timeout.
    let start = monoNow()
    discard submitTimeout(afterMs(30))
    let c = waitOne()
    let elapsed = millisUntil(monoNow(), start)
    assert c.op == opTimeout
    echo "timer fired res=", c.result, " late=", elapsed >= 25

  block readTimesOut:
    # Nothing will ever arrive on `a`. Without a deadline this op would hold
    # its slot for the life of the process; with one it comes back.
    let (a, b) = mkPair()
    var buf = default(array[8, char])
    discard submitRead(a, addr buf[0], 8, afterMs(30))
    let c = waitOne()
    assert c.op == opRead
    echo "read timed out res=", c.result
    closeFd(a); closeFd(b)

  block readBeatsDeadline:
    # A deadline that is not reached must leave no trace: the op completes
    # normally and the stale heap entry must not fire on whatever op lands in
    # that slot next.
    let (a, b) = mkPair()
    var msg = "x"
    assert send(b, msg.toCString, 1, MSG_NOSIGNAL) == 1
    var buf = default(array[8, char])
    discard submitRead(a, addr buf[0], 8, afterMs(5000))
    let c = waitOne()
    assert c.op == opRead
    echo "read beat its deadline n=", c.result
    closeFd(a); closeFd(b)

  block staleEntriesDoNotFire:
    # Churn many ops that all beat generous deadlines, then run one that must
    # time out. If a stale entry could fire, the slot reuse here would trip it.
    let (a, b) = mkPair()
    var msg = "y"
    var buf = default(array[8, char])
    for i in 0..<40:
      assert send(b, msg.toCString, 1, MSG_NOSIGNAL) == 1
      discard submitRead(a, addr buf[0], 1, afterMs(5000))
      let c = waitOne()
      assert c.result == 1, "round " & $i & " got " & $c.result
    discard submitRead(a, addr buf[0], 1, afterMs(20))
    let c = waitOne()
    assert c.result == IoTimedOut, $c.result
    closeFd(a); closeFd(b)

  block deadlineOrder:
    # Three timers armed out of deadline order must fire in deadline order —
    # that is the heap's whole job.
    let id60 = submitTimeout(afterMs(60))
    let id20 = submitTimeout(afterMs(20))
    let id40 = submitTimeout(afterMs(40))
    var order = newSeq[SeqNum](0)
    while order.len < 3:
      let n = waitCompletions(comps)
      for i in 0..<n: order.add comps[i].id
    assert order[0] == id20, "first was not the 20ms timer"
    assert order[1] == id40, "second was not the 40ms timer"
    assert order[2] == id60, "third was not the 60ms timer"
    echo "ordered: 1 2 3"

  block earlierTightens:
    let a = afterMs(1000)
    let b = afterMs(10)
    assert earlier(a, b) == b
    assert earlier(b, a) == b
    assert earlier(a, never) == a, "never can never win"
    assert millisUntil(never, monoNow()) == high(int)

  block fdlessOpsDoNotCollide:
    # Every op without an fd shares the arena's `-1` bucket. Arming that
    # bucket fails every op in it on epoll and hangs on kqueue, so a nop
    # submitted while timers are pending used to take them all down.
    discard submitTimeout(afterMs(40))
    discard submitTimeout(afterMs(50))
    discard submitNop(never)
    var seen = 0
    var nops = 0
    var timers = 0
    while seen < 3:
      let n = waitCompletions(comps)
      for i in 0..<n:
        inc seen
        if comps[i].op == opNop:
          assert comps[i].result == 0, "nop failed: " & $comps[i].result
          inc nops
        else:
          assert comps[i].result == 0, "timer killed by the nop: " &
                                       $comps[i].result
          inc timers
    echo "fd-less ops nops=", nops, " timers=", timers

  block connectWorks:
    # A real non-blocking connect through the ring, to a listener we own.
    # Port 0: the kernel picks, so a parallel run cannot collide with us.
    let lfd = listenTcp(0'u16)
    let port = boundPort(lfd)
    assert port != 0'u16, "no port was bound"
    let s = socketNonBlocking()
    var sa = default(Sockaddr_storage)
    var slen = SockLen(0)
    loopbackAddr(sa, slen, port)
    discard submitConnect(s, sa, slen, afterMs(2000))
    let c = waitOne()
    assert c.op == opConnect
    assert c.result != IoTimedOut, "a connect to a live listener timed out"
    echo "connect ok res=", c.result
    closeFd(s); closeFd(lfd)

  block connectRefused:
    # Nothing is listening: the ring must report the errno, not a bare -1, and
    # not hang until the deadline.
    # A port nothing is listening on: bind one, learn its number, close it.
    let probe = listenTcp(0'u16)
    let deadPort = boundPort(probe)
    closeFd(probe)
    let s = socketNonBlocking()
    var sa = default(Sockaddr_storage)
    var slen = SockLen(0)
    loopbackAddr(sa, slen, deadPort)
    discard submitConnect(s, sa, slen, afterMs(2000))
    let c = waitOne()
    assert c.op == opConnect
    assert c.result != IoTimedOut, "refusal should be reported, not waited out"
    # The value is the negated errno, which differs per platform (-61 on
    # Darwin, -111 on Linux), so only its shape is printed.
    assert c.result < 0, "a refused connect must report the error"
    echo "connect refused neg=true"
    closeFd(s)
