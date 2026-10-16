## A receive whose deadline passes while the kernel still owns it (io_uring,
## IOCP) is taken back from the kernel before its caller resumes: the caller
## sees `TimeoutError`, and the slot and buffer are not reused until the kernel
## has let go. Each round times out a receive on an idle socket, then sends a
## datagram and receives it on the same socket, so every round reuses the
## slots the cancelled receives held; a late kernel write would corrupt the
## next round's reply.
import std/[syncio]
import std/[socket, ioring, threadpool, atomics, assertions]

const Rounds = 50

var done: int
var log = ""

proc run() {.passive.} =
  var timeouts = 0
  var delivered = 0
  try:
    var a = openUdp(0)
    var b = openUdp(0)
    let aPort = boundPort(a)
    var buf = default(array[64, char])
    for i in 0 ..< Rounds:
      var peer = PeerAddr()
      try:
        discard recvFrom(a, buf, peer, afterMs(5))
      except ErrorCode as e:
        if e == TimeoutError: inc timeouts
      let msg = "round " & $i
      sendTo(b, msg, "127.0.0.1", aPort, afterMs(5000))
      let n = recvFrom(a, buf, peer, afterMs(5000))
      var got = ""
      for j in 0 ..< n: got.add buf[j]
      if got == msg and port(peer) == int(boundPort(b)): inc delivered
    close(a)
    close(b)
  except ErrorCode as e:
    log.add "error: " & $e & "\n"
  log.add "timeouts=" & $timeouts & " delivered=" & $delivered & "\n"
  atomicStore(done, 1, moRelease)

submit(delay(run()), 0)
let start = monoNow()
while atomicLoad(done, moAcquire) == 0:
  if millisUntil(monoNow(), start) > 30_000: quit "timed out"
stdout.write log
stdout.flushFile()

{.feature: "assumeSync".}  # test program: globals shared freely
