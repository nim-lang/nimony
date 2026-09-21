## Echo UDP round-trip built on `std/socket`: the server chain binds a UDP
## socket and echoes back every datagram it receives; the client chain — a
## second `.passive` proc in the same process — connects and pushes
## `MessageCount` datagrams, checking each reply matches what it sent.
##
## One file, both halves: the exchange is the test. Socket creation is passive
## now — `openUdp` builds its fd, flags and bind through ring ops — so each
## chain creates its own socket inside its own task and hands the other its
## port through an atomic pair: the server publishes its port then waits for
## the client's, the client waits the other way round, and each connects only
## once both are known. The two `*Connected` flags order the prints the same
## way the golden always had them. Each chain then `udpConnect`s its socket to
## the other's port: connecting a datagram socket does no traffic, it just
## pins the peer, which is what makes the ring's stream-shaped read/write
## paths work unmodified.
##
## Both chains append to their OWN log string — two workers writing to one
## shared string is a data race — and `main` prints them in a fixed order
## after both have joined, so this test's output is deterministic.
import std/[syncio]
import std/[socket, ioring, threadpool, atomics, assertions]

const MessageCount = 100
const BufLen = 64

var gServerSock: UdpSocket
var gClientSock: UdpSocket
var thePort: uint16
var clientPort: uint16
var serverReady: int
var clientReady: int
var serverConnected: int
var serverDone: int
var clientDone: int
var serverLog = ""
var clientLog = ""
var replyMismatch: bool

proc server() {.passive.} =
  var echoed = 0
  try:
    gServerSock = openUdp(0)
    thePort = boundPort(gServerSock)
    echo "listening"
    atomicStore(serverReady, 1, moRelease)
    awaitFlagPassive(clientReady)
    udpConnect(gServerSock, "127.0.0.1", clientPort, afterMs(5000))
    assert ip(gServerSock.peer) == "127.0.0.1"
    assert port(gServerSock.peer) == int(clientPort)
    serverLog.add "server connected\n"
    atomicStore(serverConnected, 1, moRelease)
    var buf = default(array[BufLen, char])
    for i in 0 ..< MessageCount:
      let n = recvDatagram(gServerSock, buf, afterMs(5000))
      if n > 0:
        sendDatagram(gServerSock, toOpenArray(buf, 0, n - 1), afterMs(5000))
        inc echoed
    close(gServerSock)
  except ErrorCode as e:
    serverLog.add "server error: " & $e & "\n"
  serverLog.add "server echoed " & $echoed & " datagrams\n"
  atomicStore(serverDone, 1, moRelease)

proc client() {.passive.} =
  var verified = 0
  try:
    awaitFlagPassive(serverReady)
    gClientSock = openUdp(0)
    clientPort = boundPort(gClientSock)
    atomicStore(clientReady, 1, moRelease)
    ## The golden has always printed the server's connection first; the flag
    ## the server sets after its own line is what keeps that order now that
    ## both chains race to connect.
    awaitFlagPassive(serverConnected)
    udpConnect(gClientSock, "127.0.0.1", thePort, afterMs(5000))
    assert ip(gClientSock.peer) == "127.0.0.1"
    assert port(gClientSock.peer) == int(thePort)
    clientLog.add "client connected\n"
    var buf = default(array[BufLen, char])
    for i in 0 ..< MessageCount:
      let msg = "hello " & $i
      sendDatagram(gClientSock, msg, afterMs(5000))
      let n = recvDatagram(gClientSock, buf, afterMs(5000))
      var reply = ""
      for j in 0 ..< n: reply.add buf[j]
      if reply != msg:
        replyMismatch = true
        clientLog.add "mismatch: " & msg & " vs " & reply & "\n"
      else:
        inc verified
    close(gClientSock)
  except ErrorCode as e:
    clientLog.add "client error: " & $e & "\n"
  clientLog.add "client verified " & $verified & " echoes\n"
  atomicStore(clientDone, 1, moRelease)

proc awaitFlag(flag: var int) =
  let start = monoNow()
  while atomicLoad(flag, moAcquire) == 0:
    if millisUntil(monoNow(), start) > 30_000: quit "timed out"
  assert atomicLoad(flag, moAcquire) == 1

proc awaitFlagPassive(flag: var int) {.passive.} =
  ## The ring-friendly twin of `awaitFlag`: parks on a 1ms timeout between
  ## checks rather than busy-spinning, so the lane keeps polling and any task
  ## sharing this worker still gets scheduled.
  while atomicLoad(flag, moAcquire) == 0:
    var res = 0
    let c = delay()
    discard submitTimeout(afterMs(1), c, addr res)
    suspend()

submit(delay(server()), 0)
submit(delay(client()), 1)
awaitFlag(serverDone)
awaitFlag(clientDone)
assert not replyMismatch
stdout.write serverLog
stdout.write clientLog
stdout.flushFile()