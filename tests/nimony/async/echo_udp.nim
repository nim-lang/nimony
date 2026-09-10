## Echo UDP round-trip built on `std/socket`: the server chain binds a UDP
## socket and echoes back every datagram it receives; the client chain — a
## second `.passive` proc in the same process — connects and pushes
## `MessageCount` datagrams, checking each reply matches what it sent.
##
## One file, both halves: the exchange is the test. Both sockets are created
## synchronously in `main` (binding has nothing for the ring to park on) so
## both ports are known before either chain starts — no cross-worker race.
## Each chain then `udpConnect`s its socket to the other's port: connecting a
## datagram socket does no traffic, it just pins the peer, which is what makes
## the ring's stream-shaped read/write paths work unmodified.
##
## Both chains append to their OWN log string — two workers writing to one
## shared string is a data race — and `main` prints them in a fixed order
## after both have joined, so this test's output is deterministic.
import std/[syncio]
import std/[socket, threadpool, atomics, assertions]

const MessageCount = 100
const BufLen = 64

var gServerSock: UdpSocket
var gClientSock: UdpSocket
var thePort: uint16
var clientPort: uint16
var serverDone: int
var clientDone: int
var serverLog = ""
var clientLog = ""
var replyMismatch: bool

proc server() {.passive.} =
  var echoed = 0
  try:
    udpConnect(gServerSock, "127.0.0.1", clientPort, afterMs(5000))
    assert ip(gServerSock.peer) == "127.0.0.1"
    assert port(gServerSock.peer) == int(clientPort)
    serverLog.add "server connected\n"
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

gServerSock = openUdp(0)
thePort = boundPort(gServerSock)
gClientSock = openUdp(0)
clientPort = boundPort(gClientSock)
echo "listening"
submit(delay(server()), 0)
submit(delay(client()), 1)
awaitFlag(serverDone)
awaitFlag(clientDone)
close(gServerSock)
close(gClientSock)
assert not replyMismatch
stdout.write serverLog
stdout.write clientLog
stdout.flushFile()