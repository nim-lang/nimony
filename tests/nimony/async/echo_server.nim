## Echo TCP round-trip built on `std/socket`: the server chain listens,
## accepts, and echoes back every line it reads; the client chain — a second
## `.passive` proc in the same process — connects and pushes `MessageCount`
## messages, checking each reply matches what it sent.
##
## One file, both halves: the exchange is the test. The listener is created
## synchronously in `main` (binding has nothing for the ring to park on) so
## the port is known before either chain starts — no cross-worker race.
##
## Both chains append to their OWN log string — two workers writing to one
## shared string is a data race — and `main` prints them in a fixed order
## after both have joined, so this test's output is deterministic.
import std/[syncio]
import std/[socket, threadpool, atomics, assertions]

const MessageCount = 100

var gL: Listener
var thePort: uint16
var serverDone: int
var clientDone: int
var serverLog = ""
var clientLog = ""
var replyMismatch: bool

proc server() {.passive.} =
  var echoed = 0
  try:
    var s = accept(gL)
    serverLog.add "server accepted\n"
    for i in 0 ..< MessageCount:
      let line = readLine(s)
      writeLine(s, line)
      inc echoed
    close(s)
  except ErrorCode as e:
    serverLog.add "server error: " & $e & "\n"
  serverLog.add "server echoed " & $echoed & " messages\n"
  atomicStore(serverDone, 1, moRelease)

proc client() {.passive.} =
  var verified = 0
  try:
    var s = connect("127.0.0.1", thePort, afterMs(5000))
    clientLog.add "client connected\n"
    for i in 0 ..< MessageCount:
      let msg = "hello " & $i
      writeLine(s, msg)
      let reply = readLine(s, afterMs(5000))
      if reply != msg:
        replyMismatch = true
        clientLog.add "mismatch: " & msg & " vs " & reply & "\n"
      else:
        inc verified
    close(s)
  except ErrorCode as e:
    clientLog.add "client error: " & $e & "\n"
  clientLog.add "client verified " & $verified & " echoes\n"
  atomicStore(clientDone, 1, moRelease)

proc awaitFlag(flag: var int) =
  let start = monoNow()
  while atomicLoad(flag, moAcquire) == 0:
    if millisUntil(monoNow(), start) > 30_000: quit "timed out"
  assert atomicLoad(flag, moAcquire) == 1

gL = listen(0)
thePort = boundPort(gL)
echo "listening"
submit(delay(server()), 0)
submit(delay(client()), 1)
awaitFlag(serverDone)
awaitFlag(clientDone)
close(gL)
assert not replyMismatch
stdout.write serverLog
stdout.write clientLog
stdout.flushFile()