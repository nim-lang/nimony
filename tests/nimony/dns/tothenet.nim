## The DNS protocol exercised in one process: an in-process server on an
## ephemeral loopback port answers `query`, and the resolver's wire format
## round-trips a CNAME answer through compression. Nothing here touches a
## network, /etc/resolv.conf, or any privileged port — the resolver's
## `/etc/hosts` path is the only environment-dependent line, and `localhost`
## mapping is assumed present as it is on any machine that can run the suite.
##
## Layout mirrors echo_udp: the server chain answers `QueryCount` datagrams on
## its own thread, the client chain asks them on another, and each appends to
## its own log so the golden is deterministic after both join.
import std/[syncio]
import std/[socket, dns, threadpool, atomics, assertions]

const QueryCount = 2
  ## served before the server chain stops: one known name, one ghost.

var gServer: DnsServer
var thePort: uint16
var serverDone: int
var clientDone: int
var serverLog = ""
var clientLog = ""
var failures = 0

proc server() {.passive.} =
  try:
    for i in 0 ..< QueryCount:
      serveOnce(gServer, afterMs(5000))
    close(gServer)
  except ErrorCode as e:
    serverLog.add "server error: " & $e & "\n"
  serverLog.add "server served " & $QueryCount & " datagrams\n"
  atomicStore(serverDone, 1, moRelease)

proc client() {.passive.} =
  var verified = 0
  try:
    ## The `/etc/hosts` half of the resolver is the only line that reads the
    ## machine; `contributor`-less CI images still map localhost to 127.0.0.1.
    var r = initResolver()
    let localIp = resolve(r, "LOCALHOST.")
    if localIp == "127.0.0.1": inc verified
    else:
      inc failures
      clientLog.add "hosts lookup: " & localIp & "\n"

    ## Aim the resolver's question path at the in-process server, which sits
    ## on a random port: `query` takes the port, `resolve` presumes 53 but
    ## every builder of the question/wire/reply machinery is the same code.
    r.servers = @["127.0.0.1"]
    var m = query(r, "HermetiC.Test.", 0, thePort, afterMs(5000))
    if (m.flags and 0x8000'u16) != 0 and (m.flags and 0x0400'u16) != 0:
      inc verified
    else:
      inc failures
      clientLog.add "response flags: " & $m.flags & "\n"
    if m.answers.len == 1 and m.answers[0].ip4 == "10.0.0.7":
      inc verified
    else:
      inc failures
      clientLog.add "answers: " & $m.answers.len & "\n"

    ## The name the table does not know is answered NXDOMAIN, and `query`
    ## maps that rcode to `NameNotFound` — the resolver's "not a name" error.
    var nx = false
    try:
      discard query(r, "ghost.test", 0, thePort, afterMs(5000))
    except ErrorCode as e:
      if e == NameNotFound: nx = true
    if nx: inc verified
    else:
      inc failures
      clientLog.add "nxdomain not raised\n"

    ## Wire round-trip through the *codec* (the server above only ever encodes
    ## plain A answers, so a compressed CNAME is built by hand): one CNAME from
    ## a host to an alias, one A on the alias. Encoding then decoding must come
    ## back as exactly that, and the alias's address must decode identically.
    var m2 = Message(id: 0x2026'u16, flags: 0x8180'u16)
    m2.questions.add Question(name: "pivot.example", rtype: RTypeA,
                             rclass: 1'u16)
    m2.answers.add Answer(name: "pivot.example", rtype: RTypeCname, ttl: 60,
                          cname: "alias.example")
    m2.answers.add Answer(name: "alias.example", rtype: RTypeA, ttl: 60,
                          ip4: "9.9.9.9")
    var buf: seq[char] = @[]
    encodeMessage(m2, buf)
    var m3 = Message(id: 0'u16, flags: 0'u16)
    if decodeMessage(buf, m3):
      if m3.answers.len == 2 and
         m3.answers[0].cname == "alias.example" and
         m3.answers[1].ip4 == "9.9.9.9":
        inc verified
      else:
        inc failures
        clientLog.add "cname roundtrip mismatch\n"
    else:
      inc failures
      clientLog.add "cname roundtrip undecodable\n"
  except ErrorCode as e:
    clientLog.add "client error: " & $e & "\n"
  clientLog.add "client verified " & $verified & " checks\n"
  atomicStore(clientDone, 1, moRelease)

proc awaitFlag(flag: var int) =
  let start = monoNow()
  while atomicLoad(flag, moAcquire) == 0:
    if millisUntil(monoNow(), start) > 30_000: quit "timed out"
  assert atomicLoad(flag, moAcquire) == 1

gServer = newDnsServer(0)
thePort = boundPort(gServer)
gServer.add "hermetic.test", "10.0.0.7"
echo "listening"
submit(delay(server()), 0)
submit(delay(client()), 1)
awaitFlag(serverDone)
awaitFlag(clientDone)
close(gServer)
stdout.write serverLog
stdout.write clientLog
assert failures == 0
stdout.flushFile()