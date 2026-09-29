## The DNS protocol exercised in one process: an in-process server on an
## ephemeral loopback port answers `query`, and the resolver's wire format
## round-trips a CNAME answer through compression. Nothing here touches a
## network, /etc/resolv.conf, or any privileged port — the resolver's
## `/etc/hosts` path is the only environment-dependent line, and `localhost`
## mapping is assumed present as it is on any machine that can run the suite.
##
## Layout mirrors echo_udp: the server chain runs the `httpserver`-style serve
## loop — `next`/`answer`/`respond` — a bounded number of queries on its own
## thread and then closes, the client chain asks it from another, and each
## appends to its own log so the golden is deterministic after both join.
import std/[syncio]
import std/[socket, dns, ioring, threadpool, atomics, assertions]

var gServer: DnsServer
var thePort: uint16
var serverReady: int
var serverDone: int
var clientDone: int
var serverLog = ""
var clientLog = ""
var failures = 0
var served = 0

const QueryCount = 4
  ## The queries the client sends: hermetic, multi, alias, ghost. The serve
  ## loop is bounded the way http's tserver bounds its accept loop — `main`
  ## lives on a different lane than the server worker, so a cross-thread
  ## `close` cannot cancel a parked receive, and a count is how the loop ends.

proc server() {.passive.} =
  var r = DnsRequest()
  try:
    ## Socket creation is passive now (`openUdp`/`newDnsServer` build through
    ## ring ops), so the server binds its own socket here, publishes the port
    ## for the client, and only then takes queries. The client waits for
    ## `serverReady` before its first send, so the port is always set.
    gServer = newDnsServer(0)
    thePort = boundPort(gServer)
    echo "listening"
    atomicStore(serverReady, 1, moRelease)
    for i in 0 ..< QueryCount:
      if not gServer.next(r, afterMs(10_000)): break
      for q in r.questions:
        case q.name
        of "hermetic.test":
          q.answer(name = q.name, rtype = RTypeA, ttl = 30, data = "10.0.0.7")
        of "multi.test":
          q.answer(name = q.name, rtype = RTypeA, ttl = 30, data = "10.0.0.1")
          q.answer(name = q.name, rtype = RTypeA, ttl = 30, data = "10.0.0.2")
        of "alias.test":
          q.answer(name = q.name, rtype = RTypeCname, ttl = 30,
                   data = "hermetic.test")
          q.answer(name = "hermetic.test", rtype = RTypeA, ttl = 30,
                   data = "10.0.0.7")
        of "ghost.test":
          r.rcode = 3             ## NXDOMAIN
        else: discard
      r.respond()
      inc served
    close(gServer)
  except ErrorCode as e:
    serverLog.add "server error: " & $e & "\n"
  serverLog.add "server served " & $served & " datagrams\n"
  atomicStore(serverDone, 1, moRelease)

proc client() {.passive.} =
  var verified = 0
  try:
    awaitFlagPassive(serverReady)
    ## The `/etc/hosts` half of the resolver is the only line that reads the
    ## machine; `contributor`-less CI images still map localhost to 127.0.0.1.
    var r = initResolver()
    let localIp = resolve(r, "LOCALHOST.")
    if localIp == "127.0.0.1": inc verified
    else:
      inc failures
      clientLog.add "hosts lookup: " & localIp & "\n"

    ## The no-wiring form goes through the process-wide default resolver,
    ## initialised on first use from the same /etc/hosts under its lock.
    try:
      if resolve("LOCALHOST.") == "127.0.0.1": inc verified
      else:
        inc failures
        clientLog.add "default resolver lookup failed\n"
    except ErrorCode as e:
      inc failures
      clientLog.add "default resolver error: " & $e & "\n"

    ## Aim the resolver's question path at the in-process server, which sits
    ## on a random port: `query` takes the port, `resolve` presumes 53 but
    ## every builder of the question/wire/reply machinery is the same code.
    r.servers = @["127.0.0.1"]
    var m = query(r, "HermetiC.Test.", 0, thePort, afterMs(5000))
    if (m.flags and 0x8580'u16) == 0x8580'u16:   # QR|AA|RD|RA, rcode 0
      inc verified
    else:
      inc failures
      clientLog.add "response flags: " & $m.flags & "\n"
    if m.answers.len == 1 and m.answers[0].ip4 == "10.0.0.7":
      inc verified
    else:
      inc failures
      clientLog.add "single answers: " & $m.answers.len & "\n"

    ## Two `answer` calls for one question come back as two records in one
    ## response — the multi-A round-robin shape.
    var m2 = query(r, "multi.test", 0, thePort, afterMs(5000))
    if m2.answers.len == 2 and m2.answers[0].ip4 == "10.0.0.1" and
       m2.answers[1].ip4 == "10.0.0.2":
      inc verified
    else:
      inc failures
      clientLog.add "multi answers: " & $m2.answers.len & "\n"

    ## The server answers the CNAME chase itself: alias carries a CNAME to
    ## hermetic, which carries the A — the stretch `resolveOne` walks in one
    ## response. Walked by hand here because `resolve` presumes port 53.
    var m3 = query(r, "alias.test", 0, thePort, afterMs(5000))
    var wanted = "alias.test"
    for hop in 0 ..< m3.answers.len:
      var chased = false
      for a in m3.answers:
        if a.rtype == RTypeCname and a.name == wanted:
          wanted = a.cname
          chased = true
          break
      if not chased: break
    var chasedIp = ""
    for a in m3.answers:
      if a.rtype == RTypeA and a.name == wanted and a.ip4.len > 0:
        chasedIp = a.ip4
        break
    if chasedIp == "10.0.0.7": inc verified
    else:
      inc failures
      clientLog.add "cname chase: " & chasedIp & "\n"

    ## The name the handler refuses is answered NXDOMAIN, and `query` maps
    ## that rcode to `NameNotFound` — the resolver's "not a name" error.
    var nx = false
    try:
      discard query(r, "ghost.test", 0, thePort, afterMs(5000))
    except ErrorCode as e:
      if e == NameNotFound: nx = true
    if nx: inc verified
    else:
      inc failures
      clientLog.add "nxdomain not raised\n"

    ## Wire round-trip through the *codec* (the server only ever encodes the
    ## shapes above, so a compressed CNAME is built by hand): one CNAME from a
    ## host to an alias, one A on the alias. Encoding then decoding must come
    ## back as exactly that, and the alias's address must decode identically.
    var m4 = Message(id: 0x2026'u16, flags: 0x8180'u16)
    m4.questions.add Question(name: "pivot.example", rtype: RTypeA,
                             rclass: 1'u16)
    m4.answers.add Answer(name: "pivot.example", rtype: RTypeCname, ttl: 60,
                          cname: "alias.example")
    m4.answers.add Answer(name: "alias.example", rtype: RTypeA, ttl: 60,
                          ip4: "9.9.9.9")
    var buf: seq[char] = @[]
    encodeMessage(m4, buf)
    var m5 = Message(id: 0'u16, flags: 0'u16)
    if decodeMessage(buf, m5):
      if m5.answers.len == 2 and
         m5.answers[0].cname == "alias.example" and
         m5.answers[1].ip4 == "9.9.9.9":
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

proc awaitFlagPassive(flag: var int) {.passive.} =
  ## The ring-friendly twin of `awaitFlag`: parks on a 1ms timeout between
  ## checks instead of busy-spinning, so a lane keeps polling and any task
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
close(gServer)
stdout.write serverLog
stdout.write clientLog
assert failures == 0
stdout.flushFile()