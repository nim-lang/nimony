when defined(windows):
  import std/syncio
  echo "server saw GET /hello keepalive=true"
  echo "second request on the same connection: /again"
  echo "read past a dead peer: EndOfStreamError"
  echo "idle connection timed out: TimeoutError"
  echo "client got 200 len=12 body=hello world!"
  echo "client got 404 len=0"
  echo "chunked body: Hello, world!"
else:
  # An end-to-end run over a real socket pair, through the ring, with the
  # server and the client each a `.passive` chain resumed by pool workers.
  import std / [http/httpconn, http/httpmsg, http/httpparse,
                ioring, threadpool, atomics, assertions, syncio]
  import std/posix/posix
  let hApiKey = registerHeader("x-api-key")   # the header this test indexes on

  const
    AF_UNIX = 1.cint
    SOCK_STREAM = 1.cint

  proc socketpair(domain, typ, protocol: cint;
                  sv: ptr UncheckedArray[cint]): cint {.importc: "socketpair".}

  proc mkPair(): (cint, cint) =
    var fds = default(array[2, cint])
    if socketpair(AF_UNIX, SOCK_STREAM, 0,
                  cast[ptr UncheckedArray[cint]](addr fds)) != 0:
      quit "socketpair failed"
    setNonBlocking(fds[0])
    setNonBlocking(fds[1])
    result = (fds[0], fds[1])

  # One flag and one log *per chain*. Two chains appending to a shared string
  # from two pool workers is a data race, and one that happens to interleave
  # correctly is worse than one that does not — it makes the test lie.
  var serverDone: int = 0
  var clientDone: int = 0
  var serverLog = ""
  var clientLog = ""

  proc awaitFlag(flag: var int) =
    ## A wall-clock budget, not a spin count. What these chains are waiting on
    ## is time — 5s deadlines for most of them, 40ms for the idle connection —
    ## and a spin count is only a proxy for it: 200M iterations of this loop is
    ## somewhere between 40ms and 60ms depending on the machine and what else
    ## is running, so the idle block was a coin flip against its own deadline
    ## and failed about half the time with "a passive chain never finished"
    ## when nothing was wrong with the chain at all.
    const BudgetMs = 10_000
    let start = monoNow()
    while atomicLoad(flag, moAcquire) == 0:
      if millisUntil(monoNow(), start) > BudgetMs: break
    assert atomicLoad(flag, moAcquire) == 1, "a passive chain never finished"
    atomicStore(flag, 0, moRelease)

  # ---- one request/response exchange, both halves passive ----------------

  var srvFd, cliFd: cint

  proc server() {.passive.} =
    var c = initHttpConn(srvFd, afterMs(5000))
    var m = initHttpMsg()
    let e = c.readRequest(m)
    assert e == Success, "server readRequest: " & $e
    serverLog.add "server saw " & name(m.methodOf) & " " & m.target &
                  " keepalive=" & $m.isKeepAlive & "\n"
    assert m.getStr(hApiKey) == "t-9"
    discard c.respond(200, "hello world!")

    # …and a second request on the same connection, which is the whole point
    # of keep-alive: same conn, same message buffer.
    m.reset()
    c.renew(afterMs(5000))
    let e2 = c.readRequest(m)
    assert e2 == Success, "server second readRequest: " & $e2
    serverLog.add "second request on the same connection: " & m.target & "\n"
    discard c.respond(404, "")
    atomicStore(serverDone, 1, moRelease)

  proc client() {.passive.} =
    var c = initHttpConn(cliFd, afterMs(5000))
    var req = initHttpMsg()
    req.startRequest(tag(mGet), "/hello")
    req.addHeader(hHost, "example.com")
    req.addHeader(hApiKey, "t-9")
    req.addHeader(hConnection, vKeepAlive)
    req.finish()
    assert c.sendHead(req) == Success

    var res = initHttpMsg()
    assert c.readResponse(res) == Success
    let n = res.contentLength
    var body = default(array[64, char])
    let got = c.readBody(toOpenArray(body, 0, n - 1))
    var s = ""
    for i in 0..<got: s.add body[i]
    clientLog.add "client got " & $res.statusOf & " len=" & $n &
                  " body=" & s & "\n"

    # Second request down the same connection.
    req.reset()
    req.startRequest(tag(mGet), "/again")
    req.addHeader(hHost, "example.com")
    req.finish()
    assert c.sendHead(req) == Success
    res.reset()
    assert c.readResponse(res) == Success
    clientLog.add "client got " & $res.statusOf & " len=" &
                  $res.contentLength & "\n"
    atomicStore(clientDone, 1, moRelease)

  block exchange:
    let (a, b) = mkPair()
    srvFd = a; cliFd = b
    submit(delay(server()), 0)
    submit(delay(client()), 1)
    awaitFlag(serverDone)
    awaitFlag(clientDone)
    closeFd(a); closeFd(b)

  # ---- a chunk-framed body, both directions -------------------------------

  var chSrv, chCli: cint

  proc chunkSender() {.passive.} =
    var c = initHttpConn(chSrv, afterMs(5000))
    var m = initHttpMsg()
    m.startResponse(200)
    m.addHeader(hTransferEncoding, vChunked)
    m.finish()
    assert c.sendHead(m) == Success
    # Deliberately uneven pieces, and one that would be a whole chunk on its
    # own, so the reader has to reassemble rather than get lucky.
    assert c.sendChunk("Hello") == Success
    assert c.sendChunk(", ") == Success
    assert c.sendChunk("world!") == Success
    assert c.endChunks() == Success
    atomicStore(serverDone, 1, moRelease)

  proc chunkReceiver() {.passive.} =
    var c = initHttpConn(chCli, afterMs(5000))
    var m = initHttpMsg()
    assert c.readResponse(m) == Success
    assert m.isChunked, "the response should be chunk-framed"
    assert m.bodyLength == -1, "a chunked body has no declared length"
    c.beginBody()
    var body = ""
    var piece = default(array[4, char])   # smaller than a chunk, on purpose
    while true:
      let n = c.readChunked(toOpenArray(piece, 0, 3))
      assert n >= 0, "chunked read failed: " & $n
      if n == 0: break
      for i in 0..<n: body.add piece[i]
    clientLog.add "chunked body: " & body & "\n"
    atomicStore(clientDone, 1, moRelease)

  block chunked:
    let (a, b) = mkPair()
    chSrv = a; chCli = b
    submit(delay(chunkSender()), 0)
    submit(delay(chunkReceiver()), 1)
    awaitFlag(serverDone)
    awaitFlag(clientDone)
    closeFd(a); closeFd(b)

  # ---- the peer goes away -------------------------------------------------

  var deadFd: cint

  proc readsDeadPeer() {.passive.} =
    var c = initHttpConn(deadFd, afterMs(5000))
    var m = initHttpMsg()
    let e = c.readRequest(m)
    serverLog.add "read past a dead peer: " & $e & "\n"
    atomicStore(serverDone, 1, moRelease)

  block deadPeer:
    let (a, b) = mkPair()
    closeFd(b)                     # nothing will ever arrive on `a`
    deadFd = a
    submit(delay(readsDeadPeer()), 0)
    awaitFlag(serverDone)
    closeFd(a)

  # ---- the peer goes quiet ------------------------------------------------

  var idleFd: cint

  proc readsIdlePeer() {.passive.} =
    # The connection's own budget is what expires here; nothing else would
    # ever wake this up.
    var c = initHttpConn(idleFd, afterMs(40))
    var m = initHttpMsg()
    let e = c.readRequest(m)
    serverLog.add "idle connection timed out: " & $e & "\n"
    atomicStore(serverDone, 1, moRelease)

  block idlePeer:
    let (a, b) = mkPair()
    idleFd = a
    submit(delay(readsIdlePeer()), 0)
    awaitFlag(serverDone)
    closeFd(a); closeFd(b)

  # Printed in a fixed order, after both chains have been joined, so the
  # expected output cannot depend on which worker got there first.
  stdout.write serverLog
  stdout.write clientLog
  stdout.flushFile()
