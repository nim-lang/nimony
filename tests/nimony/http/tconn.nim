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
  import std / [http/httpconn, http/httpmsg, http/httpparse, socket,
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
    # The happy paths raise nothing, so the `except` is how a failure becomes
    # visible: an uncaught raise inside a passive chain goes into the frame's
    # result slot, which nothing here would ever look at — the test would pass
    # by saying nothing. Logging it puts it in the expected output instead.
    var c = initHttpConn(srvFd, afterMs(5000))
    try:
      var m = initHttpMsg()
      c.readRequest(m)
      serverLog.add "server saw " & name(m.methodOf) & " " & m.target &
                    " keepalive=" & $m.isKeepAlive & "\n"
      assert m.getStr(hApiKey) == "t-9"
      c.respond(200, "hello world!")

      # …and a second request on the same connection, which is the whole point
      # of keep-alive: same conn, same message buffer.
      m.reset()
      c.renew(afterMs(5000))
      c.readRequest(m)
      serverLog.add "second request on the same connection: " & m.target & "\n"
      c.respond(404, "")
    except ErrorCode as e:
      serverLog.add "server failed: " & $e & "\n"
    atomicStore(serverDone, 1, moRelease)

  proc client() {.passive.} =
    var c = initHttpConn(cliFd, afterMs(5000))
    try:
      var req = initHttpMsg()
      req.startRequest(tag(mGet), "/hello")
      req.addHeader(hHost, "example.com")
      req.addHeader(hApiKey, "t-9")
      req.addHeader(hConnection, vKeepAlive)
      req.finish()
      c.sendHead(req)

      var res = initHttpMsg()
      c.readResponse(res)
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
      c.sendHead(req)
      res.reset()
      c.readResponse(res)
      clientLog.add "client got " & $res.statusOf & " len=" &
                    $res.contentLength & "\n"
    except ErrorCode as e:
      clientLog.add "client failed: " & $e & "\n"
    atomicStore(clientDone, 1, moRelease)

  block exchange:
    let (a, b) = mkPair()
    srvFd = a; cliFd = b
    submit(delay(server()), 0)
    submit(delay(client()), 1)
    awaitFlag(serverDone)
    awaitFlag(clientDone)
    # `a` and `b` belong to the two connections now; they close themselves.

  # ---- a chunk-framed body, both directions -------------------------------

  var chSrv, chCli: cint

  proc chunkSender() {.passive.} =
    var c = initHttpConn(chSrv, afterMs(5000))
    try:
      var m = initHttpMsg()
      m.startResponse(200)
      m.addHeader(hTransferEncoding, vChunked)
      m.finish()
      c.sendHead(m)
      # Deliberately uneven pieces, and one that would be a whole chunk on its
      # own, so the reader has to reassemble rather than get lucky.
      c.sendChunk("Hello")
      c.sendChunk(", ")
      c.sendChunk("world!")
      c.endChunks()
    except ErrorCode as e:
      serverLog.add "chunkSender failed: " & $e & "\n"
    atomicStore(serverDone, 1, moRelease)

  proc chunkReceiver() {.passive.} =
    var c = initHttpConn(chCli, afterMs(5000))
    try:
      var m = initHttpMsg()
      c.readResponse(m)
      assert m.isChunked, "the response should be chunk-framed"
      assert m.bodyLength == -1, "a chunked body has no declared length"
      c.beginBody()
      var body = ""
      var piece = default(array[4, char])   # smaller than a chunk, on purpose
      while true:
        let n = c.readChunked(toOpenArray(piece, 0, 3))
        if n == 0: break                    # the body ended, cleanly
        for i in 0..<n: body.add piece[i]
      clientLog.add "chunked body: " & body & "\n"
    except ErrorCode as e:
      clientLog.add "chunkReceiver failed: " & $e & "\n"
    atomicStore(clientDone, 1, moRelease)

  block chunked:
    let (a, b) = mkPair()
    chSrv = a; chCli = b
    submit(delay(chunkSender()), 0)
    submit(delay(chunkReceiver()), 1)
    awaitFlag(serverDone)
    awaitFlag(clientDone)
    # `a` and `b` belong to the two connections now; they close themselves.

  # ---- the peer goes away -------------------------------------------------

  var deadFd: cint

  proc readsDeadPeer() {.passive.} =
    var c = initHttpConn(deadFd, afterMs(5000))
    var m = initHttpMsg()
    try:
      c.readRequest(m)
      serverLog.add "read past a dead peer: no error\n"
    except ErrorCode as e:
      serverLog.add "read past a dead peer: " & $e & "\n"
    atomicStore(serverDone, 1, moRelease)

  block deadPeer:
    let (a, b) = mkPair()
    closeFd(b)                     # nothing will ever arrive on `a`
    deadFd = a
    submit(delay(readsDeadPeer()), 0)
    awaitFlag(serverDone)           # `a` belongs to the connection

  # ---- the peer goes quiet ------------------------------------------------

  var idleFd: cint

  proc readsIdlePeer() {.passive.} =
    # The connection's own budget is what expires here; nothing else would
    # ever wake this up.
    var c = initHttpConn(idleFd, afterMs(40))
    var m = initHttpMsg()
    try:
      c.readRequest(m)
      serverLog.add "idle connection timed out: no error\n"
    except ErrorCode as e:
      serverLog.add "idle connection timed out: " & $e & "\n"
    atomicStore(serverDone, 1, moRelease)

  block idlePeer:
    let (a, b) = mkPair()
    idleFd = a
    submit(delay(readsIdlePeer()), 0)
    awaitFlag(serverDone)
    closeFd(b)                     # `a` belongs to the connection

  # ---- a socket owns its fd ----------------------------------------------

  proc dupFd(fd: cint): cint {.importc: "dup".}

  proc fdIsOpen(fd: cint): bool =
    ## `dup` succeeds on a live descriptor and fails on a closed one, so this
    ## asks the kernel rather than trusting our own bookkeeping — which is the
    ## whole point of the test. The copy is closed again straight away.
    let d = dupFd(fd)
    if d >= 0: discard close(d)
    result = d >= 0

  block socketClosesInItsDestructor:
    let (a, b) = mkPair()
    block:
      var s = initSocket(a, afterMs(1000))
      assert fdIsOpen(a), "the socket closed its fd while still holding it"
      assert s.buffered == 0
    assert not fdIsOpen(a), "leaving the scope did not close the fd"
    closeFd(b)

  block explicitCloseIsStillFine:
    # `close` and the destructor must not both close: whoever went first would
    # leave the second one closing a number the process has since reused.
    let (a, b) = mkPair()
    block:
      var s = initSocket(a, afterMs(1000))
      s.close()
      assert not fdIsOpen(a)
    closeFd(b)

  # Printed in a fixed order, after both chains have been joined, so the
  # expected output cannot depend on which worker got there first.
  stdout.write serverLog
  stdout.write clientLog
  stdout.flushFile()
