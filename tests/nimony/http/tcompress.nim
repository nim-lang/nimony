# Content codings end to end: `httpserver` with `compression` on, and a
# client on `httpconn` that asks for gzip or deflate and reads the content
# back with `httpcoding.readContent`. Over a real loopback socket, so the
# framing (Content-Length vs chunked) is the one a peer actually sees.

import std / [httpserver, http/httpconn, socket, ioring, threadpool, atomics,
              assertions, syncio, compress/gzip]

let tags = newHttpTags()

var serverDone: int = 0
var clientDone: int = 0
var gPort: int = 0
var listening: int = 0
var log = ""

proc awaitFlag(flag: var int) =
  const BudgetMs = 20_000
  let start = monoNow()
  while atomicLoad(flag, moAcquire) == 0:
    if millisUntil(monoNow(), start) > BudgetMs: break
  assert atomicLoad(flag, moAcquire) == 1, "a passive chain never finished"
  atomicStore(flag, 0, moRelease)

proc page(): string =
  ## Big enough to be worth compressing, repetitive enough that it must
  ## shrink a lot.
  result = ""
  for i in 0..<400:
    result.add "<li>item " & $i & " of the list that compresses well</li>\n"

# ------------------------------------------------------------ the server --

proc handle(c: sink HttpConnection) {.passive.} =
  while c.next():
    try:
      if c.path == "/page":
        c.respond(200, page(), "text/html; charset=utf-8")
      elif c.path == "/small":
        c.respond(200, "tiny", "text/plain")
      elif c.path == "/png":
        c.respond(200, page(), "image/png")
      elif c.path == "/stream":
        c.beginStream(200, "text/plain")
        for i in 0..<50:
          c.write("streamed line " & $i & " with some repeated text\n")
        c.finish()
      elif c.path == "/echo":
        # The request body arrives gzipped; `readContent` undoes it. A coding
        # it cannot undo is the handler's to answer, and 415 is the answer.
        var body = ""
        var status = 200
        try:
          body = c.readContent()
        except ErrorCode as e:
          status = if e == UnimplementedOperation: 415 else: 400
        if status == 200:
          c.respond(200, "got " & $body.len & " bytes, same=" &
                    $(body == page()), "text/plain")
        else:
          c.respond(status, "", "text/plain")
      else:
        c.respond(404, "", "text/plain")
    except ErrorCode:
      c.close()

var gServer = 0

proc serverChain() {.passive.} =
  var s = listenHttp(0'u16, tags)
  s.compression = true
  gPort = int(boundPort(s.fd))
  atomicStore(listening, 1, moRelease)
  var left = gServer
  while left > 0:
    let c = s.accept()
    if c.isClosed: break
    submit(delay(handle(c)), -1)
    dec left
  s.close()
  atomicStore(serverDone, 1, moRelease)

# ------------------------------------------------------------ the client --

proc fetch(meth: HttpTag; path, accept: string; body = "";
           bodyCoding = ""): string {.passive.} =
  ## One request on its own connection. Answers a line describing what came
  ## back: status, the coding headers, and the content after decoding.
  result = ""
  let fd = socketNonBlocking()
  var sa = default(Sockaddr_storage)
  var sl = default(SockLen)
  loopbackAddr(sa, sl, uint16(gPort))
  var r = 0
  let k = delay()
  discard submitConnect(cint(fd), sa, sl, afterMs(5000), k, addr r)
  suspend()
  if r != 0:
    closeFd(cint(fd))
    return "CONNECT FAILED " & $r
  var c = initHttpConn(cint(fd), afterMs(5000), tags)
  try:
    var req = initHttpMsg(tags)
    req.startRequest(tag(meth), path)
    req.addHeader(hHost, "x")
    req.addHeader(hConnection, vClose)
    if accept.len > 0: req.addHeader(hAcceptEncoding, accept)
    if bodyCoding.len > 0: req.addHeader(hContentEncoding, bodyCoding)
    if body.len > 0 or meth == mPost: req.addHeader(hContentLength, body.len)
    req.finish()
    c.sendHead(req)
    if body.len > 0: c.sendBody(toOpenArray(body, 0, body.len - 1))
    var res = initHttpMsg(tags)
    c.readResponse(res)
    result = $res.statusOf & " ce=" & res.getStr(hContentEncoding) &
             " vary=" & res.getStr(hVary) &
             " chunked=" & $res.isChunked
    if meth == mHead:
      result.add " length=" & $res.contentLength
    else:
      let wire = res.contentLength
      let content = c.readContent(res)
      if wire >= 0: result.add " wire=" & $wire
      result.add " content=" & $content.len
      if path == "/page" or path == "/png":
        result.add " same=" & $(content == page())
      elif content.len < 64:
        result.add " text=" & content
  except ErrorCode as e:
    result.add " CLIENT " & $e
  c.close()

proc clients() {.passive.} =
  log.add "gzip:     " & fetch(mGet, "/page", "gzip, deflate") & "\n"
  log.add "deflate:  " & fetch(mGet, "/page", "deflate") & "\n"
  log.add "q=0:      " & fetch(mGet, "/page", "gzip;q=0, *") & "\n"
  log.add "none:     " & fetch(mGet, "/page", "") & "\n"
  log.add "identity: " & fetch(mGet, "/page", "identity") & "\n"
  log.add "small:    " & fetch(mGet, "/small", "gzip") & "\n"
  log.add "png:      " & fetch(mGet, "/png", "gzip") & "\n"
  log.add "head:     " & fetch(mHead, "/page", "gzip") & "\n"
  log.add "stream:   " & fetch(mGet, "/stream", "gzip") & "\n"
  log.add "upload:   " & fetch(mPost, "/echo", "",
                               compress(page(), fmtGzip), "gzip") & "\n"
  log.add "badcode:  " & fetch(mPost, "/echo", "", "xyz", "br") & "\n"
  atomicStore(clientDone, 1, moRelease)

proc main =
  initIoRing()
  gServer = 11
  submit(delay(serverChain()), 0)
  let start = monoNow()
  while atomicLoad(listening, moAcquire) == 0:
    if millisUntil(monoNow(), start) > 5000: break
  assert atomicLoad(listening, moAcquire) == 1, "the listener never came up"
  submit(delay(clients()), 1)
  awaitFlag(clientDone)
  awaitFlag(serverDone)
  stdout.write log

main()

{.feature: "assumeSync".}  # test program: globals shared freely
