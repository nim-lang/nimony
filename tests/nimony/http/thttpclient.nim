# `std/httpclient` against an in-process `std/httpserver`: the response body
# as a `Reader`, de-framed (Content-Length, chunked) and decoded (gzip), a
# non-2xx status raised as its `ErrorCode`, a streamed POST body, and the
# reserved-header and header-injection checks.

import std / [httpserver, httpclient, ioring, threadpool, atomics, assertions,
              syncio, strutils]

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

proc bigText(): string =
  result = ""
  for i in 0 ..< 400: result.add "line " & $i & " of a compressible body\n"

proc handle(c: sink HttpConnection) {.passive.} =
  while c.next():
    try:
      if c.path == "/":
        c.respond(200, "hello\n", "text/plain")
      elif c.path == "/stream":
        c.beginStream(200, "text/plain")
        c.write("one ")
        c.write("two ")
        c.write("three")
        c.finish()
      elif c.path == "/big":
        c.respond(200, bigText(), "text/plain")
      elif c.path == "/echo":
        let body = c.readBody()
        c.respond(200, "got " & $body.len & ": " & body, "text/plain")
      else:
        c.respond(404, "no\n", "text/plain")
    except ErrorCode:
      c.close()

var gServer = 0

proc serverChain() {.passive.} =
  var s = listenHttp(0'u16, tags)
  s.serverName = "t"
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

proc url(path: string): string = "http://127.0.0.1:" & $gPort & path

proc clientChain() {.passive.} =
  try:
    var r = httpGet(url("/"))
    log.add "[1] " & r.readAll()

    var resp = HttpHeaders("")
    var s = httpGet(url("/stream"), HttpHeaders(""), resp)
    log.add "[2] " & s.readAll() & " (transfer-encoding: " &
            resp.get("Transfer-Encoding") & ")\n"

    var b = httpGet(url("/big"), HttpHeaders(""), resp)
    let big = b.readAll()
    log.add "[3] " & $(big == bigText()) & " (content-encoding: " &
            resp.get("Content-Encoding") & ")\n"

    try:
      var nf = httpGet(url("/missing"))
      log.add "[4] no error: " & nf.readAll() & "\n"
    except ErrorCode as e:
      log.add "[4] " & $e & "\n"

    var payload = ""
    for i in 0 ..< 5000: payload.add char(ord('a') + i mod 26)
    var p = httpPost(url("/echo"), openReader(payload), "text/plain")
    let answer = p.readAll()
    log.add "[5] " & answer.substr(0, 12) & " ... " & $(answer.endsWith(payload)) & "\n"

    var h = headers"X-Trace: 1\r\n"
    try:
      h.add "X-Evil", "a\r\nInjected: yes"
      log.add "[6] no error\n"
    except ErrorCode as e:
      log.add "[6] injection: " & $e & "\n"
    try:
      discard httpGet(url("/"), headers"Content-Length: 5\r\n")
      log.add "[7] no error\n"
    except ErrorCode as e:
      log.add "[7] reserved header: " & $e & "\n"
    log.add "[8] " & h.get("x-trace") & "\n"
  except ErrorCode as e:
    log.add "CLIENT FAILED: " & $e & "\n"
  atomicStore(clientDone, 1, moRelease)

proc main =
  initIoRing()
  gServer = 5           # one connection per request that reaches the server
  submit(delay(serverChain()), 0)
  let start = monoNow()
  while atomicLoad(listening, moAcquire) == 0:
    if millisUntil(monoNow(), start) > 5000: break
  assert atomicLoad(listening, moAcquire) == 1, "the listener never came up"
  submit(delay(clientChain()), 1)
  awaitFlag(clientDone)
  awaitFlag(serverDone)
  stdout.write log

main()

{.feature: "assumeSync".}  # test program: globals shared freely
