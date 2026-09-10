# lib/std/httpserver — the protocol rules a handler must not have to know.
#
# Every case here is one of the six things a server written straight on
# `httpconn` gets wrong on the *second* request, so each one is checked over a
# real socket, through the ring, with the server and the client each a
# `.passive` chain resumed by a pool worker.
#
# One transcript for every platform: the listener is `listenHttp(0)` on
# loopback, so the Windows arm (the IOCP proactor) is driven through the same
# nine cases as the io_uring and epoll ones and has to answer them the same.

import std / [httpserver, socket, ioring, threadpool, atomics, assertions,
              syncio]

let tags = newHttpTags()

var serverDone: int = 0
var clientDone: int = 0
var gPort: int = 0
var listening: int = 0
var log = ""

proc awaitFlag(flag: var int) =
  ## A wall-clock budget, not a spin count — what these chains wait on is
  ## time, and a spin count is only a proxy for it. (Same reasoning as
  ## `tconn.nim`.)
  const BudgetMs = 20_000
  let start = monoNow()
  while atomicLoad(flag, moAcquire) == 0:
    if millisUntil(monoNow(), start) > BudgetMs: break
  assert atomicLoad(flag, moAcquire) == 1, "a passive chain never finished"
  atomicStore(flag, 0, moRelease)

# ------------------------------------------------------------ the server --

proc handle(c: sink HttpConnection) {.passive.} =
  ## `sink`, and not `var`: this chain outlives the `accept` loop that spawned
  ## it, so a `var` parameter would leave the frame pointing at a local the
  ## next iteration overwrites. A `sink` parameter is stored in the frame and
  ## is mutable there, which is what `next`/`respond` need.
  while c.next():
    try:
      if c.path == "/":
        c.respond(200, "hello\n", "text/plain")
      elif c.path == "/ignore":
        # Deliberately does NOT read the body. The driver has to drain it,
        # or the next request line is read out of the middle of it.
        c.respond(200, "ignored\n", "text/plain")
      elif c.path == "/empty":
        c.respond(204, "")
      elif c.path == "/stream":
        c.beginStream(200, "text/plain")
        c.write("one ")
        c.write("two ")
        c.write("three")
        c.finish()
      else:
        c.respond(404, "no\n", "text/plain")
    except ErrorCode:
      c.close()

var gServer = 0
  ## How many connections this run still expects. The accept loop is not
  ## infinite here: the test has to end, and `close` on the listener is what
  ## ends it.

proc serverChain() {.passive.} =
  var s = listenHttp(0'u16, tags)     # port 0: the kernel picks, nothing collides
  s.serverName = "t"
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

proc scrub(s: string): string =
  ## `Date` names the second the test ran in, so it cannot be in the
  ## expected output. Everything else about the line is checked — that it is
  ## present, and where — by replacing only what varies.
  result = ""
  var i = 0
  while i < s.len:
    if (i == 0 or s[i - 1] == '\n') and i + 6 <= s.len and
       s[i] == 'd' and s[i+1] == 'a' and s[i+2] == 't' and s[i+3] == 'e' and
       s[i+4] == ':':
      result.add "date: <when>"
      while i < s.len and s[i] != '\n': inc i
    else:
      if s[i] == '\r': discard
      else: result.add s[i]
      inc i

proc exchange(request: string): string {.passive.} =
  ## One connection: send `request` verbatim, read until the peer closes.
  ## Raw bytes on purpose — a client built from the same message layer would
  ## agree with the server about a mistake they both make.
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
    result = "CONNECT FAILED " & $r
    closeFd(cint(fd))
    return
  var sock = initSocket(cint(fd), afterMs(5000))
  try:
    sock.write(toOpenArray(request, 0, request.len - 1))
    while true:
      if sock.fill() == 0: break
    let n = sock.buffered
    for i in 0..<n: result.add sock.peek[i]
    sock.consume n
  except ErrorCode as e:
    result.add "\nCLIENT " & $e

proc case1() {.passive.} =
  log.add "[1] " & scrub(exchange(
    "GET / HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n")) & "\n"
  atomicStore(clientDone, 1, moRelease)

proc case2() {.passive.} =
  log.add "[2] " & scrub(exchange(
    "HEAD / HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n")) & "END\n"
  atomicStore(clientDone, 1, moRelease)

proc case3() {.passive.} =
  # Two requests down one connection, and the first sends a body the handler
  # never reads. Without the drain, the second is parsed out of "HELLO".
  log.add "[3] " & scrub(exchange(
    "POST /ignore HTTP/1.1\r\nHost: x\r\nContent-Length: 5\r\n\r\nHELLO" &
    "GET / HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n")) & "\n"
  atomicStore(clientDone, 1, moRelease)

proc case4() {.passive.} =
  log.add "[4] " & scrub(exchange("GET /\x01 HTTP/9\r\n\r\n")) & "\n"
  atomicStore(clientDone, 1, moRelease)

proc case5() {.passive.} =
  log.add "[5] " & scrub(exchange("GET / HTTP/1.1\r\n\r\n")) & "\n"
  atomicStore(clientDone, 1, moRelease)

proc case6() {.passive.} =
  log.add "[6] " & scrub(exchange(
    "GET /empty HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n")) & "END\n"
  atomicStore(clientDone, 1, moRelease)

proc case7() {.passive.} =
  # The four spellings of the same attack. The last two only fail on a
  # server that decodes before it normalizes.
  var acc = ""
  for t in ["/../etc/passwd", "/a/../../etc/passwd", "/%2e%2e/etc/passwd",
            "/a/%2e%2e%2f%2e%2e%2fetc/passwd", "/a/./b", "/a//b"]:
    let r = exchange("GET " & t & " HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n")
    var status = ""
    for ch in r:
      if ch == '\r' or ch == '\n': break
      status.add ch
    acc.add "  " & t & " -> " & status & "\n"
  log.add "[7]\n" & acc
  atomicStore(clientDone, 1, moRelease)

proc case8() {.passive.} =
  log.add "[8] " & scrub(exchange(
    "GET /stream HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n")) & "\n"
  atomicStore(clientDone, 1, moRelease)

proc case9() {.passive.} =
  # Keep-alive for real: five requests, one connection, one chain.
  var req = ""
  for i in 0..3: req.add "GET / HTTP/1.1\r\nHost: x\r\n\r\n"
  req.add "GET / HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n"
  let r = exchange(req)
  var n = 0
  for i in 0..<r.len:
    if (i == 0 or r[i-1] == '\n') and i + 8 <= r.len and
       r[i] == 'H' and r[i+1] == 'T' and r[i+2] == 'T' and r[i+3] == 'P': inc n
  log.add "[9] 5 requests on one connection -> " & $n & " responses\n"
  atomicStore(clientDone, 1, moRelease)

# ---------------------------------------------------------------- driver --

proc main =
  initIoRing()
  gServer = 14        # 8 single-connection cases + 6 targets in case7
  submit(delay(serverChain()), 0)
  let start = monoNow()
  while atomicLoad(listening, moAcquire) == 0:
    if millisUntil(monoNow(), start) > 5000: break
  assert atomicLoad(listening, moAcquire) == 1, "the listener never came up"

  submit(delay(case1()), 1); awaitFlag(clientDone)
  submit(delay(case2()), 1); awaitFlag(clientDone)
  submit(delay(case3()), 1); awaitFlag(clientDone)
  submit(delay(case4()), 1); awaitFlag(clientDone)
  submit(delay(case5()), 1); awaitFlag(clientDone)
  submit(delay(case6()), 1); awaitFlag(clientDone)
  submit(delay(case7()), 1); awaitFlag(clientDone)
  submit(delay(case8()), 1); awaitFlag(clientDone)
  submit(delay(case9()), 1); awaitFlag(clientDone)
  awaitFlag(serverDone)
  stdout.write log

main()
