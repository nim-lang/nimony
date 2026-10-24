# lib/std/websocket — the server, over a real socket, against a peer that
# speaks the wire itself.
#
# The peer is `peer/wspeer`, which builds every frame byte by byte from the
# RFC and shares no code with the server: a client built from the same frame
# codec would agree with the server about exactly the thing being tested.
#
# What is checked here is the protocol, not the plumbing: the handshake's
# answer, the three length forms, the fragments put back together, the ping
# answered while a message is half through, and — the part that is easy to get
# wrong — what the server does when the peer breaks a rule. A server that
# only ever meets a well-behaved client has not been tested at all.
#
# The cases run one after the other, each in a chain of its own, so the report
# reads in the order the cases ran, and every one of them is a connection of
# its own.
#
# The close codes a suite sees are the ones of RFC 6455 §7.4.1: 1002 for a
# peer that broke the protocol, 1007 for text that is not text, 1009 for a
# message or a frame bigger than the endpoint accepts.

import std / [httpserver, websocket, socket, ioring, threadpool, atomics,
              assertions, syncio]
import peer/wspeer

let tags = newHttpTags()

var serverDone: int = 0
var clientsDone: int = 0
var handlerDone: int = 0
var gPort: int = 0
var listening: int = 0
var report: array[29, string]
  ## One slot per case, written only by that case's task and read after all of
  ## them have finished. That is why there is no lock: the cases run in
  ## parallel, but no two of them share a slot. The report is printed in index
  ## order at the end, so the order the cases finish in does not matter.
var slog = ""

proc awaitFlag(flag: var int; name: string) =
  ## A wall-clock budget, not a spin count — what these chains wait on is
  ## time, and a spin count is only a proxy for it.
  const BudgetMs = 20_000
  let start = monoNow()
  while atomicLoad(flag, moAcquire) == 0:
    if millisUntil(monoNow(), start) > BudgetMs: break
  assert atomicLoad(flag, moAcquire) == 1, "the chain " & name & " never finished"
  atomicStore(flag, 0, moRelease)

proc awaitCount(count: var int; expected: int; name: string) =
  ## The same budget for a counter rather than a flag. The cases run in
  ## parallel, so the driver has one finish line instead of one per case.
  const BudgetMs = 20_000
  let start = monoNow()
  while atomicLoad(count, moAcquire) < expected:
    if millisUntil(monoNow(), start) > BudgetMs: break
  assert atomicLoad(count, moAcquire) >= expected,
         "only " & $atomicLoad(count, moAcquire) & " of " & $expected &
         " " & name & " finished"

# ------------------------------------------------------------ the server --

proc echoLoop(ws: var WebSocket) {.passive, raises.} =
  ## Everything the peer says comes back, until the peer says goodbye.
  while true:
    var msg = ""
    let kind = ws.recv(msg)
    case kind
    of mkText:
      ws.send(mkText, msg)
    of mkBinary:
      ws.send(mkBinary, msg)
    of mkPong:
      ws.send(mkPong, msg)
    of mkClose:
      break
    of mkPing:
      # A ping never gets here: the connection answers it by itself.
      ws.send(mkPing, msg)

proc handle(c: sink HttpConnection) {.passive.} =
  ## One connection: the path says what this server does with it.
  if not c.next(): return
  try:
    if c.path == "/close":
      var ws = upgradeWebSocket(c)
      ws.close(1000, "bye")
    elif c.path == "/small":
      var ws = upgradeWebSocket(c, [], 64)
      echoLoop(ws)
    elif c.path == "/origin":
      # The origin check belongs to the handler, not to the protocol: the
      # request is refused before the upgrade, with an ordinary HTTP answer.
      if c.requestHeader("Origin") != "https://allowed.example":
        c.respond(403, "this origin is not welcome\n")
        return
      var ws = upgradeWebSocket(c)
      echoLoop(ws)
    else:
      var ws = upgradeWebSocket(c)
      echoLoop(ws)
  except ErrorCode as e:
    # Only the hang-up case is reported: the other failures are the ones the
    # peer is meant to see, and the peer has already seen them.
    if c.path == "/hangup":
      slog.add "the server saw the hang-up: " & $e & "\n"
      atomicStore(handlerDone, 1, moRelease)

var gServer = 0
  ## How many connections this run still expects.

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

proc say(n: int; s: string) =
  ## One line of the report, into the slot of case `n`. The cases run in
  ## parallel, but each is its own task and writes only its own slot, so
  ## there is nothing to lock. The report is printed in index order, so it
  ## does not depend on which pool worker got there first.
  if n > 0 and n < report.len:
    report[n].add "[" & $n & "] "
    report[n].add s
    report[n].add "\n"

proc containsText(hay: string; needle: string): bool =
  ## Whether `hay` holds `needle`.
  if needle.len == 0: return true
  var i = 0
  while i + needle.len <= hay.len:
    var same = true
    for k in 0 ..< needle.len:
      if hay[i + k] != needle[k]:
        same = false
        break
    if same: return true
    inc i
  result = false

proc handshake(path: string; extra = ""): string =
  ## A well-formed handshake request.
  result = "GET " & path & " HTTP/1.1\r\n"
  result.add "Host: x\r\n"
  result.add "Upgrade: websocket\r\n"
  result.add "Connection: Upgrade\r\n"
  result.add "Sec-WebSocket-Version: 13\r\n"
  result.add "Sec-WebSocket-Key: " & DefaultKey & "\r\n"
  result.add extra
  result.add "\r\n"

proc closePayload(code: int): string =
  ## The payload of a close frame with the code `code`.
  result = newString(2)
  result[0] = chr((code shr 8) and 0xFF)
  result[1] = chr(code and 0xFF)

proc textFrame(payload: string): WsFrameSpec =
  ## A masked text frame.
  result = frame(payload = payload)

proc case1() {.passive, raises.} =
  ## The handshake, and the answer to it. The key is the RFC's own, so the
  ## accept value is the one RFC 6455 §1.3 spells out.
  var p = connectWs(gPort, handshake("/echo"))
  let (status, head) = p.readHead()
  say(1, "the handshake: " & $status)
  say(1, "the accept value of RFC 6455: " &
      $containsText(head, "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo="))
  p.closePeer()

proc case2() {.passive, raises.} =
  ## One text message, echoed back.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(textFrame("hello"))
  let f = p.readFrame()
  say(2, "the echo: " & f.payload)
  p.closePeer()

proc case3() {.passive, raises.} =
  ## A message bigger than 64 KiB: the length has to be spelled in the wide
  ## form, and the payload has to come back whole.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  var big = newString(70000)
  for i in 0 ..< big.len: big[i] = chr(i and 0x7F)
  p.send(frame(opcode = 0x2, payload = big))
  let f = p.readFrame()
  say(3, "70000 bytes echoed: " & $(f.payload == big))
  p.closePeer()

proc case4() {.passive, raises.} =
  ## Many small messages: the frames of one connection may not run into each
  ## other.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  var ok = true
  for i in 0 ..< 50:
    p.send(textFrame("m" & $i))
    let f = p.readFrame()
    if f.payload != "m" & $i: ok = false
  say(4, "50 messages echoed: " & $ok)
  p.closePeer()

proc case5() {.passive, raises.} =
  ## A ping is answered with a pong, and the answer carries the same bytes.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(opcode = 0x9, payload = "ping"))
  let f = p.readFrame()
  say(5, "the pong: " & $(f.opcode == 0xA) & " " & f.payload)
  p.closePeer()

proc case6() {.passive, raises.} =
  ## A message in three fragments.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(fin = false, opcode = 0x1, payload = "one "))
  p.send(frame(fin = false, opcode = 0x0, payload = "two "))
  p.send(frame(fin = true, opcode = 0x0, payload = "three"))
  let f = p.readFrame()
  say(6, "three fragments, put back together: " & f.payload)
  p.closePeer()

proc case7() {.passive, raises.} =
  ## A ping in the middle of a message: the ping is answered, and the
  ## message is put back together anyway.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(fin = false, opcode = 0x1, payload = "one "))
  p.send(frame(opcode = 0x9, payload = "ping"))
  p.send(frame(fin = true, opcode = 0x0, payload = "two"))
  let pong = p.readFrame()
  let msg = p.readFrame()
  say(7, "the pong came first: " & $(pong.opcode == 0xA) &
      ", the message: " & msg.payload)
  p.closePeer()

proc case8() {.passive, raises.} =
  ## The peer says goodbye: the answer carries the same code.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(opcode = 0x8, payload = closePayload(1000)))
  let f = p.readFrame()
  say(8, "the close is echoed: " & $closeCode(f))
  p.closePeer()

proc case9() {.passive, raises.} =
  ## The server says goodbye: the peer is answered, so that the server's
  ## waiting ends.
  var p = connectWs(gPort, handshake("/close"))
  discard p.readHead()
  let f = p.readFrame()
  say(9, "the server closes: " & $closeCode(f))
  p.send(frame(opcode = 0x8, payload = closePayload(1000)))
  p.closePeer()

proc case10() {.passive, raises.} =
  ## The peer hangs up without saying goodbye. The server has to notice, and
  ## it has to say so rather than end the connection quietly; the report of
  ## that appears at the end, from the handler's side.
  var p = connectWs(gPort, handshake("/hangup"))
  discard p.readHead()
  p.closePeer()

proc case11() {.passive, raises.} =
  ## A version this server does not speak: the answer says which one it does.
  var p = connectWs(gPort,
    "GET /echo HTTP/1.1\r\nHost: x\r\nUpgrade: websocket\r\n" &
    "Connection: Upgrade\r\nSec-WebSocket-Version: 8\r\n" &
    "Sec-WebSocket-Key: " & DefaultKey & "\r\n\r\n")
  let (status, head) = p.readHead()
  say(11, "a wrong version: " & $status & ", and the one to use: " &
      $containsText(head, "Sec-WebSocket-Version: 13"))
  p.closePeer()

proc case12() {.passive, raises.} =
  ## No key at all: there is no handshake without one.
  var p = connectWs(gPort,
    "GET /echo HTTP/1.1\r\nHost: x\r\nUpgrade: websocket\r\n" &
    "Connection: Upgrade\r\nSec-WebSocket-Version: 13\r\n\r\n")
  let (status, _) = p.readHead()
  say(12, "no key: " & $status)
  p.closePeer()

proc case13() {.passive, raises.} =
  ## Not a GET: a WebSocket handshake is a GET.
  var p = connectWs(gPort,
    "POST /echo HTTP/1.1\r\nHost: x\r\nUpgrade: websocket\r\n" &
    "Connection: Upgrade\r\nSec-WebSocket-Version: 13\r\n" &
    "Sec-WebSocket-Key: " & DefaultKey & "\r\n\r\n")
  let (status, _) = p.readHead()
  say(13, "not a GET: " & $status)
  p.closePeer()

proc case14() {.passive, raises.} =
  ## A frame that is not masked: a client has to mask, and one that does not
  ## is not speaking this protocol.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(payload = "hello", mask = false))
  let f = p.readFrame()
  say(14, "an unmasked frame: " & $closeCode(f))
  p.closePeer()

proc case15() {.passive, raises.} =
  ## Text that is not text.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(payload = "\xff\xfe"))
  let f = p.readFrame()
  say(15, "text that is not text: " & $closeCode(f))
  p.closePeer()

proc case16() {.passive, raises.} =
  ## A message bigger than the server's limit, which the server announced in
  ## the handshake it would not take.
  var p = connectWs(gPort, handshake("/small"))
  discard p.readHead()
  p.send(textFrame(newString(100)))
  let f = p.readFrame()
  say(16, "a message that is too big: " & $closeCode(f))
  p.closePeer()

proc case17() {.passive, raises.} =
  ## An opcode nobody defined.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(opcode = 0x3, payload = "x"))
  let f = p.readFrame()
  say(17, "a reserved opcode: " & $closeCode(f))
  p.closePeer()

proc case18() {.passive, raises.} =
  ## A reserved bit set: the bits belong to the extensions, and this server
  ## speaks none.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(rsv = 1, payload = "x"))
  let f = p.readFrame()
  say(18, "a reserved bit: " & $closeCode(f))
  p.closePeer()

proc case19() {.passive, raises.} =
  ## An origin the server does not want. The check is the handler's, and it
  ## happens before the upgrade: the answer is an ordinary HTTP answer.
  var p = connectWs(gPort,
    handshake("/origin", "Origin: https://somewhere.example\r\n"))
  let (status, _) = p.readHead()
  say(19, "an origin that is not welcome: " & $status)
  p.closePeer()

proc case20() {.passive, raises.} =
  ## A length in 64 bits for a payload the short form would hold. Both are
  ## readable, and only one is what the RFC asks for; a peer that picks the
  ## other one is refused rather than accommodated.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(payload = "hi", wide = true))
  let f = p.readFrame()
  say(20, "a 64 bit length for two bytes: " & $closeCode(f))
  p.closePeer()

proc case21() {.passive, raises.} =
  ## A close code that may never go on the wire: 1005 is what a peer sees
  ## when no code was sent, and sending it would be a lie about what
  ## happened.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(opcode = 0x8, payload = closePayload(1005)))
  let f = p.readFrame()
  say(21, "the close code 1005: " & $closeCode(f))
  p.closePeer()

proc case22() {.passive, raises.} =
  ## A control frame longer than 125 bytes: it has to be readable without
  ## reading the message it may sit in the middle of.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(opcode = 0x9, payload = newString(126)))
  let f = p.readFrame()
  say(22, "a ping of 126 bytes: " & $closeCode(f))
  p.closePeer()

proc case23() {.passive, raises.} =
  ## A control frame that is fragmented: a ping is one frame, always.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(fin = false, opcode = 0x9, payload = "x"))
  let f = p.readFrame()
  say(23, "a fragmented ping: " & $closeCode(f))
  p.closePeer()

proc case24() {.passive, raises.} =
  ## A continuation frame with no message to continue.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(opcode = 0x0, payload = "x"))
  let f = p.readFrame()
  say(24, "a continuation out of nowhere: " & $closeCode(f))
  p.closePeer()

proc case25() {.passive, raises.} =
  ## A UTF-8 sequence split across two fragments: the fragments are bytes,
  ## the message is text, and the check belongs to the message.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(fin = false, opcode = 0x1, payload = "\xc3"))
  p.send(frame(fin = true, opcode = 0x0, payload = "\xa9"))
  let f = p.readFrame()
  say(25, "text split mid-sequence: " & $(f.payload == "\xc3\xa9") &
      " (" & $f.payload.len & " bytes)")
  p.closePeer()

proc case26() {.passive, raises.} =
  ## ... and the other way round: fragments that are fine one by one and not
  ## text once they are one message.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(fin = false, opcode = 0x1, payload = "\xc3"))
  p.send(frame(fin = true, opcode = 0x0, payload = "\x28"))
  let f = p.readFrame()
  say(26, "text that is not text once assembled: " & $closeCode(f))
  p.closePeer()

proc case27() {.passive, raises.} =
  ## A frame that announces two megabytes and sends nothing: the refusal has
  ## to come from the header, before any of it is read — a peer must not be
  ## able to make the server allocate by announcing a length.
  var p = connectWs(gPort, handshake("/echo"))
  discard p.readHead()
  p.send(frame(opcode = 0x2, payload = "", lengthField = 2_000_000))
  let f = p.readFrame()
  say(27, "a frame that claims two megabytes: " & $closeCode(f))
  p.closePeer()

proc case28() {.passive, raises.} =
  ## HTTP/1.0: the upgrade is an HTTP/1.1 request, and the answer says so.
  var p = connectWs(gPort,
    "GET /echo HTTP/1.0\r\nHost: x\r\nUpgrade: websocket\r\n" &
    "Connection: Upgrade\r\nSec-WebSocket-Version: 13\r\n" &
    "Sec-WebSocket-Key: " & DefaultKey & "\r\n\r\n")
  let (status, _) = p.readHead()
  say(28, "HTTP/1.0: " & $status)
  p.closePeer()

proc runCase(n: int) {.passive, raises.} =
  ## The case `n`.
  case n
  of 1: case1()
  of 2: case2()
  of 3: case3()
  of 4: case4()
  of 5: case5()
  of 6: case6()
  of 7: case7()
  of 8: case8()
  of 9: case9()
  of 10: case10()
  of 11: case11()
  of 12: case12()
  of 13: case13()
  of 14: case14()
  of 15: case15()
  of 16: case16()
  of 17: case17()
  of 18: case18()
  of 19: case19()
  of 20: case20()
  of 21: case21()
  of 22: case22()
  of 23: case23()
  of 24: case24()
  of 25: case25()
  of 26: case26()
  of 27: case27()
  of 28: case28()
  else: discard

proc clientCase(n: int) {.passive.} =
  ## One case, in a chain of its own. The cases run in parallel; each writes
  ## only its own report slot.
  try:
    runCase(n)
  except ErrorCode as e:
    say(n, "failed: " & $e)
  atomicInc(clientsDone)

# ---------------------------------------------------------------- driver --

proc main =
  initIoRing()
  gServer = 28
  submit(delay(serverChain()), 0)
  let start = monoNow()
  while atomicLoad(listening, moAcquire) == 0:
    if millisUntil(monoNow(), start) > 5000: break
  assert atomicLoad(listening, moAcquire) == 1, "the listener never came up"

  # All cases at once: they are independent connections, and the report is
  # sorted by index at the end rather than by who finished first.
  for n in 1 .. 28:
    submit(delay(clientCase(n)), 1)
  awaitCount(clientsDone, 28, "cases")
  # The server notices the hang-up after the client is gone; the report has
  # to wait for it.
  awaitFlag(handlerDone, "handler")
  awaitFlag(serverDone, "server")
  for i in 1 .. 28:
    stdout.write report[i]
  stdout.write slog

main()

{.feature: "assumeSync".}  # test program: globals shared freely
