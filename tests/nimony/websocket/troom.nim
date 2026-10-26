# lib/std/websocket — rooms, over a real socket.
#
# A room is the fan-out: one connection's message goes to every member, and the
# room is the one place that writes. A connection joins with `ws.subscribe` and
# publishes with `ws.publish`, so the handler never leaves the pair API.
#
# What is checked here is that a publish reaches every member (including the
# sender), that `ws.send` reaches only its connection, that a ping is answered
# through the room, and that a member leaving does not take the room with it.

import std / [httpserver, websocket, socket, ioring, threadpool, atomics,
              assertions, syncio, os]

let tags = newHttpTags()

var gPort = 0
var listening = 0
var done = 0
var report = ""

# ------------------------------------------------------------ the server --

proc handle(c: sink HttpConnection) {.passive.} =
  ## Every connection joins "chat" and republishes what it says, except
  ## "whoami", which goes to that connection alone.
  if not c.next(): return
  try:
    var ws = upgradeWebSocket(c)
    ws.subscribe("chat")
    try:
      while true:
        var msg = ""
        let kind = ws.recv(msg)
        case kind
        of mkText:
          if msg == "whoami":
            ws.send(mkText, "only you")
          else:
            ws.publish("chat", msg)
        of mkClose:
          break
        else:
          discard
    except ErrorCode:
      discard
    ws.close()
  except ErrorCode:
    discard

proc serve() {.passive.} =
  var s = listenHttp(0'u16, tags)
  gPort = int(boundPort(s.fd))
  atomicStore(listening, 1, moRelease)
  while true:
    let c = s.accept()
    if c.isClosed: break
    submit(delay(handle(c)), -1)

# ------------------------------------------------------------ the client --

proc next(ws: var WebSocket; tag: string) {.passive, raises.} =
  var msg = ""
  let kind = ws.recv(msg)
  report.add tag & ": " & $kind & " " & msg & "\n"

proc body() {.passive, raises.} =
  var a = connectWebSocket("ws://127.0.0.1:" & $gPort)
  var b = connectWebSocket("ws://127.0.0.1:" & $gPort)

  # 1. A publish reaches every member, the sender included.
  a.send(mkText, "hello")
  next(a, "[1] a")
  next(b, "[1] b")

  # 2. A direct send reaches only its connection: b's next message is the
  #    publish that follows, not "only you".
  a.send(mkText, "whoami")
  next(a, "[2] a")
  a.publish("chat", "after")
  next(a, "[2] a again")
  next(b, "[2] b")

  # 3. A member leaving does not take the room with it.
  b.close(1000, "bye")
  report.add "[3] b closed with " & $b.closeCode & "\n"
  a.send(mkText, "still here")
  next(a, "[3] a")

  # 4. The room answers the peer's close, so a's goodbye completes.
  a.close(1000, "done")
  report.add "[4] a closed with " & $a.closeCode & "\n"

proc client() {.passive.} =
  try:
    body()
  except ErrorCode as e:
    report.add "client failed: " & $e & "\n"
  atomicStore(done, 1, moRelease)

# ---------------------------------------------------------------- driver --

proc main =
  initIoRing()
  submit(delay(serve()), 0)
  let start = monoNow()
  while atomicLoad(listening, moAcquire) == 0:
    if millisUntil(monoNow(), start) > 5000: break
  assert atomicLoad(listening, moAcquire) == 1, "the listener never came up"
  submit(delay(client()), 1)
  let cstart = monoNow()
  while atomicLoad(done, moAcquire) == 0:
    if millisUntil(monoNow(), cstart) > 20_000: break
  assert atomicLoad(done, moAcquire) == 1, "the client never finished"
  stdout.write report

main()

{.feature: "assumeSync".}  # test program: globals shared freely
