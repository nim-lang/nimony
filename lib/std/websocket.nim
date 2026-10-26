# (c) 2026 Andreas Rumpf
#
# WebSockets: RFC 6455 on top of `std/httpserver`, and a client for it.
#
#   import std/[httpserver, websocket]
#
#   let tags = newHttpTags()
#
#   proc handle(c: sink HttpConnection) {.passive.} =
#     if not c.next(): return
#     try:
#       var ws = upgradeWebSocket(c)
#       while true:
#         var msg = ""
#         if ws.recv(msg) == mkClose: break
#         ws.send(mkText, msg)
#     except ErrorCode:
#       discard
#
# The client side is the same object:
#
#   var ws = connectWebSocket("ws://example.com/chat")
#   ws.send(mkText, "hello")
#   var msg = ""
#   echo ws.recv(msg)
#
# ## What the upgrade changes
#
# RFC 6455 is HTTP's request for a different protocol, and the answer is not
# "the connection is now a stream". Three things change at the upgrade, and a
# server that gets one of them wrong looks like it works until the peer sends
# the case that breaks it.
#
# * **The handshake.** The request has to be a WebSocket request: `GET`, an
#   `Upgrade: websocket`, a `Connection` that says `Upgrade`, a version this
#   server speaks and a key. Each of those has a right answer, and the wrong
#   one is a status code rather than a closed connection — 400 for a request
#   that is not a WebSocket request, 405 for the wrong method, 426 for a
#   version this server does not speak, with the version it does speak in the
#   answer so that the peer can try again with it.
# * **The framing.** From the 101 on, the bytes are frames and not HTTP: a
#   header of two bytes, a length, a mask and a payload. The HTTP layer is
#   gone — it has no idea what a frame is, and it must not be asked.
# * **The closing handshake.** A WebSocket does not end when the socket
#   ends; it ends when both sides have said goodbye, and a peer that vanishes
#   without saying so has left the connection in a state that has to be
#   reported rather than papered over.
#
# ## The shape of the thing
#
# `WebSocket` is a pair: a `WebSocketReader` and a `WebSocketWriter`. The
# reader owns the `Reader`, parses frames, reassembles fragmented messages and
# checks that text is text; the writer owns the `Writer`, frames messages,
# masks them when this end is the client and sends the close frame. The pair
# owns what belongs to both: the subprotocol, the closing handshake and the
# error. It is move-only, because a connection with two owners is a connection
# with two writers.
#
# The streams are the ones the HTTP layer was already using:
# `HttpConnection.takeStreams` hands them over with their buffers, so a client
# that sent its first frame right behind the handshake request has it read
# already and it is not lost between the two protocols. The framing lives in
# this module and not in the stream layer's codec slot: a codec transforms a
# byte stream, and a WebSocket is not a byte stream, it is a sequence of
# messages with rules about which of them may be interrupted by which.
#
# The connection has one owner and this module is it: the pair's `recv`
# answers a ping with a pong by itself, and the closing handshake starts on
# whichever side did not ask for it. A handler that writes to the connection
# from another task while `recv` is running interleaves two frames in the
# middle of each other. There is one writer, so there is one place that
# writes.
#
# ## The two roles
#
# A client masks every frame it sends and a server masks none; a server
# rejects an unmasked frame from a client, and a client rejects a masked one
# from a server. That is the only difference between the roles — the framing,
# the fragmentation and the closing handshake are the same code, which is why
# both halves carry a `role` field rather than there being two of each type.

import std / [ioring, base64, sha1, unicode, uri, strutils, streamio,
              channels, threadpool, atomics, ticketlocks]
import std / httpserver except add, close
import std / http/[httpheaders, httpparse]
import std / syncio
import std / crypto/sysrand

const
  WsGuid* = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
    ## The GUID of RFC 6455 §4.2.2, appended to the key to make the accept.
  WsVersion* = "13"
    ## The only version this module speaks. RFC 6455 has never had another.
  DefaultMaxMessage* = 1024 * 1024
    ## The largest message accepted, in bytes. A message is assembled before
    ## it is handed out, so this is also the most memory one peer can make the
    ## other spend on a message.
  DefaultHandshakeMs* = 10_000
    ## How long the client gives the server to answer the handshake.
  DefaultCloseMs* = 5_000
    ## How long a side waits for the peer's half of the closing handshake.

  MaxBufferedFrame = 32 * 1024
    ## The largest frame that is copied into the stream's buffer whole. A
    ## frame costs one `copyMem` more that way and one allocation less; past
    ## this size the copy is the bigger of the two, so the payload goes to the
    ## kernel straight from the caller's memory instead.

  TokenSeparators = {',', ' ', '\t'}
    ## What may sit between the tokens of a comma-separated header value.
  TokenWhitespace = {' ', '\t'}
    ## What is trimmed from the end of such a token.

  # RFC 6455 §7.4.1, the codes that mean something to the protocol.
  wscNormal* = 1000
    ## The connection did what it was for.
  wscGoingAway* = 1001
    ## One side is going away: a server shutting down, a page unloading.
  wscProtocolError* = 1002
    ## The peer broke the protocol.
  wscUnsupported* = 1003
    ## A message of a type this endpoint does not take.
  wscNoStatus* = 1005
    ## Never sent: what a peer sees when no code was sent at all.
  wscAbnormal* = 1006
    ## Never sent: what a peer sees when the connection went away without a
    ## closing handshake.
  wscInvalidPayload* = 1007
    ## A text message that was not UTF-8.
  wscPolicy* = 1008
    ## The message was understood and refused.
  wscTooBig* = 1009
    ## A frame or message was larger than the endpoint accepts.
  wscExtension* = 1010
    ## The client wanted an extension the server did not agree to.
  wscInternalError* = 1011
    ## The endpoint hit a condition of its own that it cannot describe.
  wscTlsError* = 1015
    ## Never sent: the TLS handshake failed, which is a local fact.

type
  MsgKind* = enum
    ## What a message is.
    mkText
      ## Text: UTF-8 bytes.
    mkBinary
      ## Binary: bytes.
    mkPing
      ## A ping. Only `send` takes it; `recv` answers a peer's ping by
      ## itself and does not hand it over.
    mkPong
      ## A pong: the answer to a ping.
    mkClose
      ## A close: the end of the conversation.

  WsOpcode* = enum
    ## The opcode of a frame.
    wsoContinuation = 0x0
    wsoText = 0x1
    wsoBinary = 0x2
    wsoClose = 0x8
    wsoPing = 0x9
    wsoPong = 0xA

  WsFrame* = object
    ## The header of one frame.
    fin*: bool
      ## Whether this is the last fragment of the message.
    rsv*: int
      ## The reserved bits. RFC 6455 hands them to the extensions; an endpoint
      ## that speaks none of them has to see them zero.
    opcode*: WsOpcode
    masked*: bool
      ## Whether the payload is masked. A client masks what it sends; a
      ## server does not.
    key*: array[4, char]
      ## The mask key, when the frame carries one.
    len*: int
      ## The length of the payload.

  WsRole = enum
    wrServer, wrClient

  WebSocketReader* = object
    ## The receiving half of one WebSocket connection.
    ##
    ## Move-only: it owns the `Reader`. It parses frames, reassembles
    ## fragmented messages and checks that text is text. Control frames are
    ## handed back to the caller (`mkPing`, `mkPong`, `mkClose`) rather than
    ## answered here: answering a ping needs the writer.
    r: Reader
    role: WsRole
    maxMessage: int
    idleMs: int          ## the budget `recv` gives each message; 0: none
    msgOp: int           ## the opcode of the message being assembled; 0: none
    msgBuf: string       ## its payload so far
    closeSeen: bool
    closeCode: int
    failCode: int        ## the close code to send for the error that stopped us
    err: ErrorCode

  WebSocketWriter* = object
    ## The sending half of one WebSocket connection.
    ##
    ## Move-only, like the reader: it owns the `Writer`, and one connection
    ## has one writer. It frames messages, masks them when this end is the
    ## client, and sends the close frame without waiting for the peer.
    w: Writer
    role: WsRole
    closeSent: bool
    closeCode: int

  WebSocket* = object
    ## One WebSocket connection: a reader, a writer and the state that belongs
    ## to the pair.
    ##
    ## Move-only: the halves own the streams, and two owners of one connection
    ## is two owners of one descriptor. Leaving scope ends the connection — see
    ## the note at the end of this module.
    reader: WebSocketReader
    writer: WebSocketWriter
    sub: string
    err: ErrorCode
    aborted: bool
    room: nil Room
      ## The room this connection joined, if any. A connection in a room has
      ## given its writer to it — `subscribe` moves it — and every write goes
      ## through the room from then on.
    member: int
      ## This connection's id in `room`; `-1` while it is in none.

# ------------------------------------------------------------------- frames --

proc applyMask*(data: var openArray[char]; key: array[4, char]; offset = 0) =
  ## XORs `data` with the mask `key`, starting `offset` bytes into the payload.
  ##
  ## Masking is its own inverse, so this both masks and unmasks: a sender
  ## applies it before the bytes go out, a receiver after they arrive. The
  ## offset is what makes a payload that arrives in pieces come out right —
  ## the key restarts at the front of the frame, not at the front of the read.
  for i in 0 ..< data.len:
    data[i] = char(ord(data[i]) xor ord(key[(offset + i) and 3]))

proc parseFrameHeader*(bytes: openArray[char]; f: var WsFrame): int =
  ## How many bytes the frame header at the front of `bytes` takes,
  ## `ParseIncomplete` while it has not all arrived, `ParseBad` when the
  ## header breaks the protocol.
  ##
  ## The length is not checked against a limit here: a peer announcing a
  ## length is not the same as a peer sending one, and the caller decides how
  ## large a frame it will wait for.
  if bytes.len < 2: return ParseIncomplete
  let b0 = ord(bytes[0])
  let b1 = ord(bytes[1])
  f.fin = (b0 and 0x80) != 0
  f.rsv = (b0 and 0x70) shr 4
  f.masked = (b1 and 0x80) != 0
  var length = b1 and 0x7F
  var ext = 0
  if length == 126:
    if bytes.len < 4: return ParseIncomplete
    length = (ord(bytes[2]) shl 8) or ord(bytes[3])
    if length < 126:
      # RFC 6455 §5.2: the length has to be written in the fewest bytes that
      # hold it. A peer that says "126" for five bytes is not a peer whose
      # library was written badly — it is a peer that is not speaking this
      # protocol, and the two have to be told apart.
      return ParseBad
    ext = 2
  elif length == 127:
    if bytes.len < 10: return ParseIncomplete
    var wide = 0'u64
    for i in 0 ..< 8:
      wide = (wide shl 8) or uint64(ord(bytes[2 + i]))
    if (wide and (1'u64 shl 63)) != 0:
      # The top bit of a 64 bit length is reserved and must be zero.
      return ParseBad
    if wide > uint64(high(int)): return ParseBad
    length = int(wide)
    if length <= 65535:
      # The same rule as above, for the wide form.
      return ParseBad
    ext = 8
  case b0 and 0x0F
  of 0: f.opcode = wsoContinuation
  of 1: f.opcode = wsoText
  of 2: f.opcode = wsoBinary
  of 8: f.opcode = wsoClose
  of 9: f.opcode = wsoPing
  of 10: f.opcode = wsoPong
  else:
    # An opcode nobody defined. The reserved ones are for extensions that
    # were never negotiated, and a peer that uses one is talking a protocol
    # this endpoint does not speak.
    return ParseBad
  if ord(f.opcode) >= 8:
    # A control frame: it may not be fragmented, and it may not be longer
    # than 125 bytes. Both rules exist so that a control frame can be read
    # without reading the whole message it may sit in the middle of.
    if not f.fin or length > 125: return ParseBad
  result = 2 + ext + (if f.masked: 4 else: 0)
  if bytes.len < result: return ParseIncomplete
  if f.masked:
    for i in 0 ..< 4:
      f.key[i] = bytes[2 + ext + i]
  f.len = length

proc headerLen(b1: char): int {.inline.} =
  ## How many bytes the header of a frame takes, from its second byte alone.
  ## That byte holds the mask bit and the length, which is all the header's
  ## size depends on.
  let n = ord(b1) and 0x7F
  result = 2 + (if n == 126: 2 elif n == 127: 8 else: 0)
  if (ord(b1) and 0x80) != 0: result += 4

proc frameHeaderLen(payloadLen: int; masked: bool): int {.inline.} =
  ## How many bytes `fillHeader` writes for a payload of `payloadLen`.
  (if payloadLen < 126: 2 elif payloadLen <= 65535: 4 else: 10) +
    (if masked: 4 else: 0)

proc fillHeader(dest: var openArray[char]; opcode: WsOpcode; payloadLen: int;
                masked: bool; key: array[4, char]; fin: bool): int =
  ## The header of a frame whose payload is `payloadLen` bytes, spelled into
  ## `dest`; returns how many bytes it took. The length goes in the fewest
  ## bytes that hold it, as RFC 6455 §5.2 asks — a peer that does not is a
  ## peer the receiver has to refuse, so both sides write it the same way.
  ##
  ## In place and not as a fresh string: a server that fans one message out to
  ## many connections spells one header per delivery, and an allocation there
  ## is a cost the message does not deserve.
  var i = 0
  dest[i] = chr((if fin: 0x80 else: 0) or ord(opcode))
  inc i
  let maskBit = if masked: 0x80 else: 0
  if payloadLen < 126:
    dest[i] = chr(payloadLen or maskBit)
    inc i
  elif payloadLen <= 65535:
    dest[i] = chr(126 or maskBit)
    inc i
    dest[i] = chr((payloadLen shr 8) and 0xFF)
    inc i
    dest[i] = chr(payloadLen and 0xFF)
    inc i
  else:
    dest[i] = chr(127 or maskBit)
    inc i
    let wide = uint64(payloadLen)
    for k in 0 ..< 8:
      dest[i] = char((wide shr (8 * (7 - k))) and 0xFF)
      inc i
  if masked:
    for k in 0 ..< 4:
      dest[i] = key[k]
      inc i
  result = i

proc fillFrame(dest: var openArray[char]; opcode: WsOpcode;
               payload: openArray[char]; masked: bool; key: array[4, char];
               fin: bool): int =
  ## The whole frame in `dest`, and how many bytes it took. One `copyMem` for
  ## the payload and one pass of masking for a client's frame; nothing is
  ## allocated.
  result = fillHeader(dest, opcode, payload.len, masked, key, fin)
  if payload.len > 0:
    copyMem(addr dest[result], unsafeAddr payload[0], payload.len)
    if masked:
      applyMask(toOpenArray(dest, result, result + payload.len - 1), key)
    result += payload.len

proc frameHeader*(opcode: WsOpcode; payloadLen: int; masked: bool;
                  key: array[4, char]; fin = true): string =
  ## The header of a frame whose payload is `payloadLen` bytes.
  ##
  ## `fillHeader` with a string to put it in: for a caller that needs the
  ## bytes on their own, not as part of a frame.
  result = newString(frameHeaderLen(payloadLen, masked))
  discard fillHeader(toOpenArray(result, 0, result.len - 1), opcode, payloadLen,
                     masked, key, fin)

# ------------------------------------------------------------------ helpers --

proc isValidUtf8(s: openArray[char]): bool =
  ## Whether `s` is well-formed UTF-8.
  ##
  ## Strictly: no overlong forms, no surrogates, nothing above U+10FFFF. A
  ## text message that is not text is a protocol error, and a lenient check
  ## would let a peer smuggle bytes into a string that a program then treats
  ## as characters.
  result = validateUtf8(s) < 0

proc isSendableCloseCode*(code: int): bool =
  ## Whether `code` may go on the wire.
  ##
  ## 1005 and 1006 are what a peer sees when no code was sent, and 1015 is a
  ## local fact about TLS; sending any of them would be a lie about what
  ## happened. 1004 was never defined.
  ##
  ## The rest of 1000 to 2999 belongs to the protocol and is not assigned:
  ## a peer that sends such a code is not speaking RFC 6455 (§7.4.2).
  ##
  ## 3000 to 3999 are for libraries and frameworks to use, 4000 to 4999 for
  ## private use; both are legal on the wire.
  case code
  of 1000..1003, 1007..1014, 3000..4999: true
  else: false

iterator tokens(value: string): string =
  ## The tokens of the comma-separated header `value`, trimmed.
  var i = 0
  while i < value.len:
    while i < value.len and value[i] in TokenSeparators:
      inc i
    if i >= value.len: break
    let start = i
    while i < value.len and value[i] != ',':
      inc i
    var stop = i
    while stop > start and value[stop - 1] in TokenWhitespace:
      dec stop
    yield value.substr(start, stop - 1)

proc hasToken(value: string; token: string): bool =
  ## Whether the comma-separated `value` holds `token`, case-insensitively.
  ##
  ## `Connection: keep-alive, Upgrade` is one header with two tokens in it,
  ## and the order is not fixed.
  result = false
  for t in tokens(value):
    if cmpIgnoreCase(t, token) == 0: return true

proc isValidKey(key: string): bool =
  ## Whether `key` is a nonce as RFC 6455 §4.1 wants it: 16 random bytes,
  ## base64-encoded.
  const Base64Chars = {'A'..'Z', 'a'..'z', '0'..'9', '+', '/', '='}
  if key.len != 24: return false
  for c in key:
    if c notin Base64Chars: return false
  result = true

proc pickProtocol(supported: openArray[string]; offered: string): string =
  ## The first of the offered protocols that this server speaks.
  ##
  ## The server's list is in the server's order, so the server picks — the
  ## client only says what it is willing to speak.
  result = ""
  for want in supported:
    if hasToken(offered, want): return want

proc acceptValue(key: string): string =
  ## The `Sec-WebSocket-Accept` value for `key`.
  ##
  ## The point of it is not secrecy — the GUID is in the RFC — but that the
  ## client has to show that it read the RFC, which is what keeps a caching
  ## proxy from handing a WebSocket answer to a request that never asked for
  ## one.
  var ctx = newSha1State()
  ctx.update(key)
  ctx.update(WsGuid)
  let digest = ctx.finalize()
  var raw = newString(20)
  for i in 0 ..< 20:
    raw[i] = char(digest[i])
  raw.endStore()
  result = encode(raw)

proc statusText(status: int): string =
  case status
  of 400: "Bad Request"
  of 403: "Forbidden"
  of 405: "Method Not Allowed"
  of 426: "Upgrade Required"
  else: "Error"

# ---------------------------------------------------------- the wire itself --

proc closePayload(code: int; reason: string): string =
  ## The payload of a close frame: the code, then the reason. Code `0` is the
  ## close that carried no code, and its answer carries none either: 0 is not
  ## a code that may go on the wire.
  result = ""
  if code != 0:
    # one allocation of the final size, filled in place
    let p = beginStore(result, 2 + reason.len)
    p[0] = char((code shr 8) and 0xFF)
    p[1] = char(code and 0xFF)
    if reason.len > 0: copyMem(addr p[2], readRawData(reason), reason.len)
    result.endStore()

proc avail(rd: WebSocketReader): int {.inline.} =
  ## Protocol bytes buffered and not yet consumed.
  rd.r.buffered

proc take(rd: var WebSocketReader; dest: var openArray[char]): int =
  ## Copies what is buffered into `dest`, consuming it. No I/O, so no deadline.
  result = min(dest.len, rd.r.buffered)
  if result > 0:
    copyMem(addr dest[0], addr rd.r.peek[0], result)
    rd.r.consume result

proc ensure(rd: var WebSocketReader; n: int): bool {.passive, raises.} =
  ## Whether at least `n` protocol bytes are buffered; reads until they are.
  ##
  ## `fill` returns what it *added*, and `0` at the end of the stream even
  ## when unread bytes remain. Asking it again after a short read therefore
  ## cannot make progress: that is the end, and the answer is `false`.
  while rd.avail < n:
    if rd.r.fill() == 0: return false
  result = true

proc deadline*(rd: WebSocketReader): Deadline {.raises.} =
  ## The standing deadline of the receive side; the writer shares it.
  rd.r.deadline

proc renew*(rd: var WebSocketReader; dl: Deadline) {.raises.} =
  ## Replaces the standing deadline of the connection. Raises `BadOperation`
  ## for a closed or default-constructed reader.
  rd.r.renew(dl)

proc close*(rd: var WebSocketReader) =
  ## Drops the receive side without reading anything else.
  try:
    rd.r.close()
  except ErrorCode:
    discard

proc writeProto(wr: var WebSocketWriter; data: openArray[char]) {.
    passive, raises.} =
  ## Puts protocol bytes on the wire and flushes them: a frame written is a
  ## frame handed to the kernel.
  if data.len == 0: return
  wr.w.write(data)
  wr.w.flush()

# --------------------------------------------------------------- the server --

proc refusalText(status: int; body: string; extra: string): string =
  ## A whole refusal response, written by hand.
  ##
  ## `respond` cannot say this: it adds a `Content-Length` and a `Connection`
  ## of its own, and it cannot carry a header nobody registered. The answer
  ## here is short, fixed and written by this module, so nothing a peer sent
  ## can end up in it.
  # The fixed part is about 120 bytes: reserve it all up front and append in
  # place, with no temporary strings for the numbers or the pieces.
  result = newStringOfCap(160 + extra.len + body.len)
  result.add "HTTP/1.1 "
  result.addInt status
  result.add ' '
  result.add statusText(status)
  result.add "\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: "
  result.addInt body.len
  result.add "\r\nConnection: close\r\n"
  result.add extra
  result.add "\r\n"
  result.add body

proc refuse(c: var HttpConnection; status: int; body: string;
            extra: string) {.passive, raises.} =
  ## The handshake is refused: the answer goes out, and the connection ends.
  ##
  ## The connection is taken over for the answer, because the answer is the
  ## last thing that goes over it — after this the HTTP layer has nothing to
  ## say and must not touch the streams again.
  var (r, w) = c.takeStreams()
  try:
    w.write(refusalText(status, body, extra))
    w.flush()
    w.close()
    r.close()
  except ErrorCode:
    # A peer that asked for a WebSocket and left is the ordinary case here;
    # the answer it did not wait for is not worth an exception of its own.
    w.abort()
    try:
      r.close()
    except ErrorCode:
      discard
  if status == 426: raise BadProtocol
  else: raise BadOperation

proc upgradeImpl(c: var HttpConnection; protocols: openArray[string];
                 maxMessage: int; idleMs: int): WebSocket {.
  passive, raises.} =
  ## The handshake, and the connection behind it.
  if c.meth != tag(mGet):
    refuse(c, 405, "websocket: only GET speaks WebSocket\n", "")
  if c.version != tag(tV11):
    refuse(c, 400, "websocket: only HTTP/1.1 speaks WebSocket\n", "")
  if not hasToken(c.requestHeader("Upgrade"), "websocket"):
    refuse(c, 400, "websocket: this is not an upgrade request\n", "")
  if not hasToken(c.requestHeader("Connection"), "upgrade"):
    refuse(c, 400, "websocket: this is not an upgrade request\n", "")
  if c.countRequestHeader("Sec-WebSocket-Version") != 1:
    refuse(c, 426, "websocket: this server speaks version 13\n",
           "Sec-WebSocket-Version: 13\r\n")
  if c.requestHeader("Sec-WebSocket-Version") != WsVersion:
    refuse(c, 426, "websocket: this server speaks version 13\n",
           "Sec-WebSocket-Version: 13\r\n")
  if c.countRequestHeader("Sec-WebSocket-Key") != 1:
    refuse(c, 400, "websocket: exactly one Sec-WebSocket-Key is wanted\n", "")
  let key = c.requestHeader("Sec-WebSocket-Key")
  if not isValidKey(key):
    refuse(c, 400, "websocket: Sec-WebSocket-Key is not a nonce\n", "")
  var sub = ""
  if c.countRequestHeader("Sec-WebSocket-Protocol") == 1:
    sub = pickProtocol(protocols, c.requestHeader("Sec-WebSocket-Protocol"))
  # The handshake succeeded: the connection is not HTTP any more.
  let accept = acceptValue(key)
  var (r, w) = c.takeStreams()
  if idleMs > 0: r.renew(afterMs(idleMs))
  else: r.renew(never)
  var ws = WebSocket()
  ws.room = nil
  ws.member = -1
  ws.reader.r = ensureMove r
  ws.reader.role = wrServer
  ws.reader.maxMessage = maxMessage
  ws.reader.idleMs = idleMs
  ws.writer.w = ensureMove w
  ws.writer.role = wrServer
  ws.sub = sub
  # The 101 goes out before the first frame does, and it says nothing about
  # length: the connection is not a body any more, it is a stream of frames.
  var head = "HTTP/1.1 101 Switching Protocols\r\n"
  head.add "Upgrade: websocket\r\n"
  head.add "Connection: Upgrade\r\n"
  head.add "Sec-WebSocket-Accept: " & accept & "\r\n"
  if sub.len > 0: head.add "Sec-WebSocket-Protocol: " & sub & "\r\n"
  head.add "\r\n"
  ws.writer.writeProto(head)
  result = ws

proc upgradeWebSocket*(c: var HttpConnection; protocols: openArray[string] = [];
                       maxMessage = DefaultMaxMessage; idleMs = 0): WebSocket {.
  passive, raises.} =
  ## The WebSocket behind the request `c`.
  ##
  ## The request has to be a WebSocket request — `GET`, HTTP/1.1, an
  ## `Upgrade: websocket`, a `Connection` that says `Upgrade`, the version 13
  ## and a key — and the answer is the 101 that goes out before this returns.
  ## A request that is not one gets 400 (405 for the wrong method, 426 with
  ## the version this server speaks for a version it does not), and this
  ## raises: there is no falling back to HTTP, because a client that asked
  ## for a WebSocket and got a page has been told something false.
  ##
  ## `protocols` are the subprotocols this server speaks, in the order it
  ## prefers them; the one the client asked for that this server has is
  ## named in the 101.
  ##
  ## `maxMessage` is the largest message the connection accepts, in bytes.
  ## `idleMs` is how long `recv` waits for the next message, counted afresh
  ## for every call; 0 means no limit, which is what a WebSocket usually
  ## wants — a connection with nothing to say is idle, not stuck.
  ##
  ## The `Origin` of the request is not this module's business. A server that
  ## cares about where a connection comes from — the cross-site WebSocket
  ## hijacking of 2010 was about a server that did not — reads the header
  ## itself and answers before the upgrade, because the answer is then an
  ## ordinary HTTP answer:
  ##
  ## ```nim
  ## if not allowed(c.requestHeader("Origin")):
  ##   c.respond(403, "this origin is not welcome\n")
  ##   return
  ## var ws = upgradeWebSocket(c)
  ## ```
  ##
  ## The connection is move-only and has to be closed: leaving scope ends it.
  upgradeImpl(c, protocols, maxMessage, idleMs)

# --------------------------------------------------------------- the client --

proc connectWebSocket*(url: string;
                       headers: HttpHeaders = HttpHeaders("");
                       protocols: openArray[string] = [];
                       maxMessage = DefaultMaxMessage;
                       timeoutMs = DefaultHandshakeMs;
                       idleMs = 0;
                       tls: nil TlsEngine = nil): WebSocket {.
  passive, raises.} =
  ## A WebSocket to `url`, which is `ws://host:port/path` — or `wss://` for
  ## TLS.
  ##
  ## `headers` are extra request headers, in their wire format, and are put on
  ## the handshake unchanged; `protocols` are offered as
  ## `Sec-WebSocket-Protocol`, in this order, and the one the server picks is
  ## `subprotocol`. `timeoutMs` bounds the whole handshake — connect, TLS and
  ## the answer — and `idleMs` is how long `recv` waits for the next
  ## message, counted afresh for every call, 0 for no limit.
  ##
  ## A `wss://` URL needs a TLS engine: `std/tls`'s `newTlsEngine()` is the
  ## one to pass, and this module does not import it, so that a server that
  ## only listens pays for none of it. A `wss://` URL without an engine, or an
  ## engine on a `ws://` URL, is a `BadOperation`: one would speak plaintext
  ## to a TLS port, the other run a handshake the URL never asked for.
  ##
  ## The answer is checked the way RFC 6455 §4.1 says a client has to: the
  ## status is 101, `Upgrade` and `Connection` say what they should, and
  ## `Sec-WebSocket-Accept` is the accept of the key this call made up.
  ## Anything else is a `ProtocolError` (or `BadProtocol` for a status that is
  ## not 101) — a server that does not upgrade did not, and pretending
  ## otherwise turns a protocol mismatch into a stream of nonsense frames.
  let u = parseUri(url)
  if u.hostname.len == 0: raise BadOperation
  let scheme = toLowerAscii(u.scheme)
  if scheme != "ws" and scheme != "wss": raise BadOperation
  let secure = scheme == "wss"
  if secure:
    if tls == nil: raise BadOperation
  elif tls != nil:
    raise BadOperation
  var port = 0
  if u.port.len > 0: port = parseInt(u.port)
  elif secure: port = 443
  else: port = 80
  if port <= 0 or port > 65535: raise BadOperation
  var host = u.hostname
  if u.port.len > 0: host.add ":" & u.port
  var path = if u.path.len > 0: u.path else: "/"
  if u.query.len > 0: path.add "?" & u.query
  # The key is the client's nonce: 16 random bytes, base64. The accept the
  # server has to come back with is derived from it, so a proxy that answers
  # a request it did not understand cannot fake one.
  var raw = default(array[16, byte])
  if not urandom(raw): raise IOError
  var keyRaw = newString(16)
  for i in 0 ..< 16: keyRaw[i] = char(int(raw[i]))
  keyRaw.endStore()
  let key = encode(keyRaw)
  var (r, w) = connect(u.hostname & ":" & $port, tls, afterMs(timeoutMs))
  r.renew(afterMs(timeoutMs))
  var ws = WebSocket()
  ws.room = nil
  ws.member = -1
  ws.reader.r = ensureMove r
  ws.reader.role = wrClient
  ws.reader.maxMessage = maxMessage
  ws.reader.idleMs = idleMs
  ws.writer.w = ensureMove w
  ws.writer.role = wrClient
  var req = "GET " & path & " HTTP/1.1\r\n"
  req.add "Host: " & host & "\r\n"
  req.add "Upgrade: websocket\r\n"
  req.add "Connection: Upgrade\r\n"
  req.add "Sec-WebSocket-Key: " & key & "\r\n"
  req.add "Sec-WebSocket-Version: " & WsVersion & "\r\n"
  if protocols.len > 0:
    req.add "Sec-WebSocket-Protocol: " & join(protocols, ", ") & "\r\n"
  req.add string(headers)
  req.add "\r\n"
  ws.writer.writeProto(req)
  # The response head, and not one byte more: the bytes after it are frames,
  # and a server that sent its first message behind the 101 has put them in
  # the reader's buffer already.
  var scan = default(HeadScanner)
  var headLen = 0
  while true:
    headLen = findHeadEnd(scan, ws.reader.r.peek)
    if headLen == ParseBad: raise ContentTooLong
    if headLen >= 0: break
    if ws.reader.r.fill() == 0: raise EndOfStreamError
  var rawHead = newString(headLen)
  discard ws.reader.take(toOpenArray(rawHead, 0, headLen - 1))
  if not rawHead.endsWith("\r\n\r\n"): raise ProtocolError
  let stop = rawHead.len - 4            # the head lines end before the last CRLF
  let eol = rawHead.find("\r\n")
  if eol < 0 or eol > stop: raise ProtocolError
  let status = rawHead.substr(0, eol - 1)
  if not status.startsWith("HTTP/1.1 101"): raise BadProtocol
  var hdr = ""
  if eol + 2 < stop: hdr = rawHead.substr(eol + 2, stop - 1)
  let answer = HttpHeaders(hdr)
  if not hasToken(answer.get("Upgrade"), "websocket"): raise ProtocolError
  if not hasToken(answer.get("Connection"), "upgrade"): raise ProtocolError
  if answer.get("Sec-WebSocket-Accept") != acceptValue(key):
    raise ProtocolError
  # RFC 6455 §4.1: a subprotocol the client did not offer, or an extension
  # it did not ask for (it asks for none), fails the connection.
  ws.sub = answer.get("Sec-WebSocket-Protocol")
  if ws.sub.len > 0 and ws.sub notin protocols: raise ProtocolError
  if answer.has("Sec-WebSocket-Extensions"): raise ProtocolError
  if idleMs > 0: ws.reader.renew(afterMs(idleMs))
  else: ws.reader.renew(never)
  result = ws

# ------------------------------------------------------------ the connection --

proc reader*(ws: var WebSocket): var WebSocketReader {.inline.} =
  ## The receiving half of the connection. The pair's `recv` is the usual
  ## entry point: it answers the pings and closes this half reports.
  ws.reader

proc writer*(ws: var WebSocket): var WebSocketWriter {.inline.} =
  ## The sending half of the connection. The pair's `send` is the usual entry
  ## point: it is the only place that knows the close handshake.
  ws.writer

proc closeCode*(ws: WebSocket): int =
  ## The close code of the connection: the one the peer sent, the one that was
  ## sent, or 0 while the connection is still open.
  if ws.reader.closeCode != 0: ws.reader.closeCode
  else: ws.writer.closeCode

proc closeCode*(rd: WebSocketReader): int =
  ## The close code the peer sent, or 0 while no close has been seen.
  rd.closeCode

proc subprotocol*(ws: WebSocket): string =
  ## The subprotocol the handshake agreed on; `""` when there is none.
  ws.sub

proc lastError*(ws: WebSocket): ErrorCode =
  ## The error that ended the connection, if one did.
  ws.err

proc lastError*(rd: WebSocketReader): ErrorCode =
  ## The error that ended the receive side, if one did.
  rd.err

proc abort*(wr: var WebSocketWriter) =
  ## Drops the sending side without a close frame.
  wr.w.abort()

proc abort*(ws: var WebSocket) =
  ## Drops the connection without a closing handshake.
  ##
  ## The peer sees the connection go away without a goodbye, which is what
  ## an error looks like from its side. Use it when the connection is not
  ## worth a clean end — and see the note at the end of this module about the
  ## trap it is there to avoid.
  ws.aborted = true
  ws.writer.abort()
  ws.reader.close()

# ------------------------------------------------------------- writing frames --

proc sendFrame(wr: var WebSocketWriter; opcode: WsOpcode;
               payload: openArray[char]; fin = true; flush = true) {.
    passive, raises.} =
  ## One frame with `payload`.
  ##
  ## A masked frame (a client's) gets a fresh key, which is what a client has
  ## to do and what a server must not: the mask is there to keep a broken
  ## proxy from seeing the same bytes twice, and only the client knows the
  ## key. The frame is written as one piece, so a peer is never left in the
  ## middle of a header it has to wait for.
  ##
  ## The header and the payload go into the stream's buffer and are flushed
  ## *once*: a flush is a syscall and, on a ring, a submit-and-wait, so a frame
  ## that flushed twice would cost two of them for a header of two to fourteen
  ## bytes. A server that fans one message out to many connections pays this
  ## per delivery, which is where it shows. With `flush = false` even that one
  ## is deferred, so a run of frames reaches the kernel together.
  ##
  ## The frame is spelled into the writer's own buffer (`Writer.grow`), so a
  ## frame costs no allocation: the header used to be a string of its own and a
  ## client's payload a second one.
  var key = default(array[4, char])
  let masked = wr.role == wrClient
  if masked:
    var rnd = default(array[4, byte])
    if not urandom(rnd):
      # No key, no frame: a masked frame with a predictable key is the thing
      # the mask exists to prevent.
      raise IOError
    for i in 0 ..< 4: key[i] = char(int(rnd[i]))
  let n = frameHeaderLen(payload.len, masked) + payload.len
  if n <= MaxBufferedFrame:
    # The frame is small enough to be worth copying into the buffer, where the
    # next frame joins it.
    discard fillFrame(wr.w.grow(n), opcode, payload, masked, key, fin)
  else:
    # Too big to be worth a copy: the header goes in the buffer (anything
    # already there goes out in front of it) and the payload is handed to the
    # kernel straight from the caller's memory.
    wr.w.flush()
    var head = newString(frameHeaderLen(payload.len, masked))
    discard fillHeader(toOpenArray(head, 0, head.len - 1), opcode, payload.len,
                       masked, key, fin)
    wr.w.write(head)
    if payload.len > 0:
      if masked:
        var body = newString(payload.len)
        for i in 0 ..< payload.len: body[i] = payload[i]
        applyMask(toOpenArray(body, 0, body.len - 1), key)
        wr.w.write(body)
      else:
        wr.w.write(payload)
  if flush: wr.w.flush()

proc fail(wr: var WebSocketWriter; code: int) {.passive, raises.} =
  ## Tells the peer which rule it broke. The connection is dropped by the
  ## caller, which owns both halves and the state they share.
  if wr.closeCode == 0: wr.closeCode = code
  if not wr.closeSent:
    wr.closeSent = true
    try:
      wr.sendFrame(wsoClose, closePayload(code, ""))
    except ErrorCode:
      discard

proc send*(wr: var WebSocketWriter; kind: MsgKind; data: openArray[char];
           flush = true) {.passive, raises.} =
  ## Sends a message of `kind` with the bytes `data`.
  ##
  ## Text has to be text: a `mkText` whose bytes are not UTF-8 raises rather
  ## than goes out, because the peer would have to fail the connection over
  ## it. A ping or a pong carries at most 125 bytes. Nothing may follow a
  ## close frame, so after one this raises `BadOperation`.
  ##
  ## With `flush = false` the frame is only put in the connection's buffer and
  ## goes out with the next `flush`, or with the next frame that asks for one,
  ## or when the buffer fills. That is what makes a fan-out cheap: a task with
  ## several frames for one connection writes them all with one syscall — and
  ## the wait for that syscall is the backpressure that lets the next batch
  ## exist. A writer that stops writing has to `flush`; `close` does, and so
  ## does the pair's `recv` before it parks.
  if wr.closeSent: raise BadOperation
  case kind
  of mkText:
    if not isValidUtf8(data): raise ValueError
    wr.sendFrame(wsoText, data, flush = flush)
  of mkBinary:
    wr.sendFrame(wsoBinary, data, flush = flush)
  of mkPing:
    if data.len > 125: raise ValueError
    wr.sendFrame(wsoPing, data, flush = flush)
  of mkPong:
    if data.len > 125: raise ValueError
    wr.sendFrame(wsoPong, data, flush = flush)
  of mkClose:
    # A close is not a message: it has a code and it ends the connection.
    raise ValueError

proc flush*(wr: var WebSocketWriter) {.passive, raises.} =
  ## Hands everything sent with `flush = false` to the kernel.
  ##
  ## A flush of an empty buffer is free — it does not reach the descriptor —
  ## so this is safe to call whenever a batch of frames is finished.
  wr.w.flush()

proc close*(wr: var WebSocketWriter; code = 1000, reason = "") {.
  passive, raises.} =
  ## Sends the close frame and nothing else. Waiting for the peer's half is
  ## the pair's job; this is for a caller that only drives the writer.
  if wr.closeSent: return
  if not isSendableCloseCode(code): raise ValueError
  if reason.len > 0 and not isValidUtf8(toOpenArray(reason, 0, reason.len - 1)):
    raise ValueError
  wr.closeCode = code
  wr.closeSent = true
  wr.sendFrame(wsoClose, closePayload(code, reason))

# -------------------------------------------------------------- reading frames --

proc markFailure(rd: var WebSocketReader; code: int) {.inline.} =
  ## Remembers the close code that belongs to the protocol error being raised.
  if rd.failCode == 0: rd.failCode = code

proc readFrameHeader(rd: var WebSocketReader; f: var WsFrame): bool {.
  passive, raises.} =
  ## The next frame's header; `false` when the stream ended before one began.
  ##
  ## A frame that breaks the protocol raises `ProtocolError`; one that claims
  ## more than `maxMessage` bytes raises `ContentTooLong` — and that is decided
  ## from the header, before the payload is read, so that a peer cannot make
  ## the other side allocate by announcing a length.
  if not rd.ensure(2): return false
  var head = default(array[14, char])
  discard rd.take(toOpenArray(head, 0, 1))
  let need = headerLen(head[1])
  if need > 2:
    if not rd.ensure(need - 2): raise EndOfStreamError
    discard rd.take(toOpenArray(head, 2, need - 1))
  if parseFrameHeader(toOpenArray(head, 0, need - 1), f) != need:
    raise ProtocolError
  if f.rsv != 0:
    # The reserved bits belong to the extensions; this endpoint speaks none,
    # and a peer that sets them is speaking another protocol.
    raise ProtocolError
  if f.len > rd.maxMessage: raise ContentTooLong
  result = true

proc readPayload(rd: var WebSocketReader; f: WsFrame; dest: var string): int {.
  passive, raises.} =
  ## The payload of the frame `f`, unmasked, appended to `dest`.
  ##
  ## How many bytes arrived. Fewer than the frame announced means the stream
  ## ended in the middle of the frame.
  let n = f.len
  if n == 0: return 0
  let old = dest.len
  let dp = beginStore(dest, old + n, old)
  var got = 0
  while got < n:
    if rd.avail == 0:
      if rd.r.fill() == 0: break
    let took = min(n - got, rd.avail)
    copyMem(addr dp[got], addr rd.r.peek[0], took)
    rd.r.consume took
    if f.masked:
      for i in got ..< got + took:
        dp[i] = char(ord(dp[i]) xor ord(f.key[i and 3]))
    got += took
  dest.endStore()
  if got < n: dest.setLen old + got
  result = got

proc readMessagePart(rd: var WebSocketReader; f: WsFrame): bool {.
  passive, raises.} =
  ## The payload of `f` appended to the message being assembled; `false` when
  ## the stream ended in the middle of the frame.
  ##
  ## The buffer is moved out and back rather than passed in: a `var` argument
  ## that is a field of the reader would alias the reader itself, and the
  ## move costs nothing — a string is a pointer, and it moves back.
  var buf = move rd.msgBuf
  let got = rd.readPayload(f, buf)
  rd.msgBuf = ensureMove buf
  result = got == f.len

proc nextFrame(rd: var WebSocketReader; f: var WsFrame): bool {.
  passive, raises.} =
  ## The next frame of the connection; `false` at the end of the stream.
  result = rd.readFrameHeader(f)
  if result:
    if rd.role == wrServer and not f.masked:
      # A client masks what it sends; a server does not. An unmasked frame
      # from a client is not a client that forgot, it is a client that is
      # not speaking this protocol.
      raise ProtocolError
    if rd.role == wrClient and f.masked:
      # The same rule, the other way round: a masked frame from a server is
      # a server that is confused about which end of the connection it is.
      raise ProtocolError

proc recvImpl(rd: var WebSocketReader; msg: var string): MsgKind {.
  passive, raises.} =
  while true:
    if rd.closeSeen:
      # The peer said goodbye; there is nothing more to read.
      msg = ""
      return mkClose
    var f = WsFrame()
    if not rd.nextFrame(f):
      # The stream ended without a closing handshake. There is no close code
      # to send — nobody is there to receive it — so the connection is
      # dropped and the caller is told why.
      raise EndOfStreamError
    if ord(f.opcode) >= 8:
      # A control frame: it may come in the middle of a message, and
      # `parseFrameHeader` has already checked that it is not fragmented and
      # not longer than 125 bytes.
      case f.opcode
      of wsoClose:
        var payload = ""
        if rd.readPayload(f, payload) < f.len: raise EndOfStreamError
        var code = 0
        if payload.len == 1:
          rd.markFailure(wscProtocolError)
          raise ProtocolError
        if payload.len >= 2:
          code = (ord(payload[0]) shl 8) or ord(payload[1])
          if not isSendableCloseCode(code):
            rd.markFailure(wscProtocolError)
            raise ProtocolError
          if not isValidUtf8(toOpenArray(payload, 2, payload.len - 1)):
            rd.markFailure(wscInvalidPayload)
            raise ProtocolError
        rd.closeCode = code
        rd.closeSeen = true
        msg = if payload.len > 2: payload.substr(2) else: ""
        return mkClose
      of wsoPing:
        var payload = ""
        if rd.readPayload(f, payload) < f.len: raise EndOfStreamError
        msg = payload
        return mkPing
      of wsoPong:
        var payload = ""
        if rd.readPayload(f, payload) < f.len: raise EndOfStreamError
        msg = payload
        return mkPong
      else:
        discard
    else:
      # A data frame: the first one of a message says what the message is,
      # the ones after it continue it.
      if f.opcode == wsoContinuation:
        if rd.msgOp == 0:
          # A continuation without a message to continue.
          rd.markFailure(wscProtocolError)
          raise ProtocolError
      else:
        if rd.msgOp != 0:
          # A new message while the last one is unfinished.
          rd.markFailure(wscProtocolError)
          raise ProtocolError
        rd.msgOp = ord(f.opcode)
        rd.msgBuf = ""
      if rd.msgBuf.len + f.len > rd.maxMessage:
        raise ContentTooLong
      if not rd.readMessagePart(f):
        raise EndOfStreamError
      if f.fin:
        let kind = if rd.msgOp == 1: mkText else: mkBinary
        if kind == mkText and not isValidUtf8(toOpenArray(rd.msgBuf, 0,
                                                          rd.msgBuf.len - 1)):
          rd.markFailure(wscInvalidPayload)
          raise ProtocolError
        msg = rd.msgBuf
        rd.msgOp = 0
        rd.msgBuf = ""
        return kind

proc recv*(rd: var WebSocketReader; msg: var string): MsgKind {.
  passive, raises.} =
  ## The next message of the receive side, in `msg`.
  ##
  ## A text or binary message is its payload. A ping, a pong and a close are
  ## handed over as `mkPing`, `mkPong` and `mkClose`: answering a ping and
  ## echoing a close need the writer, so that is the pair's business. A text
  ## message is checked as a whole; UTF-8 may be split across fragments.
  ##
  ## The fragments of a message are put back together here. A protocol error
  ## raises `ProtocolError` with `failCode` naming the close code to send; the
  ## caller of a bare reader has to send it (the pair does).
  ##
  ## With an `idleMs`, each call gets that long: it replaces whatever deadline
  ## the connection had.
  try:
    if rd.idleMs > 0: rd.r.renew(afterMs(rd.idleMs))
    result = recvImpl(rd, msg)
  except ErrorCode as e:
    rd.err = e
    if e == EndOfStreamError: rd.close()
    raise

# ------------------------------------------------------ the pair's recv/send --

proc recv*(ws: var WebSocket; msg: var string): MsgKind {.passive, raises.} =
  ## The next message of the connection, in `msg`.
  ##
  ## A text or binary message is its payload. A ping is answered with a pong
  ## and not handed over — the peer asked whether the connection is alive,
  ## and the answer is not the application's business. A pong and a close
  ## are handed over: the first is the answer to a ping of this side's, the
  ## second ends the conversation.
  while true:
    # Anything this side owes the peer goes out before the chain waits for the
    # peer: a frame sent with `flush = false` and then a `recv` would otherwise
    # wait for an answer to something the peer has not been told. A flush of an
    # empty buffer costs nothing.
    try:
      ws.writer.flush()
    except ErrorCode:
      discard
    try:
      result = ws.reader.recv(msg)
    except ErrorCode as e:
      ws.err = e
      ws.reader.err = e
      case e
      of ProtocolError:
        ws.writer.fail(if ws.reader.failCode != 0: ws.reader.failCode
                       else: wscProtocolError)
      of ContentTooLong:
        ws.writer.fail(wscTooBig)
      else:
        # A timeout or a broken pipe is not the peer's fault, and there is
        # nobody left to tell it to.
        discard
      ws.abort()
      raise
    case result
    of mkPing:
      # The reader handed the ping over instead of answering it; the pair is
      # the only place that owns both halves, so the pong goes out here —
      # unless this side has said goodbye, after which it says nothing. In a
      # room the writer is the room's, so the pong is the room's to send.
      let r = ws.room
      if r != nil:
        discard r.inbox.send RoomEvent(kind: rePong, id: ws.member, text: msg)
      elif not ws.writer.closeSent:
        ws.writer.sendFrame(wsoPong, msg)
    of mkClose:
      let r = ws.room
      if r != nil:
        # The room answers the peer's goodbye, with the same code.
        discard r.inbox.send RoomEvent(kind: reClose, id: ws.member,
                                       code: ws.reader.closeCode)
        ws.room = nil
      elif not ws.writer.closeSent:
        # The peer closed first: the answer carries the same code, and then
        # the connection is over.
        ws.writer.closeSent = true
        ws.writer.closeCode = ws.reader.closeCode
        ws.writer.sendFrame(wsoClose, closePayload(ws.reader.closeCode, ""))
      return mkClose
    else:
      return result

proc send*(ws: var WebSocket; kind: MsgKind; data: openArray[char];
           flush = true) {.passive, raises.} =
  ## Sends a message of `kind` with the bytes `data`; see
  ## `WebSocketWriter.send`.
  ##
  ## A connection that is in a room sends through it — the room owns the writer
  ## — and `flush` is the room's business then: it flushes each member once per
  ## turn.
  let r = ws.room
  if r != nil:
    if kind == mkText and not isValidUtf8(data): raise ValueError
    var text = newString(data.len)
    for i in 0 ..< data.len: text[i] = data[i]
    var ev = RoomEvent(kind: reSend, id: ws.member, msg: kind, text: text)
    let queued = r.inbox.send(ev)
    if not queued: discard
  else:
    ws.writer.send(kind, data, flush)

proc flush*(ws: var WebSocket) {.passive, raises.} =
  ## Hands everything sent with `flush = false` to the peer; see
  ## `WebSocketWriter.flush`. A connection in a room has nothing to flush here:
  ## the room flushes each member once per turn.
  if ws.room == nil: ws.writer.flush()

proc close*(ws: var WebSocket; code = 1000, reason = "") {.passive, raises.} =
  ## Ends the connection the way the protocol says: the close frame goes
  ## out, and then this side waits for the peer's close.
  ##
  ## The wait is the connection's `idleMs`, or five seconds when it has
  ## none. A peer that does not answer is a timeout and raises like every
  ## other timeout in this API.
  let r = ws.room
  if r != nil:
    # The room owns the writer: it sends the goodbye, after the frames that are
    # already buffered, and drops the member.
    ws.room = nil
    discard r.inbox.send RoomEvent(kind: reClose, id: ws.member, code: code)
    ws.reader.close()
    return
  if ws.writer.closeSent: return
  try:
    ws.writer.close(code, reason)
    # The peer's close is the other half of the handshake.
    ws.reader.renew(afterMs(if ws.reader.idleMs > 0: ws.reader.idleMs
                            else: DefaultCloseMs))
    while not ws.reader.closeSeen:
      var f = WsFrame()
      if not ws.reader.nextFrame(f):
        raise EndOfStreamError
      if ord(f.opcode) >= 8:
        case f.opcode
        of wsoClose:
          var payload = ""
          if ws.reader.readPayload(f, payload) < f.len:
            raise EndOfStreamError
          if payload.len >= 2:
            ws.reader.closeCode = (ord(payload[0]) shl 8) or ord(payload[1])
          ws.reader.closeSeen = true
        of wsoPing:
          var payload = ""
          if ws.reader.readPayload(f, payload) < f.len:
            raise EndOfStreamError
          ws.writer.sendFrame(wsoPong, payload)
        else:
          discard
      else:
        # A message that arrives while the handshake is finishing is dropped:
        # the caller asked for the connection to end.
        var payload = ""
        if ws.reader.readPayload(f, payload) < f.len:
          raise EndOfStreamError
  except ErrorCode as e:
    ws.err = e
    ws.abort()
    raise

# The destructor closes the streams through the fields' own destructors. The
# one thing it adds is leaving the room: a connection in a room has given its
# writer away, so the goodbye is the room's to send, and a dropped `WebSocket`
# that said nothing would leave the room writing into a descriptor nobody else
# owns. It cannot raise, so a failed goodbye is not reported here — `close`
# does that. A peer that wants a close frame asks for one with `close`; the
# error paths call `abort`, which drops the streams so that nothing is written
# on the way out.
proc `=destroy`*(ws: WebSocket) =
  let r = ws.room
  if r != nil:
    try:
      discard r.inbox.send RoomEvent(kind: reClose, id: ws.member)
    except ErrorCode:
      discard
  `=destroy`(ws.room)
  `=destroy`(ws.reader)
  `=destroy`(ws.writer)
  `=destroy`(ws.sub)

# ------------------------------------------------------------------- rooms --
#
# A chat room is the shape a WebSocket server usually has, and the one this
# module does not give on its own: a message from one connection goes to many.
# The rule that makes the module safe — one writer per connection, one place
# that writes — is also why a fan-out cannot be done by the connection that
# received the message. It needs a place that owns the writers.
#
# A `Room` is that place, and a connection joins one with `subscribe`. From
# then on the room owns the writer and the pair's own `send`, `recv`, `close`
# and `publish` go through it:
#
#   proc handle(c: sink HttpConnection) {.passive.} =
#     if not c.next(): return
#     var ws = upgradeWebSocket(c)
#     ws.subscribe("chat")
#     try:
#       while true:
#         var msg = ""
#         case ws.recv(msg)
#         of mkText: ws.publish("chat", msg)
#         of mkClose: break
#         else: discard
#     except ErrorCode:
#       discard
#     ws.close()
#
# Topics are paths, as in uWebSockets: `"chat"`, `"chat/room1"`,
# `"chat/room1/users"`. Publishing to a topic reaches its subscribers and the
# subscribers of everything below it, so `publish("chat", ...)` is the whole
# room and `publish("chat/room1", ...)` is one part of it. The room keeps a
# tree, not a list of members to scan: a publish walks the path once and then
# the subscribers that hang off the node it lands on, so its cost is the depth
# of the topic and the size of the subtree, not the size of the room.
#
# `publish` hands the room one line; the room writes it into every member's
# buffer and flushes each member once per turn. A message therefore costs one
# channel send and one buffer append per member — not a task per delivery,
# which is what a fan-out costs when every connection writes for itself.

type
  RoomEventKind = enum
    reJoin, rePublish, reSend, rePong, reClose, reAddTopic, reDropTopic

  TopicNode = ref object
    ## One segment of a topic path, and what hangs off it.
    segment: string
    parent: nil TopicNode
    children: seq[TopicNode]
    subs: seq[Member]

  Member = ref object
    ## One connection's writer, owned by the room from `subscribe` on.
    wr: WebSocketWriter
    id: int
    topics: seq[string]
    dirty: bool
      ## Frames were buffered for this member since the last flush.
    gen: int
      ## The last publish it was given. A member subscribed to a topic *and*
      ## one of its parents is reached twice by one publish; the generation is
      ## what keeps that to one delivery.
    gone: bool
      ## Its socket failed: it is out of the tree and out of `members` at the
      ## end of the turn, without disturbing the walk that found it.

  RoomEvent = object
    kind: RoomEventKind
    member: nil Member
    id: int
    topic: string
    text: string
    msg: MsgKind
    code: int

  RoomObj = object
    inbox: Sender[RoomEvent]
    nextId: int
    gen: int
      ## Bumped for every publish; see `Member.gen`.
    root: TopicNode
    lastTopic: string
      ## The topic of the last publish and the node it resolved to: a server
      ## publishes the same topic over and over, and one string compare beats
      ## walking the path.
    lastNode: nil TopicNode

  Room* = ref RoomObj
    ## A tree of topics and the chain that writes to the connections in it.

proc roomLoop(r: Room; rx: sink Receiver[RoomEvent]) {.passive.}

proc newRoom*(capacity = 4096): Room {.raises.} =
  ## A room, and the chain that fans out for it. `ioRing` has to be up, like
  ## for any server.
  ##
  ## `capacity` is how many lines may be queued before a publisher parks. It is
  ## also the batch the room drains per turn, and so the number of frames that
  ## share one flush per member: too small and a fan-out pays a syscall per
  ## frame, too large and a slow peer's frames sit in the queue.
  var (rx, tx) = channel[RoomEvent](capacity)
  result = Room(inbox: tx, nextId: 0, gen: 0, root: newTopic("", nil),
                lastTopic: "", lastNode: nil)
  submit(delay(roomLoop(result, rx)), -1)

var
  gDefaultRoom: nil Room
  gDefaultRoomInited: int
  gDefaultLock: TicketLock

proc defaultRoom*(): Room {.raises.} =
  ## The process-wide room. `subscribe` and `publish` use it when they are not
  ## given one, which is what a single-server application wants: topics are
  ## then just names, as in Bun's `ws.subscribe("chat")`. A server that needs
  ## isolated rooms makes its own with `newRoom`.
  ##
  ## Initialised on first use under a lock, so importing this module does not
  ## start a chain for a program that never uses one.
  if atomicLoad(gDefaultRoomInited, moAcquire) == 0:
    acquire gDefaultLock
    if atomicLoad(gDefaultRoomInited, moAcquire) == 0:
      {.cast(assumeSync).}:  # written under gDefaultLock
        gDefaultRoom = newRoom()
      atomicStore(gDefaultRoomInited, 1, moRelease)
    release gDefaultLock
  {.cast(assumeSync).}:  # published by the release-store above
    let r = gDefaultRoom
    if r != nil: result = r
    else: raise BadOperation

# ------------------------------------------------------------ the tree --

proc newTopic(segment: string; parent: nil TopicNode): TopicNode =
  TopicNode(segment: segment, parent: parent, children: @[], subs: @[])

proc childFor(t: TopicNode; segment: string): TopicNode =
  ## The child of `t` called `segment`, created if it is not there yet.
  var i = 0
  while i < t.children.len:
    if t.children[i].segment == segment: return t.children[i]
    inc i
  result = newTopic(segment, t)
  t.children.add result

proc nodeFor(t: TopicNode; topic: string): TopicNode =
  ## The node of `topic`, created along the way. `""` is the root itself.
  result = t
  var i = 0
  while i <= topic.len:
    let start = i
    while i < topic.len and topic[i] != '/': inc i
    if i > start:
      result = result.childFor(topic.substr(start, i - 1))
    inc i

proc findNode(t: TopicNode; topic: string): nil TopicNode =
  ## The node of `topic`, or nil when nothing ever subscribed there.
  var cur: nil TopicNode = t
  var i = 0
  while i <= topic.len:
    let here = cur
    if here == nil: return nil
    let start = i
    while i < topic.len and topic[i] != '/': inc i
    if i > start:
      let segment = topic.substr(start, i - 1)
      var k = 0
      var next: nil TopicNode = nil
      while k < here.children.len:
        if here.children[k].segment == segment:
          next = here.children[k]
          break
        inc k
      cur = next
    inc i
  result = cur

proc unlink(n: TopicNode) =
  ## Drops an empty node from its parent, and that parent too when it becomes
  ## empty, up to the root. What a `dropTopic` leaves behind is not kept.
  var cur = n
  while true:
    let p = cur.parent
    if p == nil: return
    if cur.subs.len > 0 or cur.children.len > 0: return
    var i = 0
    while i < p.children.len:
      if p.children[i] == cur:
        p.children.delete i
        break
      inc i
    cur = p

proc dropSub(t: TopicNode; m: Member) =
  var i = 0
  while i < t.subs.len:
    if t.subs[i] == m: t.subs.delete i
    else: inc i
  if t.subs.len == 0: unlink(t)

# ------------------------------------------------------------ the room --

proc resolve(r: Room; topic: string): nil TopicNode =
  ## The node of `topic`, through the one-entry cache.
  let cached = r.lastNode
  if cached != nil and r.lastTopic == topic: return cached
  result = findNode(r.root, topic)
  let found = result
  if found != nil:
    r.lastTopic = topic
    r.lastNode = found

proc deliver(t: TopicNode; text: string; gen: int) =
  ## One publish to `t` and everything below it, each member once.
  var i = 0
  while i < t.subs.len:
    let m = t.subs[i]
    if not m.gone and m.gen != gen:
      m.gen = gen
      try:
        # `sendFrame` and not `send`: the text was checked once, when it was
        # published, and this runs once per delivery.
        m.wr.sendFrame(wsoText, text, flush = false)
        m.dirty = true
      except ErrorCode:
        # A member whose socket is gone takes the line with it; the rest of
        # the subtree does not care, and the walk does not stop for it.
        m.wr.abort()
        m.gone = true
    inc i
  i = 0
  while i < t.children.len:
    deliver(t.children[i], text, gen)
    inc i

proc forget(root: TopicNode; m: Member) =
  ## Takes a member out of every topic it was in.
  var i = 0
  while i < m.topics.len:
    let n = findNode(root, m.topics[i])
    if n != nil: dropSub(n, m)
    inc i

proc roomLoop(r: Room; rx: sink Receiver[RoomEvent]) {.passive.} =
  ## The one chain that writes to a room's sockets. Everything it writes is
  ## buffered per member with `flush = false`; the flush happens when the queue
  ## runs dry, so one write carries a whole turn's frames.
  let root = r.root
  var members: seq[Member] = @[]
  var e = RoomEvent()
  while true:
    try:
      if not rx.tryRecv(e):
        var i = 0
        while i < members.len:
          let m = members[i]
          if m.gone:
            forget(root, m)
            members.delete i
          else:
            if m.dirty:
              m.dirty = false
              try:
                m.wr.flush()
                inc i
              except ErrorCode:
                m.wr.abort()
                m.gone = true
                forget(root, m)
                members.delete i
            else:
              inc i
        if not rx.recv(e): return
      case e.kind
      of reJoin:
        let m = e.member
        if m != nil:
          members.add m
          nodeFor(root, e.topic).subs.add m
          r.lastNode = nil
      of rePublish:
        inc r.gen
        let n = resolve(r, e.topic)
        if n != nil: deliver(n, e.text, r.gen)
      of reSend, rePong:
        var i = 0
        while i < members.len and members[i].id != e.id: inc i
        if i < members.len:
          try:
            if e.kind == rePong:
              members[i].wr.send(mkPong, e.text, flush = false)
            else:
              members[i].wr.send(e.msg, e.text, flush = false)
            members[i].dirty = true
          except ErrorCode:
            members[i].wr.abort()
            members[i].gone = true
      of reAddTopic, reDropTopic:
        var i = 0
        while i < members.len and members[i].id != e.id: inc i
        if i < members.len:
          let m = members[i]
          r.lastNode = nil
          if e.kind == reAddTopic:
            var seen = false
            var k = 0
            while k < m.topics.len:
              if m.topics[k] == e.topic: seen = true
              inc k
            if not seen:
              m.topics.add e.topic
              nodeFor(root, e.topic).subs.add m
          else:
            var k = 0
            while k < m.topics.len:
              if m.topics[k] == e.topic:
                m.topics.delete k
                break
              inc k
            let n = findNode(root, e.topic)
            if n != nil: dropSub(n, m)
      of reClose:
        var i = 0
        while i < members.len and members[i].id != e.id: inc i
        if i < members.len:
          let m = members[i]
          members.delete i
          forget(root, m)
          r.lastNode = nil
          try:
            m.wr.close(if e.code == 0: 1000 else: e.code)
          except ErrorCode:
            m.wr.abort()
    except ErrorCode:
      discard

proc attach(r: Room; wr: sink WebSocketWriter; topic: string): int {.passive, raises.} =
  ## A member with this writer, in `topic`. The room writes to it from now on.
  let id = atomicFetchAdd(r.nextId, 1, moRelaxed)
  let m = Member(wr: move wr, id: id, topics: @[topic], dirty: false, gen: 0,
                 gone: false)
  discard r.inbox.send RoomEvent(kind: reJoin, member: m, id: id, topic: topic)
  result = id

proc addTopic*(r: Room; id: int; topic: string) {.passive, raises.} =
  ## Puts member `id` in `topic` as well. The room applies it in order with the
  ## publishes that follow, so a `subscribe` then a `publish` cannot lose the
  ## connection in between.
  discard r.inbox.send RoomEvent(kind: reAddTopic, id: id, topic: topic)

proc dropTopic*(r: Room; id: int; topic: string) {.passive, raises.} =
  ## Takes member `id` out of `topic`, leaving the connection in the room.
  discard r.inbox.send RoomEvent(kind: reDropTopic, id: id, topic: topic)

proc publish*(r: Room; topic: string; data: string) {.passive, raises.} =
  ## Sends `data` as a text message to every member of `topic` **and of every
  ## topic below it**: `publish("chat", ...)` is the whole room,
  ## `publish("chat/room1", ...)` one part of it.
  ##
  ## The room does the writing; this parks when its queue is full, which is the
  ## room's backpressure and the reason a publisher cannot outrun its members
  ## without bound.
  discard r.inbox.send RoomEvent(kind: rePublish, topic: topic, text: data)

proc subscribe*(ws: var WebSocket; room: Room; topic: string) {.
    passive, raises.} =
  ## Joins `topic` in `room` — a path like `"chat/room1"`, whose parents are
  ## not implied: a publish to `"chat"` reaches this connection because it is
  ## below `"chat"`, and a publish to `"chat/room1"` because it is that topic.
  ##
  ## The first call gives the connection's writer to the room, which writes to
  ## it from then on; later calls add topics. A connection belongs to one room
  ## only, because it has one writer.
  let r = ws.room
  if r == nil:
    ws.room = room
    ws.member = room.attach(move ws.writer, topic)
  else:
    if r != room: raise BadOperation
    room.addTopic(ws.member, topic)

proc subscribe*(ws: var WebSocket; topic: string) {.passive, raises.} =
  ## Joins `topic` in the process-wide room; see `defaultRoom`.
  subscribe(ws, defaultRoom(), topic)

proc unsubscribe*(ws: var WebSocket; topic: string) {.passive, raises.} =
  ## Leaves `topic`; the connection stays in the room for its other topics, and
  ## a publish to a parent of `topic` no longer reaches it.
  let r = ws.room
  if r == nil: return
  r.dropTopic(ws.member, topic)

proc publish*(ws: var WebSocket; topic: string; data: string) {.
    passive, raises.} =
  ## Sends `data` to every member of `topic` and of the topics below it, this
  ## connection included if it is one of them. A connection that never
  ## subscribed publishes to the default room.
  let r = ws.room
  if r != nil: r.publish(topic, data)
  else: defaultRoom().publish(topic, data)
