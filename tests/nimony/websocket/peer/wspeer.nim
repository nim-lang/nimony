# A raw WebSocket client — the peer of the server under test.
#
# Deliberately *not* built on `std/websocket`: a client that shares the
# server's code agrees with it about exactly the thing being tested, and the
# whole point of these suites is that the two ends do not. So this speaks the
# wire directly: the handshake is raw HTTP text, and every frame is assembled
# byte by byte from a spec — which is also what makes it possible to send the
# frames a conforming client never would.
#
# Every knob a conforming client has no use for is here on purpose: the RSV
# bits, the reserved opcodes, the mask bit, the length field. A suite that
# could only send well-formed frames would test nothing.

import std / [socket, ioring]

const
  DefaultKey* = "dGhlIHNhbXBsZSBub25jZQ=="
    ## The nonce of RFC 6455 §1.3 — the one whose accept value the RFC
    ## spells out, so a test can check the server's arithmetic against the
    ## standard rather than against itself.

  ClientMask* = true
    ## Whether frames are masked by default. A client MUST mask (RFC 6455
    ## §5.1); the suites turn it off only to check that the server notices.

type
  WsPeer* = object
    ## One connection to the server, as raw bytes.
    sock: Socket
    open: bool

  WsFrameSpec* = object
    ## A frame to send, spelled out rather than derived.
    fin*: bool
      ## Whether this is the last frame of its message.
    rsv*: int
      ## The three reserved bits, as a number. A conforming client sends
      ## `0`; the suites send `1` to check that the server refuses.
    opcode*: int
      ## The opcode as a number, so that the reserved ones (`0x3` to `0x7`,
      ## `0xB` to `0xF`) can be asked for.
    payload*: string
    mask*: bool
    maskKey*: string
      ## The four mask bytes. Empty means "one of my own", which differs
      ## from frame to frame.
    lengthField*: int
      ## What the length header says, when it is not to be the truth. `-1`
      ## means the honest length. A length that lies is how a peer tries to
      ## make a server read more than it should.
    wide*: bool
      ## Whether to spell the length in the 64-bit form even when the short
      ## one would do. Both are legal; a server has to read both.

  WsIncoming* = object
    ## A frame as it arrived.
    fin*: bool
    rsv*: int
    opcode*: int
    payload*: string
    masked*: bool

proc defaultMaskKey(payload: string): string =
  ## A mask key derived from the payload.
  ##
  ## Derived rather than counted: a counter would be a global, and the
  ## chains that use this client run in parallel — a shared counter would be
  ## a data race in the middle of a test that is about the protocol.
  var h = 0x811C9DC5'u32
  for ch in payload:
    h = (h xor uint32(ord(ch))) * 16777619'u32
  result = ""
  for i in 0 ..< 4:
    result.add chr(int((h shr (8 * i)) and 0xFF))

proc frame*(fin = true; opcode = 0x1; payload = ""; mask = true; rsv = 0;
            maskKey = ""; lengthField = -1; wide = false): WsFrameSpec =
  ## A frame to send.
  ##
  ## `lengthField` is `-1` — the payload's own length — unless a case asks
  ## for another one. The honest default lives here and not on the field,
  ## because an object field cannot carry a default of its own: a frame
  ## built by naming its fields would otherwise claim an empty payload and
  ## put its bytes on the wire anyway.
  result = WsFrameSpec(fin: fin, opcode: opcode, payload: payload, mask: mask,
                       rsv: rsv, maskKey: maskKey, lengthField: lengthField,
                       wide: wide)

proc encodeFrame(spec: WsFrameSpec): string =
  ## The bytes of one frame, header first.
  ##
  ## The length is written exactly as `spec` asks for it: the honest one, a
  ## lying one, or the 64-bit spelling of either. The payload is masked
  ## after the header, because that is where the mask lives.
  var b0 = spec.opcode and 0x0F
  if spec.fin: b0 = b0 or 0x80
  b0 = b0 or ((spec.rsv and 0x07) shl 4)
  var length = spec.lengthField
  if length < 0: length = spec.payload.len
  var maskBit = 0
  if spec.mask: maskBit = 0x80
  result = ""
  result.add chr(b0)
  if spec.wide or length >= 65536:
    result.add chr(maskBit or 127)
    for i in 0 ..< 8:
      result.add chr((length shr (8 * (7 - i))) and 0xFF)
  elif length >= 126:
    result.add chr(maskBit or 126)
    result.add chr((length shr 8) and 0xFF)
    result.add chr(length and 0xFF)
  else:
    result.add chr(maskBit or length)
  if spec.mask:
    var key = spec.maskKey
    if key.len != 4: key = defaultMaskKey(spec.payload)
    result.add key
    for i in 0 ..< spec.payload.len:
      result.add chr(ord(spec.payload[i]) xor ord(key[i and 3]))
  else:
    result.add spec.payload

proc connectWs*(port: int; request = ""): WsPeer {.passive, raises.} =
  ## Connect to `port` on the loopback and send `request` verbatim.
  ##
  ## The request is raw text on purpose: a handshake written by the same
  ## code that reads it cannot catch a mistake in either.
  let fd = socketNonBlocking()
  var sa = default(Sockaddr_storage)
  var sl = default(SockLen)
  loopbackAddr(sa, sl, uint16(port))
  var r = 0
  let k = delay()
  discard submitConnect(cint(fd), sa, sl, afterMs(5000), k, addr r)
  suspend()
  if r != 0:
    closeFd(cint(fd))
    raise IOError
  result = WsPeer(sock: initSocket(cint(fd), afterMs(10000)), open: true)
  if request.len > 0:
    result.sock.write(toOpenArray(request, 0, request.len - 1))

proc send*(p: var WsPeer; spec: WsFrameSpec) {.passive, raises.} =
  ## Send one frame, spelled out by `spec`.
  let bytes = encodeFrame(spec)
  if bytes.len > 0:
    p.sock.write(toOpenArray(bytes, 0, bytes.len - 1))

proc sendBytes*(p: var WsPeer; bytes: string) {.passive, raises.} =
  ## Send bytes that are not a frame at all — a half-written header, a
  ## handshake, whatever a case needs on the wire.
  if bytes.len > 0:
    p.sock.write(toOpenArray(bytes, 0, bytes.len - 1))

proc readExact(p: var WsPeer; n: int): string {.passive, raises.} =
  ## Exactly `n` bytes, or an error: a peer that closed half-way through a
  ## frame has said something, and "the connection ended" is not the same
  ## answer as "the frame was short".
  var buf = newString(n)
  if n > 0:
    let got = p.sock.read(toOpenArray(buf, 0, n - 1))
    if got < n: raise EndOfStreamError
    buf.endStore()
  result = buf

proc readFrame*(p: var WsPeer): WsIncoming {.passive, raises.} =
  ## The next frame the server sent.
  result = default(WsIncoming)
  let hdr = p.readExact(2)
  let b0 = ord(hdr[0])
  let b1 = ord(hdr[1])
  result.fin = (b0 and 0x80) != 0
  result.rsv = (b0 and 0x70) shr 4
  result.opcode = b0 and 0x0F
  result.masked = (b1 and 0x80) != 0
  var length = b1 and 0x7F
  if length == 126:
    let ext = p.readExact(2)
    length = (ord(ext[0]) shl 8) or ord(ext[1])
  elif length == 127:
    let ext = p.readExact(8)
    var wide = 0'u64
    for i in 0 ..< 8:
      wide = (wide shl 8) or uint64(ord(ext[i]))
    length = int(wide)
  var key = ""
  if result.masked:
    key = p.readExact(4)
  if length > 0:
    let payload = p.readExact(length)
    if result.masked:
      var plain = newString(length)
      for i in 0 ..< length:
        plain[i] = chr(ord(payload[i]) xor ord(key[i and 3]))
      plain.endStore()
      result.payload = plain
    else:
      result.payload = payload

proc statusOf(line: string): int =
  ## The status code of a status line: `HTTP/1.1 101 Switching Protocols`.
  var i = 0
  while i < line.len and line[i] != ' ': inc i
  while i < line.len and line[i] == ' ': inc i
  var n = 0
  while i < line.len and line[i] >= '0' and line[i] <= '9':
    n = n * 10 + int(ord(line[i]) - ord('0'))
    inc i
  result = n

proc readHead*(p: var WsPeer): (int, string) {.passive, raises.} =
  ## The server's answer to the handshake: the status code and the whole
  ## head, so a case can look at any header of it.
  ##
  ## The head ends at the blank line; anything after it is frames, and is
  ## left in the buffer for `readFrame`.
  var head = ""
  var status = 0
  var first = true
  while true:
    let line = p.sock.readLine()
    if line.len == 0 and first: break     # the peer closed without answering
    head.add line
    head.add "\n"
    if first:
      status = statusOf(line)
      first = false
    if line.len == 0: break              # the blank line ends the head
  result = (status, head)

proc closePeer*(p: var WsPeer) =
  ## Drop the connection without a closing handshake.
  if p.open:
    p.sock.close()
    p.open = false

proc isOpen*(p: WsPeer): bool {.inline.} =
  ## Whether the connection is still there.
  p.open

proc closeCode*(f: WsIncoming): int =
  ## The status code of a close frame, or `0` when it carried none.
  result = 0
  if f.payload.len >= 2:
    result = (ord(f.payload[0]) shl 8) or ord(f.payload[1])

proc opcodeName(op: int): string =
  ## The name of an opcode, for a report a human reads.
  case op
  of 0x0: "continuation"
  of 0x1: "text"
  of 0x2: "binary"
  of 0x8: "close"
  of 0x9: "ping"
  of 0xA: "pong"
  else: "reserved(" & $op & ")"

proc describe*(f: WsIncoming): string =
  ## One line about a frame, for a report a human reads.
  result = opcodeName(f.opcode)
  if f.opcode == 0x8:
    result.add " " & $closeCode(f)
  elif f.payload.len > 0:
    result.add " " & $f.payload.len & "B"
