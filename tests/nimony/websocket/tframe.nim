# The frame codec of `std/websocket` — the part of it that can be tested
# without a connection, and the part that has to be right before any of the
# rest can be.
#
# Every case here is a rule of RFC 6455 §5 that a server gets wrong in a way
# that only shows up against a peer that cares: the length has to be written
# in the fewest bytes that hold it, a control frame may not be fragmented and
# may not be longer than 125 bytes, and an opcode nobody defined is not an
# opcode at all.
#
# The bytes are checked against the RFC's own spelling, not against what the
# module happens to produce: a codec that agrees with itself is a codec that
# agrees with a bug.
#
# What is *not* here is the size limit: it is applied by the reader, from the
# header before the payload is read, and `tserver` checks it over a socket
# (case 16 and case 27) because that is where a peer can announce a length it
# never sends.

import std / [websocket, assertions, syncio]
import std / http/httpparse

const Key4 = ['a', 'b', 'c', 'd']
  ## A mask key, spelled out so that the bytes below have a fixed answer.

const Zero4 = ['\0', '\0', '\0', '\0']
  ## What an unmasked frame passes: the key is not written.

proc hexOf(s: string): string =
  ## The bytes of `s` as hex, for a report a human reads.
  const digits = "0123456789abcdef"
  result = ""
  for c in s:
    result.add digits[ord(c) shr 4]
    result.add digits[ord(c) and 0xF]

proc hexBytes(s: string): string =
  ## The bytes `s` spells in hex; a space between them is allowed.
  result = ""
  var hi = -1
  for c in s:
    if c == ' ': continue
    var v = 0
    if c >= '0' and c <= '9': v = ord(c) - ord('0')
    elif c >= 'a' and c <= 'f': v = ord(c) - ord('a') + 10
    else: v = ord(c) - ord('A') + 10
    if hi < 0:
      hi = v
    else:
      result.add chr((hi shl 4) or v)
      hi = -1
  assert hi < 0, "an odd number of hex digits"

proc verdict(n: int): string =
  ## What a `parseFrameHeader` return value says, in words.
  case n
  of ParseIncomplete: "incomplete"
  of ParseBad: "bad"
  else: "header of " & $n & " bytes"

proc headerOf(bytes: string): string =
  ## The verdict on the first frame in `bytes`.
  var f = WsFrame()
  verdict(parseFrameHeader(toOpenArray(bytes, 0, bytes.len - 1), f))

proc parseOk(bytes: string): WsFrame =
  ## The header of the one frame in `bytes`, which has to be all header.
  var f = WsFrame()
  let n = parseFrameHeader(toOpenArray(bytes, 0, bytes.len - 1), f)
  assert n == bytes.len, "the bytes are one whole header"
  result = f

proc opName(op: WsOpcode): string =
  case op
  of wsoContinuation: "continuation"
  of wsoText: "text"
  of wsoBinary: "binary"
  of wsoClose: "close"
  of wsoPing: "ping"
  of wsoPong: "pong"

proc main() {.raises.} =
  # --- the length forms (RFC 6455 §5.2) ----------------------------
  # The number of length bytes is not a style: 0 to 125 is the short form,
  # 126 to 65535 the 16-bit one, and everything above it the 64-bit one.
  echo "5 bytes:      ", hexOf(frameHeader(wsoText, 5, false, Zero4))
  echo "126 bytes:    ", hexOf(frameHeader(wsoBinary, 126, false, Zero4))
  echo "65535 bytes:  ", hexOf(frameHeader(wsoBinary, 65535, false, Zero4))
  echo "65536 bytes:  ", hexOf(frameHeader(wsoBinary, 65536, false, Zero4))
  echo "70000 bytes:  ", hexOf(frameHeader(wsoBinary, 70000, false, Zero4))

  # --- and back again ---------------------------------------------
  let h = parseOk(hexBytes("827e00c8"))
  echo "the 16-bit form: ", opName(h.opcode), " ", h.len, " bytes, fin: ", h.fin,
       ", masked: ", h.masked
  let w = parseOk(hexBytes("827f0000000000011170"))
  echo "the 64-bit form: ", opName(w.opcode), " ", w.len, " bytes"
  let c = parseOk(hexBytes("89 00"))
  echo "a ping: ", opName(c.opcode), " ", c.len, " bytes, fin: ", c.fin

  # --- the mask ---------------------------------------------------
  # A masked header carries the key behind the length, and the frame it
  # builds is not the text on the wire.
  echo "masked header: ", hexOf(frameHeader(wsoText, 5, true, Key4))
  var masked = newString(5)
  for i in 0 ..< 5: masked[i] = "hello"[i]
  applyMask(toOpenArray(masked, 0, 4), Key4)
  echo "masked hello:  ", hexOf(masked)
  # Masking is its own inverse — that is the whole reason one proc serves
  # both ends.
  applyMask(toOpenArray(masked, 0, 4), Key4)
  echo "unmasked:      ", masked
  # A payload that arrives in pieces restarts the key where the frame says,
  # not where the read does: the second piece is the second key position.
  var piece = newString(2)
  piece[0] = "hello"[1]
  piece[1] = "hello"[2]
  applyMask(toOpenArray(piece, 0, 1), Key4, 1)
  echo "the second and third byte, masked from offset 1: ", hexOf(piece)

  # --- the rules a peer breaks on purpose --------------------------
  # 0x3 is a reserved opcode: it belongs to an extension that was never
  # negotiated.
  echo "reserved opcode:  ", headerOf("\x83\x00")
  # A control frame may not be fragmented: it has to be readable without
  # reading the message it may sit in the middle of.
  echo "fragmented ping:  ", headerOf("\x09\x00")
  # ... and it may not be longer than 125 bytes.
  echo "long ping:        ", headerOf("\x89\x7e\x00\x7e")
  # The top bit of a 64-bit length is reserved.
  echo "top bit set:      ", headerOf("\x82\x7f\x80\x00\x00\x00\x00\x00\x00\x00")
  # A length that does not fit the fewest bytes is not this protocol: the
  # short form would have done, and a peer that does not say so is a peer
  # whose frames cannot be replayed one way.
  echo "16-bit for 5:     ", headerOf("\x82\x7e\x00\x05")
  echo "64-bit for 126:   ", headerOf("\x82\x7f\x00\x00\x00\x00\x00\x00\x00\x7e")
  # A header that has not all arrived is not a bad header; it is a header
  # that is not finished.
  echo "one byte:         ", headerOf("\x82")
  echo "half a length:    ", headerOf("\x82\x7e\x00")
  echo "half a mask:      ", headerOf("\x82\x85ab")

  # A huge length is a well-formed header: whether it is acceptable is the
  # reader's question, asked before any of it is read.
  let big = parseOk(hexBytes("827f0000000000100000"))
  echo "a length of ", big.len, " bytes parses, and its size is the reader's business"

  # --- the close codes that may not go on the wire ------------------
  # 1005 and 1006 are what a peer sees when no code was sent, and 1015 is a
  # local fact about TLS; sending any of them would be a lie about what
  # happened.
  echo "1004 may not be sent: ", not isSendableCloseCode(1004)
  echo "1005 may not be sent: ", not isSendableCloseCode(1005)
  echo "1006 may not be sent: ", not isSendableCloseCode(1006)
  echo "1015 may not be sent: ", not isSendableCloseCode(1015)
  echo "999 may not be sent:  ", not isSendableCloseCode(999)
  echo "5000 may not be sent: ", not isSendableCloseCode(5000)
  echo "1000 may be sent:     ", isSendableCloseCode(1000)
  echo "1016 may not be sent: ", not isSendableCloseCode(1016)
  echo "2999 may not be sent: ", not isSendableCloseCode(2999)
  echo "1014 may be sent:     ", isSendableCloseCode(1014)
  echo "3000 may be sent:     ", isSendableCloseCode(3000)
  echo "4999 may be sent:     ", isSendableCloseCode(4999)

try:
  main()
except ErrorCode as e:
  echo "error: ", e
