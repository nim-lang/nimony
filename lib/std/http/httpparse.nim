# (c) 2026 Andreas Rumpf
#
# HTTP/1.1 head parsing: wire bytes to an `HttpMsg`. See doc/internals/http.md.
#
# Every proc here follows the `parse(buf, i) -> int` shape: it reads `buf`
# starting at `i` and answers the index just past what it consumed, or one of
# the negative results below. Nothing slices and nothing takes a substring —
# a head is inspected in place and only the bytes that end up in the message
# are ever copied, once, into that message's pool.
#
# Resumability is the caller's business and costs it one integer. A head is
# parsed only once it is complete, so `HeadScanner` walks the buffer looking
# for the blank line that ends it and remembers where it stopped; feeding more
# bytes and asking again resumes rather than rescanning, which is what keeps a
# head arriving one byte at a time from costing O(n²).

import ./httpmsg
import std / assertions

const
  ParseIncomplete* = -1
    ## Not enough bytes yet. Read more and ask again; nothing was consumed.
  ParseBad* = -2
    ## Malformed. HTTP/1.1 framing cannot resynchronize, so the only correct
    ## response is to answer 400 and close.

const
  MaxHeadLen* = 16 * 1024   ## total bytes of request line + headers
  MaxHeaderCount* = 100     ## headers per message
  MaxTargetLen* = 8 * 1024  ## request target
  MaxValueLen* = 8 * 1024   ## a single header value

# Limits, not preferences: without them a peer picks our memory use, and a
# head that never ends is a connection that never closes.

# ------------------------------------------------------------- primitives --

proc isTokenChar(c: char): bool {.inline.} =
  ## RFC 9110 `tchar`. Notably excludes the separators, so a header name
  ## cannot contain a colon, space or control character.
  case c
  of 'a'..'z', 'A'..'Z', '0'..'9',
     '!', '#', '$', '%', '&', '\'', '*', '+', '-', '.',
     '^', '_', '`', '|', '~': true
  else: false

proc isSpace(c: char): bool {.inline.} = c == ' ' or c == '\t'

proc skipSpaces*(buf: openArray[char]; i: int): int =
  ## Past any spaces and tabs. Never fails; may return `i`.
  result = i
  while result < buf.len and isSpace(buf[result]): inc result

proc parseCrLf*(buf: openArray[char]; i: int): int =
  ## Past a CRLF. A bare LF is accepted because too much of the world emits
  ## it; a bare CR is not, since it is the classic request-smuggling seam.
  if i >= buf.len: return ParseIncomplete
  if buf[i] == '\n': return i + 1
  if buf[i] != '\r': return ParseBad
  if i + 1 >= buf.len: return ParseIncomplete
  if buf[i + 1] != '\n': return ParseBad
  result = i + 2

proc parseToken*(buf: openArray[char]; i: int; last: var int): int =
  ## A run of `tchar`. `last` is set to the index one past the run, so the
  ## caller has the bounds without cutting anything out.
  var j = i
  while j < buf.len and isTokenChar(buf[j]): inc j
  if j == i: return ParseBad
  last = j
  result = j

proc parseUntil*(buf: openArray[char]; i: int; stop: char; last: var int): int =
  ## Up to but not including `stop`. Control characters terminate the parse
  ## rather than being swallowed: a NUL or a stray CR inside a target or a
  ## value is how a request gets smuggled past one parser and into another.
  var j = i
  while j < buf.len:
    let c = buf[j]
    if c == stop: break
    if c < ' ' or c == '\x7F': return ParseBad
    inc j
  if j >= buf.len: return ParseIncomplete
  last = j
  result = j

proc parseUntilEol*(buf: openArray[char]; i: int; last: var int): int =
  ## Up to but not including the CR or LF that ends the line. Same rejection
  ## of control characters, and the same reason for it.
  var j = i
  while j < buf.len:
    let c = buf[j]
    if c == '\r' or c == '\n': break
    if c < ' ' or c == '\x7F': return ParseBad
    inc j
  if j >= buf.len: return ParseIncomplete
  last = j
  result = j

# ---------------------------------------------------------- request line ---

proc parseMethod*(buf: openArray[char]; i: int; meth: var TagId): int =
  ## A method name followed by one space.
  var stop = 0
  let j = parseToken(buf, i, stop)
  if j < 0: return j
  if j >= buf.len: return ParseIncomplete    # the name may not be complete yet
  if buf[j] != ' ': return ParseBad
  meth = lookupMethod(toOpenArray(buf, i, stop - 1))
  if meth.uint32 == 0'u32: return ParseBad   # extension methods are not served
  result = j + 1

proc parseVersion*(buf: openArray[char]; i: int; v: var TagId): int =
  ## `HTTP/1.1`, `HTTP/1.0`, or `HTTP/2`.
  const Prefix = "HTTP/"
  if buf.len - i < Prefix.len + 1: return ParseIncomplete
  for k in 0..<Prefix.len:
    if buf[i + k] != Prefix[k]: return ParseBad
  let j = i + Prefix.len
  if buf[j] == '2':
    v = tag(tV20)
    return j + 1
  if buf.len - j < 3: return ParseIncomplete
  if buf[j] != '1' or buf[j + 1] != '.': return ParseBad
  case buf[j + 2]
  of '0': v = tag(tV10)
  of '1': v = tag(tV11)
  else: return ParseBad
  result = j + 3

proc parseRequestLine*(buf: openArray[char]; i: int; m: var HttpMsg): int =
  ## `METHOD SP target SP HTTP/1.1 CRLF`, opening the message node.
  var meth = TagId(0)
  var j = parseMethod(buf, i, meth)
  if j < 0: return j

  var stop = 0
  let t = parseUntil(buf, j, ' ', stop)
  if t < 0: return t
  if stop == j: return ParseBad                  # empty target
  if stop - j > MaxTargetLen: return ParseBad
  let targetStart = j
  let targetStop = stop
  j = t + 1                                      # past the space

  var v = TagId(0)
  j = parseVersion(buf, j, v)
  if j < 0: return j
  j = parseCrLf(buf, j)
  if j < 0: return j

  m.startRequest(meth, toOpenArray(buf, targetStart, targetStop - 1), v)
  result = j

# --------------------------------------------------------------- headers ---

proc valueIsTagged(h: TagId): bool {.inline.} =
  ## Whether this header's value is worth resolving to a tag. Only the ones a
  ## server tests on every request: turning `keep-alive` into a tag here is
  ## what makes the test downstream an integer compare.
  h == tag(hConnection) or h == tag(hTransferEncoding) or
  h == tag(hContentEncoding)

proc parseHeaderLine*(buf: openArray[char]; i: int; m: var HttpMsg): int =
  ## One `name ":" OWS value OWS CRLF`, appended to `m`.
  if i < buf.len and isSpace(buf[i]):
    # Obsolete line folding. RFC 9112 says a server must reject it rather than
    # guess, and guessing is how two parsers come to disagree about where a
    # header ends.
    return ParseBad

  var nameStop = 0
  var j = parseToken(buf, i, nameStop)
  if j < 0: return j
  if j >= buf.len: return ParseIncomplete
  # No space is allowed between the name and the colon; that too is a
  # smuggling seam.
  if buf[j] != ':': return ParseBad
  inc j

  j = skipSpaces(buf, j)
  var valueEnd = 0
  let e = parseUntilEol(buf, j, valueEnd)
  if e < 0: return e

  # Trailing optional whitespace is not part of the value.
  var stop = valueEnd
  while stop > j and isSpace(buf[stop - 1]): dec stop
  if stop - j > MaxValueLen: return ParseBad

  let afterValue = parseCrLf(buf, valueEnd)
  if afterValue < 0: return afterValue

  let h = lookupHeader(toOpenArray(buf, i, nameStop - 1))
  if h.uint32 == 0'u32:
    m.addOtherHeader(toOpenArray(buf, i, nameStop - 1),
                     toOpenArray(buf, j, stop - 1))
  elif h == tag(hContentLength):
    # Parsed once, here, and stored as an integer — so nothing downstream ever
    # re-reads the digits, and a non-numeric length is refused at the door
    # rather than being interpreted differently by the next hop.
    if stop == j: return ParseBad
    var n = 0
    for k in j..<stop:
      let c = buf[k]
      if c < '0' or c > '9': return ParseBad
      if n > (high(int) - int(ord(c) - ord('0'))) div 10: return ParseBad
      n = n * 10 + int(ord(c) - ord('0'))
    m.addHeader(h, n)
  elif valueIsTagged(h):
    let v = lookupValue(toOpenArray(buf, j, stop - 1))
    if v.uint32 != 0'u32:
      m.addHeader(h, v)
    else:
      m.addHeader(h, toOpenArray(buf, j, stop - 1))
  else:
    m.addHeader(h, toOpenArray(buf, j, stop - 1))
  result = afterValue

# --------------------------------------------------------- response line ---

proc parseStatus*(buf: openArray[char]; i: int; status: var int): int =
  ## Exactly three digits, as RFC 9112 requires, and within 100..599. A code
  ## outside that range is not an extension we do not know about, it is a
  ## malformed response.
  if buf.len - i < 3: return ParseIncomplete
  var n = 0
  for k in 0..2:
    let c = buf[i + k]
    if c < '0' or c > '9': return ParseBad
    n = n * 10 + int(ord(c) - ord('0'))
  if n < 100 or n > 599: return ParseBad
  status = n
  result = i + 3

proc parseReason*(buf: openArray[char]; i: int; last: var int): int =
  ## The reason phrase: spaces, tabs and printable bytes up to the line end.
  ## Looser than `parseUntilEol` in allowing HTAB, which RFC 9112 permits
  ## here, and it is a response we are reading rather than one we are serving.
  var j = i
  while j < buf.len:
    let c = buf[j]
    if c == '\r' or c == '\n': break
    if c != '\t' and (c < ' ' or c == '\x7F'): return ParseBad
    inc j
  if j >= buf.len: return ParseIncomplete
  last = j
  result = j

proc parseStatusLine*(buf: openArray[char]; i: int; m: var HttpMsg): int =
  ## `version SP status [SP reason] CRLF`, opening the message node.
  ##
  ## The reason phrase is parsed and **discarded**. Nothing reads it — RFC 9110
  ## tells clients to ignore it and lets a proxy replace it — and keeping it
  ## would mean a payload string on every response for no reader. The cost is
  ## that a response round trip is byte-identical only up to the phrase, which
  ## `httpwire` regenerates canonically. Requests have no such gap.
  var v = TagId(0)
  var j = parseVersion(buf, i, v)
  if j < 0: return j
  if j >= buf.len: return ParseIncomplete
  if buf[j] != ' ': return ParseBad
  inc j

  var status = 0
  j = parseStatus(buf, j, status)
  if j < 0: return j

  if j < buf.len and buf[j] == ' ':
    inc j
    var stop = 0
    let e = parseReason(buf, j, stop)
    if e < 0: return e
    j = stop
  j = parseCrLf(buf, j)
  if j < 0: return j

  m.startResponse(status, v)
  result = j

# ------------------------------------------------------------ whole heads --

proc parseRequestHead*(buf: openArray[char]; i: int; m: var HttpMsg): int =
  ## A complete request head: the request line, its headers, and the blank
  ## line that ends them. Returns the index of the first body byte.
  ##
  ## `m` must be empty — `initHttpMsg` or `reset`. On a negative result it is
  ## left half-built and only `reset` is valid on it, which is fine because
  ## neither outcome lets the connection continue.
  if i >= buf.len: return ParseIncomplete
  if buf.len - i > MaxHeadLen: return ParseBad

  var j = parseRequestLine(buf, i, m)
  if j < 0: return j

  var count = 0
  while true:
    let done = parseCrLf(buf, j)
    if done >= 0:
      m.finish()
      return done
    if done != ParseBad: return done   # incomplete, not "no blank line here"
    inc count
    if count > MaxHeaderCount: return ParseBad
    j = parseHeaderLine(buf, j, m)
    if j < 0: return j

proc parseResponseHead*(buf: openArray[char]; i: int; m: var HttpMsg): int =
  ## A complete response head: the status line, its headers, and the blank
  ## line. Returns the index of the first body byte — though whether there is
  ## a body at all depends on the status and on the request that provoked it,
  ## which is the connection layer's business, not this one's.
  ##
  ## Same contract as `parseRequestHead`: `m` must be empty, and on a negative
  ## result it is left half-built and only `reset` is valid on it.
  if i >= buf.len: return ParseIncomplete
  if buf.len - i > MaxHeadLen: return ParseBad

  var j = parseStatusLine(buf, i, m)
  if j < 0: return j

  var count = 0
  while true:
    let done = parseCrLf(buf, j)
    if done >= 0:
      m.finish()
      return done
    if done != ParseBad: return done
    inc count
    if count > MaxHeaderCount: return ParseBad
    j = parseHeaderLine(buf, j, m)
    if j < 0: return j

type
  HeadScanner* = object
    ## Where the search for the end of the head got to. Carrying it across
    ## reads is what stops a head that arrives in small pieces from being
    ## rescanned from the top every time — which is the difference between
    ## O(n) and O(n²) for a peer that sends one byte at a time.
    pos*: int

proc findHeadEnd*(sc: var HeadScanner; buf: openArray[char]; i: int): int =
  ## The index just past the blank line that ends the head, or
  ## `ParseIncomplete` if it has not arrived yet, or `ParseBad` once the head
  ## is over `MaxHeadLen`.
  ##
  ## Call it again after appending to `buf`; it resumes where it stopped.
  if sc.pos < i: sc.pos = i
  # The terminator is at most four bytes (CRLFCRLF), so resuming three bytes
  # back cannot miss one that straddles where the last read ended.
  var j = if sc.pos - i >= 3: sc.pos - 3 else: i
  while j < buf.len:
    if buf[j] == '\n':
      # A blank line is a line terminator immediately after another one.
      let blank =
        (j - 1 >= i and buf[j - 1] == '\n') or
        (j - 2 >= i and buf[j - 1] == '\r' and buf[j - 2] == '\n')
      if blank:
        sc.pos = j + 1
        return j + 1
    inc j
  sc.pos = buf.len
  if buf.len - i > MaxHeadLen: return ParseBad
  result = ParseIncomplete
