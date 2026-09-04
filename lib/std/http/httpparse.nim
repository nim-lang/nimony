# (c) 2026 Andreas Rumpf
#
# HTTP/1.1 head parsing: wire bytes to an `HttpMsg`. See doc/internals/http.md.
#
# Every proc here follows the `parse(buf, …) -> int` shape: it reads `buf` from
# the front and answers how many bytes it consumed, or one of the negative
# results below. There is no `i` parameter — `openArray` already carries a
# start, so a caller that is part-way through a buffer hands over
# `toOpenArray(buf, j, buf.len-1)`, which costs nothing, and adds the answer to
# `j`. Nothing is copied out either: what a proc matched is `buf`'s first
# `result` bytes, so the caller has the bounds without a second out-parameter
# and without a substring.
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

const
  TokenChars = {'a'..'z', 'A'..'Z', '0'..'9',
                '!', '#', '$', '%', '&', '\'', '*', '+', '-', '.',
                '^', '_', '`', '|', '~'}
    ## RFC 9110 `tchar`. Notably excludes the separators, so a header name
    ## cannot contain a colon, space or control character.
  Spaces = {' ', '\t'}
  LineEnd = {'\r', '\n'}
  Controls = {'\0'..'\x1F', '\x7F'}
    ## Rejected inside a target or a value rather than swallowed: a NUL or a
    ## stray CR there is how a request gets smuggled past one parser and into
    ## another.

proc skipSpaces*(buf: openArray[char]): int =
  ## Past any spaces and tabs. Never fails; may return `0`.
  result = 0
  while result < buf.len and buf[result] in Spaces: inc result

proc parseCrLf*(buf: openArray[char]): int =
  ## Past a CRLF. A bare LF is accepted because too much of the world emits
  ## it; a bare CR is not, since it is the classic request-smuggling seam.
  if buf.len == 0: return ParseIncomplete
  if buf[0] == '\n': return 1
  if buf[0] != '\r': return ParseBad
  if buf.len < 2: return ParseIncomplete
  if buf[1] != '\n': return ParseBad
  result = 2

proc parseToken*(buf: openArray[char]): int =
  ## The length of the leading run of `tchar`, which is also the token: it is
  ## `buf`'s first `result` bytes.
  result = 0
  while result < buf.len and buf[result] in TokenChars: inc result
  if result == 0: return ParseBad

proc parseUntil*(buf: openArray[char]; stop: char): int =
  ## The length up to but not including `stop`.
  var j = 0
  while j < buf.len:
    let c = buf[j]
    if c == stop: return j
    if c in Controls: return ParseBad
    inc j
  result = ParseIncomplete

proc parseUntilEol*(buf: openArray[char]): int =
  ## The length up to but not including the CR or LF that ends the line.
  var j = 0
  while j < buf.len:
    let c = buf[j]
    if c in LineEnd: return j
    if c in Controls: return ParseBad
    inc j
  result = ParseIncomplete

# ---------------------------------------------------------- request line ---

proc parseMethod*(buf: openArray[char]; meth: var TagId): int =
  ## A method name followed by one space.
  let n = parseToken(buf)
  if n < 0: return n
  if n >= buf.len: return ParseIncomplete    # the name may not be complete yet
  if buf[n] != ' ': return ParseBad
  meth = lookupMethod(toOpenArray(buf, 0, n - 1))
  if meth.uint32 == 0'u32: return ParseBad   # extension methods are not served
  result = n + 1

proc parseVersion*(buf: openArray[char]; v: var TagId): int =
  ## `HTTP/1.1`, `HTTP/1.0`, or `HTTP/2`.
  const Prefix = "HTTP/"
  if buf.len < Prefix.len + 1: return ParseIncomplete
  for k in 0..<Prefix.len:
    if buf[k] != Prefix[k]: return ParseBad
  let j = Prefix.len
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

proc parseRequestLine*(buf: openArray[char]; m: var HttpMsg): int =
  ## `METHOD SP target SP HTTP/1.1 CRLF`, opening the message node.
  var meth = TagId(0)
  var j = parseMethod(buf, meth)
  if j < 0: return j

  let target = parseUntil(toOpenArray(buf, j, buf.len - 1), ' ')
  if target < 0: return target
  if target == 0: return ParseBad                # empty target
  if target > MaxTargetLen: return ParseBad
  let targetStart = j
  j += target + 1                                # past the target and its space

  var v = TagId(0)
  let ver = parseVersion(toOpenArray(buf, j, buf.len - 1), v)
  if ver < 0: return ver
  j += ver
  let eol = parseCrLf(toOpenArray(buf, j, buf.len - 1))
  if eol < 0: return eol

  m.startRequest(meth, toOpenArray(buf, targetStart, targetStart + target - 1), v)
  result = j + eol

# --------------------------------------------------------------- headers ---

proc valueIsTagged(h: TagId): bool {.inline.} =
  ## Whether this header's value is worth resolving to a tag. Only the ones a
  ## server tests on every request: turning `keep-alive` into a tag here is
  ## what makes the test downstream an integer compare.
  h == tag(hConnection) or h == tag(hTransferEncoding) or
  h == tag(hContentEncoding)

proc parseContentLength(buf: openArray[char]; n: var int): bool =
  ## Digits to an integer. Parsed once, here, so nothing downstream ever
  ## re-reads them and a non-numeric length is refused at the door rather than
  ## being interpreted differently by the next hop.
  if buf.len == 0: return false
  n = 0
  for c in buf:
    if c < '0' or c > '9': return false
    let d = int(ord(c) - ord('0'))
    if n > (high(int) - d) div 10: return false
    n = n * 10 + d
  result = true

proc parseHeaderLine*(buf: openArray[char]; m: var HttpMsg): int =
  ## One `name ":" OWS value OWS CRLF`, appended to `m`.
  if buf.len > 0 and buf[0] in Spaces:
    # Obsolete line folding. RFC 9112 says a server must reject it rather than
    # guess, and guessing is how two parsers come to disagree about where a
    # header ends.
    return ParseBad

  let nameLen = parseToken(buf)
  if nameLen < 0: return nameLen
  if nameLen >= buf.len: return ParseIncomplete
  # No space is allowed between the name and the colon; that too is a
  # smuggling seam.
  if buf[nameLen] != ':': return ParseBad

  var j = nameLen + 1
  j += skipSpaces(toOpenArray(buf, j, buf.len - 1))
  let valueLen = parseUntilEol(toOpenArray(buf, j, buf.len - 1))
  if valueLen < 0: return valueLen

  # Trailing optional whitespace is not part of the value.
  var stop = j + valueLen
  while stop > j and buf[stop - 1] in Spaces: dec stop
  if stop - j > MaxValueLen: return ParseBad

  let eol = parseCrLf(toOpenArray(buf, j + valueLen, buf.len - 1))
  if eol < 0: return eol

  let h = lookupHeader(toOpenArray(buf, 0, nameLen - 1))
  if h.uint32 == 0'u32:
    m.addOtherHeader(toOpenArray(buf, 0, nameLen - 1),
                     toOpenArray(buf, j, stop - 1))
  elif h == tag(hContentLength):
    var n = 0
    if not parseContentLength(toOpenArray(buf, j, stop - 1), n): return ParseBad
    m.addHeader(h, n)
  elif valueIsTagged(h):
    let v = lookupValue(toOpenArray(buf, j, stop - 1))
    if v.uint32 != 0'u32:
      m.addHeader(h, v)
    else:
      m.addHeader(h, toOpenArray(buf, j, stop - 1))
  else:
    m.addHeader(h, toOpenArray(buf, j, stop - 1))
  result = j + valueLen + eol

# --------------------------------------------------------- response line ---

proc parseStatus*(buf: openArray[char]; status: var int): int =
  ## Exactly three digits, as RFC 9112 requires, and within 100..599. A code
  ## outside that range is not an extension we do not know about, it is a
  ## malformed response.
  if buf.len < 3: return ParseIncomplete
  var n = 0
  for k in 0..2:
    let c = buf[k]
    if c < '0' or c > '9': return ParseBad
    n = n * 10 + int(ord(c) - ord('0'))
  if n < 100 or n > 599: return ParseBad
  status = n
  result = 3

proc parseReason*(buf: openArray[char]): int =
  ## The reason phrase: spaces, tabs and printable bytes up to the line end.
  ## Looser than `parseUntilEol` in allowing HTAB, which RFC 9112 permits
  ## here, and it is a response we are reading rather than one we are serving.
  var j = 0
  while j < buf.len:
    let c = buf[j]
    if c in LineEnd: return j
    if c != '\t' and c in Controls: return ParseBad
    inc j
  result = ParseIncomplete

proc parseStatusLine*(buf: openArray[char]; m: var HttpMsg): int =
  ## `version SP status [SP reason] CRLF`, opening the message node.
  ##
  ## The reason phrase is parsed and **discarded**. Nothing reads it — RFC 9110
  ## tells clients to ignore it and lets a proxy replace it — and keeping it
  ## would mean a payload string on every response for no reader. The cost is
  ## that a response round trip is byte-identical only up to the phrase, which
  ## `httpwire` regenerates canonically. Requests have no such gap.
  var v = TagId(0)
  var j = parseVersion(buf, v)
  if j < 0: return j
  if j >= buf.len: return ParseIncomplete
  if buf[j] != ' ': return ParseBad
  inc j

  var status = 0
  let st = parseStatus(toOpenArray(buf, j, buf.len - 1), status)
  if st < 0: return st
  j += st

  if j < buf.len and buf[j] == ' ':
    inc j
    let reason = parseReason(toOpenArray(buf, j, buf.len - 1))
    if reason < 0: return reason
    j += reason
  let eol = parseCrLf(toOpenArray(buf, j, buf.len - 1))
  if eol < 0: return eol

  m.startResponse(status, v)
  result = j + eol

# --------------------------------------------------------------- chunked ---

const
  MaxChunkSizeDigits* = 32
    ## Hex digits allowed in a chunk size. Leading zeros are legal, so this
    ## cannot be tight enough to bound the value — the overflow check does
    ## that. It bounds the *line*, so a peer cannot send a megabyte of `0`.
  MaxChunkExtLen* = 256
    ## Chunk extensions are parsed only to be skipped; nothing reads them.
  MaxTrailerCount* = 16

proc hexDigit(c: char): int {.inline.} =
  case c
  of '0'..'9': ord(c) - ord('0')
  of 'a'..'f': ord(c) - ord('a') + 10
  of 'A'..'F': ord(c) - ord('A') + 10
  else: -1

proc parseChunkSize*(buf: openArray[char]; size: var int): int =
  ## `1*HEXDIG [ chunk-ext ] CRLF`. Consumes up to the chunk's first data byte
  ## and sets `size` — `0` for the last chunk.
  ##
  ## Strict on purpose. A chunk size is the length of the next piece of the
  ## message, so a parser that accepts `+5`, `0x5` or a value that wraps is a
  ## parser that can be made to disagree with the next hop about where this
  ## message ends. Only hex digits, and only a value that fits.
  var j = 0
  var n = 0
  while j < buf.len:
    let d = hexDigit(buf[j])
    if d < 0: break
    if j >= MaxChunkSizeDigits: return ParseBad
    if n > (high(int) - d) div 16: return ParseBad     # would wrap
    n = n * 16 + d
    inc j
  if j == 0:
    # No digits *because the buffer ran out* is "read more", not "malformed".
    # Getting this wrong makes the very first read of a chunked body fail,
    # since at that point there is nothing buffered at all.
    return if buf.len == 0: ParseIncomplete else: ParseBad
  if j >= buf.len: return ParseIncomplete
  if buf[j] notin LineEnd:
    # Not the line end, so it must be an extension, introduced by a
    # semicolon. Trailing junk after the digits is not tolerated.
    if buf[j] != ';': return ParseBad
    let ext = parseUntilEol(toOpenArray(buf, j, buf.len - 1))
    if ext < 0: return ext
    if ext > MaxChunkExtLen: return ParseBad
    j += ext
  size = n
  let eol = parseCrLf(toOpenArray(buf, j, buf.len - 1))
  result = if eol < 0: eol else: j + eol

proc parseTrailerEnd*(buf: openArray[char]): int =
  ## Skip the trailer section after the last chunk, and the blank line that
  ## ends it. Consumes everything up to and including that blank line.
  ##
  ## Trailers are consumed, not kept: folding them into the head would let a
  ## peer set a header *after* the recipient has already acted on the ones it
  ## sent up front, which is the reason trailers are treated with suspicion.
  var j = 0
  var count = 0
  while true:
    let blank = parseCrLf(toOpenArray(buf, j, buf.len - 1))
    if blank >= 0: return j + blank
    if blank != ParseBad: return blank        # incomplete, not "no blank line"
    inc count
    if count > MaxTrailerCount: return ParseBad
    let line = parseUntilEol(toOpenArray(buf, j, buf.len - 1))
    if line < 0: return line
    j += line
    let eol = parseCrLf(toOpenArray(buf, j, buf.len - 1))
    if eol < 0: return eol
    j += eol

proc framingIsUnambiguous*(m: HttpMsg): bool =
  ## Whether exactly one thing says where this message's body ends.
  ##
  ## This is the request-smuggling check, and it is a rejection rather than a
  ## repair on purpose. If a message carries both `Content-Length` and
  ## `Transfer-Encoding`, or two `Content-Length` lines, then two hops reading
  ## the same bytes can disagree about where it stops — and an attacker picks
  ## which hop believes which. RFC 9112 permits stripping the
  ## `Content-Length` instead; doing that means trusting ourselves to strip it
  ## exactly as everything in front of us does, which is the assumption these
  ## attacks are built on.
  let cl = countHeader(m, hContentLength)
  if cl > 1: return false
  let te = countHeader(m, hTransferEncoding)
  if cl == 1 and te > 0: return false
  if te > 1: return false
  result = true

# ------------------------------------------------------------ whole heads --

proc parseHead(buf: openArray[char]; m: var HttpMsg; asRequest: bool): int =
  ## The shared body of `parseRequestHead` / `parseResponseHead`: a start line,
  ## its headers, and the blank line that ends them.
  if buf.len == 0: return ParseIncomplete
  if buf.len > MaxHeadLen: return ParseBad

  var j =
    if asRequest: parseRequestLine(buf, m)
    else: parseStatusLine(buf, m)
  if j < 0: return j

  var count = 0
  while true:
    let done = parseCrLf(toOpenArray(buf, j, buf.len - 1))
    if done >= 0:
      m.finish()
      # Only now: the check needs every header to be present.
      if not framingIsUnambiguous(m): return ParseBad
      return j + done
    if done != ParseBad: return done   # incomplete, not "no blank line here"
    inc count
    if count > MaxHeaderCount: return ParseBad
    let line = parseHeaderLine(toOpenArray(buf, j, buf.len - 1), m)
    if line < 0: return line
    j += line

proc parseRequestHead*(buf: openArray[char]; m: var HttpMsg): int =
  ## A complete request head: the request line, its headers, and the blank
  ## line that ends them. Consumes up to the first body byte.
  ##
  ## `m` must be empty — `initHttpMsg` or `reset`. On a negative result it is
  ## left half-built and only `reset` is valid on it, which is fine because
  ## neither outcome lets the connection continue.
  parseHead(buf, m, asRequest = true)

proc parseResponseHead*(buf: openArray[char]; m: var HttpMsg): int =
  ## A complete response head: the status line, its headers, and the blank
  ## line. Consumes up to the first body byte — though whether there is a body
  ## at all depends on the status and on the request that provoked it, which is
  ## the connection layer's business, not this one's.
  ##
  ## Same contract as `parseRequestHead`: `m` must be empty, and on a negative
  ## result it is left half-built and only `reset` is valid on it.
  parseHead(buf, m, asRequest = false)

type
  HeadScanner* = object
    ## How far into the unconsumed bytes the search for the end of the head
    ## got. Carrying it across reads is what stops a head that arrives in
    ## small pieces from being rescanned from the top every time — the
    ## difference between O(n) and O(n²) for a peer that sends one byte at a
    ## time.
    ##
    ## Relative to the front of the buffer it is asked about, so a connection
    ## that compacts its read buffer moves the bytes and the position together
    ## and there is nothing to fix up.
    pos*: int

proc findHeadEnd*(sc: var HeadScanner; buf: openArray[char]): int =
  ## The length of the head including the blank line that ends it, or
  ## `ParseIncomplete` if it has not arrived yet, or `ParseBad` once the head
  ## is over `MaxHeadLen`.
  ##
  ## Call it again after appending to `buf`; it resumes where it stopped.
  # The terminator is at most four bytes (CRLFCRLF), so resuming three bytes
  # back cannot miss one that straddles where the last read ended.
  var j = if sc.pos >= 3: sc.pos - 3 else: 0
  while j < buf.len:
    if buf[j] == '\n':
      # A blank line is a line terminator immediately after another one.
      let blank =
        (j >= 1 and buf[j - 1] == '\n') or
        (j >= 2 and buf[j - 1] == '\r' and buf[j - 2] == '\n')
      if blank:
        sc.pos = j + 1
        return j + 1
    inc j
  sc.pos = buf.len
  if buf.len > MaxHeadLen: return ParseBad
  result = ParseIncomplete
