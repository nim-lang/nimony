# (c) 2026 Andreas Rumpf
#
# HTTP/1.1 head serialization: an `HttpMsg` back to wire bytes.
# See doc/internals/http.md. The inverse of `httpparse`, and the same shape.
#
# Every proc is `write(dest, i, …) -> int`: write into `dest` starting at `i`
# and answer the index just past what was written, or `WriteFull` if it did
# not fit. Writers are all-or-nothing — a failed call leaves nothing the
# caller has to unpick, it just retries from the same `i` with more room.
#
# `i` is a cursor, not a redundant restatement of where `dest` begins: it is
# what a caller threads from one writer to the next, and **a negative `i` is
# passed through untouched**. So a chain of writes is written as a chain, and
# the one test at the end of it says whether the whole head fitted — rather
# than every link restating the same question about the previous one.
#
# Nothing is built up in a temporary and copied over. Header names are
# borrowed from the tag pool, values are borrowed from the message's own pool,
# and integers are formatted straight into `dest`. `headLen` says exactly how
# much room a head needs, so the usual path never has to retry at all.

import ./httpmsg
import ../../../src/lib/nifcore
import std / assertions

const
  WriteFull* = -1
    ## Did not fit. Nothing past `i` was written; grow `dest` and retry.

# ------------------------------------------------------------- primitives --

proc writeByte*(dest: var openArray[char]; i: int; c: char): int =
  if i < 0 or i >= dest.len: return WriteFull
  dest[i] = c
  result = i + 1

proc writeBytes*(dest: var openArray[char]; i: int; s: openArray[char]): int =
  if i < 0 or dest.len - i < s.len: return WriteFull
  for k in 0..<s.len: dest[i + k] = s[k]
  result = i + s.len

proc writeCrLf*(dest: var openArray[char]; i: int): int {.inline.} =
  if i < 0 or dest.len - i < 2: return WriteFull
  dest[i] = '\r'
  dest[i + 1] = '\n'
  result = i + 2

proc digitCount*(x: int): int =
  ## Digits in the decimal form of a non-negative `x`. Multiplications, not
  ## divisions: `headLen` asks this for every integer header of every head
  ## just to size a buffer, and `writeInt` then does the one division pass
  ## that actually produces the digits.
  result = 1
  var limit = 10
  while x >= limit:
    inc result
    if limit > high(int) div 10: break     # no wider decimal to compare against
    limit = limit * 10

proc writeInt*(dest: var openArray[char]; i: int; x: int): int =
  ## Decimal, formatted in place. Negative values are not written: nothing in
  ## an HTTP head is signed, and a `-1` reaching the wire would be a bug
  ## downstream rather than a value.
  ##
  ## Written back-to-front from the last digit, so the single `div` loop lands
  ## the digits straight in `dest` — no temporary to reverse out of.
  if x < 0: return WriteFull
  let n = digitCount(x)
  if i < 0 or dest.len - i < n: return WriteFull
  var v = x
  var k = i + n - 1
  while true:
    dest[k] = chr(ord('0') + v mod 10)
    v = v div 10
    if k == i: break
    dec k
  result = i + n

# ------------------------------------------------------------ status line --

proc versionText(v: TagId): string =
  ## The three versions spell themselves; anything else is a bug upstream and
  ## `HTTP/1.1` is the safe thing to put on the wire.
  ##
  ## Returning a literal costs nothing: a long string literal is static data
  ## and is COW-shared rather than copied (`StaticSlen` in
  ## `system/stringimpl.nim`), so this is a view in all but spelling. Same for
  ## `reasonPhrase` below.
  if v == tag(tV10): "HTTP/1.0"
  elif v == tag(tV20): "HTTP/2"
  else: "HTTP/1.1"

proc reasonPhrase*(status: int): string =
  ## The conventional phrase. Clients ignore it and HTTP/1.1 allows it to be
  ## empty, but middleboxes are happier when it is there. Unlisted codes get
  ## their class's phrase rather than nothing.
  case status
  of 100: "Continue"
  of 101: "Switching Protocols"
  of 200: "OK"
  of 201: "Created"
  of 202: "Accepted"
  of 204: "No Content"
  of 206: "Partial Content"
  of 301: "Moved Permanently"
  of 302: "Found"
  of 303: "See Other"
  of 304: "Not Modified"
  of 307: "Temporary Redirect"
  of 308: "Permanent Redirect"
  of 400: "Bad Request"
  of 401: "Unauthorized"
  of 403: "Forbidden"
  of 404: "Not Found"
  of 405: "Method Not Allowed"
  of 406: "Not Acceptable"
  of 408: "Request Timeout"
  of 409: "Conflict"
  of 410: "Gone"
  of 411: "Length Required"
  of 413: "Content Too Large"
  of 414: "URI Too Long"
  of 415: "Unsupported Media Type"
  of 416: "Range Not Satisfiable"
  of 421: "Misdirected Request"
  of 426: "Upgrade Required"
  of 429: "Too Many Requests"
  of 431: "Request Header Fields Too Large"
  of 500: "Internal Server Error"
  of 501: "Not Implemented"
  of 502: "Bad Gateway"
  of 503: "Service Unavailable"
  of 504: "Gateway Timeout"
  of 505: "HTTP Version Not Supported"
  else:
    if status < 200: "Informational"
    elif status < 300: "Successful"
    elif status < 400: "Redirection"
    elif status < 500: "Client Error"
    else: "Server Error"

# ---------------------------------------------------------------- values ---

proc writeValue(dest: var openArray[char]; i: int; v: Cursor): int =
  ## One header value, whatever shape it is stored in.
  if i < 0: return i
  case v.kind
  of StrLit:
    if v.isInlineLit:
      # At most three bytes, and a string that short never reaches the heap.
      let s = v.strVal
      result = writeBytes(dest, i, s)
    else:
      # Borrowed from the message's pool, not copied out of it.
      result = writeBytes(dest, i, poolStr(v.pool, v.strId))
  of IntLit:
    result = writeInt(dest, i, int(v.intVal))
  of TagLit:
    # A value that was resolved to a tag on the way in spells itself on the
    # way out, so the round trip is byte-identical.
    result = writeBytes(dest, i, spelling(v.cursorTagId))
  else:
    result = WriteFull

proc valueLen(v: Cursor): int =
  case v.kind
  of StrLit:
    if v.isInlineLit: v.strVal.len else: poolStr(v.pool, v.strId).len
  of IntLit:
    let x = int(v.intVal)
    if x < 0: 0 else: digitCount(x)
  of TagLit: spelling(v.cursorTagId).len
  else: 0

proc writeHeaderNode(dest: var openArray[char]; i: int; c: Cursor): int =
  ## `name: value[, value]*CRLF` for one header node.
  var j: int
  var body = c.sub()
  if c.cursorTagId == tag(tXhdr):
    # (xhdr "name" "value") — the name is a payload, not a tag.
    j = writeValue(dest, i, body)
    body.skip
  else:
    j = writeBytes(dest, i, spelling(c.cursorTagId))
  j = writeBytes(dest, j, ": ")
  var first = true
  while body.hasMore:
    if not first:
      j = writeBytes(dest, j, ", ")
    j = writeValue(dest, j, body)
    first = false
    body.skip
  result = writeCrLf(dest, j)

proc headerNodeLen(c: Cursor): int =
  var body = c.sub()
  if c.cursorTagId == tag(tXhdr):
    result = valueLen(body)
    body.skip
  else:
    result = spelling(c.cursorTagId).len
  result = result + 2                      # ": "
  var first = true
  while body.hasMore:
    if not first: result = result + 2      # ", "
    result = result + valueLen(body)
    first = false
    body.skip
  result = result + 2                      # CRLF

# --------------------------------------------------------------- chunked ---

proc writeHex(dest: var openArray[char]; i: int; x: int): int =
  ## Lowercase hex, no `0x`, no padding — a chunk size and nothing else.
  if x < 0: return WriteFull
  var digits = 1
  var v = x
  while v >= 16:
    v = v shr 4
    inc digits
  if i < 0 or dest.len - i < digits: return WriteFull
  v = x
  var k = i + digits - 1
  while true:
    let d = v and 0xF
    dest[k] = if d < 10: chr(ord('0') + d) else: chr(ord('a') + d - 10)
    v = v shr 4
    if k == i: break
    dec k
  result = i + digits

proc writeChunkHeader*(dest: var openArray[char]; i: int; size: int): int =
  ## `SIZE CRLF`. The data follows, then its own CRLF — see `writeChunkEnd`.
  ## No extensions are ever written: nothing reads them, and every byte in
  ## this line is a byte another parser has to agree with us about.
  result = writeCrLf(dest, writeHex(dest, i, size))

proc writeChunkEnd*(dest: var openArray[char]; i: int): int {.inline.} =
  ## The CRLF that closes a chunk's data.
  writeCrLf(dest, i)

proc writeLastChunk*(dest: var openArray[char]; i: int): int =
  ## `0 CRLF CRLF` — the zero-length chunk and an empty trailer section.
  ## Written together because a body that stops after the zero chunk is a
  ## body the peer is still waiting on.
  result = writeCrLf(dest, writeChunkHeader(dest, i, 0))

proc chunkOverhead*(size: int): int =
  ## Bytes `writeChunkHeader` + `writeChunkEnd` add around `size` bytes of
  ## data, so a caller can size a buffer without writing twice: one hex digit
  ## and the two CRLFs, plus a digit for every further nibble.
  result = 5
  var v = size
  while v >= 16:
    v = v shr 4
    inc result

# ------------------------------------------------------------ whole heads --

proc writeRequestHead*(dest: var openArray[char]; i: int; m: HttpMsg): int =
  ## `METHOD SP target SP version CRLF`, the headers, and the blank line.
  assert m.isRequest, "writeRequestHead on a response"
  # The target is the message's second child; reach it through a cursor so it
  # is borrowed rather than copied out.
  var root = m.rootCursor()
  var body = root.sub()
  body.skip                                # (METHOD)

  var j = writeBytes(dest, i, spelling(m.methodOf))
  j = writeByte(dest, j, ' ')
  j = writeValue(dest, j, body)
  j = writeByte(dest, j, ' ')
  j = writeBytes(dest, j, versionText(m.versionOf))
  j = writeCrLf(dest, j)
  for c in m.headerNodes:
    j = writeHeaderNode(dest, j, c)
  result = writeCrLf(dest, j)

proc writeResponseHead*(dest: var openArray[char]; i: int; m: HttpMsg): int =
  ## `version SP status SP reason CRLF`, the headers, and the blank line.
  assert m.isResponse, "writeResponseHead on a request"
  let status = m.statusOf
  var j = writeBytes(dest, i, versionText(m.versionOf))
  j = writeByte(dest, j, ' ')
  j = writeInt(dest, j, status)
  j = writeByte(dest, j, ' ')
  j = writeBytes(dest, j, reasonPhrase(status))
  j = writeCrLf(dest, j)
  for c in m.headerNodes:
    j = writeHeaderNode(dest, j, c)
  result = writeCrLf(dest, j)

proc writeHead*(dest: var openArray[char]; i: int; m: HttpMsg): int =
  ## Whichever kind `m` is.
  if m.isRequest: writeRequestHead(dest, i, m)
  elif m.isResponse: writeResponseHead(dest, i, m)
  else: WriteFull

proc headLen*(m: HttpMsg): int =
  ## Exactly how many bytes `writeHead` will produce. One extra walk of a tree
  ## that is already in cache, and it means the common path sizes its buffer
  ## once instead of writing, failing and retrying.
  if m.isRequest:
    var root = m.rootCursor()
    var body = root.sub()
    let meth = body.cursorTagId
    body.skip
    result = spelling(meth).len + 1 + valueLen(body) + 1 +
             versionText(m.versionOf).len + 2
  elif m.isResponse:
    let status = m.statusOf
    result = versionText(m.versionOf).len + 1 + digitCount(status) + 1 +
             reasonPhrase(status).len + 2
  else:
    return 0
  for c in m.headerNodes:
    result = result + headerNodeLen(c)
  result = result + 2                      # the blank line
