# (c) 2026 Andreas Rumpf
#
# Content codings (RFC 9110 §8.4.1): what `Content-Encoding: gzip` and
# `deflate` mean for a message, which one a peer accepts, and reading a body
# with its coding undone. The codec itself is `std/compress/gzip`.
#
#   # client side: ask for it, then read the content rather than the body
#   req.addHeader(hAcceptEncoding, AcceptCodings)
#   ...
#   conn.readResponse(res)
#   let page = conn.readContent(res, limit = 8 shl 20)
#
# The server side lives in `httpserver`, which negotiates per request and
# compresses on its own when `HttpServer.compression` is on.
#
# ## The body and the content are different lengths
#
# `Content-Length` and chunking frame the *encoded* bytes; the coding is
# undone after the framing is. So there are two limits that matter and they
# are not the same number: how much a peer may send, and how much that may
# decode to. A 10 MiB gzip body can be 10 GiB of zeros, and the only defence
# is to count output, which is what `ContentDecoder` does.

import ./httpmsg
import ./httpconn
import ../compress/gzip
import std / ioring

export gzip.Format

const
  AcceptCodings* = "gzip, deflate"
    ## What a client built on this module can decode, ready for
    ## `Accept-Encoding`.

type
  ContentCoding* = enum
    ceIdentity    ## no coding, or `identity`
    ceGzip        ## `gzip` (and the legacy `x-gzip`)
    ceDeflate     ## `deflate`: zlib, or raw DEFLATE from servers that erred
    ceUnknown     ## anything else, including a chain of several codings

proc lower(c: char): char {.inline.} =
  if c >= 'A' and c <= 'Z': char(ord(c) + 32) else: c

proc eqIgnoreCase(a: openArray[char]; b: string): bool =
  if a.len != b.len: return false
  for i in 0..<a.len:
    if lower(a[i]) != b[i]: return false
  result = true

proc startsWithIgnoreCase(a: openArray[char]; b: string): bool =
  if a.len < b.len: return false
  result = eqIgnoreCase(toOpenArray(a, 0, b.len - 1), b)

proc endsWithIgnoreCase(a: openArray[char]; b: string): bool =
  if a.len < b.len: return false
  result = eqIgnoreCase(toOpenArray(a, a.len - b.len, a.len - 1), b)

proc trimmed(s: openArray[char]; a, b: var int) =
  ## Shrink `a ..< b` past optional whitespace at both ends.
  while a < b and (s[a] == ' ' or s[a] == '\t'): inc a
  while b > a and (s[b - 1] == ' ' or s[b - 1] == '\t'): dec b

proc parseQ(s: openArray[char]): int =
  ## A qvalue (RFC 9110 §12.4.2) in thousandths: `1`, `0.8`, `0.125`. A
  ## malformed one counts as `1`, as browsers treat it — the header is a
  ## preference, and refusing a client over its syntax helps nobody.
  if s.len == 0: return 1000
  if s[0] == '1': return 1000
  if s[0] != '0': return 1000
  result = 0
  var scale = 100
  var i = 2
  while i < s.len and scale > 0:
    if s[i] < '0' or s[i] > '9': break
    result += (ord(s[i]) - ord('0')) * scale
    scale = scale div 10
    inc i

proc chooseCoding*(acceptEncoding: openArray[char]): ContentCoding =
  ## The coding to answer with, given a request's `Accept-Encoding` value:
  ## `ceGzip` or `ceDeflate` when the client accepts one with a non-zero
  ## qvalue (gzip on a tie), else `ceIdentity`.
  ##
  ## `*` covers whatever is not named, and an explicit `q=0` takes a coding
  ## off the table even when `*` would have allowed it — `gzip;q=0, *` means
  ## "anything but gzip", and that is exactly the client that has a reason.
  var qGzip = -1
  var qDeflate = -1
  var qStar = -1
  var i = 0
  let s = acceptEncoding
  while i < s.len:
    var stop = i
    while stop < s.len and s[stop] != ',': inc stop
    var a = i
    var b = stop
    var semi = a
    while semi < b and s[semi] != ';': inc semi
    var q = 1000
    if semi < b:
      var pa = semi + 1
      var pb = b
      trimmed(s, pa, pb)
      if pb - pa >= 2 and lower(s[pa]) == 'q' and s[pa + 1] == '=':
        q = parseQ(toOpenArray(s, pa + 2, pb - 1))
    b = semi
    trimmed(s, a, b)
    let name = toOpenArray(s, a, b - 1)
    if eqIgnoreCase(name, "gzip") or eqIgnoreCase(name, "x-gzip"):
      qGzip = max(qGzip, q)
    elif eqIgnoreCase(name, "deflate"):
      qDeflate = max(qDeflate, q)
    elif eqIgnoreCase(name, "*"):
      qStar = max(qStar, q)
    i = stop + 1
  if qGzip < 0: qGzip = qStar
  if qDeflate < 0: qDeflate = qStar
  if qGzip > 0 and qGzip >= qDeflate: result = ceGzip
  elif qDeflate > 0: result = ceDeflate
  else: result = ceIdentity

proc chooseCoding*(m: HttpMsg): ContentCoding =
  ## The coding a response to request `m` may use. No `Accept-Encoding` is
  ## answered with identity: RFC 9110 lets a server assume anything then, and
  ## no server does, because the clients that omit it are the ones that
  ## cannot decode.
  if not m.contains(hAcceptEncoding): ceIdentity
  else: chooseCoding(m.getStr(hAcceptEncoding))

proc isCompressible*(contentType: openArray[char]): bool =
  ## Whether a body of this media type is worth compressing: text and the
  ## structured text formats, not images, archives or video, which are
  ## compressed already and only grow.
  ##
  ## `text/event-stream` is the exception among the text types. Its events
  ## have to reach the client one at a time, and a coding buffers.
  var stop = 0
  while stop < contentType.len and contentType[stop] != ';': inc stop
  var a = 0
  trimmed(contentType, a, stop)
  let t = toOpenArray(contentType, a, stop - 1)
  if startsWithIgnoreCase(t, "text/"):
    result = not eqIgnoreCase(t, "text/event-stream")
  else:
    result = eqIgnoreCase(t, "application/json") or
             eqIgnoreCase(t, "application/javascript") or
             eqIgnoreCase(t, "application/x-javascript") or
             eqIgnoreCase(t, "application/xml") or
             eqIgnoreCase(t, "application/wasm") or
             eqIgnoreCase(t, "image/svg+xml") or
             eqIgnoreCase(t, "image/x-icon") or
             endsWithIgnoreCase(t, "+json") or
             endsWithIgnoreCase(t, "+xml")

proc codingOf*(m: HttpMsg): ContentCoding =
  ## The coding `m`'s body is in, from its `Content-Encoding`. The parser
  ## already resolved the common values to tags, so this is usually an
  ## integer compare.
  if not m.contains(hContentEncoding): return ceIdentity
  if m.countHeader(hContentEncoding) > 1: return ceUnknown
  let t = m.getTag(hContentEncoding)
  if t == tag(vGzip): return ceGzip
  if t == tag(vDeflate): return ceDeflate
  if t == tag(vIdentity): return ceIdentity
  let s = m.getStr(hContentEncoding)
  if eqIgnoreCase(toOpenArray(s, 0, s.len - 1), "x-gzip"): ceGzip
  else: ceUnknown

proc codingTag*(c: ContentCoding): HttpTag =
  ## The `Content-Encoding` value for `c`.
  case c
  of ceGzip: vGzip
  of ceDeflate: vDeflate
  else: vIdentity

proc encodeFormat*(c: ContentCoding): Format =
  ## The container to write for `c`. `deflate` is zlib: RFC 9110 says so, and
  ## raw DEFLATE is what the servers that got it wrong sent, not what anyone
  ## should send now.
  if c == ceDeflate: fmtZlib else: fmtGzip

# ------------------------------------------------------------ decoding ---

type
  ContentDecoder* = object
    ## Undoes a content coding over a body that arrives in pieces, with a
    ## ceiling on both sides of it.
    coding: ContentCoding
    dec: Decoder
    limit: int        ## most decoded bytes allowed
    rawLimit: int     ## most encoded bytes allowed
    raw: int

proc initContentDecoder*(coding: ContentCoding; limit: int): ContentDecoder =
  ## `UnimplementedOperation` is the caller's to raise for `ceUnknown`
  ## before getting here: it is a 415 on a server and a failure on a client,
  ## and only the caller knows which it is.
  ##
  ## The encoded side is capped too, a little above `limit`. Without that,
  ## empty stored blocks — five bytes each, producing nothing — would let a
  ## chunked body run forever under a decoded limit it never reaches.
  ContentDecoder(coding: coding,
                 dec: initDecoder(if coding == ceGzip: fmtGzip else: fmtDetect),
                 limit: limit, rawLimit: limit + limit div 64 + 1024, raw: 0)

proc add*(cd: var ContentDecoder; piece: openArray[char];
          dest: var string) {.raises.} =
  ## Decode `piece`, appending to `dest`. `ContentTooLong` past either
  ## limit, `SyntaxError` for a coding that is corrupt.
  cd.raw += piece.len
  if cd.raw > cd.rawLimit: raise ContentTooLong
  if cd.coding == ceIdentity:
    if dest.len + piece.len > cd.limit: raise ContentTooLong
    for c in piece: dest.add c
  else:
    cd.dec.feed(piece)
    let st = cd.dec.decode(dest, cd.limit - dest.len)
    if st == infOutputFull: raise ContentTooLong
    if st == infError: raise SyntaxError

proc finish*(cd: ContentDecoder) {.raises.} =
  ## The body has ended. `SyntaxError` when the coding had not: a compressed
  ## body cut short decodes to a prefix that looks like a whole, and the
  ## trailer check is the only thing that tells them apart.
  if cd.coding != ceIdentity and not cd.dec.isDone: raise SyntaxError

proc readContent*(c: var HttpConn; m: HttpMsg; limit = 8 * 1024 * 1024;
                  dl = never): string {.passive, raises.} =
  ## The whole body of `m` — just read by `readResponse` or `readRequest` —
  ## with its framing and its content coding both undone.
  ##
  ## A body with neither `Content-Length` nor chunking is read to the end of
  ## the connection, which for a response is what that means. Not for a
  ## response to `HEAD`, or a 1xx, 204 or 304: those have no body whatever
  ## the headers say, and only the caller knows the request was a `HEAD`.
  ##
  ## `UnimplementedOperation` for a coding this cannot undo, `ContentTooLong`
  ## past `limit` decoded bytes, `SyntaxError` for a coding that is corrupt
  ## or cut short, plus whatever reading raises.
  result = ""
  let coding = codingOf(m)
  if coding == ceUnknown: raise UnimplementedOperation
  var cd = initContentDecoder(coding, limit)
  var buf = default(array[4096, char])
  if isChunked(m):
    c.beginBody()
    while true:
      let n = c.readChunked(buf, dl)
      if n == 0: break
      cd.add(toOpenArray(buf, 0, n - 1), result)
  else:
    var left = m.contentLength
    let delimited = left < 0
    while delimited or left > 0:
      let want = if delimited or left > buf.len: buf.len else: left
      let n = c.readBody(toOpenArray(buf, 0, want - 1), dl)
      if n == 0:
        if delimited: break
        raise EndOfStreamError
      cd.add(toOpenArray(buf, 0, n - 1), result)
      if not delimited: left -= n
  cd.finish()
