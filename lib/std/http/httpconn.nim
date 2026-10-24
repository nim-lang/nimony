# (c) 2026 Andreas Rumpf
#
# HTTP over a connection: framing and keep-alive on top of `std/streamio`.
# See doc/internals/http.md.
#
#   let tags = newHttpTags()                       # once, during init
#   var c = initHttpConn(fd, afterMs(30_000), tags)
#   var m = initHttpMsg(tags)
#   c.readRequest(m)
#   c.respond(200, "hello\n")
#
# What is here is only what HTTP adds: which bytes are a head, where a body
# ends, and what may follow what. The buffer under it, its compaction and
# ceiling, the deadline every operation runs under and the write that loops
# are the `Reader`/`Writer`'s — none of that was ever HTTP.
#
# `.passive` throughout, so reading a head looks like blocking code and is
# not: these park on `std/ioring` and a pool worker resumes them. And
# `.raises`, so a failure looks like a failure: an `ErrorCode` caught with an
# ordinary `try`, rather than a code every call site has to remember to test.
# What the readers *return* is how many bytes there were, and `0` is the end
# of a body or a stream — an end, not a failure.

import ./httpmsg
import ./httpparse
import ./httpwire
import ../streamio
import std / [ioring, assertions]

export streamio.Reader, streamio.Writer, streamio.TlsEngine

# ------------------------------------------------------------ connection ---

type
  ChunkState = enum
    csHeader      ## at a chunk-size line
    csData        ## inside a chunk's data, `chunkLeft` bytes to go
    csDataEnd     ## at the CRLF that closes a chunk's data
    csTrailer     ## after the zero chunk, in the trailer section
    csDone        ## the body has ended

  HttpConn* = object
    r*: Reader
      ## The buffering, the deadline and the reads. Public because a caller
      ## that wants to speak something else over the same connection — an
      ## upgrade to WebSocket, say — needs the stream rather than a copy of
      ## its API forwarded one proc at a time.
    w*: Writer
      ## The writes, sharing the reader's core and deadline.
    tags*: HttpTags
      ## The tag space this connection's messages are built and parsed
      ## against. A connection serves many messages and they all have to agree
      ## on what an id means, so the space is threaded in once here rather than
      ## looked up per message.
    scan: HeadScanner
    chunk: ChunkState
    chunkLeft: int
    fdVal: cint
    error*: ErrorCode
      ## What ended a connection whose setup failed (`accept`'s TLS handshake,
      ## say); `Success` otherwise.

proc initHttpConn*(fd: cint; deadline: Deadline; tags: HttpTags): HttpConn =
  ## `fd` must already be non-blocking. The deadline has no default: a
  ## connection with no budget is one a quiet peer can hold forever.
  ##
  ## Plaintext only, and non-raising: an already-connected descriptor has no
  ## setup that can fail, which is what lets a test build one outside a
  ## passive chain. A server that may want TLS uses the engine-taking overload
  ## and reads `error` if its handshake failed.
  result = HttpConn(tags: tags, scan: default(HeadScanner), chunk: csHeader,
                    chunkLeft: 0, fdVal: fd, error: Success)
  try:
    var (r, w) = plainStreams(fd, deadline)
    result.r = move r
    result.w = move w
  except ErrorCode as e:
    result.fdVal = -1
    result.error = e

proc initHttpConn*(fd: cint; deadline: Deadline; tags: HttpTags;
                   tls: nil TlsEngine): HttpConn {.passive.} =
  ## The same, with an optional server TLS engine. A failed handshake closes
  ## the descriptor and answers a connection whose `error` says why; the check
  ## is `isOpen` because a constructor that owns a move-only value cannot
  ## raise.
  result = HttpConn(tags: tags, scan: default(HeadScanner), chunk: csHeader,
                    chunkLeft: 0, fdVal: fd, error: Success)
  try:
    var (r, w) = acceptStreams(fd, tls, deadline)
    r.renew(deadline)
    result.r = move r
    result.w = move w
  except ErrorCode as e:
    # `acceptStreams` owns and closed `fd` on this path.
    result.fdVal = -1
    result.error = e

proc isOpen*(c: HttpConn): bool {.inline.} =
  ## Whether the streams were set up. A connection from a failed TLS
  ## handshake is not open and carries the reason in `error`.
  c.fdVal >= 0

proc fd*(c: HttpConn): cint {.inline.} = c.fdVal

proc buffered*(c: HttpConn): int {.inline.} =
  ## Bytes read but not yet consumed — a pipelined request, or the start of a
  ## body that arrived with its head.
  c.r.buffered

proc takeStreams*(c: var HttpConn): (Reader, Writer) =
  ## The streams, given up. An upgrade — to WebSocket, say — is exactly this:
  ## from here on the bytes on the descriptor are not HTTP, and the HTTP layer
  ## must not touch them. The bytes already read travel with the `Reader`.
  c.fdVal = -1
  result = (move c.r, move c.w)

# --------------------------------------------------------------- reading ---

proc tighten(r: var Reader; dl: Deadline) {.raises.} =
  ## A per-operation deadline can only make the standing one tighter, never
  ## widen it. The tighter value stays until the next `renew`, which is what a
  ## keep-alive connection does before each exchange.
  if dl != never:
    r.renew(earlier(r.deadline, dl))

proc tighten(w: var Writer; dl: Deadline) {.raises.} =
  ## See the `Reader` overload; the deadline is shared.
  if dl != never:
    w.renew(earlier(w.deadline, dl))

proc readHead(c: var HttpConn; m: var HttpMsg; asRequest: bool;
              dl: Deadline) {.passive, raises.} =
  ## Read until a complete head has arrived, then parse it in one pass.
  ##
  ## A head is only parsed once whole, so a peer dribbling it one byte at a
  ## time costs one `findHeadEnd` resumption per read rather than a rescan —
  ## which is the difference between O(n) and O(n²) for exactly the peer who
  ## would try it.
  ##
  ## `ContentTooLong` when the head passes `MaxHeadLen`, `SyntaxError` when it
  ## is complete and malformed, `EndOfStreamError` when the peer closed before
  ## finishing one, plus whatever `fill` raises.
  c.r.tighten(dl)
  while true:
    let headLen = findHeadEnd(c.scan, c.r.peek)
    if headLen == ParseBad: raise ContentTooLong
    if headLen >= 0:
      # Exactly the head, not the head plus whatever body arrived behind it:
      # the parser then cannot run past what the scanner vouched for, and its
      # `MaxHeadLen` test is about the head rather than about the buffer.
      let n =
        if asRequest: parseRequestHead(toOpenArray(c.r.peek, 0, headLen - 1), m)
        else: parseResponseHead(toOpenArray(c.r.peek, 0, headLen - 1), m)
      if n < 0:
        # The head is complete, so `ParseIncomplete` here is not "read more",
        # it is a head that ends without being valid.
        raise SyntaxError
      c.r.consume n
      c.scan.pos = 0            # the next head starts where this one stopped
      return
    # A peer that closes between messages is a normal end; one that closes
    # part-way through a head is not, but both look like this from here and
    # the caller is the one that knows which it was expecting.
    if c.r.fill() == 0: raise EndOfStreamError

proc readRequest*(c: var HttpConn; m: var HttpMsg;
                  dl = never) {.passive, raises.} =
  ## The next request head on this connection. `m` must be empty; `reset` it
  ## between requests to reuse its buffer, which is the keep-alive path.
  readHead(c, m, true, dl)

proc readResponse*(c: var HttpConn; m: var HttpMsg;
                   dl = never) {.passive, raises.} =
  ## The next response head — the client side of the same loop.
  readHead(c, m, false, dl)

proc isChunked*(m: HttpMsg): bool {.inline.} =
  ## Whether the body is chunk-framed. The parser resolves a known
  ## `Transfer-Encoding` to a tag, so this is an integer compare.
  m.getTag(hTransferEncoding) == tag(vChunked)

proc bodyLength*(m: HttpMsg): int {.inline.} =
  ## Declared body length, or `-1` when there is none or it is chunked.
  if isChunked(m): -1 else: m.contentLength

proc beginBody*(c: var HttpConn) {.inline.} =
  ## Arm the chunk reader for a new body. Call after reading a head whose
  ## `isChunked` is true, before the first `readChunked`.
  c.chunk = csHeader
  c.chunkLeft = 0

proc needMore(c: var HttpConn) {.passive, raises.} =
  ## Read more of a body that is not finished. A peer that stops here has cut
  ## the message in half — unlike between messages, where a close is just the
  ## end — so end of stream is `EndOfStreamError` and not a value.
  if c.r.fill() == 0: raise EndOfStreamError

proc readChunked*(c: var HttpConn; dest: var openArray[char];
                  dl = never): int {.passive, raises.} =
  ## The next piece of a chunk-framed body: the bytes copied, or `0` once the
  ## body has ended.
  ##
  ## `0` means the whole body is over — the zero chunk and its trailers have
  ## been consumed and the connection is positioned at whatever follows, so
  ## it can be reused. It never means "nothing right now"; the loop keeps
  ## reading until it has either bytes or the end.
  ##
  ## `SyntaxError` for framing the peer got wrong, `EndOfStreamError` for a
  ## body it stopped sending. Those were one `-1` before, and they are not the
  ## same thing: one is a peer to answer 400, the other a connection to drop.
  result = 0
  c.r.tighten(dl)
  while true:
    case c.chunk
    of csDone:
      return 0
    of csData:
      if c.r.buffered == 0:
        needMore(c)
        continue
      let took = min(min(dest.len, c.chunkLeft), c.r.buffered)
      for i in 0..<took: dest[i] = c.r.peek[i]
      c.r.consume took
      c.chunkLeft -= took
      if c.chunkLeft == 0: c.chunk = csDataEnd
      return took
    of csHeader:
      var size = 0
      let after = parseChunkSize(c.r.peek, size)
      if after == ParseBad: raise SyntaxError
      if after == ParseIncomplete:
        needMore(c)
        continue
      c.r.consume after
      if size == 0:
        c.chunk = csTrailer
      else:
        c.chunkLeft = size
        c.chunk = csData
    of csDataEnd:
      let after = parseCrLf(c.r.peek)
      if after == ParseBad: raise SyntaxError  # chunk not closed where it claimed
      if after == ParseIncomplete:
        needMore(c)
        continue
      c.r.consume after
      c.chunk = csHeader
    of csTrailer:
      let after = parseTrailerEnd(c.r.peek)
      if after == ParseBad: raise SyntaxError
      if after == ParseIncomplete:
        needMore(c)
        continue
      c.r.consume after
      c.chunk = csDone
      return 0

proc readBody*(c: var HttpConn; dest: var openArray[char];
               dl = never): int {.passive, raises.} =
  ## Fills `dest` with body bytes, starting with whatever already arrived
  ## behind the head. The bytes copied — fewer than asked for only if the peer
  ## stopped, which the caller detects by comparing against `contentLength`.
  ##
  ## Only for `Content-Length` bodies. Ask `isChunked` first and use
  ## `readChunked` if it says so: guessing the framing is how two hops come to
  ## disagree about where a message ends. Nothing about it is HTTP, so it is
  ## the `Reader`'s read — the caller stops at `contentLength` bytes.
  c.r.tighten(dl)
  result = c.r.read(dest)

# --------------------------------------------------------------- writing ---

proc sendHead*(c: var HttpConn; m: HttpMsg;
               dl = never) {.passive, raises.} =
  ## Serialize `m`'s head into the writer's buffer and flush it. `headLen`
  ## sizes the buffer exactly, so this never writes, fails and retries.
  ##
  ## `ValueError` for a message with no head to write — one that was never
  ## built, or moved from — or with one that would not come out well formed:
  ## a CR/LF in a header value and the like (see `headLen`). A `BugError` is
  ## what it says: `headLen` and `writeHead` disagreed about the same tree.
  let need = headLen(m)
  if need <= 0: raise ValueError
  if writeHead(toOpenArray(c.w.scratch(need), 0, need - 1), 0, m) != need:
    raise BugError
  c.w.tighten(dl)
  c.w.flush()

proc sendBody*(c: var HttpConn; body: openArray[char];
               dl = never) {.passive, raises.} =
  ## Send a body already in memory, and make it visible to the peer before
  ## this returns.
  if body.len > 0:
    c.w.tighten(dl)
    c.w.write body
    c.w.flush()

proc sendChunk*(c: var HttpConn; data: openArray[char];
                dl = never) {.passive, raises.} =
  ## One chunk. An empty `data` is *not* written: a zero-length chunk is the
  ## end-of-body marker, so sending one here would end the body early.
  ## `endChunks` is how a body is ended.
  if data.len == 0: return
  let n = chunkOverhead(data.len) + data.len
  if writeChunk(toOpenArray(c.w.scratch(n), 0, n - 1), 0, data) != n:
    raise BugError
  c.w.tighten(dl)
  c.w.flush()

proc endChunks*(c: var HttpConn; dl = never) {.passive, raises.} =
  ## The zero chunk and the empty trailer section that end a chunked body.
  const LastChunkLen = 5            # "0\r\n\r\n"
  let n = writeLastChunk(toOpenArray(c.w.scratch(LastChunkLen), 0,
                                     LastChunkLen - 1), 0)
  if n != LastChunkLen: raise BugError
  c.w.tighten(dl)
  c.w.flush()

proc respond*(c: var HttpConn; status: int; body: openArray[char];
              dl = never) {.passive, raises.} =
  ## A complete response: status, `Content-Length`, `Connection`, and the
  ## body. The length is always sent — a response whose end the peer has to
  ## infer from a close is a response that cannot be followed by another.
  var m = initHttpMsg(c.tags)
  m.startResponse(status)
  m.addHeader(hContentLength, body.len)
  m.addHeader(hConnection, vKeepAlive)
  m.finish()
  sendHead(c, m, dl)
  sendBody(c, body, dl)

# ------------------------------------------------------------ keep-alive ---

proc renew*(c: var HttpConn; deadline: Deadline) =
  ## Start the next request's budget. A keep-alive connection serves many
  ## requests and each gets its own, or the first one would spend the whole
  ## connection's allowance on behalf of all of them.
  try:
    c.r.renew(deadline)
  except ErrorCode:
    discard

proc close*(c: var HttpConn) =
  ## Cancel anything still in flight and close the fd. Idempotent. Must run on
  ## the thread that submitted those ops — the ring's slot arenas are per-lane,
  ## so a connection belongs to its lane for life.
  if c.fdVal >= 0:
    c.fdVal = -1
    c.w.abort()
    try:
      c.r.close()
    except ErrorCode:
      discard
