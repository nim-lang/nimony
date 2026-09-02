# (c) 2026 Andreas Rumpf
#
# HTTP over a connection: buffering, framing, keep-alive. See
# doc/internals/http.md.
#
# This is the first layer that does IO, and it does it the way the design
# says: `.passive` procs that park on `std/ioring` rather than callbacks, and
# a deadline on every operation that can park. Reading a head looks like
# blocking code and is not.
#
#   var c = initHttpConn(fd, afterMs(30_000))
#   var m = initHttpMsg()
#   if c.readRequest(m) == Success:
#     discard c.respond(200, "hello\n")
#
# What is here: the read buffer and its compaction, head reading in both
# directions, bodies framed either by `Content-Length` or by chunking, and
# writing.

import ./httpmsg
import ./httpparse
import ./httpwire
import std / [ioring, assertions]

const
  ReadChunk* = 8 * 1024
    ## How much is asked for per `read`. Also the buffer's growth step.
  MaxBuffered* = 64 * 1024
    ## Ceiling on one connection's read buffer. A head is capped far below
    ## this by `MaxHeadLen`; the slack is for a body already in flight behind
    ## it. Past this the peer is made to wait rather than us to allocate.

# ------------------------------------------------------- ring primitives ---

proc readAsync*(fd: cint; buf: pointer; len: int; dl: Deadline): int {.passive.} =
  ## One `read`, parked on the ring. The bytes read, `0` at end of stream, or
  ## negative on error — `IoTimedOut` when the deadline arrived first.
  ##
  ## `result` is initialised before its address is taken because the ring
  ## writes through that pointer when it completes, and the compiler will not
  ## hand out the address of something it cannot prove is initialised.
  result = 0
  let c = delay()
  discard submitRead(fd, buf, len, dl, c, addr result)
  suspend()

proc writeAsync*(fd: cint; buf: pointer; len: int; dl: Deadline): int {.passive.} =
  ## One `write`, parked on the ring.
  result = 0
  let c = delay()
  discard submitWrite(fd, buf, len, dl, c, addr result)
  suspend()

# ------------------------------------------------------------ connection ---

type
  ChunkState = enum
    csHeader      ## at a chunk-size line
    csData        ## inside a chunk's data, `chunkLeft` bytes to go
    csDataEnd     ## at the CRLF that closes a chunk's data
    csTrailer     ## after the zero chunk, in the trailer section
    csDone        ## the body has ended

  HttpConn* = object
    fd*: cint
    deadline*: Deadline
      ## This connection's budget. Set at construction and re-armed at each
      ## request boundary; every operation below takes it from here, and an
      ## optional argument can only tighten it.
    rbuf: seq[char]
    rlen: int          ## bytes held
    rpos: int          ## bytes already consumed from the front
    scan: HeadScanner
    wbuf: seq[char]
    chunk: ChunkState
    chunkLeft: int

proc initHttpConn*(fd: cint; deadline: Deadline): HttpConn =
  ## `fd` must already be non-blocking. The deadline has no default: a
  ## connection with no budget is one a quiet peer can hold forever.
  HttpConn(fd: fd, deadline: deadline,
           rbuf: newSeq[char](ReadChunk), rlen: 0, rpos: 0,
           scan: default(HeadScanner), wbuf: newSeq[char](ReadChunk),
           chunk: csHeader, chunkLeft: 0)

proc budget(c: HttpConn; dl: Deadline): Deadline {.inline.} =
  ## The deadline an operation actually runs under: the connection's, or a
  ## tighter one the caller supplied. `earlier` means a caller can never widen
  ## the budget by passing a later instant.
  earlier(c.deadline, dl)

proc buffered*(c: HttpConn): int {.inline.} =
  ## Bytes read but not yet consumed — a pipelined request, or the start of a
  ## body that arrived with its head.
  c.rlen - c.rpos

proc compact(c: var HttpConn) =
  ## Move the unconsumed tail to the front. The scanner's position moves with
  ## it, or it would go on pointing into bytes that have shifted.
  if c.rpos == 0: return
  let n = c.rlen - c.rpos
  # Through a local: assigning one element of a seq straight from another is
  # a mutable/immutable alias of the same object, which the compiler refuses.
  for i in 0..<n:
    let b = c.rbuf[c.rpos + i]
    c.rbuf[i] = b
  c.rlen = n
  c.scan.pos = if c.scan.pos > c.rpos: c.scan.pos - c.rpos else: 0
  c.rpos = 0

proc fill(c: var HttpConn; dl: Deadline): int {.passive.} =
  ## One read into the buffer. Bytes added, `0` at end of stream, or negative
  ## on error / timeout. Grows the buffer up to `MaxBuffered`, then reports
  ## `FullError` rather than growing without bound at a peer's choosing.
  result = 0
  compact(c)
  if c.rlen >= c.rbuf.len:
    if c.rbuf.len >= MaxBuffered:
      return -1
    var bigger = newSeq[char](min(c.rbuf.len * 2, MaxBuffered))
    for i in 0..<c.rlen: bigger[i] = c.rbuf[i]
    c.rbuf = bigger
  let room = c.rbuf.len - c.rlen
  let n = readAsync(c.fd, addr c.rbuf[c.rlen], room, dl)
  if n > 0: c.rlen += n
  result = n

# --------------------------------------------------------------- reading ---

proc toErr(n: int): ErrorCode {.inline.} =
  if n == 0: EndOfStreamError
  elif n == IoTimedOut: TimeoutError
  else: IOError

proc readHead(c: var HttpConn; m: var HttpMsg; asRequest: bool;
              dl: Deadline): ErrorCode {.passive.} =
  ## Read until a complete head has arrived, then parse it in one pass.
  ##
  ## A head is only parsed once whole, so a peer dribbling it one byte at a
  ## time costs one `findHeadEnd` resumption per read rather than a rescan —
  ## which is the difference between O(n) and O(n²) for exactly the peer who
  ## would try it.
  result = Success
  let dl2 = budget(c, dl)
  while true:
    let hasHead = findHeadEnd(c.scan, toOpenArray(c.rbuf, 0, c.rlen - 1), c.rpos)
    if hasHead == ParseBad: return ContentTooLong
    if hasHead >= 0:
      let stop =
        if asRequest:
          parseRequestHead(toOpenArray(c.rbuf, 0, c.rlen - 1), c.rpos, m)
        else:
          parseResponseHead(toOpenArray(c.rbuf, 0, c.rlen - 1), c.rpos, m)
      if stop < 0:
        # The head is complete, so `ParseIncomplete` here is not "read more",
        # it is a head that ends without being valid.
        return SyntaxError
      c.rpos = stop
      return Success
    let n = fill(c, dl2)
    if n <= 0: return toErr(n)

proc readRequest*(c: var HttpConn; m: var HttpMsg;
                  dl = never): ErrorCode {.passive.} =
  ## The next request head on this connection. `m` must be empty; `reset` it
  ## between requests to reuse its buffer, which is the keep-alive path.
  readHead(c, m, true, dl)

proc readResponse*(c: var HttpConn; m: var HttpMsg;
                   dl = never): ErrorCode {.passive.} =
  ## The next response head — the client side of the same loop.
  readHead(c, m, false, dl)

proc isChunked*(m: var HttpMsg): bool {.inline.} =
  ## Whether the body is chunk-framed. The parser resolves a known
  ## `Transfer-Encoding` to a tag, so this is an integer compare.
  m.getTag(hTransferEncoding) == tag(vChunked)

proc bodyLength*(m: var HttpMsg): int {.inline.} =
  ## Declared body length, or `-1` when there is none or it is chunked.
  if isChunked(m): -1 else: m.contentLength

proc beginBody*(c: var HttpConn) {.inline.} =
  ## Arm the chunk reader for a new body. Call after reading a head whose
  ## `isChunked` is true, before the first `readChunked`.
  c.chunk = csHeader
  c.chunkLeft = 0

proc readChunked*(c: var HttpConn; dest: var openArray[char];
                  dl = never): int {.passive.} =
  ## The next piece of a chunk-framed body: bytes copied, `0` once the body
  ## has ended, negative on error.
  ##
  ## `0` means the whole body is over — the zero chunk and its trailers have
  ## been consumed and the connection is positioned at whatever follows, so
  ## it can be reused. It never means "nothing right now"; the loop keeps
  ## reading until it has either bytes or the end.
  result = 0
  let dl2 = budget(c, dl)
  while true:
    case c.chunk
    of csDone:
      return 0
    of csData:
      if c.buffered == 0:
        let n = fill(c, dl2)
        if n < 0: return n
        if n == 0: return -1          # truncated body: the peer went away
        continue
      let avail = c.buffered
      var take = if avail < c.chunkLeft: avail else: c.chunkLeft
      if take > dest.len: take = dest.len
      for i in 0..<take: dest[i] = c.rbuf[c.rpos + i]
      c.rpos += take
      c.chunkLeft -= take
      if c.chunkLeft == 0: c.chunk = csDataEnd
      return take
    of csHeader:
      var size = 0
      let after = parseChunkSize(toOpenArray(c.rbuf, 0, c.rlen - 1), c.rpos, size)
      if after == ParseBad: return -1
      if after == ParseIncomplete:
        let n = fill(c, dl2)
        if n < 0: return n
        if n == 0: return -1
        continue
      c.rpos = after
      if size == 0:
        c.chunk = csTrailer
      else:
        c.chunkLeft = size
        c.chunk = csData
    of csDataEnd:
      let after = parseCrLf(toOpenArray(c.rbuf, 0, c.rlen - 1), c.rpos)
      if after == ParseBad: return -1     # chunk not closed where it claimed
      if after == ParseIncomplete:
        let n = fill(c, dl2)
        if n < 0: return n
        if n == 0: return -1
        continue
      c.rpos = after
      c.chunk = csHeader
    of csTrailer:
      let after = parseTrailerEnd(toOpenArray(c.rbuf, 0, c.rlen - 1), c.rpos)
      if after == ParseBad: return -1
      if after == ParseIncomplete:
        let n = fill(c, dl2)
        if n < 0: return n
        if n == 0: return -1
        continue
      c.rpos = after
      c.chunk = csDone
      return 0

proc readBody*(c: var HttpConn; dest: var openArray[char];
               dl = never): int {.passive.} =
  ## Fills `dest` with body bytes, starting with whatever already arrived
  ## behind the head. Bytes copied, or negative on error.
  ##
  ## Only for `Content-Length` bodies. Ask `isChunked` first and use
  ## `readChunked` if it says so: guessing the framing is how two hops come to
  ## disagree about where a message ends.
  result = 0
  let dl2 = budget(c, dl)
  let limit = dest.len
  var got = 0
  while got < limit:
    if c.buffered == 0:
      let n = fill(c, dl2)
      if n < 0: return n
      if n == 0: break                    # end of stream
    let avail = c.buffered
    let take = if avail < limit - got: avail else: limit - got
    for i in 0..<take: dest[got + i] = c.rbuf[c.rpos + i]
    c.rpos += take
    got += take
  result = got

# --------------------------------------------------------------- writing ---

proc writeAll(c: var HttpConn; buf: pointer; len: int;
              dl: Deadline): ErrorCode {.passive.} =
  ## Write every byte or fail. A short write is normal — the peer's window is
  ## not our business — so it loops rather than reporting partial success that
  ## a caller would have to unpick.
  result = Success
  var sent = 0
  while sent < len:
    let n = writeAsync(c.fd, cast[pointer](cast[uint](buf) + uint(sent)),
                       len - sent, dl)
    if n <= 0: return toErr(n)
    sent += n

proc sendHead*(c: var HttpConn; m: var HttpMsg;
               dl = never): ErrorCode {.passive.} =
  ## Serialize `m`'s head into the connection's write buffer and send it.
  ## `headLen` sizes the buffer exactly, so this never writes, fails and
  ## retries.
  result = Success
  let need = headLen(m)
  if need <= 0: return ValueError
  if c.wbuf.len < need:
    c.wbuf = newSeq[char](need)
  let n = writeHead(c.wbuf, 0, m)
  if n != need: return BugError
  result = writeAll(c, addr c.wbuf[0], n, budget(c, dl))

proc sendBody*(c: var HttpConn; body: openArray[char];
               dl = never): ErrorCode {.passive.} =
  ## Send a body already in memory. Nothing is copied.
  result = Success
  if body.len == 0: return Success
  result = writeAll(c, addr body[0], body.len, budget(c, dl))

proc sendChunk*(c: var HttpConn; data: openArray[char];
                dl = never): ErrorCode {.passive.} =
  ## One chunk. An empty `data` is *not* written: a zero-length chunk is the
  ## end-of-body marker, so sending one here would end the body early.
  ## `endChunks` is how a body is ended.
  result = Success
  if data.len == 0: return Success
  let n = data.len
  let need = chunkOverhead(n) + n
  if c.wbuf.len < need:
    c.wbuf = newSeq[char](need)
  var j = writeChunkHeader(c.wbuf, 0, n)
  if j < 0: return BugError
  for i in 0..<n: c.wbuf[j + i] = data[i]
  j += n
  j = writeChunkEnd(c.wbuf, j)
  if j < 0: return BugError
  # Header, data and trailing CRLF go out as one write: a chunk split across
  # writes is a chunk a peer can be left half-way through.
  result = writeAll(c, addr c.wbuf[0], j, budget(c, dl))

proc endChunks*(c: var HttpConn; dl = never): ErrorCode {.passive.} =
  ## The zero chunk and the empty trailer section that end a chunked body.
  result = Success
  var buf = default(array[8, char])
  let n = writeLastChunk(buf, 0)
  if n < 0: return BugError
  result = writeAll(c, addr buf[0], n, budget(c, dl))

proc respond*(c: var HttpConn; status: int; body: openArray[char];
              dl = never): ErrorCode {.passive.} =
  ## A complete response: status, `Content-Length`, `Connection`, and the
  ## body. The length is always sent — a response whose end the peer has to
  ## infer from a close is a response that cannot be followed by another.
  result = Success
  var m = initHttpMsg()
  m.startResponse(status)
  m.addHeader(hContentLength, body.len)
  m.addHeader(hConnection, vKeepAlive)
  m.finish()
  let dl2 = budget(c, dl)
  result = sendHead(c, m, dl2)
  if result == Success:
    result = sendBody(c, body, dl2)

# ------------------------------------------------------------ keep-alive ---

proc renew*(c: var HttpConn; deadline: Deadline) {.inline.} =
  ## Start the next request's budget. A keep-alive connection serves many
  ## requests and each gets its own, or the first one would spend the whole
  ## connection's allowance on behalf of all of them.
  c.deadline = deadline

proc close*(c: var HttpConn) =
  ## Cancel anything still in flight on this fd and close it. Must run on the
  ## thread that submitted those ops — the ring's slot arenas are per-lane,
  ## so a connection belongs to its lane for life.
  if c.fd >= 0:
    closeFd(c.fd)
    c.fd = -1
