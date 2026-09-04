# (c) 2026 Andreas Rumpf
#
# A buffered connection: the part of talking to a peer that no protocol owns.
#
# `std/http/httpconn` grew all of this on its way to HTTP/1.1 framing, and none
# of it is HTTP. A read buffer that compacts rather than reallocates, a ceiling
# so a peer cannot pick our memory use, one deadline that every operation runs
# under and that a caller can tighten but never widen, and a write that loops
# because a short write is the peer's window and not our business.
#
#   var s = initSocket(fd, afterMs(30_000))
#   while s.peek.len < 4:
#     if s.fill() <= 0: break
#   let magic = s.peek[0..3]
#   s.consume 4
#
# The two halves of the read side are `fill`, which puts bytes in, and
# `peek`/`consume`, which take them out. A parser is handed `peek` — a view
# into the buffer, so it does not slice, copy or own anything — and says how
# much of it it used.
#
# `.passive` throughout: these park on `std/ioring` and are resumed by a pool
# worker, so they read like blocking code and are not. The ones that can park
# are the ones that can time out, and those are the ones that `raises` — an
# `ErrorCode`, caught with an ordinary `try`. A failure is not in the return
# value: `fill` answers how many bytes arrived and `read` how many it copied,
# and `0` from either is the peer closing, which is an end and not a failure.
# Nothing here reports a problem as a negative number the caller has to
# remember to test.

import std / [ioring, assertions]

export ioring.Deadline, ioring.never, ioring.afterMs, ioring.after,
       ioring.earlier, ioring.monoNow

const
  ReadChunk* = 8 * 1024
    ## How much is asked for per `read`, and the buffer's initial size and
    ## growth step.
  MaxBuffered* = 64 * 1024
    ## Ceiling on one socket's read buffer. Past this the peer is made to wait
    ## rather than us to allocate: a protocol that needs a whole message
    ## resident enforces its own, smaller limit long before this one.

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

# ------------------------------------------------------------ the socket ---

type
  Socket* = object
    fd*: cint
    deadline*: Deadline
      ## This socket's budget. Set at construction and re-armed at whatever
      ## the protocol above calls a request boundary; every operation below
      ## takes it from here, and an argument can only tighten it.
    rbuf: seq[char]
    rlen: int          ## bytes held
    rpos: int          ## bytes already consumed from the front
    wbuf: seq[char]

proc `=destroy`*(s: Socket) =
  ## Closing is not something a caller has to remember. A `Socket` owns its
  ## fd, so the fd goes when the socket does — the leak that mattered was
  ## never the buffers, it was the descriptor on the error path nobody wrote.
  ##
  ## Like the explicit `close`, this must run on the thread that submitted the
  ## socket's in-flight ops, because the ring's slot arenas are per-lane. That
  ## is what a socket belonging to its lane for life buys: it also dies there.
  if s.fd >= 0: closeFd(s.fd)

proc `=wasMoved`*(s: var Socket) {.nodestroy, inline.} =
  ## The fd went with the destination, so this one must not close it. `-1`
  ## rather than the default zeroing: `0` is a perfectly good descriptor —
  ## stdin — and a moved-from socket that closes it is the kind of bug that
  ## surfaces somewhere else entirely.
  s.fd = -1
  `=wasMoved`(s.rbuf)
  `=wasMoved`(s.wbuf)

proc `=copy`*(dest: var Socket; src: Socket) {.error.}
  ## Move-only: two sockets over one fd means two owners of one descriptor,
  ## and whichever is destroyed first closes it under the other.

proc initSocket*(fd: cint; deadline: Deadline): Socket =
  ## `fd` must already be non-blocking. The deadline has no default: a socket
  ## with no budget is one a quiet peer can hold forever.
  Socket(fd: fd, deadline: deadline,
         rbuf: newSeq[char](ReadChunk), rlen: 0, rpos: 0,
         wbuf: newSeq[char](ReadChunk))

proc budget*(s: Socket; dl: Deadline): Deadline {.inline.} =
  ## The deadline an operation actually runs under: the socket's, or a tighter
  ## one the caller supplied. `earlier` means a caller can never widen the
  ## budget by passing a later instant.
  earlier(s.deadline, dl)

proc renew*(s: var Socket; deadline: Deadline) {.inline.} =
  ## Start a fresh budget. A kept-alive connection serves many exchanges and
  ## each gets its own, or the first would spend the whole socket's allowance
  ## on behalf of all of them.
  s.deadline = deadline

proc close*(s: var Socket) =
  ## Close now rather than at the end of the scope. Idempotent, and the
  ## destructor then has nothing left to do — which is what a protocol above
  ## wants when "the peer said goodbye" happens well before the socket's
  ## owner goes out of scope.
  if s.fd >= 0:
    closeFd(s.fd)
    s.fd = -1

# --------------------------------------------------------- the read side ---

proc buffered*(s: Socket): int {.inline.} =
  ## Bytes read but not yet consumed.
  s.rlen - s.rpos

proc peek*(s: Socket): openArray[char] =
  ## The bytes read but not yet consumed, as a view into the buffer. This is
  ## what a parser is handed: it neither slices nor owns anything, and what it
  ## does not consume stays put for the next read to extend.
  toOpenArray(s.rbuf, s.rpos, s.rlen - 1)

proc consume*(s: var Socket; n: int) {.inline.} =
  ## Drop the first `n` bytes of `peek` — what the parser just used.
  assert n >= 0 and n <= s.buffered, "consume past what is buffered"
  s.rpos += n

proc compact(s: var Socket) =
  ## Move the unconsumed tail to the front. Nothing has to be fixed up
  ## afterwards: every position a caller holds is an offset into `peek`, and
  ## the bytes and that offset move together.
  if s.rpos == 0: return
  let n = s.rlen - s.rpos
  # Through a local: assigning one element of a seq straight from another is
  # a mutable/immutable alias of the same object, which the compiler refuses.
  for i in 0..<n:
    let b = s.rbuf[s.rpos + i]
    s.rbuf[i] = b
  s.rlen = n
  s.rpos = 0

proc fill*(s: var Socket; dl = never): int {.passive, raises.} =
  ## One read into the buffer. The bytes added, or `0` at end of stream —
  ## which is a peer that has finished, not a failure, so it is a value and
  ## not a raise. Compacts first, then grows up to `MaxBuffered`.
  ##
  ## Raises `FullError` when the buffer is at its ceiling and still full: the
  ## peer has sent more of something than we agreed to hold, and growing
  ## further would let it pick our memory use. `TimeoutError` when the
  ## deadline arrived first, `IOError` otherwise.
  result = 0
  compact(s)
  if s.rlen >= s.rbuf.len:
    if s.rbuf.len >= MaxBuffered:
      raise FullError
    var bigger = newSeq[char](min(s.rbuf.len * 2, MaxBuffered))
    for i in 0..<s.rlen: bigger[i] = s.rbuf[i]
    s.rbuf = bigger
  let room = s.rbuf.len - s.rlen
  let n = readAsync(s.fd, addr s.rbuf[s.rlen], room, budget(s, dl))
  if n < 0: raise toErr(n)
  s.rlen += n
  result = n

proc take*(s: var Socket; dest: var openArray[char]; limit: int): int =
  ## Copy up to `limit` bytes of what is already buffered into `dest`, and
  ## consume them. No IO, so no deadline: this is the half of a read that a
  ## protocol doing its own framing wants after `fill` has put bytes in.
  result = s.buffered
  if result > limit: result = limit
  if result > dest.len: result = dest.len
  for i in 0..<result: dest[i] = s.rbuf[s.rpos + i]
  s.consume result

proc read*(s: var Socket; dest: var openArray[char]; dl = never): int {.passive, raises.} =
  ## Fill `dest`, starting with whatever is already buffered. The bytes
  ## copied — fewer than asked for only at end of stream. Raises what `fill`
  ## raises.
  result = 0
  let dl2 = budget(s, dl)
  let limit = dest.len
  var got = 0
  while got < limit:
    if s.buffered == 0:
      if fill(s, dl2) == 0: break         # end of stream
    got += s.take(toOpenArray(dest, got, limit - 1), limit - got)
  result = got

# -------------------------------------------------------- the write side ---

proc toErr*(n: int): ErrorCode {.inline.} =
  ## What a negative ring result means. Exported because a protocol above may
  ## have to decide for itself whether a `0` — the peer closing — is an end or
  ## a truncation, and wants the same mapping for the rest.
  if n == 0: EndOfStreamError
  elif n == IoTimedOut: TimeoutError
  else: IOError

proc writeAll*(s: var Socket; buf: pointer; len: int;
               dl = never) {.passive, raises.} =
  ## Write every byte or raise. A short write is normal — the peer's window is
  ## not our business — so it loops rather than reporting partial success that
  ## a caller would have to unpick. There is no partial success to report:
  ## either every byte went or this raised.
  let dl2 = budget(s, dl)
  var sent = 0
  while sent < len:
    let n = writeAsync(s.fd, cast[pointer](cast[uint](buf) + uint(sent)),
                       len - sent, dl2)
    # `0` is not an end here the way it is on the read side: a write that
    # placed nothing and reported no error has a peer that is gone.
    if n <= 0: raise toErr(n)
    sent += n

proc write*(s: var Socket; data: openArray[char];
            dl = never) {.passive, raises.} =
  ## Send bytes already in memory. Nothing is copied.
  if data.len == 0: return
  writeAll(s, addr data[0], data.len, dl)

proc scratch*(s: var Socket; n: int): var openArray[char] =
  ## A writable staging area of at least `n` bytes, kept between calls so
  ## serializing a message does not allocate per message. Valid until the next
  ## `scratch`; `flush` is what sends it.
  if s.wbuf.len < n:
    s.wbuf = newSeq[char](n)
  toOpenArray(s.wbuf, 0, n - 1)

proc flush*(s: var Socket; n: int; dl = never) {.passive, raises.} =
  ## Send the first `n` bytes of the staging area in one write. One write and
  ## not several because a frame split across writes is a frame a peer can be
  ## left half-way through.
  if n <= 0: return
  writeAll(s, addr s.wbuf[0], n, dl)
