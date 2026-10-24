#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Streams: one `Reader`, one `Writer` (nim-lang/nimony#2635).
##
## A network connection is as unremarkable as a file. Every constructor returns
## a `Reader`, a `Writer` or both, and after construction the interface is the
## same whatever is underneath:
##
## .. code-block:: nim
##   var r = openReader(path"log.txt")
##   echo r.readLine()
##
##   let (r, w) = connect("cache.local:6379", "PING\r\n")
##   echo r.readLine()                  # +PONG
##
##   var w = openWriter()
##   w.setAdapter gzipAdapter(fmtGzip)
##   w.write data
##   let gz = extract(w)
##
## Pieces:
##
## * `Reader` and `Writer` are concrete, move-only values. A pair returned by
##   one constructor shares a `Core`, a `ref` to the descriptor and the TLS
##   engine if any, which closes the descriptor when the last end is gone.
## * Each end has ONE adapter slot, `adapter` (compression, HTTP framing, …),
##   set with `setAdapter`. Encryption is not an adapter: a `TlsEngine` lives in
##   the core, because TLS is a property of the connection, not of a direction.
##   So the write path is caller -> adapter -> TLS -> descriptor and the read path
##   the reverse; compression below encryption cannot be written.
## * `read` answering `0` is the end of the data. Everything that goes wrong
##   raises an `ErrorCode`.
## * Streams close themselves. A writer's destructor finishes it (adapter
##   trailer, TLS `close_notify`, `shutdown(SHUT_WR)`) and swallows errors; call
##   `close` to see them. `abort` ends a writer without the clean finish.
## * `deadline` reads the standing I/O deadline; `renew` replaces it, or clears
##   it with `never`. A connection's reader and writer share that deadline.
##   Connection constructors take `dl` for setup only; the returned streams
##   start with `never`, so setup does not become a connection lifetime limit.
##
## * `pipe` is an in-process `Reader`/`Writer` pair over a ring; waiting on
##   it parks the task, it does not block a thread. `std/channels` builds
##   typed channels on its record form, `recordPipe`.
##
## Not here yet (see the issue): `openReadWrite`, `startTls`, deadlines for
## waits on a pipe, `sendfile`/`splice`/kTLS fast paths in `copy`, and
## cross-thread close of a descriptor another lane is parked on.

import std/[ioring, socket, paths, commonio, strutils, threadpool, ticketlocks]
import std/compress/gzip

export ioring.Deadline, ioring.never, ioring.afterMs, ioring.earlier,
       ioring.monoNow
export commonio.FileMode
export gzip.Format

when defined(posix):
  proc posixShutdown(fd: cint; how: cint): cint {.importc: "shutdown",
    header: "<sys/socket.h>", sideEffect.}

const
  ChunkSize = 16 * 1024       ## one read from below; one TLS record's worth
  FlushAt = 64 * 1024         ## a writer pushes its buffer down past this

proc addBytes(dest: var string; src: openArray[char]) {.inline.} =
  ## `dest.add` for a slice: one capacity check and one `copyMem`.
  if src.len > 0:
    let old = dest.len
    copyMem(beginStore(dest, old + src.len, old), addr src[0], src.len)
    endStore(dest)

proc copyBytes(dest: var openArray[char]; src: openArray[char]) {.inline.} =
  ## The first `src.len` bytes of `dest` := `src`.
  copyMem(addr dest[0], addr src[0], src.len)

# ------------------------------------------------------------ extension points

type
  TlsEngine* = ref object of RootObj
    ## A TLS implementation as a buffer-driven state machine — the shape every
    ## TLS library already has (BearSSL's engine, OpenSSL with a BIO pair,
    ## rustls' `Connection`). The stream core does all the I/O: it moves
    ## records between the engine and the descriptor. `std/tls` provides the
    ## default implementation, `newTlsEngine`.
    ##
    ## An engine serves one connection.

  Adapter* = ref object of RootObj
    ## What sits in a stream's `adapter` slot: a one-direction transformation of
    ## the byte stream. Subclass it and override `run`.

method start*(e: TlsEngine; host: string) {.base, raises.} =
  ## Begin the handshake for `host` (queues the first flight).
  raise UnimplementedOperation

method recordsIn*(e: TlsEngine; src: openArray[char]; used: var int) {.base, raises.} =
  ## Bytes from the network. Reports how many were taken.
  raise UnimplementedOperation

method recordsOut*(e: TlsEngine; dest: var string) {.base, raises.} =
  ## Appends whatever must go to the network now.
  raise UnimplementedOperation

method appIn*(e: TlsEngine; src: openArray[char]; used: var int) {.base, raises.} =
  ## Plaintext to send; its records come out of `recordsOut`.
  raise UnimplementedOperation

method appOut*(e: TlsEngine; dest: var string) {.base, raises.} =
  ## Appends the plaintext received so far.
  raise UnimplementedOperation

method close*(e: TlsEngine) {.base, raises.} =
  ## Queue `close_notify`.
  raise UnimplementedOperation

method isConnected*(e: TlsEngine): bool {.base.} =
  ## The handshake has completed and application data may flow.
  result = false

method isPeerClosed*(e: TlsEngine): bool {.base.} =
  ## The peer sent `close_notify`: what `appOut` has handed out is all of it.
  result = false

method run*(a: Adapter; src: openArray[char]; used: var int;
            dest: var string; final: bool) {.base, raises.} =
  ## Transform a prefix of `src`, append the output to `dest` and report in
  ## `used` how many bytes of `src` were taken. `final` means no more input
  ## will follow: write the trailer, or check that the stream really ended.
  raise UnimplementedOperation

method attach*(a: Adapter; reading: bool) {.base, raises.} =
  ## Called by `setAdapter` with the direction the adapter now serves. An
  ## adapter that only works one way raises for the other.
  discard

method hasMore*(a: Adapter): bool {.base.} =
  ## False for a read adapter whose stream has ended on its own — an HTTP body
  ## after `Content-Length` bytes or the last chunk — although the source has
  ## not. The reader then returns `0`, and the bytes below stay where they are.
  result = true

# --------------------------------------------------------------------- ring --
#
# `pipe` and `recordPipe` share one in-process ring. Waiting on it is parking: a
# side that cannot go on leaves its continuation in the ring and suspends, and
# the side that changes the state takes it out and submits it to the pool. So
# there is no condition variable and a waiting task costs no thread. The spin
# lock only guards indices and waiter lists, and it is never held across a
# park or a `submit` (which may run the continuation inline).

type
  DropProc = proc (p: pointer) {.nimcall.}

  RingObj = object
    lock: TicketLock
    data: ptr UncheckedArray[char]   ## `cap` bytes
    cap, head, len: int    ## in bytes
    recSize: int           ## 1 for a byte pipe, else see `recordPipe`
    readerGone, writerGone: bool
    recvWaiters, sendWaiters: seq[Continuation]
    drop: nil DropProc     ## destroys a record left behind; nil for bytes

  Ring = ref RingObj
    ## A record ring's capacity is a multiple of `recSize` and every transfer is
    ## a whole record, so a record never wraps around the end.

proc `=destroy`(g: RingObj) =
  if g.data != nil: dealloc(g.data)
  `=destroy`(g.recvWaiters)
  `=destroy`(g.sendWaiters)

proc `=copy`(dest: var RingObj; src: RingObj) {.error.}

proc newRing(capacity, recSize: int): Ring =
  Ring(lock: TicketLock(), data: cast[ptr UncheckedArray[char]](alloc(capacity)),
       cap: capacity, head: 0, len: 0, recSize: recSize,
       readerGone: false, writerGone: false,
       recvWaiters: @[], sendWaiters: @[], drop: nil)

proc takeWaiters(ws: var seq[Continuation]; n: int; dest: var seq[Continuation]) =
  ## The `n` longest waiting, oldest first.
  let k = min(n, ws.len)
  if k > 0:
    for i in 0 ..< k: dest.add ws[i]
    var rest: seq[Continuation] = @[]
    for i in k ..< ws.len: rest.add ws[i]
    ws = rest

proc resume(ks: seq[Continuation]) =
  for k in ks: submit(k)

proc enlist(g: Ring; k: Continuation; sending: bool) =
  ## Parks `k` until the ring changes in its favour. Between the caller's check
  ## and this lock the other side may already have acted; then `k` runs at once.
  var ready = false
  acquire g.lock
  if sending:
    ready = g.readerGone or g.cap - g.len >= g.recSize
    if not ready: g.sendWaiters.add k
  else:
    ready = g.writerGone or g.len >= g.recSize
    if not ready: g.recvWaiters.add k
  release g.lock
  if ready: submit(k)

proc park(g: Ring; sending: bool) {.passive.} =
  let k = delay()
  enlist(g, k, sending)
  suspend()

proc ringWrite(g: Ring; data: openArray[char]; whole: bool): bool {.passive.} =
  ## Copies `data` into the ring, waiting for room. `whole`: all of it at
  ## once, as one record. False when the reader is gone; then the bytes that
  ## did not fit yet are not sent.
  result = true
  var pos = 0
  while result and pos < data.len:
    var wake: seq[Continuation] = @[]
    var n = 0
    acquire g.lock
    if g.readerGone:
      result = false
    else:
      let cap = g.cap
      let free = cap - g.len
      let want = data.len - pos
      n = if whole: (if free >= want: want else: 0) else: min(free, want)
      if n > 0:
        let tail = (g.head + g.len) mod cap
        let first = min(n, cap - tail)
        copyMem(addr g.data[tail], addr data[pos], first)
        if n > first: copyMem(addr g.data[0], addr data[pos + first], n - first)
        g.len += n
        takeWaiters(g.recvWaiters, g.len div g.recSize, wake)
    release g.lock
    resume wake
    if n > 0: pos += n
    elif result: park(g, true)

proc ringRead(g: Ring; p: pointer; maxLen: int; whole: bool): int {.passive.} =
  ## Up to `maxLen` bytes into `p`, waiting for them; `whole`: exactly one
  ## record. `0` once the writer is gone and the ring is empty.
  let dest = cast[ptr UncheckedArray[char]](p)
  result = -1
  while result < 0:
    var wake: seq[Continuation] = @[]
    acquire g.lock
    if g.len >= g.recSize:
      let cap = g.cap
      let n = if whole: g.recSize else: min(g.len, maxLen)
      let first = min(n, cap - g.head)
      copyMem(addr dest[0], addr g.data[g.head], first)
      if n > first: copyMem(addr dest[first], addr g.data[0], n - first)
      g.head = (g.head + n) mod cap
      g.len -= n
      takeWaiters(g.sendWaiters, (cap - g.len) div g.recSize, wake)
      result = n
    elif g.writerGone:
      result = 0
    release g.lock
    resume wake
    if result < 0: park(g, false)

proc takeLeft(g: Ring): seq[char] =
  ## What is still in the ring, with the lock held; whole records never wrap.
  result = newSeq[char](g.len)
  if g.len > 0: copyMem(addr result[0], addr g.data[g.head], g.len)
  g.head = 0
  g.len = 0

proc dropLeft(g: Ring; left: var seq[char]) =
  ## Destroys the whole records in `left`, which no lock protects any more.
  if g.drop != nil:
    var i = 0
    while i + g.recSize <= left.len:
      g.drop(addr left[i])
      i += g.recSize

proc ringReaderGone(g: Ring) =
  ## Nobody can take what is in the ring, and nothing more gets in: senders
  ## fail from now on, and what they sent before is destroyed here.
  var wake: seq[Continuation] = @[]
  var left: seq[char] = @[]
  acquire g.lock
  if not g.readerGone:
    g.readerGone = true
    takeWaiters(g.sendWaiters, g.sendWaiters.len, wake)
    left = takeLeft(g)
  release g.lock
  resume wake
  dropLeft(g, left)

proc ringWriterGone(g: Ring) =
  ## Readers drain what is left, then see the end.
  var wake: seq[Continuation] = @[]
  acquire g.lock
  g.writerGone = true
  takeWaiters(g.recvWaiters, g.recvWaiters.len, wake)
  release g.lock
  resume wake

# --------------------------------------------------------------------- core --

type
  CoreKind* = enum
    ckFd,          ## a file or a socket
    ckString,      ## the bytes of a string (read only)
    ckMemory       ## an in-memory sink (write only), see `extract`
    ckPipe         ## the in-process ring of `pipe` and `recordPipe`

  CoreObj* = object
    kind: CoreKind
    fd: cint
    isSocket*: bool
    tls*: nil TlsEngine
    data: string                 ## ckString: the source; ckMemory: the sink
    dataPos: int
    peerDone: bool               ## below the reader: the source has ended
    shutDown: bool               ## our direction has been ended
    dl*: Deadline
    ring*: nil Ring              ## ckPipe only

  Core* = ref CoreObj
    ## Shared by the ends one constructor returns. They are move-only, so the
    ## (atomic) reference count changes only when an end is created or dies.

proc `=destroy`(c: CoreObj) =
  if c.kind == ckFd and c.fd >= 0:
    closeFd(c.fd)
  let g = c.ring
  if g != nil and g.len > 0:
    # The reader never existed or never went away properly: last chance.
    var left = takeLeft(g)
    dropLeft(g, left)
  `=destroy`(c.tls)
  `=destroy`(c.data)
  `=destroy`(c.ring)

proc newCore*(kind: CoreKind; fd: cint): Core =
  Core(kind: kind, fd: fd, isSocket: false, tls: nil, data: "", dataPos: 0,
       peerDone: false, shutDown: false, dl: never, ring: nil)

proc endWriting*(c: Core) =
  ## Our direction is over: the peer sees end of data and may still answer.
  if c.isSocket and not c.shutDown and c.fd >= 0:
    when defined(posix):
      discard posixShutdown(c.fd, cint(1))      # SHUT_WR
  let g = c.ring
  if g != nil and not c.shutDown: ringWriterGone(g)
  c.shutDown = true

proc toErr(n: int): ErrorCode {.inline.} =
  if n == 0: EndOfStreamError
  elif n == IoTimedOut: TimeoutError
  else: IOError

proc writeFd(c: Core; data: openArray[char]) {.passive, raises.} =
  var sent = 0
  while sent < data.len:
    let n = writeAsync(c.fd, addr data[sent], data.len - sent, c.dl)
    if n <= 0: raise toErr(n)
    sent += n

proc sendRecords(c: Core; e: TlsEngine) {.passive, raises.} =
  ## Whatever the engine `e` (`c.tls`) has queued for the network.
  var recs = ""
  e.recordsOut(recs)
  if recs.len > 0: writeFd(c, recs)

proc coreSend(c: Core; data: openArray[char]) {.passive, raises.} =
  case c.kind
  of ckMemory:
    c.data.addBytes data
  of ckString:
    raise BadOperation
  of ckPipe:
    let g = c.ring
    if g == nil or not ringWrite(g, data, false): raise BadDescriptor
  of ckFd:
    let e = c.tls
    if e != nil:
      var pos = 0
      while pos < data.len:
        var used = 0
        e.appIn(toOpenArray(data, pos, data.len - 1), used)
        if used <= 0: raise ProtocolError
        pos += used
      sendRecords(c, e)
    else:
      writeFd(c, data)

proc readFd(c: Core; dest: var string): int {.passive, raises.} =
  ## Appends one read's worth; `0` at the end of the source. The kernel writes
  ## straight into `dest`'s spare room: no staging buffer, no zero-fill.
  let old = dest.len
  let n = readAsync(c.fd, beginStore(dest, old + ChunkSize, old), ChunkSize, c.dl)
  dest.setLen old + max(n, 0)            # a shrink: keeps the capacity, syncs the prefix
  if n < 0: raise toErr(n)
  result = n

proc coreRecv(c: Core; dest: var string): int {.passive, raises.} =
  ## Appends bytes from below the adapter; `0` once the source has ended.
  result = 0
  if c.peerDone: return
  case c.kind
  of ckString:
    if c.dataPos < c.data.len:
      dest.addBytes toOpenArray(c.data, c.dataPos, c.data.len - 1)
    result = c.data.len - c.dataPos
    c.dataPos = c.data.len
    if result == 0: c.peerDone = true
  of ckMemory:
    raise BadOperation
  of ckPipe:
    let g = c.ring
    if g == nil: raise BadOperation
    let old = dest.len
    result = ringRead(g, beginStore(dest, old + ChunkSize, old), ChunkSize, false)
    dest.setLen old + result
    if result == 0: c.peerDone = true
  of ckFd:
    let e = c.tls
    if e == nil:
      result = readFd(c, dest)
      if result == 0: c.peerDone = true
    else:
      while true:
        let old = dest.len
        e.appOut(dest)
        if dest.len > old: return dest.len - old
        if e.isPeerClosed:
          c.peerDone = true
          return 0
        sendRecords(c, e)               # e.g. the answer to a KeyUpdate
        var recs = ""
        if readFd(c, recs) == 0:
          # The connection ended without `close_notify`: a truncation anyone
          # on the path can cause, so it is not the end of the data.
          raise EndOfStreamError
        var used = 0
        e.recordsIn(recs, used)

proc tlsHandshake*(c: Core; e: TlsEngine; host: string) {.passive, raises.} =
  e.start(host)
  while not e.isConnected:
    sendRecords(c, e)
    var recs = ""
    if readFd(c, recs) == 0: raise EndOfStreamError
    var used = 0
    e.recordsIn(recs, used)
  sendRecords(c, e)                    # our Finished

# ------------------------------------------------------------ Reader, Writer --

type
  Reader* = object
    ## Move-only: one owner per end, which is what keeps `Core`'s reference
    ## count still while bytes flow.
    core: nil Core
    buf: string          ## decoded bytes not yet handed out start at `pos`
    pos: int
    raw: string          ## bytes from below not yet taken by the adapter
    adapter: nil Adapter
    eof: bool            ## the source below has ended

  Writer* = object
    ## Move-only, like `Reader`.
    core*: nil Core
    buf: string          ## caller bytes not yet pushed through the adapter
    adapter: nil Adapter
    finished*: bool

proc `=destroy`*(r: Reader) =
  let c = r.core
  if c != nil:
    let g = c.ring
    if g != nil: ringReaderGone(g)
  `=destroy`(r.core)
  `=destroy`(r.buf)
  `=destroy`(r.raw)
  `=destroy`(r.adapter)

proc `=dup`*(x: Reader): Reader {.error.}

proc `=copy`*(dest: var Reader; src: Reader) {.error.}

proc finishImpl(w: var Writer; clean: bool) {.passive, raises.}

proc `=destroy`*(w: Writer) =
  if w.core != nil:
    let wp = cast[ptr Writer](unsafeAddr w)
    if not w.finished:
      # A destructor cannot raise: a failed final flush is not reported here.
      # Code that must know the data arrived calls `close`.
      try:
        finishImpl(wp[], true)
      except:
        discard
  `=destroy`(w.core)
  `=destroy`(w.buf)
  `=destroy`(w.adapter)

proc `=dup`*(x: Writer): Writer {.error.}

proc `=copy`*(dest: var Writer; src: Writer) {.error.}

proc initReader*(c: Core): Reader =
  Reader(core: c, buf: "", pos: 0, raw: "", adapter: nil, eof: false)

proc initWriter*(c: Core): Writer =
  Writer(core: c, buf: "", adapter: nil, finished: false)

# ----------------------------------------------------------------- deadlines --

proc deadline*(r: Reader): Deadline {.raises.} =
  ## The standing deadline for I/O on the underlying descriptor. Bytes already
  ## buffered and in-memory streams do not wait on it. Raises `BadOperation`
  ## for a closed or default-constructed reader.
  let c = r.core
  if c == nil: raise BadOperation
  result = c.dl

proc deadline*(w: Writer): Deadline {.raises.} =
  ## The writer's standing I/O deadline, shared with its paired reader.
  ## Raises `BadOperation` for a closed or default-constructed writer.
  let c = w.core
  if c == nil: raise BadOperation
  result = c.dl

proc renew*(r: var Reader; deadline: Deadline) {.raises.} =
  ## Replace the standing I/O deadline; `never` clears it. A paired writer
  ## observes the same deadline. Raises `BadOperation` if the reader is closed
  ## or default-constructed. This does not resume a failed partial write.
  let c = r.core
  if c == nil: raise BadOperation
  c.dl = deadline

proc renew*(w: var Writer; deadline: Deadline) {.raises.} =
  ## Replace the shared I/O deadline through the writer. Raises `BadOperation`
  ## if the writer is closed or default-constructed.
  let c = w.core
  if c == nil: raise BadOperation
  c.dl = deadline

# ------------------------------------------------------------------ reading --

proc buffered*(r: Reader): int {.inline.} =
  ## Decoded bytes not yet consumed.
  r.buf.len - r.pos

proc peek*(r: Reader): openArray[char] =
  ## A view of the decoded bytes not yet consumed. No I/O or copying.
  ## Reader mutations invalidate the view.
  toOpenArray(r.buf, r.pos, r.buf.len - 1)

proc consume*(r: var Reader; n: int) {.inline,
    requires: n >= 0 and n <= r.buffered.} =
  ## Drop the first `n` bytes of `peek`, without I/O.
  r.pos += n

proc refill(r: var Reader): bool {.passive, raises.} =
  ## More decoded bytes in `buf`; `false` at the end of the data.
  if r.pos > 0:
    r.buf = if r.pos < r.buf.len: r.buf.substr(r.pos) else: ""
    r.pos = 0
  let c = r.core
  if c == nil: raise BadOperation
  let a = r.adapter
  while true:
    if a != nil:
      if not a.hasMore: return false
      if r.raw.len > 0 or r.eof:
        var used = 0
        let before = r.buf.len
        a.run(r.raw, used, r.buf, final = r.eof)
        if used > 0: r.raw = r.raw.substr(used)
        if r.buf.len > before: return true
        if not a.hasMore or r.eof: return false
        if used == 0 and r.raw.len > 0 and r.eof: return false
    else:
      if r.raw.len > 0:
        if r.buf.len == 0:
          r.buf = move r.raw
          r.raw = ""
        else:
          r.buf.add r.raw
          r.raw = ""
        return true
      if r.eof: return false
    if r.eof: return false
    if coreRecv(c, r.raw) == 0: r.eof = true

proc fill*(r: var Reader): int {.passive, raises.} =
  ## Append more decoded bytes, preserving the unread tail and compacting
  ## consumed bytes first. Returns the number added, or `0` at source or
  ## adapter end even if unread bytes remain. Raises what `read` raises.
  ##
  ## Unlike `read`, this can add bytes while others are still buffered.
  ## The caller must consume them or bound how much it allows to accumulate.
  let before = r.buffered
  discard refill(r)
  result = r.buffered - before

proc read*(r: var Reader; dest: var openArray[char]): int {.passive, raises.} =
  ## Up to `dest.len` bytes; `0` is the end of the data.
  result = 0
  if r.core == nil: raise BadOperation
  if dest.len == 0: return
  if r.pos >= r.buf.len:
    if not refill(r): return 0
  result = min(dest.len, r.buf.len - r.pos)
  copyBytes(dest, toOpenArray(r.buf, r.pos, r.pos + result - 1))
  r.pos += result

proc readLine*(r: var Reader): string {.passive, raises.} =
  ## One line without its `\n` and a trailing `\r`. At the end of the data the
  ## last, unterminated line is returned, then `""`.
  result = ""
  if r.core == nil: raise BadOperation
  while true:
    if r.pos >= r.buf.len:
      if not refill(r): return
    var i = r.pos
    while i < r.buf.len:
      if r.buf[i] == '\n':
        var last = i
        if last > r.pos and r.buf[last - 1] == '\r': dec last
        if last > r.pos: result.addBytes toOpenArray(r.buf, r.pos, last - 1)
        r.pos = i + 1
        return
      inc i
    if r.pos < r.buf.len: result.addBytes toOpenArray(r.buf, r.pos, r.buf.len - 1)
    r.pos = r.buf.len

proc readAll*(r: var Reader): string {.passive, raises.} =
  ## Everything up to the end of the data.
  result = ""
  if r.core == nil: raise BadOperation
  while true:
    if r.pos < r.buf.len: result.addBytes toOpenArray(r.buf, r.pos, r.buf.len - 1)
    r.pos = r.buf.len
    if not refill(r): return

proc close*(r: var Reader) {.raises.} =
  ## Optional: the destructor does the same.
  let c = r.core
  if c == nil: raise BadOperation
  let g = c.ring
  if g != nil: ringReaderGone(g)
  r.core = nil

# ------------------------------------------------------------------ writing --

proc push(w: var Writer; final: bool) {.passive, raises.} =
  ## Moves the buffered bytes through the adapter and down.
  let c = w.core
  if c == nil: raise BadOperation
  let a = w.adapter
  if a != nil:
    var outb = ""
    var used = 0
    a.run(w.buf, used, outb, final)
    if used >= w.buf.len: w.buf = ""
    elif used > 0: w.buf = w.buf.substr(used)
    elif final and w.buf.len > 0: raise ProtocolError   # an adapter must take all at the end
    if outb.len > 0: coreSend(c, outb)
  elif w.buf.len > 0:
    let data = move w.buf
    w.buf = ""
    coreSend(c, data)

proc write*(w: var Writer; data: openArray[char]) {.passive, raises.} =
  let c = w.core
  if c == nil or w.finished: raise BadOperation
  if c.kind == ckPipe and w.adapter == nil and w.buf.len == 0:
    # A pipe's writer has no buffer: what is written is readable at once.
    coreSend(c, data)
  else:
    w.buf.addBytes data
    if w.buf.len >= FlushAt: push(w, false)

proc write*(w: var Writer; data: string) {.passive, raises.} =
  write(w, toOpenArray(data, 0, data.len - 1))

proc flush*(w: var Writer) {.passive, raises.} =
  ## Everything written so far goes down (and, for a connection, out).
  if w.core == nil or w.finished: raise BadOperation
  push(w, false)

proc finishImpl(w: var Writer; clean: bool) {.passive, raises.} =
  w.finished = true
  if clean:
    push(w, true)
    let c = w.core
    if c != nil:
      let e = c.tls
      if e != nil and c.kind == ckFd:
        e.close()
        sendRecords(c, e)
      endWriting(c)

proc close*(w: var Writer) {.passive, raises.} =
  ## Optional: finishes the stream — adapter trailer, TLS `close_notify`, end of
  ## data for the peer — and raises if any of it fails, which the destructor
  ## cannot.
  if w.core == nil or w.finished: raise BadOperation
  finishImpl(w, true)
  w.core = nil

proc abort*(w: var Writer) =
  ## Ends the writer WITHOUT a clean finish: no adapter trailer, no
  ## `close_notify`. For error paths, where a clean finish would hand the peer
  ## a complete-looking, truncated stream.
  if w.core != nil:
    w.finished = true
    w.buf = ""
    let c = w.core
    if c != nil: endWriting(c)
    w.core = nil

proc extract*(w: sink Writer): string {.raises.} =
  ## The bytes of an in-memory writer (`openWriter(sizeHint)`), finished:
  ## adapter trailers included.
  var ww = w
  let c = ww.core
  if c == nil or c.kind != ckMemory: raise BadOperation
  finishImpl(ww, true)
  result = move c.data
  c.data = ""

# ------------------------------------------------------------------ adapters --

proc setAdapter*(r: var Reader; a: nil Adapter) {.passive, raises.} =
  ## Fills (or with `nil` empties) the reader's adapter slot. Bytes read from
  ## below but not yet handed out become the new adapter's input. Replacing a
  ## adapter that has not reached its end raises: its data would be lost.
  if r.core == nil: raise BadOperation
  let old = r.adapter
  if old != nil:
    if old.hasMore: raise BadOperation
    if r.pos < r.buf.len: raise BadOperation
    r.buf = ""
    r.pos = 0
  elif r.pos < r.buf.len:
    r.raw = r.buf.substr(r.pos) & r.raw
    r.buf = ""
    r.pos = 0
  if a != nil: a.attach(true)
  r.adapter = a

proc setAdapter*(w: var Writer; a: nil Adapter) {.passive, raises.} =
  ## Fills (or with `nil` empties) the writer's adapter slot. Bytes written
  ## before go out first, through the old adapter, which writes its trailer.
  if w.core == nil or w.finished: raise BadOperation
  push(w, w.adapter != nil)
  if a != nil: a.attach(false)
  w.adapter = a

type
  GzipAdapter* = ref object of Adapter
    ## Compresses on a writer, decompresses on a reader.
    fmt: Format
    reading: bool
    enc: Encoder
    dec: Decoder
    done: bool

proc gzipAdapter*(fmt = fmtGzip; level = 6): GzipAdapter =
  ## `fmtDetect` (gzip or zlib, recognised by header) is for reading only.
  GzipAdapter(fmt: fmt, reading: false, enc: initEncoder(fmt, level),
              dec: initDecoder(fmt), done: false)

method attach*(a: GzipAdapter; reading: bool) {.raises.} =
  if not reading and a.fmt == fmtDetect: raise BadOperation
  a.reading = reading

method run*(a: GzipAdapter; src: openArray[char]; used: var int;
            dest: var string; final: bool) {.raises.} =
  used = src.len
  if a.reading:
    if src.len > 0: a.dec.feed(src)
    let st = a.dec.decode(dest)
    if st == infError: raise SyntaxError
    if st == infDone and a.dec.isDone: a.done = true
    if final and not a.done: raise SyntaxError        # cut short
  else:
    if src.len > 0: a.enc.compress(src, dest)
    if final: a.enc.finish(dest)

method hasMore*(a: GzipAdapter): bool =
  not (a.reading and a.done)

# ------------------------------------------------------------- constructors --

proc openFd(p: Path; mode: FileMode): cint {.passive, raises.} =
  var res = 0
  let c = delay()
  var fn = $p
  discard submitOpen(fn.toCString, mode, DefaultPermissions, never, c, addr res)
  suspend()
  if res < 0: raise toErr(res)
  result = cint(res)

proc openReader*(p: Path): Reader {.passive, raises.} =
  ## The file at `p`.
  result = initReader(newCore(ckFd, openFd(p, fmRead)))

proc openReader*(s: sink string): Reader =
  ## The bytes of `s`.
  let c = newCore(ckString, -1)
  c.data = s
  result = initReader(c)

proc openWriter*(p: Path): Writer {.passive, raises.} =
  ## The file at `p`, truncated.
  result = initWriter(newCore(ckFd, openFd(p, fmWrite)))

proc openAppending*(p: Path): Writer {.passive, raises.} =
  ## The file at `p`, written at its end.
  result = initWriter(newCore(ckFd, openFd(p, fmAppend)))

proc openWriter*(sizeHint = 1024): Writer =
  ## An in-memory writer; `extract` gives its bytes back.
  let c = newCore(ckMemory, -1)
  c.data = newStringOfCap(sizeHint)
  result = initWriter(c)

# --------------------------------------------------------------------- copy --

proc copy*(dest: var Writer; src: var Reader): int64 {.passive, raises.} =
  ## Moves everything `src` has left into `dest` and returns how many bytes;
  ## `dest` stays open. Straight from the reader's buffer into the writer.
  result = 0
  if src.core == nil or dest.core == nil: raise BadOperation
  while true:
    if src.pos < src.buf.len:
      write(dest, toOpenArray(src.buf, src.pos, src.buf.len - 1))
      result += int64(src.buf.len - src.pos)
      src.pos = src.buf.len
    if not refill(src): return

# --------------------------------------------------------------------- pipe --

proc pipe*(capacity = 64 * 1024): (Reader, Writer) =
  ## An in-process byte pipe of `capacity` bytes, e.g. a body produced by
  ## another task. The writer has no buffer: what is written is readable at
  ## once. `read` parks while the pipe is empty and returns `0` once the writer
  ## is gone; `write` parks while it is full and raises `BadDescriptor` once
  ## the reader is gone.
  let c = newCore(ckPipe, -1)
  c.ring = newRing(max(capacity, 1), 1)
  result = (initReader(c), initWriter(c))

# ----------------------------------------------------------------- records --
#
# Fixed-size records moved through a pipe's ring in one piece each, the
# building blocks of `std/channels`.

proc recordPipe*(capacity, recSize: int;
                 drop: nil proc (p: pointer) {.nimcall.}): (Reader, Writer) =
  ## A `pipe` for `capacity` records of `recSize` bytes each. `drop` destroys
  ## a record that is never read; nil for plain bytes.
  let c = newCore(ckPipe, -1)
  let g = newRing(max(capacity, 1) * recSize, recSize)
  g.drop = drop
  c.ring = g
  result = (initReader(c), initWriter(c))

proc hasRecordRing*(w: Writer; recSize: int): bool =
  ## `w` is a `recordPipe` writer for `recSize` with nothing buffered and no
  ## adapter, so `writeRecord` may bypass it.
  result = false
  let c = w.core
  if c != nil:
    let g = c.ring
    result = g != nil and g.recSize == recSize and w.adapter == nil and
      w.buf.len == 0

proc hasRecordRing*(r: Reader; recSize: int): bool =
  ## Like the writer's: `readRecord` may bypass `r`.
  result = false
  let c = r.core
  if c != nil:
    let g = c.ring
    result = g != nil and g.recSize == recSize and r.adapter == nil and
      r.buffered == 0 and r.raw.len == 0

proc recordRing(c: nil Core): Ring {.raises.} =
  if c == nil: raise BadOperation
  let g = c.ring
  if g == nil: raise BadOperation
  result = g

proc writeRecord*(w: Writer; p: pointer): bool {.passive, raises.} =
  ## One record from `p` straight into the ring, waiting for room; only after
  ## `hasRecordRing`. False when the reader is gone. Safe from several threads
  ## at once.
  let g = recordRing(w.core)
  result = ringWrite(g, toOpenArray(cast[ptr UncheckedArray[char]](p), 0, g.recSize - 1), true)

proc readRecord*(r: Reader; p: pointer): bool {.passive, raises.} =
  ## One record from the ring into `p`, waiting for it; only after
  ## `hasRecordRing`. False once the writer is gone and the ring is empty.
  ## Safe from several threads at once.
  let g = recordRing(r.core)
  result = ringRead(g, p, g.recSize, true) == g.recSize

proc tryReadRecord*(r: Reader; p: pointer): int {.raises.} =
  ## Like `readRecord` but does not park: `recSize` when a whole record was
  ## copied into `p`, `0` once the writer is gone and the ring is empty, `-1`
  ## when there is nothing to read yet. Only after `hasRecordRing`.
  let g = recordRing(r.core)
  let dest = cast[ptr UncheckedArray[char]](p)
  var wake: seq[Continuation] = @[]
  result = -1
  acquire g.lock
  if g.len >= g.recSize:
    let cap = g.cap
    let n = g.recSize
    let first = min(n, cap - g.head)
    copyMem(addr dest[0], addr g.data[g.head], first)
    if n > first: copyMem(addr dest[first], addr g.data[0], n - first)
    g.head = (g.head + n) mod cap
    g.len -= n
    takeWaiters(g.sendWaiters, (cap - g.len) div g.recSize, wake)
    result = n
  elif g.writerGone:
    result = 0
  release g.lock
  resume wake

proc sleepUntil*(d: Deadline) {.passive.} =
  ## Park until `d` (a no-op for `never`). The ring's own waits have no
  ## deadline, so a caller that must bound a wait on a ring polls it between
  ## slices and uses this to give the lane back.
  if d != never:
    discard submitTimeout(d, delay())
    suspend()

proc isOpen*(r: Reader): bool =
  ## Neither closed nor default-constructed.
  result = r.core != nil

proc isOpen*(w: Writer): bool =
  ## Neither closed, finished nor default-constructed.
  result = w.core != nil and not w.finished

proc isPipe*(w: Writer): bool =
  ## `w` writes into a `pipe` or a `recordPipe`.
  let c = w.core
  result = c != nil and c.kind == ckPipe