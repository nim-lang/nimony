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
## Not here yet (see the issue): `openReadWrite`, `pipe`/channels, `startTls`,
## `sendfile`/`splice`/kTLS fast paths in `copy`, and cross-thread close of a
## descriptor another lane is parked on.

import std/[ioring, socket, dns, paths, commonio, strutils]
import std/compress/gzip

export ioring.Deadline, ioring.never, ioring.afterMs
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

# --------------------------------------------------------------------- core --

type
  CoreKind = enum
    ckFd,          ## a file or a socket
    ckString,      ## the bytes of a string (read only)
    ckMemory       ## an in-memory sink (write only), see `extract`

  CoreObj = object
    kind: CoreKind
    fd: cint
    isSocket: bool
    tls: nil TlsEngine
    data: string                 ## ckString: the source; ckMemory: the sink
    dataPos: int
    peerDone: bool               ## below the reader: the source has ended
    shutDown: bool               ## our direction has been ended
    dl: Deadline

  Core = ref CoreObj
    ## Shared by the ends one constructor returns. They are move-only, so the
    ## (atomic) reference count changes only when an end is created or dies.

proc `=destroy`(c: CoreObj) =
  if c.kind == ckFd and c.fd >= 0:
    closeFd(c.fd)
  `=destroy`(c.tls)
  `=destroy`(c.data)

proc newCore(kind: CoreKind; fd: cint): Core =
  Core(kind: kind, fd: fd, isSocket: false, tls: nil, data: "", dataPos: 0,
       peerDone: false, shutDown: false, dl: never)

proc endWriting(c: Core) =
  ## Our direction is over: the peer sees end of data and may still answer.
  if c.isSocket and not c.shutDown and c.fd >= 0:
    when defined(posix):
      discard posixShutdown(c.fd, cint(1))      # SHUT_WR
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

proc tlsHandshake(c: Core; e: TlsEngine; host: string) {.passive, raises.} =
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
    core: nil Core
    buf: string          ## caller bytes not yet pushed through the adapter
    adapter: nil Adapter
    finished: bool

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

proc initReader(c: Core): Reader =
  Reader(core: c, buf: "", pos: 0, raw: "", adapter: nil, eof: false)

proc initWriter(c: Core): Writer =
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
  if r.core == nil: raise BadOperation
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
  if w.core == nil or w.finished: raise BadOperation
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

proc splitAddress(address: string; host: var string; port: var uint16) {.raises.} =
  let colon = address.rfind(':')
  if colon <= 0 or colon == address.len - 1: raise BadOperation
  host = address.substr(0, colon - 1)
  var p = 0
  for i in colon + 1 ..< address.len:
    let ch = address[i]
    if ch notin {'0'..'9'}: raise BadOperation
    p = p * 10 + (ord(ch) - ord('0'))
    if p > 65535: raise BadOperation
  port = uint16(p)

proc isIpLiteral(host: string): bool =
  result = host.len > 0
  for ch in host:
    if ch notin {'0'..'9', '.'}: return false

proc connectCore(address: string; tls: nil TlsEngine; dl: Deadline): Core {.
    passive, raises.} =
  var host = ""
  var port = 0'u16
  splitAddress(address, host, port)
  if dl <= monoNow(): raise TimeoutError
  let ip = if isIpLiteral(host): host else: resolve(host, dl)
  var s = socket.connect(ip, port, dl)
  let c = newCore(ckFd, s.fd)
  s.fd = -1                         # the core owns the descriptor now
  c.isSocket = true
  c.dl = dl
  if tls != nil:
    c.tls = tls
    tlsHandshake(c, tls, host)     # on failure the core closes the socket
  result = c

proc connect*(address: string; tls: nil TlsEngine = nil;
              dl = never): (Reader, Writer) {.
    passive, raises.} =
  ## A connection to `address` (`host:port`); TLS when an engine is given. For
  ## a protocol where the peer speaks first (SMTP, IMAP): the next step is
  ## reading its greeting. `dl` bounds DNS, TCP and TLS setup together; the
  ## returned streams have no standing deadline. Raises `TimeoutError` on expiry.
  let c = connectCore(address, tls, dl)
  c.dl = never
  result = (initReader(c), initWriter(c))

proc connect*(address: string; request: string; tls: nil TlsEngine = nil;
              dl = never): (Reader, Writer) {.
    passive, raises.} =
  ## For a protocol where we speak first (HTTP, Redis): `request` is written
  ## and FLUSHED before this returns, so the next step is reading the reply —
  ## the forgotten flush that hangs both sides cannot be written.
  ## One `dl` covers DNS, TCP, TLS and that flush; the returned streams start
  ## with `never`. A failed request write aborts rather than cleanly finishing.
  let c = connectCore(address, tls, dl)
  var w = initWriter(c)
  try:
    write(w, request)
    flush(w)
  except:
    w.abort()
    raise
  c.dl = never
  result = (initReader(c), w)

proc connectReader*(address: string; tls: nil TlsEngine = nil;
                    dl = never): Reader {.passive, raises.} =
  ## Only the receiving end, e.g. a feed. The sending direction is left alone
  ## (not shut down) and closes with the reader. `dl` bounds construction only;
  ## use `renew` to set a deadline for later I/O.
  let c = connectCore(address, tls, dl)
  c.dl = never
  result = initReader(c)

proc connectWriter*(address: string; tls: nil TlsEngine = nil;
                    dl = never): Writer {.passive, raises.} =
  ## Only the sending end, e.g. shipping logs. `dl` bounds construction only;
  ## use `renew` to set a deadline for later I/O.
  let c = connectCore(address, tls, dl)
  c.dl = never
  result = initWriter(c)

proc detachWriter*(w: var Writer) =
  ## Drops a writer WITHOUT ending its direction: no adapter trailer, no
  ## `close_notify`, no `shutdown`. For a protocol layer that has written
  ## its request and keeps only the reader (`std/httpclient`), where ending
  ## the direction would make many servers drop the connection.
  if w.core != nil:
    w.finished = true
    w.core = nil

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
