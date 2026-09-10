# (c) 2026 Andreas Rumpf
#
# An HTTP/1.1 server: the protocol rules that sit between `std/http/httpconn`
# and an application. See doc/internals/http.md §2.
#
#   let tags = newHttpTags()
#
#   proc handle(c: sink HttpConnection) {.passive.} =
#     while c.next():
#       if c.path == "/": c.respond(200, "hello\n", "text/plain")
#       else:             c.respond(404, "")
#
#   proc acceptLoop() {.passive.} =
#     var s = listenHttp(8080'u16, tags)
#     while true:
#       submit(delay(handle(s.accept())), -1)
#
# ## What this layer is for
#
# Everything below it is a correct primitive and none of it is a policy. A
# server written straight on `httpconn` works on the first request and is
# wrong about the second, in six ways that are each one line and none of which
# announce themselves:
#
# * **An unread request body poisons keep-alive.** A handler that ignores a
#   `POST`'s body leaves those bytes in the stream, and the next `readRequest`
#   parses them as a request line. `next` drains, or closes when the leftover
#   is too big to be worth draining.
# * **`Connection`.** A response that says `keep-alive` to a request that said
#   `close`, or to an HTTP/1.0 client that never asked, is a connection one
#   side keeps and the other drops.
# * **`HEAD`, 204 and 304 have no body.** Sending one is not a cosmetic bug: a
#   client that trusts the framing reads it as the start of the next response.
# * **`Date`.** RFC 9110 §6.6.1 makes it a MUST for an origin server with a
#   clock, and every cache in the path needs it.
# * **A malformed head deserves a 400.** `httpparse` says so in its own
#   docstring; a raise out of a handler chain just drops the connection, and
#   the peer cannot tell that from a network fault.
# * **HTTP/1.1 requires `Host`.** Without the check, a request that names no
#   host is served by whichever virtual host was first in the list.
#
# ## Why the connection is the loop, not a global event queue
#
# The design sketch had one `HttpLoop` handing out `HttpEvent`s tagged with a
# `ConnId`, so a response could be produced by code that was not on that
# connection's chain. With `.passive` it does not have to be: the chain *is*
# the handle, a handler that needs to go away and come back suspends and the
# connection comes back with it, and `respond` reaches the socket directly
# instead of through a table every response has to lock. The event loop is
# still what this is — `next` is `waitEvent` with the parsing done — it is
# just per connection, which is the only scope at which HTTP/1.1 framing has
# any meaning: requests on one connection are strictly ordered and requests on
# different ones share nothing.
#
# ## `.passive` throughout
#
# `next` and `respond` park on `std/ioring` and are resumed by a pool worker,
# so a handler reads like blocking code and is not. Because `respond`
# suspends until the write lands, **suspension is the backpressure** — there
# is no drain event and no "did I check for room" bug class.

import std / [http/httpconn, http/httpmsg, http/httpparse, http/httpwire,
              http/httpdate, socket, ioring, uri, assertions]

export httpmsg, httpconn.HttpConn, socket.PeerAddr, socket.`$`, socket.ip,
       socket.port, socket.family, socket.isV4, socket.isV6
export ioring.Deadline, ioring.never, ioring.afterMs, ioring.after,
       ioring.monoNow, ioring.listenTcp, ioring.setNonBlocking,
       ioring.closeFd, ioring.boundPort

const
  DefaultHeadMs* = 10_000
    ## How long a peer may take to finish sending a request head. Short on
    ## purpose and separate from the request budget: a head arriving one byte
    ## per second is the cheapest denial of service there is, and it costs the
    ## attacker nothing precisely because it never looks like a timeout of
    ## anything else.
  DefaultRequestMs* = 60_000
    ## The budget for one whole request-response exchange once its head has
    ## arrived, body and response included.
  DefaultIdleMs* = 30_000
    ## How long a kept-alive connection may sit between requests.
  DefaultMaxRequests* = 1000
    ## Requests served on one connection before it is closed anyway. Not a
    ## resource limit — it is what keeps a fleet's connections rotating, so a
    ## deploy or a DNS change is picked up without waiting for peers to
    ## volunteer.
  MaxDrain* = 64 * 1024
    ## The most unread request body that is worth reading and discarding to
    ## keep a connection alive. Past it the connection is closed instead:
    ## draining a body the handler did not want is work a peer chose for us,
    ## and a limit is the difference between politeness and being driven.

type
  HttpServer* = object
    ## A listening socket and the policy every connection off it inherits.
    fd*: cint
    tags*: HttpTags
    headMs*: int
    requestMs*: int
    idleMs*: int
    maxRequests*: int
    serverName*: string
      ## Sent as `Server:`, or nothing when empty — which is the default,
      ## because a version string is a free hint to anyone scanning and the
      ## header buys the operator nothing.
    checkTargets*: bool
      ## Refuse a request target that does not decode to a safe path, with a
      ## 400, before the handler sees it. A proxy turns this off and reads
      ## `target` itself; an origin server wants it on, because a target it
      ## cannot make a path of is one it can only mis-serve.

  Phase = enum
    phIdle        ## between requests
    phHead        ## a head has been read, nothing sent
    phStreaming   ## a head has been sent, the body is being written
    phDone        ## the exchange is over
    phClosed      ## the connection is finished, for whatever reason

  HttpConnection* = object
    ## One connection, and the request currently on it. Move-only: it owns the
    ## descriptor, and two of them over one fd is two owners of one resource.
    conn: HttpConn
    peer*: PeerAddr
    req: HttpMsg
    res: HttpMsg
    pathBuf: string
    queryBuf: string
    targetBuf: string
    phase: Phase
    keepAlive: bool
    served: int
    bodyLeft: int      ## declared body bytes not yet read; `-1` when chunked
    chunked: bool
    headMs, requestMs, idleMs, maxRequests: int
    serverName: string
    checkTargets: bool

# No explicit `=copy` here: `HttpConn` holds a `Socket`, whose `=copy` is
# already an error, so this object is move-only by construction. Restating it
# suppresses the generated `=dup` and then `accept` cannot return one.

# ------------------------------------------------------------- listening ---

proc listenHttp*(port: uint16; tags: HttpTags; backlog = 128): HttpServer =
  ## Listen on `port`. `initIoRing()` must already have run.
  ##
  ## `tags` is the process's one tag space (`newHttpTags()`, once, during
  ## init) — every message on every connection is built and parsed against it,
  ## so it is threaded in here rather than looked up per request.
  result = HttpServer(fd: listenTcp(port, backlog), tags: tags,
                      headMs: DefaultHeadMs, requestMs: DefaultRequestMs,
                      idleMs: DefaultIdleMs, maxRequests: DefaultMaxRequests,
                      serverName: "", checkTargets: true)

proc close*(s: var HttpServer) =
  ## Stop accepting. Connections already handed out are unaffected — they own
  ## their own descriptors and finish on their own chains, which is what makes
  ## a drain-then-exit shutdown just "stop calling `accept`".
  if s.fd >= 0:
    closeFd(s.fd)
    s.fd = -1

proc initConnection(s: HttpServer; fd: cint; peer: PeerAddr): HttpConnection =
  HttpConnection(
    conn: initHttpConn(fd, afterMs(s.idleMs), s.tags),
    peer: peer,
    req: initHttpMsg(s.tags),
    res: initHttpMsg(s.tags),
    pathBuf: "", queryBuf: "", targetBuf: "",
    phase: phIdle, keepAlive: true, served: 0,
    bodyLeft: 0, chunked: false,
    headMs: s.headMs, requestMs: s.requestMs, idleMs: s.idleMs,
    maxRequests: s.maxRequests, serverName: s.serverName,
    checkTargets: s.checkTargets)

proc accept*(s: var HttpServer): HttpConnection {.passive.} =
  ## The next connection.
  ##
  ## Answers a connection whose `isClosed` is true when the listener is gone,
  ## which is what `close` does to a parked `accept` and is therefore how an
  ## accept loop is told to stop:
  ##
  ##   while true:
  ##     let c = s.accept()
  ##     if c.isClosed: break
  ##     submit(delay(handle(c)), -1)
  ##
  ## Not `.raises`, and not because failing is impossible: a `.raises` proc
  ## cannot currently return a move-only object, and this one owns a
  ## descriptor. It reads better this way regardless — a listener stopping is
  ## how an accept loop is *supposed* to end, so it is a value and not an
  ## exception.
  ##
  ## No deadline either: a listener with nothing connecting to it is idle, not
  ## stuck, and the thing that ends this wait is `close`.
  var peer = PeerAddr(raw: Sockaddr_storage())
  let fd = acceptAsync(s.fd, peer, never)
  if fd < 0:
    result = initConnection(s, -1.cint, peer)
    result.phase = phClosed
  else:
    setNonBlocking(cint(fd))
    result = initConnection(s, cint(fd), peer)

# --------------------------------------------------------- reading a head --

proc isClosed*(c: HttpConnection): bool {.inline.} = c.phase == phClosed

proc close*(c: var HttpConnection) =
  ## End the connection now. Idempotent.
  if c.phase != phClosed:
    c.phase = phClosed
    c.conn.close()

proc sendStatusOnly(c: var HttpConnection; status: int) {.passive.} =
  ## The last thing said on a connection that is about to be dropped: a bare
  ## status with `Connection: close`.
  ##
  ## Everything here is best-effort and nothing propagates. The connection is
  ## already being closed; a peer that has gone away mid-error is the ordinary
  ## case, and raising over the failure to tell it so would replace a
  ## diagnosis the client can see with one only we can.
  try:
    prepareInto(c.res, status, true, false, c.serverName)
    c.res.addHeader(hContentLength, 0)
    c.res.finish()
    c.conn.sendHead(c.res, afterMs(2000))
  except ErrorCode:
    discard

proc fail(c: var HttpConnection; status: int) {.passive.} =
  ## Answer `status` and close. The peer gets a diagnosis rather than a
  ## dropped connection, which is the difference between "your request was
  ## wrong" and "the network ate it" — and only one of those is actionable by
  ## whoever sent it.
  sendStatusOnly(c, status)
  c.close()

proc drainBody(c: var HttpConnection): bool {.passive.} =
  ## Read and discard whatever of the current body the handler left. `false`
  ## when the connection cannot be reused — the leftover was too big, or the
  ## peer stopped part-way through it.
  ##
  ## This is the one that is easy to leave out and impossible to see when it
  ## is missing: a handler that ignores a `POST`'s body leaves those bytes
  ## where the next request line is expected, and the *next* request is the
  ## one that fails.
  if c.bodyLeft == 0 and not c.chunked: return true
  if c.bodyLeft > MaxDrain: return false
  var buf = default(array[4096, char])
  try:
    if c.chunked:
      var drained = 0
      while true:
        let n = c.conn.readChunked(buf, afterMs(c.requestMs))
        if n == 0: break
        drained += n
        if drained > MaxDrain: return false
    else:
      while c.bodyLeft > 0:
        let want = if c.bodyLeft < buf.len: c.bodyLeft else: buf.len
        let n = c.conn.readBody(toOpenArray(buf, 0, want - 1),
                                afterMs(c.requestMs))
        if n <= 0: return false
        c.bodyLeft -= n
  except ErrorCode:
    return false
  c.bodyLeft = 0
  c.chunked = false
  result = true

proc splitTarget(c: var HttpConnection): bool =
  ## Fill `target`, `path` and `query` from the request line. `false` when the
  ## target is not one this server can turn into a path — which, with
  ## `checkTargets`, is a 400 rather than a guess.
  ##
  ## `path` is decoded *and* normalized, in that order, by `uri.safePath`:
  ## `%2e%2e%2f` is `../`, so a server that normalizes before it decodes has
  ## normalized a string that did not yet contain the segments it was looking
  ## for. That ordering is the whole of the traversal defence.
  c.targetBuf = c.req.target
  c.pathBuf = ""
  c.queryBuf = ""
  let t = c.targetBuf
  var stop = t.len
  for i in 0..<t.len:
    if t[i] == '?':
      stop = i
      for k in i + 1..<t.len: c.queryBuf.add t[k]
      break
  # `*` is the whole target for `OPTIONS *`, and it is not a path. It is
  # passed through as itself rather than refused: it is a legal target, and a
  # handler that does not implement it answers 404 like anything else.
  if t == "*":
    c.pathBuf = "*"
    return true
  # Absolute-form (`GET http://host/p`) is what a proxy receives and what
  # RFC 9112 §3.2.2 says an origin server must also accept. The path is the
  # part after the authority; the authority itself is `Host`'s business.
  var origin = toOpenArray(t, 0, stop - 1)
  let u = parseUri(origin)
  if u.hostname.len > 0 or u.scheme.len > 0:
    if not safePath(toOpenArray(u.path, 0, u.path.len - 1), c.pathBuf):
      return not c.checkTargets
    return true
  result = safePath(origin, c.pathBuf)
  if not result and not c.checkTargets: result = true

proc next*(c: var HttpConnection): bool {.passive.} =
  ## Read the next request on this connection. `false` once there will not be
  ## one — the peer finished, the connection was closed, or something went
  ## wrong that the peer has already been told about.
  ##
  ## Never raises. Everything that can go wrong at this level has one correct
  ## answer on the wire and this sends it: 400 for a head that does not parse
  ## or a 1.1 request with no `Host`, 431 for one that is too big, 408 for a
  ## peer that stopped mid-head. A handler that had to catch those would be
  ## re-deciding, per server, a question the protocol already settled.
  if c.phase == phClosed: return false

  # Finish the previous exchange before starting another. Both halves matter:
  # a handler that never responded owes the peer an answer, and a handler that
  # never read the body has left it in the stream.
  if c.phase == phHead:
    sendStatusOnly(c, 500)
    c.close()
    return false
  if c.phase == phStreaming:
    # A streamed response the handler walked away from: the framing says how
    # much is coming and it is not coming, so the connection cannot be reused.
    try: c.conn.endChunks(afterMs(2000))
    except ErrorCode: discard
    c.close()
    return false
  if c.phase == phDone:
    if not c.keepAlive:
      c.close()
      return false
    if not drainBody(c):
      c.close()
      return false
    if c.served >= c.maxRequests:
      c.close()
      return false

  c.phase = phIdle
  c.req.reset()
  # The idle budget covers waiting for the request to *start*; once bytes are
  # arriving, the head has its own, shorter one. A peer is allowed to think
  # for a while between requests and is not allowed to dribble a head.
  c.conn.renew(afterMs(if c.served == 0: c.headMs else: c.idleMs))
  try:
    c.conn.readRequest(c.req)
  except ErrorCode as e:
    # `EndOfStreamError` here is the peer hanging up between requests, which
    # is how a keep-alive connection normally ends — an end, not a failure,
    # and nothing to answer.
    if e == EndOfStreamError:
      c.close()
      return false
    if e == ContentTooLong: fail(c, 431)
    elif e == TimeoutError: fail(c, 408)
    elif e == FullError: fail(c, 431)
    else: fail(c, 400)
    return false

  inc c.served
  c.conn.renew(afterMs(c.requestMs))

  # HTTP/1.1 requires `Host`. Without the check a request that names no host
  # is served by whichever virtual host happened to be first.
  if c.req.versionOf == tag(tV11) and not c.req.contains(hHost):
    fail(c, 400)
    return false

  if not splitTarget(c):
    fail(c, 400)
    return false

  c.chunked = c.req.isChunked
  c.bodyLeft = if c.chunked: -1 else: (let n = c.req.contentLength; if n < 0: 0 else: n)
  if c.chunked: c.conn.beginBody()
  c.keepAlive = c.req.isKeepAlive
  c.phase = phHead
  result = true

# ------------------------------------------------------ what was asked ----

# Templates, not procs: borrowing a field out through an immutable parameter
# is what `lent` cannot do here, and returning these by value would copy the
# path on every `c.path == "/"` — which is the most common line in a handler.
# `httpmsg.spelling` is the same shape for the same reason.

template request*(c: HttpConnection): lent HttpMsg = c.req
  ## The parsed head, for anything the named accessors below do not cover.

template target*(c: HttpConnection): lent string = c.targetBuf
  ## Exactly what the request line said, undecoded. A proxy forwards this; an
  ## origin server wants `path`.

template path*(c: HttpConnection): lent string = c.pathBuf
  ## The target's path, percent-decoded and normalized, and guaranteed not to
  ## climb above `/`. Safe to join to a document root.

template query*(c: HttpConnection): lent string = c.queryBuf
  ## The raw query string, still encoded — `uri.decodeQuery` splits it, and it
  ## has to split before it decodes or a `%26` becomes a separator.

proc meth*(c: HttpConnection): TagId {.inline.} = c.req.methodOf

proc isHead*(c: HttpConnection): bool {.inline.} =
  c.req.methodOf == tag(mHead)

proc contentLength*(c: HttpConnection): int {.inline.} =
  ## Declared body length, `-1` for a chunked body, `0` for none.
  if c.chunked: -1 else: c.bodyLeft

proc hasBody*(c: HttpConnection): bool {.inline.} =
  c.chunked or c.bodyLeft > 0

proc readBody*(c: var HttpConnection; dest: var openArray[char]): int {.
    passive, raises.} =
  ## The next piece of the request body: bytes copied, `0` once it has ended.
  ##
  ## One proc for both framings, because which one the peer chose is not the
  ## handler's business — it is the difference between two correct ways of
  ## saying where a body stops, and code that asks is code that can get the
  ## answer wrong. Anything left unread is drained by the next `next`.
  result = 0
  if c.chunked:
    result = c.conn.readChunked(dest)
  elif c.bodyLeft > 0:
    let want = if c.bodyLeft < dest.len: c.bodyLeft else: dest.len
    result = c.conn.readBody(toOpenArray(dest, 0, want - 1))
    c.bodyLeft -= result

proc readBody*(c: var HttpConnection; limit = 1024 * 1024): string {.
    passive, raises.} =
  ## The whole body as a string, or `ContentTooLong` past `limit`.
  ##
  ## The limit has no "unlimited" spelling on purpose: a body's length is
  ## chosen by the peer, and the version of this proc without a ceiling is one
  ## `Content-Length: 999999999999` away from being the whole outage.
  result = ""
  var buf = default(array[4096, char])
  while true:
    let n = readBody(c, buf)
    if n == 0: break
    if result.len + n > limit: raise ContentTooLong
    for i in 0..<n: result.add buf[i]

# --------------------------------------------------------- responding -----

proc bodyIsForbidden(c: HttpConnection; status: int): bool {.inline.} =
  ## Whether this response may carry a body at all. `HEAD` answers the headers
  ## a `GET` would have — `Content-Length` included — and then stops; 1xx, 204
  ## and 304 have no body *and* no `Content-Length`, and a client that trusts
  ## the framing reads one we send anyway as the next response's head.
  c.isHead or (status >= 100 and status < 200) or status == 204 or status == 304

proc lengthIsForbidden(status: int): bool {.inline.} =
  (status >= 100 and status < 200) or status == 204 or status == 304

proc prepareInto(m: var HttpMsg; status: int; closing, isV10: bool;
                 serverName: string) =
  ## The header work that every response gets, as a function of the *facts*
  ## rather than of the connection.
  ##
  ## Taking the four values instead of the `HttpConnection` is not a style
  ## choice: `m` is one of that object's own fields on the common path, and a
  ## `var` connection plus one of its fields is the alias the compiler
  ## rejects. Naming what is actually read makes the two arguments provably
  ## disjoint, and it is the shorter signature to read anyway.
  m.reset()
  m.startResponse(status)
  var dbuf = default(array[HttpDateLen, char])
  if nowHttpDate(dbuf) == HttpDateLen:
    m.addHeader(hDate, toOpenArray(dbuf, 0, HttpDateLen - 1))
  if serverName.len > 0:
    m.addHeader(hServer, serverName)
  # `Connection` only when closing. `keep-alive` is HTTP/1.1's default and
  # saying it again is a header per response that means nothing; an HTTP/1.0
  # client that asked for it does need to hear it back.
  if closing:
    m.addHeader(hConnection, vClose)
  elif isV10:
    m.addHeader(hConnection, vKeepAlive)

proc emit(conn: var HttpConn; m: var HttpMsg; body: openArray[char];
          dropBody, noLength: bool) {.passive, raises.} =
  ## Serialize and send one complete response. Takes the socket and the
  ## message as two separate `var`s for the reason `prepareInto` does.
  if not noLength and not m.contains(hContentLength):
    m.addHeader(hContentLength, body.len)
  m.finish()
  conn.sendHead(m)
  if body.len > 0 and not dropBody:
    conn.sendBody(body)

proc prepare*(c: var HttpConnection; m: var HttpMsg; status: int) =
  ## Start a response into `m`: the status line, `Date`, `Server` and
  ## `Connection`, so a handler adds only what is its own. `respond` takes it
  ## from there.
  prepareInto(m, status, not c.keepAlive, c.req.versionOf == tag(tV10),
              c.serverName)

proc respond*(c: var HttpConnection; m: var HttpMsg;
              body: openArray[char]) {.passive, raises.} =
  ## Send `m` — built by `prepare` and whatever headers the handler added —
  ## with `body`.
  ##
  ## `Content-Length` is filled in from `body` unless the status forbids one,
  ## and the body itself is suppressed for `HEAD` and for the statuses that
  ## cannot carry one. Those two rules are why this exists rather than
  ## `sendHead` + `sendBody`: they are invisible when they are missing and
  ## they desynchronize the *next* response, not this one.
  if c.phase == phClosed: return
  let status = m.statusOf
  let drop = bodyIsForbidden(c, status)
  let noLen = lengthIsForbidden(status)
  emit(c.conn, m, body, drop, noLen)
  c.phase = phDone

proc respond*(c: var HttpConnection; status: int; body: openArray[char];
              contentType = "") {.passive, raises.} =
  ## The whole response in one call, which is what almost every handler wants.
  if c.phase == phClosed: return
  prepareInto(c.res, status, not c.keepAlive, c.req.versionOf == tag(tV10),
              c.serverName)
  if contentType.len > 0: c.res.addHeader(hContentType, contentType)
  let drop = bodyIsForbidden(c, status)
  let noLen = lengthIsForbidden(status)
  emit(c.conn, c.res, body, drop, noLen)
  c.phase = phDone

proc respond*(c: var HttpConnection; status: int; body: string;
              contentType = "") {.passive, raises, inline.} =
  respond(c, status, toOpenArray(body, 0, body.len - 1), contentType)

proc redirect*(c: var HttpConnection; location: string;
               status = 302) {.passive, raises.} =
  ## A redirect, with the `Location` a redirect is nothing without.
  if c.phase == phClosed: return
  prepareInto(c.res, status, not c.keepAlive, c.req.versionOf == tag(tV10),
              c.serverName)
  c.res.addHeader(hLocation, location)
  var empty = default(array[1, char])
  emit(c.conn, c.res, toOpenArray(empty, 0, -1),
       bodyIsForbidden(c, status), lengthIsForbidden(status))
  c.phase = phDone

# ------------------------------------------------------------ streaming ---

proc beginStream*(c: var HttpConnection; m: var HttpMsg) {.passive, raises.} =
  ## Start a chunk-framed response whose length is not known yet. Follow with
  ## `write` and then `finish`.
  ##
  ## Chunked rather than "write until close": a response that ends by closing
  ## cannot be followed by another, so it costs the connection — and on a
  ## proxy it costs the client the ability to tell a complete response from a
  ## truncated one.
  if c.phase == phClosed: return
  if not m.contains(hTransferEncoding):
    m.addHeader(hTransferEncoding, vChunked)
  m.finish()
  c.conn.sendHead(m)
  c.phase = phStreaming

proc beginStream*(c: var HttpConnection; status: int;
                  contentType = "") {.passive, raises.} =
  if c.phase == phClosed: return
  prepareInto(c.res, status, not c.keepAlive, c.req.versionOf == tag(tV10),
              c.serverName)
  if contentType.len > 0: c.res.addHeader(hContentType, contentType)
  if not c.res.contains(hTransferEncoding):
    c.res.addHeader(hTransferEncoding, vChunked)
  c.res.finish()
  c.conn.sendHead(c.res)
  c.phase = phStreaming

proc write*(c: var HttpConnection; data: openArray[char]) {.passive, raises.} =
  ## One piece of a streamed body. Suspends until it is on the wire, so a
  ## handler that produces faster than the peer consumes is slowed by the peer
  ## rather than by a queue that grows until something notices.
  ##
  ## Nothing is written for a `HEAD`, whose response is the headers a `GET`
  ## would have sent and nothing after them — so a handler streams the same
  ## way for both and does not branch.
  if c.phase != phStreaming: return
  if c.isHead: return
  c.conn.sendChunk(data)

proc write*(c: var HttpConnection; data: string) {.passive, raises, inline.} =
  write(c, toOpenArray(data, 0, data.len - 1))

proc finish*(c: var HttpConnection) {.passive, raises.} =
  ## End a streamed body.
  if c.phase != phStreaming: return
  if not c.isHead: c.conn.endChunks()
  c.phase = phDone
