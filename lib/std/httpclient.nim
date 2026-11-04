#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## HTTP/1.1 requests as `streamio` constructors (nim-lang/nimony#2635).
##
## The constructor does the talking — request out, status and headers in — and
## hands back the response BODY as a `Reader`:
##
## .. code-block:: nim
##   var page = httpGet("https://example.com/")
##   echo page.readAll()
##   var r = httpPost("https://api.example.com/upload", openReader(path"big.bin"))
##
## * `https` in the URL means TLS (`std/tls`'s `newTlsEngine()` unless an
##   engine is passed). An engine on a plain `http` URL raises.
## * In `httpGet`, `httpPost` and `httpPut` a status outside 2xx RAISES the
##   `ErrorCode` `httpToErrorCode` maps it to (404 is `NameNotFound`, 401/403
##   `PermissionDenied`, ...), so a `Reader` returned from them always carries
##   a successful body. `httpRequest` returns every status as a number instead:
##
##   .. code-block:: nim
##     var resp = httpRequest("GET", "https://example.com/", dl = afterMs(10_000))
##     echo resp.code, " ", resp.reason, " ", resp.url
## * The body is de-framed (`Content-Length`, chunked, or until the server
##   closes) and its content coding undone (gzip, deflate) by an adapter in the
##   reader's adapter slot. Trailers are read and dropped.
## * Request headers are an `HttpHeaders`, the wire format itself. The library
##   writes `Host`, `Content-Length`/`Transfer-Encoding` and `Connection`
##   itself; a user header with one of those names raises `BadOperation`
##   (conflicting framing headers are how request smuggling works).
##   `User-Agent` and `Accept-Encoding` have defaults a user header replaces.
##
## One request per connection (`Connection: close`). `httpGet`, `httpPost` and
## `httpPut` raise for a redirect, not follow it; `httpRequest` returns a
## response of any status as an `HttpResponse` (code, reason, headers, final
## URL, body) and follows redirects. Every request takes a deadline `dl`
## (`afterMs(10_000)`), which bounds it as a whole.

{.feature: "lenientnils".}

import std/[streamio, tls, uri, strutils]
import std/http/httpheaders
import std/compress/gzip
import std/errorcodes/errorcodes_http

export streamio, httpheaders

# ---------------------------------------------------------- body framing ----

type
  Framing = enum
    frLength, frChunked, frUntilClose

  ChunkState = enum
    csSize, csData, csDataEnd, csTrailer, csDone

  HttpBodyAdapter = ref object of Adapter
    ## De-frames an HTTP body and undoes its content coding: the reader's adapter
    ## for a response. Framing knows where the body ends, so bytes past it stay
    ## below, untouched.
    framing: Framing
    left: int64             ## frLength: bytes to go; csData: bytes of this chunk
    state: ChunkState
    line: string            ## a chunk-size or trailer line being assembled
    coded: bool
    dec: Decoder
    decDone: bool
    framingDone: bool

proc deliver(a: HttpBodyAdapter; piece: openArray[char]; dest: var string;
             final: bool) {.raises.} =
  if not a.coded:
    for c in piece: dest.add c
    return
  if piece.len > 0: a.dec.feed(piece)
  let st = a.dec.decode(dest)
  if st == infError: raise SyntaxError
  if st == infDone and a.dec.isDone: a.decDone = true
  if final and not a.decDone: raise SyntaxError       # the coding was cut short

proc parseHexSize(line: string): int64 {.raises.} =
  ## A chunk-size line: hex digits, optionally followed by `;extensions`.
  result = 0
  var digits = 0
  for c in line:
    if c == ';' or c == ' ' or c == '\t': break
    let d = if c in {'0'..'9'}: ord(c) - ord('0')
            elif c in {'a'..'f'}: ord(c) - ord('a') + 10
            elif c in {'A'..'F'}: ord(c) - ord('A') + 10
            else: -1
    if d < 0: raise SyntaxError
    if digits >= 15: raise SyntaxError                # 2^60: not a real chunk
    result = result * 16 + int64(d)
    inc digits
  if digits == 0: raise SyntaxError

method run(a: HttpBodyAdapter; src: openArray[char]; used: var int;
           dest: var string; final: bool) {.raises.} =
  used = 0
  case a.framing
  of frUntilClose:
    used = src.len
    deliver(a, src, dest, final)
    if final: a.framingDone = true
  of frLength:
    let take = int(min(int64(src.len), a.left))
    if take > 0: deliver(a, toOpenArray(src, 0, take - 1), dest, false)
    used = take
    a.left -= int64(take)
    if a.left == 0:
      a.framingDone = true
      deliver(a, [], dest, true)
    elif final:
      raise EndOfStreamError                          # the body was cut short
  of frChunked:
    var i = 0
    while i < src.len and a.state != csDone:
      case a.state
      of csSize, csTrailer:
        let c = src[i]
        inc i
        if c == '\n':
          var line = a.line
          if line.len > 0 and line[^1] == '\r': line.setLen(line.len - 1)
          a.line = ""
          if a.state == csSize:
            a.left = parseHexSize(line)
            a.state = if a.left == 0: csTrailer else: csData
          elif line.len == 0:
            a.state = csDone                          # end of trailers
        else:
          if a.line.len >= 8 * 1024: raise SyntaxError
          a.line.add c
      of csData:
        let take = int(min(int64(src.len - i), a.left))
        deliver(a, toOpenArray(src, i, i + take - 1), dest, false)
        i += take
        a.left -= int64(take)
        if a.left == 0: a.state = csDataEnd
      of csDataEnd:
        # the CRLF after a chunk's data
        let c = src[i]
        inc i
        if c == '\n': a.state = csSize
        elif c != '\r': raise SyntaxError
      of csDone:
        discard
    used = i
    if a.state == csDone:
      a.framingDone = true
      deliver(a, [], dest, true)
    elif final:
      raise EndOfStreamError

method hasMore(a: HttpBodyAdapter): bool =
  not a.framingDone

# ---------------------------------------------------------------- requests --

const
  Reserved = ["host", "content-length", "transfer-encoding", "connection"]

proc checkUserHeaders(h: HttpHeaders) {.raises.} =
  # `HttpHeaders("…")` is a plain conversion that `add`'s checks never saw.
  if not validHeaderBlock(string(h)): raise BadOperation
  for (n, v) in pairs(h):
    for r in Reserved:
      if cmpIgnoreCase(n, r) == 0: raise BadOperation

type
  Target = object
    address, host, path: string
    secure: bool

proc checkWireText(s: string) {.raises.} =
  ## A URL part that is written into the request line or the `Host` header
  ## verbatim: a space would end the request target, a CR/LF the line, and
  ## both would let the URL write a request of its own. Such bytes have to
  ## arrive percent-encoded.
  for c in s:
    if c <= ' ' or c == '\x7F': raise BadOperation

proc targetOf(url: string): Target {.raises.} =
  let u = parseUri(url)
  checkWireText(u.hostname)
  checkWireText(u.port)
  checkWireText(u.path)
  checkWireText(u.query)
  let scheme = toLowerAscii(u.scheme)
  if scheme != "http" and scheme != "https": raise BadOperation
  if u.hostname.len == 0: raise BadOperation
  let secure = scheme == "https"
  let port = if u.port.len > 0: u.port elif secure: "443" else: "80"
  var host = u.hostname
  if u.port.len > 0: host.add ":" & u.port
  var path = if u.path.len > 0: u.path else: "/"
  if u.query.len > 0: path.add "?" & u.query
  result = Target(address: u.hostname & ":" & port, host: host, path: path,
                  secure: secure)

proc requestHead(meth: string; t: Target; headers: HttpHeaders;
                 framing: string): string {.raises.} =
  checkUserHeaders(headers)
  result = meth & " " & t.path & " HTTP/1.1\r\nHost: " & t.host & "\r\n"
  if not headers.has("User-Agent"): result.add "User-Agent: nimony\r\n"
  if not headers.has("Accept-Encoding"): result.add "Accept-Encoding: gzip, deflate\r\n"
  result.add "Connection: close\r\n"
  result.add framing
  result.add string(headers)
  result.add "\r\n"

proc engineFor(t: Target; tls: nil TlsEngine): nil TlsEngine {.raises.} =
  if t.secure:
    result = (if tls != nil: tls else: newTlsEngine())
  else:
    if tls != nil: raise BadOperation   # an engine on plain http is a mistake
    result = nil

proc readHead(r: var Reader; response: var HttpHeaders;
              reason: var string): int {.passive, raises.} =
  ## The status line and headers of the final (non-1xx) response: answers the
  ## status code, puts the reason phrase into `reason`.
  result = 0
  while true:
    let status = r.readLine()
    # `HTTP/1.1 200 OK`
    let sp = status.find(' ')
    if not status.startsWith("HTTP/1.") or sp < 0 or status.len < sp + 4:
      raise ProtocolError
    result = 0
    for k in sp + 1 .. sp + 3:
      if status[k] notin {'0'..'9'}: raise ProtocolError
      result = result * 10 + (ord(status[k]) - ord('0'))
    if status.len > sp + 4 and status[sp + 4] != ' ': raise ProtocolError
    reason = status.substr(sp + 5).strip()
    var h = ""
    var count = 0
    while true:
      let line = r.readLine()
      if line.len == 0: break
      inc count
      if count > 200 or line.len > 16 * 1024: raise FullError
      h.add line
      h.add "\r\n"
    response = HttpHeaders(h)
    if result >= 200 or result == 101: break    # 1xx interim responses are skipped

proc setBodyAdapter(r: var Reader; code: int; response: HttpHeaders;
                    noBody: bool) {.passive, raises.} =
  ## Puts the body adapter into `r`'s adapter slot. `noBody`: the response to a
  ## `HEAD`, whose framing headers describe a body that is not sent.
  var a = HttpBodyAdapter(framing: frUntilClose, left: 0, state: csSize, line: "",
                          coded: false, dec: initDecoder(fmtDetect), decDone: false,
                          framingDone: false)
  let te = toLowerAscii(response.get("Transfer-Encoding"))
  let cl = response.get("Content-Length")
  if noBody or code == 204 or code == 205 or code == 304:
    discard                                     # framed as empty below
  elif te.len > 0:
    if not te.endsWith("chunked"): raise UnimplementedOperation
    a.framing = frChunked
  elif cl.len > 0:
    var n = 0'i64
    for c in cl:
      if c notin {'0'..'9'} or n > high(int64) div 16: raise ProtocolError
      n = n * 10 + int64(ord(c) - ord('0'))
    a.framing = frLength
    a.left = n
  if noBody or code == 204 or code == 205 or code == 304 or
      (a.framing == frLength and a.left == 0):
    a.framing = frLength
    a.left = 0
    a.framingDone = true
  let ce = toLowerAscii(response.get("Content-Encoding"))
  if a.framingDone:
    discard                                     # nothing to decode
  elif ce == "gzip" or ce == "x-gzip":
    a.coded = true
    a.dec = initDecoder(fmtGzip)
  elif ce == "deflate":
    a.coded = true
    a.dec = initDecoder(fmtDetect)
  elif ce.len > 0 and ce != "identity":
    raise UnimplementedOperation
  r.setAdapter a

proc readResponse(r: var Reader; response: var HttpHeaders) {.passive, raises.} =
  ## The status line and headers; raises for a status outside 2xx, then puts
  ## the body adapter into `r`'s adapter slot.
  var reason = ""
  let code = readHead(r, response, reason)
  if code < 200 or code > 299:
    raise httpToErrorCode(code)
  setBodyAdapter(r, code, response, false)

proc httpGet*(url: string; headers: HttpHeaders; response: out HttpHeaders;
              tls: nil TlsEngine = nil; dl = never): Reader {.passive, raises.} =
  ## The body of `url`; the response headers in `response`. `dl` bounds the
  ## whole exchange and stays the returned reader's standing deadline
  ## (`renew` lifts it); `TimeoutError` on expiry.
  let t = targetOf(url)
  let req = requestHead("GET", t, headers, "")
  var (r, w) = connect(t.address, req, engineFor(t, tls), dl)
  w.detachWriter()
  r.renew(dl)
  response = HttpHeaders("")
  readResponse(r, response)
  result = r

proc httpGet*(url: string; headers = HttpHeaders("");
              tls: nil TlsEngine = nil; dl = never): Reader {.passive, raises.} =
  ## The body of `url`.
  var response = HttpHeaders("")
  result = httpGet(url, headers, response, tls, dl)

proc upload(meth, url: string; body: sink Reader; contentType: string;
            headers: HttpHeaders; response: var HttpHeaders;
            tls: nil TlsEngine; dl: Deadline): Reader {.passive, raises.} =
  let t = targetOf(url)
  var hs = headers
  if not hs.has("Content-Type"): hs.add("Content-Type", contentType)
  let req = requestHead(meth, t, hs, "Transfer-Encoding: chunked\r\n")
  var (r, w) = connect(t.address, tls = engineFor(t, tls), dl = dl)
  w.renew(dl)
  var b = body
  w.write req
  var chunk = newString(16 * 1024)
  try:
    while true:
      let n = b.read(chunk)
      if n == 0: break
      w.write toHex(n).strip(chars = {'0'}, trailing = false) & "\r\n"
      w.write toOpenArray(chunk, 0, n - 1)
      w.write "\r\n"
  except ErrorCode as e:
    # The body failed: the server must NOT see a complete-looking request.
    w.abort()
    raise e
  w.write "0\r\n\r\n"
  w.flush()
  w.detachWriter()
  readResponse(r, response)
  result = r

proc httpPost*(url: string; body: sink Reader;
               contentType = "application/octet-stream";
               headers = HttpHeaders(""); tls: nil TlsEngine = nil;
               dl = never): Reader {.passive, raises.} =
  ## Sends `body` (streamed, chunked) and returns the response body.
  var response = HttpHeaders("")
  result = upload("POST", url, body, contentType, headers, response, tls, dl)

proc httpPost*(url: string; body: sink Reader; contentType: string;
               headers: HttpHeaders; response: out HttpHeaders;
               tls: nil TlsEngine = nil; dl = never): Reader {.
    passive, raises.} =
  response = HttpHeaders("")
  result = upload("POST", url, body, contentType, headers, response, tls, dl)

proc httpPut*(url: string; body: sink Reader;
              contentType = "application/octet-stream";
              headers = HttpHeaders(""); tls: nil TlsEngine = nil;
              dl = never): Reader {.passive, raises.} =
  var response = HttpHeaders("")
  result = upload("PUT", url, body, contentType, headers, response, tls, dl)

# ------------------------------------------------- any status, redirects --

type
  HttpResponse* = object
    ## A response of any status, as `httpRequest` returns it.
    code*: int             ## the status code: 200, 301, 403, 404, ...
    reason*: string        ## the reason phrase of the status line; may be empty
    headers*: HttpHeaders  ## the response headers
    url*: string           ## the URL that answered: the request's own, or
                           ## the last redirect target that was followed
    body*: Reader          ## the de-framed, decoded body, whatever the status

proc isRedirect*(code: int): bool {.inline.} =
  ## Whether `code` is a redirect `httpRequest` follows: 301, 302, 303, 307
  ## or 308.
  code == 301 or code == 302 or code == 303 or code == 307 or code == 308

proc checkMethod(meth: string) {.raises.} =
  if meth.len == 0: raise BadOperation
  for c in meth:
    if not isTokenChar(c): raise BadOperation

proc withoutHeaders(h: HttpHeaders; names: openArray[string]): HttpHeaders =
  var s = ""
  for (n, v) in pairs(h):
    var keep = true
    for d in names:
      if cmpIgnoreCase(n, d) == 0: keep = false
    if keep:
      s.add n
      s.add ": "
      s.add v
      s.add "\r\n"
  result = HttpHeaders(s)

proc effectivePort(u: Uri): string =
  if u.port.len > 0: u.port
  elif cmpIgnoreCase(u.scheme, "https") == 0: "443"
  else: "80"

proc sameOrigin(a, b: Uri): bool =
  ## Same scheme, host and port.
  result = cmpIgnoreCase(a.scheme, b.scheme) == 0 and
    cmpIgnoreCase(a.hostname, b.hostname) == 0 and
    effectivePort(a) == effectivePort(b)

proc exchange(meth, url, body: string; headers: HttpHeaders;
              tls: nil TlsEngine; dl: Deadline): HttpResponse {.passive, raises.} =
  ## One request, one response of any status.
  let t = targetOf(url)
  let framing =
    if body.len > 0 or meth == "POST" or meth == "PUT" or meth == "PATCH":
      "Content-Length: " & $body.len & "\r\n"
    else: ""
  var req = requestHead(meth, t, headers, framing)
  req.add body
  var (r, w) = connect(t.address, req, engineFor(t, tls), dl)
  w.detachWriter()
  r.renew(dl)
  var response = HttpHeaders("")
  var reason = ""
  let code = readHead(r, response, reason)
  setBodyAdapter(r, code, response, meth == "HEAD")
  result = HttpResponse(code: code, reason: reason, headers: response,
                        url: url, body: r)

proc httpRequest*(meth, url: string; body = ""; headers = HttpHeaders("");
                  maxRedirects = 5; tls: nil TlsEngine = nil;
                  dl = never): HttpResponse {.passive, raises.} =
  ## Sends `meth` to `url` and returns the response whatever its status: a
  ## 403 or 404 is an answer here, not an error, so `code` and `body` can be
  ## inspected. Raises only when there is no response to look at (DNS, TCP,
  ## TLS, a malformed reply, `TimeoutError`) or for a bad request
  ## (`BadOperation`: a method that is not a token, a reserved header, CR/LF
  ## in the URL).
  ##
  ## `body` is sent with `Content-Length`; set `Content-Type` in `headers`.
  ##
  ## Redirects (`isRedirect`) are followed up to `maxRedirects` times (`0`
  ## follows none), the rules of Nim's `std/httpclient`:
  ##
  ## * 301, 302, 303: the next request is a `GET` (a `HEAD` stays `HEAD`),
  ##   without the body and without the `Content-Type` header.
  ## * 307, 308: method and body are sent again unchanged.
  ## * `Location` is resolved against the URL that answered (`combine`), so a
  ##   relative one works. A redirect without `Location` is returned as is.
  ## * Once a redirect leaves the origin (scheme, host, port) of `url`, the
  ##   `Authorization` and `Cookie` headers are not sent any more.
  ## * A `tls` engine is single-use, and replacing it would discard whatever
  ##   it trusts: with one given, redirects are returned, not followed.
  ##
  ## When the limit is reached, the last redirect response is returned. `url`
  ## of the result is the URL that answered.
  ##
  ## `dl` bounds the whole exchange, redirects included, and stays the
  ## standing deadline of `body` (`renew(resp.body, never)` lifts it):
  ##
  ## .. code-block:: nim
  ##   var resp = httpRequest("GET", "https://example.com/", dl = afterMs(10_000))
  ##   if resp.code == 403: echo "refused (", resp.code, ")"
  ##   else: echo resp.url, ": ", resp.body.readAll()
  checkMethod(meth)
  result = exchange(meth, url, body, headers, tls, dl)
  if tls != nil: return
  let origin = parseUri(url)
  var m = meth
  var b = body
  var hs = headers
  var crossed = false
  var left = maxRedirects
  while left > 0 and isRedirect(result.code):
    let location = result.headers.get("Location")
    if location.len == 0: break
    let next = combine(parseUri(result.url), parseUri(location))
    if result.code == 301 or result.code == 302 or result.code == 303:
      if m != "GET" and m != "HEAD": m = "GET"
      b = ""
      hs = withoutHeaders(hs, ["Content-Type"])
    if not crossed and not sameOrigin(next, origin):
      # credentials stay with the origin they were given for, also when a
      # later redirect leads back
      crossed = true
      hs = withoutHeaders(hs, ["Authorization", "Cookie"])
    result = exchange(m, $next, b, hs, nil, dl)
    dec left
