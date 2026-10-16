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
## * A status outside 2xx RAISES the `ErrorCode` `httpToErrorCode` maps it to
##   (404 is `NameNotFound`, 401/403 `PermissionDenied`, ...), so a `Reader`
##   returned from here always carries a successful body.
## * The body is de-framed (`Content-Length`, chunked, or until the server
##   closes) and its content coding undone (gzip, deflate) by an adapter in the
##   reader's adapter slot. Trailers are read and dropped.
## * Request headers are an `HttpHeaders`, the wire format itself. The library
##   writes `Host`, `Content-Length`/`Transfer-Encoding` and `Connection`
##   itself; a user header with one of those names raises `BadOperation`
##   (conflicting framing headers are how request smuggling works).
##   `User-Agent` and `Accept-Encoding` have defaults a user header replaces.
##
## One request per connection (`Connection: close`); redirects are raised
## (`RedirectError`), not followed.

{.feature: "lenientnils".}

import std/[streamio, tls, uri, strutils]
import std/http/httpheaders
import std/compress/gzip
import std/errorcodes/errorcodes_http

export streamio, httpheaders

# ------------------------------------------------------------------ headers --

template headers*(s: string): HttpHeaders {.plugin: "deps/httpheaders".}
  ## A header block literal, checked while the program is compiled:
  ## `headers"Accept: application/json\r\n"`. A malformed block is a
  ## compile-time error. Values known only at run time go through `add`.

proc add*(h: var HttpHeaders; name, value: string) {.raises.} =
  ## Appends `name: value`. `BadOperation` for a name outside the token
  ## characters or a CR/LF in the value — header injection is caught here, at
  ## the one place it can be.
  if name.len == 0: raise BadOperation
  for c in name:
    if not isTokenChar(c): raise BadOperation
  for c in value:
    if c in {'\r', '\n'}: raise BadOperation
  var s = string(h)
  s.add name
  s.add ": "
  s.add value
  s.add "\r\n"
  h = HttpHeaders(s)

func eqIgnoreCase(a, b: string): bool =
  if a.len != b.len: return false
  result = true
  for i in 0 ..< a.len:
    if toLowerAscii(a[i]) != toLowerAscii(b[i]): return false

iterator lines(h: HttpHeaders): (string, string) =
  let s = string(h)
  var i = 0
  while i < s.len:
    let eol = s.find("\r\n", i)
    let stop = if eol < 0: s.len else: eol
    let colon = s.find(':', i)
    if colon > i and colon < stop:
      yield (s.substr(i, colon - 1), s.substr(colon + 1, stop - 1).strip())
    i = stop + 2

iterator values*(h: HttpHeaders; name: string): string =
  ## Every value of `name`, case-insensitively, in order.
  for (n, v) in lines(h):
    if eqIgnoreCase(n, name): yield v

proc get*(h: HttpHeaders; name: string): string =
  ## The first value of `name`, case-insensitively; `""` if absent.
  result = ""
  for v in values(h, name): return v

proc has(h: HttpHeaders; name: string): bool =
  result = false
  for (n, v) in lines(h):
    if eqIgnoreCase(n, name): return true

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
  for (n, v) in lines(h):
    for r in Reserved:
      if eqIgnoreCase(n, r): raise BadOperation

type
  Target = object
    address, host, path: string
    secure: bool

proc targetOf(url: string): Target {.raises.} =
  let u = parseUri(url)
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

proc readResponse(r: var Reader; response: var HttpHeaders) {.passive, raises.} =
  ## The status line and headers; raises for a status outside 2xx, then puts
  ## the body adapter into `r`'s adapter slot.
  var code = 0
  while true:
    let status = r.readLine()
    # `HTTP/1.1 200 OK`
    let sp = status.find(' ')
    if not status.startsWith("HTTP/1.") or sp < 0 or status.len < sp + 4:
      raise ProtocolError
    code = 0
    for k in sp + 1 .. sp + 3:
      if status[k] notin {'0'..'9'}: raise ProtocolError
      code = code * 10 + (ord(status[k]) - ord('0'))
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
    if code >= 200 or code == 101: break        # 1xx interim responses are skipped
  if code < 200 or code > 299:
    raise httpToErrorCode(code)
  var a = HttpBodyAdapter(framing: frUntilClose, left: 0, state: csSize, line: "",
                          coded: false, dec: initDecoder(fmtDetect), decDone: false,
                          framingDone: false)
  let te = toLowerAscii(response.get("Transfer-Encoding"))
  let cl = response.get("Content-Length")
  if te.len > 0:
    if not te.endsWith("chunked"): raise UnimplementedOperation
    a.framing = frChunked
  elif cl.len > 0:
    var n = 0'i64
    for c in cl:
      if c notin {'0'..'9'} or n > high(int64) div 16: raise ProtocolError
      n = n * 10 + int64(ord(c) - ord('0'))
    a.framing = frLength
    a.left = n
  if code == 204 or code == 205 or (a.framing == frLength and a.left == 0):
    a.framing = frLength
    a.left = 0
    a.framingDone = true
  let ce = toLowerAscii(response.get("Content-Encoding"))
  if ce == "gzip" or ce == "x-gzip":
    a.coded = true
    a.dec = initDecoder(fmtGzip)
  elif ce == "deflate":
    a.coded = true
    a.dec = initDecoder(fmtDetect)
  elif ce.len > 0 and ce != "identity":
    raise UnimplementedOperation
  r.setAdapter a

proc httpGet*(url: string; headers: HttpHeaders; response: out HttpHeaders;
              tls: nil TlsEngine = nil): Reader {.passive, raises.} =
  ## The body of `url`; the response headers in `response`.
  let t = targetOf(url)
  let req = requestHead("GET", t, headers, "")
  var (r, w) = connect(t.address, req, engineFor(t, tls))
  w.detachWriter()
  response = HttpHeaders("")
  readResponse(r, response)
  result = r

proc httpGet*(url: string; headers = HttpHeaders("");
              tls: nil TlsEngine = nil): Reader {.passive, raises.} =
  ## The body of `url`.
  var response = HttpHeaders("")
  result = httpGet(url, headers, response, tls)

proc upload(meth, url: string; body: sink Reader; contentType: string;
            headers: HttpHeaders; response: var HttpHeaders;
            tls: nil TlsEngine): Reader {.passive, raises.} =
  let t = targetOf(url)
  var hs = headers
  if not hs.has("Content-Type"): hs.add("Content-Type", contentType)
  let req = requestHead(meth, t, hs, "Transfer-Encoding: chunked\r\n")
  var (r, w) = connect(t.address, tls = engineFor(t, tls))
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
               headers = HttpHeaders(""); tls: nil TlsEngine = nil): Reader {.
    passive, raises.} =
  ## Sends `body` (streamed, chunked) and returns the response body.
  var response = HttpHeaders("")
  result = upload("POST", url, body, contentType, headers, response, tls)

proc httpPost*(url: string; body: sink Reader; contentType: string;
               headers: HttpHeaders; response: out HttpHeaders;
               tls: nil TlsEngine = nil): Reader {.passive, raises.} =
  response = HttpHeaders("")
  result = upload("POST", url, body, contentType, headers, response, tls)

proc httpPut*(url: string; body: sink Reader;
              contentType = "application/octet-stream";
              headers = HttpHeaders(""); tls: nil TlsEngine = nil): Reader {.
    passive, raises.} =
  var response = HttpHeaders("")
  result = upload("PUT", url, body, contentType, headers, response, tls)
