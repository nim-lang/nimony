# Response-head parsing, and what a response round trip does and does not keep.

import std / [http/httpmsg, http/httpparse, http/httpwire, assertions, syncio]
import httptags

let poolSizeAtInit = httpTags().tags.len

var wbuf = default(array[4096, char])

proc parseAll(s: string; m: var HttpMsg): int =
  parseResponseHead(toOpenArray(s, 0, s.len - 1), 0, m)

proc render(m: var HttpMsg): string =
  let need = headLen(m)
  let n = writeHead(wbuf, 0, m)
  assert n == need, "headLen " & $need & " vs writeHead " & $n
  result = ""
  for k in 0..<n: result.add wbuf[k]

proc testSimple =
  var m = initHttpMsg()
  let res = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n"
  assert parseAll(res, m) == res.len
  assert m.isResponse and not m.isRequest
  assert m.statusOf == 200
  assert m.versionOf == tag(tV11)
  assert m.contentLength == 5
  assert m.methodOf.uint32 == 0'u32
  assert m.target == ""

proc testStatusLineShapes =
  proc statusOf(res: string): int =
    var m = initHttpMsg()
    if parseAll(res, m) < 0: return -1
    result = m.statusOf

  assert statusOf("HTTP/1.1 204 No Content\r\n\r\n") == 204
  assert statusOf("HTTP/1.0 301 Moved Permanently\r\n\r\n") == 301
  assert statusOf("HTTP/1.1 200\r\n\r\n") == 200, "reason phrase is optional"
  assert statusOf("HTTP/1.1 200 \r\n\r\n") == 200, "and may be empty"
  assert statusOf("HTTP/1.1 599 Whatever\r\n\r\n") == 599
  assert statusOf("HTTP/1.1 418 I'm a teapot\r\n\r\n") == 418
  assert statusOf("HTTP/1.1 200 OK\twith a tab\r\n\r\n") == 200,
         "HTAB is allowed in a reason phrase"

proc testRejections =
  proc bad(res: string): bool =
    var m = initHttpMsg()
    result = parseAll(res, m) == ParseBad

  assert bad("HTTP/1.1 20 OK\r\n\r\n"), "status must be three digits"
  assert bad("HTTP/1.1 2000 OK\r\n\r\n"), "…and no more"
  assert bad("HTTP/1.1 099 X\r\n\r\n"), "below 100"
  assert bad("HTTP/1.1 600 X\r\n\r\n"), "above 599"
  assert bad("HTTP/1.1 2x0 OK\r\n\r\n"), "non-digit"
  assert bad("HTTP/1.1200 OK\r\n\r\n"), "no space after the version"
  assert bad("HTTQ/1.1 200 OK\r\n\r\n"), "not HTTP"
  assert bad("200 OK\r\n\r\n"), "no version"
  assert bad("HTTP/1.1 200 OK\r\nContent-Length: x\r\n\r\n"), "bad length"
  assert bad("HTTP/1.1 200 OK\r\n Server: x\r\n\r\n"), "obs-fold"
  assert bad("HTTP/1.1 200 OK\r\nServer: a\rb\r\n\r\n"), "bare CR in a value"

proc testIncomplete =
  # As with requests: every proper prefix says "not yet", never "bad".
  let res = "HTTP/1.1 404 Not Found\r\nServer: x\r\nContent-Length: 0\r\n\r\n"
  for n in 1..<res.len:
    var m = initHttpMsg()
    let r = parseResponseHead(toOpenArray(res, 0, n - 1), 0, m)
    assert r == ParseIncomplete, "prefix of length " & $n & " gave " & $r
  var m = initHttpMsg()
  assert parseResponseHead(toOpenArray(res, 0, res.len - 1), 0, m) == res.len

proc testTypedValues =
  var m = initHttpMsg()
  let res = "HTTP/1.1 200 OK\r\n" &
            "Content-Length: 1234\r\n" &
            "Connection: close\r\n" &
            "Content-Encoding: gzip\r\n" &
            "X-Trace-Id: t\r\n" &
            "X-Odd: raw\r\n\r\n"
  assert parseAll(res, m) == res.len
  assert m.contentLength == 1234
  assert m.getTag(hConnection) == tag(vClose)
  assert not m.isKeepAlive
  assert m.getTag(hContentEncoding) == tag(vGzip)
  assert m.getStr(hTrace) == "t"
  var others = ""
  for k, v in m.otherHeaders: others.add k & "=" & v & ";"
  assert others == "X-Odd=raw;", others
  assert httpTags().tags.len == poolSizeAtInit

proc testRoundTrip =
  # The tree round-trips; the reason phrase deliberately does not.
  let res = "HTTP/1.1 404 Not Found\r\n" &
            "Content-Length: 0\r\n" &
            "Connection: keep-alive\r\n\r\n"
  var m1 = initHttpMsg()
  assert parseAll(res, m1) == res.len
  let once = render(m1)
  assert once == "HTTP/1.1 404 Not Found\r\n" &
                 "content-length: 0\r\n" &
                 "connection: keep-alive\r\n\r\n", once

  var m2 = initHttpMsg()
  assert parseAll(once, m2) == once.len
  assert render(m2) == once, "re-serializing is idempotent"
  assert m2.statusOf == 404
  assert m2.contentLength == 0
  assert m2.getTag(hConnection) == tag(vKeepAlive)

proc testReasonIsDropped =
  # A non-standard phrase is parsed and thrown away; the canonical one comes
  # back instead. This is the one place a response is not byte-preserving,
  # and it is on purpose.
  var m = initHttpMsg()
  let res = "HTTP/1.1 404 Totally Not Here\r\n\r\n"
  assert parseAll(res, m) == res.len
  assert m.statusOf == 404
  assert render(m) == "HTTP/1.1 404 Not Found\r\n\r\n", render(m)
  # …but re-parsing that is still stable.
  var m2 = initHttpMsg()
  let once = render(m)
  assert parseAll(once, m2) == once.len
  assert render(m2) == once

proc testFindHeadEndOnResponses =
  let res = "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nBODY"
  let headLen = res.len - 4
  var sc = default(HeadScanner)
  assert findHeadEnd(sc, toOpenArray(res, 0, res.len - 1), 0) == headLen
  var sc2 = default(HeadScanner)
  var found = ParseIncomplete
  for n in 1..res.len:
    found = findHeadEnd(sc2, toOpenArray(res, 0, n - 1), 0)
    if found >= 0: break
  assert found == headLen

proc testPipelinedAndRecycled =
  let two = "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" &
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n"
  var m = initHttpMsg()
  let n = parseResponseHead(toOpenArray(two, 0, two.len - 1), 0, m)
  assert n > 0 and m.statusOf == 200
  m.reset()
  assert parseResponseHead(toOpenArray(two, 0, two.len - 1), n, m) == two.len
  assert m.statusOf == 500

  for i in 0..<50:
    m.reset()
    let r = "HTTP/1.1 " & $(200 + i mod 100) & " X\r\nContent-Length: " &
            $i & "\r\n\r\n"
    assert parseAll(r, m) == r.len
    assert m.statusOf == 200 + i mod 100
    assert m.contentLength == i

testSimple()
testStatusLineShapes()
testRejections()
testIncomplete()
testTypedValues()
testRoundTrip()
testReasonIsDropped()
testFindHeadEndOnResponses()
testPipelinedAndRecycled()
echo "response parsing: ok"
