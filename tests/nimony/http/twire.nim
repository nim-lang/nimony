# lib/std/http/httpwire — an HttpMsg back to wire bytes.

import std / [http/httpmsg, http/httpparse, http/httpwire, assertions, syncio]

let hTrace = registerHeader("x-trace-id")
sealHttpTags()

var wbuf = default(array[4096, char])

proc render(m: var HttpMsg): string =
  ## Serialize and materialize — for the test only; the writer itself never
  ## builds a string.
  let need = headLen(m)
  let n = writeHead(wbuf, 0, m)
  assert n == need, "headLen said " & $need & " but writeHead wrote " & $n
  result = ""
  for k in 0..<n: result.add wbuf[k]

proc testResponse =
  var m = initHttpMsg()
  m.startResponse(404)
  m.addHeader(hContentLength, 0)
  m.addHeader(hConnection, vClose)
  m.finish()
  assert render(m) ==
    "HTTP/1.1 404 Not Found\r\ncontent-length: 0\r\nconnection: close\r\n\r\n",
    render(m)

proc testRequest =
  var m = initHttpMsg()
  m.startRequest(tag(mPost), "/submit", tag(tV10))
  m.addHeader(hHost, "example.com")
  m.addHeader(hContentLength, 1234)
  m.addHeader(hTrace, "abc")
  m.addOtherHeader("X-Weird", "1")
  m.finish()
  assert render(m) ==
    "POST /submit HTTP/1.0\r\n" &
    "host: example.com\r\n" &
    "content-length: 1234\r\n" &
    "x-trace-id: abc\r\n" &
    "X-Weird: 1\r\n\r\n", render(m)

proc testMultiValue =
  var m = initHttpMsg()
  m.startResponse(200)
  m.addHeader(hAccept, ["text/html", "application/xhtml+xml", "*/*"])
  m.finish()
  assert render(m) ==
    "HTTP/1.1 200 OK\r\naccept: text/html, application/xhtml+xml, */*\r\n\r\n",
    render(m)

proc testReasonPhrases =
  assert reasonPhrase(200) == "OK"
  assert reasonPhrase(404) == "Not Found"
  assert reasonPhrase(503) == "Service Unavailable"
  assert reasonPhrase(299) == "Successful", "unlisted codes get their class"
  assert reasonPhrase(499) == "Client Error"
  assert reasonPhrase(599) == "Server Error"

var ibuf = default(array[32, char])

proc rendered(x: int): string =
  let n = writeInt(ibuf, 0, x)
  assert n > 0
  result = ""
  for k in 0..<n: result.add ibuf[k]

proc testInt =
  var b = default(array[32, char])
  assert rendered(0) == "0"
  assert rendered(7) == "7"
  assert rendered(10) == "10"
  assert rendered(1234) == "1234"
  assert rendered(high(int)) == $high(int)
  assert digitCount(0) == 1
  assert digitCount(9) == 1
  assert digitCount(10) == 2
  assert digitCount(999) == 3
  assert writeInt(b, 0, -1) == WriteFull, "nothing in a head is signed"

proc testTooSmall =
  # Every buffer shorter than the head must be refused, and refused cleanly:
  # the answer is WriteFull, never a partial index the caller would trust.
  var m = initHttpMsg()
  m.startResponse(200)
  m.addHeader(hContentLength, 5)
  m.finish()
  let need = headLen(m)
  var small = default(array[128, char])
  for n in 0..<need:
    assert writeHead(toOpenArray(small, 0, n - 1), 0, m) == WriteFull,
           "fits in " & $n & " but needs " & $need
  assert writeHead(toOpenArray(small, 0, need - 1), 0, m) == need

proc testOffset =
  # Writing at a non-zero offset must not disturb what is already there.
  var m = initHttpMsg()
  m.startResponse(204)
  m.finish()
  var b = default(array[256, char])
  b[0] = 'X'
  b[1] = 'Y'
  let n = writeHead(b, 2, m)
  assert n == 2 + headLen(m)
  assert b[0] == 'X' and b[1] == 'Y'
  assert b[2] == 'H'

proc testRoundTrip =
  # Parse, serialize, parse again: the second parse must see exactly what the
  # first one did, and the second serialization must be byte-identical to the
  # first. That is the property that says the tree lost nothing.
  let wire = "POST /a/b?c=d HTTP/1.1\r\n" &
             "Host: example.com\r\n" &
             "Content-Length: 42\r\n" &
             "Connection: keep-alive\r\n" &
             "Transfer-Encoding: chunked\r\n" &
             "X-Trace-Id: t-1\r\n" &
             "X-Unregistered: raw\r\n\r\n"
  var m1 = initHttpMsg()
  assert parseRequestHead(toOpenArray(wire, 0, wire.len - 1), 0, m1) == wire.len
  let once = render(m1)

  var m2 = initHttpMsg()
  let n = parseRequestHead(toOpenArray(once, 0, once.len - 1), 0, m2)
  assert n == once.len, "re-parsing our own output: " & $n & " of " & $once.len
  let twice = render(m2)
  assert once == twice, "not idempotent:\n" & once & "---\n" & twice

  # …and the second message says the same things as the first.
  assert m2.methodOf == m1.methodOf
  assert m2.target == "/a/b?c=d"
  assert m2.contentLength == 42
  assert m2.getTag(hConnection) == tag(vKeepAlive)
  assert m2.getTag(hTransferEncoding) == tag(vChunked)
  assert m2.getStr(hTrace) == "t-1"
  var others = ""
  for k, v in m2.otherHeaders: others.add k & "=" & v & ";"
  assert others == "X-Unregistered=raw;", others

proc testRecycle =
  var m = initHttpMsg()
  for i in 0..<50:
    m.reset()
    m.startResponse(200 + (i mod 3))
    m.addHeader(hContentLength, i)
    m.finish()
    let s = render(m)
    assert s.len == headLen(m)

testResponse()
testRequest()
testMultiValue()
testReasonPhrases()
testInt()
testTooSmall()
testOffset()
testRoundTrip()
testRecycle()
echo "httpwire: ok"
