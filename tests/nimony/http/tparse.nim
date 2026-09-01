# lib/std/http/httpparse — wire bytes to an HttpMsg, without slicing.

import std / [http/httpmsg, http/httpparse, assertions, syncio]

let hTrace = registerHeader("x-trace-id")
let poolSizeAtSeal = httpTags().tags.len
sealHttpTags()

proc parseAll(s: string; m: var HttpMsg): int =
  ## Parse a complete head held in one buffer.
  parseRequestHead(toOpenArray(s, 0, s.len - 1), 0, m)

proc testSimple =
  var m = initHttpMsg()
  let req = "GET /index.html HTTP/1.1\r\nHost: example.com\r\n\r\n"
  let n = parseAll(req, m)
  assert n == req.len, $n
  assert m.isRequest
  assert m.methodOf == tag(mGet)
  assert m.target == "/index.html"
  assert m.versionOf == tag(tV11)
  assert m.getStr(hHost) == "example.com"

proc testTypedValues =
  var m = initHttpMsg()
  let req = "POST /submit HTTP/1.1\r\n" &
            "Host: h\r\n" &
            "Content-Length: 1234\r\n" &
            "Connection: keep-alive\r\n" &
            "Content-Encoding: gzip\r\n\r\n"
  assert parseAll(req, m) == req.len
  # Content-Length arrived as digits and is stored as an integer.
  assert m.contentLength == 1234
  assert m.getInt(hContentLength) == 1234
  # A known value became a tag, so the check downstream is an integer compare.
  assert m.getTag(hConnection) == tag(vKeepAlive)
  assert m.isKeepAlive
  assert m.getTag(hContentEncoding) == tag(vGzip)

proc testCaseAndWhitespace =
  var m = initHttpMsg()
  let req = "GET / HTTP/1.1\r\n" &
            "HOST:   example.com   \r\n" &          # folded name, OWS both sides
            "CoNtEnT-lEnGtH:0\r\n" &                # no space after colon
            "X-Trace-Id: abc\r\n\r\n"
  assert parseAll(req, m) == req.len
  assert m.getStr(hHost) == "example.com", "'" & m.getStr(hHost) & "'"
  assert m.contentLength == 0
  assert m.getStr(hTrace) == "abc"

proc testUnknownHeader =
  var m = initHttpMsg()
  let req = "GET / HTTP/1.1\r\nX-Weird: 1\r\nX-Other: two\r\n\r\n"
  assert parseAll(req, m) == req.len
  var got = ""
  for k, v in m.otherHeaders: got.add k & "=" & v & ";"
  assert got == "X-Weird=1;X-Other=two;", got
  assert httpTags().tags.len == poolSizeAtSeal,
         "unknown names never reach the pool"

proc testEmptyValue =
  var m = initHttpMsg()
  let req = "GET / HTTP/1.1\r\nHost:\r\n\r\n"
  assert parseAll(req, m) == req.len
  assert hHost in m
  assert m.getStr(hHost) == ""

proc testLfOnly =
  var m = initHttpMsg()
  let req = "GET /x HTTP/1.0\nHost: h\n\n"
  assert parseAll(req, m) == req.len
  assert m.target == "/x"
  assert m.versionOf == tag(tV10)
  assert not m.isKeepAlive, "HTTP/1.0 without Connection is not keep-alive"

proc testVersions =
  var v = TagId(0)
  assert parseVersion(toOpenArray("HTTP/1.1", 0, 7), 0, v) == 8 and v == tag(tV11)
  assert parseVersion(toOpenArray("HTTP/1.0", 0, 7), 0, v) == 8 and v == tag(tV10)
  assert parseVersion(toOpenArray("HTTP/2", 0, 5), 0, v) == 6 and v == tag(tV20)
  assert parseVersion(toOpenArray("HTTP/1.", 0, 6), 0, v) == ParseIncomplete
  assert parseVersion(toOpenArray("HTTP/1.9", 0, 7), 0, v) == ParseBad
  assert parseVersion(toOpenArray("HTTQ/1.1", 0, 7), 0, v) == ParseBad

proc testRejections =
  proc bad(req: string): bool =
    var m = initHttpMsg()
    result = parseAll(req, m) == ParseBad

  assert bad("BREW / HTTP/1.1\r\n\r\n"), "unknown method"
  assert bad("get / HTTP/1.1\r\n\r\n"), "methods are case-sensitive"
  assert bad("GET  HTTP/1.1\r\n\r\n"), "empty target"
  assert bad("GET / HTTP/1.1\r\nHost example.com\r\n\r\n"), "no colon"
  assert bad("GET / HTTP/1.1\r\nHost : h\r\n\r\n"), "space before the colon"
  assert bad("GET / HTTP/1.1\r\n Host: h\r\n\r\n"), "obs-fold is rejected"
  assert bad("GET / HTTP/1.1\r\nHost: a\rb\r\n\r\n"), "bare CR inside a value"
  assert bad("GET / HTTP/1.1\r\nContent-Length: abc\r\n\r\n"), "non-numeric length"
  assert bad("GET / HTTP/1.1\r\nContent-Length: \r\n\r\n"), "empty length"
  assert bad("GET / HTTP/1.1\r\nContent-Length: 12x\r\n\r\n"), "trailing junk"

  # Framing must be unambiguous, or two hops can disagree about where the
  # message ends and an attacker picks which one believes which.
  assert bad("GET / HTTP/1.1\r\nContent-Length: 5\r\n" &
             "Transfer-Encoding: chunked\r\n\r\n"), "Content-Length with TE"
  assert bad("GET / HTTP/1.1\r\nContent-Length: 5\r\nContent-Length: 5\r\n\r\n"),
         "duplicate Content-Length, even when they agree"
  assert bad("GET / HTTP/1.1\r\nContent-Length: 5\r\nContent-Length: 6\r\n\r\n"),
         "duplicate Content-Length that disagree"
  assert bad("GET / HTTP/1.1\r\nTransfer-Encoding: chunked\r\n" &
             "Transfer-Encoding: chunked\r\n\r\n"), "duplicate TE"

  # A length that cannot fit an int must be refused, not wrapped.
  assert bad("GET / HTTP/1.1\r\nContent-Length: " &
             "99999999999999999999999\r\n\r\n"), "overflowing length"

  # Header count is capped.
  var many = "GET / HTTP/1.1\r\n"
  for i in 0..<(MaxHeaderCount + 5): many.add "X-H" & $i & ": v\r\n"
  many.add "\r\n"
  assert bad(many), "header count limit"

proc testIncomplete =
  # Every proper prefix of a complete head must say "not yet", never "bad".
  let req = "GET /a HTTP/1.1\r\nHost: h\r\nContent-Length: 5\r\n\r\n"
  for n in 1..<req.len:
    var m = initHttpMsg()
    let r = parseRequestHead(toOpenArray(req, 0, n - 1), 0, m)
    assert r == ParseIncomplete, "prefix of length " & $n & " gave " & $r
  var m = initHttpMsg()
  assert parseRequestHead(toOpenArray(req, 0, req.len - 1), 0, m) == req.len

proc testFindHeadEnd =
  let req = "GET /a HTTP/1.1\r\nHost: h\r\n\r\nBODYBODY"
  let headLen = req.len - "BODYBODY".len

  # All at once.
  var sc = default(HeadScanner)
  assert findHeadEnd(sc, toOpenArray(req, 0, req.len - 1), 0) == headLen

  # One byte at a time, with the scanner carried across: it must land on the
  # same place, and resuming must not miss a terminator that straddles a read.
  var sc2 = default(HeadScanner)
  var found = ParseIncomplete
  for n in 1..req.len:
    found = findHeadEnd(sc2, toOpenArray(req, 0, n - 1), 0)
    if found >= 0:
      assert n >= headLen, "found the end before it arrived"
      break
  assert found == headLen, $found

  # LF-only terminator.
  var sc3 = default(HeadScanner)
  let lf = "GET / HTTP/1.1\nHost: h\n\nbody"
  assert findHeadEnd(sc3, toOpenArray(lf, 0, lf.len - 1), 0) == lf.len - 4

  # No terminator at all.
  var sc4 = default(HeadScanner)
  let partial = "GET / HTTP/1.1\r\nHost: h\r\n"
  assert findHeadEnd(sc4, toOpenArray(partial, 0, partial.len - 1), 0) ==
         ParseIncomplete

proc testPipelined =
  # Two requests in one buffer: parsing the first must stop exactly at the
  # second, so the caller can carry on from the returned index.
  let two = "GET /one HTTP/1.1\r\nHost: a\r\n\r\n" &
            "GET /two HTTP/1.1\r\nHost: b\r\n\r\n"
  var m = initHttpMsg()
  let n = parseRequestHead(toOpenArray(two, 0, two.len - 1), 0, m)
  assert n > 0
  assert m.target == "/one"
  assert m.getStr(hHost) == "a"

  m.reset()
  let n2 = parseRequestHead(toOpenArray(two, 0, two.len - 1), n, m)
  assert n2 == two.len, $n2
  assert m.target == "/two"
  assert m.getStr(hHost) == "b"

proc testRecycleAcrossParses =
  # The keep-alive shape: one message reused for every request on a connection.
  var m = initHttpMsg()
  for i in 0..<50:
    m.reset()
    let req = "GET /p" & $i & " HTTP/1.1\r\nHost: h" & $i & "\r\n" &
              "Content-Length: " & $i & "\r\n\r\n"
    assert parseAll(req, m) == req.len
    assert m.target == "/p" & $i
    assert m.getStr(hHost) == "h" & $i
    assert m.contentLength == i

testSimple()
testTypedValues()
testCaseAndWhitespace()
testUnknownHeader()
testEmptyValue()
testLfOnly()
testVersions()
testRejections()
testIncomplete()
testFindHeadEnd()
testPipelined()
testRecycleAcrossParses()
echo "httpparse: ok"
