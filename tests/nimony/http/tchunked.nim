# Chunked framing: the parser, the writer, and a real chunked exchange.

import std / [http/httpmsg, http/httpparse, http/httpwire, assertions, syncio]

sealHttpTags()

proc testParseChunkSize =
  proc sz(s: string; expect: int; consumed: int) =
    var got = -1
    let n = parseChunkSize(toOpenArray(s, 0, s.len - 1), 0, got)
    assert n == consumed, s & ": index " & $n & " wanted " & $consumed
    assert got == expect, s & ": size " & $got & " wanted " & $expect

  sz("5\r\n", 5, 3)
  sz("0\r\n", 0, 3)
  sz("ff\r\n", 255, 4)
  sz("FF\r\n", 255, 4)
  sz("1a2b\r\n", 6699, 6)
  sz("00000005\r\n", 5, 10)          # leading zeros are legal
  sz("5;ext=1\r\n", 5, 9)            # extensions are skipped
  sz("5;a;b=c\r\n", 5, 9)
  sz("5\n", 5, 2)                    # bare LF tolerated, as elsewhere

proc testRejectChunkSize =
  proc bad(s: string): bool =
    var got = -1
    result = parseChunkSize(toOpenArray(s, 0, s.len - 1), 0, got) == ParseBad

  assert bad("\r\n"), "no digits"
  assert bad("+5\r\n"), "no sign"
  assert bad("-5\r\n"), "no negative"
  assert bad("0x5\r\n"), "no 0x prefix"
  assert bad("5 \r\n"), "no trailing junk"
  assert bad("5x\r\n"), "no trailing junk"
  assert bad("g\r\n"), "not hex"
  # A size that does not fit an int must be refused, not wrapped: a wrapped
  # length is a body boundary in the wrong place.
  assert bad("FFFFFFFFFFFFFFFFF\r\n"), "overflow"
  var many = ""
  for i in 0..<40: many.add '0'
  many.add "5\r\n"
  assert bad(many), "absurdly long size line"

proc testIncompleteChunkSize =
  proc inc0(s: string): bool =
    var got = -1
    result = parseChunkSize(toOpenArray(s, 0, s.len - 1), 0, got) == ParseIncomplete
  assert inc0("5")
  assert inc0("5\r")
  assert inc0("5;ext")

  # Every proper prefix of a chunk-size line must say "not yet", never "bad".
  # The empty prefix is the one that matters most: it is the state a reader is
  # in before the first byte of a chunked body has arrived, and answering
  # `ParseBad` there fails the body before it starts.
  let line = "1a;ext=v\r\n"
  for n in 0..<line.len:
    var got = -1
    let r = parseChunkSize(toOpenArray(line, 0, n - 1), 0, got)
    assert r == ParseIncomplete, "prefix of length " & $n & " gave " & $r

proc testTrailers =
  proc te(s: string; expect: int) =
    let n = parseTrailerEnd(toOpenArray(s, 0, s.len - 1), 0)
    assert n == expect, s & ": " & $n & " wanted " & $expect
  te("\r\n", 2)                                    # no trailers
  te("X-A: 1\r\n\r\n", 10)                          # one trailer
  te("X-A: 1\r\nX-B: 2\r\n\r\n", 18)                # two
  assert parseTrailerEnd(toOpenArray("X-A: 1\r\n", 0, 7), 0) == ParseIncomplete
  var many = ""
  for i in 0..<20: many.add "X-" & $i & ": v\r\n"
  many.add "\r\n"
  assert parseTrailerEnd(toOpenArray(many, 0, many.len - 1), 0) == ParseBad,
         "trailer count is capped"

var wb = default(array[64, char])

proc rendered(n: int): string =
  result = ""
  for k in 0..<n: result.add wb[k]

proc testWriteChunks =
  var j = writeChunkHeader(wb, 0, 5)
  assert rendered(j) == "5\r\n", rendered(j)
  j = writeChunkHeader(wb, 0, 255)
  assert rendered(j) == "ff\r\n", rendered(j)
  j = writeChunkHeader(wb, 0, 0)
  assert rendered(j) == "0\r\n"
  j = writeLastChunk(wb, 0)
  assert rendered(j) == "0\r\n\r\n", rendered(j)
  assert chunkOverhead(5) == 5      # "5" + CRLF + CRLF
  assert chunkOverhead(255) == 6    # "ff" + CRLF + CRLF

  # Every buffer shorter than needed must refuse cleanly.
  for n in 0..<5:
    assert writeLastChunk(toOpenArray(wb, 0, n - 1), 0) == WriteFull

proc testRoundTripAChunkedBody =
  # Build a chunked body the way the writer does, then walk it the way the
  # reader does, and check the pieces come back.
  var enc = default(array[256, char])
  var j = 0
  let pieces = ["Hello", ", ", "world!"]
  for p in pieces:
    j = writeChunkHeader(enc, j, p.len)
    assert j > 0
    for k in 0..<p.len: enc[j + k] = p[k]
    j += p.len
    j = writeChunkEnd(enc, j)
    assert j > 0
  j = writeLastChunk(enc, j)
  assert j > 0

  var wire = ""
  for k in 0..<j: wire.add enc[k]
  assert wire == "5\r\nHello\r\n2\r\n, \r\n6\r\nworld!\r\n0\r\n\r\n", wire

  # Read it back.
  var i = 0
  var body = ""
  while true:
    var size = 0
    let after = parseChunkSize(toOpenArray(wire, 0, wire.len - 1), i, size)
    assert after > 0, "chunk size at " & $i
    i = after
    if size == 0: break
    for k in 0..<size: body.add wire[i + k]
    i += size
    let e = parseCrLf(toOpenArray(wire, 0, wire.len - 1), i)
    assert e > 0, "chunk not closed"
    i = e
  let done = parseTrailerEnd(toOpenArray(wire, 0, wire.len - 1), i)
  assert done == wire.len, $done & " vs " & $wire.len
  assert body == "Hello, world!", body

proc testFramingRejections =
  # The head-level half of the same problem: exactly one thing may say where
  # the body ends.
  proc bad(res: string): bool =
    var m = initHttpMsg()
    result = parseResponseHead(toOpenArray(res, 0, res.len - 1), 0, m) == ParseBad

  assert bad("HTTP/1.1 200 OK\r\nContent-Length: 5\r\n" &
             "Transfer-Encoding: chunked\r\n\r\n")
  assert bad("HTTP/1.1 200 OK\r\nContent-Length: 5\r\nContent-Length: 5\r\n\r\n")

  # …and a message framed by exactly one of them is fine.
  var m = initHttpMsg()
  let ok = "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
  assert parseResponseHead(toOpenArray(ok, 0, ok.len - 1), 0, m) == ok.len
  assert m.getTag(hTransferEncoding) == tag(vChunked)

testParseChunkSize()
testRejectChunkSize()
testIncompleteChunkSize()
testTrailers()
testWriteChunks()
testRoundTripAChunkedBody()
testFramingRejections()
echo "chunked: ok"
