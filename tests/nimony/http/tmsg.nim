# lib/std/http/httpmsg — the message layer of doc/internals/http.md §1.

import std / [http/httpmsg, assertions, syncio]
import httptags

# --- init: `httptags` registered what we index on --------------------------

assert hTrace.uint32 != 0'u32
assert registerHeader("x-trace-id") == hTrace, "registration is idempotent"
assert not isKnownHeader(hTrace), "app headers sit past the built-in range"

let poolSizeAtInit = httpTags().tags.len

proc testLookupNeverGrows =
  # The parser's only lookup, and the reason the vocabulary stays fixed:
  # `lookupHeader` answers from a byte compare and cannot intern, so bytes a
  # peer sent never reach the pool.
  assert lookupHeader("host") == tag(hHost)
  assert lookupHeader("HOST") == tag(hHost), "ASCII case is folded"
  assert lookupHeader("Content-Length") == tag(hContentLength)
  assert lookupHeader("x-trace-id") == hTrace
  assert lookupHeader("X-Trace-ID") == hTrace
  for i in 0..<200:
    assert lookupHeader("x-attacker-" & $i).uint32 == 0'u32
  assert lookupHeader("").uint32 == 0'u32
  # 64 is the cap; one over must be refused rather than looked up.
  var long = ""
  for i in 0..<65: long.add 'a'
  assert lookupHeader(long).uint32 == 0'u32
  assert httpTags().tags.len == poolSizeAtInit, "the pool did not grow"

proc testMethods =
  assert lookupMethod("GET") == tag(mGet)
  assert lookupMethod("POST") == tag(mPost)
  assert lookupMethod("get").uint32 == 0'u32, "methods are case-sensitive"
  assert lookupMethod("BREW").uint32 == 0'u32
  assert lookupMethod("host").uint32 == 0'u32, "a header is not a method"
  assert isMethod(tag(mDelete))
  assert not isMethod(tag(hHost))
  assert isKnownHeader(tag(hHost))
  assert not isKnownHeader(tag(mGet))

proc testRequest =
  var m = initHttpMsg()
  m.startRequest(tag(mPost), "/submit")
  m.addHeader(hHost, "example.com")
  m.addHeader(hContentLength, 42)
  m.addHeader(hContentType, "application/json")
  m.addHeader(hConnection, vKeepAlive)
  m.addHeader(hTrace, "abc123")
  m.addOtherHeader("X-Weird", "1")
  m.addOtherHeader("X-Other", "2")
  m.finish()

  assert m.isRequest and not m.isResponse
  assert m.methodOf == tag(mPost)
  assert name(m.methodOf) == "POST"
  assert m.target == "/submit"
  assert m.versionOf == tag(tV11)
  assert m.statusOf == 0, "a request has no status"

  # Typed payloads: no re-parsing on access.
  assert m.contentLength == 42
  assert m.getInt(hContentLength) == 42
  assert m.getStr(hHost) == "example.com"
  assert m.getStr(hContentType) == "application/json"

  # A value drawn from the known vocabulary is a tag, so the check is an
  # integer compare.
  assert m.getTag(hConnection) == tag(vKeepAlive)
  assert m.isKeepAlive
  assert m.getStr(hConnection) == "keep-alive", "a tag value still prints"

  # An app-registered header behaves exactly like a built-in one.
  assert hTrace in m
  assert m.getStr(hTrace) == "abc123"

  assert hHost in m
  assert hDate notin m
  assert m.getStr(hDate) == "", "absent reads empty"
  assert m.getInt(hDate) == -1
  assert m.getInt(hHost) == -1, "a string value is not an int"
  assert m.getTag(hHost).uint32 == 0'u32

  var n = 0
  for h in m.headers: inc n
  assert n == 7

  var others = ""
  for k, v in m.otherHeaders: others.add k & "=" & v & ";"
  assert others == "X-Weird=1;X-Other=2;", others

proc testResponse =
  var m = initHttpMsg()
  m.startResponse(404)
  m.addHeader(hContentLength, 0)
  m.addHeader(hConnection, vClose)
  m.finish()
  assert m.isResponse and not m.isRequest
  assert m.statusOf == 404
  assert m.methodOf.uint32 == 0'u32
  assert m.target == ""
  assert m.contentLength == 0
  assert not m.isKeepAlive, "Connection: close overrides the 1.1 default"

proc testKeepAliveDefaults =
  var a = initHttpMsg()
  a.startRequest(tag(mGet), "/", tag(tV11))
  a.finish()
  assert a.isKeepAlive, "HTTP/1.1 defaults to keep-alive"

  var b = initHttpMsg()
  b.startRequest(tag(mGet), "/", tag(tV10))
  b.finish()
  assert not b.isKeepAlive, "HTTP/1.0 does not"

proc testUpgradeIsOneTag =
  # `upgrade` names a header and one of Connection's values; a tag pool maps
  # spellings to ids, so it is one tag used in two positions.
  assert tag(vUpgrade) == tag(hUpgrade)
  var m = initHttpMsg()
  m.startRequest(tag(mGet), "/ws")
  m.addHeader(hConnection, vUpgrade)
  m.addHeader(hUpgrade, "websocket")
  m.finish()
  assert m.getTag(hConnection) == tag(vUpgrade)
  assert m.getStr(hUpgrade) == "websocket"
  assert not m.isKeepAlive

proc testRecycle =
  # `reset` keeps both allocations. Build, wipe, rebuild — many rounds, so a
  # pool that only survived one round would show up here.
  var m = initHttpMsg()
  for round in 0..<50:
    m.reset()
    m.startRequest(tag(mGet), "/page/" & $round)
    m.addHeader(hHost, "host-" & $round & ".example.com")
    m.addHeader(hContentLength, round)
    m.finish()
    assert m.target == "/page/" & $round
    assert m.getStr(hHost) == "host-" & $round & ".example.com"
    assert m.contentLength == round
  assert httpTags().tags.len == poolSizeAtInit, "recycling never touches tags"

proc testMoveOnly =
  var m = initHttpMsg()
  m.startRequest(tag(mGet), "/")
  m.finish()
  assert m.hasBuf
  var taken = move(m)
  assert taken.hasBuf
  assert not m.hasBuf, "a moved-from message no longer owns a buffer"
  assert taken.target == "/"
  # …and it can be donated back, which is what the event loop does.
  m = move(taken)
  assert m.hasBuf and not taken.hasBuf
  assert m.target == "/"

testLookupNeverGrows()
testMethods()
testRequest()
testResponse()
testKeepAliveDefaults()
testUpgradeIsOneTag()
testRecycle()
testMoveOnly()
echo "httpmsg: ok"
