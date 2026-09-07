# lib/std/http/httpmsg — the message layer of doc/internals/http.md §1.

import std / [http/httpmsg, assertions, syncio]

let tags = newHttpTags()   # this test's own tag space
let hApiKey = registerHeader(tags, "x-api-key")   # the header this test indexes on

# --- init ------------------------------------------------------------------

assert hApiKey.uint32 != 0'u32
assert registerHeader(tags, "x-api-key") == hApiKey, "registration is idempotent"
assert not isKnownHeader(hApiKey), "app headers sit past the built-in range"

let poolSizeAtInit = tags.pool.tags.len

proc testLookupNeverGrows =
  # The parser's only lookup, and the reason the vocabulary stays fixed:
  # `lookupHeader` answers from a byte compare and cannot intern, so bytes a
  # peer sent never reach the pool.
  assert lookupHeader(tags, "host") == tag(hHost)
  assert lookupHeader(tags, "HOST") == tag(hHost), "ASCII case is folded"
  assert lookupHeader(tags, "Content-Length") == tag(hContentLength)
  assert lookupHeader(tags, "x-api-key") == hApiKey
  assert lookupHeader(tags, "X-API-Key") == hApiKey
  for i in 0..<200:
    assert lookupHeader(tags, "x-attacker-" & $i).uint32 == 0'u32
  assert lookupHeader(tags, "").uint32 == 0'u32
  # 64 is the cap; one over must be refused rather than looked up.
  var long = ""
  for i in 0..<65: long.add 'a'
  assert lookupHeader(tags, long).uint32 == 0'u32
  assert tags.pool.tags.len == poolSizeAtInit, "the pool did not grow"

proc testMethods =
  assert lookupMethod(tags, "GET") == tag(mGet)
  assert lookupMethod(tags, "POST") == tag(mPost)
  assert lookupMethod(tags, "get").uint32 == 0'u32, "methods are case-sensitive"
  assert lookupMethod(tags, "BREW").uint32 == 0'u32
  assert lookupMethod(tags, "host").uint32 == 0'u32, "a header is not a method"
  assert isMethod(tag(mDelete))
  assert not isMethod(tag(hHost))
  assert isKnownHeader(tag(hHost))
  assert not isKnownHeader(tag(mGet))

proc testRequest =
  var m = initHttpMsg(tags)
  m.startRequest(tag(mPost), "/submit")
  m.addHeader(hHost, "example.com")
  m.addHeader(hContentLength, 42)
  m.addHeader(hContentType, "application/json")
  m.addHeader(hConnection, vKeepAlive)
  m.addHeader(hApiKey, "abc123")
  m.addOtherHeader("X-Weird", "1")
  m.addOtherHeader("X-Other", "2")
  m.finish()

  assert m.isRequest and not m.isResponse
  assert m.methodOf == tag(mPost)
  assert name(tags, m.methodOf) == "POST"
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
  assert hApiKey in m
  assert m.getStr(hApiKey) == "abc123"

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
  var m = initHttpMsg(tags)
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
  var a = initHttpMsg(tags)
  a.startRequest(tag(mGet), "/", tag(tV11))
  a.finish()
  assert a.isKeepAlive, "HTTP/1.1 defaults to keep-alive"

  var b = initHttpMsg(tags)
  b.startRequest(tag(mGet), "/", tag(tV10))
  b.finish()
  assert not b.isKeepAlive, "HTTP/1.0 does not"

proc testUpgradeIsOneTag =
  # `upgrade` names a header and one of Connection's values; a tag pool maps
  # spellings to ids, so it is one tag used in two positions.
  assert tag(vUpgrade) == tag(hUpgrade)
  var m = initHttpMsg(tags)
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
  var m = initHttpMsg(tags)
  for round in 0..<50:
    m.reset()
    m.startRequest(tag(mGet), "/page/" & $round)
    m.addHeader(hHost, "host-" & $round & ".example.com")
    m.addHeader(hContentLength, round)
    m.finish()
    assert m.target == "/page/" & $round
    assert m.getStr(hHost) == "host-" & $round & ".example.com"
    assert m.contentLength == round
  assert tags.pool.tags.len == poolSizeAtInit, "recycling never touches tags"

proc testMoveOnly =
  var m = initHttpMsg(tags)
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

proc testSpacesAreIndependent =
  # The point of threading the tag space rather than keeping one per process:
  # two spaces are two vocabularies, and an id only means anything against the
  # one it came from.
  let a = newHttpTags()
  let b = newHttpTags()
  let inA = registerHeader(a, "x-only-in-a")
  let inB = registerHeader(b, "x-only-in-b")
  assert inA.uint32 != 0'u32 and inB.uint32 != 0'u32
  # Both took the first free slot, so the ids collide by number...
  assert inA == inB, "independent spaces number their first app header alike"
  # ...and mean different things, which is exactly why the space travels with
  # the message instead of being looked up.
  assert name(a, inA) == "x-only-in-a"
  assert name(b, inB) == "x-only-in-b"
  assert lookupHeader(a, "x-only-in-b").uint32 == 0'u32, "no leak b -> a"
  assert lookupHeader(b, "x-only-in-a").uint32 == 0'u32, "no leak a -> b"
  # The built-in vocabulary is seeded per space, so it is shared by value.
  assert lookupHeader(a, "host") == tag(hHost)
  assert lookupHeader(b, "host") == tag(hHost)
  # A message reports spellings from the space it was built against.
  var m = initHttpMsg(a)
  m.startRequest(tag(mGet), "/")
  m.addHeader(inA, "v")
  m.finish()
  assert m.getStr(inA) == "v"
  for h in m.headers:
    if h == inA: assert name(m.tags, h) == "x-only-in-a"
  # This space registered nothing, so `tags` above is untouched by either.
  assert tags.pool.tags.len == poolSizeAtInit

testLookupNeverGrows()
testMethods()
testRequest()
testResponse()
testKeepAliveDefaults()
testUpgradeIsOneTag()
testRecycle()
testMoveOnly()
testSpacesAreIndependent()
echo "httpmsg: ok"
