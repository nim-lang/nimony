# (c) 2026 Andreas Rumpf
#
# HTTP messages as NIF trees. See doc/internals/http.md §1.
#
# A request and a response are the same type: a `TokenBuf` holding one node.
#
#   (req (GET) "/index.html" (v11)
#     (host          "example.com")
#     (content-length 42)
#     (connection    (keep-alive))
#     (xhdr          "X-Weird" "1"))
#
# The vocabulary is split in two, and the split is the whole point:
#
# * The **tag space** is one process-global `TagPool`, filled during init and
#   sealed before the first connection. Everything below plus whatever headers
#   the application registers lives there, so a header name is a `TagId` and
#   comparing one is an integer compare — no lowercasing, no hashing, no
#   case-insensitive string compare on the hot path.
# * The **payload space** is a `Pool` per message that dies with it, so bytes
#   a peer sent can never accumulate.
#
# Nothing here does IO; that is `httpconn`'s job. This module is meant to be
# testable on its own.

import ../../../src/lib/nifcore
import std / [assertions, syncio]

export nifcore.TagId, nifcore.`==`

type
  HttpTag* = enum
    ## The built-in vocabulary, in tag-pool order: the id of `e` is
    ## `ord(e) + 1` (BiTable ids start at 1). Groups are contiguous so
    ## membership is a range test — see `isMethod` / `isKnownHeader`.
    # -- structure --------------------------------------------------------
    tReq            ## request node
    tRes            ## response node
    tXhdr           ## a header nobody registered: (xhdr "name" "value")
    # -- versions ---------------------------------------------------------
    tV10
    tV11
    tV20
    # -- methods ----------------------------------------------------------
    mGet
    mHead
    mPost
    mPut
    mDelete
    mConnect
    mOptions
    mTrace
    mPatch
    # -- header names -----------------------------------------------------
    hAccept
    hAcceptCharset
    hAcceptEncoding
    hAcceptLanguage
    hAcceptRanges
    hAge
    hAllow
    hAuthorization
    hCacheControl
    hConnection
    hContentDisposition
    hContentEncoding
    hContentLanguage
    hContentLength
    hContentLocation
    hContentRange
    hContentType
    hCookie
    hDate
    hEtag
    hExpect
    hExpires
    hForwarded
    hFrom
    hHost
    hIfMatch
    hIfModifiedSince
    hIfNoneMatch
    hIfRange
    hIfUnmodifiedSince
    hLastModified
    hLocation
    hMaxForwards
    hOrigin
    hPragma
    hProxyAuthenticate
    hProxyAuthorization
    hRange
    hReferer
    hRetryAfter
    hServer
    hSetCookie
    hTe
    hTrailer
    hTransferEncoding
    hUpgrade
    hUserAgent
    hVary
    hVia
    hWarning
    hWwwAuthenticate
    hXForwardedFor
    hXForwardedProto
    hXRequestId
    # -- header values worth a tag ----------------------------------------
    vKeepAlive
    vClose
    vChunked
    vGzip
    vDeflate
    vIdentity
    vTrailers
    vNoCache
    vNoStore

const
  vUpgrade* = hUpgrade
    ## `Connection: upgrade`. The same spelling names a header and one of
    ## `Connection`'s values, and a tag pool maps spellings to ids — so they
    ## are one tag, and the position in the tree says which role it is in.

  MethodLow* = mGet
  MethodHigh* = mPatch
  HeaderLow* = hAccept
  HeaderHigh* = hXRequestId
  ValueLow* = vKeepAlive
  ValueHigh* = vNoStore

const TagNames: array[HttpTag, string] = [
  "req", "res", "xhdr",
  "v10", "v11", "v20",
  "GET", "HEAD", "POST", "PUT", "DELETE", "CONNECT", "OPTIONS", "TRACE", "PATCH",
  "accept", "accept-charset", "accept-encoding", "accept-language",
  "accept-ranges", "age", "allow", "authorization", "cache-control",
  "connection", "content-disposition", "content-encoding", "content-language",
  "content-length", "content-location", "content-range", "content-type",
  "cookie", "date", "etag", "expect", "expires", "forwarded", "from", "host",
  "if-match", "if-modified-since", "if-none-match", "if-range",
  "if-unmodified-since", "last-modified", "location", "max-forwards", "origin",
  "pragma", "proxy-authenticate", "proxy-authorization", "range", "referer",
  "retry-after", "server", "set-cookie", "te", "trailer", "transfer-encoding",
  "upgrade", "user-agent", "vary", "via", "warning", "www-authenticate",
  "x-forwarded-for", "x-forwarded-proto", "x-request-id",
  "keep-alive", "close", "chunked", "gzip", "deflate", "identity",
  "trailers", "no-cache", "no-store"]
  ## Wire spellings. Header names are canonical lowercase (what HTTP/2 requires
  ## and what HTTP/1.1 matching folds to); methods keep their uppercase form,
  ## which is case-sensitive on the wire.

# ---------------------------------------------------------------- tag pool --

var gHttpTags: TagPool = nil
  ## Process-global and, after `sealHttpTags`, immutable. Deliberately has NO
  ## `escapeTag`: with one, ids past 511 stay legal at the cost of a second
  ## token and there is no wall to detect, so any cap would be a number we
  ## invented. Without one, 511 is structural and "the pool is full" is a real
  ## condition that `registerHeader` reports.

proc initHttpTags() =
  gHttpTags = newTagPool()
  for e in HttpTag.low..HttpTag.high:
    let id = gHttpTags.registerTag(TagNames[e])
    assert id.uint32 == e.uint32 + 1'u32,
      "httpmsg: tag/enum misalignment at " & TagNames[e]

initHttpTags()

proc httpTags*(): TagPool {.inline.} =
  ## The shared tag pool. Messages are built against it; nothing else should
  ## need it.
  gHttpTags

template tag*(e: HttpTag): TagId =
  ## The pool id of a built-in tag. Ids are `ord + 1` by construction, so this
  ## is arithmetic, not a lookup.
  TagId(e.uint32 + 1'u32)

proc isMethod*(t: TagId): bool {.inline.} =
  t.uint32 >= tag(MethodLow).uint32 and t.uint32 <= tag(MethodHigh).uint32

proc isKnownHeader*(t: TagId): bool {.inline.} =
  t.uint32 >= tag(HeaderLow).uint32 and t.uint32 <= tag(HeaderHigh).uint32

proc name*(t: TagId): string =
  ## The wire spelling of a registered tag; `""` for an id nobody registered.
  if t.uint32 == 0'u32 or t.uint32 > gHttpTags.tags.len.uint32: ""
  else: gHttpTags.tagName(t)

const MaxHttpTags* = 511
  ## One-token tag ids. See `gHttpTags` on why we stop here rather than
  ## spending a second token.

proc registerHeader*(name: string): TagId =
  ## Register a header the application indexes on, so it gets the same
  ## integer-compare treatment as `hHost`. Call during init; the pool is
  ## sealed before serving and this is a defect afterwards.
  ##
  ## Returns `TagId(0)` when the tag space is full — a startup error, and the
  ## only reason this can fail.
  assert name.len > 0, "registerHeader: empty name"
  let existing = gHttpTags.tagId(name)
  if existing.uint32 != 0'u32: return existing
  if gHttpTags.tags.len >= MaxHttpTags: return TagId(0)
  result = gHttpTags.registerTag(name)

proc sealHttpTags*() =
  ## Close the vocabulary. Call once, after every `registerHeader`, before the
  ## first connection is accepted. From here on the only way to name a header
  ## is `lookupHeader`, which cannot grow the pool.
  gHttpTags.seal()

proc httpTagsSealed*(): bool {.inline.} = gHttpTags.sealed

const
  MaxTagNameLen* = 64
    ## Longest spelling `lookupHeader` will even consider. Every built-in tag
    ## is far shorter, so this only ever rejects something that was going to
    ## miss anyway — cheaply, and without touching the pool.

type
  FoldBuf = array[MaxTagNameLen, char]

proc foldAscii(dest: var FoldBuf; name: openArray[char]): bool {.inline.} =
  ## Lowercase `name` into `dest`. False when it cannot fit, which is the
  ## caller's cue to stop. No allocation: HTTP asks this question once per
  ## header of every request, so it may not touch the heap.
  if name.len == 0 or name.len > MaxTagNameLen: return false
  for i in 0..<name.len:
    let c = name[i]
    dest[i] = if c >= 'A' and c <= 'Z': chr(ord(c) + 32) else: c
  result = true

# The vocabulary is sealed and smaller than 512, so looking a spelling up
# does not need the pool's hash table — and must not, because `getKeyId`
# takes a `string` and building one per header per request is exactly the
# allocation this layer exists to avoid. Bucketing the tags by name length
# leaves a handful of candidates per bucket, which a byte compare settles.

var
  gByLen: seq[TagId] = @[]
  gLenStart: array[MaxTagNameLen + 2, int32]
  gIndexedUpTo = 0

proc buildLookupIndex() =
  let n = gHttpTags.tags.len
  var counts = default(array[MaxTagNameLen + 2, int32])
  for i in 0..<counts.len: counts[i] = 0'i32
  for id in 1..n:
    let L = gHttpTags.tagName(TagId(id.uint32)).len
    if L <= MaxTagNameLen: inc counts[L]
  var acc = 0'i32
  for L in 0..MaxTagNameLen:
    gLenStart[L] = acc
    acc = acc + counts[L]
  gLenStart[MaxTagNameLen + 1] = acc
  gByLen = newSeq[TagId](acc.int)
  var cursor = default(array[MaxTagNameLen + 2, int32])
  for i in 0..<cursor.len: cursor[i] = gLenStart[i]
  for id in 1..n:
    let t = TagId(id.uint32)
    let L = gHttpTags.tagName(t).len
    if L <= MaxTagNameLen:
      gByLen[cursor[L].int] = t
      inc cursor[L]
  gIndexedUpTo = n

proc sameBytes(t: TagId; folded: FoldBuf; n: int): bool {.inline.} =
  let spelling = gHttpTags.tagName(t)
  if spelling.len != n: return false
  for i in 0..<n:
    if spelling[i] != folded[i]: return false
  result = true

proc lookupTag(name: openArray[char]; fold: bool): TagId =
  var buf = default(FoldBuf)
  if name.len == 0 or name.len > MaxTagNameLen: return TagId(0)
  if fold:
    if not foldAscii(buf, name): return TagId(0)
  else:
    for i in 0..<name.len: buf[i] = name[i]
  if gIndexedUpTo != gHttpTags.tags.len: buildLookupIndex()
  let n = name.len
  var k = gLenStart[n]
  while k < gLenStart[n + 1]:
    let t = gByLen[k.int]
    if sameBytes(t, buf, n): return t
    inc k
  result = TagId(0)

proc lookupHeader*(name: openArray[char]): TagId =
  ## Wire bytes to a `TagId`, folding ASCII case as HTTP requires. Answers
  ## `TagId(0)` for a name nobody registered — those become `(xhdr …)`.
  ##
  ## **This never interns**, and it never allocates. It is the one lookup the
  ## parser is allowed to perform on attacker-controlled bytes.
  lookupTag(name, fold = true)

proc lookupValue*(name: openArray[char]): TagId =
  ## Like `lookupHeader`, but answers only for spellings in the header-*value*
  ## range — so `Connection: host` does not come back as the `host` tag.
  let t = lookupTag(name, fold = true)
  result = if t.uint32 >= tag(ValueLow).uint32 and
              t.uint32 <= tag(ValueHigh).uint32: t else: TagId(0)

proc lookupMethod*(name: openArray[char]): TagId =
  ## Wire bytes to a method tag, case-sensitively as HTTP requires. Answers
  ## `TagId(0)` for anything that is not one of the nine built-in methods.
  let t = lookupTag(name, fold = false)
  result = if t.isMethod: t else: TagId(0)

# ----------------------------------------------------------------- message --

type
  HttpMsg* = object
    ## A request or a response. Move-only: `TokenBuf` is `=copy {.error.}`, so
    ## this inherits that and a message has exactly one owner at a time.
    buf: TokenBuf
    live: bool

proc `=wasMoved`*(m: var HttpMsg) {.nodestroy, inline.} =
  ## `.nodestroy`: the buffer has been handed to the destination, so clearing
  ## this one must not run its destructor. Without this hook `live` would
  ## survive the move and a moved-from message would claim to own a buffer it
  ## no longer has — which is exactly the question the event loop asks when it
  ## decides whether to reclaim `e.msg`.
  `=wasMoved`(m.buf)
  m.live = false

proc hasBuf*(m: HttpMsg): bool {.inline.} =
  ## Whether this message owns a buffer. A moved-from message does not, which
  ## is how the event loop tells "the handler took it" from "it is still mine".
  m.live

proc initHttpMsg*(cap = 64): HttpMsg =
  ## A message with its own literal pool, sharing the global tag space.
  HttpMsg(buf: createTokenBuf(cap, sharedTags = gHttpTags), live: true)

proc reset*(m: var HttpMsg) =
  ## Drop the content, keep both allocations — the token buffer's and the
  ## literal pool's. This is what makes recycling a message cheaper than
  ## building a new one.
  if m.live:
    m.buf.shrink 0
    m.buf.pool.clear()

# -------------------------------------------------------------- building ---

proc addEmpty(b: var TokenBuf; t: TagId) {.inline.} =
  b.openTag t
  b.closeTag()

proc startRequest*(m: var HttpMsg; meth: TagId; target: string; v = tag(tV11)) =
  ## Begin `(req (METHOD) target (version) …)`. Headers follow; call
  ## `finish` when done.
  assert m.live, "startRequest on a moved-from message"
  assert m.buf.len == 0, "startRequest on a message already built; reset first"
  assert meth.isMethod, "startRequest: not a method tag"
  m.buf.openTag tag(tReq)
  m.buf.addEmpty meth
  m.buf.addStrLit target
  m.buf.addEmpty v

proc startResponse*(m: var HttpMsg; status: int; v = tag(tV11)) =
  ## Begin `(res status (version) …)`.
  assert m.live, "startResponse on a moved-from message"
  assert m.buf.len == 0, "startResponse on a message already built; reset first"
  assert status >= 100 and status <= 599, "startResponse: implausible status"
  m.buf.openTag tag(tRes)
  m.buf.addIntLit status
  m.buf.addEmpty v

proc addHeader*(m: var HttpMsg; h: TagId; value: string) =
  ## A header whose value is a string.
  assert h.uint32 != 0'u32, "addHeader: unregistered tag; use addOtherHeader"
  m.buf.buildTree h:
    m.buf.addStrLit value

proc addHeader*(m: var HttpMsg; h: TagId; value: openArray[char]) =
  ## As above, from a byte view — what a parser has, without cutting a slice
  ## out of its read buffer to get it.
  assert h.uint32 != 0'u32, "addHeader: unregistered tag; use addOtherHeader"
  m.buf.buildTree h:
    m.buf.addStrLit value

proc addHeader*(m: var HttpMsg; h: TagId; value: int) =
  ## A header whose value is numeric — `Content-Length` and friends. Stored as
  ## an `IntLit`, so it is parsed once here and never re-parsed on access.
  assert h.uint32 != 0'u32, "addHeader: unregistered tag"
  m.buf.buildTree h:
    m.buf.addIntLit value

proc addHeader*(m: var HttpMsg; h, value: TagId) =
  ## A header whose value is itself drawn from a known vocabulary, e.g.
  ## `(connection (keep-alive))`. Checking one is then an integer compare.
  assert h.uint32 != 0'u32 and value.uint32 != 0'u32
  m.buf.buildTree h:
    m.buf.addEmpty value

proc addHeader*(m: var HttpMsg; h: HttpTag; value: string) {.inline.} =
  addHeader(m, tag(h), value)
proc addHeader*(m: var HttpMsg; h: HttpTag; value: int) {.inline.} =
  addHeader(m, tag(h), value)
proc addHeader*(m: var HttpMsg; h, value: HttpTag) {.inline.} =
  addHeader(m, tag(h), tag(value))

proc addOtherHeader*(m: var HttpMsg; name, value: string) =
  ## A header nobody registered: `(xhdr "name" "value")`. The application
  ## provably does not index this by constant — if it did, it would have
  ## registered it — so this form is never on a hot path.
  m.buf.buildTree tag(tXhdr):
    m.buf.addStrLit name
    m.buf.addStrLit value

proc addOtherHeader*(m: var HttpMsg; name, value: openArray[char]) =
  ## As above, from byte views.
  m.buf.buildTree tag(tXhdr):
    m.buf.addStrLit name
    m.buf.addStrLit value

proc startRequest*(m: var HttpMsg; meth: TagId; target: openArray[char];
                   v = tag(tV11)) =
  ## As `startRequest`, with the target still in the parser's buffer.
  assert m.live, "startRequest on a moved-from message"
  assert m.buf.len == 0, "startRequest on a message already built; reset first"
  assert meth.isMethod, "startRequest: not a method tag"
  m.buf.openTag tag(tReq)
  m.buf.addEmpty meth
  m.buf.addStrLit target
  m.buf.addEmpty v

proc finish*(m: var HttpMsg) =
  ## Close the message node. Required before any accessor.
  m.buf.closeTag()

# --------------------------------------------------------------- reading ---

proc isRequest*(m: var HttpMsg): bool =
  m.live and m.buf.len > 0 and m.buf.beginRead().cursorTagId == tag(tReq)

proc isResponse*(m: var HttpMsg): bool =
  m.live and m.buf.len > 0 and m.buf.beginRead().cursorTagId == tag(tRes)

proc methodOf*(m: var HttpMsg): TagId =
  ## The request's method tag, or `TagId(0)` on a response.
  var c = m.buf.beginRead()
  if c.cursorTagId != tag(tReq): return TagId(0)
  var body = c.sub()
  result = body.cursorTagId

proc target*(m: var HttpMsg): string =
  ## The request target, or `""` on a response.
  var c = m.buf.beginRead()
  if c.cursorTagId != tag(tReq): return ""
  var body = c.sub()
  body.skip                      # past (METHOD)
  result = body.strVal

proc statusOf*(m: var HttpMsg): int =
  ## The response status, or `0` on a request.
  var c = m.buf.beginRead()
  if c.cursorTagId != tag(tRes): return 0
  var body = c.sub()
  result = int(body.intVal)

proc versionOf*(m: var HttpMsg): TagId =
  var c = m.buf.beginRead()
  var body = c.sub()
  if c.cursorTagId == tag(tReq):
    body.skip                    # (METHOD)
    body.skip                    # target
  else:
    body.skip                    # status
  result = body.cursorTagId

proc headersStart(m: var HttpMsg): Cursor =
  ## A bounded cursor positioned at the first header child.
  var c = m.buf.beginRead()
  result = c.sub()
  if c.cursorTagId == tag(tReq):
    result.skip                  # (METHOD)
    result.skip                  # target
    result.skip                  # (version)
  else:
    result.skip                  # status
    result.skip                  # (version)

iterator headers*(m: var HttpMsg): TagId =
  ## Every header in wire order. `tag(tXhdr)` is yielded for the unregistered
  ## ones; ask `otherHeaders` for their names.
  ##
  ## The `cast` is because reading a `TokenBuf` goes through a `Cursor`, and a
  ## cursor holds a refcount on the buffer's owner — so walking a message is
  ## observably a mutation even though nothing about the message changes.
  {.cast(noSideEffect).}:
    if m.live and m.buf.len > 0:
      var c = headersStart(m)
      while c.hasMore:
        yield c.cursorTagId
        c.skip

iterator otherHeaders*(m: var HttpMsg): (string, string) =
  ## Name/value for the headers that have no tag. See `headers` on the cast.
  {.cast(noSideEffect).}:
    if m.live and m.buf.len > 0:
      var c = headersStart(m)
      while c.hasMore:
        if c.cursorTagId == tag(tXhdr):
          var kv = c.sub()
          let n = kv.strVal
          kv.inc
          yield (n, kv.strVal)
        c.skip

proc contains*(m: var HttpMsg; h: TagId): bool =
  ## Whether the message carries `h` at least once.
  if not m.live or m.buf.len == 0 or h.uint32 == 0'u32: return false
  var c = headersStart(m)
  while c.hasMore:
    if c.cursorTagId == h: return true
    c.skip
  result = false

proc contains*(m: var HttpMsg; h: HttpTag): bool {.inline.} = contains(m, tag(h))

proc getStr*(m: var HttpMsg; h: TagId): string =
  ## The first value of `h` as a string, or `""` when absent. A value stored
  ## as a tag (`(connection (keep-alive))`) reads back as its wire spelling.
  if not m.live or m.buf.len == 0 or h.uint32 == 0'u32: return ""
  var c = headersStart(m)
  while c.hasMore:
    if c.cursorTagId == h:
      var v = c.sub()
      if not v.hasMore: return ""
      case v.kind
      of StrLit: return v.strVal
      of IntLit: return $v.intVal
      of TagLit: return name(v.cursorTagId)
      else: return ""
    c.skip
  result = ""

proc getStr*(m: var HttpMsg; h: HttpTag): string {.inline.} = getStr(m, tag(h))

proc getInt*(m: var HttpMsg; h: TagId; default = -1): int =
  ## The first value of `h` as an integer — no re-parsing, it was stored as
  ## one. `default` when the header is absent or is not numeric.
  if not m.live or m.buf.len == 0 or h.uint32 == 0'u32: return default
  var c = headersStart(m)
  while c.hasMore:
    if c.cursorTagId == h:
      var v = c.sub()
      if v.hasMore and v.kind == IntLit: return int(v.intVal)
      return default
    c.skip
  result = default

proc getInt*(m: var HttpMsg; h: HttpTag; default = -1): int {.inline.} =
  getInt(m, tag(h), default)

proc getTag*(m: var HttpMsg; h: TagId): TagId =
  ## The first value of `h` when it is itself a tag, else `TagId(0)`. This is
  ## the integer-compare path: `m.getTag(hConnection) == tag(vKeepAlive)`.
  if not m.live or m.buf.len == 0 or h.uint32 == 0'u32: return TagId(0)
  var c = headersStart(m)
  while c.hasMore:
    if c.cursorTagId == h:
      var v = c.sub()
      if v.hasMore and v.kind == TagLit: return v.cursorTagId
      return TagId(0)
    c.skip
  result = TagId(0)

proc getTag*(m: var HttpMsg; h: HttpTag): TagId {.inline.} = getTag(m, tag(h))

proc contentLength*(m: var HttpMsg): int {.inline.} =
  ## `-1` when absent. One token read; the digits were parsed at parse time.
  getInt(m, tag(hContentLength), -1)

proc isKeepAlive*(m: var HttpMsg): bool =
  ## The check every request pays for, as an integer compare rather than a
  ## case-insensitive string compare.
  let v = getTag(m, tag(hConnection))
  if v.uint32 != 0'u32:
    result = v == tag(vKeepAlive)
  else:
    # No Connection header: HTTP/1.1 is keep-alive by default, 1.0 is not.
    result = versionOf(m) == tag(tV11) or versionOf(m) == tag(tV20)
