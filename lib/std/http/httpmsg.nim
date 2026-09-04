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
#   never again: the parser's only lookup is `lookupHeader`, which cannot
#   intern. Everything below plus whatever headers the application registers
#   lives there, so a header name is a `TagId` and comparing one is an integer
#   compare — no lowercasing, no hashing, no case-insensitive string compare
#   on the hot path.
# * The **payload space** is a `Pool` per message that dies with it, so bytes
#   a peer sent can never accumulate.
#
# Nothing here does IO; that is `httpconn`'s job. This module is meant to be
# testable on its own.

import ../../../src/lib/nifcore
import std / [assertions, syncio]

export nifcore.TagId, nifcore.`==`, nifcore.Cursor

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

const
  MaxHttpTags* = 511
    ## One-token tag ids. See `HttpTags` on why we stop here rather than
    ## spending a second token.
  MaxTagNameLen* = 64
    ## Longest spelling `lookupHeader` will even consider. Every built-in tag
    ## is far shorter, so this only ever rejects something that was going to
    ## miss anyway — cheaply, and without touching the pool.

type
  HttpTags = object
    ## The tag space and the index `lookupHeader` searches, in one object
    ## because the second is a *function* of the first: an index that has not
    ## seen a registration answers `TagId(0)` for a name that is in the pool.
    ## Keeping them in one declaration is what makes "written by
    ## `registerHeader`, read by everyone else" a thing you can check by
    ## looking, rather than a protocol spread over three variables and a
    ## rebuild-if-stale test on the hot path.
    pool: TagPool
      ## Deliberately has NO `escapeTag`: with one, ids past 511 stay legal at
      ## the cost of a second token and there is no wall to detect, so any cap
      ## would be a number we invented. Without one, `MaxHttpTags` is
      ## structural and "the pool is full" is a real condition that
      ## `registerHeader` reports.
    byLen: seq[TagId]
      ## Every tag id, grouped by the length of its spelling. The vocabulary is
      ## under 512 entries, so a group is a handful of candidates that a byte
      ## compare settles — no hashing, and nothing that needs a `string` to ask
      ## with.
    lenStart: array[MaxTagNameLen + 2, int32]
      ## `byLen[lenStart[n] ..< lenStart[n+1]]` are the tags spelled with `n`
      ## bytes.

var gTags: HttpTags
  ## Process-global, because the tag space is: an id has to mean the same
  ## thing in every message the process handles, which is the whole reason a
  ## header name can be compared as an integer.

proc rebuildIndex(t: var HttpTags) =
  ## Group the pool's spellings by length. Called from the two places that
  ## grow the pool, both of which run during init, so the lookup path never
  ## has to wonder whether the index is current.
  let n = t.pool.tags.len
  var counts = default(array[MaxTagNameLen + 2, int32])
  for id in 1..n:
    let L = t.pool.tagName(TagId(id.uint32)).len
    if L <= MaxTagNameLen: inc counts[L]
  var acc = 0'i32
  for L in 0..MaxTagNameLen:
    t.lenStart[L] = acc
    acc = acc + counts[L]
  t.lenStart[MaxTagNameLen + 1] = acc
  t.byLen = newSeq[TagId](acc.int)
  var cursor = t.lenStart
  for id in 1..n:
    let tg = TagId(id.uint32)
    let L = t.pool.tagName(tg).len
    if L <= MaxTagNameLen:
      t.byLen[cursor[L].int] = tg
      inc cursor[L]

proc initHttpTags() =
  gTags.pool = newTagPool()
  for e in HttpTag.low..HttpTag.high:
    let id = gTags.pool.registerTag(TagNames[e])
    assert id.uint32 == e.uint32 + 1'u32,
      "httpmsg: tag/enum misalignment at " & TagNames[e]
  rebuildIndex(gTags)

initHttpTags()

proc httpTags*(): TagPool {.inline.} =
  ## The shared tag pool. Messages are built against it; nothing else should
  ## need it.
  gTags.pool

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
  if t.uint32 == 0'u32 or t.uint32 > gTags.pool.tags.len.uint32: ""
  else: gTags.pool.tagName(t)

template spelling*(t: TagId): lent string =
  ## The wire spelling, borrowed rather than copied — a template so it inlines
  ## to the table access. This is what a serializer wants; `name` copies.
  gTags.pool.tagName(t)

proc registerHeader*(name: string): TagId =
  ## Register a header the application indexes on, so it gets the same
  ## integer-compare treatment as `hHost`. Call during init, before the first
  ## connection: the pool is process-global and monotonic, so a name added
  ## later is one every message parsed so far reported as `(xhdr …)`.
  ##
  ## Returns `TagId(0)` when the tag space is full — a startup error, and the
  ## only reason this can fail.
  ##
  ## Idempotent: a name already registered comes back with the pool untouched,
  ## so several independent components (or several tests sharing one process)
  ## can each ask for the headers they need without having to agree on who
  ## goes first.
  assert name.len > 0, "registerHeader: empty name"
  let existing = gTags.pool.tags.getKeyId(name)
  if existing.uint32 != 0'u32: return existing
  if gTags.pool.tags.len >= MaxHttpTags: return TagId(0)
  result = gTags.pool.registerTag(name)
  rebuildIndex(gTags)

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

proc sameBytes(t: TagId; folded: FoldBuf; n: int): bool {.inline.} =
  let spelling = gTags.pool.tagName(t)
  if spelling.len != n: return false
  for i in 0..<n:
    if spelling[i] != folded[i]: return false
  result = true

proc lookupTag(name: openArray[char]; fold: bool): TagId =
  ## The pool's own hash table is not used and must not be: `getKeyId` takes a
  ## `string`, and building one per header of every request is exactly the
  ## allocation this layer exists to avoid. `gTags.byLen` answers from the
  ## caller's bytes.
  var buf = default(FoldBuf)
  if name.len == 0 or name.len > MaxTagNameLen: return TagId(0)
  if fold:
    if not foldAscii(buf, name): return TagId(0)
  else:
    for i in 0..<name.len: buf[i] = name[i]
  let n = name.len
  var k = gTags.lenStart[n]
  while k < gTags.lenStart[n + 1]:
    let t = gTags.byLen[k.int]
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
  HttpMsg(buf: createTokenBuf(cap, sharedTags = gTags.pool), live: true)

proc reset*(m: var HttpMsg) =
  ## Drop the content, keeping the token buffer's allocation — which is the
  ## one that grew to fit the traffic and the reason recycling a message beats
  ## building a new one. The literal pool is replaced rather than emptied: it
  ## holds only the values too long to sit inline in a token, so it is
  ## typically small or untouched, and a fresh `Pool` is one allocation
  ## against the certainty that no id from the old message stays reachable.
  if m.live:
    m.buf.shrink 0
    m.buf.pool = newPool()

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

proc addHeader*(m: var HttpMsg; h: TagId; values: openArray[string]) =
  ## A list-valued header as one node with several children — the shape a
  ## comma-separated header is meant to take, so that reading it back does not
  ## mean splitting a string again.
  assert h.uint32 != 0'u32, "addHeader: unregistered tag"
  m.buf.buildTree h:
    for v in values: m.buf.addStrLit v

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
proc addHeader*(m: var HttpMsg; h: HttpTag; values: openArray[string]) {.inline.} =
  addHeader(m, tag(h), values)

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
  ## Close the message node, and take the buffer's cursor owner while `m` is
  ## still mutable. Required before any accessor, and for two reasons now: the
  ## tree has to be closed to be walked, and every accessor below reads through
  ## `readonlyCursorAt`, which needs the buffer to be owned *already* — an
  ## ownerless cursor resolves `strVal` against the fallback pool rather than
  ## this message's own. `finish` is the one point in a message's life that is
  ## both mutable and guaranteed to run before any read.
  ##
  ## The owner survives until the next mutation, which `prepareMutation` uses
  ## to drop it again — so a recycled message re-takes it here, once.
  m.buf.closeTag()
  discard m.buf.beginRead()

# --------------------------------------------------------------- reading ---
#
# Reading a message is not a mutation of it, so every accessor below takes the
# message by value. `readonlyCursorAt` is what allows that: `beginRead` needs
# `var TokenBuf` because it takes a refcount on the buffer's owner, which would
# make asking a request for its `Host` header require a mutable reference to it
# — and force that `var` back up through every caller that only wants to look.
# The cursor is ownerless and valid while `m` is, which is the lifetime an
# accessor's result had anyway.

proc readCursor(m: HttpMsg): Cursor {.inline.} =
  ## A cursor at the message node. Only valid once `finish` has run, which is
  ## what `hasContent` checks for the accessors that can be asked earlier.
  readonlyCursorAt(m.buf, 0)

proc hasContent(m: HttpMsg): bool {.inline.} =
  ## Whether there is a tree to read at all: a moved-from message has no
  ## buffer, and a `reset` one has an empty buffer.
  m.live and m.buf.len > 0

proc isRequest*(m: HttpMsg): bool =
  m.hasContent and m.readCursor().cursorTagId == tag(tReq)

proc isResponse*(m: HttpMsg): bool =
  m.hasContent and m.readCursor().cursorTagId == tag(tRes)

proc methodOf*(m: HttpMsg): TagId =
  ## The request's method tag, or `TagId(0)` on a response.
  if not m.hasContent: return TagId(0)
  var c = m.readCursor()
  if c.cursorTagId != tag(tReq): return TagId(0)
  var body = c.sub()
  result = body.cursorTagId

proc target*(m: HttpMsg): string =
  ## The request target, or `""` on a response.
  if not m.hasContent: return ""
  var c = m.readCursor()
  if c.cursorTagId != tag(tReq): return ""
  var body = c.sub()
  body.skip                      # past (METHOD)
  result = body.strVal

proc statusOf*(m: HttpMsg): int =
  ## The response status, or `0` on a request.
  if not m.hasContent: return 0
  var c = m.readCursor()
  if c.cursorTagId != tag(tRes): return 0
  var body = c.sub()
  result = int(body.intVal)

proc versionOf*(m: HttpMsg): TagId =
  if not m.hasContent: return TagId(0)
  var c = m.readCursor()
  var body = c.sub()
  if c.cursorTagId == tag(tReq):
    body.skip                    # (METHOD)
    body.skip                    # target
  else:
    body.skip                    # status
  result = body.cursorTagId

proc rootCursor*(m: HttpMsg): Cursor =
  ## A cursor at the message node itself, for consumers that need the raw
  ## tree — a serializer reaching the target without copying it out, say.
  m.readCursor()

proc headersStart(m: HttpMsg): Cursor =
  ## A bounded cursor positioned at the first header child.
  var c = m.readCursor()
  result = c.sub()
  if c.cursorTagId == tag(tReq):
    result.skip                  # (METHOD)
    result.skip                  # target
    result.skip                  # (version)
  else:
    result.skip                  # status
    result.skip                  # (version)

iterator headers*(m: HttpMsg): TagId =
  ## Every header in wire order. `tag(tXhdr)` is yielded for the unregistered
  ## ones; ask `otherHeaders` for their names.
  ##
  ## The `cast` is the cursor refcount, not the message: an iterator is
  ## inferred `.noSideEffect` and taking a reference on the buffer's owner is
  ## a write. Nothing about `m` changes — see `readCursor`.
  {.cast(noSideEffect).}:
    if m.hasContent:
      var c = headersStart(m)
      while c.hasMore:
        yield c.cursorTagId
        c.skip

iterator headerNodes*(m: HttpMsg): Cursor =
  ## A cursor at each header node, so a serializer can reach the raw values
  ## without them being copied out first. `sub` descends into one. See
  ## `headers` on the cast.
  {.cast(noSideEffect).}:
    if m.hasContent:
      var c = headersStart(m)
      while c.hasMore:
        yield c
        c.skip

iterator otherHeaders*(m: HttpMsg): (string, string) =
  ## Name/value for the headers that have no tag. See `headers` on the cast.
  {.cast(noSideEffect).}:
    if m.hasContent:
      var c = headersStart(m)
      while c.hasMore:
        if c.cursorTagId == tag(tXhdr):
          var kv = c.sub()
          let n = kv.strVal
          kv.inc
          yield (n, kv.strVal)
        c.skip

proc contains*(m: HttpMsg; h: TagId): bool =
  ## Whether the message carries `h` at least once.
  if not m.hasContent or h.uint32 == 0'u32: return false
  var c = headersStart(m)
  while c.hasMore:
    if c.cursorTagId == h: return true
    c.skip
  result = false

proc contains*(m: HttpMsg; h: HttpTag): bool {.inline.} = contains(m, tag(h))

proc countHeader*(m: HttpMsg; h: TagId): int =
  ## How many times `h` appears. More than once matters for the headers that
  ## frame a message: two `Content-Length` lines are a request-smuggling
  ## vector, not a formatting quirk.
  result = 0
  if not m.hasContent or h.uint32 == 0'u32: return 0
  var c = headersStart(m)
  while c.hasMore:
    if c.cursorTagId == h: inc result
    c.skip

proc countHeader*(m: HttpMsg; h: HttpTag): int {.inline.} =
  countHeader(m, tag(h))

proc getStr*(m: HttpMsg; h: TagId): string =
  ## The first value of `h` as a string, or `""` when absent. A value stored
  ## as a tag (`(connection (keep-alive))`) reads back as its wire spelling.
  if not m.hasContent or h.uint32 == 0'u32: return ""
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

proc getStr*(m: HttpMsg; h: HttpTag): string {.inline.} = getStr(m, tag(h))

proc getInt*(m: HttpMsg; h: TagId; default = -1): int =
  ## The first value of `h` as an integer — no re-parsing, it was stored as
  ## one. `default` when the header is absent or is not numeric.
  if not m.hasContent or h.uint32 == 0'u32: return default
  var c = headersStart(m)
  while c.hasMore:
    if c.cursorTagId == h:
      var v = c.sub()
      if v.hasMore and v.kind == IntLit: return int(v.intVal)
      return default
    c.skip
  result = default

proc getInt*(m: HttpMsg; h: HttpTag; default = -1): int {.inline.} =
  getInt(m, tag(h), default)

proc getTag*(m: HttpMsg; h: TagId): TagId =
  ## The first value of `h` when it is itself a tag, else `TagId(0)`. This is
  ## the integer-compare path: `m.getTag(hConnection) == tag(vKeepAlive)`.
  if not m.hasContent or h.uint32 == 0'u32: return TagId(0)
  var c = headersStart(m)
  while c.hasMore:
    if c.cursorTagId == h:
      var v = c.sub()
      if v.hasMore and v.kind == TagLit: return v.cursorTagId
      return TagId(0)
    c.skip
  result = TagId(0)

proc getTag*(m: HttpMsg; h: HttpTag): TagId {.inline.} = getTag(m, tag(h))

proc contentLength*(m: HttpMsg): int {.inline.} =
  ## `-1` when absent. One token read; the digits were parsed at parse time.
  getInt(m, tag(hContentLength), -1)

proc isKeepAlive*(m: HttpMsg): bool =
  ## The check every request pays for, as an integer compare rather than a
  ## case-insensitive string compare.
  let v = getTag(m, tag(hConnection))
  if v.uint32 != 0'u32:
    result = v == tag(vKeepAlive)
  else:
    # No Connection header: HTTP/1.1 is keep-alive by default, 1.0 is not.
    result = versionOf(m) == tag(tV11) or versionOf(m) == tag(tV20)
