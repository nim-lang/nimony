## json — a NIF-backed JSON model for the Nimony stdlib.
##
## Built directly on `nifcore`, the same in-memory NIF representation the
## compiler's plugin API uses. A JSON document is **not** a tree of heap
## `ref JsonNode`s; it is a single flat `nifcore.TokenBuf`, navigated with a
## `JsonNode` — a thin wrapper over a `nifcore.Cursor`. The traversal verbs
## (`into` / `hasMore` / `skip`) and the
## builder verbs (`openTag` / `addStrLit` / …) are *literally the same calls* a
## plugin author uses on a compiler tree — so learning the JSON model and the
## NIF model is one act, not two. This is the unifying-stdlib idea: the concepts
## translate directly because the substrate is shared.
##
## Encoding (a JSON value as a `nifcore` tag-stream)::
##
##   null   → (null)              # 1 TagLit, empty body
##   <err>  → (null "message")    # parse error: a null carrying a diagnostic
##   true   → (true)
##   false  → (false)
##   42     → IntLit 42           # inline atom
##   3.14   → FloatLit 3.14
##   "hi"   → StrLit "hi"
##   [a,b]  → (aconstr a b)
##   {k:v}  → (oconstr (kv "k" v))
##
## Three layers, mirroring how `nifply` relates typed Nim to NIF:
##
## * **parse / read** — `parseJson` / `parseFile` build a `JsonTree`; navigate
##   with `kind`, `getStr`/`getInt`/…, `len`, `items`, `pairs`, `keys`,
##   `hasKey`/`contains`, `[]`/`{}`, `getFields`/`getElems`, and deep
##   `==`/`hash`. `$`, `toUgly`, `pretty`, and `escapeJson*` render a node.
## * **typed map** — `toJson` (typed Nim → JSON) and `fromJson` (JSON → typed
##   Nim), the JSON siblings of `nifply`'s `toNif`/`fromNif`. Same reflection
##   magics (`internalFieldPairs` / `internalTypeName`), same `oconstr`/`kv`/
##   `aconstr` tag vocabulary — differing only where idiomatic JSON differs
##   (string keys, enums as name-strings, optional `"type"` discriminator).
## * **line info** — `parseJson` takes a `LineInfoMode`; under `KeepLineInfo`
##   each node carries a `nifcore.LineInfoLit`, the same cheap source-position
##   token NIF uses. Building (`toJson`) carries none — there is no source.

when defined(nimony):
  {.feature: "untyped".}      # generic bodies sem-checked at instantiation
  {.feature: "lenientnils".}  # nifcore returns nil pools for ownerless cursors

import std / [parsejson, streams, parseutils, assertions, formatfloat,
              syncio, tables, hashes]
import ../../src/lib/nifcore

export parsejson  # callers may want JsonParser, getTok, etc.

# ── Reflection magics (shared with nifply) ───────────────────────────────────

func internalTypeName*[T](x: typedesc[T]): string {.magic: "InternalTypeName", noSideEffect.}

iterator internalFieldPairs*[T: tuple|object](x: T): tuple[key: string, val: untyped] {.
  magic: "InternalFieldPairs", noSideEffect.}

# ── Types ────────────────────────────────────────────────────────────────────

type
  JsonKind* = enum
    ## Tag-side kind of a JSON node. `JKNull` sits at ordinal 0 — the
    ## semantically appropriate "no value" answer. `nifcore`'s BiTable ids start
    ## at 1 (id 0 is the "not used" sentinel), so the `tagId` / `jsonKind` shims
    ## add and subtract 1 at the boundary. The string forms double as the NIF
    ## tag names registered by `createTags[JsonKind]`.
    JKNull       = (0, "null")
    JKTrue       = (1, "true")
    JKFalse      = (2, "false")
    JKObject     = (3, "oconstr")
    JKArray      = (4, "aconstr")
    JKKv         = (5, "kv")

  JsonNodeKind* = enum
    ## High-level value kind, the way `std/json` exposes it. Combines tag-driven
    ## kinds (Null/Bool/Object/Array) with atom-driven kinds (Int/Float/String).
    JNull, JBool, JInt, JFloat, JString, JObject, JArray

  LineInfoMode* = enum
    ## Whether (and how precisely) `parseJson` attaches source positions to the
    ## nodes it builds. The cost is one trailing `nifcore.LineInfoLit` token per
    ## node; `DiscardLineInfo` emits none, so the buffer never grows for it.
    DiscardLineInfo     ## no positions (cheapest; default)
    KeepLineInfo        ## file + line per node
    KeepColumnInfo      ## file + line + column per node

  JsonOptions* = object
    ## Configuration threaded recursively through every `toJson` / `fromJson`
    ## overload. Lives in one place so the scalar leaves and the composite
    ## walkers agree on policy.
    typeFieldName*: string
      ## When non-empty, object encodings carry a discriminator pair
      ## `("<typeFieldName>": "<TypeName>")` as the first member — an ordinary
      ## JSON key/value, readable by any consumer. `fromJson` recognises and
      ## skips it. Empty (default) ⇒ no discriminator.

  JsonTree* = object
    ## Owns the flat token buffer backing a JSON document. Move-only (the
    ## underlying `TokenBuf` forbids copy); a `JsonNode` obtained via `root` keeps
    ## the data alive independently through `nifcore`'s reference counting.
    buf*: TokenBuf

  JsonNode* = object
    ## A handle to one node inside a `JsonTree`: a `nifcore.Cursor` wrapped so the
    ## read API (`kind`, `getStr`, `items`, …) can overload on `JsonNode`. That
    ## is the whole point of the wrapper — `nifcore` already exports a
    ## `kind(Cursor)`, and a second `kind(Cursor)` returning `JsonNodeKind` would
    ## be an outright redefinition. Wrapping the cursor changes the parameter
    ## type, so the two `kind`s coexist as ordinary overloads and the in-module
    ## `nifcore.` qualifications disappear. Copyable (the underlying `Cursor` is
    ## reference-counted), so nodes can be passed and stored freely.
    c*: Cursor

proc `=copy`(dest: var JsonTree; src: JsonTree) {.error.}

# ── Boundary shims (unchanged from the jsonnif blueprint) ────────────────────
# The +/-1 collapses to a single add/sub; both are register-only.

template tagId*(k: JsonKind): TagId   = TagId(uint32(k) + 1'u32)
template jsonKind*(t: TagId): JsonKind = cast[JsonKind](uint32(t) - 1'u32)

# ── Construction ─────────────────────────────────────────────────────────────

proc createJsonTree*(sharedPool: Pool = nil): JsonTree =
  ## Mint an empty tree. Pass `sharedPool` to intern this tree's string literals
  ## (and, under `Keep*LineInfo`, filenames) into a pool shared with other trees
  ## or other NIF adapters — cross-format dedup, the point of the split-pool
  ## design. Each tree still gets its own fresh `TagPool` so the `JsonKind` cast
  ## stays a register move.
  result = JsonTree(buf: createTokenBuf(16, sharedPool, createTags[JsonKind]()))

# ── Parser ───────────────────────────────────────────────────────────────────
#
# The whole parse path is `raises`-free: `lexbase` collects IO failures in
# `ioError`, and `parsejson` collects syntax errors in `err`/`kind` state. So we
# call `open`/`getTok`/`close` directly — no exceptions to guard.
#
# Errors are propagated *through the tree itself*. Wherever a value was expected
# but the input was malformed, the parser emits a `(null "<diagnostic>")` in that
# slot (see `emitError`). Under NIF rules that is still a perfectly valid `null`:
# a reader that never looks inside sees `JNull` and keeps going, so a broken
# document degrades gracefully rather than raising or silently truncating. Code
# that cares calls `isError` / `errorMsg` (per node) or `errorMsg(tree)` /
# `hasError(tree)` (whole document) to recover the first diagnostic and its
# source position. No separate error channel; the bad spot is marked in place.

proc emitInfo(p: var JsonParser; t: var JsonTree; mode: LineInfoMode;
              file: FileId) {.inline.} =
  ## Attach the current source position to the head token just emitted. Must run
  ## immediately after the `addX` / `openTag` so the `LineInfoLit` lands as that
  ## head's trailing suffix.
  if mode != DiscardLineInfo:
    let col = if mode == KeepColumnInfo: int32(getColumn(p)) else: 0'i32
    t.buf.appendLineInfo(file, int32(getLine(p)), col)

proc emitError(p: var JsonParser; t: var JsonTree; mode: LineInfoMode;
               file: FileId; err: JsonError) =
  ## Emit an error placeholder — `(null "<diagnostic>")` — in the value slot the
  ## parser failed on. The node reads as an ordinary `JNull` to everything that
  ## does not look inside; `isError` / `errorMsg` recover the message. `setError`
  ## also records the (first) error in the parser state, keeping `hasError(p)`
  ## true. The message is built for *this* `err`, not the sticky `p.err`, so each
  ## placeholder names the error at its own position.
  setError(p, err)
  t.buf.openTag JKNull.tagId
  emitInfo(p, t, mode, file)
  t.buf.addStrLit errorMsgFor(p, err)
  t.buf.closeTag()

proc emitErrorKv(p: var JsonParser; t: var JsonTree; mode: LineInfoMode;
                 file: FileId; err: JsonError) =
  ## Wrap an `emitError` placeholder in an empty-keyed `kv` so it can sit in an
  ## object body without breaking the `(kv …)`-per-member invariant `pairs`
  ## relies on. Used for object-level failures (bad key, missing `}`).
  t.buf.openTag JKKv.tagId
  t.buf.addStrLit ""
  emitError(p, t, mode, file, err)
  t.buf.closeTag()

const
  JsonMaxDepth* = 1000
    ## Deepest array/object nesting `parseJson` accepts. The parser and every
    ## tree walker (`$`, `pretty`, `fromJson`, …) recurse once per level, so
    ## unbounded nesting in untrusted input would overflow the stack. A deeper
    ## value is replaced by a `(null "nesting too deep")` error placeholder and
    ## the rest of the input is discarded.

proc parseValue(p: var JsonParser; t: var JsonTree; mode: LineInfoMode; file: FileId;
                depth: int)

proc skipRest(p: var JsonParser) =
  ## After a structural error the parser cannot know where the broken
  ## container was meant to end, so the rest of the input is dropped. Every
  ## enclosing level then sees `tkEof` and marks its own truncation: the
  ## document is cut short *visibly*, never silently.
  while p.tok != tkEof: discard getTok(p)

proc parseObject(p: var JsonParser; t: var JsonTree; mode: LineInfoMode; file: FileId;
                 depth: int) =
  t.buf.openTag JKObject.tagId
  emitInfo(p, t, mode, file)
  discard getTok(p)
  var truncated = false
  while p.tok != tkCurlyRi and p.tok != tkEof:
    if p.tok != tkString:
      emitErrorKv(p, t, mode, file, errStringExpected)   # malformed key
      skipRest(p)
      truncated = true
      break
    t.buf.openTag JKKv.tagId
    t.buf.addStrLit p.a
    emitInfo(p, t, mode, file)
    discard getTok(p)
    if p.tok == tkColon:
      discard getTok(p)
      parseValue(p, t, mode, file, depth + 1)
    else:
      emitError(p, t, mode, file, errColonExpected)      # value slot ← error null
    t.buf.closeTag()        # close kv
    if p.tok != tkComma: break
    discard getTok(p)
  if not truncated:
    if p.tok == tkCurlyRi:
      discard getTok(p)
    elif p.tok == tkEof:
      emitErrorKv(p, t, mode, file, errCurlyRiExpected)  # mark truncation in-tree
    else:
      emitErrorKv(p, t, mode, file, errCommaExpected)    # `{"a": 1 "b": 2}`
      skipRest(p)
  t.buf.closeTag()          # close oconstr

proc parseArray(p: var JsonParser; t: var JsonTree; mode: LineInfoMode; file: FileId;
                depth: int) =
  t.buf.openTag JKArray.tagId
  emitInfo(p, t, mode, file)
  discard getTok(p)
  while p.tok != tkBracketRi and p.tok != tkEof:
    parseValue(p, t, mode, file, depth + 1)
    if p.tok != tkComma: break
    discard getTok(p)
  if p.tok == tkBracketRi:
    discard getTok(p)
  elif p.tok == tkEof:
    emitError(p, t, mode, file, errBracketRiExpected)    # mark truncation in-tree
  else:
    emitError(p, t, mode, file, errCommaExpected)        # `[1 2]`
    skipRest(p)
  t.buf.closeTag()

proc parseValue(p: var JsonParser; t: var JsonTree; mode: LineInfoMode; file: FileId;
                depth: int) =
  case p.tok
  of tkString:
    t.buf.addStrLit p.a; emitInfo(p, t, mode, file); discard getTok(p)
  of tkInt:
    var v: BiggestInt = 0
    if parseBiggestInt(p.a, v) > 0:
      t.buf.addIntLit int64(v)
    else:
      # Integer literal outside the int64 range (e.g. C's UINT64_MAX in header
      # dumps): the standard JSON fallback is to keep it as a (lossy) float atom
      # rather than aborting the whole parse.
      var f: BiggestFloat = 0.0
      discard parseBiggestFloat(p.a, f)
      t.buf.addFloatLit float64(f)
    emitInfo(p, t, mode, file); discard getTok(p)
  of tkFloat:
    var v: BiggestFloat = 0.0
    discard parseBiggestFloat(p.a, v)
    t.buf.addFloatLit float64(v); emitInfo(p, t, mode, file); discard getTok(p)
  of tkTrue:
    t.buf.openTag JKTrue.tagId; emitInfo(p, t, mode, file); t.buf.closeTag()
    discard getTok(p)
  of tkFalse:
    t.buf.openTag JKFalse.tagId; emitInfo(p, t, mode, file); t.buf.closeTag()
    discard getTok(p)
  of tkNull:
    t.buf.openTag JKNull.tagId; emitInfo(p, t, mode, file); t.buf.closeTag()
    discard getTok(p)
  of tkCurlyLe, tkBracketLe:
    if depth >= JsonMaxDepth:
      # Too deep: mark the slot and drop the rest of the input, so neither
      # this parser nor any later tree walk recurses past the limit. Every
      # enclosing level then sees `tkEof` and closes with its own
      # truncation marker.
      emitError(p, t, mode, file, errNestingTooDeep)
      skipRest(p)
    elif p.tok == tkCurlyLe:
      parseObject(p, t, mode, file, depth)
    else:
      parseArray(p, t, mode, file, depth)
  else:
    # no value here — emit a (null "expr expected") placeholder and advance so
    # the caller's loop makes progress instead of spinning on the bad token.
    emitError(p, t, mode, file, errExprExpected)
    discard getTok(p)

proc parseInto(stream: Stream; filename: string; lineInfo: LineInfoMode;
               t: var JsonTree) =
  ## Shared worker: drive the event parser into the already-created `t`. Kept
  ## separate so each public overload builds its own `result` in place — `JsonTree`
  ## is move-only, so returning one overload's value from another would need a
  ## (non-existent) `=dup`.
  var p = default(JsonParser)
  open(p, stream, filename)
  var file = NoFile
  if lineInfo != DiscardLineInfo:
    file = t.buf.pool.filenames.getOrIncl(filename)
  discard getTok(p)
  parseValue(p, t, lineInfo, file, 0)
  if p.tok != tkEof:
    # Trailing input after the document (`[1] [2]`, `{} x`): recorded as a
    # second top-level error node, which `errorMsg(tree)` looks at but `root`
    # never reaches.
    emitError(p, t, lineInfo, file, errEofExpected)
  close(p)

proc parseJson*(stream: Stream; filename = "";
                lineInfo = DiscardLineInfo; sharedPool: Pool = nil): JsonTree =
  ## Parse a full document off `stream` into an in-memory `JsonTree`. Under
  ## `KeepLineInfo` / `KeepColumnInfo` every node records its source position
  ## (the filename interned in the tree's `pool`, deduped if `sharedPool` is
  ## passed) — the same cheap mechanism NIF trees use, which `std/json` cannot
  ## offer.
  result = createJsonTree(sharedPool)
  parseInto(stream, filename, lineInfo, result)

proc parseJson*(buffer: string; filename = "input";
                lineInfo = DiscardLineInfo; sharedPool: Pool = nil): JsonTree =
  result = createJsonTree(sharedPool)
  parseInto(newStringStream(buffer), filename, lineInfo, result)

proc parseFile*(filename: string;
                lineInfo = DiscardLineInfo; sharedPool: Pool = nil): JsonTree =
  var fs = newFileStream(filename, fmRead)
  if fs == nil:
    quit "json.parseFile: cannot read " & filename
  result = createJsonTree(sharedPool)
  parseInto(fs, filename, lineInfo, result)

# ── Node accessors (forward to the wrapped Cursor) ───────────────────────────
# These let the rest of the module treat a `JsonNode` exactly the way the old
# code treated a bare `Cursor` — `c.skip`, `c.into`, `rawKind(c)` — without ever
# spelling out `c.c`. `rawKind` is the one true alias for `nifcore.kind`; with
# `kind` now taking a `JsonNode` there is no shadowing left to qualify around.

# `rawKind` / `cursorTagId` are *procs*, not templates: a template forwarding to
# the module-qualified `nifcore.kind` expands at the call site, and the generic
# `fromJson` instantiates in modules that never imported `nifcore`, so the
# qualifier would not resolve there. As procs they are sem-checked here, in
# json.nim's scope, once.
proc rawKind(n: JsonNode): NifKind {.inline.} =
  if cursorIsNil(n.c): DotToken else: nifcore.kind(n.c)
proc cursorTagId(n: JsonNode): TagId {.inline.} = nifcore.cursorTagId(n.c)
proc strVal(n: JsonNode): string {.inline.}     = strVal(n.c)
proc intVal(n: JsonNode): int64 {.inline.}      = intVal(n.c)
proc floatVal(n: JsonNode): float64 {.inline.}  = floatVal(n.c)
proc inc(n: var JsonNode) {.inline.} =
  if not cursorIsNil(n.c): inc n.c
proc skip(n: var JsonNode) {.inline.} =
  if not cursorIsNil(n.c): skip n.c
template hasMore(n: JsonNode): bool             = hasMore(n.c)
template into(n: var JsonNode; body: untyped)   = into(n.c, body)

# ── Read API ─────────────────────────────────────────────────────────────────

proc root*(t: var JsonTree): JsonNode {.inline.} = JsonNode(c: t.buf.beginRead())

proc kind*(n: JsonNode): JsonNodeKind =
  ## Effective high-level kind of the node. Coexists with `nifcore.kind(Cursor)`
  ## as a plain overload because `n` is a `JsonNode`, not a `Cursor`.
  case rawKind(n)
  of IntLit:   JInt
  of FloatLit: JFloat
  of StrLit:   JString
  of TagLit:
    case jsonKind(n.cursorTagId)
    of JKNull:           JNull
    of JKTrue, JKFalse:  JBool
    of JKObject:         JObject
    of JKArray:          JArray
    of JKKv:             JNull   # kv shouldn't appear where a value is expected
  else: JNull

proc info*(n: JsonNode): NifLineInfo {.inline.} =
  ## Source position recorded for the node, or `NoNifLineInfo` when the tree was
  ## parsed with `DiscardLineInfo` (or built programmatically).
  if cursorIsNil(n.c): NoNifLineInfo else: rawLineInfo(n.c)

proc fileName*(n: JsonNode): string {.inline.} =
  ## Source filename for the node, or `""` when no line info is present.
  if cursorIsNil(n.c): "" else: lineInfoFile(n.c)

proc getStr*(n: JsonNode; default = ""): string =
  if rawKind(n) == StrLit: strVal(n) else: default

proc getInt*(n: JsonNode; default: int64 = 0): int64 =
  if rawKind(n) == IntLit: intVal(n) else: default

proc getFloat*(n: JsonNode; default = 0.0): float =
  case rawKind(n)
  of FloatLit: floatVal(n)
  of IntLit:   float(intVal(n))
  else:        default

proc getBool*(n: JsonNode; default = false): bool =
  if rawKind(n) == TagLit:
    case jsonKind(n.cursorTagId)
    of JKTrue:  return true
    of JKFalse: return false
    else: discard
  default

proc getBiggestInt*(n: JsonNode; default: BiggestInt = 0): BiggestInt =
  ## Retrieves the integer value as a `BiggestInt`; `getInt` is the
  ## `int64`-shaped equivalent in this model.
  if rawKind(n) == IntLit: BiggestInt(intVal(n)) else: default

proc getFields*(n: JsonNode;
                default: OrderedTable[string, JsonNode]):
                OrderedTable[string, JsonNode] =
  ## Materialises a `JObject` into an ordered table. Returns `default` for
  ## non-objects, matching `std/json`'s fallback contract.
  if kind(n) != JObject: return default
  result = initOrderedTable[string, JsonNode]()
  for k, v in pairs(n):
    result[k] = v

proc getFields*(n: JsonNode): OrderedTable[string, JsonNode] =
  getFields(n, initOrderedTable[string, JsonNode]())

proc getElems*(n: JsonNode; default: seq[JsonNode]): seq[JsonNode] =
  ## Materialises a `JArray` into a sequence. Returns `default` for
  ## non-arrays, matching `std/json`'s fallback contract.
  if kind(n) != JArray: return default
  result = @[]
  for el in items(n):
    result.add el

proc getElems*(n: JsonNode): seq[JsonNode] =
  getElems(n, @[])

# ── Error inspection ──────────────────────────────────────────────────────────
# A parse error is stored as a `null` carrying a diagnostic string: `(null
# "msg")`. To any reader that does not look inside it is a plain `JNull`, so a
# malformed document still traverses cleanly. These helpers are the *opt-in*
# channel that surfaces the diagnostic.

proc errorMsg*(n: JsonNode): string =
  ## If the node is an error placeholder (a `null` with a diagnostic string
  ## child, `(null "msg")`), return that message; otherwise `""`. A clean `null`
  ## — and any other node — yields `""`.
  result = ""
  if rawKind(n) == TagLit and jsonKind(n.cursorTagId) == JKNull:
    var c2 = n
    c2.into:
      while c2.hasMore:
        if rawKind(c2) == StrLit: result = strVal(c2)
        c2.skip

proc isError*(n: JsonNode): bool {.inline.} =
  ## True when the node is an error placeholder (see `errorMsg`).
  errorMsg(n).len > 0

proc firstErrorMsg(n: JsonNode): string =
  ## Depth-first search for the first error placeholder under `n`; `""` if clean.
  if isError(n): return errorMsg(n)
  result = ""
  case kind(n)
  of JArray:
    var c2 = n
    c2.into:
      while c2.hasMore:
        let m = firstErrorMsg(c2)
        if m.len > 0: return m
        c2.skip
  of JObject:
    var c2 = n
    c2.into:
      while c2.hasMore:
        # each member is a (kv key value); descend into the value
        var kv = c2
        kv.into:
          kv.inc                       # skip key
          let m = firstErrorMsg(kv)
          if m.len > 0: return m       # early-out: leaves `kv`'s `into` unbalanced,
                                       # but we exit the proc, so its assert is skipped
          kv.skip
        c2.skip
  else: discard

proc errorMsg*(t: var JsonTree): string =
  ## The first error message embedded anywhere in the document (depth-first), or
  ## `""` if it parsed cleanly. Ignore it and malformed nodes simply read as
  ## `null`; consult it for the diagnostic and source position of the first
  ## failure. Trailing input after the document is reported here as well.
  result = ""
  var c = t.root
  while c.hasMore:
    result = firstErrorMsg(c)
    if result.len > 0: return
    c.skip

proc hasError*(t: var JsonTree): bool {.inline.} =
  ## True when the document contains any parse error (see `errorMsg`).
  errorMsg(t).len > 0

proc len*(n: JsonNode): int =
  ## Number of elements (array) or members (object). 0 for scalars.
  result = 0
  case kind(n)
  of JArray, JObject:
    var c = n
    c.into:
      while c.hasMore:
        inc result
        c.skip
  else: discard

iterator items*(n: JsonNode): JsonNode =
  # Inline iterators are inferred `noSideEffect` by Nimony; the cursor traversal
  # helpers (`into`/`skip`, and `assert`'s failure path) count as effects, so we
  # vouch for purity the same way tables.nim does for its checked accessors.
  {.cast(noSideEffect).}:
    assert kind(n) == JArray, "items: not a JArray"
    var c2 = n
    c2.into:
      while c2.hasMore:
        yield c2
        c2.skip

iterator pairs*(n: JsonNode): (string, JsonNode) =
  {.cast(noSideEffect).}:
    assert kind(n) == JObject, "pairs: not a JObject"
    var c2 = n
    c2.into:
      while c2.hasMore:
        assert rawKind(c2) == TagLit and jsonKind(c2.cursorTagId) == JKKv,
               "malformed object: child is not a kv"
        c2.into:
          let key = strVal(c2)
          c2.inc
          yield (key, c2)
          c2.skip

proc nullNode(): JsonNode =
  ## A standalone `(null)`. The node's cursor keeps its one-token buffer alive,
  ## so it is as safe to read, skip and print as any node of a parsed tree.
  var t = createJsonTree()
  t.buf.openTag JKNull.tagId
  t.buf.closeTag()
  result = t.root

proc hasKey*(node: JsonNode; key: string): bool =
  ## Checks whether `key` exists in a `JObject`.
  assert kind(node) == JObject, "hasKey: not a JObject"
  result = false
  for k, v in pairs(node):
    if k == key: return true

proc contains*(node: JsonNode; key: string): bool =
  ## Checks whether `key` exists in a `JObject`.
  assert kind(node) == JObject, "contains: not a JObject"
  result = hasKey(node, key)

proc getOrDefault*(node: JsonNode; key: string): JsonNode =
  ## Returns the value for `key`, or a `JNull` node when `node` is not an
  ## object or the key is absent.
  if kind(node) != JObject: return nullNode()
  for k, v in pairs(node):
    if k == key: return v
  result = nullNode()

proc `[]`*(node: JsonNode; name: string): JsonNode {.raises.} =
  ## Gets a field from a `JObject`; raises `KeyError` when absent. Use
  ## `getOrDefault` or `{}` when a missing field should produce `JNull`.
  assert kind(node) == JObject, "[]: not a JObject"
  for k, v in pairs(node):
    if k == name: return v
  raise KeyError

proc `[]`*(node: JsonNode; index: int): JsonNode =
  ## Gets the element at `index` in a `JArray`.
  assert kind(node) == JArray, "[]: not a JArray"
  let n = len(node)
  assert index >= 0 and index < n, "[]: index out of bounds"
  var i = 0
  for el in items(node):
    if i == index: return el
    inc i
  result = nullNode()

proc `[]`*(node: JsonNode; index: BackwardsIndex): JsonNode =
  ## `node[^i]` is `node[node.len - i]`.
  `[]`(node, node.len - int(index))

proc `[]`*[U, V](node: JsonNode; x: HSlice[U, V]): JsonNode =
  ## Returns a new `JArray` containing the inclusive slice.
  assert kind(node) == JArray, "[]: not a JArray"
  let lo = when x.a is BackwardsIndex: node.len - int(x.a) else: int(x.a)
  let count = (when x.b is BackwardsIndex: node.len - int(x.b)
               else: int(x.b)) - lo + 1
  var t = createJsonTree()
  t.buf.openTag JKArray.tagId
  for i in 0 ..< count:
    t.buf.addSubtree(node[lo + i].c)
  t.buf.closeTag()
  result = t.root

# A single-key lookup (`node{"key"}`) is covered by this varargs overload;
# Nimony reports a separate single-string overload as ambiguous with it.
proc `{}`*(node: JsonNode; keys: varargs[string]): JsonNode =
  ## Traverses nested objects; returns a `JNull` node when a step is missing.
  result = node
  for key in keys:
    if kind(result) != JObject: return nullNode()
    result = result.getOrDefault(key)

proc `{}`*(node: JsonNode; indexes: varargs[int]): JsonNode =
  ## Traverses nested arrays; returns a `JNull` node when a step is missing.
  result = node
  for i in indexes:
    if kind(result) != JArray or i < 0 or i >= len(result):
      return nullNode()
    result = result[i]

iterator keys*(node: JsonNode): string =
  ## Iterates over the keys of a `JObject`.
  for k, v in pairs(node):
    yield k

proc `{}`*(t: var JsonTree; key: string): JsonNode =
  ## Object member lookup. Returns a `JNull` node when the root is not an
  ## object or the key is absent.
  var c = t.root
  if kind(c) == JObject:
    for k, v in pairs(c):
      if k == key:
        return v
  result = nullNode()

func `==`*(a, b: JsonNode): bool =
  ## Deep JSON value equality. Object member order is irrelevant.
  {.cast(noSideEffect).}:
    if kind(a) != kind(b): return false
    case kind(a)
    of JNull:  result = true
    of JBool:  result = getBool(a) == getBool(b)
    of JInt:   result = getInt(a) == getInt(b)
    of JFloat: result = getFloat(a) == getFloat(b)
    of JString: result = getStr(a) == getStr(b)
    of JArray:
      if len(a) != len(b): return false
      for i in 0 ..< len(a):
        if a[i] != b[i]: return false
      result = true
    of JObject:
      if len(a) != len(b): return false
      for key, val in pairs(a):
        if not hasKey(b, key): return false
        if val != b.getOrDefault(key): return false
      result = true

func hash*(n: JsonNode): Hash =
  ## Deep hash matching `==`: object insertion order does not matter.
  {.cast(noSideEffect).}:
    case kind(n)
    of JNull:  result = Hash(0)
    of JBool:  result = hash(getBool(n))
    of JInt:   result = hash(getInt(n))
    of JFloat: result = hash(getFloat(n))
    of JString: result = hash(getStr(n))
    of JArray:
      result = 0'u
      for el in items(n):
        result = result !& hash(el)
      result = !$result
    of JObject:
      result = 0'u
      for key, val in pairs(n):
        result = result xor (hash(key) !& hash(val))
      result = !$result

proc contains*(node: JsonNode; val: JsonNode): bool =
  ## Checks whether `val` exists in a `JArray`.
  assert kind(node) == JArray, "contains: not a JArray"
  result = false
  for el in items(node):
    if el == val: return true

# ── Pretty-print ─────────────────────────────────────────────────────────────

proc escapeJsonUnquoted*(s: string; result: var string) =
  ## Appends the JSON representation of `s` without surrounding quotes.
  const hexChars = "0123456789ABCDEF"
  for c in s:
    case c
    of '\L': result.add "\\n"
    of '\b': result.add "\\b"
    of '\f': result.add "\\f"
    of '\t': result.add "\\t"
    of '\v': result.add "\\u000b"
    of '\r': result.add "\\r"
    of '"':  result.add "\\\""
    of '\0'..'\7':
      result.add "\\u000"
      result.add $ord(c)
    of '\14'..'\31':
      result.add "\\u00"
      result.add hexChars[ord(c) shr 4]
      result.add hexChars[ord(c) and 0xF]
    of '\\': result.add "\\\\"
    else:    result.add c

proc escapeJsonUnquoted*(s: string): string =
  ## Returns the JSON representation of `s` without surrounding quotes.
  result = ""
  escapeJsonUnquoted(s, result)

proc escapeJson*(s: string; result: var string) =
  ## Appends the quoted JSON representation of `s`.
  result.add '"'
  escapeJsonUnquoted(s, result)
  result.add '"'

proc escapeJson*(s: string): string =
  ## Returns the quoted JSON representation of `s`.
  result = ""
  escapeJson(s, result)

proc emitUgly(c: var JsonNode; r: var string) =
  case kind(c)
  of JNull:  r.add "null"; c.skip
  of JBool:  r.add (if getBool(c): "true" else: "false"); c.skip
  of JInt:   r.add $getInt(c);   c.inc
  of JFloat: r.add $getFloat(c); c.inc
  of JString:
    escapeJson(getStr(c), r); c.inc
  of JArray:
    r.add '['
    var first = true
    c.into:
      while c.hasMore:
        if not first: r.add ','
        first = false
        emitUgly(c, r)
    r.add ']'
  of JObject:
    r.add '{'
    var first = true
    c.into:
      while c.hasMore:
        if not first: r.add ','
        first = false
        assert jsonKind(c.cursorTagId) == JKKv
        c.into:
          escapeJson(strVal(c), r)
          r.add ':'
          c.inc
          emitUgly(c, r)
    r.add '}'

proc toUgly*(result: var string, node: JsonNode) =
  ## Appends the compact JSON representation of `node` to `result`.
  var c = node
  emitUgly(c, result)

proc `$`*(node: JsonNode): string =
  ## Returns the compact JSON representation of `node`.
  result = newStringOfCap(node.len shl 1)
  toUgly(result, node)

proc addIndent(r: var string; count: int) =
  for i in 0 ..< count:
    r.add ' '

proc toPretty(r: var string; node: JsonNode; indent, currIndent: int;
              inArray: bool) =
  case kind(node)
  of JObject:
    if inArray: addIndent(r, currIndent)
    if len(node) == 0:
      r.add "{}"
      return
    r.add "{\n"
    var first = true
    for key, val in pairs(node):
      if not first: r.add ",\n"
      first = false
      addIndent(r, currIndent + indent)
      escapeJson(key, r)
      r.add ": "
      toPretty(r, val, indent, currIndent + indent, false)
    r.add "\n"
    addIndent(r, currIndent)
    r.add "}"
  of JArray:
    if inArray: addIndent(r, currIndent)
    if len(node) == 0:
      r.add "[]"
      return
    r.add "[\n"
    var first = true
    for el in items(node):
      if not first: r.add ",\n"
      first = false
      toPretty(r, el, indent, currIndent + indent, true)
    r.add "\n"
    addIndent(r, currIndent)
    r.add "]"
  else:
    if inArray: addIndent(r, currIndent)
    toUgly(r, node)

proc pretty*(node: JsonNode; indent = 2): string =
  ## Returns a multi-line, indented JSON representation of `node`.
  result = ""
  toPretty(result, node, indent, 0, false)

proc `$`*(t: var JsonTree): string =
  result = newStringOfCap(64)
  toUgly(result, t.root)

# ── Typed map: toJson (typed Nim → JSON) ─────────────────────────────────────
# Siblings of nifply's `toNif`. Every overload takes `opts` in the same
# position so the composite walkers can recurse with `toJson(t, field, opts)`.

proc bareName(mangled: string): string =
  ## `internalFieldPairs` / `internalTypeName` yield names with a NIF
  ## disambiguation suffix (`x.0`, `Box.0.<module>`); JSON keys and the `"type"`
  ## discriminator want the bare source name before the first `.`.
  result = ""
  for c in mangled:
    if c == '.': break
    result.add c

proc toJson*(t: var JsonTree; x: int; opts: JsonOptions) =
  t.buf.addIntLit int64(x)
proc toJson*(t: var JsonTree; x: float; opts: JsonOptions) =
  t.buf.addFloatLit float64(x)
proc toJson*(t: var JsonTree; x: string; opts: JsonOptions) =
  t.buf.addStrLit x
proc toJson*(t: var JsonTree; x: bool; opts: JsonOptions) =
  t.buf.buildTree (if x: JKTrue.tagId else: JKFalse.tagId):
    discard
proc toJson*[E: enum](t: var JsonTree; x: E; opts: JsonOptions) =
  t.buf.addStrLit $x                       # idiomatic JSON: enum by name

proc toJson*[T](t: var JsonTree; x: seq[T]; opts: JsonOptions) =
  t.buf.openTag JKArray.tagId
  for it in x:
    toJson(t, it, opts)
  t.buf.closeTag()

proc toJson*[O: object](t: var JsonTree; x: O; opts: JsonOptions) =
  t.buf.openTag JKObject.tagId
  if opts.typeFieldName.len > 0:
    t.buf.buildTree JKKv.tagId:
      t.buf.addStrLit opts.typeFieldName
      t.buf.addStrLit bareName(internalTypeName(O))
  for name, f in internalFieldPairs(x):
    t.buf.buildTree JKKv.tagId:
      t.buf.addStrLit bareName(name)
      toJson(t, f, opts)                   # opts threads down
  t.buf.closeTag()

proc toJson*[T](x: T; opts = JsonOptions()): JsonTree =
  ## Entry point: serialise `x` to a fresh `JsonTree`. Distinguished from the
  ## recursive workers by arity (1–2 args, no leading `var JsonTree`).
  result = createJsonTree()
  toJson(result, x, opts)

# ── Typed map: fromJson (JSON → typed Nim) ───────────────────────────────────
# Siblings of nifply's `fromNif`. A mutating `var T` overload (used by the
# object walker via internalFieldPairs) delegates to the typedesc overloads.

proc fromJson*(c: var JsonNode; t: typedesc[int]; opts: JsonOptions): int =
  assert rawKind(c) == IntLit, "expected int"
  result = int(intVal(c)); c.inc
proc fromJson*(c: var JsonNode; t: typedesc[float]; opts: JsonOptions): float =
  result = 0.0
  case rawKind(c)
  of FloatLit: result = floatVal(c)
  of IntLit:   result = float(intVal(c))
  else:        assert false, "expected number"
  c.inc
proc fromJson*(c: var JsonNode; t: typedesc[string]; opts: JsonOptions): string =
  assert rawKind(c) == StrLit, "expected string"
  result = strVal(c); c.inc
proc fromJson*(c: var JsonNode; t: typedesc[bool]; opts: JsonOptions): bool =
  assert rawKind(c) == TagLit, "expected bool"
  result = jsonKind(c.cursorTagId) == JKTrue
  c.skip

proc fromJson*[E: enum](c: var JsonNode; t: typedesc[E]; opts: JsonOptions): E =
  assert rawKind(c) == StrLit, "expected enum string"
  let s = strVal(c); c.inc
  for e in E.low..E.high:
    if $e == s: return e
  assert false, "unknown enum value: " & s

proc fromJson*[T: not typedesc](c: var JsonNode; x: var T; opts: JsonOptions) {.untyped, inline.} =
  x = fromJson(c, T, opts)

proc fromJson*[T](c: var JsonNode; t: typedesc[seq[T]]; opts: JsonOptions): seq[T] =
  assert kind(c) == JArray, "expected array"
  result = @[]
  c.into:
    while c.hasMore:
      result.add fromJson(c, T, opts)

proc skipTypeField(c: var JsonNode; opts: JsonOptions) =
  ## If the next member is the `"type"` discriminator, consume it.
  if opts.typeFieldName.len > 0 and c.hasMore and
     rawKind(c) == TagLit and jsonKind(c.cursorTagId) == JKKv:
    var probe = c
    probe.into:
      if strVal(probe) == opts.typeFieldName:
        c.skip                       # drop the whole kv pair

proc fromJson*[O: object](c: var JsonNode; t: typedesc[O]; opts: JsonOptions): O {.noinit.} =
  assert kind(c) == JObject, "expected object"
  c.into:
    skipTypeField(c, opts)
    for name, f in internalFieldPairs(result):
      assert rawKind(c) == TagLit and jsonKind(c.cursorTagId) == JKKv,
             "expected object member"
      c.into:
        c.inc                        # skip key (fields assumed in declared order)
        fromJson(c, f, opts)

proc fromJson*[T](t: var JsonTree; tt: typedesc[T]; opts = JsonOptions()): T =
  ## Entry point: deserialise the root of `t` into a `T`.
  var c = t.root
  result = fromJson(c, T, opts)
