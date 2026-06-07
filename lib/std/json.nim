## json — a NIF-backed JSON model for the Nimony stdlib.
##
## Built directly on `nifcore`, the same in-memory NIF representation the
## compiler's plugin API uses. A JSON document is **not** a tree of heap
## `ref JsonNode`s; it is a single flat `nifcore.TokenBuf`, navigated with a
## `nifcore.Cursor`. The traversal verbs (`into` / `hasMore` / `skip`) and the
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
##   with `kind`, `getStr`/`getInt`/…, `len`, `items`, `pairs`, `{}`.
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

import std / [parsejson, streams, parseutils, assertions, formatfloat]
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
    ## underlying `TokenBuf` forbids copy); a `Cursor` obtained via `root` keeps
    ## the data alive independently through `nifcore`'s reference counting.
    buf*: TokenBuf

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

proc parseValue(p: var JsonParser; t: var JsonTree; mode: LineInfoMode; file: FileId)

proc parseObject(p: var JsonParser; t: var JsonTree; mode: LineInfoMode; file: FileId) =
  t.buf.openTag JKObject.tagId
  emitInfo(p, t, mode, file)
  discard getTok(p)
  var truncated = false
  while p.tok != tkCurlyRi and p.tok != tkEof:
    if p.tok != tkString:
      emitErrorKv(p, t, mode, file, errStringExpected)   # malformed key
      truncated = true
      break
    t.buf.openTag JKKv.tagId
    t.buf.addStrLit p.a
    emitInfo(p, t, mode, file)
    discard getTok(p)
    if p.tok == tkColon:
      discard getTok(p)
      parseValue(p, t, mode, file)
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
  t.buf.closeTag()          # close oconstr

proc parseArray(p: var JsonParser; t: var JsonTree; mode: LineInfoMode; file: FileId) =
  t.buf.openTag JKArray.tagId
  emitInfo(p, t, mode, file)
  discard getTok(p)
  while p.tok != tkBracketRi and p.tok != tkEof:
    parseValue(p, t, mode, file)
    if p.tok != tkComma: break
    discard getTok(p)
  if p.tok == tkBracketRi:
    discard getTok(p)
  elif p.tok == tkEof:
    emitError(p, t, mode, file, errBracketRiExpected)    # mark truncation in-tree
  t.buf.closeTag()

proc parseValue(p: var JsonParser; t: var JsonTree; mode: LineInfoMode; file: FileId) =
  case p.tok
  of tkString:
    t.buf.addStrLit p.a; emitInfo(p, t, mode, file); discard getTok(p)
  of tkInt:
    var v: BiggestInt = 0
    discard parseBiggestInt(p.a, v)
    t.buf.addIntLit int64(v); emitInfo(p, t, mode, file); discard getTok(p)
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
  of tkCurlyLe:   parseObject(p, t, mode, file)
  of tkBracketLe: parseArray(p, t, mode, file)
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
  parseValue(p, t, lineInfo, file)
  eat(p, tkEof)
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

# ── Read API ─────────────────────────────────────────────────────────────────

proc root*(t: var JsonTree): Cursor {.inline.} = t.buf.beginRead()

proc kind*(c: Cursor): JsonNodeKind =
  ## Effective high-level kind. The `nifcore.` prefix is required: this proc
  ## shadows the unqualified `kind`.
  case nifcore.kind(c)
  of IntLit:   JInt
  of FloatLit: JFloat
  of StrLit:   JString
  of TagLit:
    case jsonKind(c.cursorTagId)
    of JKNull:           JNull
    of JKTrue, JKFalse:  JBool
    of JKObject:         JObject
    of JKArray:          JArray
    of JKKv:             JNull   # kv shouldn't appear where a value is expected
  else: JNull

proc info*(c: Cursor): NifLineInfo {.inline.} =
  ## Source position recorded for the node at `c`, or `NoNifLineInfo` when the
  ## tree was parsed with `DiscardLineInfo` (or built programmatically).
  rawLineInfo(c)

proc fileName*(c: Cursor): string {.inline.} =
  ## Source filename for the node at `c`, or `""` when no line info is present.
  lineInfoFile(c)

proc getStr*(c: Cursor; default = ""): string =
  if nifcore.kind(c) == StrLit: strVal(c) else: default

proc getInt*(c: Cursor; default: int64 = 0): int64 =
  if nifcore.kind(c) == IntLit: intVal(c) else: default

proc getFloat*(c: Cursor; default = 0.0): float =
  case nifcore.kind(c)
  of FloatLit: floatVal(c)
  of IntLit:   float(intVal(c))
  else:        default

proc getBool*(c: Cursor; default = false): bool =
  if nifcore.kind(c) == TagLit:
    case jsonKind(c.cursorTagId)
    of JKTrue:  return true
    of JKFalse: return false
    else: discard
  default

# ── Error inspection ──────────────────────────────────────────────────────────
# A parse error is stored as a `null` carrying a diagnostic string: `(null
# "msg")`. To any reader that does not look inside it is a plain `JNull`, so a
# malformed document still traverses cleanly. These helpers are the *opt-in*
# channel that surfaces the diagnostic.

proc errorMsg*(c: Cursor): string =
  ## If the node at `c` is an error placeholder (a `null` with a diagnostic
  ## string child, `(null "msg")`), return that message; otherwise `""`. A clean
  ## `null` — and any other node — yields `""`.
  result = ""
  if nifcore.kind(c) == TagLit and jsonKind(c.cursorTagId) == JKNull:
    var c2 = c
    c2.into:
      while c2.hasMore:
        if nifcore.kind(c2) == StrLit: result = strVal(c2)
        c2.skip

proc isError*(c: Cursor): bool {.inline.} =
  ## True when the node at `c` is an error placeholder (see `errorMsg`).
  errorMsg(c).len > 0

proc firstErrorMsg(c: Cursor): string =
  ## Depth-first search for the first error placeholder under `c`; `""` if clean.
  if isError(c): return errorMsg(c)
  result = ""
  case kind(c)
  of JArray:
    var c2 = c
    c2.into:
      while c2.hasMore:
        let m = firstErrorMsg(c2)
        if m.len > 0: return m
        c2.skip
  of JObject:
    var c2 = c
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
  ## failure.
  firstErrorMsg(t.root)

proc hasError*(t: var JsonTree): bool {.inline.} =
  ## True when the document contains any parse error (see `errorMsg`).
  errorMsg(t).len > 0

proc len*(c: Cursor): int =
  ## Number of elements (array) or members (object). 0 for scalars.
  result = 0
  case kind(c)
  of JArray, JObject:
    var c = c
    c.into:
      while c.hasMore:
        inc result
        c.skip
  else: discard

iterator items*(c: Cursor): Cursor =
  # Inline iterators are inferred `noSideEffect` by Nimony; the cursor traversal
  # helpers (`into`/`skip`, and `assert`'s failure path) count as effects, so we
  # vouch for purity the same way tables.nim does for its checked accessors.
  {.cast(noSideEffect).}:
    assert kind(c) == JArray, "items: not a JArray"
    var c2 = c
    c2.into:
      while c2.hasMore:
        yield c2
        c2.skip

iterator pairs*(c: Cursor): (string, Cursor) =
  {.cast(noSideEffect).}:
    assert kind(c) == JObject, "pairs: not a JObject"
    var c2 = c
    c2.into:
      while c2.hasMore:
        assert nifcore.kind(c2) == TagLit and jsonKind(c2.cursorTagId) == JKKv,
               "malformed object: child is not a kv"
        c2.into:
          let key = strVal(c2)
          c2.inc
          yield (key, c2)
          c2.skip

proc `{}`*(t: var JsonTree; key: string): Cursor =
  ## Object member lookup. Returns a nil-ish cursor when the root is not an
  ## object or the key is absent (`kind` of it is `JNull`).
  result = t.root
  if kind(result) != JObject:
    return
  var c = t.root
  for k, v in pairs(c):
    if k == key:
      result = v
      return
  result = Cursor()

# ── Pretty-print ─────────────────────────────────────────────────────────────

proc emitEscaped(s: string; r: var string) =
  r.add '"'
  for c in s:
    case c
    of '\L': r.add "\\n"
    of '\b': r.add "\\b"
    of '\f': r.add "\\f"
    of '\t': r.add "\\t"
    of '\r': r.add "\\r"
    of '"':  r.add "\\\""
    of '\\': r.add "\\\\"
    else:    r.add c
  r.add '"'

proc emitUgly(c: var Cursor; r: var string) =
  case kind(c)
  of JNull:  r.add "null"; c.skip
  of JBool:  r.add (if getBool(c): "true" else: "false"); c.skip
  of JInt:   r.add $getInt(c);   c.inc
  of JFloat: r.add $getFloat(c); c.inc
  of JString:
    emitEscaped(getStr(c), r); c.inc
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
          emitEscaped(strVal(c), r)
          r.add ':'
          c.inc
          emitUgly(c, r)
    r.add '}'

proc `$`*(t: var JsonTree): string =
  result = newStringOfCap(64)
  var c = t.root
  emitUgly(c, result)

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

proc fromJson*(c: var Cursor; t: typedesc[int]; opts: JsonOptions): int =
  assert nifcore.kind(c) == IntLit, "expected int"
  result = int(intVal(c)); c.inc
proc fromJson*(c: var Cursor; t: typedesc[float]; opts: JsonOptions): float =
  result = 0.0
  case nifcore.kind(c)
  of FloatLit: result = floatVal(c)
  of IntLit:   result = float(intVal(c))
  else:        assert false, "expected number"
  c.inc
proc fromJson*(c: var Cursor; t: typedesc[string]; opts: JsonOptions): string =
  assert nifcore.kind(c) == StrLit, "expected string"
  result = strVal(c); c.inc
proc fromJson*(c: var Cursor; t: typedesc[bool]; opts: JsonOptions): bool =
  assert nifcore.kind(c) == TagLit, "expected bool"
  result = jsonKind(c.cursorTagId) == JKTrue
  c.skip

proc fromJson*[E: enum](c: var Cursor; t: typedesc[E]; opts: JsonOptions): E =
  assert nifcore.kind(c) == StrLit, "expected enum string"
  let s = strVal(c); c.inc
  for e in E.low..E.high:
    if $e == s: return e
  assert false, "unknown enum value: " & s

proc fromJson*[T: not typedesc](c: var Cursor; x: var T; opts: JsonOptions) {.untyped, inline.} =
  x = fromJson(c, T, opts)

proc fromJson*[T](c: var Cursor; t: typedesc[seq[T]]; opts: JsonOptions): seq[T] =
  assert kind(c) == JArray, "expected array"
  result = @[]
  c.into:
    while c.hasMore:
      result.add fromJson(c, T, opts)

proc skipTypeField(c: var Cursor; opts: JsonOptions) =
  ## If the next member is the `"type"` discriminator, consume it.
  if opts.typeFieldName.len > 0 and c.hasMore and
     nifcore.kind(c) == TagLit and jsonKind(c.cursorTagId) == JKKv:
    var probe = c
    probe.into:
      if strVal(probe) == opts.typeFieldName:
        c.skip                       # drop the whole kv pair

proc fromJson*[O: object](c: var Cursor; t: typedesc[O]; opts: JsonOptions): O {.noinit.} =
  assert kind(c) == JObject, "expected object"
  c.into:
    skipTypeField(c, opts)
    for name, f in internalFieldPairs(result):
      assert nifcore.kind(c) == TagLit and jsonKind(c.cursorTagId) == JKKv,
             "expected object member"
      c.into:
        c.inc                        # skip key (fields assumed in declared order)
        fromJson(c, f, opts)

proc fromJson*[T](t: var JsonTree; tt: typedesc[T]; opts = JsonOptions()): T =
  ## Entry point: deserialise the root of `t` into a `T`.
  var c = t.root
  result = fromJson(c, T, opts)

# ── Self-test ────────────────────────────────────────────────────────────────

when isMainModule:
  import std/syncio

  type Color = enum red, green, blue
  type Point = object
    x, y: int
    label: string
    shade: Color
  type Box = object
    v: int

  template check(a, b) =
    let xx = a
    if xx != b:
      echo "test failed ", astToStr(a), ": got ", xx, " expected ", b
      quit 1

  proc main() =
    block atoms:
      var t = parseJson("""{"a":1,"b":"hi","c":true,"d":null,"e":3.5}""")
      check len(t.root), 5
      check getInt(t{"a"}), 1
      check getStr(t{"b"}), "hi"
      check getBool(t{"c"}), true
      check kind(t{"d"}), JNull
      check getFloat(t{"e"}), 3.5

    block nested:
      var t = parseJson("""{"x":[1,2,{"y":42}]}""")
      var c = t{"x"}
      check kind(c), JArray
      check len(c), 3
      var elems: seq[int64] = @[]
      for el in items(c):
        var m = el
        if kind(m) == JInt: elems.add getInt(m)
      check elems.len, 2
      check elems[0], 1'i64
      check elems[1], 2'i64

    block round_trip:
      var t1 = parseJson("""{"a":[1,2,3],"b":"hi","c":{"x":1.5}}""")
      let s = $t1
      var t2 = parseJson(s)
      check $t2, s

    block line_info:
      var t = parseJson("{\n  \"a\": 1\n}", "doc.json", KeepLineInfo)
      let c = t{"a"}
      check info(c).line, 2'i32
      check fileName(c), "doc.json"

    block typed_round_trip:
      let p = Point(x: 3, y: 4, label: "hi", shade: green)
      var tree = toJson(p)
      check $tree, """{"x":3,"y":4,"label":"hi","shade":"green"}"""
      var back = fromJson(tree, Point)
      check back.x, 3
      check back.label, "hi"
      check back.shade, green

    block typed_with_discriminator:
      var tree = toJson(Box(v: 7), JsonOptions(typeFieldName: "type"))
      check $tree, """{"type":"Box","v":7}"""

    block error_as_null:
      # A malformed value becomes (null "msg"): still a JNull, still prints null,
      # but the diagnostic is recoverable.
      var t = parseJson("xyz")
      check kind(t.root), JNull       # reads as null to anything not looking
      check $t, "null"
      check isError(t.root), true     # ... but the error is recoverable
      check hasError(t), true
      check getStr(t.root), ""        # scalar readers degrade to defaults

    block clean_null_is_not_error:
      var t = parseJson("null")
      check kind(t.root), JNull
      check isError(t.root), false
      check hasError(t), false
      check errorMsg(t), ""

    block error_inside_array:
      var t = parseJson("[1, nul, 3]")
      check kind(t.root), JArray
      check len(t.root), 3
      check $t, "[1,null,3]"          # truncation-free print, errors look like null
      check hasError(t), true         # ... yet detectable
      var c = t.root
      var seen = 0
      for el in items(c):
        if isError(el): inc seen
      check seen, 1

    block error_inside_object:
      var t = parseJson("""{"a": 1, "b": nope}""")
      check kind(t.root), JObject
      check hasError(t), true
      check isError(t{"b"}), true
      check isError(t{"a"}), false

    block truncated_object:
      var t = parseJson("""{"a": 1""")  # missing closing brace
      check kind(t.root), JObject
      check hasError(t), true

    echo "json self-tests passed"

  main()
