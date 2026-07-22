## nifcore_compat — the old nifcursors/nifstreams public surface, backed by
## nifcore, so nimsem/hexer compile against nifcore **without renaming call
## sites**. Selected by `-d:useNifcore` (see nifcursors.nim / nifstreams.nim,
## which re-export this module under that switch). Off by default → the classic
## bundle ships and boot stays byte-identical. Full design: doc/nifcore_shim.md.
##
## Three model bridges do the real work; everything else is a rename:
##
##  1. Global pool.  nifstreams exposes ONE global `var pool: Literals` holding
##     every intern table + tags + the line-info manager (~1000 `pool.X` sites).
##     nifcore scopes pools per-TokenBuf. We emulate the global by threading a
##     single `globalPool`/`globalTags` through every `createTokenBuf`, and
##     expose `pool.strings/syms/files/tags/man/integers/…` as views onto them.
##     nifcore's `Pool.strings/syms/filenames` are the SAME `BiTable[_,string]`
##     type nifstreams used, so `pool.syms.getOrIncl` / `pool.strings[id]` work
##     unchanged.
##
##  2. Line info.  nifstreams stamps a packed `PackedLineInfo` on every token;
##     nifcore keeps a sparse `LineInfoLit` suffix decoded to a `NifLineInfo`
##     struct. We keep `PackedLineInfo` = the real `lineinfos` type (no alias,
##     no collision with direct `lineinfos` importers) and pack/unpack across
##     the boundary via the shared `lineMan`. `.info` is ~99% pass-through, so
##     the round-trip is invisible to callers.
##
##  3. Inline literals.  nifstreams interns int/uint/float into pools referenced
##     by `IntId`/`UIntId`/`FloatId`; nifcore stores them inline. The id becomes
##     a value carrier and `pool.integers[id]` is an identity proxy.
##
## Tier-3 GAPs a shim can't honestly cover are `{.error.}` stubs (raw ParLe/ParRi
## token values, the streaming text reader) — real per-site migrations.

import std / assertions
import nifcore
# Re-export nifcore verbatim except the two helpers whose shim versions below
# thread the global pool. `kind`/`NifKind`/its members are re-exported AS-IS:
# the sem port uses nifcore's own kind model directly (`TagLit`, `StrLit`,
# `hasMore` for end-of-scope) — there is no classic `NifKind` shim, because
# nimony's type-bound lookup makes `nifcore.kind(Cursor)` impossible to shadow.
export nifcore except createTokenBuf, pool, addIdent
# nifcore re-exports bitabs and lineinfos.{FileId,NoFile,isValid}. We need the
# rest of lineinfos too (PackedLineInfo / LineInfoManager / pack / unpack /
# NoLineInfo). PackedLineInfo stays the real lineinfos type — no shadowing.
import lineinfos
export lineinfos
import ".." / models / tags   # TagEnum + TagData (the master tag namespace)

# ── Global pool / tags / line-info manager ───────────────────────────────

var globalTags*: TagPool = createTags[TagEnum]()
var lineMan*: LineInfoManager

# The old global `pool` (a nifstreams `Literals`) IS a nifcore `Pool` here, so
# `pool.strings` / `pool.syms` / `pool.filenames` resolve to real BiTable fields
# — `pool.strings.getOrIncl`, `.len`, `[id]` all work unchanged. Tags live in a
# separate `TagPool`, and int/float have no pool, so `pool.tags` / `.files` /
# `.integers` / `.man` are accessors (below, after the proxy types).
type Literals* = Pool
var pool*: Pool = newPool()

# ── Type aliases ─────────────────────────────────────────────────────────

type
  PackedToken* = NifToken           ## bare nifcore word
  IntId*   = distinct int64         ## value carriers (nifcore stores inline)
  UIntId*  = distinct uint64
  FloatId* = distinct float64

  ## Identity proxies: the id already carries the value, `[]` returns it.
  IntegersProxy*  = object
  UIntegersProxy* = object
  FloatsProxy*    = object

func `==`*(a, b: IntId): bool {.borrow.}
func `==`*(a, b: UIntId): bool {.borrow.}
func `==`*(a, b: FloatId): bool {.borrow.}

# ── Global-pool views (nifstreams `pool.X` compatibility) ────────────────

template files*(p: Pool): untyped = p.filenames
template tags*(p: Pool): untyped = globalTags.tags
template man*(p: Pool): untyped = lineMan
template integers*(p: Pool): IntegersProxy = IntegersProxy()
template uintegers*(p: Pool): UIntegersProxy = UIntegersProxy()
template floats*(p: Pool): FloatsProxy = FloatsProxy()

template `[]`*(x: IntegersProxy; id: IntId): int64 = int64(id)
template `[]`*(x: UIntegersProxy; id: UIntId): uint64 = uint64(id)
template `[]`*(x: FloatsProxy; id: FloatId): float64 = float64(id)

# nifcore stores integers inline: the "id" is the value itself (identity proxy).
template getOrIncl*(x: IntegersProxy; v: int64): IntId = IntId(v)
template getOrIncl*(x: UIntegersProxy; v: uint64): UIntId = UIntId(v)

# ── Buffer construction: thread the global pool + tags ───────────────────

proc createTokenBuf*(cap = 16): TokenBuf =
  ## Every shim buffer shares the one global literals + tag namespace, so ids
  ## are comparable across buffers (the nifstreams global-`pool` invariant).
  nifcore.createTokenBuf(cap, sharedPool = pool, sharedTags = globalTags)

proc registerTag*(tag: string): TagId = registerTag(globalTags, tag)

const ErrT* = TagId(ord(ErrTagId))  ## the `(err …)` tag id (classic nifstreams.ErrT)

proc typebits*(n: NifToken): int {.inline.} =
  ## Bit width stored in an int-literal type token (classic nifstreams had this).
  ## nifcore keeps small ints inline, so the value is the signed operand.
  if n.kind == IntLit: int(n.soperand) else: 0

# ── Line info bridge (PackedLineInfo <-> nifcore NifLineInfo) ─────────────

proc toPacked(li: NifLineInfo): PackedLineInfo =
  if li.file.isValid: pack(lineMan, li.file, li.line, li.col)
  else: NoLineInfo

proc info*(c: Cursor): PackedLineInfo {.inline.} = toPacked(rawLineInfo(c))
proc endInfo*(c: Cursor): PackedLineInfo {.inline.} = toPacked(rawLineInfo(c))

proc emitInfo(dest: var TokenBuf; info: PackedLineInfo) =
  if info.isValid:
    let u = unpack(lineMan, info)
    appendLineInfo(dest, u.file, u.line, u.col)

proc addStrLit*(dest: var TokenBuf; s: StrId; info: PackedLineInfo) =
  nifcore.addStrLit(dest, pool.strings[s]); emitInfo(dest, info)
proc addStrLit*(dest: var TokenBuf; s: string; info: PackedLineInfo) =
  nifcore.addStrLit(dest, s); emitInfo(dest, info)

# ── End-of-scope / node predicates ───────────────────────────────────────
#
# nifcore has no ParLe/ParRi kind: a node is a `TagLit`, and a scope's end is
# `rem == 0` (`hasMore` false). `nifcore.kind` ASSERTS at rem == 0, so the sem
# port replaces the classic sentinels with these safe predicates:
#   `n.kind == ParLe`  → `n.isTagLit`      (also false at scope end / on atoms)
#   `n.kind != ParLe`  → `not n.isTagLit`
#   `n.kind == ParRi`  → `not n.hasMore`
#   `n.kind != ParRi`  → `n.hasMore`
# Inside a scope, `n.kind` (nifcore's) is safe and callers compare to `TagLit`,
# `StrLit`, `DotToken`, … directly.

proc isTagLit*(c: Cursor): bool {.inline.} =
  ## True when the cursor is inside its scope and sits on a tag (compound) node
  ## — the nifcore-safe replacement for the classic `n.kind == ParLe`.
  hasMore(c) and load(c).kind == TagLit
proc isDotToken*(c: Cursor): bool {.inline.} = hasMore(c) and load(c).kind == DotToken
proc isIdent*(c: Cursor): bool {.inline.} = hasMore(c) and load(c).kind == Ident
proc isSymbol*(c: Cursor): bool {.inline.} = hasMore(c) and load(c).kind == Symbol
proc isSymbolDef*(c: Cursor): bool {.inline.} = hasMore(c) and load(c).kind == SymbolDef
proc isStringLit*(c: Cursor): bool {.inline.} = hasMore(c) and load(c).kind == StrLit
proc isIntLit*(c: Cursor): bool {.inline.} = hasMore(c) and load(c).kind == IntLit
proc isUIntLit*(c: Cursor): bool {.inline.} = hasMore(c) and load(c).kind == UIntLit
proc isFloatLit*(c: Cursor): bool {.inline.} = hasMore(c) and load(c).kind == FloatLit
proc isCharLit*(c: Cursor): bool {.inline.} = hasMore(c) and load(c).kind == CharLit
  ## Build-agnostic token predicates (also defined in nifcursors_classic): the
  ## sem port uses these instead of `n.kind == ParLe/DotToken/…` so one source
  ## compiles under both the classic and the `-d:useNifcore` (nifcore) builds.

const
  OpenTagKind* = TagLit   ## build-agnostic `case n.kind` label for a tag head
  StrLitKind* = StrLit     ## build-agnostic `case n.kind` label for a string lit
  # Classic-only sentinels that never occur as a nifcore cursor head; mapped to
  # nifcore-internal suffix kinds purely so `case n.kind of …` stays exhaustive.
  # (`ParRi` is not aliased — those branches are dropped, `hasMore` catches the
  # close in both builds.)
  UnknownTokenKind* = ExtendedSuffix
  EofTokenKind* = LineInfoLit

# ── Raw-token (NifToken) accessors for the CF-listing / raw index walks ───
# Best-effort for pool-ref tokens (nifcore may inline short strings/syms; the
# control-flow buffer uses pool refs). `InlineInt`/`int28Token`/`getInt28`
# re-encode the CF's 28-bit goto operand onto nifcore's `ExtendedSuffix` kind
# (the classic CF repurposed `UnknownToken`; nifcore has no such spare atom).
# NB: an `ExtendedSuffix` in a buffer that is later CURSOR-walked would be read
# as a suffix of the preceding token — the CF buffer is only raw-index walked,
# so this is safe there, but revisit if the CF stream ever gets cursor-walked.
const InlineInt* = ExtendedSuffix

proc int28Token*(operand: int32; info: PackedLineInfo): NifToken {.inline.} =
  extendedSuffixToken(cast[uint32](operand))
proc getInt28*(n: NifToken): int32 {.inline.} = n.soperand
proc getInt28*(c: Cursor): int32 {.inline.} = load(c).soperand

proc symId*(n: NifToken): SymId {.inline.} = SymId(nifcore.uoperand(n) shr 1)
proc litId*(n: NifToken): StrId {.inline.} = StrId(nifcore.uoperand(n) shr 1)
proc intId*(n: NifToken): IntId {.inline.} = IntId(n.soperand)
proc uintId*(n: NifToken): UIntId {.inline.} = UIntId(nifcore.uoperand(n))
proc floatId*(n: NifToken): FloatId {.inline.} = FloatId(0)
proc charLit*(n: NifToken): char {.inline.} = char(nifcore.uoperand(n) and 0xFF)
proc tagId*(n: NifToken): TagId {.inline.} = TagId((uint32(n) shr TagShift) and TagMask)

proc skipUntilEnd*(c: var Cursor) =
  ## Skip a bounded cursor's remaining children (stops at the virtual close).
  while hasMore(c): skip c

proc isLastSon*(n: Cursor): bool =
  ## True if `n` is the last child in its bounded scope.
  var c = n
  skip c
  result = not hasMore(c)

proc patchInt28Token*(n: var NifToken; operand: int32) {.inline.} =
  n = extendedSuffixToken(cast[uint32](operand))

proc charToken*(ch: char; info: PackedLineInfo): NifToken {.inline.} = charToken(ch)
  ## Info is dropped (a single nifcore token carries none); prefer `addCharLit`.
proc strToken*(s: StrId; info: PackedLineInfo): NifToken {.inline.} = strLitToken(s)
  ## Info dropped; prefer `addStrLit`.

proc setSymId*(dest: var NifToken; sym: SymId) {.inline.} =
  ## Rewrite a Symbol/SymbolDef token's id in place (kind preserved).
  dest = (if dest.kind == SymbolDef: symdefToken(sym) else: symToken(sym))
proc copyKeepLineInfo*(dest: var NifToken; src: NifToken) {.inline.} = dest = src
  ## nifcore keeps line info in a separate suffix, so there is nothing to keep.
proc replaceWithOpenTag*(dest: var TokenBuf; tag: NifToken; pos: int) {.inline.} =
  dest[pos] = tag

# Standalone atom builders with a line-info argument (dropped: nifcore keeps
# line info in a separate suffix that a bare token cannot carry).
proc identToken*(id: StrId; info: PackedLineInfo): NifToken {.inline.} = identToken(id)
proc dotToken*(info: PackedLineInfo): NifToken {.inline.} = dotToken()
proc symToken*(id: SymId; info: PackedLineInfo): NifToken {.inline.} = symToken(id)

# `buildTree` with an explicit info argument (classic signature). nifcore's own
# `buildTree` takes no info; these thread it through `addParLe`.
template buildTree*(dest: var TokenBuf; tag: TagId; info: PackedLineInfo; body: untyped) =
  addParLe(dest, tag, info)
  body
  addParRi(dest)
template buildTree*[T: enum](dest: var TokenBuf; tag: T; info: PackedLineInfo; body: untyped) =
  addParLe(dest, cast[TagId](uint32(ord(tag))), info)
  body
  addParRi(dest)

proc withLineInfo*(n: NifToken; info: PackedLineInfo): NifToken {.inline.} = n
  ## Classic stamped `info` into the token; nifcore keeps line info in a separate
  ## suffix, so an in-place token rewrite cannot carry it — returned unchanged.

proc span*(c: Cursor): int {.inline.} = subtreeWidth(c)
proc firstSon*(n: Cursor): Cursor {.inline.} = childCursor(n)

proc widenSealed*(dest: var TokenBuf; enclosing: int; growth: int) =
  ## After an insert/replace grew a *sealed* scope's contents, widen its jump.
  ## Handles only the in-field (non-overflow) case; asserts otherwise.
  if growth <= 0: return
  var tok = dest[enclosing]
  let cur = uint32(tok) shr JumpShift
  setJump(tok, cur + uint32(growth))
  dest[enclosing] = tok

proc widenEnclosingSealed*(dest: var TokenBuf; pos, growth: int) =
  ## Widen the jump of EVERY sealed scope enclosing `pos` after inserting
  ## `growth` tokens there (classic-name mirror; see nifcursors_classic).
  if growth <= 0: return
  var i = 0
  while i < pos:
    let c = readonlyCursorAt(dest, i)
    if c.kind == TagLit:
      let total = span(c)          # head + suffix + body
      if i + total > pos:          # this scope encloses the insertion point
        widenSealed(dest, i, growth)
        i += tokenWidth(c)         # descend into the body
      else:
        i += total                 # sealed subtree entirely before `pos`
    else:
      inc i

proc cursorToPosition*(base: Cursor; c: Cursor): int {.inline.} =
  ## Token distance of `c` from `base` (classic took two cursors; nifcore's
  ## same-named proc takes a TokenBuf, hence this overload).
  (toUniqueId(c) - toUniqueId(base)) div sizeof(NifToken)

proc isTagLit*(n: NifToken): bool {.inline.} = n.kind == TagLit
proc isDotToken*(n: NifToken): bool {.inline.} = n.kind == DotToken
proc isIdent*(n: NifToken): bool {.inline.} = n.kind == Ident
proc isSymbol*(n: NifToken): bool {.inline.} = n.kind == Symbol
proc isSymbolDef*(n: NifToken): bool {.inline.} = n.kind == SymbolDef
proc isStringLit*(n: NifToken): bool {.inline.} = n.kind == StrLit
proc isIntLit*(n: NifToken): bool {.inline.} = n.kind == IntLit
proc isUIntLit*(n: NifToken): bool {.inline.} = n.kind == UIntLit
proc isFloatLit*(n: NifToken): bool {.inline.} = n.kind == FloatLit
proc isCharLit*(n: NifToken): bool {.inline.} = n.kind == CharLit

proc tag*(n: NifToken): TagId {.inline.} =
  ## Tag of a raw head token (classic nifstreams had `tag(PackedToken)`).
  TagId((uint32(n) shr TagShift) and TagMask)

# ── Cursor atom accessors (renames) ──────────────────────────────────────

proc tag*(c: Cursor): TagId {.inline.} = cursorTagId(c)
proc tagId*(c: Cursor): TagId {.inline.} = cursorTagId(c)
proc litId*(c: Cursor): StrId {.inline.} = strId(c)
proc jump*(c: Cursor): uint64 {.inline.} = cursorJump(c)

proc intId*(c: Cursor): IntId {.inline.} = IntId(intVal(c))
proc uintId*(c: Cursor): UIntId {.inline.} = UIntId(uintVal(c))
proc floatId*(c: Cursor): FloatId {.inline.} = FloatId(floatVal(c))

# ── Structural navigation ────────────────────────────────────────────────

# ── TokenBuf building: openTag/closeTag bridge with line info ─────────────

proc addParLe*(dest: var TokenBuf; tag: TagId; info = NoLineInfo) =
  openTag(dest, tag)
  emitInfo(dest, info)

proc addParRi*(dest: var TokenBuf) = closeTag(dest)
proc addParRi*(dest: var TokenBuf; info: PackedLineInfo) = closeTag(dest)

proc isUnknownToken*(c: Cursor): bool {.inline.} = hasMore(c) and load(c).kind == UnknownTokenKind
proc isUnknownToken*(n: NifToken): bool {.inline.} = n.kind == UnknownTokenKind

proc addUnstructured*(dest: var TokenBuf; c: Cursor) =
  ## Copy the remaining forest under `c` verbatim, preserving each subtree's
  ## suffixes (line info). Handles a sequence of top-level trees/atoms.
  var c = c
  while hasMore(c):
    dest.addSubtree c
    skip c

proc insert*(dest: var TokenBuf; src: Cursor; pos: int) =
  ## Insert the single subtree at `src` into `dest` at token position `pos`.
  var tmp = createTokenBuf(span(src) + 2)
  tmp.addSubtree src
  dest.insert tmp, pos
  ## nifcore has no close-token info; the arg is accepted for compatibility.

# ── Atom builders carrying line info (classic `dest.add xToken(v, info)`) ──
# nifcore builds an atom then attaches its line info as a trailing suffix, so
# these are add-then-`emitInfo`, not a single info-bearing token.

proc addSymUse*(dest: var TokenBuf; s: SymId; info: PackedLineInfo) =
  nifcore.addSymUse(dest, s); emitInfo(dest, info)
proc addSymDef*(dest: var TokenBuf; s: SymId; info: PackedLineInfo) =
  nifcore.addSymDef(dest, s); emitInfo(dest, info)
proc addDotToken*(dest: var TokenBuf; info: PackedLineInfo) =
  nifcore.addDotToken(dest); emitInfo(dest, info)
proc addIdent*(dest: var TokenBuf; s: StrId; info: PackedLineInfo) =
  nifcore.addIdent(dest, pool.strings[s]); emitInfo(dest, info)
proc addIdent*(dest: var TokenBuf; s: string; info = NoLineInfo) =
  nifcore.addIdent(dest, s); emitInfo(dest, info)
proc addIntLit*(dest: var TokenBuf; v: int64; info: PackedLineInfo) =
  nifcore.addIntLit(dest, v); emitInfo(dest, info)
proc addIntLit*(dest: var TokenBuf; id: IntId; info: PackedLineInfo) =
  addIntLit(dest, pool.integers[id], info)
proc addUIntLit*(dest: var TokenBuf; v: uint64; info: PackedLineInfo) =
  nifcore.addUIntLit(dest, v); emitInfo(dest, info)
proc addFloatLit*(dest: var TokenBuf; v: float64; info: PackedLineInfo) =
  nifcore.addFloatLit(dest, v); emitInfo(dest, info)
proc addCharLit*(dest: var TokenBuf; v: char; info: PackedLineInfo) =
  nifcore.addCharLit(dest, v); emitInfo(dest, info)

proc add*(dest: var TokenBuf; src: TokenBuf) {.inline.} =
  ## Append a whole buffer (classic `dest.add someTokenBuf`).
  addBufferSamePool(dest, src)

from nifcoreparse import nil
from nifreader import nil
from vfs import FileWriteMode, AlwaysWrite, OnlyIfChanged, vfsRead, vfsWrite

# Custom pragmas don't cross module boundaries, so re-declare `canRaise` locally
# (mirrors compat2.nim).
when defined(nimony):
  {.pragma: canRaise, raises.}
else:
  {.pragma: canRaise.}

proc writeFile*(b: var TokenBuf; filename: string; mode: FileWriteMode = AlwaysWrite) {.canRaise.} =
  ## Serialize the buffer to a textual `.nif` module file (nifcore renderer).
  let content = nifcoreparse.toModuleString(b, "." & nifreader.extractModuleSuffix(filename))
  if mode == OnlyIfChanged:
    let existingContent = try: vfsRead(filename) except: ""
    if existingContent == content: return
  vfsWrite(filename, content)

proc parseFromFile*(filename: string; sizeHint = 100): TokenBuf =
  ## Whole-file read (classic nifcursors.parseFromFile) via the nifcore reader.
  result = createTokenBuf(sizeHint)
  var r = nifreader.open(filename)
  nifcoreparse.parse(r, result)
  nifreader.close(r)

proc parseFromBuffer*(input: string; thisModule: sink string; sizeHint = 100): TokenBuf =
  ## Parse NIF text into a buffer sharing the global pool/tags (shim invariant).
  nifcoreparse.parseFromBuffer(input, thisModule, sizeHint,
                               sharedPool = pool, sharedTags = globalTags)
proc toString*(c: Cursor; produceLineInfo = false): string {.inline.} =
  ## Classic `toString(Cursor, produceLineInfo)`; nifcore's is keyword-arg based.
  nifcoreparse.toString(c, includeLineInfo = produceLineInfo)
proc toString*(b: TokenBuf; produceLineInfo = false): string {.inline.} =
  ## Read-only render (the nifcore renderer wants `var` for its cursor, but only
  ## reads); safe to alias an immutable buffer. Accepts both `var` and immutable.
  nifcoreparse.toString(cast[ptr TokenBuf](unsafeAddr b)[], includeLineInfo = produceLineInfo)

proc addRaw*(b: var TokenBuf; t: NifToken) {.inline.} = nifcore.add(b, t)
  ## Append a token verbatim (classic distinguished raw appends from sealing
  ## ones; nifcore's `add` never re-seals, so it IS the raw append).

template linearScan*(n: var Cursor; body: untyped) =
  ## Pre-order visit of every tag (`TagLit`) node strictly inside `n`'s subtree,
  ## `n` positioned at each; `body` may `break` (leaving `n` at the match) and
  ## must not advance `n`. `inc` gives pre-order over the flat token stream
  ## (enters tags, steps atoms; suffixes are skipped by `tokenWidth`).
  if n.isTagLit:
    discard enterScope(n)   # bound to the children; n now at the first child
    while hasMore(n):
      if n.isTagLit:
        body
      inc n

proc endRead*(b: var TokenBuf) {.inline.} = discard
  ## Classic released a buffer read-lock here; nifcore cursors are refcounted
  ## owners, so ending a read on the *buffer* is a no-op (only `endRead(Cursor)`
  ## exists in nifcore, and it is re-exported for cursor readers).

template copyInto*(dest: var TokenBuf; tag: TagId; info: PackedLineInfo; body: untyped) =
  addParLe(dest, tag, info)
  body
  closeTag(dest)

template copyIntoUnchecked*(dest: var TokenBuf; tag: string; info: PackedLineInfo; body: untyped) =
  addParLe(dest, pool.tags.getOrIncl(tag), info)
  body
  closeTag(dest)

proc shrink*(b: var TokenBuf; newLen: int) =
  ## Truncate to `newLen`. For a shrink (newLen <= len <= cap) `growRawUninit`
  ## never reallocates, so the existing prefix words are preserved.
  discard growRawUninit(b, newLen)

proc freeze*(b: var TokenBuf) {.inline.} = discard  ## CursorOwner refcounts; no-op
proc thaw*(b: var TokenBuf) {.inline.} = discard

# ── Subtree / token moves ────────────────────────────────────────────────

proc takeTree*(dest: var TokenBuf; n: var Cursor) =
  dest.addSubtree n
  skip n

proc takeToken*(dest: var TokenBuf; n: var Cursor) =
  # A lone TagLit head has no nifcore analogue (subtrees are atomic here); no
  # current caller does this, but assert to catch a regression.
  assert load(n).kind != TagLit
  dest.addSubtree n
  skip n

# ── Skip intents / structural helpers ────────────────────────────────────

type
  SkipIntent* = enum
    SkipTag       ## advance past a ParLe tag (entering a node to rewrite children)
    SkipParRi     ## advance past a closing paren
    SkipName      ## skip a name/SymbolDef child
    SkipExport    ## skip an export marker child
    SkipPragmas   ## skip a pragmas section
    SkipType      ## skip a type child
    SkipExpr      ## skip an expression child
    SkipStmt      ## skip a statement child
    SkipValue     ## skip a value/expression child
    SkipGenParams ## skip generic parameters
    SkipCond      ## skip a condition expression
    SkipBody      ## skip a body/stmts section
    SkipEffects   ## skip an effects section
    SkipResult    ## skip a result that has been handled separately
    SkipFull      ## skip an entire subtree being dropped or replaced

template skip*(c: var Cursor; intent: SkipIntent) =
  ## The intent is documentation only in the nifcore port (the classic runtime
  ## predicate depended on the ParLe/ParRi model). `skip` advances one subtree.
  skip(c)

template inc*(c: var Cursor; intent: SkipIntent) =
  ## As `skip(c, intent)` but advances past the head only (`inc`).
  inc c

type
  TagClass* = enum
    Anything, AnyExpr, AnyStmt, AnyType

template skip*(c: var Cursor; expected: TagClass) = skip(c)
  ## Categorical skip intent — documentation only in the nifcore port.
template inc*(c: var Cursor; expected: TagClass) = inc c

proc consumeParRi*(c: var Cursor) {.inline.} =
  ## nifcore never materialises a close token — the scope end is implicit at
  ## `rem == 0`. There is nothing to advance past; this exists so classic
  ## `skipParRi` sites compile and stay no-ops.
  assert not hasMore(c), "consumeParRi: cursor not at a (virtual) ParRi"

proc uoperand*(c: Cursor): uint32 {.inline.} = nifcore.uoperand(c.load)
proc soperand*(c: Cursor): int32 {.inline.} = nifcore.soperand(c.load)

# ═════════════════════════════════════════════════════════════════════════
# Tier 3 GAPs — real per-site migration (doc/nifcore_shim.md).
# ═════════════════════════════════════════════════════════════════════════

proc parLeToken*(tag: TagId; info = NoLineInfo): NifToken {.error:
  "nifcore ParLe must be sealed: replace `dest.add parLeToken(t, i)` with `dest.addParLe(t, i)`".}
proc parRiToken*(info = NoLineInfo): NifToken {.error:
  "nifcore ParRi is implicit: replace `dest.add parRiToken()` with `dest.addParRi()`".}

# GAP: streaming text reader — Stream, open/openFromBuffer, close, next, parse,
# parents/parent. nifcore reads via nifcoreparse.nim (separate handoff).
