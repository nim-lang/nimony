#       Nif library
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## bif — **B**inary N**IF**: direct binary load/store of a `nifcore.TokenBuf`.
##
## NIF on disk is a *textual* s-expression format: great for tooling, diffing and
## bootstrapping, but for a compiler **cache** the text is pure overhead. Loading
## a text NIF means a full tokenizer/parser pass, and the text is several times
## larger than the in-memory token stream — e.g. a Nimbus module measured at
## 890 MiB of text collapses to a 176 MiB `TokenBuf` (0.20×), of which 99.5 % is
## the flat `uint32` token array and < 1 MiB is the (already deduped) string
## pools. `bif` writes exactly that in-memory representation to disk and reads it
## straight back, so a cache load is one `readBuffer` of the token block plus a
## quick re-intern of the small pools — no parsing.
##
## Layout (all integers host-endian — a `.bif` file is a machine-local cache and
## is never shared between hosts; the `Magic` carries the word size + a version
## so a mismatched file is rejected rather than misread):
##
## ```
##   Magic            8 bytes  ("NIFBIN" + sizeof(int) + Version)
##   indexOffset      u32      -- BYTE offset of the index section (see below)
##   tokenCount       u32
##   nTags            u32
##   nStrings         u32
##   nSyms            u32
##   nFiles           u32
##   tokens           tokenCount * sizeof(NifToken)   -- one raw block
##   tags             nTags    * (u32 len + bytes)
##   strings          nStrings * (u32 len + bytes)
##   syms             nSyms    * (u32 len + bytes)
##   filenames        nFiles   * (u32 len + bytes)
##   index (@indexOffset)
##     nIndex         u32
##     entries        nIndex   * (u32 symId, i32 tokenPos, u8 vis)
## ```
##
## The `index` is the binary analogue of a text NIF's embedded `(.index …)`:
## global `SymbolDef` -> declaration token position + visibility, so a loaded
## buffer can locate one symbol without scanning. It is ALWAYS present (a cache
## with no index would force a full rescan to find any declaration) and lives at
## the **end** of the file, exactly like the text format's trailing index. The
## single `indexOffset` header field is the binary `(.indexat …)`: the one `u32`
## a reader follows to jump straight to the index. Because the offset is only
## known once the token block and pools have been written, `storeToFile` writes
## a placeholder, streams everything, then patches the slot — mirroring the text
## writer's `addHeader27` reservation + `patchIndexAt`. The index entries
## themselves are gathered in the single forward token traversal `buildIndex`,
## the way the text writer accumulates `(.index …)` entries while emitting tokens.
##
## The token block is stored verbatim. Every pool-referencing token embeds a
## *pool id* — `StrLit`/`Symbol`/`SymbolDef`/`Ident` (strings/syms id),
## `LineInfoLit` (a `FileId`) and `TagLit` (a 9-bit tag id) — and `nifcore`
## assigns those ids 1, 2, … in intern order. The pools are written in that same
## id order and re-interned in order on load, reproducing identical ids, so the
## raw token words stay valid **without any patching**.
##
## INVARIANT (important): that zero-patch property holds *only* because the load
## interns into **empty, freshly minted** pools, so the first `getOrIncl` returns
## id 1 again. `bif` therefore never loads into a shared/pre-populated pool: doing
## so would assign different ids to the same strings while the token words still
## carry the old ids — silent corruption. Cross-module pool sharing would require
## remapping every pool-referencing token, which defeats the whole point. If you
## need a buffer to share a pool with others, load it with `bif` first and then
## copy across pools via `nifcore.addSubtree` (which re-interns properly).

import std / [syncio, assertions, strutils]
import nifcore
import vfs

const
  Version = 3'u8
  MagicLen = 8

# ── embedded index ──────────────────────────────────────────────────────────
# A text NIF carries a trailing `(.index …)` mapping every *global* `SymbolDef`
# to the BYTE offset of its declaration, so a reader can seek straight to one
# symbol without scanning. Byte offsets are meaningless in the binary token
# world, so `bif` persists its OWN index: each global symbol's SymId, the TOKEN
# POSITION of its declaration's enclosing tag, and its visibility. This is what
# lets a `bif`-loaded buffer back an on-demand symbol loader (no full scan).

type
  IndexVis* = enum
    ivHidden,    ## not importable as a bare identifier (the `.` marker)
    ivExported   ## importable (`x` marker)
  IndexEntry* = object
    sym*: SymId    ## the global symbol (resolve its name via `buf.pool.syms`)
    pos*: int32    ## token index of the declaration's enclosing tag
    vis*: IndexVis
  BifModule* = object
    ## A loaded `.bif`: the token buffer plus its symbol index.
    buf*: TokenBuf
    index*: seq[IndexEntry]

proc isGlobalSymbol(s, dottedSuffix: string): bool =
  ## Mirror of `nifbuilder.addSymbolDefRetIsGlobal`: a symbol is "global" (gets
  ## an index entry) when its name — with a self-module `dottedSuffix` compressed
  ## to a single trailing dot — has >= 2 dots (`addSymbolImpl` counts dots from
  ## index 1).
  var lim = s.len
  if dottedSuffix.len > 0 and s.endsWith(dottedSuffix):
    lim = s.len - dottedSuffix.len + 1
  if lim > s.len: lim = s.len
  var dots = 0
  for i in 1 ..< lim:
    if s[i] == '.': inc dots
  dots >= 2

proc buildIndex*(b: var TokenBuf; dottedSuffix = ""): seq[IndexEntry] =
  ## Scan `b` for global `SymbolDef`s and record, for each, its SymId, the token
  ## position of the most-recently-opened tag (the declaration's `(`, mirroring
  ## `nifcoreparse.toModuleString`'s `mostRecentOffset`), and its visibility
  ## (hidden when the marker after the def is a `DotToken`, else exported).
  result = @[]
  if b.len == 0: return
  var c = b.beginRead()
  var mostRecentTagPos = 0'i32
  while c.hasMore:
    case c.kind
    of TagLit:
      mostRecentTagPos = int32 cursorToPosition(b, c)
      inc c                          # descend into the body (visit every token)
    of SymbolDef:
      let isG = isGlobalSymbol(symName(c), dottedSuffix)
      let sid = c.symId
      let tagPos = mostRecentTagPos
      inc c                          # advance to the marker / next sibling
      if isG:
        let vis = if c.hasMore and c.kind == DotToken: ivHidden else: ivExported
        result.add IndexEntry(sym: sid, pos: tagPos, vis: vis)
    else:
      inc c
  c.endRead()

proc magic(): array[MagicLen, char] =
  result = ['N', 'I', 'F', 'B', 'I', 'N', char(sizeof(int)), char(Version)]

proc writeU32(f: File; x: uint32) =
  var v = x
  assert f.writeBuffer(addr v, 4) == 4

proc readU32(f: File): uint32 =
  var v = 0'u32
  assert f.readBuffer(addr v, 4) == 4
  v

proc writeStr(f: File; s: string) =
  writeU32(f, uint32 s.len)
  if s.len > 0:
    assert f.writeBuffer(unsafeAddr s[0], s.len) == s.len

proc readStr(f: File): string =
  let n = int readU32(f)
  result = newString(n)
  if n > 0:
    assert f.readBuffer(addr result[0], n) == n

# ── store ─────────────────────────────────────────────────────────────────

proc storeToFile*(b: var TokenBuf; f: File; dottedSuffix = "") =
  ## Write `b` (token stream + pools + symbol index) to an already-open binary
  ## file. The pools are written whole, in id order. If `b` shares a pool with
  ## other buffers, the *entire* shared pool is written (still correct — ids stay
  ## dense and load reproduces them — but larger than this module needs). For a
  ## compact, self-contained cache file, build the buffer against its own private
  ## pool. `dottedSuffix` is the self-module suffix used to decide which symbols
  ## are global (it gets the same compression as the text writer).
  # The index is built in a single forward traversal of the token stream and is
  # always emitted — see the module doc.
  let index = buildIndex(b, dottedSuffix)
  var m = magic()
  assert f.writeBuffer(addr m[0], MagicLen) == MagicLen
  # Reserve the `indexOffset` slot. We can only fill it once tokens and pools are
  # written, so write a placeholder now and patch it at the end — the binary
  # equivalent of `addHeader27`'s `(.indexat …)` reservation + `patchIndexAt`.
  let indexAtPos = getFilePos(f)
  writeU32(f, 0'u32)                   # placeholder for indexOffset
  writeU32(f, uint32 b.len)
  writeU32(f, uint32 b.tags.tags.len)
  writeU32(f, uint32 b.pool.strings.len)
  writeU32(f, uint32 b.pool.syms.len)
  writeU32(f, uint32 b.pool.filenames.len)
  # token stream: one contiguous block.
  let bytes = b.len * sizeof(NifToken)
  if bytes > 0:
    assert f.writeBuffer(b.rawTokenPtr, bytes) == bytes
  # pools, each in id order (ids are 1-based, dense up to len).
  for i in 1 .. b.tags.tags.len:      writeStr(f, b.tags.tags[TagId(i)])
  for i in 1 .. b.pool.strings.len:   writeStr(f, b.pool.strings[StrId(i)])
  for i in 1 .. b.pool.syms.len:      writeStr(f, b.pool.syms[SymId(i)])
  for i in 1 .. b.pool.filenames.len: writeStr(f, b.pool.filenames[FileId(i)])
  # symbol index — self-contained at the offset we now know.
  let indexOffset = getFilePos(f)
  assert indexOffset <= int64(high(uint32)), "bif cache exceeds 4 GiB"
  writeU32(f, uint32 index.len)
  for e in index:
    writeU32(f, uint32 e.sym)
    writeU32(f, cast[uint32](e.pos))
    var v = uint8(ord(e.vis))
    assert f.writeBuffer(addr v, 1) == 1
  # Patch the header slot to point at the index, then leave the cursor at EOF.
  setFilePos(f, indexAtPos)
  writeU32(f, uint32 indexOffset)
  setFilePos(f, 0, fspEnd)

proc store*(b: var TokenBuf; filename: string; dottedSuffix = "") =
  ## Write `b` to `filename` in binary `.bif` form (with its symbol index).
  var f = open(filename, fmWrite)
  storeToFile(b, f, dottedSuffix)
  close(f)

# ── load ──────────────────────────────────────────────────────────────────

proc checkMagic(f: File) =
  var m = default(array[MagicLen, char])
  let want = magic()
  assert f.readBuffer(addr m[0], MagicLen) == MagicLen
  for i in 0 ..< MagicLen:
    if m[i] != want[i]:
      quit "bif: bad magic / incompatible format or word size"

proc readIndex(f: File): seq[IndexEntry] =
  ## Read the self-contained index section (`nIndex` followed by the entries),
  ## assuming `f` is already positioned at its start.
  let nIndex = int readU32(f)
  result = newSeq[IndexEntry](nIndex)
  for i in 0 ..< nIndex:
    let sym = SymId readU32(f)
    let pos = cast[int32](readU32(f))
    var v = 0'u8
    assert f.readBuffer(addr v, 1) == 1
    result[i] = IndexEntry(sym: sym, pos: pos, vis: IndexVis(v))

proc loadIndexFromFile*(f: File): seq[IndexEntry] =
  ## Read ONLY the symbol index, by following the header's `indexOffset` and
  ## seeking straight to the trailing index section — the token block and pools
  ## are never touched. This is the binary analogue of `nifindexes` jumping to a
  ## text NIF's `(.indexat …)` target. NOTE: each entry's `sym` is a `SymId`
  ## that is only meaningful together with the buffer's `syms` pool, so a fully
  ## standalone consumer must also load that pool (or use the names another way).
  checkMagic(f)
  let indexOffset = readU32(f)
  setFilePos(f, int64 indexOffset)
  result = readIndex(f)

proc loadIndex*(filename: string): seq[IndexEntry] =
  ## Read just the symbol index of a `.bif` file (see `loadIndexFromFile`).
  var f = open(filename, fmRead)
  result = loadIndexFromFile(f)
  close(f)

proc loadFromFile*(f: File): BifModule =
  ## Read a `BifModule` (token buffer + symbol index) from an already-open binary
  ## file. Always mints FRESH pools — see the INVARIANT in the module doc: the
  ## raw token words embed pool ids that are only reproduced by interning into
  ## empty pools from id 1, so loading into a shared/pre-populated pool is
  ## deliberately not supported. Quits on a magic/version mismatch.
  result = BifModule()
  checkMagic(f)
  discard readU32(f)               # indexOffset — only `loadIndex*` follows it;
                                   # a full load reaches the index sequentially.
  let tokenCount = int readU32(f)
  let nTags      = int readU32(f)
  let nStrings   = int readU32(f)
  let nSyms      = int readU32(f)
  let nFiles     = int readU32(f)

  result.buf = createTokenBuf(max(tokenCount, 16))   # fresh pools — see INVARIANT
  # token stream: read straight into the storage in one block.
  if tokenCount > 0:
    let dest = result.buf.growRawUninit(tokenCount)
    let bytes = tokenCount * sizeof(NifToken)
    assert f.readBuffer(dest, bytes) == bytes
  # pools: re-intern in stored (id) order so ids 1,2,… match the token refs.
  for _ in 1 .. nTags:    discard result.buf.tags.tags.getOrIncl(readStr(f))
  for _ in 1 .. nStrings: discard result.buf.pool.strings.getOrIncl(readStr(f))
  for _ in 1 .. nSyms:    discard result.buf.pool.syms.getOrIncl(readStr(f))
  for _ in 1 .. nFiles:   discard result.buf.pool.filenames.getOrIncl(readStr(f))
  # symbol index (we are now positioned exactly at indexOffset).
  result.index = readIndex(f)

# ── mmap-backed load (zero-copy token block) ────────────────────────────────
# `load` maps the whole `.bif` with `vfs.vfsOpenMmap` and BORROWS its token block
# straight into the returned `TokenBuf` via `adoptForeignTokens` — no multi-hundred
# -MB `readBuffer` into the heap. The pages are file-backed and read-only, so every
# process that maps the same cache file shares one physical copy (the whole point
# for the parallel IC backend, where dozens of workers reload `system.s.bif` & co).
# Only the small pools + index are copied out, so they don't pin the mapping. The
# mapping is intentionally never unmapped (see `adoptForeignTokens`): a `.bif` load
# feeds a short-lived backend process and is reclaimed wholesale at exit.

type
  BifReader = object
    base: uint     # start of the mapped bytes
    pos: int       # cursor within [0, size)
    size: int

proc rU32(r: var BifReader): uint32 =
  assert r.pos + 4 <= r.size, "bif: truncated header/pool"
  result = cast[ptr uint32](r.base + uint(r.pos))[]
  r.pos += 4

proc rStr(r: var BifReader): string =
  let n = int rU32(r)
  assert r.pos + n <= r.size, "bif: truncated string"
  result = newString(n)
  if n > 0:
    copyMem(addr result[0], cast[pointer](r.base + uint(r.pos)), n)
    r.pos += n

proc load*(filename: string): BifModule =
  ## Read a `BifModule` from a binary `.bif` file, memory-mapping it and BORROWING
  ## the token block (zero-copy — see `adoptForeignTokens`). Mints fresh pools (the
  ## bif INVARIANT). The mapping stays resident for the process lifetime.
  let blob = vfsOpenMmap(filename)          # left mapped for the buffer's lifetime
  var r = BifReader(base: cast[uint](blob.data), pos: 0, size: blob.size)
  block:                                    # magic
    let want = magic()
    assert r.size >= MagicLen, "bif: file too small"
    for i in 0 ..< MagicLen:
      if cast[ptr char](r.base + uint(i))[] != want[i]:
        quit "bif: bad magic / incompatible format or word size"
  r.pos = MagicLen
  discard rU32(r)                  # indexOffset — a full load reaches it linearly
  let tokenCount = int rU32(r)
  let nTags      = int rU32(r)
  let nStrings   = int rU32(r)
  let nSyms      = int rU32(r)
  let nFiles     = int rU32(r)
  # token block starts here (byte 32) and is borrowed straight from the mapping.
  result = BifModule()
  let tokenBytes = tokenCount * sizeof(NifToken)
  assert r.pos + tokenBytes <= r.size, "bif: truncated token block"
  result.buf = adoptForeignTokens(cast[pointer](r.base + uint(r.pos)), tokenCount)
  r.pos += tokenBytes
  # pools: re-intern in stored (id) order so ids 1,2,… match the token refs.
  for _ in 1 .. nTags:    discard result.buf.tags.tags.getOrIncl(rStr(r))
  for _ in 1 .. nStrings: discard result.buf.pool.strings.getOrIncl(rStr(r))
  for _ in 1 .. nSyms:    discard result.buf.pool.syms.getOrIncl(rStr(r))
  for _ in 1 .. nFiles:   discard result.buf.pool.filenames.getOrIncl(rStr(r))
  # symbol index (we are now positioned exactly at indexOffset).
  let nIndex = int rU32(r)
  result.index = newSeq[IndexEntry](nIndex)
  for i in 0 ..< nIndex:
    let sym = SymId rU32(r)
    let pos = cast[int32](rU32(r))
    assert r.pos + 1 <= r.size, "bif: truncated index"
    let v = cast[ptr uint8](r.base + uint(r.pos))[]
    r.pos += 1
    result.index[i] = IndexEntry(sym: sym, pos: pos, vis: IndexVis(v))

# ── self-test ───────────────────────────────────────────────────────────────

when isMainModule:
  proc sameTokens(a, b: TokenBuf): bool =
    if a.len != b.len: return false
    for i in 0 ..< a.len:
      if not (a[i] == b[i]): return false
    true

  block round_trip:
    # Build a buffer exercising every pool + inline/overflow paths, then
    # store → load and require token-identical + pool-identical results.
    var src = createTokenBuf(16)
    let tFoo = src.tags.registerTag("foo")
    let tBar = src.tags.registerTag("bar")
    let f = src.pool.filenames.getOrIncl("some/where.nim")
    src.buildTree tFoo:
      src.appendLineInfo f, 10'i32, 2'i32
      src.buildTree tBar:
        src.addStrLit "hi"                         # inline (<=3)
        src.addStrLit "a longer interned string"   # pool
        src.addSymUse "some.long.symbol.name.0"    # sym pool
        src.addSymDef "another.symbol.def.1"
        src.addIntLit 42
        src.addIntLit 1'i64 shl 40                 # suffix-extended
        src.addUIntLit 0'u64
        src.addFloatLit 3.14
        src.addIdent "ident_name"

    let tmp = "/tmp/bif_selftest.bif"
    store(src, tmp)
    var m = load(tmp)
    doAssert sameTokens(src, m.buf), "token streams differ after round-trip"
    # pools must match value-for-value (so ids line up)
    doAssert m.buf.tags.tags.len == src.tags.tags.len
    doAssert m.buf.pool.strings.len == src.pool.strings.len
    doAssert m.buf.pool.syms.len == src.pool.syms.len
    doAssert m.buf.pool.filenames.len == src.pool.filenames.len
    # spot-check a decoded value via a cursor
    var c = m.buf.beginRead()
    doAssert c.kind == TagLit
    doAssert m.buf.tags.tagName(c.cursorTagId) == "foo"

  block embedded_index:
    # A module-shaped buffer: a global exported def, a global hidden def, and a
    # purely-local def (< 2 dots) which must NOT be indexed.
    var src = createTokenBuf(16)
    let tStmts = src.tags.registerTag("stmts")
    let tSdef = src.tags.registerTag("sdef")
    src.buildTree tStmts:
      src.buildTree tSdef:                 # exported global: `x` marker
        src.addSymDef "foo.3.mymod"
        src.addIdent "x"
      src.buildTree tSdef:                 # hidden global: `.` marker
        src.addSymDef "bar.4.mymod"
        src.addDotToken()
      src.buildTree tSdef:                 # local (1 dot) -> not indexed
        src.addSymDef "loc.0"
        src.addIdent "x"

    let tmp = "/tmp/bif_index_selftest.bif"
    store(src, tmp, dottedSuffix = ".mymod")
    var m = load(tmp)
    doAssert m.index.len == 2, "expected 2 global syms, got " & $m.index.len
    # entry 0: foo, exported; its pos points at the enclosing (sdef tag.
    let e0 = m.index[0]
    doAssert m.buf.pool.syms[e0.sym] == "foo.3.mymod"
    doAssert e0.vis == ivExported
    var c = cursorAt(m.buf, int e0.pos)
    doAssert c.kind == TagLit and m.buf.tags.tagName(c.cursorTagId) == "sdef"
    # entry 1: bar, hidden.
    doAssert m.buf.pool.syms[m.index[1].sym] == "bar.4.mymod"
    doAssert m.index[1].vis == ivHidden

    # jump-only load: following indexOffset must yield exactly the same entries
    # the full load produced, without touching the token block or pools.
    let idx = loadIndex(tmp)
    doAssert idx.len == m.index.len
    for i in 0 ..< idx.len:
      doAssert idx[i].sym == m.index[i].sym
      doAssert idx[i].pos == m.index[i].pos
      doAssert idx[i].vis == m.index[i].vis

  echo "bif self-tests passed"
