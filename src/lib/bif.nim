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
## Layout. A `.bif` file is stored **little-endian**: the token block and the
## `indexOffset` are raw host-endian dumps and BIF is *defined* to be
## little-endian (every supported target is), so the `Magic` carries a fixed
## endianness byte (`0` = little) plus a version, and a mismatched/foreign file is
## rejected rather than misread. The host word size does NOT appear — a token is
## always `uint32`. Every integer is a SQLite-style `varint`
## (`std/varints`) — most counts and lengths are small, so the header and the
## per-string/-entry length prefixes shrink to a byte or two. The SOLE exception
## is `indexOffset`, which is a **fixed 8-byte** little integer: it is written as
## a placeholder and patched in place once the tail offset is known, so it must
## keep a constant width (a varint would change size when patched):
##
## ```
##   Magic            8 bytes  ("NIFBIN" + endianness byte (0=little) + Version)
##   indexOffset      u64      -- FIXED 8 bytes; BYTE offset of the index (patched)
##   tokenCount       varint
##   nTags            varint
##   nStrings         varint
##   nSyms            varint
##   nFiles           varint
##   pad              0..3 zero bytes  -- align the token block to NifToken (4 B)
##   tokens           tokenCount * sizeof(NifToken)   -- one raw block
##   tags             nTags    * (varint len + bytes)
##   strings          nStrings * (varint len + bytes)
##   syms             nSyms    * (varint len + bytes)
##   filenames        nFiles   * (varint len + bytes)
##   index (@indexOffset)
##     nIndex         varint
##     entries        nIndex   * (varint symId, varint tokenPos, varint vis)
## ```
##
## The `index` is the binary analogue of a text NIF's embedded `(.index …)`:
## global `SymbolDef` -> declaration token position + visibility, so a loaded
## buffer can locate one symbol without scanning. It is ALWAYS present (a cache
## with no index would force a full rescan to find any declaration) and lives at
## the **end** of the file, exactly like the text format's trailing index. The
## single `indexOffset` header field is the binary `(.indexat …)`: the fixed
## 8-byte slot a reader follows to jump straight to the index. Because the offset
## is only known once the token block and pools have been written, `storeToFile`
## writes a placeholder, streams everything, then patches the slot — mirroring the
## text writer's `addHeader27` reservation + `patchIndexAt`. It stays a constant
## width precisely so that in-place patch keeps working. The index entries
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

when defined(nimony):
  {.feature: "lenientnils".}

import std / [syncio, assertions, strutils, varints]
import nifcore
import vfs

include compat2   # `onRaiseQuit`: run a raising call and `quit` on failure; identity under Nim

# Bulk string data access. Under Nimony these come from `system`; a `string`'s
# payload cannot be reached with a bare `addr s[0]` (it may be inline-stored), so
# reads go through `readRawData` and bulk writes through `beginStore`/`endStore`.
# Host Nim has no such API, so shim it (guarded, matching `stringviews.nim`).
when not defined(nimony):
  when not declared(readRawData):
    proc readRawData*(s: string): ptr UncheckedArray[char] {.inline.} =
      if s.len == 0: nil
      else: cast[ptr UncheckedArray[char]](addr(s[0]))
  when not declared(beginStore):
    proc beginStore*(s: var string; newLen: int; start = 0): ptr UncheckedArray[char] {.inline.} =
      s.setLen(newLen)
      if s.len == 0: nil
      else: cast[ptr UncheckedArray[char]](addr(s[start]))
  when not declared(endStore):
    proc endStore*(s: var string) {.inline.} = discard

const
  Version = 5'u8
  MagicLen = 8
  LittleEndianTag = 0'u8
    ## Endianness marker in the `Magic`. BIF stores the token block and the
    ## `indexOffset` as raw host-endian bytes and is *defined* to be
    ## little-endian (every supported target is), so this byte is fixed at 0 =
    ## little. It replaces a former `sizeof(int)` byte that guarded nothing: a
    ## token is always `uint32`, so the host word size never affects the layout.

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
  result = ['N', 'I', 'F', 'B', 'I', 'N', char(LittleEndianTag), char(Version)]

# A `.bif` file is a machine-local cache; a read/write/seek error is unrecoverable
# here. Under Nimony the syncio primitives are `{.raises.}`, so rather than thread
# `raises` through the whole module we funnel every raising call through these thin
# wrappers and `quit` on failure (via compat2's `onRaiseQuit`, an identity on host
# Nim). Everything above this line is pure; everything below only does I/O via
# these four wrappers, so no `.raises` call ever escapes.
proc readBytes(f: File; p: pointer; n: int): int = onRaiseQuit f.readBuffer(p, n)
proc getPos(f: File): int64 = onRaiseQuit getFilePos(f)
proc setPos(f: File; pos: int64) = onRaiseQuit setFilePos(f, pos)
proc setPosEnd(f: File) = onRaiseQuit setFilePos(f, 0, fspEnd)

proc writeExact(f: File; p: pointer; n: int) =
  let written = onRaiseQuit f.writeBuffer(p, n)
  assert written == n

proc readExact(f: File; p: pointer; n: int) =
  let got = readBytes(f, p, n)
  assert got == n

# `indexOffset` is the one fixed-width field: a placeholder that gets patched in
# place, so it must always occupy exactly 8 bytes (a varint would resize on patch).
proc writeU64(f: File; x: uint64) =
  var v = x
  writeExact(f, addr v, 8)

proc readU64(f: File): uint64 =
  result = 0'u64
  readExact(f, addr result, 8)

# Everything else is a varint. `varintLen` recovers a varint's total byte length
# from its first byte (mirrors `readVu64`'s `int(z[0]-246)` length check), so a
# file reader can pull the first byte, then the remaining `n-1`, then decode.
proc varintLen(b0: byte): int =
  if b0 <= 240: 1
  elif b0 <= 248: 2
  else: int(b0) - 246

proc writeVarint(f: File; x: uint64) =
  var buf = default(array[maxVarIntLen, byte])
  let n = writeVu64(buf, x)
  writeExact(f, addr buf[0], n)

proc readVarint(f: File): uint64 =
  var buf = default(array[maxVarIntLen, byte])
  readExact(f, addr buf[0], 1)
  let n = varintLen(buf[0])
  if n > 1:
    readExact(f, addr buf[1], n - 1)
  result = 0'u64
  discard readVu64(buf, result)

proc writeStr(f: File; s: string) =
  writeVarint(f, uint64 s.len)
  if s.len > 0:
    writeExact(f, cast[pointer](readRawData(s)), s.len)

proc readStr(f: File): string =
  let n = int readVarint(f)
  result = newString(n)
  if n > 0:
    readExact(f, cast[pointer](beginStore(result, n)), n)
    endStore(result)

proc tokenPad(pos: int): int =
  ## Number of zero bytes to insert after the (variable-width varint) header so
  ## the token block begins `NifToken`-aligned. The mmap loader borrows that block
  ## in place and `adoptForeignTokens` asserts it is aligned; the fixed-header
  ## format got this for free, but a varint header can end on any byte. Writer and
  ## every reader compute this from the SAME post-header position, so they agree.
  let a = sizeof(NifToken)
  (a - (pos and (a - 1))) and (a - 1)

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
  writeExact(f, addr m[0], MagicLen)
  # Reserve the `indexOffset` slot. We can only fill it once tokens and pools are
  # written, so write a placeholder now and patch it at the end — the binary
  # equivalent of `addHeader27`'s `(.indexat …)` reservation + `patchIndexAt`.
  let indexAtPos = getPos(f)
  writeU64(f, 0'u64)                   # fixed 8-byte placeholder for indexOffset
  writeVarint(f, uint64 b.len)
  writeVarint(f, uint64 b.tags.tags.len)
  writeVarint(f, uint64 b.pool.strings.len)
  writeVarint(f, uint64 b.pool.syms.len)
  writeVarint(f, uint64 b.pool.filenames.len)
  # Pad to NifToken alignment so the mmap loader can borrow the token block in
  # place (see `tokenPad`).
  let pad = tokenPad(int getPos(f))
  if pad > 0:
    var zeros = default(array[8, byte])
    writeExact(f, addr zeros[0], pad)
  # token stream: one contiguous block.
  let bytes = b.len * sizeof(NifToken)
  if bytes > 0:
    writeExact(f, b.rawTokenPtr, bytes)
  # pools, each in id order (ids are 1-based, dense up to len).
  for i in 1 .. b.tags.tags.len:      writeStr(f, b.tags.tags[TagId(i)])
  for i in 1 .. b.pool.strings.len:   writeStr(f, b.pool.strings[StrId(i)])
  for i in 1 .. b.pool.syms.len:      writeStr(f, b.pool.syms[SymId(i)])
  for i in 1 .. b.pool.filenames.len: writeStr(f, b.pool.filenames[FileId(i)])
  # symbol index — self-contained at the offset we now know. `pos` is a token
  # index (always >= 0) and `vis` a 0/1 enum, so plain varints suffice.
  let indexOffset = getPos(f)
  writeVarint(f, uint64 index.len)
  for e in index:
    writeVarint(f, uint64 e.sym)
    writeVarint(f, uint64 e.pos)
    writeVarint(f, uint64 ord(e.vis))
  # Patch the fixed-width header slot to point at the index, then leave the
  # cursor at EOF. The slot is 8 bytes wide, so this overwrites exactly it.
  setPos(f, indexAtPos)
  writeU64(f, uint64 indexOffset)
  setPosEnd(f)

proc store*(b: var TokenBuf; filename: string; dottedSuffix = "") =
  ## Write `b` to `filename` in binary `.bif` form (with its symbol index).
  var f = open(filename, fmWrite)
  storeToFile(b, f, dottedSuffix)
  close(f)

# ── load ──────────────────────────────────────────────────────────────────

proc checkMagic(f: File) =
  var m = default(array[MagicLen, char])
  let want = magic()
  readExact(f, addr m[0], MagicLen)
  for i in 0 ..< MagicLen:
    if m[i] != want[i]:
      quit "bif: bad magic / incompatible format or word size"

proc readIndex(f: File): seq[IndexEntry] =
  ## Read the self-contained index section (`nIndex` followed by the entries),
  ## assuming `f` is already positioned at its start.
  let nIndex = int readVarint(f)
  result = newSeq[IndexEntry](nIndex)
  for i in 0 ..< nIndex:
    let sym = SymId readVarint(f)
    let pos = int32(readVarint(f))
    let v = int readVarint(f)
    result[i] = IndexEntry(sym: sym, pos: pos, vis: IndexVis(v))

proc loadIndexFromFile*(f: File): seq[IndexEntry] =
  ## Read ONLY the symbol index, by following the header's `indexOffset` and
  ## seeking straight to the trailing index section — the token block and pools
  ## are never touched. This is the binary analogue of `nifindexes` jumping to a
  ## text NIF's `(.indexat …)` target. NOTE: each entry's `sym` is a `SymId`
  ## that is only meaningful together with the buffer's `syms` pool, so a fully
  ## standalone consumer must also load that pool (or use the names another way).
  checkMagic(f)
  let indexOffset = readU64(f)
  setPos(f, int64 indexOffset)
  result = readIndex(f)

proc loadIndex*(filename: string): seq[IndexEntry] =
  ## Read just the symbol index of a `.bif` file (see `loadIndexFromFile`).
  var f = open(filename, fmRead)
  result = loadIndexFromFile(f)
  close(f)

proc containsSym*(filename, name: string): bool =
  ## Cheap membership probe: does the `.bif` at `filename` reference the symbol
  ## `name`? Reads ONLY the trailing symbol table — seeking past the (large) token
  ## block and the tags/strings tables to reach `syms`, which it scans comparing
  ## just the equal-length entries. No token block is mapped and no pool `BiTable`
  ## is built, so a module that never mentions `name` is rejected far more cheaply
  ## than a full `load`; `nim track` uses it to skip loading the whole nimcache for
  ## a query whose symbol lives in only a handful of modules.
  ##
  ## A symbol is in `syms` iff it is stored by pool id — i.e. any name longer than
  ## `StrInlineMaxLen`, which a mangled `ident.disamb.suffix` always is; shorter
  ## inline-encoded names never reach the table, so only use this for such pooled
  ## names. A foreign or truncated file yields `false` ("skip it").
  result = false                        # every non-match path falls through to this
  var f = default(File)
  if not open(f, filename, fmRead): return false
  try:
    var m = default(array[MagicLen, char])
    if readBytes(f, addr m[0], MagicLen) != MagicLen or m != magic(): return false
    discard readU64(f)                  # indexOffset (fixed 8 bytes)
    let tokenCount = int readVarint(f)
    let nTags      = int readVarint(f)
    let nStrings   = int readVarint(f)
    let nSyms      = int readVarint(f)
    discard readVarint(f)               # nFiles
    # Skip the alignment pad, the token block and the tags/strings tables to reach `syms`.
    let pad = tokenPad(int getPos(f))
    setPos(f, getPos(f) + int64(pad) + int64(tokenCount) * sizeof(NifToken))
    for _ in 1 .. nTags + nStrings:
      let n = int readVarint(f)         # read length first, THEN skip the bytes
      setPos(f, getPos(f) + n)
    # Scan the syms table; only entries of matching length can be `name`.
    for _ in 1 .. nSyms:
      let n = int readVarint(f)
      if n == name.len:
        var s = newString(n)
        if n > 0:
          if readBytes(f, cast[pointer](beginStore(s, n)), n) != n: return false
          endStore(s)
        if s == name: return true
      else:
        setPos(f, getPos(f) + n)
    result = false
  finally:
    close(f)

proc loadFromFile*(f: File): BifModule =
  ## Read a `BifModule` (token buffer + symbol index) from an already-open binary
  ## file. Always mints FRESH pools — see the INVARIANT in the module doc: the
  ## raw token words embed pool ids that are only reproduced by interning into
  ## empty pools from id 1, so loading into a shared/pre-populated pool is
  ## deliberately not supported. Quits on a magic/version mismatch.
  result = BifModule()
  checkMagic(f)
  discard readU64(f)               # indexOffset — only `loadIndex*` follows it;
                                   # a full load reaches the index sequentially.
  let tokenCount = int readVarint(f)
  let nTags      = int readVarint(f)
  let nStrings   = int readVarint(f)
  let nSyms      = int readVarint(f)
  let nFiles     = int readVarint(f)
  # Skip the alignment pad the writer inserted before the token block.
  let pad = tokenPad(int getPos(f))
  if pad > 0: setPos(f, getPos(f) + int64(pad))

  result.buf = createTokenBuf(max(tokenCount, 16))   # fresh pools — see INVARIANT
  # token stream: read straight into the storage in one block.
  if tokenCount > 0:
    let dest = result.buf.growRawUninit(tokenCount)
    let bytes = tokenCount * sizeof(NifToken)
    readExact(f, dest, bytes)
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

proc rU64(r: var BifReader): uint64 =
  assert r.pos + 8 <= r.size, "bif: truncated header"
  result = cast[ptr uint64](r.base + uint(r.pos))[]
  r.pos += 8

proc rVarint(r: var BifReader): uint64 =
  assert r.pos + 1 <= r.size, "bif: truncated varint"
  let b0 = cast[ptr byte](r.base + uint(r.pos))[]
  let n = varintLen(b0)
  assert r.pos + n <= r.size, "bif: truncated varint"
  var buf = default(array[maxVarIntLen, byte])
  var i = 0
  while i < n:
    buf[i] = cast[ptr byte](r.base + uint(r.pos + i))[]
    inc i
  r.pos += n
  result = 0'u64
  discard readVu64(buf, result)

proc rStr(r: var BifReader): string =
  let n = int rVarint(r)
  assert r.pos + n <= r.size, "bif: truncated string"
  result = newString(n)
  if n > 0:
    copyMem(cast[pointer](beginStore(result, n)), cast[pointer](r.base + uint(r.pos)), n)
    endStore(result)
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
  discard rU64(r)                  # indexOffset — a full load reaches it linearly
  let tokenCount = int rVarint(r)
  let nTags      = int rVarint(r)
  let nStrings   = int rVarint(r)
  let nSyms      = int rVarint(r)
  let nFiles     = int rVarint(r)
  # Skip the alignment pad so the borrowed token block is NifToken-aligned. The
  # mapping's base is page-aligned, so aligning `r.pos` aligns the absolute address.
  r.pos += tokenPad(r.pos)
  # token block follows the varint header and is borrowed straight from the mapping.
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
  let nIndex = int rVarint(r)
  result.index = newSeq[IndexEntry](nIndex)
  for i in 0 ..< nIndex:
    let sym = SymId rVarint(r)
    let pos = int32(rVarint(r))
    let v = int rVarint(r)
    result.index[i] = IndexEntry(sym: sym, pos: pos, vis: IndexVis(v))

# ── self-test ───────────────────────────────────────────────────────────────

when isMainModule:
  when defined(nimony):
    # Nimony's `system` has `assert` but not `doAssert`; the self-test wants an
    # always-on check, so provide the two arities it uses.
    template doAssert(cond: bool) =
      if not cond: quit "bif self-test: assertion failed"
    template doAssert(cond: bool; msg: string) =
      if not cond: quit "bif self-test: " & msg

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

    # the non-mmap File reader must decode byte-for-byte the same buffer as the
    # mmap path (it walks the same varint header + alignment pad).
    var ff = open(tmp, fmRead)
    let fm = loadFromFile(ff)
    close(ff)
    doAssert sameTokens(src, fm.buf), "loadFromFile disagrees with mmap load"
    doAssert fm.buf.pool.syms.len == src.pool.syms.len

    # containsSym probes only the syms table (skipping header, pad, tokens, tags,
    # strings); it must find a pooled name and reject an absent one.
    doAssert containsSym(tmp, "some.long.symbol.name.0")
    doAssert containsSym(tmp, "another.symbol.def.1")
    doAssert not containsSym(tmp, "no.such.symbol.here.9")

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
