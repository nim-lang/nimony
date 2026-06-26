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
## ```
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

import std / [syncio, assertions]
import nifcore

const
  Version = 1'u8
  MagicLen = 8

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

proc storeToFile*(b: TokenBuf; f: File) =
  ## Write `b` (token stream + pools) to an already-open binary file. The pools
  ## are written whole, in id order. If `b` shares a pool with other buffers, the
  ## *entire* shared pool is written (still correct — ids stay dense and load
  ## reproduces them — but larger than this module needs). For a compact,
  ## self-contained cache file, build the buffer against its own private pool.
  var m = magic()
  assert f.writeBuffer(addr m[0], MagicLen) == MagicLen
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

proc store*(b: TokenBuf; filename: string) =
  ## Write `b` to `filename` in binary `.bif` form.
  var f = open(filename, fmWrite)
  storeToFile(b, f)
  close(f)

# ── load ──────────────────────────────────────────────────────────────────

proc loadFromFile*(f: File): TokenBuf =
  ## Read a `TokenBuf` from an already-open binary file. Always mints FRESH
  ## pools — see the INVARIANT in the module doc: the raw token words embed pool
  ## ids that are only reproduced by interning into empty pools from id 1, so
  ## loading into a shared/pre-populated pool is deliberately not supported.
  ## Quits on a magic/version mismatch.
  var m = default(array[MagicLen, char])
  let want = magic()
  assert f.readBuffer(addr m[0], MagicLen) == MagicLen
  for i in 0 ..< MagicLen:
    if m[i] != want[i]:
      quit "bif: bad magic / incompatible format or word size"
  let tokenCount = int readU32(f)
  let nTags      = int readU32(f)
  let nStrings   = int readU32(f)
  let nSyms      = int readU32(f)
  let nFiles     = int readU32(f)

  result = createTokenBuf(max(tokenCount, 16))   # fresh pools — see INVARIANT
  # token stream: read straight into the storage in one block.
  if tokenCount > 0:
    let dest = result.growRawUninit(tokenCount)
    let bytes = tokenCount * sizeof(NifToken)
    assert f.readBuffer(dest, bytes) == bytes
  # pools: re-intern in stored (id) order so ids 1,2,… match the token refs.
  for _ in 1 .. nTags:    discard result.tags.tags.getOrIncl(readStr(f))
  for _ in 1 .. nStrings: discard result.pool.strings.getOrIncl(readStr(f))
  for _ in 1 .. nSyms:    discard result.pool.syms.getOrIncl(readStr(f))
  for _ in 1 .. nFiles:   discard result.pool.filenames.getOrIncl(readStr(f))

proc load*(filename: string): TokenBuf =
  ## Read a `TokenBuf` from a binary `.bif` file. Mints fresh pools (see INVARIANT).
  var f = open(filename, fmRead)
  result = loadFromFile(f)
  close(f)

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
    var dst = load(tmp)
    doAssert sameTokens(src, dst), "token streams differ after round-trip"
    # pools must match value-for-value (so ids line up)
    doAssert dst.tags.tags.len == src.tags.tags.len
    doAssert dst.pool.strings.len == src.pool.strings.len
    doAssert dst.pool.syms.len == src.pool.syms.len
    doAssert dst.pool.filenames.len == src.pool.filenames.len
    # spot-check a decoded value via a cursor
    var c = dst.beginRead()
    doAssert c.kind == TagLit
    doAssert dst.tags.tagName(c.cursorTagId) == "foo"

  echo "bif self-tests passed"
