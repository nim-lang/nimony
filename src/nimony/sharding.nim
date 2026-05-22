#       Nimony
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Backend sharder: invoked by deps.nim between the frontend and backend
## nifmake runs. For every dep node whose original Nim file basename
## matches the sharding gate (currently only "sem.nim"), partitions the
## umbrella `.s.nif` produced by sem into several `.s.nif` files. Each
## resulting shard is a **well-formed NIF module in its own right** —
## its filename modname and its symbol suffixes agree, so hexer / nifc /
## DCE never need to know that sharding happened.
##
## Layout (for module `<base>`):
##
##   - Shard 0 keeps the umbrella filename `<base>.s.nif`. It contains
##     every *exported* top-level decl plus a portion of the hidden
##     decls. Symbols defined here keep the original `<base>` suffix so
##     importing modules' frontend output (which references exports as
##     `<sym>.0.<base>`) resolves transparently.
##   - Shards 1..N are written as `<base>_<i>.s.nif`. They contain only
##     *hidden* (non-exported) decls. Symbols defined here are rewritten
##     to use the shard's filename modname as their suffix
##     (`<sym>.0.<base>_<i>`). References from any shard to a hidden
##     decl that moved to shard i are rewritten to point at its new
##     suffixed name.
##
## Why this layout:
##
##   - Importers (nimsem on a module that `import sem`s) only care
##     about exported symbols. Those stay in shard 0 with the umbrella
##     suffix, so `programs.load(<base>)` finds them via the unchanged
##     embedded-index path.
##   - Hidden cross-shard references resolve through the standard NIF
##     cross-module mechanism: the symbol's suffix names the file to
##     look in, and the file's embedded index points at the body.
##   - Hexer auto-generated hooks for shared types get distinct C names
##     across shards (different sym suffixes → different mangled
##     names), and DCE's `resolveSymbolConflicts` picks one canonical
##     instance, so the linker sees no duplicate definitions.
##   - Each shard's hexer generates `'ini.0.<filename-modname>`; the
##     one belonging to shard 0 IS `'ini.0.<base>`, which is what
##     nimsem's main calls. Other shards' inits stay unreferenced and
##     are removed by linker DCE.

when defined(nimony):
  {.feature: "lenientnils".}

import std / [os, syncio, assertions, tables, sets, hashes, formatfloat, times]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / [symparser, nifindexes]
import nimony_model

const
  ShardTokenThreshold = 60_000
    ## Token-count threshold per shard. Sem's post-sem `.s.nif` buffer
    ## is ~310K tokens (measured 2026-05-20); 60K yields ~5
    ## roughly-balanced shards.

proc shouldShard*(nifFile: string): bool {.inline.} =
  try:
    result = getFileSize(nifFile) > 100*1024
  except:
    result = false

# ---- partitioning ---------------------------------------------------------

type
  TopLevelDecl = object
    start, stop: int       # half-open range into the parsed umbrella buf
    exported: bool         # true iff its first SymbolDef is in the umbrella's `(x …)` list

  Shard = object
    suffix: string
    declIndices: seq[int]
    tokenCount: int        # running sum of (stop-start) over chosen decls

proc size(d: TopLevelDecl): int {.inline.} = d.stop - d.start

proc collectTopLevelDecls(buf: TokenBuf; exportedSyms: HashSet[SymId]): seq[TopLevelDecl] =
  ## Walks buf top-level children. For each, records its byte range and
  ## whether its first SymbolDef is exported.
  result = @[]
  if buf.len == 0 or buf[0].kind != ParLe: return
  var i = 1
  while i < buf.len:
    if buf[i].kind == ParRi: break  # outer stmts closer
    let start = i
    var firstSym = SymId(0)
    if buf[i].kind == ParLe:
      var nesting = 0
      while i < buf.len:
        case buf[i].kind
        of ParLe:
          inc nesting
          inc i
        of ParRi:
          dec nesting
          inc i
          if nesting == 0: break
        of SymbolDef:
          if firstSym == SymId(0): firstSym = buf[i].symId
          inc i
        else: inc i
    else:
      inc i
    result.add TopLevelDecl(
      start: start, stop: i,
      exported: firstSym != SymId(0) and firstSym in exportedSyms)

proc loadExportedSyms(umbrellaPath: string): HashSet[SymId] =
  ## Reads the umbrella's embedded `.index` block and collects every
  ## symbol that appears as `(x …)` (Exported). Hidden `(h …)` entries
  ## are ignored — only exports need to live in shard 0.
  result = initHashSet[SymId]()
  var stream = nifstreams.open(umbrellaPath)
  let embedded = readEmbeddedIndex(stream)
  for k, entry in embedded:
    if entry.vis == Exported:
      result.incl pool.syms.getOrIncl(k)
  close stream

# ---- shard plan -----------------------------------------------------------

proc shardSuffix(baseSuffix: string; idx: int): string =
  if idx == 0: baseSuffix
  else: baseSuffix & "_" & $idx

proc planShards(decls: seq[TopLevelDecl]; baseSuffix: string;
                threshold: int): seq[Shard] =
  ## Bin-packs decls into shards. Every exported decl goes into shard 0
  ## (the umbrella). The total token count and threshold pick a target
  ## shard count N; hidden decls are placed in original order, filling
  ## each shard up to `total/N` before spilling. Picking the target
  ## upfront keeps the last shard from being a tiny tail.
  var total = 0
  for d in decls: total += d.size
  let shardCount = max(1, (total + threshold - 1) div threshold)
  let target = (total + shardCount - 1) div shardCount

  result = @[Shard(suffix: baseSuffix)]
  for idx, d in pairs decls:
    if d.exported:
      result[0].declIndices.add idx
      result[0].tokenCount += d.size
  for idx, d in pairs decls:
    if not d.exported:
      if result[^1].tokenCount >= target and result.len < shardCount:
        result.add Shard(suffix: shardSuffix(baseSuffix, result.len))
      result[^1].declIndices.add idx
      result[^1].tokenCount += d.size

# ---- SymId rewriting ------------------------------------------------------

proc buildRemap(decls: seq[TopLevelDecl]; buf: TokenBuf;
                shards: seq[Shard]; baseSuffix: string): Table[SymId, SymId] =
  ## For every top-level decl that's been routed to a shard with index
  ## > 0, collect every SymbolDef in the decl whose current suffix is
  ## the base suffix and map it to a freshly-interned name using the
  ## shard's suffix. Refs to those symbols (anywhere in any shard's
  ## buffer) will be rewritten via this table.
  result = initTable[SymId, SymId]()
  for shardIdx in 1 ..< shards.len:
    let shard = shards[shardIdx]
    for declIdx in shard.declIndices:
      let d = decls[declIdx]
      for j in d.start ..< d.stop:
        if buf[j].kind == SymbolDef:
          let oldId = buf[j].symId
          if oldId notin result:
            let split = splitSymName(pool.syms[oldId])
            if split.module == baseSuffix:
              result[oldId] = pool.syms.getOrIncl(split.name & "." & shard.suffix)

proc rewriteToken(t: PackedToken; remap: Table[SymId, SymId]): PackedToken =
  ## Returns a token with its symId remapped (if applicable), otherwise
  ## the input token unchanged.
  case t.kind
  of Symbol:
    let new = remap.getOrDefault(t.symId, SymId(0))
    if new != SymId(0): symToken(new, t.info) else: t
  of SymbolDef:
    let new = remap.getOrDefault(t.symId, SymId(0))
    if new != SymId(0): symdefToken(new, t.info) else: t
  else: t

# ---- shard emit -----------------------------------------------------------

proc buildShardBuf(buf: TokenBuf; decls: seq[TopLevelDecl]; shard: Shard;
                   remap: Table[SymId, SymId]): TokenBuf =
  ## Build one shard's TokenBuf: wrap the chosen decls in the same
  ## outer `(stmts …)` shape as the umbrella, applying the global
  ## SymId remap as we copy. Pre-sized to avoid reallocations.
  assert buf.len > 0 and buf[0].kind == ParLe
  result = createTokenBuf(shard.tokenCount + 2)
  result.add buf[0]  # (stmts …) opener with the umbrella's info
  if remap.len == 0:
    # Shard 0 / no-remap case: tight inner loop with no per-token
    # case-dispatch.
    for declIdx in shard.declIndices:
      let d = decls[declIdx]
      for j in d.start ..< d.stop:
        result.add buf[j]
  else:
    for declIdx in shard.declIndices:
      let d = decls[declIdx]
      for j in d.start ..< d.stop:
        result.add rewriteToken(buf[j], remap)
  result.addParRi()

proc shardPath(dir, baseSuffix: string; idx: int): string =
  let suffix = shardSuffix(baseSuffix, idx)
  if dir.len > 0: dir / suffix & ".s.nif"
  else: suffix & ".s.nif"

proc shardFile*(umbrellaPath: string; baseSuffix: string): seq[string] =
  ## Partition `umbrellaPath` into shards. Writes `<baseSuffix>.s.nif`
  ## (shard 0, overwriting the umbrella) and `<baseSuffix>_<i>.s.nif`
  ## for i in 1..<N. Returns the seq of shard suffixes. When the file
  ## is below threshold, returns `@[baseSuffix]` and leaves the umbrella
  ## untouched.
  ##
  ## Incremental short-circuit: if a previously-produced spillover
  ## shard already exists on disk and is at least as recent as the
  ## umbrella file, the sharder previously ran on the *current*
  ## umbrella content (sem.nim didn't re-emit) so we just enumerate the
  ## existing shards and return — no I/O, no mtime bumps, no
  ## downstream cascade. This is the make-friendly way to avoid
  ## rewriting per-shard `.s.nif` files on every deps.nim invocation;
  ## the alternative (always partition) breaks nifmake's incremental
  ## detection because the umbrella file at `<baseSuffix>.s.nif` is
  ## both sem's output and shard 0's filename.
  result = @[baseSuffix]
  let dir = parentDir(umbrellaPath)
  let firstSpillover = shardPath(dir, baseSuffix, 1)
  if fileExists(firstSpillover):
    # `getLastModificationTime` can raise on transient I/O errors; we
    # don't actually care about those — a stat failure just means "no
    # incremental shortcut available, partition the umbrella again."
    var canShortCircuit = false
    try:
      let umbrellaMtime = getLastModificationTime(umbrellaPath)
      let firstSpilloverMtime = getLastModificationTime(firstSpillover)
      canShortCircuit = firstSpilloverMtime >= umbrellaMtime
    except:
      canShortCircuit = false
    if canShortCircuit:
      # Enumerate existing shards. Stop at the first gap.
      var shards = @[baseSuffix]
      var i = 1
      while fileExists(shardPath(dir, baseSuffix, i)):
        shards.add shardSuffix(baseSuffix, i)
        inc i
      return shards

  var buf = parseFromFile(umbrellaPath, sizeHint = 1024)
  if buf.len == 0 or buf[0].kind != ParLe: return

  let exported = loadExportedSyms(umbrellaPath)
  let decls = collectTopLevelDecls(buf, exported)
  var total = 0
  for d in decls: total += d.size
  if total < ShardTokenThreshold: return

  let shards = planShards(decls, baseSuffix, ShardTokenThreshold)
  if shards.len == 1: return  # everything fit into shard 0

  let remap = buildRemap(decls, buf, shards, baseSuffix)

  result = newSeq[string](shards.len)
  for i, shard in pairs shards:
    result[i] = shard.suffix
    let shardBuf = buildShardBuf(buf, decls, shard, remap)
    let outpath = shardPath(dir, baseSuffix, i)
    try:
      writeFile(shardBuf, outpath, OnlyIfChanged)
    except:
      quit "failed to write shard file " & outpath
