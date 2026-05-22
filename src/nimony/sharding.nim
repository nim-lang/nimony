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
    firstSym: SymId        # first SymbolDef inside, or SymId(0) if none
    exported: bool         # true iff `firstSym` is in the umbrella's `(x …)` list
    size: int              # stop - start (token count)

proc collectTopLevelDecls(buf: TokenBuf; exportedSyms: HashSet[SymId]): seq[TopLevelDecl] =
  ## Walks dest top-level children. For each, records its byte range,
  ## first SymbolDef, and whether that sym is exported.
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
      start: start, stop: i, firstSym: firstSym,
      exported: firstSym != SymId(0) and firstSym in exportedSyms,
      size: i - start)

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

type
  ShardPlan = object
    decls: seq[seq[int]]   # indices into `decls[]` per shard (0..N-1)
    suffixes: seq[string]  # shard suffix per shard
    baseSuffix: string

proc planShards(decls: seq[TopLevelDecl]; baseSuffix: string; threshold: int): ShardPlan =
  ## Bin-packs decls into shards. Every exported decl goes into shard 0
  ## (the umbrella). Hidden decls fill the remaining capacity of shard
  ## 0 up to `threshold`, then spill into shards 1, 2, … as each shard
  ## crosses its own threshold.
  var initial: seq[seq[int]] = @[]
  initial.add newSeq[int]()
  result = ShardPlan(decls: initial, suffixes: @[baseSuffix], baseSuffix: baseSuffix)
  var currentSize = 0
  # Pass 1: exports → shard 0.
  for idx, d in pairs decls:
    if d.exported:
      result.decls[0].add idx
      currentSize += d.size
  # Pass 2: hidden decls fill shard 0 then spill.
  for idx, d in pairs decls:
    if not d.exported:
      if currentSize >= threshold:
        # Start a new shard.
        let nextIdx = result.decls.len
        result.decls.add @[]
        result.suffixes.add baseSuffix & "_" & $nextIdx
        currentSize = 0
      result.decls[^1].add idx
      currentSize += d.size

# ---- SymId rewriting ------------------------------------------------------

proc buildRemap(decls: seq[TopLevelDecl]; buf: TokenBuf;
                plan: ShardPlan): Table[SymId, SymId] =
  ## For every top-level decl that's been routed to a shard with index
  ## > 0, collect every SymbolDef in the decl whose current suffix is
  ## the base suffix and map it to a freshly-interned name using the
  ## shard's suffix. Refs to those symbols (anywhere in any shard's
  ## buffer) will be rewritten via this table.
  result = initTable[SymId, SymId]()
  for shardIdx in 1 ..< plan.decls.len:
    let shardSuffix = plan.suffixes[shardIdx]
    for declIdx in plan.decls[shardIdx]:
      let d = decls[declIdx]
      for j in d.start ..< d.stop:
        if buf[j].kind == SymbolDef:
          let oldId = buf[j].symId
          if oldId notin result:
            let oldName = pool.syms[oldId]
            let owner = extractModule(oldName)
            if owner == plan.baseSuffix:
              var newName = removeModule(oldName)
              newName.add '.'
              newName.add shardSuffix
              result[oldId] = pool.syms.getOrIncl(newName)

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

proc buildShardBuf(buf: TokenBuf; decls: seq[TopLevelDecl]; declIndices: seq[int];
                   remap: Table[SymId, SymId]): TokenBuf =
  ## Build one shard's TokenBuf: wrap the chosen decls in the same
  ## outer `(stmts …)` shape as the umbrella, applying the global
  ## SymId remap as we copy.
  assert buf.len > 0 and buf[0].kind == ParLe
  result = createTokenBuf(64)
  result.add buf[0]  # (stmts …) opener with the umbrella's info
  for declIdx in declIndices:
    let d = decls[declIdx]
    for j in d.start ..< d.stop:
      result.add rewriteToken(buf[j], remap)
  result.addParRi()

proc shardPath(dir, baseSuffix: string; idx: int): string =
  let shardSuffix =
    if idx == 0: baseSuffix
    else: baseSuffix & "_" & $idx
  if dir.len > 0: dir / shardSuffix & ".s.nif"
  else: shardSuffix & ".s.nif"

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
      var i = 1
      var shards = newSeq[string]()
      shards.add baseSuffix
      while true:
        let p = shardPath(dir, baseSuffix, i)
        if not fileExists(p): break
        shards.add baseSuffix & "_" & $i
        inc i
      return shards

  var buf = parseFromFile(umbrellaPath, sizeHint = 1024)
  if buf.len == 0 or buf[0].kind != ParLe: return

  # Quick size check: if total decl-token count is below threshold,
  # don't bother sharding at all.
  let exported = loadExportedSyms(umbrellaPath)
  let decls = collectTopLevelDecls(buf, exported)
  var total = 0
  for d in decls: total += d.size
  if total < ShardTokenThreshold: return

  let plan = planShards(decls, baseSuffix, ShardTokenThreshold)
  if plan.decls.len == 1: return  # everything fit into shard 0

  let remap = buildRemap(decls, buf, plan)

  result = newSeq[string](plan.suffixes.len)
  for i in 0 ..< plan.suffixes.len:
    let shardSuffix = plan.suffixes[i]
    result[i] = shardSuffix
    let shardBuf = buildShardBuf(buf, decls, plan.decls[i], remap)
    let outpath = shardPath(dir, baseSuffix, i)
    try:
      writeFile(shardBuf, outpath, OnlyIfChanged)
    except:
      quit "failed to write shard file " & outpath
