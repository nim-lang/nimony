#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Dead code elimination and generic instance merging.

import std / [os, tables, hashes, sets, assertions, syncio, algorithm]
include ".." / lib / nifprelude
include ".." / lib / compat2

import ".." / lib / symparser
import dce1
import ".." / lengc / [leng_model]

type
  ResolveTable = Table[string, SymId]
    # `foo.1.I<type hash>` -> `foo.1.I<type hash>.module`
    # that is selected for the generic instance

proc resolveSymbolConflicts(modules: Table[string, ModuleAnalysis]): ResolveTable =
  # Resolve conflicts between duplicate symbols (e.g., generic instantiations)
  # Returns: symbol mapping from key to canonical
  result = initTable[string, SymId]()
  for m in modules.values:
    for offer in m.offers:
      let key = pool.symWithoutModule(offer)
      let existing = result.getOrDefault(key, SymId(0))
      # deterministic pick among the copies: the smallest spelling wins
      if existing == SymId(0) or pool.symString(offer) < pool.symString(existing):
        result[key] = offer

proc translate(resolved: ResolveTable; sym: SymId): SymId =
  if pool.symIsInstantiation(sym):
    result = resolved.getOrDefault(pool.symWithoutModule(sym), sym)
  else:
    result = sym

proc markLive(moduleGraphs: Table[string, ModuleAnalysis]; resolved: ResolveTable): Table[string, HashSet[SymId]] =
  var worklist = newSeq[SymId](0)

  result = initTable[string, HashSet[SymId]]()

  for k, m in moduleGraphs:
    result[k] = initHashSet[SymId]()
    for root in m.roots:
      worklist.add(root)

  while worklist.len > 0:
    let sym = translate(resolved, worklist.pop())
    let moduleName = pool.symModule(sym)
    assert moduleName.len > 0, "moduleName is empty for " & pool.symString(sym)

    # Check if symbol is already live in its owning module
    if not result.getOrQuit(moduleName).containsOrIncl(sym):
      # Process dependencies from the symbol's own module
      if moduleName in moduleGraphs:
        # Immutable view to avoid deepcopy of the graphs
        let graph {.cursor.} = moduleGraphs.getOrQuit(moduleName)
        if sym in graph.uses:
          for dep in graph.uses.getOrQuit(sym):
            let s = translate(resolved, dep)
            let sowner = pool.symModule(s)
            # Check if dependency is already live in its owning module
            if sowner.len > 0:
              assert sowner in result, "sowner is not in result for " & pool.symString(s)
            if sowner.len > 0 and s notin result.getOrQuit(sowner):
              worklist.add(s)

template toLengName(sym: SymId): SymId = sym

proc tr(dest: var TokenBuf; n: var Cursor; alive: HashSet[SymId]; resolved: ResolveTable) =
  case n.kind
  of TagLit:
    let stmtKind = n.stmtKind
    case stmtKind
    of TypeS:
      # types are fundamentally different from procs when it comes to generic instantiations:
      # We need to ensure **consistency** for types, but for procs we need to ensure **uniqueness**.
      let headTag = n.cursorTagId
      dest.addParLe(headTag, n.info)
      n.into:
        if n.isSymbolDef:
          let def = n.symId
          let t = translate(resolved, def)
          dest.addSymDef t.toLengName, n.info
          skip n # skip symbol def (atom)
          while n.hasMore:
            tr dest, n, alive, resolved
        else:
          # let errors propagate:
          while n.hasMore:
            tr dest, n, alive, resolved
      dest.addParRi()

    of ProcS, VarS, ConstS, GvarS, TvarS:
      let headTag = n.cursorTagId
      let headInfo = n.info
      n.into:
        if n.isSymbolDef:
          let def = n.symId
          if pool.symIsLocal(def):
            dest.addParLe(headTag, headInfo)
            dest.addSymDef def.toLengName, n.info
            inc n # skip symbol def
            while n.hasMore:
              tr dest, n, alive, resolved
            dest.addParRi()
          elif alive.contains(def):
            let t = translate(resolved, def)
            if t != def:
              # we are a loser and need to add an `extern` declaration:
              dest.addParLe(globalTags.registerTag("imp"), headInfo)

              dest.addParLe(headTag, headInfo)
              dest.addSymDef t.toLengName, n.info
              inc n # skip symbol def
              var untilBody = if stmtKind == ProcS: 3 else: 2 # pragmas type (for procs: return type)
              while n.hasMore and untilBody > 0:
                dec untilBody
                tr dest, n, alive, resolved
              skip n # skip the body
              # replace it with an empty body:
              dest.addDotToken()
              dest.addParRi()
              dest.addParRi() # also close the "imp" declaration
            else:
              dest.addParLe(headTag, headInfo)
              dest.addSymDef def.toLengName, n.info
              inc n # skip symbol def
              while n.hasMore:
                tr dest, n, alive, resolved
              dest.addParRi()
          else:
            # skip it, it's dead
            inc n # skip symbol def
            while n.hasMore: skip n
        else:
          # let errors propagate:
          dest.addParLe(headTag, headInfo)
          while n.hasMore:
            tr dest, n, alive, resolved
          dest.addParRi()
    else:
      dest.addParLe(n.cursorTagId, n.info)
      n.into:
        while n.hasMore:
          tr dest, n, alive, resolved
      dest.addParRi()
  of Symbol:
    let t = translate(resolved, n.symId)
    dest.addSymUse t.toLengName, n.info
    inc n
  of SymbolDef:
    let t = translate(resolved, n.symId)
    dest.addSymDef t.toLengName, n.info
    inc n
  else: # atoms and suffix kinds; classic: a physical ParRi cannot appear here
    dest.takeTree n

proc rewriteModule(file: string; live: HashSet[SymId]; resolved: ResolveTable; outdir: string) =
  var buf = parseFromFile(file)
  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len)
  # `(stmts` is opened here rather than by `tr` so the leading `(dce …)` can be
  # dropped: it is hexer's analysis section for `dceLive`, not code, and the
  # back end must never see it. Only the head statement can be one, so this
  # costs the emit a single comparison.
  dest.addParLe n.cursorTagId, n.info
  n.into:
    if n.isTagLit and n.cursorTagId == globalTags.registerTag(dceName):
      skip n
    while n.hasMore:
      tr dest, n, live, resolved
  dest.addParRi()
  let outPath =
    if outdir.len > 0:
      outdir / splitModulePath(file).name & ".c.nif"
    else:
      file.changeModuleExt ".c.nif"
  try:
    writeFile(dest, outPath, OnlyIfChanged)
  except:
    quit "could not write file: " & outPath

proc deadCodeElimination*(files: openArray[string]; outdir: string) =
  ## Single-shot DCE: read every module's analysis section, compute global
  ## liveness, then sequentially rewrite each module's .x.nif to .c.nif. Kept
  ## for the single-process API; the build pipeline now goes through the split
  ## `computeLiveSet` + `dceEmit` pair so the per-module rewrite step
  ## parallelizes across modules.
  var graphs = initTable[string, ModuleAnalysis]()
  for file in files:
    let modName = splitModulePath(file).name
    graphs[modName] = readModuleAnalysis(file)

  let resolved = resolveSymbolConflicts(graphs)

  let live = markLive(graphs, resolved)
  for file in files:
    let modName = splitModulePath(file).name
    rewriteModule(file, live.getOrQuit(modName), resolved, outdir)

# ---- Split DCE: liveness computation and per-module emit -----------------

const
  liveTag    = "live"      # `(live (sym Symbol Symbol …))` — per-module live syms
  resolveTag = "resolved"  # `(resolved (kv String Symbol)*)` — generic-instance picks
  modTag     = "mod"       # `(mod String (sym …)*)` — block per module
  symTag     = "sym"

proc writeLiveFile*(outfile: string; resolved: ResolveTable;
                    modName: string; live: HashSet[SymId]) =
  ## Serialize one module's slice of the global DCE result to the file its
  ## `dceEmit` reads. Symbols are written with their full module suffix (no
  ## abbreviation): the dotted-suffix shortcut expands using the reader's
  ## `thisModule`, which is derived from the filename, but the resolve entries
  ## name winners from other modules, where that expansion would be wrong. So
  ## we pay the file-size cost rather than mis-expand.
  ##
  ## In name order, for the reason `dce1.sortedSymNames` gives.
  var b = nifbuilder.open(outfile, writeMode = OnlyIfChanged)
  b.withTree "stmts":
    b.withTree resolveTag:
      for key in sortedKeys(resolved):
        b.withTree "kv":
          b.addStrLit key
          let winner = resolved.getOrQuit(key)
          b.addSymbol pool.symString(winner), ""
    b.withTree liveTag:
      b.withTree modTag:
        b.addStrLit modName
        for s in sortedSymNames(live):
          b.addSymbol s, ""
  b.close()

type
  LiveSet* = object
    resolved*: ResolveTable
    live*: Table[string, HashSet[SymId]]

proc readLiveFile*(infile: string): LiveSet =
  var buf = parseFromFile(infile)
  var n = beginRead(buf)
  result = LiveSet(
    resolved: initTable[string, SymId](),
    live: initTable[string, HashSet[SymId]]())
  if n.stmtKind != StmtsS:
    raiseAssert infile & ": expected (stmts ...)"
  let liveTagId = globalTags.registerTag(liveTag)
  let resolveTagId = globalTags.registerTag(resolveTag)
  let modTagId = globalTags.registerTag(modTag)
  n.into:                                       # (stmts ...)
    while n.hasMore:
      if not n.isTagLit:
        raiseAssert infile & ": expected ParLe"
      if n.cursorTagId == resolveTagId:
        n.into:                                 # (resolved ...)
          while n.hasMore:
            if n.isTagLit and n.substructureKind == KvU:
              n.into:                           # (kv ...)
                if not n.isStringLit:
                  raiseAssert infile & ": kv key must be StringLit"
                let key = pool.strings[n.strId]
                skip n
                if n.kind != Symbol:
                  raiseAssert infile & ": kv value must be Symbol"
                result.resolved[key] = n.symId
                skip n
                if n.hasMore:
                  raiseAssert infile & ": expected ')' closing kv"
            else:
              raiseAssert infile & ": expected (kv …)"
      elif n.cursorTagId == liveTagId:
        n.into:                                 # (live ...)
          while n.hasMore:
            if not n.isTagLit or n.cursorTagId != modTagId:
              raiseAssert infile & ": expected (mod …)"
            n.into:                             # (mod ...)
              if not n.isStringLit:
                raiseAssert infile & ": (mod) name must be StringLit"
              let modName = pool.strings[n.strId]
              skip n
              var syms = initHashSet[SymId]()
              while n.hasMore:
                if n.kind != Symbol:
                  raiseAssert infile & ": expected Symbol in (mod)"
                syms.incl n.symId
                skip n
              result.live[modName] = syms
      else:
        raiseAssert infile & ": expected (resolved|live …)"

proc inclResolveKey(keys: var HashSet[string]; s: SymId) =
  if pool.symIsInstantiation(s): keys.incl pool.symWithoutModule(s)

proc resolveKeysOf(a: ModuleAnalysis): HashSet[string] =
  ## The resolve-table keys this module's emit can look up. `translate` maps
  ## only instantiations, and `dce1` records each one in this module's tree: a
  ## use as a root or in `uses`, a declaration's head as an offer.
  result = initHashSet[string]()
  for s in a.roots: result.inclResolveKey s
  for s in a.offers: result.inclResolveKey s
  for owner, deps in pairs(a.uses):
    result.inclResolveKey owner
    for d in deps: result.inclResolveKey d

proc resolvedFor(a: ModuleAnalysis; resolved: ResolveTable): ResolveTable =
  ## The entries of `resolved` this module's emit can look up.
  result = initTable[string, SymId]()
  for key in resolveKeysOf(a):
    if resolved.hasKey(key): result[key] = resolved.getOrQuit(key)

proc computeLiveSet*(xnifFiles: openArray[string]; outdir: string) =
  ## Read every module's `(dce …)` section out of its `.x.nif`, compute the
  ## global resolve table + live sets, and write each module's share to
  ## `<M>.live.nif`: in `outdir`, or next to its `.x.nif` when that is "",
  ## as `rewriteModule` places a `.c.nif`. This is the small serial step in
  ## the split DCE pipeline — small because `readModuleAnalysis` parses one
  ## subtree per module and leaves the bodies on disk.
  var graphs = initTable[string, ModuleAnalysis]()
  for file in xnifFiles:
    let modName = splitModulePath(file).name
    graphs[modName] = readModuleAnalysis(file)

  let resolved = resolveSymbolConflicts(graphs)
  let live = markLive(graphs, resolved)
  for file in xnifFiles:
    let modName = splitModulePath(file).name
    let outfile =
      if outdir.len > 0: outdir / modName & ".live.nif"
      else: file.changeModuleExt ".live.nif"
    # `markLive` seeds every module, so its live set is there.
    writeLiveFile(outfile, resolvedFor(graphs.getOrQuit(modName), resolved),
                  modName, live.getOrQuit(modName))

proc dceEmit*(xnif, liveFile, outdir: string) =
  ## Per-module emit: read `M.x.nif` plus its own `liveFile`, write
  ## `M.c.nif`. Multiple invocations run in parallel under the build
  ## scheduler.
  let ls = readLiveFile(liveFile)
  let modName = splitModulePath(xnif).name
  let liveForMod =
    if ls.live.hasKey(modName): ls.live.getOrQuit(modName)
    else: initHashSet[SymId]()
  rewriteModule(xnif, liveForMod, ls.resolved, outdir)
