#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Dead code elimination and generic instance merging.

import std / [os, tables, hashes, sets, assertions, syncio]
include ".." / lib / nifprelude
include ".." / lib / compat2

import ".." / lib / symparser
import dce1, dce_inliner
import ".." / nifc / [nifc_model]

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
      let offerName = pool.syms[offer]
      let key = removeModule(offerName)
      let existing = result.getOrDefault(key, SymId(0))
      if existing == SymId(0) or offerName < pool.syms[existing]:
        result[key] = offer

proc translate(resolved: ResolveTable; sym: SymId): SymId =
  let symName = pool.syms[sym]
  if isInstantiation(symName):
    let key = removeModule(symName)
    result = resolved.getOrDefault(key, sym)
  else:
    result = sym

proc markLive(moduleGraphs: Table[string, ModuleAnalysis]; resolved: ResolveTable): Table[string, HashSet[SymId]] =
  ## Pure reachability: any sym reachable from a root via the use-graph
  ## is live, regardless of `.inline` pragma. The inliner is a separate
  ## concern — whether or not it ends up splicing every call to a given
  ## `.inline` proc does not change that proc's liveness here. (The
  ## former optimization that dropped fully-inlinable `.inline` decls
  ## from the live set coupled the two passes by *predicting* the
  ## inliner's behaviour from a pragma; any inliner policy that
  ## declined some splices — depth caps, body-size budgets, unsupported
  ## call shapes — silently broke that prediction and produced
  ## dangling references at nifc-time. Decoupling here lets the inliner
  ## evolve without correctness risk; a separate post-emit pass can
  ## recover the size win for procs that ended up fully inlined out.)
  var worklist = newSeq[SymId](0)
  result = initTable[string, HashSet[SymId]]()

  for k, m in moduleGraphs:
    result[k] = initHashSet[SymId]()
    for root in m.roots:
      worklist.add(root)

  while worklist.len > 0:
    let sym = translate(resolved, worklist.pop())
    let moduleName = extractModule(pool.syms[sym])
    assert moduleName.len > 0, "moduleName is empty for " & pool.syms[sym]

    if not result.getOrQuit(moduleName).containsOrIncl(sym):
      if moduleName in moduleGraphs:
        let graph = moduleGraphs.getOrQuit(moduleName)
        if sym in graph.uses:
          for dep in graph.uses.getOrQuit(sym):
            let translated = translate(resolved, dep)
            let depOwner = extractModule(pool.syms[translated])
            if depOwner.len == 0: continue
            if translated notin result.getOrQuit(depOwner):
              worklist.add(translated)

template toNifcName(sym: SymId): SymId = sym

type
  LiveSet* = object
    resolved*: ResolveTable
    live*: Table[string, HashSet[SymId]]

const
  liveTag    = "live"      # `(live (sym Symbol Symbol …))` — per-module live syms
  resolveTag = "resolved"  # `(resolved (kv String Symbol)*)` — generic-instance picks
  modTag     = "mod"       # `(mod String (sym …)*)` — block per module
  symTag     = "sym"

proc writeLiveFile*(outfile: string; resolved: ResolveTable;
                    live: Table[string, HashSet[SymId]]) =
  ## Serialize the global DCE result to a single file consumed by all
  ## downstream per-module `drop` invocations. Symbols are written with
  ## their full module suffix (no abbreviation): the dotted-suffix
  ## shortcut expands using the reader's `thisModule` which is derived
  ## from the filename, but this file aggregates symbols from many
  ## modules — only one expansion would be correct, all the others would
  ## be wrong.
  var b = nifbuilder.open(outfile)
  b.withTree "stmts":
    b.withTree resolveTag:
      for key, winner in pairs(resolved):
        b.withTree "kv":
          b.addStrLit key
          b.addSymbol pool.syms[winner], ""
    b.withTree liveTag:
      for modName, syms in pairs(live):
        b.withTree modTag:
          b.addStrLit modName
          for s in syms:
            b.addSymbol pool.syms[s], ""
  b.close()

proc readLiveFile*(infile: string): LiveSet =
  var buf = parseFromFile(infile)
  var n = beginRead(buf)
  result = LiveSet(
    resolved: initTable[string, SymId](),
    live: initTable[string, HashSet[SymId]]())
  if n.stmtKind != StmtsS:
    raiseAssert infile & ": expected (stmts ...)"
  let liveTagId = pool.tags.getOrIncl(liveTag)
  let resolveTagId = pool.tags.getOrIncl(resolveTag)
  let modTagId = pool.tags.getOrIncl(modTag)
  n.into:                                       # (stmts ...)
    while n.hasMore:
      if n.kind != ParLe:
        raiseAssert infile & ": expected ParLe"
      if n.tag == resolveTagId:
        n.into:                                 # (resolved ...)
          while n.hasMore:
            if n.kind == ParLe and n.substructureKind == KvU:
              n.into:                           # (kv ...)
                if n.kind != StringLit:
                  raiseAssert infile & ": kv key must be StringLit"
                let key = pool.strings[n.litId]
                skip n
                if n.kind != Symbol:
                  raiseAssert infile & ": kv value must be Symbol"
                result.resolved[key] = n.symId
                skip n
                if n.hasMore:
                  raiseAssert infile & ": expected ')' closing kv"
            else:
              raiseAssert infile & ": expected (kv …)"
      elif n.tag == liveTagId:
        n.into:                                 # (live ...)
          while n.hasMore:
            if n.kind != ParLe or n.tag != modTagId:
              raiseAssert infile & ": expected (mod …)"
            n.into:                             # (mod ...)
              if n.kind != StringLit:
                raiseAssert infile & ": (mod) name must be StringLit"
              let modName = pool.strings[n.litId]
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

# ---- Pass 1: cross-module inliner ---------------------------------------
# Walks `(stmts …)` / `(scope …)` / `(var …)` children looking for spliceable
# calls and uses `dce_inliner.trySplice` / `trySpliceVarInit` to expand them.
# No liveness, no generic-instance translation — those happen in pass 2.

proc trInline(dest: var TokenBuf; n: var Cursor; inliner: var InlinerCtx) =
  case n.kind
  of ParLe:
    let stmtKind = n.stmtKind
    case stmtKind
    of VarS, ConstS, GvarS, TvarS:
      # Bound-call form `(var :tmp <pragmas> <type> (call f arg…))` is
      # how xelim and nifcgen's complex-init path bind a call result; the
      # var-init splice rewrites it into a result-target inlining.
      if stmtKind == VarS:
        var probe = n
        inc probe
        if probe.kind == SymbolDef:
          inc probe; skip probe; skip probe
        var calleeSym = SymId(0)
        if probe.kind == ParLe and probe.stmtKind == CallS:
          inc probe
          if probe.kind == Symbol:
            calleeSym = probe.symId
        if calleeSym != SymId(0):
          var spliced = createTokenBuf(32)
          let nEmitted = trySpliceVarInit(inliner, spliced, n)
          if nEmitted > 0:
            inliner.inProgress.incl calleeSym
            var inner = beginRead(spliced)
            for _ in 0 ..< nEmitted:
              trInline(dest, inner, inliner)
            endRead(inner)
            inliner.inProgress.excl calleeSym
            return
      dest.takeToken n
      while n.hasMore:
        trInline(dest, n, inliner)
      dest.takeToken n
    of StmtsS, ScopeS:
      # Direct-stmt-position call: candidate for the void splice.
      dest.takeToken n
      while n.hasMore:
        if n.kind == ParLe and n.stmtKind == CallS:
          var probe = n
          inc probe
          let calleeSym =
            if probe.kind == Symbol: probe.symId else: SymId(0)
          var spliced = createTokenBuf(32)
          let nEmitted = trySplice(inliner, spliced, n)
          if nEmitted > 0:
            if calleeSym != SymId(0):
              inliner.inProgress.incl calleeSym
            var inner = beginRead(spliced)
            for _ in 0 ..< nEmitted:
              trInline(dest, inner, inliner)
            endRead(inner)
            if calleeSym != SymId(0):
              inliner.inProgress.excl calleeSym
            continue
        trInline(dest, n, inliner)
      dest.takeToken n
    else:
      dest.takeToken n
      while n.hasMore:
        trInline(dest, n, inliner)
      dest.takeToken n
  of ParRi:
    raiseAssert "ParRi should not be encountered here"
  else:
    dest.takeToken n

# ---- Pass 2: liveness-driven drop + generic-instance translation --------
# Walks the *post-inline* buffer, drops decls not in `alive`, and renames
# losing-instance syms to the canonical pick from `resolved`.

proc trDrop(dest: var TokenBuf; n: var Cursor; alive: HashSet[SymId];
            resolved: ResolveTable) =
  case n.kind
  of ParLe:
    let stmtKind = n.stmtKind
    case stmtKind
    of TypeS:
      # Types: ensure **consistency** (translate to canonical) so two
      # modules that offer the same generic instance still typecheck
      # against each other after merging.
      let head = n.load()
      dest.add head
      n.into:
        if n.kind == SymbolDef:
          let def = n.symId
          let t = translate(resolved, def)
          dest.addSymDef t.toNifcName, n.info
          skip n
          while n.hasMore:
            trDrop(dest, n, alive, resolved)
        else:
          while n.hasMore:
            trDrop(dest, n, alive, resolved)
      dest.addParRi()
    of ProcS, VarS, ConstS, GvarS, TvarS:
      let head = n.load()
      inc n
      if n.kind == SymbolDef:
        let def = n.symId
        if isLocalName(pool.syms[def]):
          dest.add head
          dest.addSymDef def.toNifcName, n.info
          inc n
          while n.hasMore:
            trDrop(dest, n, alive, resolved)
          dest.takeToken n
        elif alive.contains(def):
          let t = translate(resolved, def)
          if t != def:
            # Lost the generic-instance race — emit an `(imp …)` extern
            # decl referencing the canonical pick from the winning module.
            dest.add parLeToken(pool.tags.getOrIncl("imp"), head.info)
            dest.add head
            dest.addSymDef t.toNifcName, n.info
            inc n
            var untilBody = if stmtKind == ProcS: 3 else: 2
            while n.hasMore and untilBody > 0:
              dec untilBody
              trDrop(dest, n, alive, resolved)
            skip n
            dest.addDotToken()
            assert n.kind == ParRi
            dest.takeToken n
            dest.addParRi()
          else:
            dest.add head
            dest.addSymDef def.toNifcName, n.info
            inc n
            while n.hasMore:
              trDrop(dest, n, alive, resolved)
            dest.takeToken n
        else:
          # Dead — drop the whole decl.
          inc n
          while n.hasMore: skip n
          inc n
      else:
        dest.add head
        while n.hasMore:
          trDrop(dest, n, alive, resolved)
        dest.takeToken n
    else:
      dest.takeToken n
      while n.hasMore:
        trDrop(dest, n, alive, resolved)
      dest.takeToken n
  of Symbol:
    let t = translate(resolved, n.symId)
    dest.addSymUse t.toNifcName, n.info
    inc n
  of SymbolDef:
    let t = translate(resolved, n.symId)
    dest.addSymDef t.toNifcName, n.info
    inc n
  of UnknownToken, EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit:
    dest.takeToken n
  of ParRi: raiseAssert "ParRi should not be encountered here"

proc crossInline*(xnif, outXnif, outDce: string) =
  ## Phase 2 (per module): cross-module inliner. Reads `M.x.nif` plus
  ## whatever foreign modules' artefacts the inliner lazy-loads on demand
  ## (`dce_inliner.loadForeign`/`loadForeignAnalysis`), splices every
  ## inlinable call, then re-analyzes the result so dceLive sees the
  ## post-inline use-graph.
  var buf = parseFromFile(xnif)
  let mp = splitModulePath(xnif)
  let modName = mp.name
  # The inliner consults `c.infos` for `.inline` thresholds. Seed it
  # with this module's own pre-inline analysis from `hexer c`; foreign
  # entries get filled in lazily by `loadForeignAnalysis`.
  var graphs = initTable[string, ModuleAnalysis]()
  graphs[modName] = readModuleAnalysis(xnif.changeModuleExt ".dce.nif")
  # Cross-module pass: foreign `.inline` bodies have already been
  # cascaded against their same-module callees by `intraModuleInline`.
  # A deep chain here only happens across module boundaries — rarely
  # worth the explosion. Cap at 2 levels. Safe because liveness no
  # longer predicts inlining: any call the cap leaves behind references
  # a live decl by construction.
  var inliner = initInlinerCtx(modName, addr buf, addr graphs, mp.dir,
                               maxDepth = 2, counterPrefix = "d")
  collectProcBodies inliner
  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len)
  trInline(dest, n, inliner)
  endRead(buf)
  try:
    writeFile(dest, outXnif, OnlyIfChanged)
  except:
    quit "could not write file: " & outXnif
  # Re-analyse the post-inline buffer so dceLive operates on the actual
  # post-inline use-graph (no prediction).
  writeDceOutput dest, outDce, "." & modName

proc dropDead*(xinif, liveFile, outdir: string) =
  ## Phase 4 (per module): DCE drop. Reads the post-inline `M.xi.nif`
  ## plus the shared `liveFile`, drops decls not in the live set,
  ## applies generic-instance translation, writes `M.c.nif`.
  let ls = readLiveFile(liveFile)
  let modName = splitModulePath(xinif).name
  let liveForMod =
    if ls.live.hasKey(modName): ls.live.getOrQuit(modName)
    else: initHashSet[SymId]()
  var buf = parseFromFile(xinif)
  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len)
  trDrop(dest, n, liveForMod, ls.resolved)
  endRead(buf)
  let outPath =
    if outdir.len > 0: outdir / modName & ".c.nif"
    else: xinif.changeModuleExt ".c.nif"
  try:
    writeFile(dest, outPath, OnlyIfChanged)
  except:
    quit "could not write file: " & outPath

proc deadCodeElimination*(files: openArray[string]; outdir: string) =
  ## Single-shot pipeline kept for the in-process API (tests, one-off
  ## tools). Runs the four split phases sequentially in one process.
  # Phase 2: per-module cross-inline + post-inline analysis.
  var postInline = initTable[string, ModuleAnalysis]()
  var inlinedBufs = initTable[string, TokenBuf]()
  for file in files:
    let mp = splitModulePath(file)
    let modName = mp.name
    var buf = parseFromFile(file)
    var graphs = initTable[string, ModuleAnalysis]()
    graphs[modName] = readModuleAnalysis(file.changeModuleExt ".dce.nif")
    var inliner = initInlinerCtx(modName, addr buf, addr graphs, mp.dir,
                                 maxDepth = 2, counterPrefix = "d")
    collectProcBodies inliner
    var n = beginRead(buf)
    var dest = createTokenBuf(buf.len)
    trInline(dest, n, inliner)
    endRead(buf)
    postInline[modName] = analyzeModule(dest)
    inlinedBufs[modName] = ensureMove dest

  # Phase 3: global liveness from post-inline analyses.
  let resolved = resolveSymbolConflicts(postInline)
  let live = markLive(postInline, resolved)

  # Phase 4: drop dead, write per-module .c.nif.
  for file in files:
    let modName = splitModulePath(file).name
    var dest = createTokenBuf(64)
    var n = beginRead(inlinedBufs.getOrQuit(modName))
    trDrop(dest, n, live.getOrQuit(modName), resolved)
    endRead(inlinedBufs.getOrQuit(modName))
    let outPath =
      if outdir.len > 0: outdir / modName & ".c.nif"
      else: file.changeModuleExt ".c.nif"
    try:
      writeFile(dest, outPath, OnlyIfChanged)
    except:
      quit "could not write file: " & outPath

proc computeLiveSet*(diFiles: openArray[string]; liveOut: string) =
  ## Phase 3: read per-module *post-inline* `.di.nif` analyses, compute
  ## the global resolve table + live sets, write `liveOut`. Liveness is
  ## pure reachability over the post-inline use-graph — no prediction.
  var graphs = initTable[string, ModuleAnalysis]()
  for file in diFiles:
    let modName = splitModulePath(file).name
    graphs[modName] = readModuleAnalysis(file)

  let resolved = resolveSymbolConflicts(graphs)
  let live = markLive(graphs, resolved)
  writeLiveFile(liveOut, resolved, live)
