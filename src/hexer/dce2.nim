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
import dce1
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

    # Check if symbol is already live in its owning module
    if not result.getOrQuit(moduleName).containsOrIncl(sym):
      # Process dependencies from the symbol's own module
      if moduleName in moduleGraphs:
        let graph = moduleGraphs.getOrQuit(moduleName)
        if sym in graph.uses:
          for dep in graph.uses.getOrQuit(sym):
            let s = translate(resolved, dep)
            let sowner = extractModule(pool.syms[s])
            # Check if dependency is already live in its owning module
            if sowner.len > 0:
              assert sowner in result, "sowner is not in result for " & pool.syms[s]
            if sowner.len > 0 and s notin result.getOrQuit(sowner):
              worklist.add(s)

template toNifcName(sym: SymId): SymId = sym

proc tr(dest: var TokenBuf; n: var Cursor; alive: HashSet[SymId]; resolved: ResolveTable) =
  case n.kind
  of ParLe:
    let stmtKind = n.stmtKind
    case stmtKind
    of TypeS:
      # types are fundamentally different from procs when it comes to generic instantiations:
      # We need to ensure **consistency** for types, but for procs we need to ensure **uniqueness**.
      let head = n.load()
      inc n
      if n.kind == SymbolDef:
        let def = n.symId
        let t = translate(resolved, def)
        dest.add head
        dest.addSymDef t.toNifcName, n.info
        inc n # skip symbol def
        while n.kind != ParRi:
          tr dest, n, alive, resolved
        dest.takeToken n
      else:
        # let errors propagate:
        dest.add head
        while n.kind != ParRi:
          tr dest, n, alive, resolved
        dest.takeToken n

    of ProcS, VarS, ConstS, GvarS, TvarS:
      let head = n.load()
      inc n
      if n.kind == SymbolDef:
        let def = n.symId
        if isLocalName(pool.syms[def]):
          dest.add head
          dest.addSymDef def.toNifcName, n.info
          inc n # skip symbol def
          while n.kind != ParRi:
            tr dest, n, alive, resolved
          dest.takeToken n
        elif alive.contains(def):
          let t = translate(resolved, def)
          if t != def:
            # we are a loser and need to add an `extern` declaration:
            dest.add parLeToken(pool.tags.getOrIncl("imp"), head.info)

            dest.add head
            dest.addSymDef t.toNifcName, n.info
            inc n # skip symbol def
            var untilBody = if stmtKind == ProcS: 3 else: 2 # pragmas type (for procs: return type)
            while n.kind != ParRi and untilBody > 0:
              dec untilBody
              tr dest, n, alive, resolved
            skip n # skip the body
            # replace it with an empty body:
            dest.addDotToken()
            assert n.kind == ParRi
            dest.takeToken n
            dest.addParRi() # also close the "imp" declaration
          else:
            dest.add head
            dest.addSymDef def.toNifcName, n.info
            inc n # skip symbol def
            while n.kind != ParRi:
              tr dest, n, alive, resolved
            dest.takeToken n
        else:
          # skip it, it's dead
          inc n # skip symbol def
          while n.kind != ParRi: skip n
          inc n
      else:
        # let errors propagate:
        dest.add head
        while n.kind != ParRi:
          tr dest, n, alive, resolved
        dest.takeToken n
    else:
      dest.takeToken n
      while n.kind != ParRi:
        tr dest, n, alive, resolved
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

proc rewriteModule(file: string; live: HashSet[SymId]; resolved: ResolveTable; outdir: string) =
  var buf = parseFromFile(file)
  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len)
  tr dest, n, live, resolved
  endRead(buf)
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
  ## Single-shot DCE: read all .dce.nif analyses, compute global liveness,
  ## then sequentially rewrite each module's .x.nif to .c.nif. Kept for
  ## the single-process API; the build pipeline now goes through the split
  ## `computeLiveSet` + `dceEmit` pair so the per-module rewrite step
  ## parallelizes across modules.
  var graphs = initTable[string, ModuleAnalysis]()
  for file in files:
    let modName = splitModulePath(file).name
    graphs[modName] = readModuleAnalysis(file.changeModuleExt ".dce.nif")

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
                    live: Table[string, HashSet[SymId]]) =
  ## Serialize the global DCE result to a single file consumed by all
  ## downstream `dceEmit` invocations. Symbols are written with their
  ## full module suffix (no abbreviation): the dotted-suffix shortcut
  ## expands using the reader's `thisModule` which is derived from the
  ## filename, but this file aggregates symbols from many modules — only
  ## one expansion would be correct, all the others would be wrong. So
  ## we pay the file-size cost rather than mis-expand.
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
  inc n
  let liveTagId = pool.tags.getOrIncl(liveTag)
  let resolveTagId = pool.tags.getOrIncl(resolveTag)
  let modTagId = pool.tags.getOrIncl(modTag)
  while n.kind != ParRi:
    if n.kind != ParLe:
      raiseAssert infile & ": expected ParLe"
    if n.tag == resolveTagId:
      inc n
      while n.kind != ParRi:
        if n.kind == ParLe and n.substructureKind == KvU:
          inc n  # past `kv`
          if n.kind != StringLit:
            raiseAssert infile & ": kv key must be StringLit"
          let key = pool.strings[n.litId]
          inc n
          if n.kind != Symbol:
            raiseAssert infile & ": kv value must be Symbol"
          result.resolved[key] = n.symId
          inc n
          if n.kind != ParRi:
            raiseAssert infile & ": expected ')' closing kv"
          inc n
        else:
          raiseAssert infile & ": expected (kv …)"
      inc n
    elif n.tag == liveTagId:
      inc n
      while n.kind != ParRi:
        if n.kind != ParLe or n.tag != modTagId:
          raiseAssert infile & ": expected (mod …)"
        inc n
        if n.kind != StringLit:
          raiseAssert infile & ": (mod) name must be StringLit"
        let modName = pool.strings[n.litId]
        inc n
        var syms = initHashSet[SymId]()
        while n.kind != ParRi:
          if n.kind != Symbol:
            raiseAssert infile & ": expected Symbol in (mod)"
          syms.incl n.symId
          inc n
        result.live[modName] = syms
        inc n
      inc n
    else:
      raiseAssert infile & ": expected (resolved|live …)"

proc computeLiveSet*(dceFiles: openArray[string]; liveOut: string) =
  ## Read the per-module `.dce.nif` analyses, compute the global
  ## resolve table + live sets, and write them to `liveOut`. This is the
  ## small serial step in the split DCE pipeline.
  var graphs = initTable[string, ModuleAnalysis]()
  for file in dceFiles:
    let modName = splitModulePath(file).name
    graphs[modName] = readModuleAnalysis(file)

  let resolved = resolveSymbolConflicts(graphs)
  let live = markLive(graphs, resolved)
  writeLiveFile(liveOut, resolved, live)

proc dceEmit*(xnif, liveFile, outdir: string) =
  ## Per-module emit: read `M.x.nif` plus the shared `liveFile`, write
  ## `M.c.nif`. Multiple invocations run in parallel under the build
  ## scheduler.
  let ls = readLiveFile(liveFile)
  let modName = splitModulePath(xnif).name
  let liveForMod =
    if ls.live.hasKey(modName): ls.live.getOrQuit(modName)
    else: initHashSet[SymId]()
  rewriteModule(xnif, liveForMod, ls.resolved, outdir)
