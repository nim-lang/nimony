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

proc computeRecursiveInlines(graphs: Table[string, ModuleAnalysis]): HashSet[SymId] =
  ## Walks the call graph restricted to `.inline` callees to find any
  ## proc that can reach itself. Those procs *must* stay live: their
  ## recursive call inside the spliced body is left as a normal call
  ## by dce_inliner's cycle guard, so the original proc decl needs to
  ## remain in the emitted IR.
  result = initHashSet[SymId]()

  proc inlineThresholdIsZero(s: SymId): bool =
    let m = extractModule(pool.syms[s])
    if m notin graphs: return false
    let info = graphs[m].inlineInfo.getOrDefault(s, DefaultInlineInfo)
    info.threshold == 0

  proc reachesBack(start, current: SymId; visited: var HashSet[SymId]): bool =
    if visited.containsOrIncl(current): return false
    if not inlineThresholdIsZero(current): return false
    let m = extractModule(pool.syms[current])
    if current notin graphs[m].uses: return false
    for dep in graphs[m].uses[current]:
      if dep == start: return true
      if reachesBack(start, dep, visited): return true
    return false

  for modName, ma in graphs:
    for procSym, info in ma.inlineInfo:
      if info.threshold == 0:
        var visited = initHashSet[SymId]()
        if reachesBack(procSym, procSym, visited):
          result.incl procSym

proc isAlwaysInlinedFor(callerMod: string; dep: SymId;
                        depOwner: string;
                        graphs: Table[string, ModuleAnalysis];
                        recursiveInlines: HashSet[SymId]): bool =
  ## Whether `dep` will be fully inlined out of every call site in
  ## `callerMod`, leaving no need to emit a callable symbol for it.
  ## Treats `dep` as eliminable iff:
  ##   - it is `.inline` (threshold == 0); and
  ##   - it isn't recursive (directly or via other `.inline` procs).
  ##
  ## Cross-module is now allowed: dce2's `loadForeign` lazily fetches
  ## the foreign module's `.x.nif` so the splice fires wherever the
  ## call shape is spliceable, and `stringcases.nim` no longer leaves
  ## `(call …)` in `(elif …)` conditions (it binds each cond to a
  ## `(var :tcc.N … (call …))` first so the bound-form splice reaches
  ## it). Every `.inline` call site is now spliceable in principle, so
  ## the safety-net cross-module restriction is gone.
  discard callerMod
  if dep in recursiveInlines: return false
  if depOwner notin graphs: return false
  let info = graphs[depOwner].inlineInfo.getOrDefault(dep, DefaultInlineInfo)
  result = info.threshold == 0

proc enqueueDep(dep: SymId; callerMod: string;
                graphs: Table[string, ModuleAnalysis]; resolved: ResolveTable;
                recursiveInlines: HashSet[SymId];
                live: Table[string, HashSet[SymId]];
                worklist: var seq[SymId];
                expanded: var HashSet[SymId]) =
  ## Treats `dep` as a use-edge from a caller in `callerMod`. If `dep`
  ## is always-inlined into the caller, mark `dep` itself non-live and
  ## propagate `dep`'s own uses upward instead — they become effective
  ## uses of the caller once the splice runs. Otherwise mark `dep` live.
  let translated = translate(resolved, dep)
  let depOwner = extractModule(pool.syms[translated])
  if depOwner.len == 0: return

  if isAlwaysInlinedFor(callerMod, translated, depOwner, graphs,
                        recursiveInlines):
    # Always-inlined: don't mark live; expand transparently.
    if expanded.containsOrIncl(translated): return  # avoid re-expanding
    if translated in graphs[depOwner].uses:
      for innerDep in graphs[depOwner].uses.getOrQuit(translated):
        # The splice happens in `callerMod`; uses of `dep`'s body land
        # in `callerMod` after splicing, so propagate them with that
        # caller module.
        enqueueDep(innerDep, callerMod, graphs, resolved,
                   recursiveInlines, live, worklist, expanded)
    return

  if translated notin live.getOrQuit(depOwner):
    worklist.add(translated)

proc markLive(moduleGraphs: Table[string, ModuleAnalysis]; resolved: ResolveTable): Table[string, HashSet[SymId]] =
  var worklist = newSeq[SymId](0)

  result = initTable[string, HashSet[SymId]]()

  for k, m in moduleGraphs:
    result[k] = initHashSet[SymId]()
    for root in m.roots:
      worklist.add(root)

  let recursiveInlines = computeRecursiveInlines(moduleGraphs)
  var expanded = initHashSet[SymId]()
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
            enqueueDep(dep, moduleName, moduleGraphs, resolved,
                       recursiveInlines, result, worklist, expanded)

template toNifcName(sym: SymId): SymId = sym

proc tr(dest: var TokenBuf; n: var Cursor; alive: HashSet[SymId];
        resolved: ResolveTable; inliner: ptr InlinerCtx) =
  case n.kind
  of ParLe:
    let stmtKind = n.stmtKind
    case stmtKind
    of TypeS:
      # types are fundamentally different from procs when it comes to generic instantiations:
      # We need to ensure **consistency** for types, but for procs we need to ensure **uniqueness**.
      let head = n.load()
      dest.add head
      n.into:
        if n.kind == SymbolDef:
          let def = n.symId
          let t = translate(resolved, def)
          dest.addSymDef t.toNifcName, n.info
          skip n # skip symbol def (atom)
          while n.hasMore:
            tr dest, n, alive, resolved, inliner
        else:
          # let errors propagate:
          while n.hasMore:
            tr dest, n, alive, resolved, inliner
      dest.addParRi()

    of ProcS, VarS, ConstS, GvarS, TvarS:
      # Bound-call form: `(var :tmp <pragmas> <type> (call f arg…))`.
      # When the call is to an inlinable proc, splice it with `tmp` as
      # the result target. The splice emits two stmts (uninitialised var
      # plus a (scope …) holding the inlined body) into a scratch buffer
      # so we can re-process the result with `tr` — nested calls inside
      # the inlined body get spliced too, which is what makes the
      # residual-graph liveness analysis safe.
      if stmtKind == VarS and inliner != nil:
        # Peek into `(var :tmp <pragmas> <type> (call f arg…))` for the
        # callee sym — same cycle guard as the StmtsS/ScopeS path.
        var probe = n
        inc probe                        # past `var` tag
        if probe.kind == SymbolDef:
          inc probe                      # past name
          skip probe                     # pragmas
          skip probe                     # type
        var calleeSym = SymId(0)
        if probe.kind == ParLe and probe.stmtKind == CallS:
          inc probe                      # past `call`
          if probe.kind == Symbol:
            calleeSym = probe.symId
        var spliced = createTokenBuf(32)
        let nEmitted = trySpliceVarInit(inliner[], spliced, n)
        if nEmitted > 0:
          if calleeSym != SymId(0):
            inliner[].inProgress.incl calleeSym
          var inner = beginRead(spliced)
          for _ in 0 ..< nEmitted:
            tr dest, inner, alive, resolved, inliner
          endRead(inner)
          if calleeSym != SymId(0):
            inliner[].inProgress.excl calleeSym
          return
      let head = n.load()
      inc n
      if n.kind == SymbolDef:
        let def = n.symId
        if isLocalName(pool.syms[def]):
          dest.add head
          dest.addSymDef def.toNifcName, n.info
          inc n # skip symbol def
          while n.hasMore:
            tr dest, n, alive, resolved, inliner
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
            while n.hasMore and untilBody > 0:
              dec untilBody
              tr dest, n, alive, resolved, inliner
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
            while n.hasMore:
              tr dest, n, alive, resolved, inliner
            dest.takeToken n
        else:
          # skip it, it's dead
          inc n # skip symbol def
          while n.hasMore: skip n
          inc n
      else:
        # let errors propagate:
        dest.add head
        while n.hasMore:
          tr dest, n, alive, resolved, inliner
        dest.takeToken n
    of StmtsS, ScopeS:
      # Statement-list parent: each child is in statement position, so
      # candidates for the call splice. Anywhere else (e.g. an `(elif
      # cond ...)` slot), a `(call ...)` is an *expression* and replacing
      # it with a `(scope ...)` would produce invalid IR.
      dest.takeToken n
      while n.hasMore:
        if inliner != nil and n.kind == ParLe and n.stmtKind == CallS:
          # Peek the callee sym so we can guard against recursive
          # `.inline` (direct or mutual). The guard is set around the
          # recursive `tr` over the splice scratch buf — without it,
          # `(call A)` inside A's spliced body would splice A again,
          # forever.
          var probe = n
          inc probe
          let calleeSym =
            if probe.kind == Symbol: probe.symId else: SymId(0)
          var spliced = createTokenBuf(32)
          let nEmitted = trySplice(inliner[], spliced, n)
          if nEmitted > 0:
            if calleeSym != SymId(0):
              inliner[].inProgress.incl calleeSym
            var inner = beginRead(spliced)
            for _ in 0 ..< nEmitted:
              tr dest, inner, alive, resolved, inliner
            endRead(inner)
            if calleeSym != SymId(0):
              inliner[].inProgress.excl calleeSym
            continue
        tr dest, n, alive, resolved, inliner
      dest.takeToken n
    else:
      dest.takeToken n
      while n.hasMore:
        tr dest, n, alive, resolved, inliner
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

proc rewriteModule(file: string; live: HashSet[SymId]; resolved: ResolveTable;
                   outdir: string; infos: ptr Table[string, ModuleAnalysis] = nil) =
  var buf = parseFromFile(file)
  let mp = splitModulePath(file)
  let modName = mp.name
  var inliner = initInlinerCtx(modName, addr buf, infos, mp.dir)
  if infos != nil:
    collectProcBodies inliner
  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len)
  tr dest, n, live, resolved, (if infos != nil: addr inliner else: nil)
  endRead(buf)
  let outPath =
    if outdir.len > 0:
      outdir / modName & ".c.nif"
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
    rewriteModule(file, live.getOrQuit(modName), resolved, outdir, addr graphs)

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
  # Seed with this module's own `.dce.nif`. Cross-module callees are
  # handled by `dce_inliner.loadForeign`, which lazy-loads the foreign
  # module's `.dce.nif` (for `.inline` lookups) and `.x.nif` (for the
  # body to splice) the first time we ask about a callee from there.
  var graphs = initTable[string, ModuleAnalysis]()
  let dceFile = xnif.changeModuleExt ".dce.nif"
  graphs[modName] = readModuleAnalysis(dceFile)
  rewriteModule(xnif, liveForMod, ls.resolved, outdir, addr graphs)
