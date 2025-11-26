#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Dead code elimination and generic instance merging.

import std / [tables, sets, assertions]
include nifprelude

import symparser, dce1
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
    if not result[moduleName].containsOrIncl(sym):
      # Process dependencies from the symbol's own module
      if moduleName in moduleGraphs:
        let graph = moduleGraphs[moduleName]
        if sym in graph.uses:
          for dep in graph.uses[sym]:
            let s = translate(resolved, dep)
            let sowner = extractModule(pool.syms[s])
            # Check if dependency is already live in its owning module
            if sowner.len > 0:
              assert sowner in result, "sowner is not in result for " & pool.syms[s]
            if sowner.len > 0 and s notin result[sowner]:
              worklist.add(s)

proc toNifcName(sym: SymId): SymId =
  var symName = pool.syms[sym]
  if symName[symName.high] == ExternMarker:
    translateExtern symName
    result = pool.syms.getOrIncl(symName)
  else:
    result = sym

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

    of ImpS:
      dest.takeToken n # Imp
      if n.stmtKind in {ProcS, VarS, ConstS, GvarS, TvarS, TypeS}:
        # prevent the logic for ProcS, etc from being applied to `imp` declarations:
        dest.takeToken n
        while n.kind != ParRi:
          tr dest, n, alive, resolved
        dest.takeToken n
      else:
        while n.kind != ParRi:
          tr dest, n, alive, resolved
        dest.takeToken n
      assert n.kind == ParRi
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

proc rewriteModule(file: string; live: HashSet[SymId]; resolved: ResolveTable) =
  var buf = parseFromFile(file)
  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len)
  tr dest, n, live, resolved
  endRead(buf)
  writeFile(dest, file.changeModuleExt ".c.nif", OnlyIfChanged)

proc deadCodeElimination*(files: openArray[string]) =
  var graphs = initTable[string, ModuleAnalysis]()
  for file in files:
    let modName = splitModulePath(file).name
    graphs[modName] = readModuleAnalysis(file.changeModuleExt ".dce.nif")

  let resolved = resolveSymbolConflicts(graphs)

  let live = markLive(graphs, resolved)
  # TODO: we could do this step in parallel:
  for file in files:
    let modName = splitModulePath(file).name
    rewriteModule(file, live[modName], resolved)
