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

proc markLive*(moduleGraphs: Table[string, ModuleAnalysis]; resolved: ResolveTable): Table[string, HashSet[SymId]] =
  var worklist = newSeq[SymId](1)
  worklist[0] = pool.syms.getOrIncl(RootSym)

  result = initTable[string, HashSet[SymId]]()

  for m in moduleGraphs.keys:
    result[m] = initHashSet[SymId]()

  while worklist.len > 0:
    let sym = translate(resolved, worklist.pop())
    let moduleName = extractModule(pool.syms[sym])

    # Check if symbol is already live in its owning module
    if not result[moduleName].containsOrIncl(sym):
      # Add symbol to its module's live set
      result[moduleName].incl(sym)

      # Process dependencies from the symbol's own module
      if moduleName in moduleGraphs:
        let graph = moduleGraphs[moduleName]
        if sym in graph.deps:
          for dep in graph.deps[sym]:
            let s = translate(resolved, dep)
            let sowner = extractModule(pool.syms[s])
            # Check if dependency is already live in its owning module
            if s notin result[sowner]:
              worklist.add(s)

proc rewriteModule*(file: string; live: HashSet[SymId]; resolved: ResolveTable) =
  discard "to implement"

proc deadCodeElimination*(files: openArray[string]) =
  var graphs = initTable[string, ModuleAnalysis]()
  for file in files:
    graphs[file] = readModuleAnalysis(file)

  let resolved = resolveSymbolConflicts(graphs)

  let live = markLive(graphs, resolved)
  # TODO: we could do this step in parallel:
  for file in files:
    rewriteModule(file, live[file], resolved)
