#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Concept helpers that are independent of the `Match` object: structural
## comparison of concept requirements, `Self` typevar collection and the
## name-based symbol lookups used during concept matching. `sigmatch` builds
## the actual matching logic on top of these.

import std / [sets, tables, assertions]

include ".." / lib / nifprelude
include ".." / lib / compat2

import nimony_model, decls, programs, semdata, typeprops, features, symtabs, conceptcache
import ".." / lib / symparser

proc isConceptType*(a: Cursor): bool {.inline.} =
  a.kind == Symbol and isConceptSym(a.symId)

proc conceptTargetNeedsStrictCheck*(a: Cursor): bool =
  ## Standalone concepts keep lenient matching for generic typevars and for
  ## all types except user `distinct` types. Distinct types must satisfy
  ## requirements structurally so they cannot inherit base-type operators
  ## silently; other types (including `string` objects and ranges) keep the
  ## legacy acceptance until full requirement matching is complete.
  if a.kind == DotToken:
    return false
  if a.kind == Symbol:
    let res = tryLoadSym(a.symId)
    if res.status == LacksNothing and res.decl.symKind == TypevarY:
      return false
  var t = a
  if t.kind == Symbol:
    t = typeImpl(t.symId)
  t.typeKind == DistinctT

proc conceptRoutineBasename*(routine: Cursor): StrId =
  var prc = routine
  assert prc.symKind in RoutineKinds
  inc prc
  assert prc.kind == SymbolDef
  var name = pool.syms[prc.symId]
  extractBasename(name)
  pool.strings.getOrIncl(name)

proc collectSelfSymsInType*(typ: Cursor; result: var seq[SymId]) =
  ## Collect every `Self` typevar referenced inside a type tree.
  var typ = typ
  case typ.kind
  of Symbol:
    let res = tryLoadSym(typ.symId)
    if res.status == LacksNothing and res.decl.symKind == TypevarY:
      if typ.symId notin result:
        result.add typ.symId
  of ParLe:
    typ.loopInto:
      collectSelfSymsInType(typ, result)
      skip typ
  else:
    discard

proc conceptSelfSymFromSlot*(body: Cursor): SymId =
  let selfSlot = conceptSelfSlot(body)
  if selfSlot.symKind != TypevarY:
    return SymId(0)
  var s = selfSlot
  inc s
  if s.kind == SymbolDef:
    s.symId
  else:
    SymId(0)

proc conceptSelfSyms*(body: Cursor; routine: Cursor): seq[SymId] =
  ## The `Self` slot in the concept header and the `Self` referenced in
  ## requirement signatures can be different syms after sema; bind them all.
  result = @[]
  var n = routine
  skipToParams n
  if n.substructureKind == ParamsU:
    # `into` advances `n` past the params subtree, landing on the return type.
    n.into ParamsU:
      while n.hasMore:
        let param = takeLocal(n, SkipFinalParRi)
        collectSelfSymsInType(param.typ, result)
  else:
    skip n # void params slot
  collectSelfSymsInType(n, result) # n now at the return type
  let headerSelf = conceptSelfSymFromSlot(body)
  if headerSelf != SymId(0) and headerSelf notin result:
    result.add headerSelf

iterator visibleNamedSyms*(c: ptr SemContext; basename: StrId): SymId {.sideEffect.} =
  let ignoreStyle = IgnoreStyleFeature in c.features
  var it = c.currentScope
  while it.up != nil:
    it = it.up
  for k in stylesOfScope(it, basename, ignoreStyle):
    for sym in it.tab.getOrDefault(k):
      yield sym.name
  for realName in stylesOfImport(c.importTab, basename, ignoreStyle):
    for moduleId in c.importTab.getOrDefault(realName):
      let m = addr c.importedModules.getOrQuit(moduleId)
      for k in stylesOfIface(m[].iface, realName, ignoreStyle):
        for defId in m[].iface.getOrDefault(k):
          yield defId

iterator conceptRoutineCandidates*(c: ptr SemContext; conceptSym: SymId; basename: StrId): SymId {.sideEffect.} =
  ## All routines named `basename` that could satisfy a concept requirement:
  ## the defining module's syms, every imported interface, and the visible
  ## scope. Deduplicated across those sources.
  var seen = initHashSet[SymId]()
  if c != nil:
    let ignoreStyle = IgnoreStyleFeature in c.features
    if conceptSym != SymId(0):
      let modSuffix = extractModule(pool.syms[conceptSym])
      if modSuffix != "":
        for cand in loadSyms(modSuffix, basename):
          if not seen.containsOrIncl(cand):
            yield cand
      for _, im in c.importedModules:
        for k in stylesOfIface(im.iface, basename, ignoreStyle):
          for defId in im.iface.getOrDefault(k):
            if not seen.containsOrIncl(defId):
              yield defId
    for cand in visibleNamedSyms(c, basename):
      if not seen.containsOrIncl(cand):
        yield cand

proc collectConceptRoutineCandidates*(c: ptr SemContext; conceptSym: SymId; basename: StrId): seq[SymId] =
  let (hit, cached) = tryCandidatesFromCache(c, conceptSym, basename)
  if hit:
    return cached
  result = default(seq[SymId])
  for cand in conceptRoutineCandidates(c, conceptSym, basename):
    result.add cand
  storeCandidates(c, conceptSym, basename, result)

proc routineHasNoSideEffect*(routine: Cursor): bool {.inline.} =
  let r = asRoutine(routine)
  whichEffect(routine.stmtKind, r.pragmas) == HasNoSideEffect

proc conceptRoutineKindsCompatible*(requirement, implementation: SymKind;
                                   implementationDecl: Cursor = default(Cursor)): bool {.inline.} =
  ## A `func` or `template` implementation may satisfy a `proc` requirement.
  ## A `proc` with the `noSideEffect` pragma may satisfy a `func` requirement.
  if requirement == implementation:
    return true
  if requirement == ProcY and implementation in {FuncY, TemplateY}:
    return true
  if requirement == FuncY and implementation == ProcY:
    if cursorIsNil(implementationDecl):
      return false
    return routineHasNoSideEffect(implementationDecl)
  false

proc conceptRoutinesEquivalentKinds*(a, b: SymKind): bool {.inline.} =
  ## For deduplicating concept requirements that differ only by proc/func.
  conceptRoutineKindsCompatible(a, b) or conceptRoutineKindsCompatible(b, a)

proc sameConceptRoutineParamTypes*(aParams, bParams: Cursor): bool =
  var a = aParams
  var b = bParams
  if a.substructureKind != ParamsU or b.substructureKind != ParamsU:
    return false
  a.into ParamsU:
    b.into ParamsU:
      while a.hasMore and b.hasMore:
        let aTyp = takeLocal(a, SkipFinalParRi).typ
        let bTyp = takeLocal(b, SkipFinalParRi).typ
        if not sameTreesButIgnoreSymIds(aTyp, bTyp):
          return false
      return not a.hasMore and not b.hasMore
  false

proc sameConceptRoutineTrees*(requirement, candidate: Cursor;
                              equivKinds = false): bool =
  ## Compare concept routine requirements by basename, kind, and signature shape.
  ## Parameter names are ignored; only types and return type matter.
  if requirement.symKind notin RoutineKinds or candidate.symKind notin RoutineKinds:
    return false
  let kindsOk = if equivKinds:
    conceptRoutinesEquivalentKinds(requirement.symKind, candidate.symKind)
  else:
    conceptRoutineKindsCompatible(requirement.symKind, candidate.symKind, candidate)
  if not kindsOk:
    return false
  if conceptRoutineBasename(requirement) != conceptRoutineBasename(candidate):
    return false
  var rReq = requirement
  var rCand = candidate
  skipToParams rReq
  skipToParams rCand
  # `sameConceptRoutineParamTypes` reads copies, so `rReq`/`rCand` stay on the
  # params slot; skip past it to reach the return type.
  if not sameConceptRoutineParamTypes(rReq, rCand):
    return false
  skip rReq, AnyType
  skip rCand, AnyType
  sameTreesButIgnoreSymIds(rReq, rCand)

proc conceptRequirementInBody*(routine: Cursor; actualBody: Cursor): bool =
  for _, req in conceptHierarchyRoutines(actualBody):
    if sameConceptRoutineTrees(routine, req):
      return true
  false
