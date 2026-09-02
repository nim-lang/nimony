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
  a.isSymbol and isConceptSym(a.symId)

proc conceptRoutineBasename*(routine: Cursor): StrId =
  var prc = routine
  assert prc.symKind in RoutineKinds
  inc prc
  assert prc.isSymbolDef
  var name = pool.syms[prc.symId]
  extractBasename(name)
  pool.strings.getOrIncl(name)

proc conceptSelfSymFromSlot*(body: Cursor): SymId =
  let selfSlot = conceptSelfSlot(body)
  if selfSlot.symKind != TypevarY:
    return SymId(0)
  var s = selfSlot
  inc s
  if s.isSymbolDef:
    s.symId
  else:
    SymId(0)

proc isConceptSelfSym(s: SymId; headerSelf: SymId): bool =
  ## The `Self` slot in the concept header and the `Self` referenced in
  ## requirement signatures can be different syms after sema, so match the
  ## name as well. Other typevars in a signature — the requirement's own
  ## generic parameters, or an enclosing generic's — are not `Self` and stay
  ## open.
  if s == headerSelf:
    return true
  let res = tryLoadSym(s)
  if res.status != LacksNothing or res.decl.symKind != TypevarY:
    return false
  var name = pool.syms[s]
  extractBasename(name)
  name == "Self"

proc collectSelfSymsInType(typ: Cursor; headerSelf: SymId; result: var seq[SymId]) =
  var typ = typ
  case typ.kind
  of Symbol:
    if typ.symId notin result and isConceptSelfSym(typ.symId, headerSelf):
      result.add typ.symId
  of TagLit:
    typ.loopInto:
      collectSelfSymsInType(typ, headerSelf, result)
      skip typ
  else:
    discard

proc collectOpenTypevars*(typ: Cursor; result: var HashSet[SymId]) =
  ## Every typevar symbol referenced inside a type tree.
  var typ = typ
  case typ.kind
  of Symbol:
    if isOpenTypevar(typ):
      result.incl typ.symId
  of TagLit:
    typ.loopInto:
      collectOpenTypevars(typ, result)
      skip typ
  else:
    discard

proc scanSignature(body: Cursor; routine: Cursor;
                   selfSyms: var seq[SymId]; openTvs: var HashSet[SymId]) =
  ## One walk over a requirement's parameter types and its return type,
  ## collecting the `Self` syms it names and every typevar it names.
  let headerSelf = conceptSelfSymFromSlot(body)
  var n = routine
  skipToParams n
  if n.substructureKind == ParamsU:
    # `into` advances `n` past the params subtree, landing on the return type.
    n.into ParamsU:
      while n.hasMore:
        let param = takeLocal(n, SkipFinalParRi)
        collectSelfSymsInType(param.typ, headerSelf, selfSyms)
        collectOpenTypevars(param.typ, openTvs)
  else:
    skip n # void params slot
  collectSelfSymsInType(n, headerSelf, selfSyms) # n now at the return type
  collectOpenTypevars(n, openTvs)

proc conceptSelfSymsInSignature(body: Cursor; routine: Cursor): seq[SymId] =
  ## The `Self` syms the requirement's own signature refers to. Empty means the
  ## requirement says nothing about the checked type.
  result = @[]
  var openTvs = initHashSet[SymId]()
  scanSignature(body, routine, result, openTvs)

proc conceptSelfSyms*(body: Cursor; routine: Cursor): seq[SymId] =
  ## Every `Self` sym a requirement signature refers to, plus the header's.
  result = conceptSelfSymsInSignature(body, routine)
  let headerSelf = conceptSelfSymFromSlot(body)
  if headerSelf != SymId(0) and headerSelf notin result:
    result.add headerSelf

proc conceptRequirementOwnTypevars*(routine: Cursor): seq[SymId] =
  ## The generic parameters a requirement declares for itself, as in
  ## `proc sample[G: HasNext](s: Self; g: var G)`.
  result = @[]
  var tv = asRoutine(routine).typevars
  if tv.substructureKind == TypevarsU:
    tv.loopInto:
      if isTypevarLike(tv.symKind):
        var name = tv
        inc name
        if name.isSymbolDef:
          result.add name.symId
      skip tv

proc conceptRoutineUsesSelf*(body: Cursor; routine: Cursor): bool {.inline.} =
  ## Does the requirement's signature mention `Self`? That is what ties a
  ## requirement to the type being checked; one that does not says nothing
  ## about it.
  conceptSelfSymsInSignature(body, routine).len > 0

proc conceptRoutineTypevars*(body: Cursor; routine: Cursor): seq[SymId] =
  ## The typevars a requirement's signature names, minus the generic parameters
  ## the requirement declares for itself: those are universally quantified and
  ## are never inferred. What is left are the concept's container parameters and
  ## an enclosing generic's.
  var selfSyms: seq[SymId] = @[]
  var open = initHashSet[SymId]()
  scanSignature(body, routine, selfSyms, open)
  for own in conceptRequirementOwnTypevars(routine):
    open.excl own
  result = @[]
  for tv in open: result.add tv

proc substituteTypevars*(dest: var TokenBuf; typ: Cursor; bindings: Table[SymId, Cursor]) =
  ## Copies the type tree `typ` into `dest`, replacing every symbol that has a
  ## binding by the bound type. Token-level, like `subs` in sem.nim, but with
  ## no renaming: this is for probing, nothing here is declared.
  var n = typ
  case n.kind
  of Symbol:
    let arg = bindings.getOrDefault(n.symId)
    if arg != default(Cursor):
      dest.addSubtree arg
    else:
      dest.addSubtree n
  of TagLit:
    dest.addParLe(n.cursorTagId, n.info)
    n.into:
      while n.hasMore:
        substituteTypevars(dest, n, bindings)
        skip n
      dest.addParRi(n.endInfo)
  else:
    dest.addSubtree n

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

iterator conceptRoutineCandidates*(c: ptr SemContext; conceptSym: SymId; basename: StrId;
                                   typeRoot: SymId): SymId {.sideEffect.} =
  ## All routines named `basename` that could satisfy a concept requirement,
  ## mirroring what a call resolves against: the concept's declaring module,
  ## the checked type's type-bound operations (the module that declares the
  ## type), and what the checking module sees (every imported interface and
  ## the visible scope). Deduplicated across those sources.
  var seen = initHashSet[SymId]()
  if c != nil:
    let ignoreStyle = IgnoreStyleFeature in c.features
    if conceptSym != SymId(0):
      let modSuffix = extractModule(pool.syms[conceptSym])
      if modSuffix != "":
        for cand in loadSyms(modSuffix, basename):
          if not seen.containsOrIncl(cand):
            yield cand
      if typeRoot != SymId(0):
        let typeModule = extractModule(pool.syms[typeRoot])
        if typeModule != "" and typeModule != c.thisModuleSuffix:
          for cand in loadSyms(typeModule, basename):
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

proc collectConceptRoutineCandidates*(c: ptr SemContext; conceptSym: SymId; basename: StrId;
                                      a: Cursor): seq[SymId] =
  let typeRoot = nominalRoot(a)
  let (hit, cached) = tryCandidatesFromCache(c, conceptSym, basename, typeRoot)
  if hit:
    return cached
  result = default(seq[SymId])
  for cand in conceptRoutineCandidates(c, conceptSym, basename, typeRoot):
    result.add cand
  storeCandidates(c, conceptSym, basename, typeRoot, result)

proc routineHasNoSideEffect*(routine: Cursor): bool {.inline.} =
  let r = asRoutine(routine)
  whichEffect(routine.stmtKind, r.pragmas) == HasNoSideEffect

proc conceptRoutineKindsCompatible*(requirement, implementation: SymKind;
                                   implementationDecl: Cursor = default(Cursor)): bool {.inline.} =
  ## A `func` or `template` implementation may satisfy a `proc` requirement.
  ## A `proc` with the `noSideEffect` pragma may satisfy a `func` requirement,
  ## and so may a `template`: its expansion is effect-checked where it lands,
  ## which for a `func` body is exactly the check the requirement asks for.
  if requirement == implementation:
    return true
  if requirement == ProcY and implementation in {FuncY, TemplateY}:
    return true
  if requirement == FuncY and implementation == TemplateY:
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
