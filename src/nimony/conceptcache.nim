#       Nimony
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Self-contained concept-match cache. The rest of the compiler only needs
## the lifecycle hooks (`initConceptCache`, `onConceptDeclSem`,
## `onConceptImportsChanged`) and the lookup/store procs used from
## `sigmatch.nim`.
##
## Besides memoizing `(concept, type)` verdicts the cache tracks which body
## checks are currently *on the stack*. Requirement checking runs real
## overload resolution on candidate routines, and a candidate such as
## `min[T: Orderable]` asks `X is Orderable` while that very question is
## being answered. A re-entrant query is answered "not satisfied": a type
## satisfies a concept only through a derivation that does not assume the
## conclusion (the inductive reading, as in classic Nim).

import std / [tables, sets, hashes]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / symparser
import nimony_model, decls, programs, semdata, typeprops, symtabs

const DefaultConceptCacheCapacity* = 1024

type
  ConceptTypeKey* = object
    root*: SymId
    aux*: Hash

  BodyCacheKey* = object
    conceptSym*: SymId
    typeKey*: ConceptTypeKey

  CandidatesCacheKey* = object
    conceptSym*: SymId
    basename*: StrId
    typeRoot*: SymId ## nominal root of the type being checked: its module's
                     ## type-bound operations are candidates too

  ConceptBodyResult* = object
    satisfied*: bool
    missing*: seq[SymId] ## the requirement syms that were not met; replayed
                         ## into the diagnostics when a negative verdict hits
    generation: int ## `declGeneration` at store time; a negative verdict is
                    ## only valid while no routine has been declared since

  CandidatesEntry = object
    generation: int
    syms: seq[SymId]

  ConceptMetadata* = object
    parents*: seq[SymId]

proc `==`*(a, b: ConceptTypeKey): bool {.inline, noSideEffect.} =
  a.root == b.root and a.aux == b.aux

proc hash*(k: ConceptTypeKey): Hash {.noSideEffect.} =
  result = Hash(k.root.int) xor k.aux

proc `==`*(a, b: BodyCacheKey): bool {.inline, noSideEffect.} =
  a.conceptSym == b.conceptSym and a.typeKey == b.typeKey

proc hash*(k: BodyCacheKey): Hash {.noSideEffect.} =
  result = Hash(k.conceptSym.int) !& hash(k.typeKey)

proc `==`*(a, b: CandidatesCacheKey): bool {.inline, noSideEffect.} =
  a.conceptSym == b.conceptSym and a.basename == b.basename and a.typeRoot == b.typeRoot

proc hash*(k: CandidatesCacheKey): Hash {.noSideEffect.} =
  result = Hash(k.conceptSym.int) !& Hash(k.basename.int) !& Hash(k.typeRoot.int)

type
  ConceptCacheImpl* = ref object of RootObj
    capacity*: int
    bodyCache*: Table[BodyCacheKey, ConceptBodyResult]
    candidatesCache*: Table[CandidatesCacheKey, CandidatesEntry]
    metadata*: Table[SymId, ConceptMetadata]
    declGeneration*: int ## bumped by every routine declaration: a new
                         ## overload can turn "not satisfied" into "satisfied"
                         ## and extend a candidate list, so those entries
                         ## expire; positive verdicts only ever stay true
    inProgress*: HashSet[BodyCacheKey] ## body checks currently on the stack
    assumptionHits*: int ## how many re-entrant queries were answered so far;
                         ## a verdict computed while this moved must not be
                         ## memoized (see `conceptVerdictIsFinal`)

proc initConceptCache*(c: var SemContext) =
  if c.conceptCache == nil:
    c.conceptCache = ConceptCacheImpl(capacity: DefaultConceptCacheCapacity)

proc initConceptCache*(c: ptr SemContext) =
  if c != nil and c.conceptCache == nil:
    c.conceptCache = ConceptCacheImpl(capacity: DefaultConceptCacheCapacity)

proc asConceptCacheImpl(cache: RootRef): ConceptCacheImpl {.inline.} =
  cast[ConceptCacheImpl](cache)

proc ensureConceptCache(c: ptr SemContext): ConceptCacheImpl =
  if c != nil and c.conceptCache == nil:
    initConceptCache(c)
  asConceptCacheImpl(c.conceptCache)

proc onConceptImportsChanged*(c: var SemContext) =
  if c.conceptCache == nil:
    return
  let cache = asConceptCacheImpl(c.conceptCache)
  cache.bodyCache.clear()
  cache.candidatesCache.clear()

proc onRoutineDeclSem*(c: var SemContext) =
  if c.conceptCache != nil:
    inc asConceptCacheImpl(c.conceptCache).declGeneration

proc invalidateConceptSymCache(cache: ConceptCacheImpl; conceptSym: SymId) =
  block body:
    var remove: seq[BodyCacheKey] = @[]
    for k in cache.bodyCache.keys:
      if k.conceptSym == conceptSym:
        remove.add k
    for k in remove:
      cache.bodyCache.del k
  block candidates:
    var remove: seq[CandidatesCacheKey] = @[]
    for k in cache.candidatesCache.keys:
      if k.conceptSym == conceptSym:
        remove.add k
    for k in remove:
      cache.candidatesCache.del k

proc onConceptDeclSem*(c: var SemContext; ownerSym: SymId; dest: var TokenBuf; conceptStart: int) =
  if ownerSym == SymId(0) or c.conceptCache == nil:
    return
  let cache = asConceptCacheImpl(c.conceptCache)
  invalidateConceptSymCache(cache, ownerSym)
  let body = cursorAt(dest, conceptStart)
  let parents = conceptParentsSlot(body)
  if conceptParentsWellFormed(parents):
    var meta = ConceptMetadata()
    for p in conceptParentSyms(parents):
      meta.parents.add p
    cache.metadata[ownerSym] = meta

proc hashTypeCursor(n: Cursor): Hash =
  var h: Hash = 0
  case n.kind
  of Symbol:
    h = h !& Hash(n.symId.int)
  of TagLit:
    h = h !& Hash(n.cursorTagId.int)
    var child = sub(n)
    while hasMore(child):
      h = h !& hashTypeCursor(child)
      skip child
  of Ident, StrLit:
    h = h !& Hash(n.strId.int)
  of IntLit, InlineInt:
    h = h !& Hash(n.intVal.int)
  of FloatLit:
    h = h !& Hash(cast[int64](n.floatVal))
  else:
    h = h !& Hash(ord(n.kind))
  result = h

proc conceptTypeKey*(a: Cursor): ConceptTypeKey =
  ## `root` is the nominal head symbol, which for a generic *instance* is the
  ## generic's own symbol — `Box[int]` and `Box[Foo]` share it. The structural
  ## hash of the whole type tree is what tells the instances apart.
  ConceptTypeKey(root: nominalRoot(a, allowTypevar = true), aux: hashTypeCursor(a))

proc bodyCacheKey(conceptSym: SymId; a: Cursor): BodyCacheKey =
  BodyCacheKey(conceptSym: conceptSym, typeKey: conceptTypeKey(a))

proc isOpenTypevar*(a: Cursor): bool =
  if a.isSymbol:
    let res = tryLoadSym(a.symId)
    if res.status == LacksNothing and res.decl.symKind == TypevarY:
      return true
  false

proc hasOpenTypevarDeep(a: Cursor): bool =
  case a.kind
  of Symbol:
    isOpenTypevar(a)
  of TagLit:
    var child = sub(a)
    while hasMore(child):
      if hasOpenTypevarDeep(child):
        return true
      skip child
    false
  else:
    false

proc isCacheableConcreteType*(a: Cursor): bool =
  not hasOpenTypevarDeep(a)

proc conceptRequirementSym*(routine: Cursor): SymId =
  var prc = routine
  if prc.symKind in RoutineKinds:
    inc prc
    if prc.isSymbolDef:
      return prc.symId
  SymId(0)

proc collectConceptMetadata(body: Cursor): ConceptMetadata =
  result = ConceptMetadata()
  let parents = conceptParentsSlot(body)
  if conceptParentsWellFormed(parents):
    for p in conceptParentSyms(parents):
      result.parents.add p

proc getConceptMetadata*(c: ptr SemContext; conceptSym: SymId; body: Cursor): ConceptMetadata =
  if c != nil and conceptSym != SymId(0) and c.conceptCache != nil:
    let cache = asConceptCacheImpl(c.conceptCache)
    if cache.metadata.hasKey(conceptSym):
      return cache.metadata.getOrDefault(conceptSym)
  collectConceptMetadata(body)

proc isConceptTypeArg(a: Cursor): bool {.inline.} =
  a.isSymbol and isConceptSym(a.symId)

proc cacheCapacity(cache: ConceptCacheImpl): int =
  if cache.capacity > 0: cache.capacity else: DefaultConceptCacheCapacity

proc bodyCheckCacheable(c: ptr SemContext; conceptSym: SymId; a: Cursor): bool {.inline.} =
  c != nil and conceptSym != SymId(0) and isCacheableConcreteType(a) and not isConceptTypeArg(a)

proc tryBodyCheckFromCache*(c: ptr SemContext; conceptSym: SymId; a: Cursor): (bool, ConceptBodyResult) =
  if not bodyCheckCacheable(c, conceptSym, a):
    return (false, default(ConceptBodyResult))
  let cache = ensureConceptCache(c)
  let key = bodyCacheKey(conceptSym, a)
  if not cache.bodyCache.hasKey(key):
    return (false, default(ConceptBodyResult))
  let res = cache.bodyCache.getOrDefault(key)
  if not res.satisfied and res.generation != cache.declGeneration:
    return (false, default(ConceptBodyResult))
  (true, res)

proc storeBodyCheck*(c: ptr SemContext; conceptSym: SymId; a: Cursor; res: sink ConceptBodyResult) =
  if not bodyCheckCacheable(c, conceptSym, a):
    return
  let cache = ensureConceptCache(c)
  # Hits are the hot path: overload resolution asks the same `(concept, type)`
  # question once per candidate per call. A plain clear-at-capacity keeps the
  # hit free instead of paying for LRU bookkeeping.
  if cache.bodyCache.len >= cacheCapacity(cache):
    cache.bodyCache.clear()
  var entry = res
  entry.generation = cache.declGeneration
  cache.bodyCache[bodyCacheKey(conceptSym, a)] = entry

proc enterBodyCheck*(c: ptr SemContext; conceptSym: SymId; a: Cursor): bool =
  ## False when the same check is already on the stack: the caller answers
  ## "not satisfied" without recursing. The guard applies to every type,
  ## cacheable or not, since a generic candidate can re-ask for `seq[T]` as
  ## easily as for `int`.
  if c == nil or conceptSym == SymId(0):
    return true
  let cache = ensureConceptCache(c)
  let key = bodyCacheKey(conceptSym, a)
  if key in cache.inProgress:
    inc cache.assumptionHits
    return false
  cache.inProgress.incl key
  true

proc leaveBodyCheck*(c: ptr SemContext; conceptSym: SymId; a: Cursor) =
  if c == nil or conceptSym == SymId(0):
    return
  let cache = ensureConceptCache(c)
  cache.inProgress.excl bodyCacheKey(conceptSym, a)

proc conceptAssumptionHits*(c: ptr SemContext): int =
  if c == nil or c.conceptCache == nil: 0
  else: asConceptCacheImpl(c.conceptCache).assumptionHits

proc conceptVerdictIsFinal*(c: ptr SemContext; hitsBefore: int): bool =
  ## Whether a verdict computed since `hitsBefore` may be memoized. A verdict
  ## that consulted an in-progress assumption is only trusted for the
  ## outermost check, whose own assumption is the one that was consulted; an
  ## inner verdict may have been shaped by an assumption about a sibling and
  ## is recomputed next time.
  if c == nil or c.conceptCache == nil:
    return true
  let cache = asConceptCacheImpl(c.conceptCache)
  cache.assumptionHits == hitsBefore or cache.inProgress.len == 0

proc storeCandidates*(c: ptr SemContext; conceptSym: SymId; basename: StrId;
                      typeRoot: SymId; res: sink seq[SymId]) =
  if c == nil:
    return
  let cache = ensureConceptCache(c)
  if cache.candidatesCache.len >= cacheCapacity(cache):
    cache.candidatesCache.clear()
  cache.candidatesCache[CandidatesCacheKey(conceptSym: conceptSym, basename: basename, typeRoot: typeRoot)] =
    CandidatesEntry(generation: cache.declGeneration, syms: res)

proc tryCandidatesFromCache*(c: ptr SemContext; conceptSym: SymId; basename: StrId;
                             typeRoot: SymId): (bool, seq[SymId]) =
  if c == nil:
    return (false, default(seq[SymId]))
  let cache = ensureConceptCache(c)
  let key = CandidatesCacheKey(conceptSym: conceptSym, basename: basename, typeRoot: typeRoot)
  if not cache.candidatesCache.hasKey(key):
    return (false, default(seq[SymId]))
  let entry = cache.candidatesCache.getOrDefault(key)
  if entry.generation != cache.declGeneration:
    return (false, default(seq[SymId]))
  (true, entry.syms)
