#       Nimony
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Self-contained concept-match cache. The rest of the compiler only needs
## the lifecycle hooks (`initConceptCache`, `onConceptDeclSem`,
## `onConceptImportsChanged`) and the `remember*` templates used from
## `sigmatch.nim`.

import std / [tables, hashes]
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

  RoutineImplCacheKey* = object
    conceptSym*: SymId
    reqSym*: SymId
    typeKey*: ConceptTypeKey

  CandidatesCacheKey* = object
    conceptSym*: SymId
    basename*: StrId

  ConceptBodyResult* = object
    satisfied*: bool
    missing*: seq[SymId]

  ConceptRoutineImplResult* = object
    found*: bool
    impl*: SymId

  ConceptMetadata* = object
    parents*: seq[SymId]

  ConceptCacheImpl* = ref object of RootObj
    capacity*: int
    bodyCache*: Table[BodyCacheKey, ConceptBodyResult]
    bodyCacheOrder*: seq[BodyCacheKey]
    routineImplCache*: Table[RoutineImplCacheKey, ConceptRoutineImplResult]
    routineImplCacheOrder*: seq[RoutineImplCacheKey]
    candidatesCache*: Table[CandidatesCacheKey, seq[SymId]]
    candidatesCacheOrder*: seq[CandidatesCacheKey]
    metadata*: Table[SymId, ConceptMetadata]

when defined(nimonyProfileConcepts):
  var
    conceptBodyChecks* = 0
    conceptBodyCacheHits* = 0
    conceptRoutineAvailableCalls* = 0
    conceptRoutineImplCacheHits* = 0
    conceptCandidateScans* = 0
    conceptCandidateCacheHits* = 0
    matchConceptRoutineSigCalls* = 0

  proc printConceptProfile*() =
    echo "concept profile:"
    echo "  body_checks=", conceptBodyChecks, " body_cache_hits=", conceptBodyCacheHits
    echo "  routine_available=", conceptRoutineAvailableCalls,
         " routine_impl_hits=", conceptRoutineImplCacheHits
    echo "  candidate_scans=", conceptCandidateScans,
         " candidate_cache_hits=", conceptCandidateCacheHits
    echo "  sig_match_calls=", matchConceptRoutineSigCalls
else:
  template conceptBodyChecks*() = discard
  template conceptBodyCacheHits*() = discard
  template conceptRoutineAvailableCalls*() = discard
  template conceptRoutineImplCacheHits*() = discard
  template conceptCandidateScans*() = discard
  template conceptCandidateCacheHits*() = discard
  template matchConceptRoutineSigCalls*() = discard
  template printConceptProfile*() = discard

proc `==`*(a, b: ConceptTypeKey): bool {.inline, noSideEffect.} =
  a.root == b.root and a.aux == b.aux

proc hash*(k: ConceptTypeKey): Hash {.noSideEffect.} =
  result = Hash(k.root.int) xor k.aux

proc `==`*(a, b: BodyCacheKey): bool {.inline, noSideEffect.} =
  a.conceptSym == b.conceptSym and a.typeKey == b.typeKey

proc hash*(k: BodyCacheKey): Hash {.noSideEffect.} =
  result = Hash(k.conceptSym.int) !& hash(k.typeKey)

proc `==`*(a, b: RoutineImplCacheKey): bool {.inline, noSideEffect.} =
  a.conceptSym == b.conceptSym and a.reqSym == b.reqSym and a.typeKey == b.typeKey

proc hash*(k: RoutineImplCacheKey): Hash {.noSideEffect.} =
  result = Hash(k.conceptSym.int) !& Hash(k.reqSym.int) !& hash(k.typeKey)

proc `==`*(a, b: CandidatesCacheKey): bool {.inline, noSideEffect.} =
  a.conceptSym == b.conceptSym and a.basename == b.basename

proc hash*(k: CandidatesCacheKey): Hash {.noSideEffect.} =
  result = Hash(k.conceptSym.int) !& Hash(k.basename.int)

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
  cache.bodyCacheOrder.setLen(0)
  cache.routineImplCache.clear()
  cache.routineImplCacheOrder.setLen(0)
  cache.candidatesCache.clear()
  cache.candidatesCacheOrder.setLen(0)

proc invalidateConceptSymCache(cache: ConceptCacheImpl; conceptSym: SymId) =
  block body:
    var remove: seq[BodyCacheKey] = @[]
    for k in cache.bodyCache.keys:
      if k.conceptSym == conceptSym:
        remove.add k
    for k in remove:
      cache.bodyCache.del k
    var i = 0
    while i < cache.bodyCacheOrder.len:
      if cache.bodyCacheOrder[i].conceptSym == conceptSym:
        cache.bodyCacheOrder.delete(i)
      else:
        inc i
  block routine:
    var remove: seq[RoutineImplCacheKey] = @[]
    for k in cache.routineImplCache.keys:
      if k.conceptSym == conceptSym:
        remove.add k
    for k in remove:
      cache.routineImplCache.del k
    var i = 0
    while i < cache.routineImplCacheOrder.len:
      if cache.routineImplCacheOrder[i].conceptSym == conceptSym:
        cache.routineImplCacheOrder.delete(i)
      else:
        inc i
  block candidates:
    var remove: seq[CandidatesCacheKey] = @[]
    for k in cache.candidatesCache.keys:
      if k.conceptSym == conceptSym:
        remove.add k
    for k in remove:
      cache.candidatesCache.del k
    var i = 0
    while i < cache.candidatesCacheOrder.len:
      if cache.candidatesCacheOrder[i].conceptSym == conceptSym:
        cache.candidatesCacheOrder.delete(i)
      else:
        inc i

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
  var nested = 0
  var c = n
  while nested >= 0:
    case c.kind
    of Symbol:
      h = h !& Hash(c.symId.int)
      inc c
    of ParLe:
      h = h !& Hash(c.tagId.int)
      inc nested
      inc c
    of ParRi:
      inc c
      dec nested
    of Ident, StringLit:
      h = h !& Hash(c.litId.int)
      inc c
    of IntLit, InlineInt:
      h = h !& Hash(c.intId.int)
      inc c
    of FloatLit:
      h = h !& Hash(c.floatId.int)
      inc c
    else:
      h = h !& Hash(ord(c.kind))
      inc c
    if nested == 0:
      break
  result = h

proc conceptTypeKey*(a: Cursor): ConceptTypeKey =
  let root = nominalRoot(a, allowTypevar = true)
  if root != SymId(0):
    result = ConceptTypeKey(root: root)
  else:
    result = ConceptTypeKey(root: SymId(0), aux: hashTypeCursor(a))

proc bodyCacheKey(conceptSym: SymId; a: Cursor): BodyCacheKey =
  BodyCacheKey(conceptSym: conceptSym, typeKey: conceptTypeKey(a))

proc routineImplCacheKey(conceptSym, reqSym: SymId; a: Cursor): RoutineImplCacheKey =
  RoutineImplCacheKey(conceptSym: conceptSym, reqSym: reqSym, typeKey: conceptTypeKey(a))

proc isOpenTypevar*(a: Cursor): bool =
  if a.kind == Symbol:
    let res = tryLoadSym(a.symId)
    if res.status == LacksNothing and res.decl.symKind == TypevarY:
      return true
  false

proc hasOpenTypevarDeep(a: Cursor): bool =
  var nested = 0
  var c = a
  while nested >= 0:
    if c.kind == Symbol:
      let res = tryLoadSym(c.symId)
      if res.status == LacksNothing and res.decl.symKind == TypevarY:
        return true
      inc c
    elif c.kind == ParLe:
      inc nested
      inc c
    elif c.kind == ParRi:
      dec nested
      inc c
    else:
      inc c
    if nested == 0:
      break
  false

proc isCacheableConcreteType*(a: Cursor): bool =
  not hasOpenTypevarDeep(a)

proc conceptRequirementSym*(routine: Cursor): SymId =
  var prc = routine
  if prc.symKind in RoutineKinds:
    inc prc
    if prc.kind == SymbolDef:
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

proc loadConceptRequirement*(reqSym: SymId): Cursor =
  let res = tryLoadSym(reqSym)
  if res.status == LacksNothing:
    res.decl
  else:
    default(Cursor)

proc lruTouchBody(order: var seq[BodyCacheKey]; key: BodyCacheKey) =
  for i, k in order:
    if k == key:
      if i < order.high:
        order.delete(i)
        order.add key
      return
  order.add key

proc lruPutBody(table: var Table[BodyCacheKey, ConceptBodyResult];
                order: var seq[BodyCacheKey]; capacity: int;
                key: BodyCacheKey; val: sink ConceptBodyResult) =
  let isNew = not table.hasKey(key)
  table[key] = val
  if isNew:
    order.add key
  else:
    lruTouchBody(order, key)
  while order.len > capacity:
    let oldKey = order[0]
    order.delete(0)
    table.del oldKey

proc lruTouchRoutine(order: var seq[RoutineImplCacheKey]; key: RoutineImplCacheKey) =
  for i, k in order:
    if k == key:
      if i < order.high:
        order.delete(i)
        order.add key
      return
  order.add key

proc lruPutRoutine(table: var Table[RoutineImplCacheKey, ConceptRoutineImplResult];
                   order: var seq[RoutineImplCacheKey]; capacity: int;
                   key: RoutineImplCacheKey; val: sink ConceptRoutineImplResult) =
  let isNew = not table.hasKey(key)
  table[key] = val
  if isNew:
    order.add key
  else:
    lruTouchRoutine(order, key)
  while order.len > capacity:
    let oldKey = order[0]
    order.delete(0)
    table.del oldKey

proc lruTouchCandidates(order: var seq[CandidatesCacheKey]; key: CandidatesCacheKey) =
  for i, k in order:
    if k == key:
      if i < order.high:
        order.delete(i)
        order.add key
      return
  order.add key

proc lruPutCandidates(table: var Table[CandidatesCacheKey, seq[SymId]];
                      order: var seq[CandidatesCacheKey]; capacity: int;
                      key: CandidatesCacheKey; val: sink seq[SymId]) =
  let isNew = not table.hasKey(key)
  table[key] = val
  if isNew:
    order.add key
  else:
    lruTouchCandidates(order, key)
  while order.len > capacity:
    let oldKey = order[0]
    order.delete(0)
    table.del oldKey

proc isConceptTypeArg(a: Cursor): bool {.inline.} =
  a.kind == Symbol and isConceptSym(a.symId)

proc cacheCapacity(cache: ConceptCacheImpl): int =
  if cache.capacity > 0: cache.capacity else: DefaultConceptCacheCapacity

proc tryBodyCheckFromCache*(c: ptr SemContext; conceptSym: SymId; a: Cursor): (bool, ConceptBodyResult) =
  if c == nil or conceptSym == SymId(0) or not isCacheableConcreteType(a) or isConceptTypeArg(a):
    return (false, default(ConceptBodyResult))
  let cache = ensureConceptCache(c)
  let key = bodyCacheKey(conceptSym, a)
  if not cache.bodyCache.hasKey(key):
    return (false, default(ConceptBodyResult))
  conceptBodyCacheHits()
  lruTouchBody(cache.bodyCacheOrder, key)
  (true, cache.bodyCache.getOrDefault(key))

proc storeRoutineImpl*(c: ptr SemContext; conceptSym, reqSym: SymId; a: Cursor;
                        res: sink ConceptRoutineImplResult) =
  if c == nil or conceptSym == SymId(0) or reqSym == SymId(0) or not isCacheableConcreteType(a):
    return
  let cache = ensureConceptCache(c)
  let key = routineImplCacheKey(conceptSym, reqSym, a)
  lruPutRoutine(cache.routineImplCache, cache.routineImplCacheOrder, cacheCapacity(cache), key, res)

proc tryRoutineImplFromCache*(c: ptr SemContext; conceptSym, reqSym: SymId; a: Cursor): (bool, ConceptRoutineImplResult) =
  if c == nil or conceptSym == SymId(0) or reqSym == SymId(0) or not isCacheableConcreteType(a):
    return (false, default(ConceptRoutineImplResult))
  let cache = ensureConceptCache(c)
  let key = routineImplCacheKey(conceptSym, reqSym, a)
  if not cache.routineImplCache.hasKey(key):
    return (false, default(ConceptRoutineImplResult))
  conceptRoutineImplCacheHits()
  lruTouchRoutine(cache.routineImplCacheOrder, key)
  (true, cache.routineImplCache.getOrDefault(key))

proc bodyResultFromMissing*(missing: openArray[Cursor]): ConceptBodyResult =
  result = ConceptBodyResult(satisfied: missing.len == 0)
  for routine in missing:
    let rs = conceptRequirementSym(routine)
    if rs != SymId(0):
      result.missing.add rs

proc storeBodyCheck*(c: ptr SemContext; conceptSym: SymId; a: Cursor; res: sink ConceptBodyResult) =
  if c == nil or conceptSym == SymId(0) or not isCacheableConcreteType(a) or isConceptTypeArg(a):
    return
  let cache = ensureConceptCache(c)
  let key = bodyCacheKey(conceptSym, a)
  lruPutBody(cache.bodyCache, cache.bodyCacheOrder, cacheCapacity(cache), key, res)

proc tryMissingFromBodyCache*(c: ptr SemContext; conceptSym: SymId; a: Cursor;
                              missing: var seq[Cursor]): bool =
  if c == nil or conceptSym == SymId(0) or not isCacheableConcreteType(a) or isConceptTypeArg(a):
    return false
  let cache = ensureConceptCache(c)
  let key = bodyCacheKey(conceptSym, a)
  if not cache.bodyCache.hasKey(key):
    return false
  conceptBodyCacheHits()
  lruTouchBody(cache.bodyCacheOrder, key)
  let cached = cache.bodyCache.getOrDefault(key)
  if not cached.satisfied and cached.missing.len == 0:
    return false
  if cached.satisfied:
    missing = @[]
  else:
    missing = @[]
    for reqSym in cached.missing:
      missing.add loadConceptRequirement(reqSym)
  true

proc storeCandidates*(c: ptr SemContext; conceptSym: SymId; basename: StrId;
                      res: sink seq[SymId]) =
  if c == nil:
    return
  let cache = ensureConceptCache(c)
  let key = CandidatesCacheKey(conceptSym: conceptSym, basename: basename)
  lruPutCandidates(cache.candidatesCache, cache.candidatesCacheOrder, cacheCapacity(cache), key, res)

proc tryCandidatesFromCache*(c: ptr SemContext; conceptSym: SymId; basename: StrId): (bool, seq[SymId]) =
  if c == nil:
    return (false, default(seq[SymId]))
  let cache = ensureConceptCache(c)
  let key = CandidatesCacheKey(conceptSym: conceptSym, basename: basename)
  if not cache.candidatesCache.hasKey(key):
    return (false, default(seq[SymId]))
  conceptCandidateCacheHits()
  lruTouchCandidates(cache.candidatesCacheOrder, key)
  (true, cache.candidatesCache.getOrDefault(key))
