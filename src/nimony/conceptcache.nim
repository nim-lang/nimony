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

  ConceptBodyResult* = object
    satisfied*: bool
    missing*: seq[SymId]

  ConceptRoutineImplResult* = object
    found*: bool
    impl*: SymId

  ConceptMetadata* = object
    parents*: seq[SymId]

  ConceptCacheImpl* = ref object of ConceptCache
    capacity*: int
    bodyCache*: Table[(SymId, ConceptTypeKey), ConceptBodyResult]
    bodyCacheOrder*: seq[(SymId, ConceptTypeKey)]
    routineImplCache*: Table[(SymId, SymId, ConceptTypeKey), ConceptRoutineImplResult]
    routineImplCacheOrder*: seq[(SymId, SymId, ConceptTypeKey)]
    candidatesCache*: Table[(SymId, StrId), seq[SymId]]
    candidatesCacheOrder*: seq[(SymId, StrId)]
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

proc `==`*(a, b: ConceptTypeKey): bool {.inline.} =
  a.root == b.root and a.aux == b.aux

proc hash*(k: ConceptTypeKey): Hash =
  result = Hash(k.root.int) xor k.aux

proc initConceptCache*(c: var SemContext) =
  if c.conceptCache.isNil:
    c.conceptCache = ConceptCacheImpl(capacity: DefaultConceptCacheCapacity)

proc initConceptCache*(c: ptr SemContext) =
  if c != nil and c.conceptCache.isNil:
    c.conceptCache = ConceptCacheImpl(capacity: DefaultConceptCacheCapacity)

proc ensureConceptCache(c: ptr SemContext): ConceptCacheImpl =
  if c != nil and c.conceptCache.isNil:
    initConceptCache(c)
  ConceptCacheImpl(c.conceptCache)

proc onConceptImportsChanged*(c: var SemContext) =
  if c.conceptCache.isNil:
    return
  let cache = ConceptCacheImpl(c.conceptCache)
  cache.bodyCache.clear()
  cache.bodyCacheOrder.setLen(0)
  cache.routineImplCache.clear()
  cache.routineImplCacheOrder.setLen(0)
  cache.candidatesCache.clear()
  cache.candidatesCacheOrder.setLen(0)

proc invalidateConceptSymCache(cache: ConceptCacheImpl; conceptSym: SymId) =
  block body:
    var remove: seq[(SymId, ConceptTypeKey)] = @[]
    for k in cache.bodyCache.keys:
      if k[0] == conceptSym:
        remove.add k
    for k in remove:
      cache.bodyCache.del k
    var i = 0
    while i < cache.bodyCacheOrder.len:
      if cache.bodyCacheOrder[i][0] == conceptSym:
        cache.bodyCacheOrder.delete(i)
      else:
        inc i
  block routine:
    var remove: seq[(SymId, SymId, ConceptTypeKey)] = @[]
    for k in cache.routineImplCache.keys:
      if k[0] == conceptSym:
        remove.add k
    for k in remove:
      cache.routineImplCache.del k
    var i = 0
    while i < cache.routineImplCacheOrder.len:
      if cache.routineImplCacheOrder[i][0] == conceptSym:
        cache.routineImplCacheOrder.delete(i)
      else:
        inc i
  block candidates:
    var remove: seq[(SymId, StrId)] = @[]
    for k in cache.candidatesCache.keys:
      if k[0] == conceptSym:
        remove.add k
    for k in remove:
      cache.candidatesCache.del k
    var i = 0
    while i < cache.candidatesCacheOrder.len:
      if cache.candidatesCacheOrder[i][0] == conceptSym:
        cache.candidatesCacheOrder.delete(i)
      else:
        inc i

proc onConceptDeclSem*(c: var SemContext; ownerSym: SymId; dest: var TokenBuf; conceptStart: int) =
  if ownerSym == SymId(0):
    return
  initConceptCache(c)
  let cache = ConceptCacheImpl(c.conceptCache)
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
  if c != nil and conceptSym != SymId(0):
    initConceptCache(c)
    let cache = ConceptCacheImpl(c.conceptCache)
    if cache.metadata.hasKey(conceptSym):
      return cache.metadata[conceptSym]
  collectConceptMetadata(body)

proc loadConceptRequirement*(reqSym: SymId): Cursor =
  let res = tryLoadSym(reqSym)
  if res.status == LacksNothing:
    res.decl
  else:
    default(Cursor)

proc lruTouch[K](order: var seq[K]; key: K) =
  for i, k in order:
    if k == key:
      if i < order.high:
        order.delete(i)
        order.add key
      return
  order.add key

proc lruPut[K, V](table: var Table[K, V]; order: var seq[K];
                  capacity: int; key: K; val: sink V) =
  let isNew = key notin table
  table[key] = val
  if isNew:
    order.add key
  else:
    lruTouch(order, key)
  while order.len > capacity:
    let oldKey = order[0]
    order.delete(0)
    table.del oldKey

proc isConceptTypeArg(a: Cursor): bool {.inline.} =
  a.kind == Symbol and isConceptSym(a.symId)

proc cacheCapacity(cache: ConceptCacheImpl): int =
  if cache.capacity > 0: cache.capacity else: DefaultConceptCacheCapacity

template rememberBodyCheck*(c: ptr SemContext; conceptSym: SymId; a: Cursor;
                             compute: untyped): untyped =
  if c != nil and conceptSym != SymId(0) and isCacheableConcreteType(a) and not isConceptTypeArg(a):
    let cache = ensureConceptCache(c)
    let key = (conceptSym, conceptTypeKey(a))
    if key in cache.bodyCache:
      conceptBodyCacheHits()
      lruTouch(cache.bodyCacheOrder, key)
      cache.bodyCache[key]
    else:
      conceptBodyChecks()
      let res = compute
      lruPut(cache.bodyCache, cache.bodyCacheOrder, cacheCapacity(cache), key, res)
      res
  else:
    compute

template rememberRoutineImpl*(c: ptr SemContext; conceptSym, reqSym: SymId; a: Cursor;
                              compute: untyped): untyped =
  if c != nil and conceptSym != SymId(0) and reqSym != SymId(0) and isCacheableConcreteType(a):
    let cache = ensureConceptCache(c)
    let key = (conceptSym, reqSym, conceptTypeKey(a))
    if key in cache.routineImplCache:
      conceptRoutineImplCacheHits()
      lruTouch(cache.routineImplCacheOrder, key)
      cache.routineImplCache[key].found
    else:
      let res = compute
      lruPut(cache.routineImplCache, cache.routineImplCacheOrder, cacheCapacity(cache), key, res)
      res.found
  else:
    (compute).found

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
  let key = (conceptSym, conceptTypeKey(a))
  lruPut(cache.bodyCache, cache.bodyCacheOrder, cacheCapacity(cache), key, res)

proc tryMissingFromBodyCache*(c: ptr SemContext; conceptSym: SymId; a: Cursor;
                              missing: var seq[Cursor]): bool =
  if c == nil or conceptSym == SymId(0) or not isCacheableConcreteType(a) or isConceptTypeArg(a):
    return false
  let cache = ensureConceptCache(c)
  let key = (conceptSym, conceptTypeKey(a))
  if key notin cache.bodyCache:
    return false
  conceptBodyCacheHits()
  lruTouch(cache.bodyCacheOrder, key)
  let cached = cache.bodyCache[key]
  if not cached.satisfied and cached.missing.len == 0:
    return false
  if cached.satisfied:
    missing = @[]
  else:
    missing = @[]
    for reqSym in cached.missing:
      missing.add loadConceptRequirement(reqSym)
  true

template rememberCandidates*(c: ptr SemContext; conceptSym: SymId; basename: StrId;
                             compute: untyped): untyped =
  if c != nil:
    let cache = ensureConceptCache(c)
    let key = (conceptSym, basename)
    if key in cache.candidatesCache:
      conceptCandidateCacheHits()
      lruTouch(cache.candidatesCacheOrder, key)
      cache.candidatesCache[key]
    else:
      conceptCandidateScans()
      let res = compute
      lruPut(cache.candidatesCache, cache.candidatesCacheOrder, cacheCapacity(cache), key, res)
      res
  else:
    compute
