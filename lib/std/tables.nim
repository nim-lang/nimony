{.feature: "staticContracts".}

import algorithm, hashes, assertions

type
  Keyable* = concept of Hashable ## Types usable as table keys: `Hashable` plus `==`.
    func `==`(a, b: Self): bool

const
  defaultInitialSize* = 32
  growthFactor = 2
  linearScanLimit = 4

type
  HashEntry = object
    fullhash: Hash
    position: int # 1-based index into `data`; 0 marks an empty slot
  TableData[T] = object
    data: seq[T]
    hashes: seq[HashEntry]
  TableCore[T] = object
    entries: TableData[T]
  Table*[A, B] = TableCore[(A, B)] ## Generic hash table, consisting of
    ## key-value pairs. Tiny tables are scanned linearly; larger ones keep an
    ## open-addressed index beside the dense pair sequence.

  TableRefImpl[T] = object
    r: ref TableCore[T]
  TableRef*[A, B] = object ## Ref version of `Table`.
    impl: TableRefImpl[(A, B)]

# ------------------------------ helpers ---------------------------------

func isEmpty(hcode: Hash): bool {.inline.} =
  hcode == 0

func isFilled(hcode: Hash): bool {.inline.} =
  hcode != 0

func isIndexed(a: HashEntry): bool {.inline.} =
  a.position > 0

func indexNeedsGrowth(length, counter: int): bool {.inline.} =
  ## Whether an index of `length` slots is too full for `counter` entries.
  ## Total, unlike `mustRehash`, so the dense table can also try to build an
  ## index after an earlier allocation failure left it without one.
  result = (length < counter div 2 + counter) or (length - counter < 4)

func mustRehash(length, counter: int): bool {.inline, requires: length > counter.} =
  result = (length < counter div 2 + counter) or (length - counter < 4)

func nextPowerOfTwo(x: int): int {.inline.} =
  result = 1
  while result < x:
    result = result shl 1

func slotsNeeded(count: int): int {.inline.} =
  ## Capacity for `count` entries, keeping at least four free slots. This is
  ## the same sizing rule as `mustRehash`.
  let wanted = if count < 0: 4 else: count div 2 + count + 4
  nextPowerOfTwo(wanted)

func initialProbe(hc: Hash; dataLen: int): int {.inline,
    requires: dataLen > 0, ensures: 0 <= result and result < dataLen.} =
  ## Home slot for `hc`; `dataLen` is always a power of two.
  result = int(hc and uint(dataLen - 1))

func nextProbe(h, mask: int): int {.inline, requires: mask >= 0,
    ensures: 0 <= result and result <= mask.} =
  result = (h + 1) and mask

func genHash[A: Hashable](key: A): Hash {.inline.} =
  result = hash(key)
  # A zero hash marks an empty slot, so real keys never carry one.
  if result == 0:
    result = 314159265'u

# ------------------------- dense table helpers ---------------------------

func emptySlot(s: seq[HashEntry]; h: Hash): int {.requires: s.len > 0,
    ensures: 0 <= result and result < s.len, inline.} =
  ## The first unfilled slot on `h`'s linear probe sequence.
  var i = h
  while isIndexed(s[i and high(s).uint]):
    inc i
  result = int(i and high(s).uint)

func resizeIndex(t: var seq[HashEntry]) =
  ## Grows an existing index. The old index stays in place when allocation
  ## fails; callers then refuse the insert rather than let `data` outgrow it.
  let newLen = if t.len == 0: 4 else: t.len * 2
  var s = newSeq[HashEntry](newLen)
  if s.len == 0:
    return
  for i in 0 ..< t.len:
    if isIndexed(t[i]):
      s[emptySlot(s, t[i].fullhash)] = t[i]
  t = ensureMove s

func fillHashPart[K: Keyable, V](t: var Table[K, V]) =
  ## Builds a complete index for `data`, or leaves `hashes` empty. The
  ## capacity is chosen for what `data` already holds, including the case
  ## where an earlier attempt failed and `data` kept growing without an index.
  var cap = linearScanLimit * 2
  while cap <= t.entries.data.len or
      indexNeedsGrowth(cap, t.entries.data.len):
    cap = cap * 2
  t.entries.hashes = newSeq[HashEntry](cap)
  if t.entries.hashes.len == 0:
    return
  for i in 0 ..< t.entries.data.len:
    let fullhash = hash(t.entries.data[i][0])
    t.entries.hashes[emptySlot(t.entries.hashes, fullhash)] =
      HashEntry(fullhash: fullhash, position: i + 1)

func rawGet[K: Keyable, V](t: Table[K, V]; k: K; kh: out Hash): int {.
    ensures: result < t.entries.data.len.} =
  ## Returns the dense index of `k`, or -1. `kh` receives the normalized hash
  ## only when the index is consulted; tiny tables only scan.
  if t.entries.data.len <= linearScanLimit or t.entries.hashes.len == 0:
    kh = 0
    for i in 0 ..< t.entries.data.len:
      if t.entries.data[i][0] == k:
        return i
  else:
    let h = hash(k)
    kh = h
    var probe = h
    while isIndexed(t.entries.hashes[probe and high(t.entries.hashes).uint]):
      let d = t.entries.hashes[probe and high(t.entries.hashes).uint]
      # `position` names a live pair; a stale entry is never a match.
      if d.fullhash == h and d.position >= 1 and
          d.position <= t.entries.data.len and
          t.entries.data[d.position - 1][0] == k:
        return d.position - 1
      inc probe
  result = -1

func delAt[K: Keyable, V](t: var Table[K, V]; idx: int) =
  ## Removes the dense entry at `idx`, preserving insertion order.
  if idx < 0 or idx >= t.entries.data.len:
    return
  var j = idx
  while j < t.entries.data.len - 1:
    t.entries.data[j] = move(t.entries.data[j + 1])
    inc j
  t.entries.data.shrink(t.entries.data.len - 1)
  if t.entries.data.len > linearScanLimit:
    fillHashPart(t)
  else:
    t.entries.hashes.shrink(0)

func rawPut[K: Keyable, V](t: var Table[K, V]; k: sink K; v: sink V; h: Hash) =
  ## Appends a missing pair and keeps the index in step. Hashing is delayed
  ## until the fourth entry is exceeded.
  if t.entries.hashes.len == 0:
    if t.entries.data.len >= linearScanLimit:
      fillHashPart(t)
  elif indexNeedsGrowth(t.entries.hashes.len, t.entries.data.len):
    resizeIndex(t.entries.hashes)
    if indexNeedsGrowth(t.entries.hashes.len, t.entries.data.len):
      # The index could not grow. Refuse the insert; the dense sequence must
      # not run past an index whose probe loops still assume a free slot.
      return
  var kh = h
  if t.entries.hashes.len > 0 and kh == 0:
    kh = hash(k)
  let oldLen = t.entries.data.len
  t.entries.data.add (k, v)
  # `data.add` can also fail. Only index when a new pair was actually stored.
  if t.entries.data.len > oldLen and t.entries.hashes.len > 0:
    t.entries.hashes[emptySlot(t.entries.hashes, kh)] =
      HashEntry(fullhash: kh, position: t.entries.data.len)

# ------------------------------ Table -----------------------------------

func initTable*[A, B](initialSize = defaultInitialSize): Table[A, B] =
  ## Creates a new empty hash table. `initialSize` is a capacity hint; the
  ## dense representation grows with its entries.
  discard initialSize
  result = TableCore[(A, B)](
      entries: TableData[(A, B)](data: @[], hashes: @[]))

func toTable*[A: Keyable, B](pairs: openArray[(A, B)]): Table[A, B] =
  ## Creates a table containing `pairs`.
  result = initTable[A, B](pairs.len)
  for (key, val) in pairs:
    result[key] = val

func `[]=`*[A: Keyable, B](t: var Table[A, B]; key: sink A; val: sink B) =
  ## Inserts or updates `(key, val)`.
  var hc: Hash
  let index = rawGet(t, key, hc)
  if index >= 0:
    t.entries.data[index][1] = val
  else:
    rawPut(t, key, val, hc)

func hasKey*[A: Keyable, B](t: Table[A, B]; key: A): bool =
  var hc: Hash
  rawGet(t, key, hc) >= 0

func contains*[A: Keyable, B](t: Table[A, B]; key: A): bool {.inline.} =
  hasKey(t, key)

func hasKeyOrPut*[A: Keyable, B](t: var Table[A, B]; key: A; val: B): bool =
  ## True when `key` is present; otherwise inserts it with `val` and returns
  ## false.
  var hc: Hash
  let index = rawGet(t, key, hc)
  if index >= 0:
    return true
  rawPut(t, key, val, hc)
  false

func getOrDefault*[A: Keyable, B: HasDefault](t: Table[A, B]; key: A): B =
  var hc: Hash
  let index = rawGet(t, key, hc)
  if index >= 0:
    t.entries.data[index][1]
  else:
    default(B)

func getOrDefault*[A: Keyable, B](t: Table[A, B]; key: A; fallback: B): B =
  var hc: Hash
  let index = rawGet(t, key, hc)
  if index >= 0:
    t.entries.data[index][1]
  else:
    fallback

func getOrQuit*[A: Keyable, B](t: Table[A, B]; key: A): var B =
  ## Like `[]`, but terminates the program when `key` is absent.
  var hc: Hash
  let index = rawGet(t, key, hc)
  if index < 0:
    {.cast(noSideEffect).}:
      raiseAssert "key not found"
  result = t.entries.data[index][1]

func mgetOrPut*[A: Keyable, B](t: var Table[A, B]; key: sink A;
                              val: sink B): var B =
  ## Retrieves `t[key]`, inserting `val` when the key is missing. The result is
  ## mutable.
  var hc: Hash
  var index = rawGet(t, key, hc)
  if index < 0:
    let oldLen = t.entries.data.len
    rawPut(t, key, val, hc)
    index = t.entries.data.len - 1
    # A refused insert leaves the old pair at `len - 1`; never return a ref to
    # somebody else's value.
    if index < 0 or t.entries.data.len <= oldLen:
      {.cast(noSideEffect).}:
        raiseAssert "out of memory"
  result = t.entries.data[index][1]

func mgetOrPut*[A: Keyable, B: HasDefault](t: var Table[A, B];
    key: sink A): var B =
  mgetOrPut(t, key, default(B))

func len*[A, B](t: Table[A, B]): int {.inline.} =
  t.entries.data.len

func del*[A: Keyable, B](t: var Table[A, B]; key: A) =
  ## Removes `key`; does nothing when it is absent.
  var hc: Hash
  let index = rawGet(t, key, hc)
  if index >= 0:
    delAt(t, index)

func pop*[A: Keyable, B](t: var Table[A, B]; key: A; val: var B): bool =
  ## Removes `key` and stores its value in `val`.
  var hc: Hash
  let index = rawGet(t, key, hc)
  if index < 0:
    return false
  val = move(t.entries.data[index][1])
  delAt(t, index)
  true

func take*[A: Keyable, B](t: var Table[A, B]; key: A; val: var B): bool {.inline.} =
  pop(t, key, val)

func clear*[A, B](t: var Table[A, B]) =
  t.entries.data.shrink(0)
  t.entries.hashes.shrink(0)

iterator pairs*[A, B](t: Table[A, B]): (lent A, lent B) =
  for i in 0 ..< t.entries.data.len:
    yield (t.entries.data[i][0], t.entries.data[i][1])

iterator mpairs*[A, B](t: var Table[A, B]): (lent A, var B) =
  for i in 0 ..< t.entries.data.len:
    yield (t.entries.data[i][0], t.entries.data[i][1])

iterator keys*[A, B](t: Table[A, B]): lent A =
  for i in 0 ..< t.entries.data.len:
    yield t.entries.data[i][0]

iterator values*[A, B](t: Table[A, B]): lent B =
  for i in 0 ..< t.entries.data.len:
    yield t.entries.data[i][1]

iterator mvalues*[A, B](t: var Table[A, B]): var B =
  for i in 0 ..< t.entries.data.len:
    yield t.entries.data[i][1]

func `$`*[A: Stringable, B: Stringable](t: Table[A, B]): string =
  if t.entries.data.len == 0:
    return "{:}"
  result = "{"
  var first = true
  for i in 0 ..< t.entries.data.len:
    if not first:
      result.add ", "
    first = false
    result.addQuoted t.entries.data[i][0]
    result.add ": "
    result.addQuoted t.entries.data[i][1]
  result.add "}"

func `==`*[T: Equatable](s, t: TableData[T]): bool =
  ## Content equality: insertion order and internal index layout do not matter.
  if s.data.len != t.data.len:
    return false
  if s.data.len == 0:
    return true
  if s.hashes.len > 0 and t.hashes.len > 0:
    for i in 0 ..< s.hashes.len:
      let entry = s.hashes[i]
      if entry.position == 0:
        continue
      if entry.position < 1 or entry.position > s.data.len:
        return false
      let hc = entry.fullhash
      var probe = hc
      var probes = 0
      var found = false
      while probes < t.hashes.len and
          isIndexed(t.hashes[probe and high(t.hashes).uint]):
        let candidate = t.hashes[probe and high(t.hashes).uint]
        if candidate.fullhash == hc and candidate.position >= 1 and
            candidate.position <= t.data.len and
            t.data[candidate.position - 1] == s.data[entry.position - 1]:
          found = true
          break
        inc probe
        inc probes
      if not found:
        return false
    return true
  # At least one table has no index (it is tiny, or an index allocation was
  # refused). The linear scan also keeps equality independent of slot order.
  for i in 0 ..< s.data.len:
    var found = false
    for j in 0 ..< t.data.len:
      if s.data[i] == t.data[j]:
        found = true
        break
    if not found:
      return false
  true

when defined(nimony):
  func `[]`*[A: Keyable, B](t: Table[A, B]; key: A): var B {.raises.} =
    ## Retrieves `key`'s value or raises `KeyError` when absent.
    var hc: Hash
    let index = rawGet(t, key, hc)
    if index < 0:
      raise KeyError
    result = t.entries.data[index][1]
else:
  func `[]`*[A, B](t: var Table[A, B]; key: A): var B {.raises.} =
    var hc: Hash
    let index = rawGet(t, key, hc)
    if index < 0:
      raise newException(KeyError, "key not found")
    result = t.entries.data[index][1]

  func `[]`*[A, B](t: Table[A, B]; key: A): B {.raises.} =
    var hc: Hash
    let index = rawGet(t, key, hc)
    if index < 0:
      raise newException(KeyError, "key not found")
    result = t.entries.data[index][1]

# ---------------------------- TableRef ----------------------------------

proc newTable*[A: Keyable, B](initialSize = defaultInitialSize): TableRef[A, B] =
  ## Creates a new empty ref table.
  var tmp = initTable[A, B](initialSize)
  var inner: ref TableCore[(A, B)]
  new(inner)
  inner[].entries.data = move(tmp.entries.data)
  inner[].entries.hashes = move(tmp.entries.hashes)
  result = TableRef[A, B](impl: TableRefImpl[(A, B)](r: inner))

proc newTable*[A: Keyable, B](pairs: openArray[(A, B)]): TableRef[A, B] =
  ## Creates a new ref table containing `pairs`.
  result = newTable[A, B](pairs.len)
  for (key, val) in pairs:
    result[key] = val

func `[]`*[A, B](t: TableRef[A, B]): var Table[A, B] =
  ## Dereferences a ref table. The result aliases the ref's storage.
  t.impl.r[]

func `[]`*[A: Keyable, B](t: TableRef[A, B]; key: A): var B {.raises.} =
  t.impl.r[][key]

proc `[]=`*[A: Keyable, B](t: TableRef[A, B]; key: sink A; val: sink B) =
  t.impl.r[][key] = val

func hasKey*[A: Keyable, B](t: TableRef[A, B]; key: A): bool {.inline.} =
  t.impl.r[].hasKey(key)

func contains*[A: Keyable, B](t: TableRef[A, B]; key: A): bool {.inline.} =
  t.impl.r[].contains(key)

proc hasKeyOrPut*[A: Keyable, B](t: TableRef[A, B]; key: A; val: B): bool =
  t.impl.r[].hasKeyOrPut(key, val)

func getOrDefault*[A: Keyable, B: HasDefault](t: TableRef[A, B]; key: A): B =
  t.impl.r[].getOrDefault(key)

func getOrDefault*[A: Keyable, B](t: TableRef[A, B]; key: A; fallback: B): B =
  t.impl.r[].getOrDefault(key, fallback)

func getOrQuit*[A: Keyable, B](t: TableRef[A, B]; key: A): var B =
  t.impl.r[].getOrQuit(key)

proc mgetOrPut*[A: Keyable, B](t: TableRef[A, B]; key: sink A;
                              val: sink B): var B =
  t.impl.r[].mgetOrPut(key, val)

proc mgetOrPut*[A: Keyable, B: HasDefault](t: TableRef[A, B];
    key: sink A): var B =
  t.impl.r[].mgetOrPut(key)

func len*[A, B](t: TableRef[A, B]): int {.inline.} =
  t.impl.r[].entries.data.len

proc del*[A: Keyable, B](t: TableRef[A, B]; key: A) =
  t.impl.r[].del(key)

proc pop*[A: Keyable, B](t: TableRef[A, B]; key: A; val: var B): bool =
  t.impl.r[].pop(key, val)

proc take*[A: Keyable, B](t: TableRef[A, B]; key: A; val: var B): bool {.inline.} =
  pop(t, key, val)

proc clear*[A, B](t: TableRef[A, B]) =
  t.impl.r[].clear()

iterator pairs*[A, B](t: TableRef[A, B]): (lent A, lent B) =
  for i in 0 ..< t.impl.r[].entries.data.len:
    yield (t.impl.r[].entries.data[i][0],
           t.impl.r[].entries.data[i][1])

iterator mpairs*[A, B](t: TableRef[A, B]): (lent A, var B) =
  for i in 0 ..< t.impl.r[].entries.data.len:
    yield (t.impl.r[].entries.data[i][0],
           t.impl.r[].entries.data[i][1])

iterator keys*[A, B](t: TableRef[A, B]): lent A =
  for i in 0 ..< t.impl.r[].entries.data.len:
    yield t.impl.r[].entries.data[i][0]

iterator values*[A, B](t: TableRef[A, B]): lent B =
  for i in 0 ..< t.impl.r[].entries.data.len:
    yield t.impl.r[].entries.data[i][1]

iterator mvalues*[A, B](t: TableRef[A, B]): var B =
  for i in 0 ..< t.impl.r[].entries.data.len:
    yield t.impl.r[].entries.data[i][1]

func `$`*[A: Stringable, B: Stringable](t: TableRef[A, B]): string =
  $t[]

func `==`*[T: Equatable](a, b: TableRefImpl[T]): bool =
  ## Exported for the structural equality instantiated in importing modules.
  a.r[] == b.r[]

# --------------------------- OrderedTable --------------------------------

type
  OrderedKeyValuePair[T] = tuple[hcode: Hash, next: int, item: T]
  OrderedKeyValuePairSeq[T] = seq[OrderedKeyValuePair[T]]
  OrderedTableData[T] = object
    data: OrderedKeyValuePairSeq[T]
    counter, first, last: int
  OrderedTableCore[T] = object
    entries: OrderedTableData[T]
  OrderedTable*[A, B] = OrderedTableCore[(A, B)] ## Hash table that remembers
    ## insertion order.

  OrderedTableRefImpl[T] = object
    r: ref OrderedTableCore[T]
  OrderedTableRef*[A, B] = object ## Ref version of `OrderedTable`.
    impl: OrderedTableRefImpl[(A, B)]

func ordRawGetKnownHC[A: Keyable, B](t: OrderedTable[A, B]; key: A;
                                    hc: Hash): int =
  result = -1
  if t.entries.data.len > 0:
    let mask = t.entries.data.len - 1
    var h = initialProbe(hc, t.entries.data.len)
    var searching = true
    while searching and 0 <= h and h < t.entries.data.len and
        isFilled(t.entries.data[h].hcode):
      if t.entries.data[h].hcode == hc and t.entries.data[h].item[0] == key:
        result = h
        searching = false
      else:
        h = nextProbe(h, mask)
    if searching:
      result = -1 - h

func ordRawGet[A: Keyable, B](t: OrderedTable[A, B]; key: A; hc: out Hash): int =
  hc = genHash(key)
  result = ordRawGetKnownHC(t, key, hc)

func ordRawInsert[A, B](t: var OrderedTable[A, B]; key: sink A; val: sink B;
                     hc: Hash; h: int) {.
    requires: 0 <= h and h < t.entries.data.len.} =
  t.entries.data[h] = (hcode: hc, next: -1, item: (key, val))
  if t.entries.first < 0:
    t.entries.first = h
  if t.entries.last >= 0:
    {.assume: 0 <= t.entries.last and
              t.entries.last < t.entries.data.len.}
    t.entries.data[t.entries.last].next = h
  t.entries.last = h

func ordInitImpl[A, B](t: var OrderedTable[A, B]; initialSize: int): bool =
  let correctSize = slotsNeeded(initialSize)
  var s = newSeq[OrderedKeyValuePair[(A, B)]](correctSize)
  if s.len < correctSize:
    return false
  t.entries.data = s
  t.entries.counter = 0
  t.entries.first = -1
  t.entries.last = -1
  true

func ordCheckIfInitialized[A, B](t: var OrderedTable[A, B]): bool =
  if t.entries.data.len > 0:
    return true
  ordInitImpl(t, defaultInitialSize)

func ordEnlarge[A: Keyable, B](t: var OrderedTable[A, B]): bool =
  let oldLen = t.entries.data.len
  if oldLen == 0:
    return false
  var n = newSeq[OrderedKeyValuePair[(A, B)]](oldLen * growthFactor)
  if n.len < oldLen * growthFactor:
    return false
  var h = t.entries.first
  t.entries.first = -1
  t.entries.last = -1
  swap(t.entries.data, n)
  let mask = t.entries.data.len - 1
  while 0 <= h and h < n.len:
    let nxt = n[h].next
    let eh = n[h].hcode
    if isFilled(eh):
      var j = -1 - ordRawGetKnownHC(t, n[h].item[0], eh)
      {.assume: 0 <= j and j < t.entries.data.len.}
      var kv = move(n[h].item)
      ordRawInsert(t, move(kv[0]), move(kv[1]), eh, j)
    h = nxt
  true

func ordPutWithHash[A: Keyable, B](t: var OrderedTable[A, B]; key: sink A;
                                val: sink B; hc: Hash; index: int): int =
  var idx = index
  {.assume: t.entries.data.len > t.entries.counter.}
  if mustRehash(t.entries.data.len, t.entries.counter):
    if not ordEnlarge(t):
      return -1
    idx = ordRawGetKnownHC(t, key, hc)
  idx = -1 - idx
  {.assume: 0 <= idx and idx < t.entries.data.len.}
  ordRawInsert(t, key, val, hc, idx)
  inc t.entries.counter
  result = idx

func initOrderedTable*[A, B](
    initialSize = defaultInitialSize): OrderedTable[A, B] =
  ## Creates a new empty ordered table.
  result = OrderedTableCore[(A, B)](
      entries: OrderedTableData[(A, B)](
          data: default(OrderedKeyValuePairSeq[(A, B)]),
          counter: 0, first: -1, last: -1))
  discard ordInitImpl(result, initialSize)

func toOrderedTable*[A: Keyable, B](
    pairs: openArray[(A, B)]): OrderedTable[A, B] =
  ## Creates an ordered table containing `pairs`. A repeated key keeps its
  ## first position and its last value.
  result = initOrderedTable[A, B](pairs.len)
  for (key, val) in pairs:
    result[key] = val

func `[]=`*[A: Keyable, B](t: var OrderedTable[A, B]; key: sink A;
                          val: sink B) =
  if not ordCheckIfInitialized(t):
    return
  var hc: Hash
  let index = ordRawGet(t, key, hc)
  if index >= 0:
    {.assume: 0 <= index and index < t.entries.data.len.}
    t.entries.data[index].item[1] = val
  else:
    discard ordPutWithHash(t, key, val, hc, index)

when defined(nimony):
  func `[]`*[A: Keyable, B](t: OrderedTable[A, B]; key: A): var B {.raises.} =
    ## Retrieves `key`'s value or raises `KeyError` when absent.
    var hc: Hash
    let index = ordRawGet(t, key, hc)
    if index < 0:
      raise KeyError
    {.assume: 0 <= index and index < t.entries.data.len.}
    result = t.entries.data[index].item[1]
else:
  func `[]`*[A, B](t: var OrderedTable[A, B]; key: A): var B {.raises.} =
    var hc: Hash
    let index = ordRawGet(t, key, hc)
    if index < 0:
      raise newException(KeyError, "key not found")
    {.assume: 0 <= index and index < t.entries.data.len.}
    result = t.entries.data[index].item[1]

  func `[]`*[A, B](t: OrderedTable[A, B]; key: A): B {.raises.} =
    var hc: Hash
    let index = ordRawGet(t, key, hc)
    if index < 0:
      raise newException(KeyError, "key not found")
    {.assume: 0 <= index and index < t.entries.data.len.}
    result = t.entries.data[index].item[1]

func hasKey*[A: Keyable, B](t: OrderedTable[A, B]; key: A): bool =
  var hc: Hash
  ordRawGet(t, key, hc) >= 0

func contains*[A: Keyable, B](t: OrderedTable[A, B]; key: A): bool {.inline.} =
  hasKey(t, key)

func hasKeyOrPut*[A: Keyable, B](t: var OrderedTable[A, B]; key: A;
                                val: B): bool =
  if not ordCheckIfInitialized(t):
    return false
  var hc: Hash
  let index = ordRawGet(t, key, hc)
  if index >= 0:
    return true
  discard ordPutWithHash(t, key, val, hc, index)
  false

func getOrDefault*[A: Keyable, B: HasDefault](
    t: OrderedTable[A, B]; key: A): B =
  var hc: Hash
  let index = ordRawGet(t, key, hc)
  if index >= 0:
    {.assume: 0 <= index and index < t.entries.data.len.}
    t.entries.data[index].item[1]
  else:
    default(B)

func getOrDefault*[A: Keyable, B](t: OrderedTable[A, B]; key: A;
                                  fallback: B): B =
  var hc: Hash
  let index = ordRawGet(t, key, hc)
  if index >= 0:
    {.assume: 0 <= index and index < t.entries.data.len.}
    t.entries.data[index].item[1]
  else:
    fallback

func getOrQuit*[A: Keyable, B](t: OrderedTable[A, B]; key: A): var B =
  var hc: Hash
  let index = ordRawGet(t, key, hc)
  if index < 0:
    {.cast(noSideEffect).}:
      raiseAssert "key not found"
  {.assume: 0 <= index and index < t.entries.data.len.}
  result = t.entries.data[index].item[1]

func mgetOrPut*[A: Keyable, B](t: var OrderedTable[A, B]; key: sink A;
                              val: sink B): var B =
  if not ordCheckIfInitialized(t):
    {.cast(noSideEffect).}:
      raiseAssert "out of memory"
  var hc: Hash
  var index = ordRawGet(t, key, hc)
  if index < 0:
    index = ordPutWithHash(t, key, val, hc, index)
    if index < 0:
      {.cast(noSideEffect).}:
        raiseAssert "out of memory"
  {.assume: 0 <= index and index < t.entries.data.len.}
  result = t.entries.data[index].item[1]

func mgetOrPut*[A: Keyable, B: HasDefault](t: var OrderedTable[A, B];
    key: sink A): var B =
  mgetOrPut(t, key, default(B))

func len*[A, B](t: OrderedTable[A, B]): int {.inline.} =
  t.entries.counter

func ordDelImpl[A: Keyable, B](t: var OrderedTable[A, B]; key: A;
    n: var OrderedKeyValuePairSeq[(A, B)]): bool =
  ## Rebuilds `t` into the caller-provided `n`, dropping `key`. `n` must
  ## already be large enough; no allocation happens here.
  if t.entries.counter == 0:
    return true
  let hc = genHash(key)
  if ordRawGetKnownHC(t, key, hc) < 0:
    return true
  let oldLen = t.entries.data.len
  if n.len < oldLen:
    return false
  var h = t.entries.first
  t.entries.first = -1
  t.entries.last = -1
  swap(t.entries.data, n)
  while 0 <= h and h < n.len:
    let nxt = n[h].next
    let eh = n[h].hcode
    if isFilled(eh):
      if eh == hc and n[h].item[0] == key:
        dec t.entries.counter
      else:
        let j = -1 - ordRawGetKnownHC(t, n[h].item[0], eh)
        {.assume: 0 <= j and j < t.entries.data.len.}
        var kv = move(n[h].item)
        ordRawInsert(t, move(kv[0]), move(kv[1]), eh, j)
    h = nxt
  true

func del*[A: Keyable, B](t: var OrderedTable[A, B]; key: A) =
  ## Removes `key`; does nothing when it is absent.
  if t.entries.counter == 0:
    return
  let oldLen = t.entries.data.len
  var n = newSeq[OrderedKeyValuePair[(A, B)]](oldLen)
  if n.len < oldLen:
    return
  discard ordDelImpl(t, key, n)

func pop*[A: Keyable, B](t: var OrderedTable[A, B]; key: A;
                         val: var B): bool =
  var hc: Hash
  let index = ordRawGet(t, key, hc)
  if index < 0:
    return false
  let oldLen = t.entries.data.len
  var n = newSeq[OrderedKeyValuePair[(A, B)]](oldLen)
  if n.len < oldLen:
    return false
  {.assume: 0 <= index and index < t.entries.data.len.}
  val = move(t.entries.data[index].item[1])
  discard ordDelImpl(t, key, n)
  true

func take*[A: Keyable, B](t: var OrderedTable[A, B]; key: A;
                          val: var B): bool {.inline.} =
  pop(t, key, val)

func clear*[A, B](t: var OrderedTable[A, B]) =
  for i in 0 ..< t.entries.data.len:
    t.entries.data[i] = default(OrderedKeyValuePair[(A, B)])
  t.entries.counter = 0
  t.entries.first = -1
  t.entries.last = -1

proc sort*[A: Keyable, B](t: var OrderedTable[A, B];
    cmp: proc (x, y: (A, B)): int; order = SortOrder.Ascending) =
  ## Sorts the internal insertion-order list. Lookups still work afterwards.
  var list = t.entries.first
  var p, q, e, tail: int
  var nmerges, psize, qsize, i: int
  if t.entries.counter == 0:
    return
  let dataLen = t.entries.data.len
  {.assume: dataLen > 0.}
  var insize = 1
  while true:
    p = list
    list = -1
    tail = -1
    nmerges = 0
    while 0 <= p and p < dataLen:
      inc nmerges
      q = p
      psize = 0
      i = 0
      while i < insize and 0 <= q and q < dataLen:
        inc psize
        q = t.entries.data[q].next
        if q < 0:
          break
        inc i
      qsize = insize
      while psize > 0 or (qsize > 0 and q >= 0):
        if psize == 0:
          {.assume: 0 <= q and q < dataLen.}
          e = q
          q = t.entries.data[q].next
          dec qsize
        elif qsize == 0 or q < 0:
          {.assume: 0 <= p and p < dataLen.}
          e = p
          p = t.entries.data[p].next
          dec psize
        else:
          {.assume: 0 <= p and p < dataLen.}
          {.assume: 0 <= q and q < dataLen.}
          if cmp((t.entries.data[p].item[0], t.entries.data[p].item[1]),
                 (t.entries.data[q].item[0], t.entries.data[q].item[1])) *
                 order <= 0:
            e = p
            p = t.entries.data[p].next
            dec psize
          else:
            e = q
            q = t.entries.data[q].next
            dec qsize
        if tail >= 0:
          {.assume: 0 <= tail and tail < dataLen.}
          t.entries.data[tail].next = e
        else:
          list = e
        tail = e
      p = q
    {.assume: 0 <= tail and tail < dataLen.}
    t.entries.data[tail].next = -1
    if nmerges <= 1:
      break
    insize = insize * 2
  t.entries.first = list
  t.entries.last = tail

iterator pairs*[A, B](t: OrderedTable[A, B]): (lent A, lent B) =
  var h = t.entries.first
  while 0 <= h and h < t.entries.data.len:
    let nxt = t.entries.data[h].next
    if isFilled(t.entries.data[h].hcode):
      yield (t.entries.data[h].item[0], t.entries.data[h].item[1])
    h = nxt

iterator mpairs*[A, B](t: var OrderedTable[A, B]): (lent A, var B) =
  var h = t.entries.first
  while 0 <= h and h < t.entries.data.len:
    let nxt = t.entries.data[h].next
    if isFilled(t.entries.data[h].hcode):
      yield (t.entries.data[h].item[0], t.entries.data[h].item[1])
    h = nxt

iterator keys*[A, B](t: OrderedTable[A, B]): lent A =
  var h = t.entries.first
  while 0 <= h and h < t.entries.data.len:
    let nxt = t.entries.data[h].next
    if isFilled(t.entries.data[h].hcode):
      yield t.entries.data[h].item[0]
    h = nxt

iterator values*[A, B](t: OrderedTable[A, B]): lent B =
  var h = t.entries.first
  while 0 <= h and h < t.entries.data.len:
    let nxt = t.entries.data[h].next
    if isFilled(t.entries.data[h].hcode):
      yield t.entries.data[h].item[1]
    h = nxt

iterator mvalues*[A, B](t: var OrderedTable[A, B]): var B =
  var h = t.entries.first
  while 0 <= h and h < t.entries.data.len:
    let nxt = t.entries.data[h].next
    if isFilled(t.entries.data[h].hcode):
      yield t.entries.data[h].item[1]
    h = nxt

func `$`*[A: Stringable, B: Stringable](t: OrderedTable[A, B]): string =
  if t.entries.counter == 0:
    return "{:}"
  result = "{"
  var h = t.entries.first
  var first = true
  while 0 <= h and h < t.entries.data.len:
    let nxt = t.entries.data[h].next
    if isFilled(t.entries.data[h].hcode):
      if not first:
        result.add ", "
      first = false
      result.addQuoted t.entries.data[h].item[0]
      result.add ": "
      result.addQuoted t.entries.data[h].item[1]
    h = nxt
  result.add "}"

func `==`*[T: Equatable](s, t: OrderedTableData[T]): bool =
  ## Ordered equality: both the contents and the order must match.
  if s.counter != t.counter:
    return false
  if s.counter == 0:
    return true
  var hs = s.first
  var ht = t.first
  while 0 <= hs and hs < s.data.len and 0 <= ht and ht < t.data.len:
    if not (s.data[hs].item == t.data[ht].item):
      return false
    hs = s.data[hs].next
    ht = t.data[ht].next
  result = hs < 0 and ht < 0
# --------------------------- OrderedTableRef -----------------------------

proc newOrderedTable*[A: Keyable, B](
    initialSize = defaultInitialSize): OrderedTableRef[A, B] =
  var tmp = initOrderedTable[A, B](initialSize)
  var inner: ref OrderedTableCore[(A, B)]
  new(inner)
  inner[].entries.data = move(tmp.entries.data)
  inner[].entries.counter = tmp.entries.counter
  inner[].entries.first = tmp.entries.first
  inner[].entries.last = tmp.entries.last
  result = OrderedTableRef[A, B](
      impl: OrderedTableRefImpl[(A, B)](r: inner))

proc newOrderedTable*[A: Keyable, B](
    pairs: openArray[(A, B)]): OrderedTableRef[A, B] =
  result = newOrderedTable[A, B](pairs.len)
  for (key, val) in pairs:
    result[key] = val

func `[]`*[A, B](t: OrderedTableRef[A, B]): var OrderedTable[A, B] =
  ## Dereferences a ref ordered table. The result aliases the ref's storage.
  t.impl.r[]

func `[]`*[A: Keyable, B](t: OrderedTableRef[A, B]; key: A): var B {.raises.} =
  t.impl.r[][key]

proc `[]=`*[A: Keyable, B](t: OrderedTableRef[A, B]; key: sink A;
                          val: sink B) =
  t.impl.r[][key] = val

func hasKey*[A: Keyable, B](t: OrderedTableRef[A, B]; key: A): bool {.inline.} =
  t.impl.r[].hasKey(key)

func contains*[A: Keyable, B](
    t: OrderedTableRef[A, B]; key: A): bool {.inline.} =
  t.impl.r[].contains(key)

proc hasKeyOrPut*[A: Keyable, B](t: OrderedTableRef[A, B]; key: A;
                                val: B): bool =
  t.impl.r[].hasKeyOrPut(key, val)

func getOrDefault*[A: Keyable, B: HasDefault](
    t: OrderedTableRef[A, B]; key: A): B =
  t.impl.r[].getOrDefault(key)

func getOrDefault*[A: Keyable, B](t: OrderedTableRef[A, B]; key: A;
                                  fallback: B): B =
  t.impl.r[].getOrDefault(key, fallback)

func getOrQuit*[A: Keyable, B](
    t: OrderedTableRef[A, B]; key: A): var B =
  t.impl.r[].getOrQuit(key)

proc mgetOrPut*[A: Keyable, B](t: OrderedTableRef[A, B]; key: sink A;
                              val: sink B): var B =
  t.impl.r[].mgetOrPut(key, val)

proc mgetOrPut*[A: Keyable, B: HasDefault](
    t: OrderedTableRef[A, B]; key: sink A): var B =
  t.impl.r[].mgetOrPut(key)

func len*[A, B](t: OrderedTableRef[A, B]): int {.inline.} =
  t.impl.r[].entries.counter

proc del*[A: Keyable, B](t: OrderedTableRef[A, B]; key: A) =
  t.impl.r[].del(key)

proc pop*[A: Keyable, B](t: OrderedTableRef[A, B]; key: A; val: var B): bool =
  t.impl.r[].pop(key, val)

proc take*[A: Keyable, B](t: OrderedTableRef[A, B]; key: A;
                          val: var B): bool {.inline.} =
  pop(t, key, val)

proc clear*[A, B](t: OrderedTableRef[A, B]) =
  t.impl.r[].clear()

proc sort*[A: Keyable, B](t: OrderedTableRef[A, B];
    cmp: proc (x, y: (A, B)): int; order = SortOrder.Ascending) =
  t.impl.r[].sort(cmp, order)

iterator pairs*[A, B](t: OrderedTableRef[A, B]): (lent A, lent B) =
  var h = t.impl.r[].entries.first
  while 0 <= h and h < t.impl.r[].entries.data.len:
    let nxt = t.impl.r[].entries.data[h].next
    if isFilled(t.impl.r[].entries.data[h].hcode):
      yield (t.impl.r[].entries.data[h].item[0],
             t.impl.r[].entries.data[h].item[1])
    h = nxt

iterator mpairs*[A, B](t: OrderedTableRef[A, B]): (lent A, var B) =
  var h = t.impl.r[].entries.first
  while 0 <= h and h < t.impl.r[].entries.data.len:
    let nxt = t.impl.r[].entries.data[h].next
    if isFilled(t.impl.r[].entries.data[h].hcode):
      yield (t.impl.r[].entries.data[h].item[0],
             t.impl.r[].entries.data[h].item[1])
    h = nxt

iterator keys*[A, B](t: OrderedTableRef[A, B]): lent A =
  var h = t.impl.r[].entries.first
  while 0 <= h and h < t.impl.r[].entries.data.len:
    let nxt = t.impl.r[].entries.data[h].next
    if isFilled(t.impl.r[].entries.data[h].hcode):
      yield t.impl.r[].entries.data[h].item[0]
    h = nxt

iterator values*[A, B](t: OrderedTableRef[A, B]): lent B =
  var h = t.impl.r[].entries.first
  while 0 <= h and h < t.impl.r[].entries.data.len:
    let nxt = t.impl.r[].entries.data[h].next
    if isFilled(t.impl.r[].entries.data[h].hcode):
      yield t.impl.r[].entries.data[h].item[1]
    h = nxt

iterator mvalues*[A, B](t: OrderedTableRef[A, B]): var B =
  var h = t.impl.r[].entries.first
  while 0 <= h and h < t.impl.r[].entries.data.len:
    let nxt = t.impl.r[].entries.data[h].next
    if isFilled(t.impl.r[].entries.data[h].hcode):
      yield t.impl.r[].entries.data[h].item[1]
    h = nxt

func `$`*[A: Stringable, B: Stringable](t: OrderedTableRef[A, B]): string =
  $t[]

func `==`*[T: Equatable](a, b: OrderedTableRefImpl[T]): bool =
  ## Exported for the structural equality instantiated in importing modules.
  a.r[] == b.r[]

# ----------------------------- CountTable --------------------------------

type
  CountEntry[A] = tuple[key: A, val: int]
  CountEntrySeq[A] = seq[CountEntry[A]]
  CountTableData[A] = object
    data: CountEntrySeq[A]
    counter: int
    isSorted: bool
  CountTableCore[A] = object
    entries: CountTableData[A]
  CountTable*[A] = CountTableCore[A] ## Hash table that counts the occurrences
    ## of keys.

  CountTableRefImpl[A] = object
    r: ref CountTableCore[A]
  CountTableRef*[A] = object ## Ref version of `CountTable`.
    impl: CountTableRefImpl[A]

func ctRawGet[A: Keyable](t: CountTable[A]; key: A): int =
  result = -1
  if t.entries.data.len > 0:
    let mask = t.entries.data.len - 1
    var h = initialProbe(genHash(key), t.entries.data.len)
    var searching = true
    while searching and 0 <= h and h < t.entries.data.len and
        t.entries.data[h].val != 0:
      if t.entries.data[h].key == key:
        result = h
        searching = false
      else:
        h = nextProbe(h, mask)
    if searching:
      result = -1 - h

func ctRawInsert[A](data: var CountEntrySeq[A]; key: sink A; val: int;
                    h: int) {.requires: 0 <= h and h < data.len.} =
  data[h] = (key: key, val: val)

func ctInitImpl[A](t: var CountTable[A]; initialSize: int): bool =
  let correctSize = slotsNeeded(initialSize)
  var s = newSeq[CountEntry[A]](correctSize)
  if s.len < correctSize:
    return false
  t.entries.data = s
  t.entries.counter = 0
  t.entries.isSorted = false
  true

func ctCheckIfInitialized[A](t: var CountTable[A]): bool =
  if t.entries.data.len > 0:
    return true
  ctInitImpl(t, defaultInitialSize)

func ctEnlarge[A: Keyable](t: var CountTable[A]): bool =
  let oldLen = t.entries.data.len
  if oldLen == 0:
    return false
  var n = newSeq[CountEntry[A]](oldLen * growthFactor)
  if n.len < oldLen * growthFactor:
    return false
  swap(t.entries.data, n)
  let dataLen = t.entries.data.len
  if dataLen == 0:
    return false
  let mask = dataLen - 1
  for i in 0 ..< n.len:
    if n[i].val != 0:
      var j = initialProbe(genHash(n[i].key), dataLen)
      while 0 <= j and j < t.entries.data.len and
          t.entries.data[j].val != 0:
        j = nextProbe(j, mask)
      let v = n[i].val
      {.assume: 0 <= j and j < t.entries.data.len.}
      ctRawInsert(t.entries.data, move(n[i].key), v, j)
  true

func putCount[A: Keyable](t: var CountTable[A]; key: A; val: int;
                          index: int): bool =
  ## Inserts a missing key with a positive count.
  var idx = index
  {.assume: t.entries.data.len > t.entries.counter.}
  if mustRehash(t.entries.data.len, t.entries.counter):
    if not ctEnlarge(t):
      return false
    idx = ctRawGet(t, key)
  idx = -1 - idx
  {.assume: 0 <= idx and idx < t.entries.data.len.}
  ctRawInsert(t.entries.data, key, val, idx)
  inc t.entries.counter
  true

func countDelIdx[A: Keyable](t: var CountTable[A]; i: int) =
  if i < 0 or i >= t.entries.data.len:
    return
  let dataLen = t.entries.data.len
  if dataLen == 0:
    return
  let mask = dataLen - 1
  var i = i
  dec t.entries.counter
  block outer:
    while true:
      var j = i
      {.assume: 0 <= i and i < t.entries.data.len.}
      t.entries.data[i] = default(CountEntry[A])
      while true:
        i = nextProbe(i, mask)
        {.assume: 0 <= i and i < t.entries.data.len.}
        if t.entries.data[i].val == 0:
          break outer
        let r = initialProbe(genHash(t.entries.data[i].key), t.entries.data.len)
        if not ((i >= r and r > j) or (r > j and j > i) or (j > i and i >= r)):
          break
      {.assume: 0 <= j and j < t.entries.data.len.}
      t.entries.data[j] = move(t.entries.data[i])

func initCountTable*[A](initialSize = defaultInitialSize): CountTable[A] =
  ## Creates a new empty count table.
  result = CountTableCore[A](
      entries: CountTableData[A](
          data: default(CountEntrySeq[A]), counter: 0, isSorted: false))
  discard ctInitImpl(result, initialSize)

func toCountTable*[A: Keyable](keys: openArray[A]): CountTable[A] =
  ## Counts how often each element of `keys` occurs.
  result = initCountTable[A](keys.len)
  for key in keys:
    result.inc(key)

func `[]`*[A: Keyable](t: CountTable[A]; key: A): int =
  ## Returns the count of `key`, or zero when absent.
  assert(not t.entries.isSorted, "CountTable must not be used after sorting")
  let index = ctRawGet(t, key)
  if index >= 0:
    {.assume: 0 <= index and index < t.entries.data.len.}
    t.entries.data[index].val
  else:
    0

func `[]=`*[A: Keyable](t: var CountTable[A]; key: A; val: int) =
  assert(not t.entries.isSorted, "CountTable must not be used after sorting")
  assert(val >= 0, "CountTable values must be non-negative")
  if val == 0:
    t.del(key)
    return
  if not ctCheckIfInitialized(t):
    return
  let index = ctRawGet(t, key)
  if index >= 0:
    {.assume: 0 <= index and index < t.entries.data.len.}
    t.entries.data[index].val = val
  else:
    discard putCount(t, key, val, index)

func inc*[A: Keyable](t: var CountTable[A]; key: A; val = 1) =
  ## Increments `t[key]` by `val`.
  assert(not t.entries.isSorted, "CountTable must not be used after sorting")
  if not ctCheckIfInitialized(t):
    return
  let index = ctRawGet(t, key)
  if index >= 0:
    {.assume: 0 <= index and index < t.entries.data.len.}
    let next = t.entries.data[index].val + val
    if next == 0:
      countDelIdx(t, index)
    else:
      t.entries.data[index].val = next
  elif val != 0:
    discard putCount(t, key, val, index)

func len*[A](t: CountTable[A]): int {.inline.} =
  t.entries.counter

func smallest*[A](t: CountTable[A]): CountEntry[A] =
  assert(t.len > 0, "counttable is empty")
  var minIdx = -1
  for h in 0 ..< t.entries.data.len:
    if t.entries.data[h].val > 0:
      if minIdx < 0:
        minIdx = h
      else:
        {.assume: 0 <= minIdx and minIdx < t.entries.data.len.}
        if t.entries.data[minIdx].val > t.entries.data[h].val:
          minIdx = h
  {.assume: 0 <= minIdx and minIdx < t.entries.data.len.}
  result = (t.entries.data[minIdx].key, t.entries.data[minIdx].val)

func largest*[A](t: CountTable[A]): CountEntry[A] =
  assert(t.len > 0, "counttable is empty")
  {.assume: t.entries.data.len > 0.}
  var maxIdx = 0
  {.assume: maxIdx < t.entries.data.len.}
  for h in 1 ..< t.entries.data.len:
    {.assume: 0 <= maxIdx and maxIdx < t.entries.data.len.}
    if t.entries.data[maxIdx].val < t.entries.data[h].val:
      maxIdx = h
  {.assume: 0 <= maxIdx and maxIdx < t.entries.data.len.}
  result = (t.entries.data[maxIdx].key, t.entries.data[maxIdx].val)

func hasKey*[A: Keyable](t: CountTable[A]; key: A): bool =
  assert(not t.entries.isSorted, "CountTable must not be used after sorting")
  ctRawGet(t, key) >= 0

func contains*[A: Keyable](t: CountTable[A]; key: A): bool {.inline.} =
  hasKey(t, key)

func getOrDefault*[A: Keyable](t: CountTable[A]; key: A; def = 0): int =
  let index = ctRawGet(t, key)
  if index >= 0:
    {.assume: 0 <= index and index < t.entries.data.len.}
    t.entries.data[index].val
  else:
    def

func del*[A: Keyable](t: var CountTable[A]; key: A) =
  ## Removes `key`; does nothing when it is absent.
  if t.entries.data.len == 0:
    return
  let mask = t.entries.data.len - 1
  var h = initialProbe(genHash(key), t.entries.data.len)
  {.assume: 0 <= h and h < t.entries.data.len.}
  while 0 <= h and h < t.entries.data.len and t.entries.data[h].val != 0:
    if t.entries.data[h].key == key:
      countDelIdx(t, h)
      return
    h = nextProbe(h, mask)
    {.assume: 0 <= h and h < t.entries.data.len.}

func pop*[A: Keyable](t: var CountTable[A]; key: A; val: var int): bool =
  let index = ctRawGet(t, key)
  if index < 0:
    return false
  {.assume: 0 <= index and index < t.entries.data.len.}
  val = t.entries.data[index].val
  countDelIdx(t, index)
  true

func clear*[A](t: var CountTable[A]) =
  for i in 0 ..< t.entries.data.len:
    t.entries.data[i] = default(CountEntry[A])
  t.entries.counter = 0
  t.entries.isSorted = false

func ctCmp[A](a, b: CountEntry[A]): int =
  if a.val < b.val: -1
  elif a.val > b.val: 1
  else: 0

proc sort*[A](t: var CountTable[A]; order = SortOrder.Descending) =
  ## Sorts by count. Once sorted, the table must not be modified.
  algorithm.sort(t.entries.data, proc (x, y: CountEntry[A]): int = ctCmp(x, y), order)
  t.entries.isSorted = true

proc merge*[A: Keyable](s: var CountTable[A]; t: CountTable[A]) =
  assert(not s.entries.isSorted, "CountTable must not be used after sorting")
  for h in 0 ..< t.entries.data.len:
    if t.entries.data[h].val != 0:
      s.inc(t.entries.data[h].key, t.entries.data[h].val)

iterator pairs*[A](t: CountTable[A]): (lent A, int) =
  for h in 0 ..< t.entries.data.len:
    if t.entries.data[h].val != 0:
      yield (t.entries.data[h].key, t.entries.data[h].val)

iterator mpairs*[A](t: var CountTable[A]): (lent A, var int) =
  for h in 0 ..< t.entries.data.len:
    if t.entries.data[h].val != 0:
      yield (t.entries.data[h].key, t.entries.data[h].val)

iterator keys*[A](t: CountTable[A]): lent A =
  for h in 0 ..< t.entries.data.len:
    if t.entries.data[h].val != 0:
      yield t.entries.data[h].key

iterator values*[A](t: CountTable[A]): int =
  for h in 0 ..< t.entries.data.len:
    if t.entries.data[h].val != 0:
      yield t.entries.data[h].val

iterator mvalues*[A](t: var CountTable[A]): var int =
  for h in 0 ..< t.entries.data.len:
    if t.entries.data[h].val != 0:
      yield t.entries.data[h].val

func `$`*[A: Stringable](t: CountTable[A]): string =
  if t.entries.counter == 0:
    return "{:}"
  result = "{"
  var first = true
  for h in 0 ..< t.entries.data.len:
    if t.entries.data[h].val != 0:
      if not first:
        result.add ", "
      first = false
      result.addQuoted t.entries.data[h].key
      result.add ": "
      result.addQuoted t.entries.data[h].val
  result.add "}"

func `==`*[A: Keyable](s, t: CountTableData[A]): bool =
  if s.counter != t.counter:
    return false
  if s.counter == 0:
    return true
  if t.data.len == 0:
    return false
  let mask = t.data.len - 1
  for h in 0 ..< s.data.len:
    if s.data[h].val != 0:
      let hc = genHash(s.data[h].key)
      var i = initialProbe(hc, t.data.len)
      var found = false
      while 0 <= i and i < t.data.len and t.data[i].val != 0:
        if t.data[i].key == s.data[h].key:
          if t.data[i].val != s.data[h].val:
            return false
          found = true
          break
        i = nextProbe(i, mask)
      if not found:
        return false
  true
# --------------------------- CountTableRef -------------------------------

proc newCountTable*[A: Keyable](
    initialSize = defaultInitialSize): CountTableRef[A] =
  var tmp = initCountTable[A](initialSize)
  var inner: ref CountTableCore[A]
  new(inner)
  inner[].entries.data = move(tmp.entries.data)
  inner[].entries.counter = tmp.entries.counter
  inner[].entries.isSorted = tmp.entries.isSorted
  result = CountTableRef[A](impl: CountTableRefImpl[A](r: inner))

proc newCountTable*[A: Keyable](keys: openArray[A]): CountTableRef[A] =
  result = newCountTable[A](keys.len)
  for key in keys:
    result.inc(key)

func `[]`*[A](t: CountTableRef[A]): var CountTable[A] =
  ## Dereferences a ref count table. The result aliases the ref's storage.
  t.impl.r[]

func `[]`*[A: Keyable](t: CountTableRef[A]; key: A): int =
  t.impl.r[][key]

proc `[]=`*[A: Keyable](t: CountTableRef[A]; key: A; val: int) =
  t.impl.r[][key] = val

proc inc*[A: Keyable](t: CountTableRef[A]; key: A; val = 1) =
  t.impl.r[].inc(key, val)

func smallest*[A](t: CountTableRef[A]): tuple[key: A, val: int] =
  t.impl.r[].smallest()

func largest*[A](t: CountTableRef[A]): tuple[key: A, val: int] =
  t.impl.r[].largest()

func hasKey*[A: Keyable](t: CountTableRef[A]; key: A): bool {.inline.} =
  t.impl.r[].hasKey(key)

func contains*[A: Keyable](t: CountTableRef[A]; key: A): bool {.inline.} =
  t.impl.r[].contains(key)

func getOrDefault*[A: Keyable](t: CountTableRef[A]; key: A; def = 0): int =
  t.impl.r[].getOrDefault(key, def)

func len*[A](t: CountTableRef[A]): int {.inline.} =
  t.impl.r[].entries.counter

proc del*[A: Keyable](t: CountTableRef[A]; key: A) =
  t.impl.r[].del(key)

proc pop*[A: Keyable](t: CountTableRef[A]; key: A; val: var int): bool =
  t.impl.r[].pop(key, val)

proc clear*[A](t: CountTableRef[A]) =
  t.impl.r[].clear()

proc sort*[A](t: CountTableRef[A]; order = SortOrder.Descending) =
  t.impl.r[].sort(order)

proc merge*[A: Keyable](s, t: CountTableRef[A]) =
  s.impl.r[].merge(t.impl.r[])

iterator pairs*[A](t: CountTableRef[A]): (lent A, int) =
  for h in 0 ..< t.impl.r[].entries.data.len:
    if t.impl.r[].entries.data[h].val != 0:
      yield (t.impl.r[].entries.data[h].key,
             t.impl.r[].entries.data[h].val)

iterator mpairs*[A](t: CountTableRef[A]): (lent A, var int) =
  for h in 0 ..< t.impl.r[].entries.data.len:
    if t.impl.r[].entries.data[h].val != 0:
      yield (t.impl.r[].entries.data[h].key,
             t.impl.r[].entries.data[h].val)

iterator keys*[A](t: CountTableRef[A]): lent A =
  for h in 0 ..< t.impl.r[].entries.data.len:
    if t.impl.r[].entries.data[h].val != 0:
      yield t.impl.r[].entries.data[h].key

iterator values*[A](t: CountTableRef[A]): int =
  for h in 0 ..< t.impl.r[].entries.data.len:
    if t.impl.r[].entries.data[h].val != 0:
      yield t.impl.r[].entries.data[h].val

iterator mvalues*[A](t: CountTableRef[A]): var int =
  for h in 0 ..< t.impl.r[].entries.data.len:
    if t.impl.r[].entries.data[h].val != 0:
      yield t.impl.r[].entries.data[h].val

func `$`*[A: Stringable](t: CountTableRef[A]): string =
  $t[]

func `==`*[A: Keyable](a, b: CountTableRefImpl[A]): bool =
  ## Exported for the structural equality instantiated in importing modules.
  a.r[] == b.r[]
