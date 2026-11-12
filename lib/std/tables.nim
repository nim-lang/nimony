{.feature: "staticContracts".}

import algorithm, hashes, assertions

type
  Keyable* = concept of Hashable ## Types usable as table keys: `Hashable` plus `==`.
    func `==`(a, b: Self): bool

  HashEntry = object
    fullhash: Hash
    position: int # index into `data`; 1 based so that 0 means "unfilled"
  Table*[K, V] = object ## Generic hash map storing key/value pairs (linear scan when tiny, open addressing otherwise).
    data: seq[(K, V)]
    hashes: seq[HashEntry]

func mustRehash(length, counter: int): bool {.inline.} =
  result = (length < counter div 2 + counter) or (length - counter < 4)

func isIndexed(a: HashEntry): bool {.inline.} = a.position > 0

func emptySlot(s: seq[HashEntry]; h: Hash): int {.requires: s.len > 0,
    ensures: 0 <= result and result < s.len, inline.} =
  ## The first unfilled slot on `h`'s linear probe sequence.
  var i = h
  while isIndexed(s[i and high(s).uint]): inc i
  result = int(i and high(s).uint)

func resize(t: var seq[HashEntry]) =
  # does not have to be generic.
  let newLen = if t.len == 0: 4 else: t.len * 2
  var s = newSeq[HashEntry](newLen)
  # the allocation failed: keep probing the old index, full as it is
  if s.len == 0: return
  for i in 0 ..< t.len:
    if isIndexed(t[i]):
      s[emptySlot(s, t[i].fullhash)] = t[i]
  t = ensureMove s

const
  HashThreshold = 4
  defaultInitialSize* = 32
  growthFactor = 2

func fillHashPart[K: Keyable, V](t: var Table[K, V]) =
  ## Builds a COMPLETE index for `data`, or leaves `hashes` empty.
  ##
  ## Sized for what `data` already holds rather than a fixed minimum: this runs
  ## again after an earlier attempt could not allocate, by which time `data` may
  ## be far past `HashThreshold`, and an index too small for it would leave
  ## `emptySlot` with no free slot to find.
  var cap = HashThreshold*2
  while mustRehash(cap, t.data.len): cap = cap * 2
  t.hashes = newSeq[HashEntry](cap)
  if t.hashes.len == 0: return
  for i in 0 ..< t.data.len:
    let fullhash = hash(t.data[i][0])
    t.hashes[emptySlot(t.hashes, fullhash)] = HashEntry(fullhash: fullhash, position: i+1)

func rawGet[K: Keyable, V](t: Table[K, V]; k: K; kh: out Hash): int {.
    ensures: result < t.data.len.} =
  if t.data.len <= HashThreshold or t.hashes.len == 0:
    kh = 0
    # No index: either the table is still tiny, or the index could not be
    # allocated. A linear scan is slow but correct, and correctness is what
    # matters here -- answering "absent" would make `[]=` store a second copy
    # of a key that is already present.
    for i in 0 ..< t.data.len:
      if t.data[i][0] == k: return i
  else:
    kh = hash(k)
    var h = kh
    while isIndexed(t.hashes[h and high(t.hashes).uint]):
      let d = t.hashes[h and high(t.hashes).uint]
      # `position` points into `data` for every filled entry; an entry that
      # does not is no match rather than a read past its end
      if d.fullhash == kh and d.position >= 1 and d.position <= t.data.len and
          t.data[d.position-1][0] == k:
        return d.position-1
      inc h
  result = -1

func contains*[K: Keyable, V](t: Table[K, V]; k: K): bool {.inline.} =
  ## True if `k` is stored in `t`.
  var hc: Hash
  rawGet(t, k, hc) >= 0

func hasKey*[K: Keyable, V](t: Table[K, V]; k: K): bool {.inline.} =
  ## Alias for `contains`.
  contains(t, k)

func hasKeyOrPut*[K: Keyable, V](t: var Table[K, V]; key: K; val: V): bool =
  ## True when `key` is present; otherwise inserts it with `val` and returns
  ## false.
  var hc: Hash
  let index = rawGet(t, key, hc)
  if index >= 0:
    return true
  rawPut(t, key, val, hc)
  false

func getOrDefault*[K: Keyable, V: HasDefault](t: Table[K, V]; k: K): V =
  var hc: Hash
  let idx = rawGet(t, k, hc)
  if idx >= 0:
    t.data[idx][1]
  else:
    default(V)

func getOrDefault*[K: Keyable, V](t: Table[K, V]; k: K; fallback: V): V =
  ## Return `t[k]` if the key is present, otherwise `fallback`.
  var hc: Hash
  let idx = rawGet(t, k, hc)
  if idx >= 0:
    t.data[idx][1]
  else:
    fallback

func getOrQuit*[K: Keyable, V](t: Table[K, V]; k: K): var V =
  ## Like `[]`, but terminates the program if `k` is missing (after `assert`).
  var hc: Hash
  let idx = rawGet(t, k, hc)
  if idx < 0:
    {.cast(noSideEffect).}:
      raiseAssert "key not found"
  t.data[idx][1]

when defined(nimony):
  func `[]`*[K: Keyable, V](t: Table[K, V]; k: K): var V {.raises.} =
    ## Retrieves `k`'s value or raises `KeyError` when absent.
    var hc: Hash
    let idx = rawGet(t, k, hc)
    if idx < 0:
      raise KeyError
    t.data[idx][1]

else:
  func `[]`*[K, V](t: var Table[K, V]; k: K): var V {.raises.} =
    var hc: Hash
    let idx = rawGet(t, k, hc)
    if idx < 0:
      raise KeyError
    t.data[idx][1]

  func `[]`*[K, V](t: Table[K, V]; k: K): V {.raises.} =
    var hc: Hash
    let idx = rawGet(t, k, hc)
    if idx < 0:
      raise KeyError
    t.data[idx][1]

func rawPut[K: Keyable, V](t: var Table[K, V]; k: sink K; v: sink V; h: Hash) =
  if t.hashes.len == 0:
    # `fillHashPart` is the only thing that builds an index from `data`;
    # `resize` merely grows an existing one, so using it here would leave an
    # index that is non-empty and yet missing every key already stored, and
    # `rawGet` would trust it.
    if t.data.len >= HashThreshold:
      fillHashPart t
  elif mustRehash(t.hashes.len, t.data.len):
    resize t.hashes
    if mustRehash(t.hashes.len, t.data.len):
      # The index could not grow. Storing the pair anyway would eventually fill
      # every slot, and both probe loops in this module assume a free slot
      # exists -- a lookup for an absent key would then never terminate. Refuse
      # the insert instead: it is an allocation failure like any other, and the
      # table stays consistent and usable.
      return
  var kh = h
  if t.hashes.len > 0 and kh == 0:
    kh = hash(k)
  let oldLen = t.data.len
  t.data.add (k, v)
  # Two ways this can have no new entry to index: an index that could not be
  # allocated (`hashes` empty), or an `add` that could not allocate -- which
  # leaves `data` untouched, so `data.len` is the test. Indexing anyway would
  # write a slot whose `position` names an OLD pair: `rawGet` compares keys and
  # so never matches it, but it stays filled forever, and enough of them make
  # `emptySlot` spin on a probe sequence with no free slot left.
  if t.data.len > oldLen and t.hashes.len > 0:
    t.hashes[emptySlot(t.hashes, kh)] = HashEntry(fullhash: kh, position: t.data.len)

func `[]=`*[K: Keyable, V](t: var Table[K, V]; k: sink K; v: sink V) =
  ## Inserts or updates `k` with `v`.
  var hc: Hash
  let idx = rawGet(t, k, hc)
  if idx >= 0:
    t.data[idx][1] = v
  else:
    rawPut(t, k, v, hc)

func mgetOrPut*[K: Keyable, V](t: var Table[K, V]; k: sink K; v: sink V): var V =
  ## Returns `t[k]`, inserting `v` when `k` was missing; result is mutable.
  var hc: Hash
  var idx = rawGet(t, k, hc)
  if idx < 0:
    let oldLen = t.data.len
    rawPut(t, k, v, hc)
    idx = t.data.len-1
    # When the insert was refused, `data.len-1` names the pair BEFORE the one
    # asked for, so a reference to someone else's value would escape. Both tests
    # matter: `idx < 0` for a table that is still empty, and `len <= oldLen` for
    # a refusal that left a non-empty one. Keeping `idx < 0` explicit is also
    # what tells the contract prover that the index below is in range.
    if idx < 0 or t.data.len <= oldLen:
      {.cast(noSideEffect).}:
        raiseAssert "out of memory"
  result = t.data[idx][1]

func mgetOrPut*[K: Keyable, V: HasDefault](t: var Table[K, V];
    k: sink K): var V =
  mgetOrPut(t, k, default(V))

func len*[K, V](t: Table[K, V]): int {.inline.} =
  ## Number of key/value pairs stored in `t`.
  t.data.len

iterator pairs*[K, V](t: Table[K, V]): (lent K, lent V) =
  ## Yields every `(key, value)` stored in `t`.
  for i in 0 ..< t.data.len:
    yield (t.data[i][0], t.data[i][1])

iterator mpairs*[K, V](t: Table[K, V]): (lent K, var V) =
  ## Mutable variant of `pairs` (values can be updated in place).
  for i in 0 ..< t.data.len:
    yield (t.data[i][0], t.data[i][1])

iterator keys*[K, V](t: Table[K, V]): lent K =
  for i in 0 ..< t.data.len:
    yield t.data[i][0]

iterator values*[K, V](t: Table[K, V]): lent V =
  for i in 0 ..< t.data.len:
    yield t.data[i][1]

iterator mvalues*[K, V](t: var Table[K, V]): var V =
  for i in 0 ..< t.data.len:
    yield t.data[i][1]

func del*[K: Keyable, V](t: var Table[K, V]; k: K) =
  ## Remove `k` from `t`. No-op if absent. Preserves insertion order by
  ## shifting later entries down; the hash index is rebuilt afterwards
  ## because the shift renamed every entry's data position.
  var hc: Hash
  let idx = rawGet(t, k, hc)
  if idx < 0: return
  var j = idx
  while j < t.data.len - 1:
    let next = t.data[j+1]
    t.data[j] = next
    inc j
  t.data.shrink(t.data.len - 1)
  if t.data.len > HashThreshold:
    # `rawGet` does NOT rebuild lazily: when `data.len > HashThreshold`
    # it indexes into `t.hashes` directly, so leaving `hashes = @[]`
    # turns the next lookup into an out-of-bounds read. Rebuild the
    # hash table sized to fit the current data.
    var size = HashThreshold * 2
    while size < t.data.len * 2:
      size = size * 2
    t.hashes = newSeq[HashEntry](size)
    if t.hashes.len == 0: return
    for i in 0 ..< t.data.len:
      let fullhash = hash(t.data[i][0])
      t.hashes[emptySlot(t.hashes, fullhash)] = HashEntry(fullhash: fullhash, position: i+1)
  else:
    # data fits the linear-scan threshold; drop the hash table entirely.
    t.hashes.shrink(0)

func pop*[K: Keyable, V](t: var Table[K, V]; key: K; val: var V): bool =
  ## Removes `key` and stores its value in `val`.
  var hc: Hash
  let index = rawGet(t, key, hc)
  if index < 0:
    return false
  val = move(t.data[index][1])
  del(t, key)
  true

func take*[K: Keyable, V](t: var Table[K, V]; key: K; val: var V): bool {.inline.} =
  pop(t, key, val)

func initTable*[K, V](initialSize = defaultInitialSize): Table[K, V] =
  ## Creates an empty table.
  discard initialSize
  Table[K, V](data: @[], hashes: @[])

func toTable*[K: Keyable, V](pairs: openArray[(K, V)]): Table[K, V] =
  ## Creates a table containing `pairs`.
  result = initTable[K, V](pairs.len)
  for (key, val) in pairs:
    result[key] = val

func clear*[K, V](t: var Table[K, V]) =
  ## Remove all entries from `t`.
  t.data.shrink(0)
  t.hashes = @[]

func `$`*[K: Stringable, V: Stringable](t: Table[K, V]): string =
  if t.data.len == 0:
    return "{:}"
  result = "{"
  var first = true
  for i in 0 ..< t.data.len:
    if not first:
      result.add ", "
    first = false
    result.addQuoted t.data[i][0]
    result.add ": "
    result.addQuoted t.data[i][1]
  result.add "}"

func `==`*[K: Keyable, V: Equatable](s, t: Table[K, V]): bool =
  ## Content equality: insertion order and internal index layout do not matter.
  if s.data.len != t.data.len:
    return false
  for i in 0 ..< s.data.len:
    var hc: Hash
    let idx = rawGet(t, s.data[i][0], hc)
    if idx < 0 or t.data[idx][1] != s.data[i][1]:
      return false
  true

# --------------------------- OrderedTable --------------------------------

func isFilled(hcode: Hash): bool {.inline.} =
  hcode != 0

func nextPowerOfTwo(x: int): int {.inline.} =
  result = 1
  while result < x:
    result = result shl 1

func slotsNeeded(count: int): int {.inline.} =
  ## Capacity for `count` entries, keeping at least four free slots. This is
  ## the same sizing rule as `mustRehash`.
  let wanted = if count < 0: 4 else: count div 2 + count + 4
  nextPowerOfTwo(wanted)

func initialProbe(hc: Hash; mask: int): int {.inline,
    requires: mask >= 0, ensures: 0 <= result and result <= mask.} =
  ## Home slot for `hc`; `mask` is one less than a power-of-two table size.
  result = int(hc and uint(mask))
  {.assume: 0 <= result and result <= mask.}

func nextProbe(h, mask: int): int {.inline, requires: mask >= 0,
    ensures: 0 <= result and result <= mask.} =
  result = (h + 1) and mask

func genHash[K: Hashable](key: K): Hash {.inline.} =
  result = hash(key)
  # A zero hash marks an empty slot, so real keys never carry one.
  if result == 0:
    result = 314159265'u

type
  OrderedKeyValuePair[T] = tuple[hcode: Hash, next: int, item: T]
  OrderedKeyValuePairSeq[T] = seq[OrderedKeyValuePair[T]]
  OrderedTable*[K, V] = object ## Hash table that remembers insertion order.
    data: OrderedKeyValuePairSeq[(K, V)]
    counter, first, last: int

func ordRawGetKnownHC[K: Keyable, V](t: OrderedTable[K, V]; key: K;
                                    hc: Hash): int =
  result = -1
  if t.data.len > 0:
    let mask = t.data.len - 1
    var h = initialProbe(hc, mask)
    var searching = true
    while searching and 0 <= h and h < t.data.len and
        isFilled(t.data[h].hcode):
      if t.data[h].hcode == hc and t.data[h].item[0] == key:
        result = h
        searching = false
      else:
        h = nextProbe(h, mask)
    if searching:
      result = -1 - h

func ordRawGet[K: Keyable, V](t: OrderedTable[K, V]; key: K; hc: out Hash): int =
  hc = genHash(key)
  result = ordRawGetKnownHC(t, key, hc)

func ordRawInsert[K, V](t: var OrderedTable[K, V]; key: sink K; val: sink V;
                     hc: Hash; h: int) {.
    requires: 0 <= h and h < t.data.len.} =
  t.data[h] = (hcode: hc, next: -1, item: (key, val))
  if t.first < 0:
    t.first = h
  if t.last >= 0:
    {.assume: 0 <= t.last and
              t.last < t.data.len.}
    t.data[t.last].next = h
  t.last = h

func ordInitImpl[K, V](t: var OrderedTable[K, V]; initialSize: int): bool =
  let correctSize = slotsNeeded(initialSize)
  var s = newSeq[OrderedKeyValuePair[(K, V)]](correctSize)
  if s.len < correctSize:
    return false
  t.data = s
  t.counter = 0
  t.first = -1
  t.last = -1
  true

func ordCheckIfInitialized[K, V](t: var OrderedTable[K, V]): bool =
  if t.data.len > 0:
    return true
  ordInitImpl(t, defaultInitialSize)

func ordEnlarge[K: Keyable, V](t: var OrderedTable[K, V]): bool =
  let oldLen = t.data.len
  if oldLen == 0:
    return false
  var n = newSeq[OrderedKeyValuePair[(K, V)]](oldLen * growthFactor)
  if n.len < oldLen * growthFactor:
    return false
  var h = t.first
  t.first = -1
  t.last = -1
  swap(t.data, n)
  let mask = t.data.len - 1
  while 0 <= h and h < n.len:
    let nxt = n[h].next
    let eh = n[h].hcode
    if isFilled(eh):
      var j = -1 - ordRawGetKnownHC(t, n[h].item[0], eh)
      {.assume: 0 <= j and j < t.data.len.}
      var kv = move(n[h].item)
      ordRawInsert(t, move(kv[0]), move(kv[1]), eh, j)
    h = nxt
  true

func ordPutWithHash[K: Keyable, V](t: var OrderedTable[K, V]; key: sink K;
                                val: sink V; hc: Hash; index: int): int =
  var idx = index
  {.assume: t.data.len > t.counter.}
  if mustRehash(t.data.len, t.counter):
    if not ordEnlarge(t):
      return -1
    idx = ordRawGetKnownHC(t, key, hc)
  idx = -1 - idx
  {.assume: 0 <= idx and idx < t.data.len.}
  ordRawInsert(t, key, val, hc, idx)
  inc t.counter
  result = idx

func initOrderedTable*[K, V](
    initialSize = defaultInitialSize): OrderedTable[K, V] =
  ## Creates a new empty ordered table.
  result = OrderedTable[K, V](
      data: default(OrderedKeyValuePairSeq[(K, V)]),
      counter: 0, first: -1, last: -1)
  discard ordInitImpl(result, initialSize)

func toOrderedTable*[K: Keyable, V](
    pairs: openArray[(K, V)]): OrderedTable[K, V] =
  ## Creates an ordered table containing `pairs`. A repeated key keeps its
  ## first position and its last value.
  result = initOrderedTable[K, V](pairs.len)
  for (key, val) in pairs:
    result[key] = val

func `[]=`*[K: Keyable, V](t: var OrderedTable[K, V]; key: sink K;
                          val: sink V) =
  if not ordCheckIfInitialized(t):
    return
  var hc: Hash
  let index = ordRawGet(t, key, hc)
  if index >= 0:
    {.assume: 0 <= index and index < t.data.len.}
    t.data[index].item[1] = val
  else:
    discard ordPutWithHash(t, key, val, hc, index)

when defined(nimony):
  func `[]`*[K: Keyable, V](t: OrderedTable[K, V]; key: K): var V {.raises.} =
    ## Retrieves `key`'s value or raises `KeyError` when absent.
    var hc: Hash
    let index = ordRawGet(t, key, hc)
    if index < 0:
      raise KeyError
    {.assume: 0 <= index and index < t.data.len.}
    result = t.data[index].item[1]
else:
  func `[]`*[K, V](t: var OrderedTable[K, V]; key: K): var V {.raises.} =
    var hc: Hash
    let index = ordRawGet(t, key, hc)
    if index < 0:
      raise newException(KeyError, "key not found")
    {.assume: 0 <= index and index < t.data.len.}
    result = t.data[index].item[1]

  func `[]`*[K, V](t: OrderedTable[K, V]; key: K): V {.raises.} =
    var hc: Hash
    let index = ordRawGet(t, key, hc)
    if index < 0:
      raise newException(KeyError, "key not found")
    {.assume: 0 <= index and index < t.data.len.}
    result = t.data[index].item[1]

func hasKey*[K: Keyable, V](t: OrderedTable[K, V]; key: K): bool =
  var hc: Hash
  ordRawGet(t, key, hc) >= 0

func contains*[K: Keyable, V](t: OrderedTable[K, V]; key: K): bool {.inline.} =
  hasKey(t, key)

func hasKeyOrPut*[K: Keyable, V](t: var OrderedTable[K, V]; key: K;
                                val: V): bool =
  if not ordCheckIfInitialized(t):
    return false
  var hc: Hash
  let index = ordRawGet(t, key, hc)
  if index >= 0:
    return true
  discard ordPutWithHash(t, key, val, hc, index)
  false

func getOrDefault*[K: Keyable, V: HasDefault](
    t: OrderedTable[K, V]; key: K): V =
  var hc: Hash
  let index = ordRawGet(t, key, hc)
  if index >= 0:
    {.assume: 0 <= index and index < t.data.len.}
    t.data[index].item[1]
  else:
    default(V)

func getOrDefault*[K: Keyable, V](t: OrderedTable[K, V]; key: K;
                                  fallback: V): V =
  var hc: Hash
  let index = ordRawGet(t, key, hc)
  if index >= 0:
    {.assume: 0 <= index and index < t.data.len.}
    t.data[index].item[1]
  else:
    fallback

func getOrQuit*[K: Keyable, V](t: OrderedTable[K, V]; key: K): var V =
  var hc: Hash
  let index = ordRawGet(t, key, hc)
  if index < 0:
    {.cast(noSideEffect).}:
      raiseAssert "key not found"
  {.assume: 0 <= index and index < t.data.len.}
  result = t.data[index].item[1]

func mgetOrPut*[K: Keyable, V](t: var OrderedTable[K, V]; key: sink K;
                              val: sink V): var V =
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
  {.assume: 0 <= index and index < t.data.len.}
  result = t.data[index].item[1]

func mgetOrPut*[K: Keyable, V: HasDefault](t: var OrderedTable[K, V];
    key: sink K): var V =
  mgetOrPut(t, key, default(V))

func len*[K, V](t: OrderedTable[K, V]): int {.inline.} =
  t.counter

func ordDelImpl[K: Keyable, V](t: var OrderedTable[K, V]; key: K;
    n: var OrderedKeyValuePairSeq[(K, V)]): bool =
  ## Rebuilds `t` into the caller-provided `n`, dropping `key`. `n` must
  ## already be large enough; no allocation happens here.
  if t.counter == 0:
    return true
  let hc = genHash(key)
  if ordRawGetKnownHC(t, key, hc) < 0:
    return true
  let oldLen = t.data.len
  if n.len < oldLen:
    return false
  var h = t.first
  t.first = -1
  t.last = -1
  swap(t.data, n)
  while 0 <= h and h < n.len:
    let nxt = n[h].next
    let eh = n[h].hcode
    if isFilled(eh):
      if eh == hc and n[h].item[0] == key:
        dec t.counter
      else:
        let j = -1 - ordRawGetKnownHC(t, n[h].item[0], eh)
        {.assume: 0 <= j and j < t.data.len.}
        var kv = move(n[h].item)
        ordRawInsert(t, move(kv[0]), move(kv[1]), eh, j)
    h = nxt
  true

func del*[K: Keyable, V](t: var OrderedTable[K, V]; key: K) =
  ## Removes `key`; does nothing when it is absent.
  if t.counter == 0:
    return
  let oldLen = t.data.len
  var n = newSeq[OrderedKeyValuePair[(K, V)]](oldLen)
  if n.len < oldLen:
    return
  discard ordDelImpl(t, key, n)

func pop*[K: Keyable, V](t: var OrderedTable[K, V]; key: K;
                         val: var V): bool =
  var hc: Hash
  let index = ordRawGet(t, key, hc)
  if index < 0:
    return false
  let oldLen = t.data.len
  var n = newSeq[OrderedKeyValuePair[(K, V)]](oldLen)
  if n.len < oldLen:
    return false
  {.assume: 0 <= index and index < t.data.len.}
  val = move(t.data[index].item[1])
  discard ordDelImpl(t, key, n)
  true

func take*[K: Keyable, V](t: var OrderedTable[K, V]; key: K;
                          val: var V): bool {.inline.} =
  pop(t, key, val)

func clear*[K, V](t: var OrderedTable[K, V]) =
  for i in 0 ..< t.data.len:
    t.data[i] = default(OrderedKeyValuePair[(K, V)])
  t.counter = 0
  t.first = -1
  t.last = -1

proc sort*[K: Keyable, V](t: var OrderedTable[K, V];
    cmp: proc (x, y: (K, V)): int; order = SortOrder.Ascending) =
  ## Sorts the internal insertion-order list. Lookups still work afterwards.
  var list = t.first
  var p, q, e, tail: int
  var nmerges, psize, qsize, i: int
  if t.counter == 0:
    return
  let dataLen = t.data.len
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
        q = t.data[q].next
        if q < 0:
          break
        inc i
      qsize = insize
      while psize > 0 or (qsize > 0 and q >= 0):
        if psize == 0:
          {.assume: 0 <= q and q < dataLen.}
          e = q
          q = t.data[q].next
          dec qsize
        elif qsize == 0 or q < 0:
          {.assume: 0 <= p and p < dataLen.}
          e = p
          p = t.data[p].next
          dec psize
        else:
          {.assume: 0 <= p and p < dataLen.}
          {.assume: 0 <= q and q < dataLen.}
          if cmp((t.data[p].item[0], t.data[p].item[1]),
                 (t.data[q].item[0], t.data[q].item[1])) *
                 order <= 0:
            e = p
            p = t.data[p].next
            dec psize
          else:
            e = q
            q = t.data[q].next
            dec qsize
        if tail >= 0:
          {.assume: 0 <= tail and tail < dataLen.}
          t.data[tail].next = e
        else:
          list = e
        tail = e
      p = q
    {.assume: 0 <= tail and tail < dataLen.}
    t.data[tail].next = -1
    if nmerges <= 1:
      break
    insize = insize * 2
  t.first = list
  t.last = tail

iterator pairs*[K, V](t: OrderedTable[K, V]): (lent K, lent V) =
  var h = t.first
  while 0 <= h and h < t.data.len:
    let nxt = t.data[h].next
    if isFilled(t.data[h].hcode):
      yield (t.data[h].item[0], t.data[h].item[1])
    h = nxt

iterator mpairs*[K, V](t: var OrderedTable[K, V]): (lent K, var V) =
  var h = t.first
  while 0 <= h and h < t.data.len:
    let nxt = t.data[h].next
    if isFilled(t.data[h].hcode):
      yield (t.data[h].item[0], t.data[h].item[1])
    h = nxt

iterator keys*[K, V](t: OrderedTable[K, V]): lent K =
  var h = t.first
  while 0 <= h and h < t.data.len:
    let nxt = t.data[h].next
    if isFilled(t.data[h].hcode):
      yield t.data[h].item[0]
    h = nxt

iterator values*[K, V](t: OrderedTable[K, V]): lent V =
  var h = t.first
  while 0 <= h and h < t.data.len:
    let nxt = t.data[h].next
    if isFilled(t.data[h].hcode):
      yield t.data[h].item[1]
    h = nxt

iterator mvalues*[K, V](t: var OrderedTable[K, V]): var V =
  var h = t.first
  while 0 <= h and h < t.data.len:
    let nxt = t.data[h].next
    if isFilled(t.data[h].hcode):
      yield t.data[h].item[1]
    h = nxt

func `$`*[K: Stringable, V: Stringable](t: OrderedTable[K, V]): string =
  if t.counter == 0:
    return "{:}"
  result = "{"
  var h = t.first
  var first = true
  while 0 <= h and h < t.data.len:
    let nxt = t.data[h].next
    if isFilled(t.data[h].hcode):
      if not first:
        result.add ", "
      first = false
      result.addQuoted t.data[h].item[0]
      result.add ": "
      result.addQuoted t.data[h].item[1]
    h = nxt
  result.add "}"

func `==`*[K: Keyable, V: Equatable](s, t: OrderedTable[K, V]): bool =
  ## Ordered equality: both the contents and the order must match.
  if s.counter != t.counter:
    return false
  if s.counter == 0:
    return true
  var hs = s.first
  var ht = t.first
  while 0 <= hs and hs < s.data.len and 0 <= ht and ht < t.data.len:
    if s.data[hs].item != t.data[ht].item:
      return false
    hs = s.data[hs].next
    ht = t.data[ht].next
  result = hs < 0 and ht < 0

# ----------------------------- CountTable --------------------------------

type
  CountEntry[K] = tuple[key: K, val: int]
  CountEntrySeq[K] = seq[CountEntry[K]]
  CountTable*[K] = object ## Hash table that counts the occurrences of keys.
    data: CountEntrySeq[K]
    counter: int
    isSorted: bool

func ctRawGet[K: Keyable](t: CountTable[K]; key: K): int =
  result = -1
  if t.data.len > 0:
    let mask = t.data.len - 1
    var h = initialProbe(genHash(key), mask)
    var searching = true
    while searching and 0 <= h and h < t.data.len and
        t.data[h].val != 0:
      if t.data[h].key == key:
        result = h
        searching = false
      else:
        h = nextProbe(h, mask)
    if searching:
      result = -1 - h

func ctRawInsert[K](data: var CountEntrySeq[K]; key: sink K; val: int;
                    h: int) {.requires: 0 <= h and h < data.len.} =
  data[h] = (key: key, val: val)

func ctInitImpl[K](t: var CountTable[K]; initialSize: int): bool =
  let correctSize = slotsNeeded(initialSize)
  var s = newSeq[CountEntry[K]](correctSize)
  if s.len < correctSize:
    return false
  t.data = s
  t.counter = 0
  t.isSorted = false
  true

func ctCheckIfInitialized[K](t: var CountTable[K]): bool =
  if t.data.len > 0:
    return true
  ctInitImpl(t, defaultInitialSize)

func ctEnlarge[K: Keyable](t: var CountTable[K]): bool =
  let oldLen = t.data.len
  if oldLen == 0:
    return false
  var n = newSeq[CountEntry[K]](oldLen * growthFactor)
  if n.len < oldLen * growthFactor:
    return false
  swap(t.data, n)
  let dataLen = t.data.len
  if dataLen == 0:
    return false
  let mask = dataLen - 1
  for i in 0 ..< n.len:
    if n[i].val != 0:
      var j = initialProbe(genHash(n[i].key), mask)
      while 0 <= j and j < t.data.len and
          t.data[j].val != 0:
        j = nextProbe(j, mask)
      let v = n[i].val
      {.assume: 0 <= j and j < t.data.len.}
      ctRawInsert(t.data, move(n[i].key), v, j)
  true

func putCount[K: Keyable](t: var CountTable[K]; key: K; val: int;
                          index: int): bool =
  ## Inserts a missing key with a positive count.
  var idx = index
  {.assume: t.data.len > t.counter.}
  if mustRehash(t.data.len, t.counter):
    if not ctEnlarge(t):
      return false
    idx = ctRawGet(t, key)
  idx = -1 - idx
  {.assume: 0 <= idx and idx < t.data.len.}
  ctRawInsert(t.data, key, val, idx)
  inc t.counter
  true

func countDelIdx[K: Keyable](t: var CountTable[K]; i: int) =
  if i < 0 or i >= t.data.len:
    return
  let dataLen = t.data.len
  if dataLen == 0:
    return
  let mask = dataLen - 1
  var i = i
  dec t.counter
  block outer:
    while true:
      var j = i
      {.assume: 0 <= i and i < t.data.len.}
      t.data[i] = default(CountEntry[K])
      while true:
        i = nextProbe(i, mask)
        {.assume: 0 <= i and i < t.data.len.}
        if t.data[i].val == 0:
          break outer
        let r = initialProbe(genHash(t.data[i].key), mask)
        if not ((i >= r and r > j) or (r > j and j > i) or (j > i and i >= r)):
          break
      {.assume: 0 <= j and j < t.data.len.}
      t.data[j] = move(t.data[i])

func initCountTable*[K](initialSize = defaultInitialSize): CountTable[K] =
  ## Creates a new empty count table.
  result = CountTable[K](
      data: default(CountEntrySeq[K]), counter: 0, isSorted: false)
  discard ctInitImpl(result, initialSize)

func toCountTable*[K: Keyable](keys: openArray[K]): CountTable[K] =
  ## Counts how often each element of `keys` occurs.
  result = initCountTable[K](keys.len)
  for key in keys:
    result.inc(key)

func `[]`*[K: Keyable](t: CountTable[K]; key: K): int =
  ## Returns the count of `key`, or zero when absent.
  assert(not t.isSorted, "CountTable must not be used after sorting")
  let index = ctRawGet(t, key)
  if index >= 0:
    {.assume: 0 <= index and index < t.data.len.}
    t.data[index].val
  else:
    0

func `[]=`*[K: Keyable](t: var CountTable[K]; key: K; val: int) =
  assert(not t.isSorted, "CountTable must not be used after sorting")
  assert(val >= 0, "CountTable values must be non-negative")
  if val == 0:
    t.del(key)
    return
  if not ctCheckIfInitialized(t):
    return
  let index = ctRawGet(t, key)
  if index >= 0:
    {.assume: 0 <= index and index < t.data.len.}
    t.data[index].val = val
  else:
    discard putCount(t, key, val, index)

func inc*[K: Keyable](t: var CountTable[K]; key: K; val = 1) =
  ## Increments `t[key]` by `val`.
  assert(not t.isSorted, "CountTable must not be used after sorting")
  if not ctCheckIfInitialized(t):
    return
  let index = ctRawGet(t, key)
  if index >= 0:
    {.assume: 0 <= index and index < t.data.len.}
    let next = t.data[index].val + val
    if next == 0:
      countDelIdx(t, index)
    else:
      t.data[index].val = next
  elif val != 0:
    discard putCount(t, key, val, index)

func len*[K](t: CountTable[K]): int {.inline.} =
  t.counter

func smallest*[K](t: CountTable[K]): CountEntry[K] =
  assert(t.len > 0, "counttable is empty")
  var minIdx = -1
  for h in 0 ..< t.data.len:
    if t.data[h].val > 0:
      if minIdx < 0:
        minIdx = h
      else:
        {.assume: 0 <= minIdx and minIdx < t.data.len.}
        if t.data[minIdx].val > t.data[h].val:
          minIdx = h
  {.assume: 0 <= minIdx and minIdx < t.data.len.}
  result = (t.data[minIdx].key, t.data[minIdx].val)

func largest*[K](t: CountTable[K]): CountEntry[K] =
  assert(t.len > 0, "counttable is empty")
  {.assume: t.data.len > 0.}
  var maxIdx = 0
  {.assume: maxIdx < t.data.len.}
  for h in 1 ..< t.data.len:
    {.assume: 0 <= maxIdx and maxIdx < t.data.len.}
    if t.data[maxIdx].val < t.data[h].val:
      maxIdx = h
  {.assume: 0 <= maxIdx and maxIdx < t.data.len.}
  result = (t.data[maxIdx].key, t.data[maxIdx].val)

func hasKey*[K: Keyable](t: CountTable[K]; key: K): bool =
  assert(not t.isSorted, "CountTable must not be used after sorting")
  ctRawGet(t, key) >= 0

func contains*[K: Keyable](t: CountTable[K]; key: K): bool {.inline.} =
  hasKey(t, key)

func getOrDefault*[K: Keyable](t: CountTable[K]; key: K; def = 0): int =
  let index = ctRawGet(t, key)
  if index >= 0:
    {.assume: 0 <= index and index < t.data.len.}
    t.data[index].val
  else:
    def

func del*[K: Keyable](t: var CountTable[K]; key: K) =
  ## Removes `key`; does nothing when it is absent.
  if t.data.len == 0:
    return
  let mask = t.data.len - 1
  var h = initialProbe(genHash(key), mask)
  {.assume: 0 <= h and h < t.data.len.}
  while 0 <= h and h < t.data.len and t.data[h].val != 0:
    if t.data[h].key == key:
      countDelIdx(t, h)
      return
    h = nextProbe(h, mask)
    {.assume: 0 <= h and h < t.data.len.}

func pop*[K: Keyable](t: var CountTable[K]; key: K; val: var int): bool =
  let index = ctRawGet(t, key)
  if index < 0:
    return false
  {.assume: 0 <= index and index < t.data.len.}
  val = t.data[index].val
  countDelIdx(t, index)
  true

func clear*[K](t: var CountTable[K]) =
  for i in 0 ..< t.data.len:
    t.data[i] = default(CountEntry[K])
  t.counter = 0
  t.isSorted = false

func ctCmp[K](a, b: CountEntry[K]): int =
  if a.val < b.val: -1
  elif a.val > b.val: 1
  else: 0

proc sort*[K](t: var CountTable[K]; order = SortOrder.Descending) =
  ## Sorts by count. Once sorted, the table must not be modified.
  algorithm.sort(t.data, proc (x, y: CountEntry[K]): int = ctCmp(x, y), order)
  t.isSorted = true

proc merge*[K: Keyable](s: var CountTable[K]; t: CountTable[K]) =
  assert(not s.isSorted, "CountTable must not be used after sorting")
  for h in 0 ..< t.data.len:
    if t.data[h].val != 0:
      s.inc(t.data[h].key, t.data[h].val)

iterator pairs*[K](t: CountTable[K]): (lent K, int) =
  for h in 0 ..< t.data.len:
    if t.data[h].val != 0:
      yield (t.data[h].key, t.data[h].val)

iterator mpairs*[K](t: var CountTable[K]): (lent K, var int) =
  for h in 0 ..< t.data.len:
    if t.data[h].val != 0:
      yield (t.data[h].key, t.data[h].val)

iterator keys*[K](t: CountTable[K]): lent K =
  for h in 0 ..< t.data.len:
    if t.data[h].val != 0:
      yield t.data[h].key

iterator values*[K](t: CountTable[K]): int =
  for h in 0 ..< t.data.len:
    if t.data[h].val != 0:
      yield t.data[h].val

iterator mvalues*[K](t: var CountTable[K]): var int =
  for h in 0 ..< t.data.len:
    if t.data[h].val != 0:
      yield t.data[h].val

func `$`*[K: Stringable](t: CountTable[K]): string =
  if t.counter == 0:
    return "{:}"
  result = "{"
  var first = true
  for h in 0 ..< t.data.len:
    if t.data[h].val != 0:
      if not first:
        result.add ", "
      first = false
      result.addQuoted t.data[h].key
      result.add ": "
      result.addQuoted t.data[h].val
  result.add "}"

func `==`*[K: Keyable](s, t: CountTable[K]): bool =
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
      var i = initialProbe(hc, mask)
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

when isMainModule:
  import std/syncio

  var tab: Table[string, int] = initTable[string, int]()
  for i in 0..<1000:
    tab[$i] = i
    assert hasKey(tab, $i)
    assert not hasKey(tab, $(i+1))
    assert tab[$i] == i
  #echo tab.data
  #echo tab.hashes
  assert tab.hasKey("100")
  echo tab.getOrDefault("500")
  echo tab["600"]
  tab.mgetOrPut("abc", -12) = -24
  echo tab["abc"]
