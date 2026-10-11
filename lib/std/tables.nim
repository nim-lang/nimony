{.feature: "staticContracts".}

import hashes, assertions

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

func isFilled(a: HashEntry): bool {.inline.} = a.position > 0

func emptySlot(s: seq[HashEntry]; h: Hash): int {.requires: s.len > 0,
    ensures: 0 <= result and result < s.len, inline.} =
  ## The first unfilled slot on `h`'s linear probe sequence.
  var i = h
  while isFilled(s[i and high(s).uint]): inc i
  result = int(i and high(s).uint)

func resize(t: var seq[HashEntry]) =
  # does not have to be generic.
  let newLen = if t.len == 0: 4 else: t.len * 2
  var s = newSeq[HashEntry](newLen)
  # the allocation failed: keep probing the old index, full as it is
  if s.len == 0: return
  for i in 0 ..< t.len:
    if isFilled(t[i]):
      s[emptySlot(s, t[i].fullhash)] = t[i]
  t = ensureMove s

const
  HashThreshold = 4

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

func rawGet[K: Keyable, V](t: Table[K, V]; k: K; kh: Hash): int {.
    ensures: result < t.data.len.} =
  if t.data.len <= HashThreshold or t.hashes.len == 0:
    # No index: either the table is still tiny, or the index could not be
    # allocated. A linear scan is slow but correct, and correctness is what
    # matters here -- answering "absent" would make `[]=` store a second copy
    # of a key that is already present.
    for i in 0 ..< t.data.len:
      if t.data[i][0] == k: return i
  else:
    var h = kh
    while isFilled(t.hashes[h and high(t.hashes).uint]):
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
  rawGet(t, k, hash(k)) >= 0

func hasKey*[K: Keyable, V](t: Table[K, V]; k: K): bool {.inline.} =
  ## Alias for `contains`.
  contains(t, k)

func getOrDefault*[K: Keyable, V: HasDefault](t: Table[K, V]; k: K): V =
  let idx = rawGet(t, k, hash(k))
  if idx >= 0:
    t.data[idx][1]
  else:
    default(V)

func getOrDefault*[K: Keyable, V](t: Table[K, V]; k: K; fallback: V): V =
  ## Return `t[k]` if the key is present, otherwise `fallback`.
  let idx = rawGet(t, k, hash(k))
  if idx >= 0:
    t.data[idx][1]
  else:
    fallback

func getOrQuit*[K: Keyable, V](t: Table[K, V]; k: K): var V =
  ## Like `[]`, but terminates the program if `k` is missing (after `assert`).
  let idx = rawGet(t, k, hash(k))
  if idx < 0:
    {.cast(noSideEffect).}:
      raiseAssert "key not found"
  t.data[idx][1]

when defined(nimony):
  func `[]`*[K: Keyable, V](t: Table[K, V]; k: K): var V {.raises.} =
    ## Retrieves `k`'s value or raises `KeyError` when absent.
    let idx = rawGet(t, k, hash(k))
    if idx < 0:
      raise KeyError
    t.data[idx][1]

else:
  func `[]`*[K, V](t: var Table[K, V]; k: K): var V {.raises.} =
    let idx = rawGet(t, k, hash(k))
    if idx < 0:
      raise KeyError
    t.data[idx][1]

  func `[]`*[K, V](t: Table[K, V]; k: K): V {.raises.} =
    let idx = rawGet(t, k, hash(k))
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
  let oldLen = t.data.len
  t.data.add (k, v)
  # Two ways this can have no new entry to index: an index that could not be
  # allocated (`hashes` empty), or an `add` that could not allocate -- which
  # leaves `data` untouched, so `data.len` is the test. Indexing anyway would
  # write a slot whose `position` names an OLD pair: `rawGet` compares keys and
  # so never matches it, but it stays filled forever, and enough of them make
  # `emptySlot` spin on a probe sequence with no free slot left.
  if t.data.len > oldLen and t.hashes.len > 0:
    t.hashes[emptySlot(t.hashes, h)] = HashEntry(fullhash: h, position: t.data.len)

func `[]=`*[K: Keyable, V](t: var Table[K, V]; k: sink K; v: sink V) =
  ## Inserts or updates `k` with `v`.
  let h = hash(k)
  let idx = rawGet(t, k, h)
  if idx >= 0:
    t.data[idx][1] = v
  else:
    rawPut(t, k, v, h)

func mgetOrPut*[K: Keyable, V](t: var Table[K, V]; k: sink K; v: sink V): var V =
  ## Returns `t[k]`, inserting `v` when `k` was missing; result is mutable.
  let h = hash(k)
  var idx = rawGet(t, k, h)
  if idx < 0:
    let oldLen = t.data.len
    rawPut(t, k, v, h)
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
  let idx = rawGet(t, k, hash(k))
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
    # hash table sized to fit the current data.len. We can't just call
    # `fillHashPart` because it hardcodes size = `HashThreshold*2`,
    # which is too small once data has grown past 8 entries — the
    # probe loop would never terminate.
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

func initTable*[K, V](): Table[K, V] =
  ## Creates an empty table.
  Table[K, V](data: @[], hashes: @[])

func clear*[K, V](t: var Table[K, V]) =
  ## Remove all entries from `t`.
  t.data.shrink(0)
  t.hashes = @[]

# Nimony's `Table` is already insertion-ordered (`data` is a `seq`), so
# `OrderedTable` is a simple alias for portability with code written
# against the Nim stdlib split.
type
  OrderedTable*[K, V] = Table[K, V]

func initOrderedTable*[K, V](): OrderedTable[K, V] {.inline.} = initTable[K, V]()

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
