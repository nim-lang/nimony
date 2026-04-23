
import hashes, assertions

type
  Keyable* = concept
    func `==`(a, b: Self): bool
    func hash(a: Self): Hash

  HashEntry = object
    fullhash: Hash
    position: int # index into `data`; 1 based so that 0 means "unfilled"
  Table*[K, V] = object
    data: seq[(K, V)]
    hashes: seq[HashEntry]

func mustRehash(length, counter: int): bool {.inline.} =
  result = (length < counter div 2 + counter) or (length - counter < 4)

func isFilled(a: HashEntry): bool {.inline.} = a.position > 0

func resize(t: var seq[HashEntry]) =
  # does not have to be generic.
  let newLen = if t.len == 0: 4 else: t.len * 2
  var s = newSeq[HashEntry](newLen)
  for i in 0 ..< t.len:
    if isFilled(t[i]):
      var h = t[i].fullhash and high(s).uint
      while isFilled(s[h]): h = nextTry(h, high(s))
      s[h] = t[i]
  t = ensureMove s

const
  HashThreshold = 4

func fillHashPart[K: Keyable, V](t: var Table[K, V]) =
  t.hashes = newSeq[HashEntry](HashThreshold*2)
  for i in 0 ..< t.data.len:
    let fullhash = hash(t.data[i][0])
    var hi = fullhash and t.hashes.high.uint
    while isFilled(t.hashes[hi]): hi = nextTry(hi, high(t.hashes))
    t.hashes[hi] = HashEntry(fullhash: fullhash, position: i+1)

func rawGet[K: Keyable, V](t: Table[K, V]; k: K; kh: Hash): int =
  if t.data.len <= HashThreshold:
    for i in 0 ..< t.data.len:
      if t.data[i][0] == k: return i
  else:
    var h = kh and t.hashes.high.uint
    while isFilled(t.hashes[h]):
      let d = t.hashes[h]
      if d.fullhash == kh and t.data[d.position-1][0] == k:
        return d.position-1
      h = nextTry(h, high(t.hashes))
  result = -1

func contains*[K: Keyable, V](t: Table[K, V]; k: K): bool {.inline.} =
  rawGet(t, k, hash(k)) >= 0

func hasKey*[K: Keyable, V](t: Table[K, V]; k: K): bool {.inline.} =
  contains(t, k)

func getOrDefault*[K: Keyable, V: HasDefault](t: Table[K, V]; k: K): V =
  let idx = rawGet(t, k, hash(k))
  if idx >= 0:
    t.data[idx][1]
  else:
    default(V)

func getOrQuit*[K: Keyable, V](t: Table[K, V]; k: K): var V =
  ## Quits if the key is not found.
  let idx = rawGet(t, k, hash(k))
  {.cast(noSideEffect).}:
    assert idx >= 0
  t.data[idx][1]

when defined(nimony):
  func `[]`*[K: Keyable, V](t: Table[K, V]; k: K): var V {.raises.} =
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
  if t.data.len == HashThreshold:
    fillHashPart t
  elif mustRehash(t.hashes.len, t.data.len):
    resize t.hashes
  t.data.add (k, v)
  var hi = h and t.hashes.high.uint
  while isFilled(t.hashes[hi]): hi = nextTry(hi, high(t.hashes))
  t.hashes[hi] = HashEntry(fullhash: h, position: t.data.len)

func `[]=`*[K: Keyable, V](t: var Table[K, V]; k: sink K; v: sink V) =
  let h = hash(k)
  let idx = rawGet(t, k, h)
  if idx >= 0:
    t.data[idx][1] = v
  else:
    rawPut(t, k, v, h)

func mgetOrPut*[K: Keyable, V](t: var Table[K, V]; k: sink K; v: sink V): var V =
  let h = hash(k)
  var idx = rawGet(t, k, h)
  if idx < 0:
    rawPut(t, k, v, h)
    idx = t.data.len-1
  result = t.data[idx][1]

func len*[K, V](t: Table[K, V]): int {.inline.} = t.data.len

iterator pairs*[K, V](t: Table[K, V]): (lent K, lent V) =
  for i in 0 ..< t.data.len:
    yield (t.data[i][0], t.data[i][1])

iterator mpairs*[K, V](t: Table[K, V]): (lent K, var V) =
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
  ## shifting later entries down; the hash index is rebuilt lazily on next
  ## access.
  let idx = rawGet(t, k, hash(k))
  if idx < 0: return
  var j = idx
  while j < t.data.len - 1:
    let next = t.data[j+1]
    t.data[j] = next
    inc j
  t.data.shrink(t.data.len - 1)
  # Invalidate the hash index so subsequent lookups rebuild it.
  t.hashes = @[]

func initTable*[K, V](): Table[K, V] =
  Table[K, V](data: @[], hashes: @[])

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
