#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## A BiTable is a table that can be seen as an optimized pair
## of `(Table[Id, Val], Table[Val, Id])`.

when defined(nimony):
  {.feature: "untyped".}
  import std/[hashes, assertions]
else:
  import std/hashes

  when defined(nimPreviewSlimSystem):
    import std/assertions

type
  LazyStrings* = ref object
    ## The backing of a `BiTable[Id, string]` whose values still sit in a
    ## mapped file: per entry a byte offset and length into `base`. An entry
    ## is copied into `vals` the first time it is READ; hashing and
    ## comparison work on the mapped bytes directly, so a table can be
    ## indexed and looked up by value without materialising anything.
    ##
    ## Why: `bif.load` fills four pools per file, and a Nim IC backend process
    ## opens ~800 files of which it reads names from a fraction — 222MB of
    ## strings, 41% of the process's heap, for names it never looked at
    ## (`--mm:refc -d:nimTypeNames` on nimbus-eth2). `base` is borrowed from
    ## the mapping, which has to outlive the table; `bif.load` leaves its
    ## mapping in place for the buffer's lifetime for the same reason.
    base: ptr UncheckedArray[char]
    offs: seq[uint32]
    lens: seq[uint32]

  BiTable*[Id, T] = object # Id must be an int/uint or a distinct type thereof
                           # that is convertible to `uint32`. `Id(0)` must mean "not used".
    vals: seq[T] # indexed by LitId
    keys: seq[Id]  # indexed by hash(val)
    lazy: LazyStrings # nil unless `T is string` and `addLazy` was used

proc initBiTable*[Id, T](): BiTable[Id, T] = BiTable[Id, T](vals: @[], keys: @[])

const
  idStart = 1

template idToIdx(x: untyped): int = x.int - idStart

template isLazyAt(t, idx: untyped): bool =
  ## Entry `idx` is still only in the mapped file.
  t.lazy != nil and t.vals[idx].len == 0 and t.lazy.lens[idx] > 0'u32

proc materialize[Id, T](t: var BiTable[Id, T]; idx: int) {.inline.} =
  when T is string:
    if isLazyAt(t, idx):
      let n = int t.lazy.lens[idx]
      var s = newString(n)
      copyMem(addr s[0], addr t.lazy.base[t.lazy.offs[idx]], n)
      t.vals[idx] = s

proc hashAt[Id, T](t: BiTable[Id, T]; idx: int): Hash {.inline.} =
  ## `hash(t.vals[idx])`, off the mapped bytes when the entry is still there.
  ## `hash(openArray[char])` and `hash(string)` run the same algorithm.
  when T is string:
    if isLazyAt(t, idx):
      let off = int t.lazy.offs[idx]
      return hash(toOpenArray(t.lazy.base, off, off + int(t.lazy.lens[idx]) - 1))
  result = hash(t.vals[idx])

proc eqAt[Id, T, V](t: BiTable[Id, T]; idx: int; v: V): bool {.inline.} =
  ## `v == t.vals[idx]`, off the mapped bytes when the entry is still there.
  ## `V` is `T` or a view of it (`getOrInclFromView`); only a `string` probe
  ## gets the no-copy comparison, a view against a lazy entry compares
  ## against a copy — that path does not run on file-loaded pools.
  when T is string:
    if isLazyAt(t, idx):
      let n = int t.lazy.lens[idx]
      when V is string:
        return v.len == n and (n == 0 or
          equalMem(unsafeAddr v[0], addr t.lazy.base[t.lazy.offs[idx]], n))
      else:
        var tmp = newString(n)
        if n > 0: copyMem(addr tmp[0], addr t.lazy.base[t.lazy.offs[idx]], n)
        return v == tmp
  result = v == t.vals[idx]

proc addLazy*[Id](t: var BiTable[Id, string]; base: pointer; off, len: int): Id =
  ## `addOrdered` for a value that stays in the mapped file at `base + off`
  ## until it is first read. Only for a table filled in id order from a file
  ## (see `bif.load`); mixing with `getOrIncl` afterwards is fine.
  if t.lazy == nil: t.lazy = LazyStrings(base: cast[ptr UncheckedArray[char]](base))
  assert cast[pointer](t.lazy.base) == base, "addLazy: one mapping per table"
  t.lazy.offs.add uint32(off)
  t.lazy.lens.add uint32(len)
  t.vals.add ""
  result = Id(t.vals.len - 1 + idStart)

proc nextTry(h, maxHash: Hash): Hash {.inline.} =
  result = (h + 1) and maxHash

template maxHash(t): untyped = high(t.keys).Hash
template isFilled(x: untyped): bool = x.uint32 > 0'u32

proc len*[Id, T](t: BiTable[Id, T]): int = t.vals.len

proc mustRehash(length, counter: int): bool {.inline.} =
  assert(length > counter)
  result = (length < counter div 2 + counter) or (length - counter < 4)


proc hasId*[Id, T](t: BiTable[Id, T]; x: Id): bool {.inline.} =
  let idx = idToIdx(x)
  result = idx >= 0 and idx < t.vals.len

proc enlarge[Id, T](t: var BiTable[Id, T]) =
  var n: seq[Id]
  newSeq(n, len(t.keys) * 2)
  swap(t.keys, n)
  for i in 0..high(n):
    let eh = n[i]
    if isFilled(eh):
      var j = hashAt(t, idToIdx eh) and maxHash(t)
      while isFilled(t.keys[j]):
        j = nextTry(j, maxHash(t))
      t.keys[j] = move n[i]

proc rebuildIndex[Id, T](t: var BiTable[Id, T]) =
  ## Build `keys` from `vals` for a table filled by `addOrdered`, which leaves
  ## none. Sized as `enlarge` would have left it after that many inserts, so the
  ## load factor matches a table that was built by interning all along.
  var cap = 16
  # `mustRehash` asserts `length > counter`, so the capacity has to clear
  # `vals.len` before the load-factor question can even be asked.
  while cap <= t.vals.len: cap = cap * 2
  while mustRehash(cap, t.vals.len): cap = cap * 2
  t.keys = newSeq[Id](cap)
  for i in 0 ..< t.vals.len:
    var j = hashAt(t, i) and maxHash(t)
    while isFilled(t.keys[j]):
      j = nextTry(j, maxHash(t))
    t.keys[j] = Id(i + idStart)

proc isIndexed*[Id, T](t: BiTable[Id, T]): bool {.inline.} =
  ## Whether the reverse (value -> id) index exists. It does not, and only does
  ## not, between an `addOrdered` fill and the first thing that needs it.
  t.keys.len != 0 or t.vals.len == 0

proc ensureIndexed*[Id, T](t: var BiTable[Id, T]) =
  ## Build the reverse index if `addOrdered` left it unbuilt. Idempotent, and
  ## free for a table that was interned into normally.
  ##
  ## `getOrIncl` calls this itself. `getKeyId` cannot — it takes the table
  ## immutably and changing that would break every caller in the ecosystem for
  ## the sake of the rare one — so a caller that looks up BY VALUE in a table
  ## that might have been filled with `addOrdered` calls this first. Getting it
  ## wrong is an assertion in `getKeyId`, not a wrong answer.
  if not t.isIndexed: rebuildIndex(t)

proc addOrdered*[Id, T](t: var BiTable[Id, T]; v: sink T): Id =
  ## Append `v` under the next id WITHOUT hashing it.
  ##
  ## For a table deserialized in id order, every entry's id is its position, so
  ## the reverse index costs a hash and an insert per entry to reproduce
  ## something the file already states. `bif.load` fills four pools that way,
  ## and on a 68-module `--ic:on` build of the Nim compiler that was 451ms of a
  ## 637ms `load`, nearly all of it the symbol pool.
  ##
  ## The table is still a BiTable: `getOrIncl` builds the index on demand (and
  ## `ensureIndexed` does it explicitly for `getKeyId`), so a pool that does get
  ## interned into behaves exactly as before — it just pays for the index at the
  ## point something needs it rather than always. Callers that only ever map
  ## id -> value never pay at all.
  t.vals.add v
  if t.lazy != nil:
    t.lazy.offs.add 0'u32
    t.lazy.lens.add 0'u32
  result = Id(t.vals.len - 1 + idStart)

proc getKeyId*[Id, T](t: BiTable[Id, T]; v: T): Id =
  assert t.isIndexed,
    "getKeyId on a table filled by addOrdered: call ensureIndexed first"
  let origH = hash(v)
  var h = origH and maxHash(t)
  if t.keys.len != 0:
    while true:
      let strId = t.keys[h]
      if not isFilled(strId): break
      if eqAt(t, idToIdx strId, v): return strId
      h = nextTry(h, maxHash(t))
  return Id(0)

{.pragma: maybeDirty, dirty.}

template getOrInclImpl() {.maybeDirty.} =
  ensureIndexed(t)
  let origH = hash(v)
  var h = origH and maxHash(t)
  if t.keys.len != 0:
    while true:
      let strId = t.keys[h]
      if not isFilled(strId): break
      if eqAt(t, idToIdx strId, v): return strId
      h = nextTry(h, maxHash(t))
    # not found, we need to insert it:
    if mustRehash(t.keys.len, t.vals.len):
      enlarge(t)
      # recompute where to insert:
      h = origH and maxHash(t)
      while true:
        let strId = t.keys[h]
        if not isFilled(strId): break
        h = nextTry(h, maxHash(t))
  else:
    setLen(t.keys, 16)
    h = origH and maxHash(t)

proc getOrIncl*[Id, T](t: var BiTable[Id, T]; v: T): Id =
  getOrInclImpl()
  result = Id(t.vals.len + idStart)
  t.keys[h] = result
  t.vals.add v
  if t.lazy != nil:
    t.lazy.offs.add 0'u32
    t.lazy.lens.add 0'u32

proc getOrInclFromView*[Id, T, View](t: var BiTable[Id, T]; v: View): Id =
  ## Optimized version that only materializes from the view `v` if the value does
  ## not exist yet.
  getOrInclImpl()
  result = Id(t.vals.len + idStart)
  t.keys[h] = result
  t.vals.add $v
  if t.lazy != nil:
    t.lazy.offs.add 0'u32
    t.lazy.lens.add 0'u32

when defined(nimony):
  proc `[]`*[Id, T](t: BiTable[Id, T]; strId: Id): var T {.inline.} =
    let idx = idToIdx strId
    assert idx < t.vals.len
    result = t.vals[idx]
else:
  proc `[]`*[Id, T](t: var BiTable[Id, T]; strId: Id): var T {.inline.} =
    let idx = idToIdx strId
    assert idx < t.vals.len
    materialize(t, idx)
    result = t.vals[idx]

  proc `[]`*[Id, T](t: BiTable[Id, T]; strId: Id): lent T {.inline.} =
    let idx = idToIdx strId
    assert idx < t.vals.len
    # A read materialises a lazy entry, and a read through a non-`var` table
    # still has to: the entry's storage is the heap seq the table owns, so
    # filling it in place is exactly what the `var` overload does — the only
    # thing `lent` forbids is doing it through this parameter.
    when T is string:
      if isLazyAt(t, idx):
        materialize(cast[ptr BiTable[Id, T]](unsafeAddr t)[], idx)
    result = t.vals[idx]

proc hash*[Id, T](t: BiTable[Id, T]): Hash =
  ## as the keys are hashes of the values, we simply use them instead
  var h: Hash = 0
  for i, n in pairs t.keys:
    h = h !& hash((i, n))
  result = !$h

proc memSize*[Id, T](t: BiTable[Id, T]): int =
  when T is string:
    var shorts = 0
    result = 0
    for x in items(t.vals):
      if x.len <= 3: shorts += 1
      result += x.len
    inc result, t.vals.len * sizeof(T) + t.keys.len * sizeof(Id)
    echo "SHORT STRINGS ", shorts
  else:
    t.vals.len * sizeof(T) + t.keys.len * sizeof(Id)

type
  # we need to distinguish `0.0` and `-0.0` even if `0.0 == -0.0`
  # as signbit and copySign procs returns the different value.
  BiTableFloat*[Id] = distinct BiTable[Id, uint64]

proc getOrIncl*[Id](t: var BiTableFloat[Id]; v: float64): Id {.inline .} =
  BiTable[Id, uint64](t).getOrIncl(cast[uint64](v))

proc `[]`*[Id](t: BiTableFloat[Id]; strId: Id): float64 {.inline.} =
  cast[float64](BiTable[Id, uint64](t)[strId])

when isMainModule:
  when defined(nimony):
    import std / syncio
  var t = initBiTable[uint32, string]()

  assert getOrIncl(t, "hello") == 1

  assert getOrIncl(t, "hello") == 1
  assert getOrIncl(t, "hello3") == 2
  assert getOrIncl(t, "hello4") == 3
  assert getOrIncl(t, "helloasfasdfdsa") == 4
  assert getOrIncl(t, "hello") == 1
  assert getKeyId(t, "hello") == 1
  assert getKeyId(t, "none") == 0

  for i in 0 ..< 100_000:
    discard t.getOrIncl($i & "___" & $i)

  for i in 0 ..< 100_000:
    assert t.getOrIncl($i & "___" & $i).idToIdx == i + 4
  assert t.vals.len == 100004

  assert t.vals[0] == "hello"
  assert t.vals[1004] == "1000___1000"

  var tf = initBiTable[uint32, float]()

  discard tf.getOrIncl(0.4)
  discard tf.getOrIncl(16.4)
  discard tf.getOrIncl(32.4)
  assert getKeyId(tf, 32.4) == 3

  # `addOrdered` must be indistinguishable from having interned all along, and
  # the interesting part is the table it leaves BEHIND: the reverse index is
  # unbuilt, so every lookup path has to notice and build it. A duplicate id
  # handed out here would be silent corruption for a `bif` pool, since the
  # token stream already refers to the first one.
  block orderedFill:
    var a = initBiTable[uint32, string]()
    for i in 0 ..< 5_000:
      assert a.addOrdered("s" & $i).idToIdx == i     # same ids getOrIncl gives
    assert a.vals.len == 5_000
    assert a[uint32 1] == "s0"                       # id -> value, no index yet
    assert a[uint32 5_000] == "s4999"

    # `getKeyId` needs the index, and says so rather than answering wrongly
    assert not a.isIndexed
    a.ensureIndexed()
    assert a.isIndexed
    a.ensureIndexed()                                # idempotent
    assert getKeyId(a, "s0") == 1
    assert getKeyId(a, "s4999") == 5_000
    assert getKeyId(a, "nope") == 0
    for i in 0 ..< 5_000:
      assert getKeyId(a, "s" & $i).idToIdx == i

    # and interning must now REUSE those ids rather than append duplicates
    for i in 0 ..< 5_000:
      assert a.getOrIncl("s" & $i).idToIdx == i
    assert a.vals.len == 5_000
    assert a.getOrIncl("fresh").idToIdx == 5_000
    assert a.vals.len == 5_001

  block orderedThenIncl:
    # `getOrIncl` builds the index itself, so it needs no `ensureIndexed`
    var b = initBiTable[uint32, string]()
    for i in 0 ..< 100: discard b.addOrdered("t" & $i)
    assert not b.isIndexed
    assert b.getOrIncl("t42").idToIdx == 42
    assert b.isIndexed
    assert b.vals.len == 100
    assert getKeyId(b, "t99") == 100

  block indexedFromTheStart:
    # a normally interned table is `isIndexed` throughout, so `ensureIndexed`
    # costs it nothing and `getKeyId` never trips its assertion
    var d = initBiTable[uint32, string]()
    assert d.isIndexed                               # empty counts as indexed
    discard d.getOrIncl("only")
    assert d.isIndexed
    d.ensureIndexed()
    assert getKeyId(d, "only") == 1

  echo "success"
