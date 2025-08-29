#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## A BiTable is a table that can be seen as an optimized pair
## of `(Table[Id, Val], Table[Val, Id])`.

import std/hashes

when defined(nimPreviewSlimSystem):
  import std/assertions

type
  BiTable*[Id, T] = object # Id must be an int/uint or a distinct type thereof
                           # that is convertible to `uint32`. `Id(0)` must mean "not used".
    vals: seq[T] # indexed by LitId
    keys: seq[Id]  # indexed by hash(val)

proc initBiTable*[Id, T](): BiTable[Id, T] = BiTable[Id, T](vals: @[], keys: @[])

proc nextTry(h, maxHash: Hash): Hash {.inline.} =
  result = (h + 1) and maxHash

template maxHash(t): untyped = high(t.keys)
template isFilled(x: untyped): bool = x.uint32 > 0'u32

proc len*[Id, T](t: BiTable[Id, T]): int = t.vals.len

proc mustRehash(length, counter: int): bool {.inline.} =
  assert(length > counter)
  result = (length < counter div 2 + counter) or (length - counter < 4)

const
  idStart = 1

template idToIdx(x: untyped): int = x.int - idStart

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
      var j = hash(t.vals[idToIdx eh]) and maxHash(t)
      while isFilled(t.keys[j]):
        j = nextTry(j, maxHash(t))
      t.keys[j] = move n[i]

proc getKeyId*[Id, T](t: BiTable[Id, T]; v: T): Id =
  let origH = hash(v)
  var h = origH and maxHash(t)
  if t.keys.len != 0:
    while true:
      let litId = t.keys[h]
      if not isFilled(litId): break
      if v == t.vals[idToIdx t.keys[h]]: return litId
      h = nextTry(h, maxHash(t))
  return Id(0)

template getOrInclImpl() {.dirty.} =
  let origH = hash(v)
  var h = origH and maxHash(t)
  if t.keys.len != 0:
    while true:
      let litId = t.keys[h]
      if not isFilled(litId): break
      if v == t.vals[idToIdx t.keys[h]]: return litId
      h = nextTry(h, maxHash(t))
    # not found, we need to insert it:
    if mustRehash(t.keys.len, t.vals.len):
      enlarge(t)
      # recompute where to insert:
      h = origH and maxHash(t)
      while true:
        let litId = t.keys[h]
        if not isFilled(litId): break
        h = nextTry(h, maxHash(t))
  else:
    setLen(t.keys, 16)
    h = origH and maxHash(t)

proc getOrIncl*[Id, T](t: var BiTable[Id, T]; v: T): Id =
  getOrInclImpl()
  result = Id(t.vals.len + idStart)
  t.keys[h] = result
  t.vals.add v

proc getOrInclFromView*[Id, T, View](t: var BiTable[Id, T]; v: View): Id =
  ## Optimized version that only materializes from the view `v` if the value does
  ## not exist yet.
  getOrInclImpl()
  result = Id(t.vals.len + idStart)
  t.keys[h] = result
  t.vals.add $v

proc `[]`*[Id, T](t: var BiTable[Id, T]; litId: Id): var T {.inline.} =
  let idx = idToIdx litId
  assert idx < t.vals.len
  result = t.vals[idx]

proc `[]`*[Id, T](t: BiTable[Id, T]; litId: Id): lent T {.inline.} =
  let idx = idToIdx litId
  assert idx < t.vals.len
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

proc `[]`*[Id](t: BiTableFloat[Id]; litId: Id): float64 {.inline.} =
  cast[float64](BiTable[Id, uint64](t)[litId])

when isMainModule:

  var t: BiTable[uint32, string]

  echo getOrIncl(t, "hello")

  echo getOrIncl(t, "hello")
  echo getOrIncl(t, "hello3")
  echo getOrIncl(t, "hello4")
  echo getOrIncl(t, "helloasfasdfdsa")
  echo getOrIncl(t, "hello")
  echo getKeyId(t, "hello")
  echo getKeyId(t, "none")

  for i in 0 ..< 100_000:
    discard t.getOrIncl($i & "___" & $i)

  for i in 0 ..< 100_000:
    assert t.getOrIncl($i & "___" & $i).idToIdx == i + 4
  echo "begin"
  echo t.vals.len

  echo t.vals[0]
  echo t.vals[1004]

  echo "middle"

  var tf: BiTable[uint32, float]

  discard tf.getOrIncl(0.4)
  discard tf.getOrIncl(16.4)
  discard tf.getOrIncl(32.4)
  echo getKeyId(tf, 32.4)

  echo "end"
