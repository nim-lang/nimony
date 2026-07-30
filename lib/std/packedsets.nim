#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The `packedsets` module implements an efficient set of `Ordinal` values as a
## `sparse bit set`:idx:. Values that cluster -- symbol ids, file ids, node
## positions -- cost about one bit each, while values far apart cost one small
## block each.
##
## Unlike `HashSet` no `hash` is required of the element type: the element's
## *ordinal value* is the bit index. That is exactly what `Ordinal` gives us --
## `int(x)` to find the bit and `A(i)` to hand the element back out of `items`
## -- so `Ordinal` is the constraint, and it is enough to typecheck every body
## here up front. Routines needing more say so: `$` also asks for `Stringable`.
##
## See also
## ========
## * `sets module <sets.html>`_ for general hash sets
## * `intsets module <intsets.html>`_ for the non-generic `int` case

import tables

const
  UIntSize = when defined(cpu16): 16'u
             elif defined(cpu32): 32'u
             else: 64'u
  TrunkSize = 8'u
  BitsPerTrunk = TrunkSize * UIntSize
    ## Values falling into the same block of `BitsPerTrunk` share one `Trunk`.

type
  Trunk = object
    a: array[TrunkSize, uint]

  PackedSet*[A: Ordinal] = object ## A sparse bit set over an ordinal type.
    t: Table[uint, Trunk]

func split(x: uint): (uint, uint, int) {.inline.} =
  ## `x` split into (trunk key, word within the trunk, bit within the word).
  (x div BitsPerTrunk, (x mod BitsPerTrunk) div UIntSize, int(x mod UIntSize))

func bitPos[A: Ordinal](key: A): uint {.inline.} =
  ## The element's bit position. `cast` rather than a conversion so that a
  ## negative ordinal (`PackedSet[int]`) wraps into the upper half of the
  ## address space instead of producing a negative index.
  cast[uint](int(key))

func countBits(x: uint): int {.inline.} =
  var v = x
  result = 0
  while v != 0'u:
    v = v and (v - 1'u) # clears the lowest set bit
    inc result

func initPackedSet*[A: Ordinal](): PackedSet[A] =
  ## Returns an empty `PackedSet[A]`.
  runnableExamples:
    let a = initPackedSet[int]()
    assert len(a) == 0

  PackedSet[A](t: initTable[uint, Trunk]())

func contains*[A: Ordinal](s: PackedSet[A]; key: A): bool =
  ## True if `key` is in `s`. This allows the usage of the `in` operator.
  runnableExamples:
    let a = toPackedSet([1, 3, 5])
    assert 3 in a
    assert 8 notin a

  let (a, b, c) = split(bitPos(key))
  if s.t.hasKey(a):
    let tr = addr getOrQuit(s.t, a)
    result = (tr.a[b] and (1'u shl c)) != 0'u
  else:
    result = false

func incl*[A: Ordinal](s: var PackedSet[A]; key: A) =
  ## Includes `key` in `s`. Does nothing if it is already in there.
  runnableExamples:
    var a = initPackedSet[int]()
    a.incl(3)
    a.incl(3)
    assert len(a) == 1

  let (a, b, c) = split(bitPos(key))
  let tr = addr(s.t.mgetOrPut(a, default(Trunk)))
  tr.a[b] = tr.a[b] or (1'u shl c)

func excl*[A: Ordinal](s: var PackedSet[A]; key: A) =
  ## Excludes `key` from `s`. Does nothing if it is not in there.
  runnableExamples:
    var a = toPackedSet([3])
    a.excl(3)
    a.excl(99)
    assert len(a) == 0

  let (a, b, c) = split(bitPos(key))
  # No `mgetOrPut`: excluding from a trunk that does not exist must not
  # allocate one.
  if s.t.hasKey(a):
    let tr = addr getOrQuit(s.t, a)
    tr.a[b] = tr.a[b] and not (1'u shl c)

func containsOrIncl*[A: Ordinal](s: var PackedSet[A]; key: A): bool =
  ## Includes `key` in `s` and tells whether it was already in there.
  runnableExamples:
    var a = initPackedSet[int]()
    assert a.containsOrIncl(3) == false
    assert a.containsOrIncl(3) == true

  let (a, b, c) = split(bitPos(key))
  let tr = addr(s.t.mgetOrPut(a, default(Trunk)))
  result = (tr.a[b] and (1'u shl c)) != 0'u
  if not result:
    tr.a[b] = tr.a[b] or (1'u shl c)

func missingOrExcl*[A: Ordinal](s: var PackedSet[A]; key: A): bool =
  ## Excludes `key` from `s` and tells whether it was already missing.
  runnableExamples:
    var a = toPackedSet([5])
    assert a.missingOrExcl(5) == false
    assert a.missingOrExcl(5) == true

  let (a, b, c) = split(bitPos(key))
  if s.t.hasKey(a):
    let tr = addr getOrQuit(s.t, a)
    result = (tr.a[b] and (1'u shl c)) == 0'u
    if not result:
      tr.a[b] = tr.a[b] and not (1'u shl c)
  else:
    result = true

iterator items*[A: Ordinal](s: PackedSet[A]): A =
  ## Iterates over every element of `s`. Trunks come out in insertion order
  ## (nimony's `Table` is insertion-ordered) and ascending within a trunk, so
  ## the traversal is reproducible across runs.
  for k, tr in s.t.pairs:
    for b in 0'u ..< TrunkSize:
      let word = tr.a[b]
      if word != 0'u:
        for c in 0'u ..< UIntSize:
          if (word and (1'u shl c)) != 0'u:
            yield A(cast[int](k * BitsPerTrunk + b * UIntSize + c))

func len*[A: Ordinal](s: PackedSet[A]): int =
  ## The number of elements in `s`.
  runnableExamples:
    let a = toPackedSet([1, 3, 5])
    assert len(a) == 3

  result = 0
  for tr in s.t.values:
    for b in 0'u ..< TrunkSize:
      result = result + countBits(tr.a[b])

func card*[A: Ordinal](s: PackedSet[A]): int {.inline.} =
  ## Alias for `len`: the [cardinality](https://en.wikipedia.org/wiki/Cardinality)
  ## of the set.
  len(s)

func clear*[A: Ordinal](s: var PackedSet[A]) =
  ## Resets `s` back to the empty set.
  runnableExamples:
    var a = toPackedSet([5, 7])
    clear(a)
    assert len(a) == 0

  s.t.clear()

func toPackedSet*[A: Ordinal](x: openArray[A]): PackedSet[A] =
  ## A new `PackedSet[A]` holding the elements of `x`; duplicates are removed.
  runnableExamples:
    let a = toPackedSet([5, 6, 7, 8, 8])
    assert len(a) == 4

  result = initPackedSet[A]()
  for item in items(x):
    result.incl item

func incl*[A: Ordinal](s: var PackedSet[A]; other: PackedSet[A]) =
  ## Includes every element of `other` in `s` -- the in-place `s + other`.
  runnableExamples:
    var a = toPackedSet([1])
    a.incl(toPackedSet([5]))
    assert len(a) == 2

  for item in items(other): incl(s, item)

func excl*[A: Ordinal](s: var PackedSet[A]; other: PackedSet[A]) =
  ## Excludes every element of `other` from `s` -- the in-place `s - other`.
  runnableExamples:
    var a = toPackedSet([1, 5])
    a.excl(toPackedSet([5]))
    assert len(a) == 1

  for item in items(other): excl(s, item)

func union*[A: Ordinal](s1, s2: PackedSet[A]): PackedSet[A] =
  ## The union of `s1` and `s2`.
  result = s1
  incl(result, s2)

func intersection*[A: Ordinal](s1, s2: PackedSet[A]): PackedSet[A] =
  ## The intersection of `s1` and `s2`.
  result = initPackedSet[A]()
  for item in items(s1):
    if contains(s2, item):
      incl(result, item)

func difference*[A: Ordinal](s1, s2: PackedSet[A]): PackedSet[A] =
  ## The difference of `s1` and `s2`: everything in `s1` that is not in `s2`.
  result = initPackedSet[A]()
  for item in items(s1):
    if not contains(s2, item):
      incl(result, item)

func symmetricDifference*[A: Ordinal](s1, s2: PackedSet[A]): PackedSet[A] =
  ## Everything that is in exactly one of `s1` and `s2`.
  result = s1
  for item in items(s2):
    if containsOrIncl(result, item):
      excl(result, item)

func `+`*[A: Ordinal](s1, s2: PackedSet[A]): PackedSet[A] {.inline.} =
  ## Alias for `union`.
  union(s1, s2)

func `*`*[A: Ordinal](s1, s2: PackedSet[A]): PackedSet[A] {.inline.} =
  ## Alias for `intersection`.
  intersection(s1, s2)

func `-`*[A: Ordinal](s1, s2: PackedSet[A]): PackedSet[A] {.inline.} =
  ## Alias for `difference`.
  difference(s1, s2)

func disjoint*[A: Ordinal](s1, s2: PackedSet[A]): bool =
  ## True if `s1` and `s2` have no element in common.
  runnableExamples:
    assert disjoint(toPackedSet([1, 2]), toPackedSet([3, 4]))
    assert not disjoint(toPackedSet([1, 2]), toPackedSet([2, 3]))

  for item in items(s1):
    if contains(s2, item):
      return false
  return true

func `<=`*[A: Ordinal](s1, s2: PackedSet[A]): bool =
  ## True if `s1` is a subset of `s2` (`s1` may equal `s2`).
  for item in items(s1):
    if not contains(s2, item):
      return false
  return true

func `<`*[A: Ordinal](s1, s2: PackedSet[A]): bool =
  ## True if `s1` is a *proper* subset of `s2`.
  s1 <= s2 and not (s2 <= s1)

func `==`*[A: Ordinal](s1, s2: PackedSet[A]): bool =
  ## True if both sets hold the same elements.
  runnableExamples:
    assert toPackedSet([1, 2]) == toPackedSet([2, 1, 2])

  s1 <= s2 and s2 <= s1

func `$`*[A: Ordinal and Stringable](s: PackedSet[A]): string =
  ## Renders `s` as `{a, b, c}`. Needs `$` of the element type on top of the
  ## `Ordinal`-ness the container itself requires.
  runnableExamples:
    assert $toPackedSet([1, 2, 3]) == "{1, 2, 3}"

  result = "{"
  for key in items(s):
    if result.len > 1: result.add ", "
    result.add $key
  result.add "}"
