{.feature: "staticContracts".}


import tables, hashes

const
  UIntSize = when defined(cpu16): 16'u
             elif defined(cpu32): 32'u
             else: 64'u

const
  TrunkSize = 8'u
  BitsPerTrunk = TrunkSize.uint * UIntSize

type
  Trunk = object
    a: array[TrunkSize, uint]

  IntSet* = object
    t: Table[uint, Trunk]

func initIntSet*(): IntSet = IntSet(t: initTable[uint, Trunk]())

template split(x: int; a, b, c: untyped) =
  # Declares the three parts in the caller, where the bounds of `b` and `c`
  # are visible; a tuple returned from a routine would carry none of them.
  let u = cast[uint](x)
  let a {.inject.} = u div BitsPerTrunk
  let b {.inject.} = (u mod BitsPerTrunk) div UIntSize
  let c {.inject.} = int(u mod UIntSize)

func incl*(s: var IntSet; x: int) =
  split(x, a, b, c)
  let tr = addr(s.t.mgetOrPut(a, default(Trunk)))
  tr.a[b] = tr.a[b] or (1'u shl c)

func excl*(s: var IntSet; x: int) =
  split(x, a, b, c)
  let tr = addr(s.t.mgetOrPut(a, default(Trunk)))
  tr.a[b] = tr.a[b] and not (1'u shl c)

func contains*(s: IntSet; x: int): bool =
  split(x, a, b, c)
  if s.t.hasKey(a):
    #let tr = s.t.getOrDefault(a)
    let tr = addr getOrQuit(s.t, a)
    result = (tr.a[b] and (1'u shl c)) != 0'u
  else:
    result = false

func containsOrIncl*(s: var IntSet; x: int): bool =
  split(x, a, b, c)
  let tr = addr(s.t.mgetOrPut(a, default(Trunk)))
  result = (tr.a[b] and (1'u shl c)) != 0'u
  if not result:
    tr.a[b] = tr.a[b] or (1'u shl c)

func isEmpty(tr: Trunk): bool {.inline.} =
  ## `excl` leaves emptied blocks in the table, so "no block" and "a block of
  ## zeros" must mean the same set everywhere below.
  result = true
  for w in tr.a:
    if w != 0'u: return false

func countBits(x: uint): int {.inline.} =
  var v = x
  result = 0
  while v != 0'u:
    v = v and (v - 1'u) # clears the lowest set bit
    inc result

func len*(s: IntSet): int =
  ## The number of elements in `s`.
  result = 0
  for _, tr in s.t.pairs:
    for w in tr.a: inc result, countBits(w)

func `==`*(a, b: IntSet): bool =
  ## Same elements, however the two sets were built.
  var blocks = 0
  for k, tr in a.t.pairs:
    if not isEmpty(tr):
      if not b.t.hasKey(k): return false
      let other = addr getOrQuit(b.t, k)
      for i in 0'u..<TrunkSize:
        if tr.a[i] != other.a[i]: return false
      inc blocks
  for _, tr in b.t.pairs:
    if not isEmpty(tr): dec blocks
  result = blocks == 0

func hash*(s: IntSet): Hash =
  ## Combines the blocks commutatively: the table keeps them in insertion
  ## order, and two equal sets need not have inserted them alike.
  var h = 0'u
  for k, tr in s.t.pairs:
    if not isEmpty(tr):
      var th = hash(k)
      for w in tr.a: th = th !& hash(w)
      h = h + !$th
  result = h

iterator items*(s: IntSet): int =
  for a, tr in s.t.pairs:
    for b in 0'u..<TrunkSize:
      let word = tr.a[b]
      if word != 0'u:
        for c in 0'u..<UIntSize:
          if (word and (1'u shl c)) != 0'u:
            yield int(a * BitsPerTrunk + b * UIntSize + c)

when isMainModule:
  import std/assertions

  var s = initIntSet()
  for i in 5000..<6000:
    s.incl i
    assert s.contains i
  for i in 500..<600:
    s.incl i
    assert s.contains i

  for i in 50000..<60000:
    s.incl i
    assert s.contains i

  for i in 0..<500:
    assert not s.contains i

  s.excl 50100
  assert not s.contains 50100

  assert not containsOrIncl(s, 7)
  assert containsOrIncl(s, 7)
