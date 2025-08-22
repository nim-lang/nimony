
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

proc initIntSet*(): IntSet = IntSet(t: initTable[uint, Trunk]())

proc split(x: uint): (uint, uint, int) {.inline.} =
  (x div BitsPerTrunk, (x mod BitsPerTrunk) div UIntSize, int(x mod UIntSize))

proc incl*(s: var IntSet; x: int) =
  let (a, b, c) = split cast[uint](x)
  let tr = addr(s.t.mgetOrPut(a, default(Trunk)))
  tr.a[b] = tr.a[b] or (1'u shl c)

proc excl*(s: var IntSet; x: int) =
  let (a, b, c) = split cast[uint](x)
  let tr = addr(s.t.mgetOrPut(a, default(Trunk)))
  tr.a[b] = tr.a[b] and not (1'u shl c)

proc contains*(s: IntSet; x: int): bool =
  let (a, b, c) = split cast[uint](x)
  if s.t.hasKey(a):
    #let tr = s.t.getOrDefault(a)
    let tr = addr getOrQuit(s.t, a)
    result = (tr.a[b] and (1'u shl c)) != 0'u
  else:
    result = false

proc containsOrIncl*(s: var IntSet; x: int): bool =
  let (a, b, c) = split cast[uint](x)
  let tr = addr(s.t.mgetOrPut(a, default(Trunk)))
  result = (tr.a[b] and (1'u shl c)) != 0'u
  if not result:
    tr.a[b] = tr.a[b] or (1'u shl c)

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
