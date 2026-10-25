# Fill loops that shoggoth's `memidioms` pass turns into `(memset …)` (#2350),
# and the near misses it must leave alone. Every result is printed, so a fill
# that writes one element too many or too few, the wrong byte, or forgets the
# value the loop leaves in its counter shows up as a diff.

import std / syncio

type
  Color = enum red, green, blue, alpha
  Inner = object
    a: int
    p: ptr int
    s: string
  Obj = object
    x, y, z: int
    f: float
    arr: array[8, int32]
    inner: Inner

proc sumArr[I, T](a: array[I, T]): int =
  result = 0
  for x in a: result = result + int(x)

proc defaults() =
  # `default(array)` is a fill loop over the whole array
  var a = default(array[16, int32])
  var c = default(array[Color, int16])
  var o = Obj(x: 0, y: 0, z: 0, f: 0.0)
  a[3] = 5
  c[blue] = 7
  echo sumArr(a), " ", sumArr(c), " ", o.x + o.y + o.z, " ", sumArr(o.arr),
       " ", o.f, " ", o.inner.a, " ", o.inner.s.len, " ", o.inner.p == nil

proc zeroTail(s: var seq[Obj]; start: int) =
  # an all-zero object stored per element: one memset over the range
  var i = start
  while i < s.len:
    s[i] = Obj(x: 0, y: 0, z: 0, f: 0.0)
    inc i

proc byteFill(p: ptr UncheckedArray[uint8]; lo, hi: int; v: uint8) =
  # a constant byte through a pointer, with the counter read afterwards
  var i = lo
  while i < hi:
    p[i] = 0xAB'u8
    inc i
  echo "counter after: ", i

proc fillNonConst(p: ptr UncheckedArray[int]; n: int; v: int) =
  # not a fill: the value is not known to be the same in every byte
  for i in 0 ..< n: p[i] = v

var globalBuf {.threadvar.}: nil ptr UncheckedArray[int]

proc fillThroughGlobal(n: int) =
  # the base is a thread-local pointer; the stores are `int`, it is a pointer,
  # so they cannot change it and the loop may become a memset
  for i in 0 ..< n: globalBuf[i] = 0

proc u8Loop(): int =
  # `le` up to the type's maximum would never terminate as a loop: such a loop
  # must stay a loop. This one stops at 254.
  var a = default(array[256, uint8])
  for i in 0'u8 .. 254'u8: a[i] = 1
  result = 0
  for x in a: result = result + int(x)

defaults()

var s = newSeq[Obj](5)
for i in 0 ..< s.len:
  s[i].x = i + 1
  s[i].arr[2] = 9
  s[i].inner.a = 3
zeroTail(s, 2)
var t = 0
for o in s: t = t + o.x + int(o.arr[2]) + o.inner.a
echo "seq tail: ", t
zeroTail(s, 5)                       # empty range: nothing happens
echo "empty: ", s[0].x

var bytes: array[10, uint8]
byteFill(cast[ptr UncheckedArray[uint8]](addr bytes[0]), 2, 7, 0xAB'u8)
var line = ""
for b in bytes: line.add $int(b) & " "
echo line
byteFill(cast[ptr UncheckedArray[uint8]](addr bytes[0]), 4, 4, 0'u8)

var ints: array[6, int]
fillNonConst(cast[ptr UncheckedArray[int]](addr ints[0]), 4, 0x0102)
echo ints[0], " ", ints[3], " ", ints[4]

var store: array[5, int]
for i in 0 ..< store.len: store[i] = 42
globalBuf = cast[ptr UncheckedArray[int]](addr store[0])
fillThroughGlobal(3)
echo store[0], " ", store[2], " ", store[3]

echo "u8: ", u8Loop()
