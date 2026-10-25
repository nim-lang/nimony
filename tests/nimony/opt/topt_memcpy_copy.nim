# Copy loops that shoggoth's `memidioms` pass turns into `(memcpy …)` (#2350).
# Two local arrays cannot overlap, so their loop is replaced. Two pointers
# can, and then the forward element loop is NOT a memcpy: copying a range one
# element up repeats the first element. Those keep the loop behind a runtime
# disjointness test, and both directions of overlap are exercised here.

import std / syncio

proc show(a: openArray[int32]): string =
  result = ""
  for x in a: result.add $int(x) & " "

proc localArrays(): string =
  var src = default(array[12, int32])
  for i in 0 ..< src.len: src[i] = int32(i * 3)
  var dst = default(array[12, int32])
  for i in 2 ..< 9: dst[i] = src[i]
  result = show(dst)

proc ptrCopy(d, s: ptr UncheckedArray[int32]; lo, hi: int) =
  var i = lo
  while i < hi:
    d[i] = s[i]
    inc i

proc seqCopy(dst: var seq[int32]; src: seq[int32]) =
  for i in 0 ..< src.len: dst[i] = src[i]

echo localArrays()

var a = default(array[10, int32])
for i in 0 ..< a.len: a[i] = int32(i + 1)
var b = default(array[10, int32])
ptrCopy(cast[ptr UncheckedArray[int32]](addr b[0]),
        cast[ptr UncheckedArray[int32]](addr a[0]), 1, 9)
echo "disjoint: ", show(b)

# overlap, destination ABOVE the source: the loop smears a[0]
ptrCopy(cast[ptr UncheckedArray[int32]](addr a[1]),
        cast[ptr UncheckedArray[int32]](addr a[0]), 0, 6)
echo "up: ", show(a)

for i in 0 ..< a.len: a[i] = int32(i + 1)
# overlap, destination BELOW the source: an ordinary shift down
ptrCopy(cast[ptr UncheckedArray[int32]](addr a[0]),
        cast[ptr UncheckedArray[int32]](addr a[2]), 0, 6)
echo "down: ", show(a)

# exact aliasing: copying a range onto itself
ptrCopy(cast[ptr UncheckedArray[int32]](addr a[0]),
        cast[ptr UncheckedArray[int32]](addr a[0]), 0, 10)
echo "self: ", show(a)

# empty range
ptrCopy(cast[ptr UncheckedArray[int32]](addr b[0]),
        cast[ptr UncheckedArray[int32]](addr a[0]), 5, 5)
echo "empty: ", show(b)

var s1 = @[1'i32, 2, 3, 4, 5]
var s2 = newSeq[int32](5)
seqCopy(s2, s1)
echo "seq: ", show(s2)
