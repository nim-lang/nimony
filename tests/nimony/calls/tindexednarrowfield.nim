import std / syncio

# Indexing an array of AGGREGATES and loading one field of the element.
#
# The element stride and the access width are different numbers here — an 8-byte
# tuple/object, a 4-byte or 1-byte field — and AArch64's register-offset addressing
# cannot say that: `[Xn, Xm, LSL #k]` carries a single SCALE bit meaning "shift by the
# ACCESS width", not a general shift amount. Emitting the element stride into it read
# `arr[i div 2]` for a 4-byte field and `arr[i div 8]` for a `bool`, silently and only
# for i > 0 — which is why nothing smaller than a self-hosted compiler noticed. It cost
# `hexer/vtables_backend.processMethods` its walk over `seq[(SymId, bool)]`.
#
# Both field widths, and a `seq` (heap) as well as an array (stack/global) base, so the
# SP-base and register-base halves of the address fold are both taken.

type
  Pair = object
    id: uint32
    flag: bool

  Wide = object
    lo, hi: int64

proc sumTagged(xs: seq[Pair]): int =
  result = 0
  for i in 0 ..< xs.len:
    if xs[i].flag:
      result = result + int(xs[i].id)

proc firstIds(xs: seq[Pair]): string =
  result = ""
  for i in 0 ..< xs.len:
    result.add $xs[i].id
    result.add " "

var s: seq[Pair] = @[]
for i in 1 .. 6:
  s.add Pair(id: uint32(i * 100), flag: i mod 2 == 1)

echo sumTagged(s)
echo firstIds(s)

var arr: array[5, Wide]
for i in 0 ..< 5:
  arr[i] = Wide(lo: int64(i), hi: int64(i) * 10)
var acc = 0
for i in 0 ..< 5:
  acc = acc + int(arr[i].hi)
echo acc
