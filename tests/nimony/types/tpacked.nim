# `{.packed.}` is not decoration: it names the layout the OTHER side of an ABI
# already uses, so the numbers below are the kernel's and not ours to choose.
#
# `struct epoll_event` is 12 bytes on x86-64 — its 8-byte union sits at offset 4,
# deliberately unaligned, for compatibility with the original 32-bit layout — and
# `epoll_wait` fills an ARRAY of them. Laid out naturally it is 16, so entry 0
# reads its `data` from the wrong offset and entry 1 from the wrong address
# entirely. That failed as a HANG, not a crash: the fd read back matched nothing,
# so no completion was ever delivered.
#
# The native backend used to ignore the pragma outright (arkham never read it and
# nifasm had no tag for it), which no test noticed because the C backend gets the
# layout from C. Hence a test that states the offsets as constants.

import std / [syncio, assertions]

type
  Data {.pure, final, union.} = object
    p: pointer
    fd: int32
    u32: uint32
    u64: uint64

  Packed {.pure, final, packed.} = object
    a: uint32
    b: Data          # 8 bytes at offset 4 — the whole point
    c: uint8

  Natural2 {.pure, final.} = object   # the same fields WITHOUT the pragma
    a: uint32
    b: Data
    c: uint8

  PackedSmall {.pure, final, packed.} = object
    x: uint8
    y: uint64        # offset 1, not 8

var p = default(Packed)
var n = default(Natural2)
var s = default(PackedSmall)
let pb = cast[uint](addr p)
let nb = cast[uint](addr n)
let sb = cast[uint](addr s)

echo "packed:  size=", sizeof(Packed), " a=", cast[uint](addr p.a) - pb,
     " b=", cast[uint](addr p.b) - pb, " c=", cast[uint](addr p.c) - pb
echo "natural: size=", sizeof(Natural2), " a=", cast[uint](addr n.a) - nb,
     " b=", cast[uint](addr n.b) - nb, " c=", cast[uint](addr n.c) - nb
echo "small:   size=", sizeof(PackedSmall), " x=", cast[uint](addr s.x) - sb,
     " y=", cast[uint](addr s.y) - sb

# The stride of an ARRAY of a packed type is the packed size — this is the half
# that `sizeof` alone does not pin down, and the half epoll actually depends on.
var arr = default(array[3, Packed])
let ab = cast[uint](addr arr[0])
echo "array stride=", cast[uint](addr arr[1]) - ab,
     " third=", cast[uint](addr arr[2]) - ab

# And with a RUNTIME index, which is a different code path from the folded one.
var i = 1
var j = 2
echo "runtime stride=", cast[uint](addr arr[i]) - ab,
     " third=", cast[uint](addr arr[j]) - ab

assert sizeof(Packed) == 13, "a packed object is the sum of its fields"
assert sizeof(Natural2) == 24, "the same fields unpacked are not"
assert sizeof(PackedSmall) == 9
assert cast[uint](addr arr[1]) - ab == 13'u, "array stride must be the packed size"
