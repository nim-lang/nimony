import std/[syncio, assertions]

type
  Payload {.union.} = object
    f: array[4, float32]
    i: array[4, int32]
    u: array[4, uint32]

# Constructing a union must initialize ONLY the given member: filling the
# siblings with defaults emits C designated initializers whose last write
# wins, zeroing the member that was actually set.
let p = Payload(f: [0.25'f32, 0.5'f32, 0.75'f32, 1.0'f32])
let words = cast[ptr UncheckedArray[uint32]](p.addr)
assert words[0] == 0x3E800000'u32
assert words[1] == 0x3F000000'u32
assert words[2] == 0x3F400000'u32
assert words[3] == 0x3F800000'u32
echo "union constr ok"

# Field assignment path (was already correct) stays correct.
var q: Payload
q.u = [7'u32, 8'u32, 9'u32, 10'u32]
assert q.u[3] == 10'u32
echo "union assign ok"
