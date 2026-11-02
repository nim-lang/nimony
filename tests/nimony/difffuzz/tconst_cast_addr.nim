# Found by tools/difffuzz.nim (reduced): hexer: a bit-cast of a `const` took `addr` of the const, which is inlined to `addr <literal>` (C backend).
# Do not simplify by hand: several of these only reproduce with this exact
# register allocation.
import std/syncio
const k = 0.5'f32
const d = 0.5
echo cast[uint32](k), " ", cast[uint64](d)
