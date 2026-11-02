# Found by tools/difffuzz.nim (reduced): sem const evaluation: `cast[float32](u32)` / `cast[uint32](f32)` ignored the float width (folded as float64).
# Do not simplify by hand: several of these only reproduce with this exact
# register allocation.
import std/syncio
const k1 = cast[float32](0xEE264B0E'u32)
const k2 = cast[uint32](0.1'f32)
const k3 = cast[uint64](0.1)
const k4 = cast[float64](0x3FF0000000000000'u64)
proc t() =
  var a7: float64 = float64((cast[float32](0xEE264B0E'u32) * -1.0'f32))
  echo cast[uint64](a7), " ", cast[uint32](k1), " ", k2, " ", k3, " ", k4
t()
