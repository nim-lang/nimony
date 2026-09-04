{.feature: "lenientnils".}
import std/[syncio, assertions]

# Repro: under lenientnils, a seq[cstring] holding both non-nil and nil
# elements is emitted as TWO distinct C seq struct types; the generated C
# fails to compile (gcc: "incompatible types when assigning to type
# 'seq_0_X' from type 'seq_0_Y'"). One Nim type must map to one C type.

proc consume(xs: seq[cstring]): int =
  result = xs.len

var names: seq[cstring] = @[]
names.add(cstring"hello")
names.add(nil)
assert consume(names) == 2
echo "ok"
