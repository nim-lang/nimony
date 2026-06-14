{.feature: "lenientnils".}
import std/[syncio, assertions]

# Regression: under lenientnils, `seq[cstring]` was instantiated twice — once
# from a bare `(cstring)` and once from `(cstring (unchecked))` — because the
# generic-instance cache key distinguished the (runtime-irrelevant) nil
# annotation. That produced two structurally-identical C structs the backend
# refused to assign between ("incompatible types ... seq_0_X from seq_0_Y").
# A bare `var names: seq[cstring]` plus an operation triggers it, with or
# without a nil element.

proc consume(xs: seq[cstring]): int =
  result = xs.len

# no nil element — still two instances pre-fix (decl form vs `@[]`/`len` form)
var a: seq[cstring] = @[]
a.add(cstring"hello")
assert consume(a) == 1
assert a.len == 1

# with a nil element under lenientnils
var b: seq[cstring] = @[]
b.add(cstring"hello")
b.add(nil)
assert consume(b) == 2

echo "ok"
