import std/syncio
import deps/mconstvec
import deps/[mconstsplittypes, mconstsplitops]

# A `const` of a type with a `ptr UncheckedArray` field from another module
# is serialized through the `len` and `[]` the evaluated program sees, also
# when the type and its routines are declared in two different modules.
# (Only `seq`'s are called as `system`'s; a same-signature `len` of the
# user's that the evaluated code calls is not kept from taking the call.)

const
  a = mkv()
  rows = [mkv(), mkv()]
  sq = @[mkv()]
  b = mkb()

echo len(a), " ", a[0], a[1], a[2]
echo len(rows[1]), " ", rows[1][2], " ", sq[0][1]
echo len(b), " ", b[0], b[1], b[2]
