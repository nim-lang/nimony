# A field named like a type must not shadow that type while its OWN object's
# later fields are still being sem-checked. `seq` here is both a field name
# and (fully qualified) the standard generic type: resolving `seq[Item]`
# in `items`'s type used to hand back the just-declared `seq: int` FIELD (an
# object's fields are added to its own scope purely so `addNonOverloadable`
# catches a duplicate name) and then crash sem-checking that field as a
# local variable — the field is never `publish`ed, so its recorded position
# is meaningless outside its own `(fld ...)`. Real Nim does not shadow here
# at all: a field is visible only through `obj.field`. See
# `rawBuildSymChoice` in sembasics.nim.
import std / [assertions, syncio]

type
  Item = object
    name: string
  Holder = object
    seq: int
    items: seq[Item]

var h = Holder(seq: 1, items: @[])
assert h.seq == 1
assert h.items.len == 0

# Same shape inside a `case` object: a guarded (`gfld`) field shadows no
# less than a plain one.
type
  Kind = enum kA, kB
  Guarded = object
    case kind: Kind
    of kA:
      seq: int
      items: seq[Item]
    of kB:
      discard

var g = Guarded(kind: kA, seq: 5, items: @[])
assert g.seq == 5

echo "OK"
