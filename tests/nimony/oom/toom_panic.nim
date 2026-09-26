# The other half of the same rule: outside a `.raises` routine a failed `new` has
# no error channel to travel on, so the `nil` it produces is the caller's to deal
# with. This module is `lenientnils`, which is the mode where it is NOT: pointers
# are `unchecked`, the prover demands nothing, and a nil that reaches a deref
# faults the way it does in Nim 2. See `doc/internals/failure_modes.md`.
{.feature: "lenientnils".}

import std / [syncio]

type Node = ref object
  data: array[64, int]

proc leak() =
  var keep: seq[Node] = @[]
  for i in 0 ..< 100_000:
    keep.add Node(data: default(array[64, int]))
  echo "allocated ", keep.len

leak()
