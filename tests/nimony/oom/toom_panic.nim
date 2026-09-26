# The other half of the same rule: outside a `.raises` routine a failed `new`
# has no error channel to travel on and no value to hand back -- `ref T` is
# not-nil -- so it panics. See `doc/internals/failure_modes.md`.
import std / [syncio]

type Node = ref object
  data: array[64, int]

proc leak() =
  var keep: seq[Node] = @[]
  for i in 0 ..< 100_000:
    keep.add Node(data: default(array[64, int]))
  echo "allocated ", keep.len

leak()
