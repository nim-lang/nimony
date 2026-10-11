# The default of `doc/internals/failure_modes.md`: a plain `ref` destination has no
# value to stand in for a failed allocation, so the construction panics rather than
# handing back a `nil` that nothing forces anyone to check. One rule, and it is
# local -- you can see from the declaration which it is.
#
# `toom_nildest.nim` is the same loop with the destination declared `nil Node`, and
# `toom_raises.nim` the same failure carried by a `.raises` signature instead.
import std / [syncio]

type Node = ref object
  data: array[64, int]

proc main() =
  var keep: seq[Node] = @[]
  for i in 0 ..< 100_000:
    keep.add Node(data: default(array[64, int]))
  echo "not reached"

echo "allocating"
flushFile(stdout)
main()
