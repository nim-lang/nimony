# The opt-in of `doc/internals/failure_modes.md`: an allocation panics on
# out-of-memory unless the DESTINATION asks to be handed the `nil` instead, which
# it does by declaring itself `nil T`. This module is not `lenientnils` and this
# routine is not `.raises` -- the annotation on the `let` is the whole request --
# so the prover then demands that every dereference narrow it, and `trNewobj`
# guards the `rc` and payload stores it generates so that the construction returns
# instead of faulting partway through.
import std / [syncio]

type Node = ref object
  data: array[64, int]

proc main() =
  var keep: seq[Node] = @[]
  var sawNil = false
  for i in 0 ..< 100_000:
    let n: nil Node = Node(data: default(array[64, int]))
    if n == nil:
      # The budget refused it. Reaching this line at all is the point.
      sawNil = true
      break
    # narrowed, so it satisfies `seq[Node]`'s not-nil element type
    keep.add n
  echo "construction returned nil: ", sawNil
  echo "kept some: ", keep.len > 0
  if keep.len > 0:
    echo "last is live: ", keep[keep.len-1].data[0] == 0
  echo "still running"

main()
