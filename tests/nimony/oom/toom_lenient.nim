# Tier 2 of `doc/internals/failure_modes.md`: outside a `.raises` routine a failed
# allocation has no error channel, so the caller simply receives the `nil`. What
# the compiler still owes is that the CONSTRUCTION itself does not fault -- it
# guards the `rc` and payload stores it generates -- because a construction that
# died before returning would leave nothing to check.
#
# This module is `lenientnils`, so the prover demands nothing and checking is
# voluntary. Here it is taken up; a module that declines it gets a segfault on the
# first dereference, which is Nim 2's behaviour and is deliberately not asserted
# by a test (a signal's exit status says far less than this does).
{.feature: "lenientnils".}

import std / [syncio]

type Node = ref object
  data: array[64, int]

proc main() =
  var keep: seq[Node] = @[]
  var sawNil = false
  for i in 0 ..< 100_000:
    let n = Node(data: default(array[64, int]))
    if n == nil:
      # The budget refused it. Reaching this line at all is the point: the
      # construction returned instead of faulting partway through.
      sawNil = true
      break
    keep.add n
  echo "construction returned nil: ", sawNil
  echo "kept some: ", keep.len > 0
  # The nils never entered `keep`, so everything in it is still usable.
  if keep.len > 0:
    echo "last is live: ", keep[keep.len-1].data[0] == 0
  echo "still running"

main()
