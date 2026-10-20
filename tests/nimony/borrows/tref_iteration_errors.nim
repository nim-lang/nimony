# Test: a loop over a location behind a `ref` is rejected: an alias of the
# `ref` reaches the same `seq` under a path the loop's borrow does not cover.

type Node = ref object
  kids: seq[int]

proc viaAlias =
  let r = Node(kids: @[1, 2, 3])
  let r2 = r
  for x in r.kids:
    r2.kids.add x   # would reallocate the buffer the loop reads

viaAlias()
