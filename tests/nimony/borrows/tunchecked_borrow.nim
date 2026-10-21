# Test: a location behind a `ref` is iterated by index, or with
# `uncheckedBorrow`, which takes the borrow checker's protection off it.

import std/[syncio, tables]

type
  Node = ref object
    kids: seq[int]
    names: Table[string, int]

proc main =
  let r = Node(kids: @[1, 2, 3], names: initTable[string, int]())
  r.names["a"] = 1
  var s = 0
  for i in 0 ..< r.kids.len:    # by index: needs no promise
    s += r.kids[i]
  for k in uncheckedBorrow(r.kids):
    s += k
  for k, v in uncheckedBorrow(r.names):
    s += v
  echo s

main()
