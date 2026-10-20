# A `{.cursor.}` field borrows the value it is constructed with: the source
# keeps its reference and destroys it at scope end, even when the
# construction is its last use.

import std/syncio

type
  Probe = object
    x: int
  Node = ref object
    p: Probe
    next: nil Node
    prev {.cursor.}: nil Node
  Entry = object
    at: int
    n {.cursor.}: Node

proc `=destroy`(o: Probe) = echo "destroyed ", o.x

proc lastUse() =
  let n = Node(p: Probe(x: 1))
  let e = Entry(at: 1, n: n)
  echo e.at, " ", e.n.p.x

proc list() =
  var head: nil Node = nil
  var entries: seq[Entry] = @[]
  for i in 2..4:
    let n: nil Node = Node(p: Probe(x: i))
    if n == nil: return
    if head != nil: head.prev = n
    n.next = head
    head = n
    entries.add Entry(at: i, n: n)
  let a = entries[0]
  let b = entries[2]
  entries[0] = b
  entries[2] = a
  echo entries[0].n.p.x
  entries.shrink 0
  echo "dropping"
  head = nil

lastUse()
list()
echo "done"
