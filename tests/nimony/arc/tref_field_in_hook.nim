## Regression: reading a field through a boxed `ref` inside a `.nodestroy`
## hook (custom `=destroy`) generated C that dereferenced the ref cell instead
## of its object payload ("has no member named 'next_0'"). Exercises a custom
## non-recursive destructor that frees a long chain and its elements.

import std / [assertions]
{.feature: "lenientnils".}
var freed = 0
type
  Elem = object
    id: int
  Node = ref object
    next: Node
    value: Elem
  List = object
    head: Node

proc `=destroy`(e: Elem) = inc freed

proc `=destroy`(x: List) {.nodestroy.} =
  var it = x.head
  while it != nil:
    var nxt = move it.next     # field read through a ref inside the hook
    `=destroy`(it)
    `=wasMoved`(it)
    it = move nxt

proc `=wasMoved`(x: var List) {.nodestroy.} =
  x.head = nil

proc push(l: var List; id: int) =
  var n = Node(value: Elem(id: id))
  n.next = l.head
  l.head = n

proc scoped =
  var l = default(List)
  var i = 0
  while i < 1000:
    push(l, i)
    inc i
  # `l` is destroyed here: non-recursive teardown must free all 1000 elements
  # without overflowing the stack.

scoped()
assert freed == 1000
