## Regression: a `{.cursor.}` local inside an inlined iterator crashed the
## hexer's iterinliner. `(cursor)` (the pragma) shares the `CursorS` tag with
## a cursor declaration, so `replaceSymbol` read a non-existent symbol name
## off the bare `(cursor)` pragma and asserted.

import std / [assertions]

type
  Node = ref object
    next: Node
    value: int

iterator walk(head: Node): int =
  var it {.cursor.} = head        # the offending declaration
  while it != nil:
    yield it.value
    it = it.next

proc main =
  let a = Node(value: 1, next: Node(value: 2, next: Node(value: 3)))
  var s = 0
  for x in walk(a): s = s + x
  assert s == 6

main()
