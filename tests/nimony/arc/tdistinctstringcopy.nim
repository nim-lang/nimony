# bug #1962: `seq.add string(distinctValue)` moved the heap buffer instead of
# copying it, so the returned seq aliased the source's storage and freed it on
# destruction — a use-after-free. A `distinct string` (and distinct of any
# resource type) must inherit the base type's `=destroy`/`=dup` hooks.

import std/syncio

type
  Comp = distinct string
  Holder = object
    items: seq[Comp]

proc heapStr(prefix: string; n: int): Comp =
  # Force a heap-allocated string (no literal sharing).
  var s = ""
  var i = 0
  while i < n:
    s.add prefix
    inc i
  Comp(s)

proc names(h: Holder): seq[string] =
  result = @[]
  var i = 0
  while i < h.items.len:
    result.add string(h.items[i])
    inc i

proc consume(h: Holder) =
  let n = names(h)
  echo "consume sees ", n.len, " names"

proc render(h: Holder): string =
  result = ""
  var i = 0
  while i < h.items.len:
    result.add string(h.items[i])
    inc i

let root = Holder(items: @[heapStr("alpha", 4), heapStr("beta", 4)])
echo "before: ", render(root)
consume(root)
echo "after:  ", render(root)
