## Closure iterators returning `var T` / `lent T`: the for-loop variable is a
## borrow into the iter's source, so assignments through it mutate the source.
## Implementation note: this works without any var-specific codegen in
## cps.nim — sem/desugar emits the iter call with an outer `(hderef ...)` and
## iterinliner peels it; the slot field's existing `ptr T` wrap then naturally
## becomes `ptr (mut T)` and yield-writes lower to address-stores.

import std / syncio

iterator mitems(s: var array[3, int]): var int {.closure.} =
  yield s[0]
  yield s[1]
  yield s[2]

iterator items(s: array[3, int]): lent int {.closure.} =
  yield s[0]
  yield s[1]
  yield s[2]

proc main() =
  var arr = [10, 20, 30]
  for x in mitems(arr):
    x = x * 2
  for v in items(arr):
    echo v

main()
