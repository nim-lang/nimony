# An `and`/`or` inside a leaf of a pure `if`/`while` condition is a value,
# not part of the condition's short-circuit spine (issue: `(a or b) != c`
# aborted nimsem with "and/or should have been handled by ... xelim.nim").

import std/syncio

proc ne(i, c: int): bool =
  if (i == 4 or i == 7) != (c == 1): false else: true

proc onSpine(i, c: int): bool =
  if c >= 0 and (i == 4 or i == 7) != (c == 1): false else: true

proc loop(i, c: int): int =
  result = 0
  var c = c
  while (i == 4 or i == 7) != (c == 1) and result < 3:
    inc result
    c = 1 - c

proc run(i, c: int) =
  echo ne(i, c), " ", onSpine(i, c), " ", loop(i, c)

run(4, 1)
run(3, 1)
run(7, 0)
