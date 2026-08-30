## Fixture: advancing a cursor whose scope is known to be empty.
##
## `hasMore` is `rem > 0` and `inc` asserts `rem != 0`, so a guard that says
## "there is nothing left" is naming exactly the case in which the advance is
## illegal. This is the classic model's `skipParRi`, which had a closing token
## to step over; nifcore has none. See nim-lang/nimony#2408.

import plugins

proc pastTheEnd(o: var NifBuilder; n: var NifCursor) =
  ## The advance runs only when the scope is empty.
  if not n.hasMore:
    inc n
  else:
    takeTree o, n

proc pastTheEndElse(o: var NifBuilder; n: var NifCursor) =
  ## Same, written the other way round.
  if n.hasMore:
    takeTree o, n
  else:
    skip n

proc rewound(o: var NifBuilder; n: var NifCursor; start: NifCursor) =
  ## Fine: the cursor is put somewhere else before it moves.
  if not n.hasMore:
    n = start
    skip n
  else:
    takeTree o, n

proc drains(o: var NifBuilder; n: var NifCursor) =
  ## Fine: the loop body never runs on an empty scope.
  if not n.hasMore:
    while n.hasMore: skip n
  else:
    takeTree o, n

proc transform(n: NifCursor): NifBuilder =
  result = createTree()
  var c = n
  let start = c
  pastTheEnd(result, c)
  pastTheEndElse(result, c)
  rewound(result, c, start)
  drains(result, c)
