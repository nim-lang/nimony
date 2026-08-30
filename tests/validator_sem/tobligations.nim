## Fixture: a `var Cursor` parameter nobody consumes, and a branch that
## advances the cursor more often than it emits.

import plugins

proc unusedCursor(o: var NifBuilder; n: var NifCursor) =
  ## `n` is never consumed: the obligation is unmet.
  o.addIntLit 1

proc droppedInput(o: var NifBuilder; n: var NifCursor) =
  ## The `TagLit` branch advances twice and emits once.
  if n.kind == TagLit:
    o.addSubtree n
    skip n
    skip n
  else:
    o.addSubtree n
    skip n

proc transform(n: NifCursor): NifBuilder =
  result = createTree()
  var c = n
  droppedInput(result, c)
  unusedCursor(result, c)
