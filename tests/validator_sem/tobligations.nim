## Fixture: a `var Cursor` parameter nobody consumes, and a branch that
## advances the cursor more often than it emits -- plus a scanner, which does
## neither and must stay quiet.

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

proc scanner(o: var NifBuilder; n: var NifCursor): bool =
  ## Advances over the whole input and emits none of it, which is the point:
  ## there is no output for the skipped subtrees to be missing from, and the
  ## obligation to reproduce them is the caller's. `o` is here only so that the
  ## signature looks like a translating pass -- it is never written to.
  result = false
  while n.hasMore:
    if n.kind == TagLit: return true
    skip n

proc transform(n: NifCursor): NifBuilder =
  result = createTree()
  var c = n
  droppedInput(result, c)
  unusedCursor(result, c)
  discard scanner(result, c)
