## Fixture: cursor-bounded loops, two that advance on every path and two
## that do not.

import plugins

proc good1(o: var NifBuilder; n: var NifCursor) =
  while n.hasMore:
    takeTree o, n

proc good2(o: var NifBuilder; n: var NifCursor) =
  while n.hasMore:
    if n.kind == TagLit:
      skip n
    else:
      takeTree o, n

proc bad1(o: var NifBuilder; n: var NifCursor) =
  ## nothing advances `n`
  while n.hasMore:
    o.addIntLit 1

proc bad2(o: var NifBuilder; n: var NifCursor) =
  ## only one branch advances, and there is no else
  while n.hasMore:
    if n.kind == TagLit:
      skip n

proc transform(n: NifCursor): NifBuilder =
  result = createTree()
  var c = n
  good1(result, c)
  good2(result, c)
  bad1(result, c)
  bad2(result, c)
