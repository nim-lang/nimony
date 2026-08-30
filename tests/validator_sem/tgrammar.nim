## Fixture: a `(var …)` built with one child where the grammar wants five.

import plugins

proc buildBad(o: var NifBuilder; info: LineInfo) =
  ## `(var D E P T .X)` — five children; this builds one literal.
  o.withTree VarS, info:
    o.addIntLit 1

proc transform(n: NifCursor): NifBuilder =
  result = createTree()
  buildBad(result, n.info)
