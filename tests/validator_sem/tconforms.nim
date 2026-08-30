## Fixture: a construction that conforms — this one must stay silent, which
## is what keeps the grammar check honest.

import plugins

proc buildGood(o: var NifBuilder; info: LineInfo; s: SymId) =
  ## `(var D E P T .X)`
  o.withTree VarS, info:
    o.addSymDef s, info
    o.addEmptyNode info
    o.addEmptyNode info
    o.withTree IT, info:
      o.addIntLit 64
    o.addEmptyNode info

proc transform(n: NifCursor): NifBuilder =
  result = createTree()
  buildGood(result, n.info, default(SymId))
