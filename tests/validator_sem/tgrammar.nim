## Fixture: constructions that do not conform to `doc/tags.md` — too few
## children, and children of the wrong kind. These are the cases the untyped
## engine's `fake_pass.nim` used to pin.

import plugins

proc buildBad(o: var NifBuilder; info: LineInfo) =
  ## `(var D E P T .X)` — five children; this builds one literal.
  o.withTree VarS, info:
    o.addIntLit 1

proc transform(n: NifCursor): NifBuilder =
  result = createTree()
  buildBad(result, n.info)
  buildRefOfNothing(result, n.info)
  buildShortAdd(result, n.info)
  buildAsgnOfDecl(result, n.info, default(SymId))

proc buildRefOfNothing(o: var NifBuilder; info: LineInfo) =
  ## `(ref T)` wants a type, not the empty placeholder.
  o.withTree RefT, info:
    o.addEmptyNode info

proc buildShortAdd(o: var NifBuilder; info: LineInfo) =
  ## `(add T X X)` — three children required, two given.
  o.withTree AddX, info:
    o.addEmptyNode info
    o.addEmptyNode info

proc buildAsgnOfDecl(o: var NifBuilder; info: LineInfo; s: SymId) =
  ## `(asgn X X)` wants expressions, not a definition and a placeholder.
  o.withTree AsgnS, info:
    o.addSymDef s, info
    o.addEmptyNode info
