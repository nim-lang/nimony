## Repro #3: variable in an if-else branch (like typeToCursorLike).

import std / [tables, assertions, syncio]
import ".." / ".." / ".." / src / lib / nifcore

type
  Ctx = object
    typeMem: Table[int, TokenBuf]

proc fill(c: var Ctx; key: int): Cursor =
  if c.typeMem.hasKey(key):
    result = cursorAt(c.typeMem.getOrQuit(key), 0)
  else:
    var nb = createTokenBuf(8)
    nb.openTag nb.tags.registerTag("a")
    nb.closeTag()
    result = cursorAt(nb, 0)
    c.typeMem[key] = nb

proc main() =
  var c = Ctx(typeMem: initTable[int, TokenBuf]())
  for round in 0 ..< 4:
    for i in 0 ..< 8:
      discard fill(c, i)
  echo "OK"

main()
