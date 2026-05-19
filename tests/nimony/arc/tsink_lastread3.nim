## Repro #3: variable in an if-else branch (like typeToCursorLike).

import std / [tables, assertions, syncio]
import ".." / ".." / ".." / src / lib / [nifstreams, nifcursors, lineinfos, bitabs]

type
  Ctx = object
    typeMem: Table[int, TokenBuf]

proc fill(c: var Ctx; key: int): Cursor =
  if c.typeMem.hasKey(key):
    result = cursorAt(c.typeMem.getOrQuit(key), 0)
  else:
    var nb = createTokenBuf(8)
    nb.add parLeToken(pool.tags.getOrIncl("a"), NoLineInfo)
    nb.add parRiToken(NoLineInfo)
    result = cursorAt(nb, 0)
    c.typeMem[key] = nb

proc main() =
  discard registerTag("a")
  var c = Ctx(typeMem: initTable[int, TokenBuf]())
  for round in 0 ..< 4:
    for i in 0 ..< 8:
      discard fill(c, i)
  echo "OK"

main()
