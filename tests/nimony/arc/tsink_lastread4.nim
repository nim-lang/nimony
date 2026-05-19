## Repro #4: pattern from typeToCursorLike with string key + src param.

import std / [tables, assertions, syncio]
import ".." / ".." / ".." / src / lib / [nifstreams, nifcursors, lineinfos, bitabs]

type
  Ctx = object
    typeMem: Table[string, TokenBuf]

proc fill(c: var Ctx; key: string; src: TokenBuf): Cursor =
  if c.typeMem.hasKey(key):
    result = cursorAt(c.typeMem.getOrQuit(key), 0)
  else:
    var nb = createTokenBuf(src.len)
    for i in 0 ..< src.len:
      nb.add src[i]
    result = cursorAt(nb, 0)
    c.typeMem[key] = nb

proc main() =
  discard registerTag("a")
  var c = Ctx(typeMem: initTable[string, TokenBuf]())
  var srcBuf = createTokenBuf(8)
  srcBuf.add parLeToken(pool.tags.getOrIncl("a"), NoLineInfo)
  srcBuf.add parRiToken(NoLineInfo)
  for round in 0 ..< 4:
    for i in 0 ..< 8:
      discard fill(c, "k" & $i, srcBuf)
  echo "OK"

main()
