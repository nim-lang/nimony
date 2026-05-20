## Repro #2: add cursorAt(nb, 0) before the sink-pass.

import std / [tables, assertions, syncio]
import ".." / ".." / ".." / src / lib / [nifstreams, nifcursors, lineinfos, bitabs]

var tab: Table[int, TokenBuf]

proc fill(key: int): Cursor =
  var b = createTokenBuf(8)
  b.add parLeToken(pool.tags.getOrIncl("a"), NoLineInfo)
  b.add parRiToken(NoLineInfo)
  result = cursorAt(b, 0)   # cursor created BEFORE the sink-pass.
  tab[key] = b

proc main() =
  discard registerTag("a")
  for i in 0 ..< 8:
    discard fill(i)
  echo "OK"

main()
