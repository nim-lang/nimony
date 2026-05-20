## Tinier repro: sink-pass of a TokenBuf without =dup hook.

import std / [tables, assertions, syncio]
import ".." / ".." / ".." / src / lib / [nifstreams, nifcursors, lineinfos, bitabs]

var tab: Table[int, TokenBuf]

proc fill(key: int) =
  var b = createTokenBuf(8)
  b.add parLeToken(pool.tags.getOrIncl("a"), NoLineInfo)
  b.add parRiToken(NoLineInfo)
  tab[key] = b   # ← sink-pass of `b`; last use of `b`.

proc main() =
  discard registerTag("a")
  for i in 0 ..< 8:
    fill(i)
  echo "OK"

main()
