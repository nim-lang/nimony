## Tinier repro: sink-pass of a TokenBuf without =dup hook.

import std / [tables, assertions, syncio]
import ".." / ".." / ".." / src / lib / nifcore

var tab: Table[int, TokenBuf]

proc fill(key: int) =
  var b = createTokenBuf(8)
  b.openTag b.tags.registerTag("a")
  b.closeTag()
  tab[key] = b   # ← sink-pass of `b`; last use of `b`.

proc main() =
  for i in 0 ..< 8:
    fill(i)
  echo "OK"

main()
