## Repro: swap of TokenBufs inside a struct.

import std / [assertions, syncio]
import ".." / ".." / ".." / src / lib / [nifstreams, nifcursors, lineinfos, bitabs]

type
  Match = object
    args: TokenBuf
    note: int

proc fill(m: var Match) =
  m.args.add parLeToken(pool.tags.getOrIncl("a"), NoLineInfo)
  m.args.add parRiToken(NoLineInfo)
  var dest = createTokenBuf(4)
  swap m.args, dest        # exchange m.args with dest
  m.args.add parLeToken(pool.tags.getOrIncl("b"), NoLineInfo)
  swap m.args, dest        # swap back
  echo "args.len=", m.args.len

proc main() =
  discard registerTag("a")
  discard registerTag("b")
  for round in 0 ..< 4:
    var m = Match(args: createTokenBuf(4), note: -1)
    fill(m)
  echo "OK"

main()
