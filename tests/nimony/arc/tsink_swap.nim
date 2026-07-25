## Repro: swap of TokenBufs inside a struct.

import std / [assertions, syncio]
import ".." / ".." / ".." / src / lib / nifcore

type
  Match = object
    args: TokenBuf
    note: int

proc fill(m: var Match) =
  m.args.openTag m.args.tags.registerTag("a")
  m.args.closeTag()
  var dest = createTokenBuf(4)
  swap m.args, dest        # exchange m.args with dest
  m.args.openTag m.args.tags.registerTag("b")
  swap m.args, dest        # swap back
  echo "args.len=", m.args.len

proc main() =
  for round in 0 ..< 4:
    var m = Match(args: createTokenBuf(4), note: -1)
    fill(m)
  echo "OK"

main()
