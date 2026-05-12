import std/[syncio, assertions]
import "../../../src/lib/nifcursors"
import "../../../src/lib/nifstreams"
import "../../../src/lib/lineinfos"
import "../../../src/lib/bitabs"

proc foo(): (TokenBuf, PackedLineInfo) =
  result = (createTokenBuf(8), NoLineInfo)

proc main() =
  var (a, b) = foo()
  echo a.len

main()
