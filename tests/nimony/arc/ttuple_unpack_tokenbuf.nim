import std/[syncio, assertions]
import "../../../src/lib/nifcore"
import "../../../src/lib/lineinfos"

proc foo(): (TokenBuf, PackedLineInfo) =
  result = (createTokenBuf(8), NoLineInfo)

proc main() =
  var (a, b) = foo()
  echo a.len

main()
