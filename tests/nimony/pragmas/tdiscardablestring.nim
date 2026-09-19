import std/syncio

# A `{.discardable.}` call whose result has a destructor (a `string`), discarded
# implicitly. The call was left as a bare statement, which a later pass bound to
# a temp for its destructor and then left that temp's symbol behind as a
# statement: "statement expected, but got: `tmp.0". An explicit `discard` was fine.

var made = 0

proc mk(x: int): string {.discardable.} =
  inc made
  result = "v" & $x

proc notTail() =
  mk(1)                 # implicit discard, followed by another statement
  echo "after ", made

proc explicit() =
  discard mk(6)         # the explicit form, which always worked

notTail()
explicit()
echo mk(5), " ", made
