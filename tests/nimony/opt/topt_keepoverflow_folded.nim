import std/syncio

# `{.keepOverflowFlag.}` asks the backend for an overflow-CHECKED operation whose
# result goes into a temp and whose overflow sets a flag instead of raising.
# The optimizer is free to rewrite the operation away -- `x * 1` -> `x`, `x + 0`
# -> `x`, `x * 0` -> `0`, literal operands -> a literal -- and every such rewrite
# yields a value that cannot overflow. lengc used to reject the result with
# "expected arithmetic operation but got: ...", which made `newSeq[T](1)` fail to
# compile at `-d:release`: `1 * sizeof(T)` folds to `sizeof(T)`.
#
# Both directions matter here: the folded operations must compile AND a genuine
# overflow must still be reported.

proc mulFolded(x: int): int =
  {.keepOverflowFlag.}:
    result = x * 1
    if overflowFlag(): result = -1

proc addFolded(x: int): int =
  {.keepOverflowFlag.}:
    result = x + 0
    if overflowFlag(): result = -1

proc zeroFolded(x: int): int =
  {.keepOverflowFlag.}:
    result = x * 0
    if overflowFlag(): result = -1

proc mulChecked(x, y: int): int =
  {.keepOverflowFlag.}:
    result = x * y
    if overflowFlag(): result = -1

proc main =
  echo mulFolded(7)
  echo addFolded(7)
  echo zeroFolded(7)
  echo mulChecked(6, 7)
  # 2^62 * 4 = 2^64: the check must still fire on the path the optimizer left alone.
  var big = 0x4000_0000_0000_0000
  var four = 4
  echo mulChecked(big, four)
  # `newSeq[T](1)` is what found this: its `size * sizeof(T)` folds to `sizeof(T)`.
  var s = newSeq[int](1)
  s[0] = 5
  echo s.len, " ", s[0]

main()
