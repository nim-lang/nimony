import std/syncio

# A `{.keepOverflowFlag.}` block reads the enclosing PROC's overflow flag. The
# flag is not sticky -- each checked operation sets it to its own verdict -- so
# a body that reads it can be spliced into its caller: an earlier inlined
# overflow, or the previous loop iteration's, must not leak into the next
# `overflowFlag()` read. Both callees are tiny and `.inline`, so they are
# spliced.

proc mulChecked(x, y: int): int {.inline.} =
  {.keepOverflowFlag.}:
    result = x * y
    if overflowFlag(): result = -1

proc addChecked(x, y: int): int {.inline.} =
  {.keepOverflowFlag.}:
    result = x + y
    if overflowFlag(): result = -1

proc main =
  var big = 0x4000_0000_0000_0000
  var four = 4
  # 2^62 * 4 overflows: the flag is set inside `mulChecked`'s own frame.
  echo mulChecked(big, four)
  # Must not see the previous call's overflow.
  echo addChecked(1, 2)
  # An overflow in one iteration must not taint the following ones.
  var acc = 0
  for i in 0 ..< 4:
    var x = i
    if i == 1: x = big
    let r = mulChecked(x, four)
    if r == -1:
      acc = acc + 100
    else:
      acc = acc + r
  echo acc
  # `newSeq`'s size computation is a `keepOverflowFlag` block too; after the
  # overflows above it must still see a clean flag.
  var s = newSeq[int](2)
  s[1] = 7
  echo s.len, " ", s[1]

main()
