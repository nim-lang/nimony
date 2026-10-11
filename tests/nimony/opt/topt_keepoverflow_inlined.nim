import std/syncio

# A `{.keepOverflowFlag.}` block reads and sets the enclosing PROC's overflow
# flag, which the backends keep as ONE sticky per-function flag: false on
# entry, set by a checked operation that overflowed, never reset. Splicing
# such a body into its caller would make it share the caller's flag, so an
# earlier inlined overflow -- or the previous loop iteration's -- leaks into
# the next `overflowFlag()` read. The inliner therefore never inlines a proc
# that touches the flag. Both callees are tiny and `.inline`: exactly what it
# would otherwise splice.

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
