# Every length is at most `MaxContainerLen` (half of `high(int)`), which the
# allocator enforces. So an index plus a length, or two lengths, cannot wrap,
# and these sums prove without an `assume`, although the difference facts
# cannot relate the three values involved.
import std/syncio

proc afterMatch(s, sub: string; j: int): Natural =
  # `j + sub.len` for a `j` inside `s`: the end of a match
  result = 0
  if j >= 0 and j < s.len:
    result = j + sub.len

proc twoLens(a, b: string): Natural =
  result = a.len + b.len

proc lenMinusIndex(s: seq[int]; i: int): Natural =
  result = 0
  if i >= 0 and i <= s.len:
    result = s.len - i

proc stepByRune(s: string; o: int; n: int): Natural =
  # `o + n` for `o < s.len` and a small, bounded `n`: a UTF-8 decoder's step
  result = 0
  if o >= 0 and o < s.len and n >= 1 and n <= 6:
    result = o + n

echo afterMatch("abcdef", "cd", 2), " ", twoLens("ab", "cde"), " ",
  lenMinusIndex(@[1, 2, 3], 1), " ", stepByRune("héllo", 1, 2)

# A view of `first .. last` is empty for `last < first`, never negative
let data = [1, 2, 3, 4]
let backwards = toOpenArray(data, 3, 0)
echo backwards.len
