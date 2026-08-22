import std/[syncio, algorithm]

proc show(xs: seq[int]): string =
  result = "@["
  for i in 0 ..< xs.len:
    if i > 0: result.add ", "
    result.add $xs[i]
  result.add "]"

proc show(xs: seq[string]): string =
  result = "@["
  for i in 0 ..< xs.len:
    if i > 0: result.add ", "
    result.add xs[i]
  result.add "]"

proc cmpInt(x, y: int): int = x - y
proc cmpStr(x, y: string): int =
  if x < y: -1 elif x > y: 1 else: 0

# ints, both orders
var xs = @[5, 2, 8, 1, 9, 3, 7, 4, 6]
sort(xs, cmpInt)
echo show xs
echo isSorted(xs, cmpInt)              # true
sort(xs, cmpInt, Descending)
echo show xs

# sorted (non-destructive) leaves the input alone
let orig = @[3, 1, 2]
echo show sorted(orig, cmpInt)
echo show orig                         # @[3, 1, 2]

# strings
var names = @["ward", "hikaru", "sumi", "kata", "ithaqua"]
sort(names, cmpStr)
echo show names

# stability: equal keys keep their input order (merge sort contract)
proc cmpFirst(x, y: (int, int)): int = x[0] - y[0]
var pairs = @[(2, 1), (1, 1), (2, 2), (1, 2), (2, 3)]
sort(pairs, cmpFirst)
for (k, v) in pairs.items:
  echo k, ":", v                       # 1:1 1:2 2:1 2:2 2:3

# (sort of a len<2 seq — empty OR one-element — segfaults the NATIVE
# backend; see nativebugs/sort_empty.nim. Re-add both here when fixed.)
