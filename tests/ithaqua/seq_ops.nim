import std/[syncio, sequtils]

# nimony has no `$` for seq/tuple, so fixtures render collections by hand.
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

proc show(xs: seq[char]): string =
  result = "@["
  for i in 0 ..< xs.len:
    if i > 0: result.add ", "
    result.add xs[i]
  result.add "]"

proc double(x: int): int = x * 2
proc isEven(x: int): bool = x mod 2 == 0

let xs = @[3, 1, 4, 1, 5, 9, 2, 6]

# basics
echo xs.len                            # 8
echo count(xs, 1)                      # 2
echo minIndex(xs), " ", maxIndex(xs)   # 1 5  (first minimum: index of the 1)
echo show deduplicate(@[1, 1, 2, 3, 3, 3, 4])

# proc-taking transforms (top-level procs: no captures needed)
echo show map(xs, double)
echo show filter(xs, isEven)
echo all(xs, isEven), " ", any(xs, isEven)

# template forms with injected `it`/`a`/`b`
echo show mapIt(xs, it + 100)
echo show filterIt(xs, it > 3)
echo foldl(xs, a + b)                  # 31
echo foldr(@[1, 2, 3, 4], a - b)       # 1-(2-(3-4)) = -2
echo anyIt(xs, it == 9), " ", allIt(xs, it < 10)
echo countIt(xs, it mod 2 == 1)        # count of odds

# structure ops
echo show concat(@[1, 2], @[3, 4])
echo show repeat(7, 3)                 # @[7, 7, 7]
echo show cycle(@[1, 2], 3)            # @[1, 2, 1, 2, 1, 2]
for (n, s) in zip(@[1, 2, 3], @["a", "b", "c"]):
  echo n, "-", s
let (nums, letters) = unzip(@[(1, 'x'), (2, 'y')])
echo show(nums), " ", show(letters)
let (lo, hi) = minmax(xs)
echo lo, " ", hi                       # 1 9

# toSeq over a range and a string
echo show toSeq(1 .. 5)
echo show toSeq("ab")

# in-place mutation
var ys = @[1, 2, 3, 4, 5]
keepIf(ys, isEven)
echo show ys                           # @[2, 4]
var zs = @[1, 2, 3]
applyIt(zs, it * it)
echo show zs                           # @[1, 4, 9]
echo show newSeqWith(3, 42)            # @[42, 42, 42]
