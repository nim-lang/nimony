import std/syncio
import std/sequtils

let nums = @[1, 2, 3, 4, 5, 6]

let doubled = nums.map(proc (x: int): int = x * 2)
echo doubled.len
echo doubled[0], " ", doubled[5]

let evens = nums.filter(proc (x: int): bool = x mod 2 == 0)
echo evens.len
var s = 0
for e in evens: s = s + e
echo s

echo nums.all(proc (x: int): bool = x > 0)
echo nums.any(proc (x: int): bool = x > 5)
echo nums.count(3)

let dup = @[1, 1, 2, 3, 3, 3, 4]
echo dup.deduplicate().len

echo foldl(nums, a + b)
echo mapIt(nums, it * it)[3]
echo countIt(nums, it > 3)
