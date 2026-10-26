# issue #2782: seq slicing must compile; the slice `[]` is a `func` that
# fills its fresh result through `rawData`.

import std/syncio

let data = @[1, 2, 3, 4]
let a = data[0..0]
echo a.len, " ", a[0]
let b = data[1..^1]
echo b.len, " ", b[0], " ", b[2]
let c = data[1..<3]
echo c.len, " ", c[0], " ", c[1]
let e = data[3..1]
echo e.len

let words = @["x", "yy", "zzz"]
let w = words[1..2]
echo w[0], " ", w[1]
