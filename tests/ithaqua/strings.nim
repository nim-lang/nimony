import std/syncio

let a = "foo"
let b = "bar"
let c = a & b
echo c                # foobar
echo c.len            # 6

let n = 42
let msg = "n=" & $n
echo msg              # n=42

echo "count: " & $(1 + 2 + 3)   # count: 6
