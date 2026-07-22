# native: SIGSEGV for ANY len<2 sort — empty and one-element both crash
# (each standalone). wasm: prints 0 then 42.
import std/[syncio, algorithm]
proc cmpInt(x, y: int): int = x - y
var empty: seq[int] = @[]
sort(empty, cmpInt)
echo empty.len                         # 0
var one = @[42]
sort(one, cmpInt)
echo one[0]                            # 42
