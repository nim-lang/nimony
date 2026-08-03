import std/assertions

# Regression for: a set constructor whose element is a `not`/`shl`/`shr`
# expression. `xints`' bitwise ops worked on sign+magnitude, so folding
# `(not 0'i32) and 7` yielded `-1 and 7 == 1` instead of `7` and the wrong
# bit was set in the constant bitset.

var a: set[uint8] = {uint8((not 0'i32) and 7), 9'u8}
assert 7'u8 in a
assert 9'u8 in a
assert 1'u8 notin a

var b: set[uint8] = {uint8(1 shl 2), 9'u8}
assert 4'u8 in b

var c: set[uint8] = {uint8(255 shr 5), 9'u8}
assert 7'u8 in c

const K = (not 0'i32) and 7
var d: set[uint8] = {uint8(K), 9'u8}
assert 7'u8 in d

# the same value routed through a `let` must agree with the folded form
let v = uint8((not 0'i32) and 7)
var e: set[uint8] = {v, 9'u8}
assert e == a
