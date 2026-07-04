import std/assertions

var x: range[0..2] = (range[0..2])(1)
var y: range[0..2] = 1

assert x == y

# in-range literals at the boundaries are accepted
var lo: range[0..10] = 0
var hi: range[0..10] = 10
assert lo == 0
assert hi == 10

# a narrower range is a subtype of a wider one: `range[2..5]` -> `range[0..10]`
var narrow: range[2..5] = 3
var wide: range[0..10] = narrow
assert wide == 3

# equal bounds still match
var same: range[0..10] = wide
assert same == 3
