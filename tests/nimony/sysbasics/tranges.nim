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

# a range of an enum or distinct type converts to an int type
# (nimony-bugs-llms#123)
type
  RangeE = enum reA, reB, reC, reD
  RangeD = distinct int
var re: range[reB..reC] = reC
var rd: range[RangeD(0)..RangeD(7)] = RangeD(3)
assert ord(re) == 2
assert int(re) == 2
assert int8(re) == 2'i8
assert int(rd) == 3

# subrange match: a range of an enum, distinct or bool type is passed to a
# parameter of its base type (nimony-bugs-llms#183)
proc byValE(x: RangeE): int = ord(x)
assert byValE(re) == 2
proc byValD(x: RangeD): int = int(x)
assert byValD(rd) == 3
proc byValB(x: bool): bool = x
var rb: range[false..true] = true
assert byValB(rb)
# a subrange match ranks below an exact and a generic match
proc ovE(x: range[reB..reC]): string = "range"
proc ovE(x: RangeE): string = "E"
assert ovE(re) == "range"
proc ovG[T](x: T): string = "generic"
proc ovG(x: RangeE): string = "E"
assert ovG(re) == "generic"
proc ovB[T](x: T): string = "generic"
proc ovB(x: bool): string = "bool"
assert ovB(rb) == "generic"
