proc foo(x: int16) = discard
proc bar(x: float) = discard
foo(123)
bar(123)
proc baz(x: int16) = discard
proc baz(x: int) = discard
baz(123)

# issue #477
var x: int32
var y: int

# Following code compiles without errors.
discard x == y
discard y == 123.int32
discard 123.int32 == y
discard 123.int == 123.int
discard 123.int32 == 123.int32

# Following code causes compile error: "Error: ambiguous call"
discard 123.int32 == 123
discard 123 == 123.int32
discard x == 123
