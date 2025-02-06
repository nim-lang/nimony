proc foo(x: int16) = discard
proc bar(x: float) = discard
foo(123)
bar(123)
proc baz(x: int16) = discard
proc baz(x: int) = discard
baz(123)
