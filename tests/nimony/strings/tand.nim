import std/assertions

# issue #1219

var x = "a"
x = x & "b"
x = x & "c"

assert x == "abc"
