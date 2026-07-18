# issue #2096
import std/assertions
import deps/mpassclosure

var c = passClosure()
assert c() == 7
