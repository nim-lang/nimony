# issue #2096
import std/assertions
import deps/mgetclosure

assert getClosure()() == 7
