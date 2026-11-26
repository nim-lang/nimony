# A forward-declared routine with an `out` parameter: its implementation is
# what the initialization check reads.
import std/syncio
proc g(x: out int)
proc g(x: out int) = x = 3
var v: int
g(v)
echo v
