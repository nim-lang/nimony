
import std/[syncio]

# bug #1451

proc testFlat(x: openArray[(string, string)]) =
  for a, b in x.items:
    echo a, " ", b

proc testUnpacked(x: openArray[(string, string)]) =
  for (a, b) in x.items:
    echo a, " ", b


testFlat([("foo1", "bar1")])
testUnpacked([("foo2", "bar2")])
