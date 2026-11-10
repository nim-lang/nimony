# Under earlyMagics the unoverloadable magics (`typeof`, `compiles`,
# `defined`, `declared`) skip overload resolution; a parameter left out still
# takes its declared default (`typeof`'s `mode = typeOfIter`).
{.feature: "earlyMagics".}
import std/syncio

iterator split(s: string): string = yield s
proc split(s: string): seq[string] = @[s]

var x = 1
var a: typeof(x)
a = 4
echo a
var b: typeof(x, typeOfProc)
echo b
var c: x.typeof
echo c
echo typeof("a b".split) is string
echo typeof("a b".split, typeOfIter) is string
echo typeof("a b".split, typeOfProc) is seq[string]
echo compiles(x + 1)
echo compiles(x + zzz)
echo defined(nimony)
echo declared(x)
echo declared(zzz)
