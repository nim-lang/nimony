import std / [syncio, assertions]
import mvarargs

echo echoSum(1, 2, 3)
echo echoSum()
echo sumWith(100, 1, 2, 3)
echo sumWith(0)
assert echoSum(5, 5) == 10
assert sumWith(7, 1, 1, 1, 1, 1) == 12

type Color = enum Red, Green, Blue
say("hi", 42, Red, "bye")
