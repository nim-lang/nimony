import std/syncio

proc p(x: float) = discard
p(1.0'f128)
echo 1.0'f128
