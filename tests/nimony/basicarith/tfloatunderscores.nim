# Digit separators in float literals are ignored (`parseBiggestFloat` stops at
# a `_`, so nifler2 strips them before it parses the literal).
import std/syncio

echo 360_000.0
echo 1_000.5
echo 1_000_000_000.0'f32
echo 2.5e1_0
const c = 2_500.0
echo c
echo 1_0.2_5
