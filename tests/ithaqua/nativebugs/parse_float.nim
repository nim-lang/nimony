# native: compile fails "[Error] Type mismatch: expected (i 64), got nil at (mov)".
# wasm: 4 3.25 / 4 -0.5 / 0.
import std/[syncio, parseutils]
var f: BiggestFloat = 0.0
echo parseBiggestFloat("3.25tail", f), " ", f
echo parseBiggestFloat("-0.5", f), " ", f
echo parseBiggestFloat("nope", f)
