# `parseutils.parseBiggestFloat`. Was quarantined as a native-backend bug: nifasm
# rejected arkham's output with "Type mismatch: expected (i 64), got nil at (mov)".
# Both legs agree again.
import std/[syncio, parseutils]
var f: BiggestFloat = 0.0
echo parseBiggestFloat("3.25tail", f), " ", f
echo parseBiggestFloat("-0.5", f), " ", f
echo parseBiggestFloat("nope", f)
