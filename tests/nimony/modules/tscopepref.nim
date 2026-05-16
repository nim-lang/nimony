
import std / [syncio, assertions]
import deps / mscopepref

# Case 1: locally defined proc with the same signature as the imported one
# should win — local exact match beats imported exact match.
proc which(x: int): string = "local-int"

assert which(1) == "local-int"

# Case 2: only an imported overload exists for `float`. The local `int`
# overload requires a conversion, so the imported `float` candidate must
# still win — implicit conversions are ranked above the scope tiebreaker.
assert which(1.0) == "imported-float"
echo "ok"
