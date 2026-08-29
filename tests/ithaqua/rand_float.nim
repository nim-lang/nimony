# `random.rand(float)`. Was quarantined as a native-backend bug: the native leg
# printed the raw u64 draw scale (8.15e+18) instead of a [0,1) float. Both legs
# agree again.
import std/[syncio, random]
var rf = initRand(7)
echo rf.rand(1.0)
