# native: prints 8.152647488794189e+18 (the raw u64 scale). wasm: a [0,1) float.
import std/[syncio, random]
var rf = initRand(7)
echo rf.rand(1.0)
