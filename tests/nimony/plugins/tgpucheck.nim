import std / syncio
import deps / gpupragma

{.plugin: "deps/gpucheck".}

proc helper(x: int): int {.gpu.} = x + 1
proc kernel(x: int): int {.gpu.} = helper(x) * 2

echo kernel(20)
