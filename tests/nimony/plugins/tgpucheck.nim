import std / syncio
import deps / gpupragma

## Positive `.gpu` checker test. `{.plugin.}` pipes this module through the
## `gpucheck` restriction checker. `kernel` is `{.gpu.}` and calls only the
## `{.gpu.}` `helper` (and the `+`/`*` magics, which are not module procs), so
## the discipline holds and the module compiles and runs normally.
{.plugin: "deps/gpucheck".}

proc helper(x: int): int {.gpu.} = x + 1
proc kernel(x: int): int {.gpu.} = helper(x) * 2

echo kernel(20)
