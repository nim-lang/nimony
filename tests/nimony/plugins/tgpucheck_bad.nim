import deps / gpupragma

## Negative `.gpu` checker test. `kernel` is `{.gpu.}` but calls `plain`, a
## same-module non-`.gpu` proc — the `gpucheck` plugin rejects it.
{.plugin: "deps/gpucheck".}

proc plain(x: int): int = x + 1
proc kernel(x: int): int {.gpu.} = plain(x) * 2
