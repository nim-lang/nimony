## Reached by `reexportuser.nim` only through `reexportfacade.nim`'s
## `export reexportdep`. The incremental suite swaps the two defaulted
## parameters of `connect` and later edits its body.
type Options* = object
  n*: int

proc initOptions*(): Options = Options(n: 7)

proc connect*(url: string; options = initOptions(); dl = 0): int =
  url.len + options.n + dl
