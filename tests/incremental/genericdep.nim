## Reached by `genericuser.nim` only through the generic `incl` of
## `genericmid.nim`. The incremental suite drops the `.requires` of
## `needPositive`, which removes its split body ``needPositive`body``.
{.feature: "staticContracts".}

func needPositive(x: int): bool {.inline, requires: x > 0.} =
  result = x > 1

func putIt*[T](x: T): bool {.inline.} =
  discard x
  needPositive(3)
