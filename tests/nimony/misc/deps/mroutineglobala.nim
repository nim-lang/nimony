when defined(nimony):
  {.feature: "assumeSync".}

proc fromA*(): int =
  var counter {.global.}: int
  inc counter
  counter

template counted*(start: int): int =
  var cached {.global.} = start
  inc cached
  cached

proc a1*(): int = counted(0)
