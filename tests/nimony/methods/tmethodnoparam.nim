# bug #2279: a method without parameters must not blame the return type.
type A = ref object of RootObj

method m(): int {.base.} =
  result = 0

method n(x: int) {.base.} =
  discard
