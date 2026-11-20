# bug #2279: a method without parameters must not blame the return type.
import deps/mvtbase

type A = ref object of RootObj

method m(): int {.base.} =
  result = 0

method n(x: int) {.base.} =
  discard

# llms#175: only the class's module emits its vtable, so a base method must be
# declared there (Araq).
method fresh(a: Animal): int {.base.} = 0
discard fresh(Animal())
