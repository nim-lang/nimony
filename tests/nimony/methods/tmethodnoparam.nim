# bug #2279: a method without parameters must not blame the return type.
import deps/mvtbase
import deps/mvtmid

type A = ref object of RootObj

method m(): int {.base.} =
  result = 0

method n(x: int) {.base.} =
  discard

# llms#175: only the class's module emits its vtable, so a base method must be
# declared there (Araq).
method fresh(a: Animal): int {.base.} = 0
discard fresh(Animal())

# llms#345: an override is in no vtable either, so it must be declared there too.
method speak(d: Dog): string = "Grr"
discard speak(Dog())
