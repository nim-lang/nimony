import std/assertions
func ctPred[T](): bool = sizeof(T) > 0
when ctPred[int]():
  discard
else:
  {.error: "generic predicate returned false".}
const top = ctPred[int]()
assert top

func bytes[T](): int = sizeof(T)
const intBytes = bytes[int]()
assert intBytes == sizeof(int)

func nested[T](): bool =
  when ctPred[T]():
    const local = ctPred[T]()
    result = local
  else:
    result = false
assert nested[int]()

func count[T](n: int): int =
  if n == 0: return sizeof(T)
  count[T](n - 1) + 1
const counted = count[uint8](3)
assert counted == 4

func inferred[T](x: T): int = sizeof(T) + 1
const inferredBytes = inferred(0'i16)
assert inferredBytes == 3
