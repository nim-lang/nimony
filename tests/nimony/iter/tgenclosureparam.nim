## Regression: semcheck must not crash when instantiating a generic call to a
## `.closure.` proc parameter inside a generic function body.

proc filterProc*[T](data: seq[T], predicate: proc(value: T): bool {.closure.}): seq[T] =
  result = @[]
  for value in data:
    if predicate(value):
      result.add value

discard filterProc(@[1, 2, 3], proc(x: int): bool = x mod 2 == 0)


func filterFunc*[T](data: seq[T], predicate: proc(value: T): bool {.closure, noSideEffect.}): seq[T] =
  result = @[]
  for value in data:
    if predicate(value):
      result.add value

discard filterFunc(@[1, 2, 3], func(x: int): bool = x mod 2 == 0)
