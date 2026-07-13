## Regression: semcheck must not crash when instantiating a generic call to a
## `.closure.` proc parameter inside an iterator-returning function body.

func filter*[T](iter: iterator(): T, predicate: proc(value: T): bool {.closure.}): iterator(): T =
  result = iterator(): T =
    for value in iter():
      if predicate(value):
        yield value
  result
