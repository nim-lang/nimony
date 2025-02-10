proc concreteArr(arr: array[0, int]) = discard
proc genericArr[I, T](x: T, arr: array[I, T]) = discard
proc genericIndexArr[I](arr: array[I, int]) = discard
proc genericElemArr[T](x: T, arr: array[0, T]) = discard

concreteArr([])
genericArr(123, [])
genericIndexArr([])
genericElemArr(123, [])

proc concreteSet(arr: set[int8]) = discard
proc genericSet[T](x: T, arr: set[T]) = discard

concreteSet({})
genericSet(123'i8, {})

