proc concreteArr(arr: array[0, int]) = discard
proc genericArr[I, T](x: T, arr: array[I, T]) = discard
proc genericIndexArr[I](arr: array[I, int]) = discard
proc genericElemArr[T](x: T, arr: array[0, T]) = discard

concreteArr([])
genericArr(123, [])
genericIndexArr([])
genericElemArr(123, [])

type Enum = enum a, b, c
proc concreteSet(arr: set[Enum]) = discard
proc genericSet[T](x: set[T], arr: set[T]) = discard

concreteSet({})
var x: set[Enum] = {}
genericSet(x, {})

proc concreteRangeSet(arr: set[range[0..3]]) = discard

concreteRangeSet({})
var rangeSet: set[range[0..3]] = {}
genericSet(rangeSet, {})
type RangeAlias = range[0..3]
var aliasRangeSet: set[RangeAlias] = {}
genericSet(rangeSet, aliasRangeSet)

