type
  set*[T] {.magic: Set.}
  array*[Index, T] {.magic: Array.}

type NormalEnum = enum
  e0, e1, e2

var normalSet: set[NormalEnum]
var normalArray: array[NormalEnum, NormalEnum]

type HoleyEnum = enum
  a = 0, b = 1, c = 4

var x: set[HoleyEnum] # works
var y: array[HoleyEnum, HoleyEnum]
