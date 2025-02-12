type
  int* {.magic: Int.}

  range*[T]{.magic: Range.}
  set*[T] {.magic: Set.}
  array* [Index, T] {.magic: Array.}

var a: array[0..2, int]
var b: set[range[0..2]]
