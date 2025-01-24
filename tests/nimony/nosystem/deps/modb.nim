
type
  int* {.magic: Int.}

proc use*(x: int) = discard

type
  char* {.magic: Char.}
  UncheckedArray* {.magic: UncheckedArray.}
  string* = object
    a: ptr UncheckedArray[char]
    i: int

proc useOverloaded*(x: string) = discard
proc useOverloaded*(x: int) = discard
