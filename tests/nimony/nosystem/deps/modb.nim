
type
  int* {.magic: Int.}

proc use*(x: int) = discard

type
  char* {.magic: Char.}
  typedesc*[T] {.magic: TypeDesc.}

proc typeof*[T](x: T): typedesc[T] {.magic: TypeOf.}

type
  string* = typeof("")

proc useOverloaded*(x: string) = discard
proc useOverloaded*(x: int) = discard
