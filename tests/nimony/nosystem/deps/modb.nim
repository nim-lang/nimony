
type
  int* {.magic: Int.}

proc use*(x: int) = discard

type
  char* {.magic: Char.}
  typedesc*[T] {.magic: TypeDesc.}
  untyped* {.magic: Expr.}

  TypeOfMode* = enum
    typeOfProc,
    typeOfIter

proc typeof(x: untyped; mode = typeOfIter): typedesc {.magic: TypeOf.}

type
  string* = typeof("")

proc useOverloaded*(x: string) = discard
proc useOverloaded*(x: int) = discard
