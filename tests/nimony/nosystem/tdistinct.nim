
type
  int* {.magic: Int.}
  bool* {.magic: Bool.}
  char* {.magic: Char.}

  int8* {.magic: Int8.}
  typedesc*[T] {.magic: TypeDesc.}
  untyped* {.magic: Expr.}

  TypeOfMode* = enum
    typeOfProc,
    typeOfIter

proc typeof(x: untyped; mode = typeOfIter): typedesc {.magic: TypeOf.}


type
  string* = typeof("")

proc `+`*(x, y: int): int {.magic: "AddI".}
proc `-`*(x, y: int): int {.magic: "SubI".}

proc `<=`*(x, y: int): bool {.magic: "LeI".}


type
  VarId* = distinct int

proc `+`*(x, y: VarId): VarId {.borrow.}
# Implementation: Generate a body for this proc in phase 3.
# The body is a single call of the current proc name, converted
# to the distinct type, if the return type is one. The type could also
# be generic, so generate the parmeter type properly. It also needs to
# skip modifiers first.

var x: VarId

x = x + x

let y = int8(x)
