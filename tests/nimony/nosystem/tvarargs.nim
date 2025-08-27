
type
  int* {.magic: Int.}
  bool* {.magic: Bool.}
  char* {.magic: Char.}
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
  File* = object

var stdout: File

proc write(f: var File; c: char) = discard
proc write(f: var File; s: string) = discard

write stdout, '\n'

iterator unpack(): untyped {.magic: Unpack.}

template echo*() {.varargs.} =
  for x in unpack():
    write stdout, x
  write stdout, '\n'

var someVar = ""
# 3:
echo "a", someVar, "b"
# 2:
echo "xzy", 'c'
# 0:
echo()
# 1:
echo someVar
