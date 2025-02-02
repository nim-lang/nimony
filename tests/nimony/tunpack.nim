
type
  int* {.magic: Int.}
  bool* {.magic: Bool.}

  cstring* {.magic: Cstring.}   ## Built-in cstring (*compatible string*) type.
  pointer* {.magic: Pointer.}   ## Built-in pointer type, use the `addr`
                                ## operator to get a pointer to a variable.
  char* {.magic: Char.}

  typedesc*[T] {.magic: TypeDesc.}

proc typeof*[T](x: T): typedesc[T] {.magic: TypeOf.}

type
  string* = typeof("")

proc `+`*(x, y: int): int {.magic: "AddI".}
proc `-`*(x, y: int): int {.magic: "SubI".}

proc `<=`*(x, y: int): bool {.magic: "LeI".}

var x = cast[int](55)

var y: pointer = nil

const
  myconst = cstring"abc"

var zz: cstring = "xzy"

proc `$`*(s: string): string
proc `$`*(s: int): string
proc `$`*(s: bool): string

type
  untyped* {.magic: Expr.}

iterator unpack(): untyped {.magic: Unpack.}
proc unpackToCall(fn: untyped) {.magic: Unpack.}

template toArray(): untyped {.varargs.} = [unpack()]
template toTuple(): untyped {.varargs.} = (unpack(),)

template toStringArray(): untyped {.varargs.} = [unpackToCall(`$`)]

proc use =
  let a = toArray(1, 2, 3)

  let t = toTuple("a", 2)

  let emptyTuple = toTuple()

  let s = toStringArray("a", 1, "b")

use()
