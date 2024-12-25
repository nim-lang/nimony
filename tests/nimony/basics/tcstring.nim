
type
  int* {.magic: Int.}
  bool* {.magic: Bool.}

  cstring* {.magic: Cstring.}   ## Built-in cstring (*compatible string*) type.
  pointer* {.magic: Pointer.}   ## Built-in pointer type, use the `addr`
                                ## operator to get a pointer to a variable.

proc `+`*(x, y: int): int {.magic: "AddI".}
proc `-`*(x, y: int): int {.magic: "SubI".}

proc `<=`*(x, y: int): bool {.magic: "LeI".}

var x = cast[int](55)

var y: pointer = nil

const
  myconst = cstring"abc"

var zz: cstring = "xzy"
