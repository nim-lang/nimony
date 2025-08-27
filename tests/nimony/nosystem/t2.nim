
type
  int* {.magic: Int.}         ## Default integer type; bitwidth depends on
                              ## architecture, but is always the same as a pointer.
  float* {.magic: Float.}
  typedesc*[T] {.magic: TypeDesc.}
  untyped* {.magic: Expr.}

  TypeOfMode* = enum
    typeOfProc,
    typeOfIter

proc typeof(x: untyped; mode = typeOfIter): typedesc {.magic: TypeOf.}

type
  string* = typeof("")


proc `+`*(x, y: int): int {.magic: "AddI".}

proc foo(x: int; y: string): int =
  var x = "abc"
  result = 4
  x = "34"

proc overloaded() =
  let someInt = `+`(23, 90)
  discard foo(34+56, "xyz")

proc exprReturn(x: int): int = x
