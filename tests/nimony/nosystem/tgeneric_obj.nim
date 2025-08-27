
type
  int* {.magic: Int.}         ## Default integer type; bitwidth depends on
                              ## architecture, but is always the same as a pointer.
  float* {.magic: Float.}
  char* {.magic: Char.}
  uint8* {.magic: UInt8.}

  set*[T] {.magic: Set.}
  array* [Index, T] {.magic: Array.}
  typedesc*[T] {.magic: TypeDesc.}
  untyped* {.magic: Expr.}

  TypeOfMode* = enum
    typeOfProc,
    typeOfIter

proc typeof(x: untyped; mode = typeOfIter): typedesc {.magic: TypeOf.}

type
  string* = typeof("")

var myset: set[uint8]
var myarr: array[3, int]
let u8 = 3'u8
myset = {1'u8, u8, 5'u8, 7'u8..9'u8}
myarr = [1, 2, 3]

type
  MyGeneric[T] = object
    x: T

var
  myglob: MyGeneric[int]
  other: MyGeneric[float]

myglob.x = 56
other.x = 79.0

proc `+`*(x, y: int): int {.magic: "AddI".}

template foobar() =
  break # allowed in a template

proc foo(x: int; y: string): int =
  var x = "abc"
  result = 4
  x = "34"

proc overloaded() =
  let someInt = `+`(23, 90)
  discard foo(34+56, "xyz")

type
  HSlice*[T, U] = object
    a: T
    b: U
  Slice*[T] = HSlice[T, T]

proc `..`*[T, U](a: T, b: U): HSlice[T, U] = discard

# needs `..` to compile for now:
var myarr2: array[0..2, int] = myarr
myarr2 = [4, 5, 6]

var m3: array[4, int] = [1, 2, 3, 45]
var x3: array[4..7, int] = m3
x3 = [2, 3, 4, 5]

proc foo[I, T](x: array[I, T]) = discard
foo(myarr2)

proc identity[I, T](x: array[I, T]): array[I, T] =
  result = x
myarr2 = identity(myarr2)
