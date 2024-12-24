

type
  int* {.magic: Int.}         ## Default integer type; bitwidth depends on
                              ## architecture, but is always the same as a pointer.
  float* {.magic: Float.}
  string* {.magic: String.}

  Color* = enum
    red = 0, blue = 1

  bool* {.magic: Bool.} = enum ## Built-in boolean type.
    false = 0, true = 1

  Student* = object
    id: int
    name: string

  Data* = tuple[a: int, b: string]

proc `+`*(x, y: int): int {.magic: "AddI".}

proc foo(x: int; y: string): int =
  var x = "abc"
  result = 4

proc overloaded() =
  let someInt = `+`(23, 90)
  discard foo(34+56, "xyz")

proc overloaded(s: string) =
  discard "with string parameter"

type
  MyObject = object
    x, y: int

var global: MyObject

global.x = 45
discard foo(global.x, "123")
discard global.x.foo("123")
global = MyObject(x: 123)
global = MyObject(x: 123, y: 456)

overloaded()
overloaded("abc")
"abc".overloaded
"abc".overloaded()

proc discardable(x: int): int {.discardable.} =
  result = x + 7

discardable(123)
discard discardable(123)

proc foo_block* =
  var x = 12
  x = 3

  block lab:
    var s = 12
    break lab

  block:
    var s = 13
    break

  block lab:
    var s = 14
    break lab

  block late:
    block lab:
      var s = 12
      break lab
    block lab2:
      break late

proc testPragmaInline*() {.inline.} =
  let data = 1

proc createData(): tuple[a: int, b: string] =
  var s: tuple[a: int, b: string]
  result = s

proc `==`*(x, y: int): bool {.magic: "EqI".}

proc whileStmt =
  while 1 == 2:
    var s = 12
    break

proc ifExpr(): int =
  let x =
    if 1 == 1:
      123
    else:
      456
  let y =
    if 1 == 2:
      "abc"
    elif 1 == 3:
      return
    else:
      "def"
  result =
    if 0 == 1:
      x
    else:
      789

var x = [1, 2, 3]
let u8 = 3'u8
let y = {1'u8, u8, 5'u8, 7'u8..9'u8}
var z = (1, "abc")
z = (2, "def")
var t: tuple[x: int, y: string] = (1, "abc")
t = (x: 2, y: "def")
let tx: int = t.x
let ty: string = t.y

var mt: Data = (1, "abc")
let mta: int = mt.a
let mtb: string = mt.b

const Inf* = 0x7FF0000000000000'f64

let s: float = Inf

proc `[]`*[I;T](a: T; i: I): T {.magic: "ArrGet".}

proc foo2 =
  var x = [1, 2, 3]
  let m = x[1]
