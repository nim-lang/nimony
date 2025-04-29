import mdistincts

import std/assertions

type
  VarId* = distinct int

proc `+`*(x, y: VarId): VarId {.borrow.}

proc foobar(x: var int, y: var int): var int =
  x = y
  result = x

proc foobar(x: var VarId, y: var VarId): var VarId {.borrow.}

var x: VarId

x = x + x

let m = foobar(x, x)

let y = int8(x)


type
  WINBOOL* = distinct int32

proc `==`(x, y: WINBOOL): bool {.borrow.}

discard isFail(DInt(0))


type
  Bytes = distinct seq[int]

proc add(x: var Bytes; b: int) {.borrow.}

var x2 = Bytes(@[99])
x2.add(42)
let base = (seq[int])(x2)
assert base.len == 2 and base[1] == 42
