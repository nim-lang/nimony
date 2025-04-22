import mdistincts

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