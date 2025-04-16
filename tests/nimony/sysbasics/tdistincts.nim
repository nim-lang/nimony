type
  VarId* = distinct int

proc `+`*(x, y: VarId): VarId {.borrow.}

var x: VarId

x = x + x

let y = int8(x)


type
  WINBOOL* = distinct int32

proc `==`(x, y: WINBOOL): bool {.borrow.}