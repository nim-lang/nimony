type StrictEq = concept
  func `==`(x, y: Self): bool

type NeedsFuncEq = concept of StrictEq

type Inner = object
  v: int

type BadType = distinct Inner

proc `==`*(x, y: BadType): bool {.sideEffect.} =
  false

type Box[T: NeedsFuncEq] = object
  v: T

var x: Box[BadType]
