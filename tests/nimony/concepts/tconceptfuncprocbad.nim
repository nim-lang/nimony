type StrictEq = concept
  func `==`(x, y: Self): bool

type NeedsFuncEq = concept of StrictEq

type Box[T: NeedsFuncEq] = object
  v: T

var x: Box[float32]
