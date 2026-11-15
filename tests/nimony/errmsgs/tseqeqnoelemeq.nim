# `==` for seq and openArray needs `==` for the element type; the field-wise
# `==` for objects must not compare their buffers instead.
type D = distinct string

proc eqOpen(a, b: openArray[D]): bool = a == b

let b = @[D("x")]
let c = @[D("x")]
discard b == c
discard eqOpen(b, c)

proc eqGeneric[T](a, b: seq[T]): bool = a == b
