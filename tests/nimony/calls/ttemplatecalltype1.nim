import std/assertions

template foo[T](): T = 123

let x = foo[float]()
let y: float = x
assert y == 123.0
