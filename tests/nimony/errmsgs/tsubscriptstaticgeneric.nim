# nim-lang/nimony#2191: a subscript on an erroneous operand inside a generic
# routine must report every error, not crash.
type Matrix[W, H: static[int], T] = object
  data: array[W * H, T]

proc `[]`(m: Matrix[W, H: static[int], T], x, y: int): T {.inline.} =
  m.data[x*H + y]
