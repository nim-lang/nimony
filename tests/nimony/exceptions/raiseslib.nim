# Helper for `traisesproctype`: the same shapes, but declared in ANOTHER
# module, which is the case `lengcgen` still has to answer for on its own —
# a foreign type declaration is pulled in as a type, never as code, so no
# hexer pass ever walks it.

type ForeignFn* = proc (x: int): int {.raises.}

proc foreignVoid*(x: int) {.raises.} =
  if x < 0: raise SyntaxError

proc foreignInt*(x: int): int {.raises.} =
  if x < 0: raise SyntaxError
  result = x * 3
