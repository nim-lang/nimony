proc getTup: tuple[x: var int] =
  var a = 100
  result = (a, )
  result = (x: a)

getTup().x = 300

block:
  var counter = 0
  proc incr() = inc counter
  incr()

block:
  # the error sits in a callee whose type the lowering needs: `yield` binds
  # the call's value to a temporary
  type Box = object
    f: proc (y: int): int {.closure.}
  proc u(a: int) =
    var b = Box(f: proc (y: int): int {.closure.} = y)
    iterator li(): int {.sideEffect.} =
      yield b.f(a)
    for x in li(): discard x
  u(3)
