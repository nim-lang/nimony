# `.noSideEffect` violations in a generic routine that do not depend on its
# type parameters are reported even when nothing instantiates it.

proc bump() = discard

var counter {.threadvar.}: int

func viaCall[T](x: T): T =
  bump()
  x

func viaGlobal[T](x: T): int =
  counter

func viaParam[T](x: T; f: proc (y: T) {.nimcall.}) =
  f(x)

proc explicit[T](x: T) {.noSideEffect.} =
  bump()

type Zappable = concept
  proc zap(x: Self)

func lateBound[T: Zappable](x: T) =
  # bound per instance: only an instance can tell
  zap(x)

func castAway[T](x: T) =
  {.cast(noSideEffect).}:
    bump()

proc sideEffecting[T](x: T) =
  bump()
