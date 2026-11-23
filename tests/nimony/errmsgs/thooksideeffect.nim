# A hook is `.noSideEffect` by default, `proc` or not; one that really has
# side effects says `.sideEffect`.

var destroyed: int

type
  Handle = object
    fd: int

proc `=destroy`(h: Handle) =
  inc destroyed

proc `=wasMoved`(h: var Handle) = h.fd = -1

type
  Logged = object
    fd: int

proc `=destroy`(h: Logged) {.sideEffect.} =
  {.cast(assumeSync).}:
    inc destroyed

proc `=wasMoved`(h: var Logged) = h.fd = -1
