# A hook written as a `func` is `.noSideEffect`; one that really has
# side effects is a `proc`.

var destroyed: int

type
  Handle = object
    fd: int

func `=destroy`(h: Handle) =
  inc destroyed

proc `=wasMoved`(h: var Handle) = h.fd = -1

type
  Logged = object
    fd: int

proc `=destroy`(h: Logged) {.sideEffect.} =
  {.cast(assumeSync).}:
    inc destroyed

proc `=wasMoved`(h: var Logged) = h.fd = -1
