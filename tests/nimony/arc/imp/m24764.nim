type
  QObject* {.inheritable.} = object

proc `=wasMoved`*(x: var QObject) {.nodestroy, inline.} = zeroMem(addr x, sizeof(x))
proc `=destroy`*(self: var QObject) =
  discard

proc `=sink`*(dest: var QObject, source: QObject) =
  discard

proc `=copy`*(dest: var QObject, source: QObject) {.error.}
