# a move-only type and a `seq` of it, both from this module (#3044)
type
  Handle* = object
    fd: int

proc `=destroy`*(h: Handle) = discard
proc `=wasMoved`*(h: var Handle) = h.fd = -1
proc `=copy`*(dst: var Handle; src: Handle) {.error.}
proc `=dup`*(src: Handle): Handle {.error.}

proc handles*(fd: int): seq[Handle] =
  result = @[]
  result.add Handle(fd: fd)
