import std/syncio

type
  Handle* = object
    fd: int

proc `=destroy`*(h: Handle) =
  if h.fd >= 0: echo "close ", h.fd
proc `=wasMoved`*(h: var Handle) = h.fd = -1
proc `=copy`*(dst: var Handle; src: Handle) {.error.}
proc `=dup`*(src: Handle): Handle {.error.}

proc open*(fd: int): Handle = Handle(fd: fd)

proc handles*(fd: int): seq[Handle] =
  result = @[]
  result.add Handle(fd: fd)
