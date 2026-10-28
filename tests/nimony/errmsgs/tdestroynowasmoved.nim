type
  Handle = object
    fd: int

proc `=destroy`(h: Handle) =
  discard h.fd
