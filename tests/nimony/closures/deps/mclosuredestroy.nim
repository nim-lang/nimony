# The foreign half of tclosure_xmod_destroy: an object whose only non-trivial
# field is a closure. Its =destroy hook is generated HERE.
type
  Sector* = object
    label*: string
    onActivate*: proc(x, y: float32) {.closure, gcsafe.}

proc mkSector*(tag: string): Sector =
  result = Sector(label: tag, onActivate: proc(x, y: float32) {.closure, gcsafe.} = discard)
