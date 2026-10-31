import mcyc4_dns {.cyclic.}

type
  Adapter* = ref object of RootObj
  Writer* = object
    adapter*: nil Adapter
    n*: int

proc `=destroy`*(w: Writer) =
  `=destroy`(w.adapter)

proc newWriter*(): Writer = Writer(n: 1)

proc `=wasMoved`*(w: var Writer) {.nodestroy.} =
  w.n = 0
