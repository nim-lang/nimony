
import std / syncio

type
  Obj = object
    x, y: int

proc `=wasMoved`(x: var Obj) {.nodestroy, inline.} = zeroMem(addr x, sizeof(x))
proc `=destroy`(o: Obj) = echo "destroy"

proc main =
  var x = [Obj(x: 1, y: 2), Obj(x: 3, y: 4)]

main()
