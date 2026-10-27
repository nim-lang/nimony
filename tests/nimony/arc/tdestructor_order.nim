import std/syncio
{.feature: "lenientnils".}
type
  Aaaa {.inheritable.} = object
    vvvv: int
  Bbbb = object of Aaaa
    c: Cccc
    d: Dddd
  Cccc = object
  Dddd = object

  Holder = object
    member: ref Aaaa

proc `=wasMoved`(x: var Cccc) {.nodestroy, inline.} = zeroMem(addr x, sizeof(x))
proc `=destroy`(v: Cccc) =
  echo "destroying c"

proc `=wasMoved`(x: var Dddd) {.nodestroy, inline.} = zeroMem(addr x, sizeof(x))
proc `=destroy`(v: Dddd) =
  echo "destroying d"

proc `=wasMoved`(x: var Aaaa) {.nodestroy, inline.} = zeroMem(addr x, sizeof(x))
proc `=destroy`(v: Aaaa) =
  echo "destroying a ", v.vvvv

func makeHolder(vvvv: int): ref Holder =
  (ref Holder)(member: (ref Bbbb)(vvvv: vvvv))

block:
  var v = makeHolder(1)
  var v2 = makeHolder(2)
