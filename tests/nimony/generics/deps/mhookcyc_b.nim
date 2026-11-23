import std/syncio
import mhookcyc_a {.cyclic.}

type
  CycBox[T] = object
    item: T
  CycHolder*[T] = object
    v*: T

proc mk[T](x: T): CycBox[T] = CycBox[T](item: x)

proc `=destroy`*[T](h: CycHolder[T]) =
  let b = mk(h.v) # the first use of `CycBox[int]`
  echo "cyclic destroyed"
proc `=wasMoved`*[T](h: var CycHolder[T]) = discard
