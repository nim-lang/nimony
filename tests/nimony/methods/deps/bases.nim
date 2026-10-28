
import std/syncio

type
  CustomBase* {.inheritable.} = object

proc `=wasMoved`(x: var CustomBase) {.nodestroy, inline.} = zeroMem(addr x, sizeof(x))
method `=destroy`(b: CustomBase) = discard
method `=trace`(b: var CustomBase; p: pointer) = discard

method reportIdentity*(b: CustomBase) =
  echo "CustomBase"
