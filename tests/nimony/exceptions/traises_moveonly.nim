# A `.raises` routine returning a move-only type: its `raise` hands back
# `result` itself, so no `=dup` is needed, and the partially built result
# is destroyed exactly once on the failure path.
import std/syncio

type
  Res = object
    fd: int

proc `=destroy`(r: Res) =
  if r.fd >= 0: echo "close ", r.fd
proc `=wasMoved`(r: var Res) = r.fd = -1
proc `=copy`(dest: var Res; src: Res) {.error.}

proc make(fail: bool): Res {.raises.} =
  result = Res(fd: 3)
  if fail: raise IOError

proc inner() {.raises.} =
  let a = make(false)
  echo "got ", a.fd

proc inner2() {.raises.} =
  let b = make(true)
  echo "unreachable ", b.fd

try:
  inner()
  inner2()
except ErrorCode:
  echo "raised"
