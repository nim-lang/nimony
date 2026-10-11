# A local of a `try` body is destroyed when a raise leaves the body for the
# handler, wherever the `try` sits: at the top of a routine, nested in a
# block, or at module level.
import std/syncio

type
  Res = object
    id: int

proc `=destroy`(r: Res) =
  if r.id >= 0: echo "close ", r.id
proc `=wasMoved`(r: var Res) = r.id = -1
proc `=copy`(dest: var Res; src: Res) {.error.}

proc make(id: int): Res = Res(id: id)
proc mayFail(x: bool) {.raises.} =
  if x: raise IOError

proc viaCall() =
  try:
    let a = make(1)
    mayFail(true)
    echo "unreachable ", a.id
  except ErrorCode:
    echo "caught 1"

proc viaRaise() {.raises.} =
  try:
    let b = make(2)
    if b.id == 2: raise IOError
    echo "unreachable ", b.id
  except ErrorCode:
    echo "caught 2"

proc nested() =
  block:
    let outer = make(3)
    try:
      let inner = make(4)
      mayFail(true)
      echo "unreachable ", inner.id
    except ErrorCode:
      echo "caught 4"
    echo "outer alive ", outer.id

proc noRaise() =
  try:
    let c = make(5)
    mayFail(false)
    echo "kept ", c.id
  except ErrorCode:
    echo "unreachable"

viaCall()
try:
  viaRaise()
except ErrorCode:
  echo "unreachable"
nested()
noRaise()

try:
  let top = make(6)
  mayFail(true)
  echo "unreachable ", top.id
except ErrorCode:
  echo "caught 6"
