# Unpacking a tuple of move-only values moves each field out of the temp —
# also when the tuple comes from a `.raises` call, whose `(ErrorCode, (R, W))`
# result makes the field reads nested: `tmp[1][0]`, `tmp[1][1]`.
import std/syncio

type
  R = object
    x: int
  W = object
    y: int

proc `=destroy`(r: R) = echo "destroy R ", r.x
proc `=wasMoved`(r: var R) = r.x = 0
proc `=dup`(r: R): R {.error.}
proc `=copy`(d: var R; s: R) {.error.}
proc `=destroy`(w: W) = echo "destroy W ", w.y
proc `=wasMoved`(w: var W) = w.y = 0
proc `=dup`(w: W): W {.error.}
proc `=copy`(d: var W; s: W) {.error.}

proc mk(): (R, W) = (R(x: 1), W(y: 2))

proc mkRaises(): (R, W) {.raises.} =
  result = (R(x: 3), W(y: 4))

proc main() {.raises.} =
  var (r, w) = mk()
  echo r.x, " ", w.y
  let (r2, w2) = mkRaises()
  echo r2.x, " ", w2.y

try:
  main()
except:
  echo "failed"
