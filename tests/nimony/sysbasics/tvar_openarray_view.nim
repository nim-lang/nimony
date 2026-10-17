# issue #2665: an `openArray` variable passed to a `var openArray` parameter
import std/syncio

proc setIt(x: var openArray[int]; i: int; elem: int) =
  x[i] = elem

var data = [0, 0]
var view = toOpenArray(data)
view[0] = 1
setIt(view, 1, 2)
echo data[0], " ", data[1]

proc main =
  var d = [0]
  var v = toOpenArray(d)
  v[0] = 3
  setIt(v, 0, v[0] + 1)
  echo d[0]

main()
