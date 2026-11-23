# `system.move` resets its source to `default(T)`; `=wasMoved` only has to
# leave a state that `=destroy` accepts, and is a no-op for types without hooks.

import std/syncio

type
  Watermark = object
    len: int    # `=wasMoved` resets only this
    tag: int    # left alone by `=wasMoved`, reset by `move`
    data: seq[int]

  Small = range[3..9]

proc `=destroy`(x: Watermark) =
  `=destroy`(x.data)

proc `=wasMoved`(x: var Watermark) =
  x.len = 0
  `=wasMoved`(x.data)


proc main =
  var i = 5
  let j = move(i)
  echo j, " ", i

  var k = 7
  `=wasMoved`(k)
  echo k

  var s = @[1, 2, 3]
  let t = move(s)
  echo t.len, " ", s.len

  var str = "abc"
  let str2 = move(str)
  echo str2, " [", str, "]"

  var r: Small = 7
  let r2 = move(r)
  echo r2, " ", r

  var w = Watermark(len: 2, tag: 42, data: @[1, 2])
  `=wasMoved`(w)
  echo w.len, " ", w.tag, " ", w.data.len

  var v = Watermark(len: 2, tag: 42, data: @[1, 2])
  let v2 = move(v)
  echo v2.len, " ", v2.tag, " ", v2.data.len
  echo v.len, " ", v.tag, " ", v.data.len

main()
