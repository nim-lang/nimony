proc main =
  var x: ptr int = nil
  if false:
    discard x[]
    x[] = 123

proc bar(x: int) =
  var m = x

proc foo(x: var int) =
  let s = x
  bar(s)
  bar(x)

var m = 12
foo(m)
