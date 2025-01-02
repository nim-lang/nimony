proc main =
  var x: ptr int
  if false:
    discard x[]
    x[] = 123
