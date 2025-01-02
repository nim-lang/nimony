proc generic[T](a, b: ptr T) =
  discard # a == b

proc main =
  var x: ptr int = nil
  x = nil
  var y: ref int = nil
  y = nil
  var z: cstring = nil
  z = nil
  var t: pointer = nil
  t = nil

  generic(x, nil)
  if x == nil: discard
