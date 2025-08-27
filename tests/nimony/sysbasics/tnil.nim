proc generic[T](a, b: ptr T) =
  discard a == nil

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


let x: ref int = nil
if false:
  let y = x[]

var s = true
case s
of false:
  let y = x[]
else:
  let y = x

var weg: ptr int

if weg == nil:
  discard "X is nil!"

if nil == weg: # Crashes the compiler
  discard "X is nil!"
