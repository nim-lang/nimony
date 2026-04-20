proc generic[T](a, b: nil ptr T) =
  discard a == nil

proc main =
  var x: nil ptr int = nil
  x = nil
  var y: nil ref int = nil
  y = nil
  var z: nil cstring = nil
  z = nil
  var t: nil pointer = nil
  t = nil

  generic(x, nil)
  if x == nil: discard


let x: nil ref int = nil
if x != nil:
  let y = x[]

var s = true
case s
of false:
  if x != nil:
    let y = x[]
else:
  let y = x

var weg: nil ptr int

if weg == nil:
  discard "X is nil!"

if nil == weg: # Crashes the compiler
  discard "X is nil!"
