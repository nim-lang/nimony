import std / syncio

proc p[T: object](x: T) = echo "object"
proc q[T: object](x: T) = echo "object"

type
  G[T] = object

proc p[T](g: G[T]) = echo "G of T"

proc main =
  var g = G[string]()
  p g
  q g

main()
