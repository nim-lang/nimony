# without the feature `owned` is erased
import std/syncio
type Node = ref object
  next: owned nil Node
var x: owned[Node] = Node()
var y = Node()
x = y
var z: owned(Node) = x
let w = z
echo w == z
proc ident[T](x: owned T): owned T = x
echo ident(3)
