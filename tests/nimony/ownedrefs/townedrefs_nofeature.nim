# `owned` is meaningful in a module without `.feature: "ownedRefs"`, too:
# writing it is the opt-in. Only object constructions stay unowned here.
import std/syncio
type Node = ref object
  next: owned nil Node
  data: int
var x: owned[Node] = Node(data: 1)  # fresh, so it may become the owner
let y = Node(data: 2)               # an ordinary counted reference
let z: Node = x                     # owned -> unowned
let w = y                           # a copy of an unowned reference
echo y.data, " ", z.data, " ", w == y
