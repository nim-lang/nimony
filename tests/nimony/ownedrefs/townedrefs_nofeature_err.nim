# without `.feature: "ownedRefs"` an object construction bound to a name is an
# ordinary reference, and shared cannot be upgraded to unique
type Node = ref object
  next: owned nil Node
let y = Node()
var x: owned Node = Node()
x = y
