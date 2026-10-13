# a non-sink `owned` parameter cannot be stored into an owned location
{.feature: "ownedRefs".}
type
  Node = ref object
    next: owned nil Node
    data: int

proc append(list: Node; n: owned Node) =
  list.next = n

append(Node(), Node())
