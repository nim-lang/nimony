# a value type with an `owned` field is move-only
{.feature: "ownedRefs".}
import std/syncio
type
  Node = ref object
    data: int
  Holder = object
    n: owned Node
proc main =
  var h = Holder(n: Node(data: 1))
  var h2 = h
  echo h.n.data, h2.n.data
main()
