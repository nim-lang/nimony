# shared cannot be upgraded to unique
{.feature: "ownedRefs".}
type Node = ref object
  next: owned nil Node
proc main(u: Node) =
  let a = Node()
  a.next = u
main(Node())
