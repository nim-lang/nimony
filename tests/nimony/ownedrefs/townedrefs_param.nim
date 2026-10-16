# RFC #575: an `owned` parameter is borrowed; only `sink owned` transfers
{.feature: "ownedRefs".}
import std/syncio

type
  Node = ref object
    next: owned nil Node
    data: int

proc rename(n: owned Node; data: int) =
  n.data = data
  let u: Node = n
  echo u.data

proc append(list: Node; n: sink owned Node) =
  list.next = n

proc main =
  let a = Node(data: 1)
  rename(a, 2)
  echo a.data
  rename(Node(data: 3), 4)
  let b = Node(data: 5)
  append(a, b)
  let nx: nil Node = a.next
  if nx != nil: echo nx.data

main()
