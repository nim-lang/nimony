# copying an `owned` location is an error unless it is the last read
{.feature: "ownedRefs".}
import std/syncio
type Node = ref object
  next: owned nil Node
  data: int

proc main =
  let a = Node(data: 1)
  let b = a
  echo a.data, b.data
main()
