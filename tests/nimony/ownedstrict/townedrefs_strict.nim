# `-d:nimOwnedStrict` turns an unowned reference that outlives its owner into
# a diagnostic. Without it the reference keeps the object alive.
{.feature: "ownedRefs".}
{.feature: "assumeSync".}
import std/syncio
type Node = ref object
  next: owned nil Node
  data: int

var keep: seq[Node] = @[]
proc main =
  var root = Node(data: 1)
  keep.add root          # unowned counted copy
  echo root.data         # the owner is still used afterwards
main()                   # the owner dies, `keep[0]` outlives it
echo keep[0].data
