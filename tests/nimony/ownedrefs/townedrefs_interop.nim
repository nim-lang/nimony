# A module without `.feature: "ownedRefs"` can still build values whose
# types use `owned`: fresh values may initialize an owned location.
import std/syncio
import deps / mownedtypes

proc main =
  var head = Node(data: 1)
  head.next = Node(data: 2)
  let second: nil Node = head.next   # owned -> unowned
  if second != nil:
    new(second.next)
  head.onChange = proc () {.closure.} = echo "changed"
  let cb: nil proc () {.closure.} = head.onChange
  if cb != nil: cb()
  echo len(head)
  head.next = nil
  if second != nil:
    echo second.data, " ", len(head), " ", len(second)

main()
