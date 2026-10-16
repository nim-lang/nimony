# an `owned` parameter cannot be reassigned, unlike a `sink owned` one
{.feature: "ownedRefs".}
type
  Node = ref object
    data: int

proc reassign(n: owned nil Node) =
  n = nil

reassign(nil)
