{.feature: "ownedRefs".}
type
  Node* = ref object
    next*: owned nil Node
    data*: int
    onChange*: owned nil proc () {.closure.}

proc len*(n: nil Node): int =
  result = 0
  var it = n
  while it != nil:
    inc result
    it = it.next
