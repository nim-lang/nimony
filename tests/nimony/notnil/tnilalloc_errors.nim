# Asking to be handed the `nil` is also asking to be held to it: a `nil T` binding
# has to be narrowed before it can be dereferenced, or passed where a not-nil `T`
# is wanted. See `tnilalloc.nim` for the forms that do.
type
  Node = ref object
    val: int

  Holder = object
    n: Node

proc use(n: Node): int = n.val

proc unnarrowedDeref(): int =
  let p: nil Node = Node(val: 1)
  result = p.val

proc unnarrowedArgument(): int =
  let p: nil Node = Node(val: 2)
  result = use(p)

proc unnarrowedField(): int =
  let p: nil Node = Node(val: 3)
  let h = Holder(n: p)
  result = h.n.val

proc narrowedOnlyInTheOtherBranch(): int =
  let p: nil Node = Node(val: 4)
  if p == nil:
    result = p.val
  else:
    result = 0

discard unnarrowedDeref()
discard unnarrowedArgument()
discard unnarrowedField()
discard narrowedOnlyInTheOtherBranch()
