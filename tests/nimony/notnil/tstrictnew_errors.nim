{.feature: "strictnew".}
# The other side of `tstrictnew`: outside a `.raises` routine an allocation's
# result is nilable, so these uses of it are the ones tier 3 rejects.
type
  Node = ref object
    val: int

  Tree = ref object
    case
    of Pair:
      a, b: Tree
    of Leaf:
      n: int

proc use(n: Node): int = n.val

proc unnarrowedDeref(): int =
  let p = Node(val: 1)
  result = p.val

proc inlineArgument(): int =
  result = use(Node(val: 2))

proc declaredNotNilResult(): Node =
  result = Node(val: 3)

proc nestedSumType(): Tree =
  result = Pair(a: Leaf(n: 4), b: Leaf(n: 5))

discard unnarrowedDeref()
discard inlineArgument()
discard declaredNotNilResult()
discard nestedSumType()
