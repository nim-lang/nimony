# Test: cases the static cycle-prevention check accepts.
# See doc/nocycles.md.

type
  Node = ref object
    next: Node
    data: int

  TreeNode = ref object
    left: TreeNode
    right: TreeNode
    parent {.cursor.}: TreeNode
    value: int

# Self-projection (one step): splice-out a successor.
proc removeNext(x: Node) =
  x.next = x.next.next

# Self-projection (multiple steps).
proc removeTwo(x: Node) =
  x.next = x.next.next.next

# Sibling re-pointing within the same receiver: both owning, both rooted
# at `t`. Cycle theorem says this is safe because nothing new is added to
# t's reachable set.
proc swapChildren(t: TreeNode) =
  t.left = t.right

# Deeper sibling-rooted projection.
proc liftLeft(t: TreeNode) =
  t.left = t.left.right

# Nil is always safe.
proc clearTail(x: Node) =
  x.next = nil

# Cursor field write — exempt from the rule entirely. Free choice of RHS.
proc setParent(n, p: TreeNode) =
  n.parent = p

# Cursor field can also be set across-root with no obligation.
proc reparent(a, b: TreeNode) =
  a.parent = b

# Result-rooted writes: result is fresh, so any RHS is OK.
proc buildLinked(): Node =
  new(result)
  new(result.next)
  result.data = 1
  result.next.data = 2

# Result-rooted write with a cross-root looking RHS — still accepted
# because the receiver is `result`.
proc attach(child: Node): Node =
  new(result)
  result.next = child

# `{.cast(uncheckedCycle).}:` block disables the cycle-prevention
# check inside its body. Used when the caller is responsible for
# disjointness (e.g. BST insert: caller guarantees the new node is
# not already in the tree).
proc bstInsert(root: Node, n: Node) =
  {.cast(uncheckedCycle).}:
    root.next = n


