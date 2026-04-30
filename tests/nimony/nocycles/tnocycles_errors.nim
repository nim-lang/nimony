# Test: cases the static cycle-prevention check rejects.
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

# Cross-root assignment: receiver `a` and RHS `b` are different bindings,
# neither is fresh nor projection-rooted at `a`.
proc badCrossRoot(a, b: Node) =
  a.next = b

# RHS rooted at the wrong receiver: receiver is `child`, RHS is rooted at
# `grandparent` — different paths even though both are owning.
proc badDifferentRoots(child, grandparent: Node) =
  child.next = grandparent.next

# RHS extension goes through a `.cursor` field. Cursor traversal can leave
# the owning subgraph, so the projection rule rejects it.
proc badThroughCursor(x: TreeNode) =
  x.left = x.parent.left
