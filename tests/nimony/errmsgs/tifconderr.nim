import std / [syncio]

type
  BinaryTree[T] = ref object
    le, ri: BinaryTree[T]
    data: T

proc newNode*[T](data: sink T): BinaryTree[T] = BinaryTree[T](data: data)

proc append*[Ty](root: var BinaryTree[Ty], n: BinaryTree[Ty]) =
  # insert a node into the tree
  if root == nil:
    root = n
  else:
    var it = root
    while it != nil:
      var c = compare(n.data, it.data)
      if c < 0:
        if it.le == nil:
          it.le = n
          return
        it = it.le
      else:
        if it.ri == nil:
          it.ri = n
          return
        it = it.ri
