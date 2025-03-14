
import std / [syncio]

type
  BinaryTree = ref object
    le, ri: BinaryTree
    data: string

proc newNode*(data: sink string): BinaryTree = BinaryTree(data: data)

proc append*(root: var BinaryTree; n: BinaryTree) =
  # insert a node into the tree
  if root == nil:
    root = n
  else:
    var it = root
    while it != nil:
      var c = cmp(n.data, it.data)
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

proc append*(root: var BinaryTree, data: sink string) =
  append(root, newNode(data))

proc toString(n: BinaryTree; result: var string) =
  if n == nil: return
  result.add n.data
  toString n.le, result
  toString n.ri, result

proc `$`*(n: BinaryTree): string =
  result = ""
  toString n, result

proc main =
  var x = newNode("abc")
  x.append "def"
  echo $x

main()
