
import std / [syncio]

type
  BinaryTree[T] = ref object
    le, ri: nil BinaryTree[T]
    data: T

proc newNode*[T](data: sink T): BinaryTree[T] = BinaryTree[T](data: data)

proc append*[Ty: Comparable](root: var nil BinaryTree[Ty], n: BinaryTree[Ty]) =
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

proc append*[Ty](root: var BinaryTree[Ty], data: sink Ty) =
  append(root, newNode(data))

type
  Stringable = concept
    proc `$`(x: Self): string

proc toString[T: Stringable](n: nil BinaryTree[T]; result: var string) =
  if n == nil: return
  result.add $n.data
  toString n.le, result
  toString n.ri, result

proc `$`*[T](n: BinaryTree[T]): string =
  result = ""
  toString n, result

proc main =
  var x = newNode("abc")
  x.append "def"
  echo $x

main()

