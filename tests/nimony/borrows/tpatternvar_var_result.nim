# Returning a field bound by a hidden sum type's `case` pattern as `var T`
# (#3008).
import std/[assertions, syncio]

type
  Node = ref NodeObj
  NodeObj = object
    fallback: int
    case
    of Leaf:
      val: int
    of Branch:
      child: array[2, nil Node]

  Box[T] = ref BoxObj[T]
  BoxObj[T] = object
    fallback: T
    case
    of GLeaf:
      val: T
    of GBranch:
      child: array[2, nil Box[T]]

proc getNode(n: Node): Node = n

proc value(n: Node): var int =
  case n
  of Leaf(val):
    return val
  of Branch:
    return n.fallback

proc assignValue(n: Node): var int =
  case n
  of Leaf(val):
    result = val
  of Branch:
    result = n.fallback

proc boxValue[T](b: Box[T]): var T =
  case b
  of GLeaf(val):
    return val
  of GBranch:
    return b.fallback

var leaf: Node = Leaf(val: 41)
value(leaf) = 42
assert value(leaf) == 42
assignValue(leaf) = 43
assert assignValue(leaf) == 43
value(getNode(leaf)) = 44
assert value(leaf) == 44
echo "leaf ", value(leaf)

var branch: Node = Branch(fallback: 7, child: [nil, nil])
value(branch) = 8
assert assignValue(branch) == 8
echo "branch ", value(branch)

var box: Box[int] = GLeaf(fallback: 0, val: 50)
boxValue(box) = 51
assert boxValue(box) == 51
echo "generic ", boxValue(box)
