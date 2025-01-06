type int* {.magic: Int.}

type
  NodeObj[T] = object
    data: T
    left, right: ref NodeObj[T]
  Node[T] = ref NodeObj[T]

var x = Node[int](data: 123, left: nil, right: nil)
x.data = 456
x.left = Node[int](data: 123, left: nil, right: nil)
x.right = nil
x.left.right = nil
var y = Node[int](data: 987, left: nil, right: nil)
x.left.left = Node[int](data: -123, left: y, right: y)

proc foo[T](data: T): Node[T] =
  result = Node[T](data: data, left: nil, right: nil)
  result.data = data
let a = foo(123)
x = a
