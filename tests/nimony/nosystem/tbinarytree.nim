type int* {.magic: Int.}

type
  NodeObj = object
    data: int
    left, right: Node
  Node = ref NodeObj

var x = Node(data: 123, left: nil, right: nil)
x.data = 456
x.left = Node(data: 123, left: nil, right: nil)
x.right = nil
x.left.right = nil
var y = Node(data: 987, left: nil, right: nil)
x.left.left = (ref NodeObj)(data: -123, left: y, right: y)
