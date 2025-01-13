type
  Foo[T] = object
    x: ref Foo[T]

var foo: Foo[int]

type
  Node[T] = ref NodeObj[T]
  NodeObj[T] = object
    val: T
    left, right: Node[T]

var node: Node[int]
