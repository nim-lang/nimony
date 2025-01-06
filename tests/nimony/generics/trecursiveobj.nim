type
  Foo[T] = object
    x: ref Foo[T]

var foo: Foo[int]

type
  NodeObj[T] = object
    val: T
    left, right: Node[T]
  Node[T] = ref NodeObj[T]

var node: Node[int]
