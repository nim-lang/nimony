type int* {.magic: Int.}

type Foo = ref object
  x: int
  parent: Foo

var foo: Foo

type Node[T] = ref object
  val: T
  left, right: Node[T]

var node: Node[int]
