type Foo = ref object
  x: int
  parent: Foo

var foo: Foo

type
  Node[T] = ref object
    val: T
    left, right: Node[T]

var node: Node[int]

type
  Forward1[T] = object
    x: ForwardNode1[T]
  ForwardNode1[T] = ref object
    val: T
    left, right: ForwardNode1[T]

var forwardNode1: ForwardNode1[int]

type
  Forward2 = object
    x: ForwardNode2[int]
  ForwardNode2[T] = ref object
    val: T
    left, right: ForwardNode2[T]

var forwardNode2: ForwardNode2[int]
