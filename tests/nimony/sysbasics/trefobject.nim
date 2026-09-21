# Every self-link here is a `nil ref`: a recursive ref field that cannot be nil
# describes an infinite chain, so neither the type nor the globals below would
# have a value to start from.
type Foo = ref object
  x: int
  parent: nil Foo

var foo: nil Foo

type
  Node[T] = nil ref NodeObj[T]
  NodeObj[T] = object
    val: T
    left, right: Node[T]

var node: Node[int]

type
  Forward1[T] = object
    x: ForwardNode1[T]
  ForwardNode1[T] = nil ref ForwardNode1Obj[T]
  ForwardNode1Obj[T] = object
    val: T
    left, right: ForwardNode1[T]

var forwardNode1: ForwardNode1[int]

type
  Forward2 = object
    x: ForwardNode2[int]
  ForwardNode2[T] = nil ref ForwardNode2Obj[T]
  ForwardNode2Obj[T] = object
    val: T
    left, right: ForwardNode2[T]

var forwardNode2: ForwardNode2[int]
