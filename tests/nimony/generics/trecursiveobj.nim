# The links are `nil ref`: a recursive ref field that cannot be nil has no
# value at all -- building one would mean building an infinite chain -- so the
# globals below could not be default-initialized either.
type
  Foo[T] = object
    x: nil ref Foo[T]

var foo: Foo[int]

type
  Node[T] = nil ref NodeObj[T]
  NodeObj[T] = object
    val: T
    left, right: Node[T]

var node: Node[int]
