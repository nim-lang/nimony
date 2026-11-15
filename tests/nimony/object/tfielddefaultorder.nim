# A field with a default value always has a written type. A default that
# constructs its own type, directly or through another type's defaults, has
# no value, and a `const` cannot construct a type whose section comes later.
const K = 3
type
  Node = ref object
    v: int
    next: Node = Node(v: 1)
  P = ref object
    q: Q = Q()
  Q = object
    p: P = P()
  NoType = object
    n = 3
    k = K
    s = @[1]
  G[T] = object
    x = 3
    y: T
  A = object
    b: B = B()

const C = A()

type
  B = object
    k: int = 2 + 3

proc local() =
  type L = object
    v = 'c'
  discard L()

discard Node()
discard NoType()
discard G[int]()
