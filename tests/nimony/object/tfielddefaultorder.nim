# A field default may use any routine, constant or type visible where the
# type is declared (tfielddefaultcalls.nim), but a default that constructs
# its own type, directly or through another type's defaults, has no value,
# and a `const` cannot construct a type whose section comes later. A field
# without a written type takes its type from a literal or constant default.
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
    s = @[1]
    k = laterK # a constant declared before the section would do
    m = -K
  A = object
    b: B = B()

const laterK = 3
const C = A()

type
  B = object
    k: int = 2 + 3

proc local() =
  let x = 4
  type L = object
    v = x
    w = @[1]
  discard L()

discard Node()
discard NoType()
