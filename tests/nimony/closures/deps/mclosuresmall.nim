# The foreign half of tclosure_xmod_destroy_small: objects whose closure field
# makes them exactly three words once lowered. Their =destroy hooks are
# generated HERE.
type
  Entry* = object
    a*: uint64
    fn*: proc (): int {.closure.}

  Node* = ref object
    val*: int

  WithRef* = object
    node*: Node
    fn*: proc (): int {.closure.}

  Outer* = object
    inner*: Entry
    tag*: int

proc mkEntry*(a: uint64; r: int): Entry =
  Entry(a: a, fn: proc (): int {.closure.} = r)

proc mkWithRef*(v: int): WithRef =
  WithRef(node: Node(val: v), fn: proc (): int {.closure.} = v)

proc mkOuter*(a: uint64; r: int): Outer =
  Outer(inner: mkEntry(a, r), tag: r)
