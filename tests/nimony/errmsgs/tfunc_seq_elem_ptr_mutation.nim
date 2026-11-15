## A `var`-returning accessor (`s[i]` for a `seq`) returns a location inside its
## first argument, so the element of a `seq` behind a `ref`/`ptr` is behind it
## too: a `.noSideEffect` routine can neither mutate it nor return it as `var`.
type
  Elem = object
    x: int
  Obj = object
    s: seq[int]
    es: seq[Elem]
  Node = ref Obj
  Holder = object
    r: Node
  GNode[T] = ref object
    s: seq[T]

func bump(x: var int) = inc x
func at(s: seq[int]; i: int): var int = s[i]

func viaInc(n: Node) = inc n.s[0]
func viaVarArg(h: var Holder) = bump(h.r.s[0])
func viaSwap(n: Node) = swap(n.s[0], n.s[1])
func viaVarResult(n: var Node): var int = n.s[0]
func viaResultAsgn(n: Node): var int =
  result = n.s[0]
func viaPlusEq(n: Node) = n.s[0] += 1
func viaPtr(p: ptr Obj) = inc p.s[0]
func viaField(n: Node) = n.es[0].x = 3
func viaOpenArray(n: Node) = inc toOpenArray(n.s, 0, 1)[0]
func viaUserAccessor(n: Node) = inc at(n.s, 0)
func viaGeneric[T](n: GNode[T]) = swap(n.s[0], n.s[1])
proc viaProc(n: Node) {.noSideEffect.} = inc n.s[0]

func ownSeq(s: var seq[int]) =
  inc s[0]                 # the own storage of the `var` parameter
  swap(s[0], s[1])
func ownField(o: var Obj) = inc o.es[0].x
func withCast(n: Node) =
  {.cast(noSideEffect).}:
    inc n.s[0]
proc sideEffectsAllowed(n: Node) = inc n.s[0]

viaGeneric(GNode[int](s: @[1, 2]))

# a template's expansion is the location it names
template elem(n: Node): untyped = n.s[0]
template field(h: Holder): untyped = h.r.s
func viaTemplate(n: Node) = inc elem(n)
func viaTemplateField(h: Holder) = field(h) = @[]
iterator viaIterator(n: Node): int =
  inc n.s[0]
  yield 1
for _ in viaIterator(Node(s: @[1])): discard
