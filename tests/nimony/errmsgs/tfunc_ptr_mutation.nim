## A `.noSideEffect` routine must not mutate through a `ref`/`ptr`, also when
## the head of the access path is a `var` parameter: it may write the own
## storage of the parameter, not what the parameter points to.
## `{.cast(noSideEffect).}` lifts it.
type
  Node = ref object
    x: int
  Plain = object
    x: int
  Holder = object
    r: Node
    p: ptr Plain

func viaRef(n: Node) = n.x = 5
func viaPtr(p: ptr Plain) = p.x = 5
func viaVarArg(n: Node) = inc n.x
func viaLocal(h: Holder) =
  var copy = h
  copy.r.x = 1

func varHead(h: var Holder) =
  h.r.x = 1
  inc h.p.x

func viaVarResult(h: var Holder): var int = h.r.x

func direct(h: var Holder; n: Node) =
  h.r = n          # the own storage of the `var` parameter

func withCast(p: ptr Plain) =
  {.cast(noSideEffect).}:
    p.x = 4

proc sideEffectsAllowed(n: Node) = n.x = 5
