## The parameter `get` infers binds `put` too: `Box` is not a `Gettable[T]`
## when `put` takes another type than `get` returns (nim-lang/nimony#3206).

type
  Box = object
    a: float

func get(b: Box): float = b.a
func put(b: var Box; v: string) = discard

type
  Gettable[T] = concept
    func get(x: Self): T
    func put(x: var Self, v: T)

func roundtrip[T; C: Gettable[T]](a: C): C =
  result = a
  put(result, get(a))

discard roundtrip(Box())
