## A typevar of the checked type is not inferred by the check: `put` taking
## an `int` does not make `Box[T]` a `Gettable[T]` for every `T`.

type
  Box[T] = object
    a: T

func get[T](b: Box[T]): T = b.a
func put[T](b: var Box[T]; v: int) = discard

type
  Gettable[T] = concept
    func get(x: Self): T
    func put(x: var Self, v: T)

func use[T; C: Gettable[T]](a: C): int = 1

func free[T](a: Box[T]): int = use(a)
