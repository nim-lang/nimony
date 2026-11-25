# A vtable slot needs code: a generic method gets it only per instance of its
# class's typevars, so a class without typevars cannot have one.
type
  A = ref object of RootObj
  B = ref object of A
  G[T] = ref object of RootObj

method gm[T](a: A, x: T): int {.base.} = 1
method nb(a: A, x: int): int {.base.} = 2
method nb[T](a: B, x: T): int = 3
method gi[U](a: G[int], x: U): int {.base.} = 4
method ok[T](a: G[T]): int {.base.} = 5
