import tcyclic_a {.cyclic.}

type B* = object

proc fromB*(): int = 2

proc useA*(a: A): int = fromA()
