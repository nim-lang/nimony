import tcyclic_a {.cyclic.}

proc fromB*(): int = 2

proc useA*(): int = fromA()
