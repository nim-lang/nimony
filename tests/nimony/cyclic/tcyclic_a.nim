import std/syncio
import mcyclic_b {.cyclic.}

proc fromA*(): int = 1

proc useB*(): int = fromB()

echo fromA()
echo useB()
