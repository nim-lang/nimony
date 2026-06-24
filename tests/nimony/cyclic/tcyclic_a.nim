import std/syncio
import mcyclic_b {.cyclic.}

type A* = object

proc fromA*(): int = 1

proc useB*(b: B): int = fromB()

echo fromA()
echo useB(B())
