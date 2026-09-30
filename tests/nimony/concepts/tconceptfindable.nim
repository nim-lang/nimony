## A container concept as a constraint (`C: Findable[T]`): at the call site `T`
## is inferred from the requirements; inside the generic body the requirements
## are available with the concept's `T` being the routine's `T` — `items(x)`
## yields `T` and `==` on two `T`s resolves via the concept.

import std/[syncio, assertions]

type
  Findable[T] = concept
    iterator items(x: Self): T
    proc `==`(a, b: T): bool

proc find[T; C: Findable[T]](x: C; elem: T): int =
  var i = 0
  for a in items(x):
    if a == elem: return i
    inc i
  return -1

type
  MyCollection = object
    data: seq[int]

  Names = object
    first, second: string

  NoItems = object

iterator items(x: MyCollection): int =
  for d in x.data: yield d

iterator items(x: Names): string =
  yield x.first
  yield x.second

let myCollection = MyCollection(data: @[1, 2, 3, 4, 5])
echo find(myCollection, 3)
echo find(myCollection, 7)
echo find(Names(first: "a", second: "b"), "b")

assert not compiles(find(NoItems(), 1))
# `T` is inferred as `int`, `elem` must agree:
assert not compiles(find(myCollection, "x"))
