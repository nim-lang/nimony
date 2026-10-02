## Generic bodies resolve the requirements of a typevar constrained by an
## instantiated generic concept (`A: Indexable[T]`), even when `T` is only
## inferred via the concept, and an inherited concept invocation
## (`MutableIndexable[T]`) satisfies its parent's (`Indexable[T]`), issue #2603.

import std/[syncio]

type
  Findable[T] = concept
    iterator items(x: Self): T
    proc `==`(a, b: T): bool

proc count[T; C: Findable[T]](x: C): int =
  var i = 0
  for a in items(x):
    inc i
  return i

type
  MyCollection = object
    data: seq[int]

iterator items(x: MyCollection): int =
  for d in x.data: yield d

echo count(MyCollection(data: @[1, 2, 3]))

type
  Indexable*[T] = concept
    func `[]`(x: Self, idx: int): T

func get*[T; A: Indexable[T]](x: A; idx: int): T =
  x[idx]

func getPtr*[T; A: Indexable[T]](x: ptr A; idx: int): T =
  (x[])[idx]

type
  MutableIndexable*[T] = concept of Indexable[T]
    func `[]=`(x: var Self, idx: int, value: T)

type
  IndexedBox*[T; A: Indexable[T]] = object
    data: ptr A

func set*[T; A: MutableIndexable[T]](b: IndexedBox[T, A]; idx: int; value: T) =
  b.data[][idx] = value

func first*[T; A: MutableIndexable[T]](b: IndexedBox[T, A]): T =
  b.data[][0]

var s = @[1, 2, 3]
echo get(s, 1)
echo getPtr(addr s, 2)
var b = IndexedBox[int, seq[int]](data: addr s)
set(b, 0, 10)
echo s[0]
echo first(b)
