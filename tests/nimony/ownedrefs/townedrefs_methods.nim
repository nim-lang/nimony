# method dispatch on an `owned` receiver of a class hierarchy
{.feature: "ownedRefs".}
import std/syncio

type
  Parent = ref object of RootObj
    id: int
  Child = ref object of Parent

method m(x: Parent) {.base.} = echo "parent ", x.id
method m(x: Child) = echo "child ", x.id

proc main =
  let c = Child(id: 1)   # owned
  c.m()
  procCall Parent(c).m()
  let p: Parent = Child(id: 2)
  p.m()

main()
