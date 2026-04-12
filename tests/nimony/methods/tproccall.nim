import std/syncio

type Parent = ref object of RootObj
type Child = ref object of Parent

method m(x: Parent) =
  echo "parent"

method m(x: Child) =
  echo "child"

proc main =
  var c = Child()
  if c != nil:
    procCall Parent(c).m()

main()
