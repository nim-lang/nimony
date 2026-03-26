import std/syncio

type Parent = ref object of RootObj
type Child = ref object of Parent

method m(x: Parent) =
  echo "parent"

method m(x: Child) =
  echo "child"

var c = Child()
procCall Parent(c).m()