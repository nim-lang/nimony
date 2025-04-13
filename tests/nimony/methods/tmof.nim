
import std / [syncio]

type
  RootObj {.inheritable.} = object

type Obj = object of RootObj
  a, b: int
  c: string

method m(o: RootObj) =
  echo "RootObj"

method m(o: Obj) =
  echo "Obj"

proc test(o: RootObj) =
  echo o of RootObj
  echo o of Obj
  m o

test(Obj(a: 1, b: 2, c: "3"))
test(RootObj())
