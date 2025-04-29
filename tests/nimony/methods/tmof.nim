
import std / [syncio, assertions]

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

type Obj2 = ref object of RootObj

method m(o: Obj2) =
  echo "Obj2"

let x = RootObj(Obj2()[])
assert x of Obj2
test(Obj2()[])

let y = (ref RootObj)(Obj2())
assert y of Obj2

#proc testRef(o: ref RootObj) =
#  echo o of RootObj
#  echo o of Obj
#  m o[]
#
#testRef y
