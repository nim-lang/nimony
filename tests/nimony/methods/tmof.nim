
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
test(y[])

let z = Obj2(y)
assert z of Obj2
test(z[])

proc testRef(o: ref RootObj) =
  echo "testing ref"
  echo o of RootObj
  echo o of Obj
  echo o of Obj2
  m o[]

testRef y
testRef z

proc testNil() =
  var nilRootObj: ref RootObj = nil
  #echo "nil of RootObj: ", nilRootObj of RootObj
  # ^ gives always true warning, original nim just compiles it to `true` but here gives `false` at runtime
  echo "nil of Obj: ", nilRootObj of Obj
  echo "nil of Obj2: ", nilRootObj of Obj2
testNil()
