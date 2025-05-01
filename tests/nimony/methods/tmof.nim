
import std / [syncio, assertions]

type
  RootObj2 {.inheritable.} = object

type Obj = object of RootObj2
  a, b: int
  c: string

method m(o: RootObj2) =
  echo "RootObj"

method m(o: Obj) =
  echo "Obj"

proc test(o: RootObj2) =
  echo o of RootObj2
  echo o of Obj
  m o

test(Obj(a: 1, b: 2, c: "3"))
test(RootObj2())

type Obj2 = ref object of RootObj2

method m(o: Obj2) =
  echo "Obj2"

let x = RootObj2(Obj2()[])
assert x of Obj2
test(Obj2()[])

let y = (ref RootObj2)(Obj2())
assert y of Obj2
test(y[])

let z = Obj2(y)
assert z of Obj2
test(z[])

proc testRef(o: ref RootObj2) =
  echo "testing ref"
  echo o of RootObj2
  echo o of Obj
  echo o of Obj2
  m o[]

testRef y
testRef z

proc testNil() =
  var nilRootObj: ref RootObj2 = nil
  #echo "nil of RootObj: ", nilRootObj of RootObj2
  # ^ gives always true warning, original nim just compiles it to `true` but here gives `false` at runtime
  echo "nil of Obj: ", nilRootObj of Obj
  echo "nil of Obj2: ", nilRootObj of Obj2
testNil()
