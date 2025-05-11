const x = 1
x = 12

type
  MyEnum = enum
    valA, valB, valC

proc maine(e: MyEnum) =
  let x: int
  case e
  of valA:
    x = 4
  of valB:
    x = 5
  of valC:
    x = 6
  bar int x
  x = 9

maine(valA)
