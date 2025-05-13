
proc use(x: int) = discard

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
  use x
  x = 9

maine(valA)
