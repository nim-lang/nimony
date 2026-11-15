# What field facts must not claim: a value that reads the variable it is stored
# into (its fields are the old ones), a field written after the copy, and a
# variant object, whose fields overlap.

type
  I = object
    thing, other: int
  V = object
    case k: bool
    of true: a: int
    of false: b: int

proc selfRef(x: int): I {.ensures: result.thing >= 0 and result.thing < 3.} =
  result = I(thing: 0, other: x)
  result = I(thing: result.other, other: 0)

proc copyThenWrite(x: int): I {.ensures: result.thing >= 0 and result.thing < 3.} =
  var a = I(thing: 1, other: 0)
  var b = a
  b.thing = x
  result = b

proc variant(x: int): int {.ensures: result >= 0 and result < 3.} =
  let v = V(k: true, a: 1)
  result = v.a

discard selfRef(1)
discard copyThenWrite(1)
discard variant(1)
