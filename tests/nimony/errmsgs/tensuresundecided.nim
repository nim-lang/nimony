# An undecided `.ensures` is an error in every mode: nothing checks it at run
# time and every caller assumes it, so `arr[a(7).thing]` would read out of
# bounds without an index check.

type I = object
  thing: int

proc a(x: int): I {.ensures: result.thing >= 0 and result.thing < 3.} =
  result = I(thing: x)

proc b(x: int): int {.ensures: result >= 0.} =
  result = x

const arr = [10, 20, 30]
discard arr[a(7).thing]
discard b(1)
