import std/syncio

# An object constructor in an untyped template whose field key shares its name
# with a proc: the key is still the field, here an inherited one.
type
  Mid = ref object of RootObj
    value: float
  Leaf = ref object of Mid
    c: float
  Other = object
    m: float

proc value(o: Other): float = o.m

template make(T: untyped) {.untyped.} =
  let x = T(value: 1.0)
  echo x.value

template makeLocal(T: untyped) {.untyped.} =
  let value = 2.5
  let x = T(value: value, c: 3.0)
  echo x.value, " ", x.c

make(Leaf)
makeLocal(Leaf)
