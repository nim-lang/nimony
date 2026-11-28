import std/syncio
type
  E1 = enum
    value1,
    value2
  E2 = enum
    value1,
    value2 = 4
  E3 = enum
    value1 = 4,
    value2

const
  Lookuptable = [
    E1.value1: "1",
    # no need to qualify value2, known to be E1.value2
    value2: "2"
  ]

echo Lookuptable[E1.value1]
echo Lookuptable[E1.value2]

var
  Lookuptable2 = [
    E1.value1: "1",
    value2: "2"
  ]
echo Lookuptable2[E1.value1]
echo Lookuptable2[E1.value2]

var
  Lookuptable3 = [
    E3.value1: "1",
    value2: "2"
  ]
echo Lookuptable3[E3.value1]
echo Lookuptable3[E3.value2]

# a key that is a `block` expression gives the start index like a literal one
const FromBlock = [(block: 1): "a", 2: "b"]
echo FromBlock[1], FromBlock[2]

template keyed(k: untyped): untyped = [k: "c", k+1: "d"]
echo keyed(0)[1]
