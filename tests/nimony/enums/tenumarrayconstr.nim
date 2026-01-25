import std/syncio
type
  E1 = enum
    value1,
    value2
  E2 = enum
    value1,
    value2 = 4

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