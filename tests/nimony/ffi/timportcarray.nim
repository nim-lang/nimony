import std/assertions

type
  CInt = int32
  CStruct {.importc: "struct CStruct", header: "ctestarray.h".} = object
    field {.importc: "field".}: array[10, CInt]
  CStruct2 {.importc: "struct CStruct2", header: "ctestarray.h".} = object
    cs {.importc: "cs".}: CStruct
    cs2 {.importc: "cs2".}: array[2, CStruct]

proc setCArray(arr: out array[2, CInt]) {.importc: "setCArray", header: "ctestarray.h".}
proc setCStruct(cs: ptr CStruct) {.importc: "setCStruct", header: "ctestarray.h".}
proc setCStruct2(cs2: ptr CStruct2) {.importc: "setCStruct2", header: "ctestarray.h".}

block:
  var carray: array[2, CInt]
  setCArray(carray)
  assert carray[0] == 1
  assert carray[1] == 2

block:
  var cstruct {.noinit.}: CStruct
  setCStruct(addr cstruct)
  assert cstruct.field[0] == 567
  assert cstruct.field[1] == 8910

block:
  var cstruct2 {.noinit.}: CStruct2
  setCStruct2(addr cstruct2)
  assert cstruct2.cs.field[0] == 111
  assert cstruct2.cs.field[1] == 222
  assert cstruct2.cs2[0].field[0] == 333
  assert cstruct2.cs2[0].field[1] == 4444
  assert cstruct2.cs2[1].field[0] == 55555
  assert cstruct2.cs2[1].field[1] == 666666
