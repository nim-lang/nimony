import std/assertions

type
  CInt = int32
  CStruct {.importc: "struct CStruct", header: "ctestarray.h".} = object
    field {.importc: "field".}: array[10, CInt]
  CStruct2 {.importc: "struct CStruct2", header: "ctestarray.h".} = object
    cs {.importc: "cs".}: CStruct
    cs2 {.importc: "cs2".}: array[2, CStruct]
  CStruct3 {.importc: "struct CStruct3", header: "ctestarray.h".} = object
    k: CInt
    m: array[2, array[3, CInt]]

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

# An array field of an imported struct is a plain C array: its initializer
# is a brace list, in every placement.
const constStruct = CStruct(field: [1'i32, 2, 3, 4, 5, 6, 7, 8, 9, 10])
var globalStruct = default(CStruct)

block:
  let zero = default(CStruct)
  let lit = CStruct(field: [21'i32, 22, 23, 24, 25, 26, 27, 28, 29, 30])
  let outer = CStruct2(cs2: [lit, zero])
  let nested = CStruct3(k: 7, m: [[1'i32, 2, 3], [4'i32, 5, 6]])
  let nestedZero = default(CStruct3)
  assert constStruct.field[9] == 10
  assert zero.field[3] == 0
  assert lit.field[0] == 21
  assert outer.cs.field[1] == 0
  assert outer.cs2[0].field[1] == 22
  assert outer.cs2[1].field[2] == 0
  assert nested.k == 7
  assert nestedZero.k == 0
  {.cast(assumeSync).}:
    assert globalStruct.field[5] == 0
