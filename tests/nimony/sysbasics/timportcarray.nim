type
  CInt = int32
  CStruct {.importc: "struct CStruct", header: "ctestarray.h".} = object
    field {.importc: "field".}: array[10, CInt]
  CStruct2 {.importc: "struct CStruct2", header: "ctestarray.h".} = object
    cs {.importc: "cs".}: CStruct
    cs2 {.importc: "cs2".}: array[2, CStruct]

proc setCStruct(cs: ptr CStruct) {.importc: "setCStruct", header: "ctestarray.h"}
proc setCStruct2(cs2: ptr CStruct2) {.importc: "setCStruct2", header: "ctestarray.h"}

block:
  var cstruct: CStruct
  setCStruct(addr cstruct)
  # TODO: Use doAssert when it become available
  discard cstruct.field[0] == 567
  discard cstruct.field[1] == 8910

block:
  var cstruct2: CStruct2
  setCStruct2(addr cstruct2)
  discard cstruct2.cs.field[0] == 111
  discard cstruct2.cs.field[1] == 222
  discard cstruct2.cs2[0].field[0] == 333
  discard cstruct2.cs2[0].field[1] == 444
  discard cstruct2.cs2[1].field[0] == 55555
  discard cstruct2.cs2[1].field[1] == 666666
