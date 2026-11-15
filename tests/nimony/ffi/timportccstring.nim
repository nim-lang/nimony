type CStringTypeDef {.importc: "CStringTypeDef", header: "cstringtypedef.h".} = cstring

var x: CStringTypeDef = CStringTypeDef("a")
discard x
