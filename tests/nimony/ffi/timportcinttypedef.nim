type CIntTypeDef {.importc: "CIntTypeDef", header: "cinttypedef.h".} = int32

var x: CIntTypeDef
discard x
