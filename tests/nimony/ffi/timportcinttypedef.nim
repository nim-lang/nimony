type CIntType {.importc: "CIntType", header: "cinttype.h".} = int32

var x: CIntType
discard x
