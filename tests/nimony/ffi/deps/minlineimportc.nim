type
  CInt* = int32

proc cfunc() {.importc: "cfunc", header: "inlineimportc.h".}
proc cfuncWithParam(x: CInt): CInt {.importc: "cfuncWithParam", header: "inlineimportc.h".}

proc callcfunc*() =
  cfunc()

proc callcfuncInline*() {.inline.} =
  cfunc()

proc callcfuncWithParam*(x: int): int =
  cfuncWithParam(x.CInt)

proc callcfuncWithParamInline*(x: int): int {.inline.} =
  cfuncWithParam(x.CInt)
