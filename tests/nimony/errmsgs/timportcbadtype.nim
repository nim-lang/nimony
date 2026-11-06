type
  S {.importc.} = seq[int]
  T {.importc: "T".} = (int, int)
  Obj = object
    x: int
  P {.importc: "int*".} = ptr cint
  Q {.importc: "Obj*".} = ptr Obj
  R {.importc: "R*".} = ref Obj

var x: S
