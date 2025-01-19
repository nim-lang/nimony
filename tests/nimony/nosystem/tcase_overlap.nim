
type
  int* {.magic: Int.}
  bool* {.magic: Bool.}
  float* {.magic: Float.}

proc `+`*(x, y: int): int {.magic: "AddI".}
proc `-`*(x, y: int): int {.magic: "SubI".}

proc `<=`*(x, y: int): bool {.magic: "LeI".}

proc `+`*(x, y: float): float {.magic: "AddF64".}
proc `-`*(x, y: float): float {.magic: "SubF64".}

proc `<=`*(x, y: float): bool {.magic: "LeF64".}

proc inc*(x: var int) = discard


type
  MyEnum = enum
    ValueA, ValueB, ValueC

var global: MyEnum
#inc global

global = ValueB

case global
of ValueA:
  discard
of ValueB, ValueA:
  discard
of ValueC:
  discard
