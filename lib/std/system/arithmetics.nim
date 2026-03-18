func succ*[T, V: Ordinal](x: T; y: V = T(1)): T {.magic: "Succ", noSideEffect.}
func pred*[T, V: Ordinal](x: T; y: V = T(1)): T {.magic: "Pred", noSideEffect.}

func inc*[T, V: Ordinal](x: var T, y: V) {.inline.} =
  ## Increments the ordinal `x` by `y`.
  x = succ(x, y)

func dec*[T, V: Ordinal](x: var T, y: V) {.inline.} =
  ## Decrements the ordinal `x` by `y`.
  x = pred(x, y)

func inc*[T: Ordinal](x: var T) {.inline.} =
  ## Increments the ordinal `x` by 1.
  x = succ(x)

func dec*[T: Ordinal](x: var T) {.inline.} =
  ## Decrements the ordinal `x` by 1.
  x = pred(x)

# --------------------------------------------------------------------------
# built-in operators

# integer calculations:
template `+`*(x: int8): int8 = x
template `+`*(x: int16): int16 = x
template `+`*(x: int32): int32 = x
template `+`*(x: int64): int64 = x
  ## Unary `+` operator for an integer. Has no effect.

func `-`*(x: int8): int8 {.magic: "UnaryMinusI", noSideEffect.}
func `-`*(x: int16): int16 {.magic: "UnaryMinusI", noSideEffect.}
func `-`*(x: int32): int32 {.magic: "UnaryMinusI", noSideEffect.}
func `-`*(x: int64): int64 {.magic: "UnaryMinusI", noSideEffect.}
  ## Unary `-` operator for an integer. Negates `x`.

func `not`*(x: int8): int8 {.magic: "BitnotI", noSideEffect.}
func `not`*(x: int16): int16 {.magic: "BitnotI", noSideEffect.}
func `not`*(x: int32): int32 {.magic: "BitnotI", noSideEffect.}
func `not`*(x: int64): int64 {.magic: "BitnotI", noSideEffect.}

func `+`*(x, y: int8): int8 {.magic: "AddI", noSideEffect.}
func `+`*(x, y: int16): int16 {.magic: "AddI", noSideEffect.}
func `+`*(x, y: int32): int32 {.magic: "AddI", noSideEffect.}
func `+`*(x, y: int64): int64 {.magic: "AddI", noSideEffect.}
  ## Binary `+` operator for an integer.

func `-`*(x, y: int8): int8 {.magic: "SubI", noSideEffect.}
func `-`*(x, y: int16): int16 {.magic: "SubI", noSideEffect.}
func `-`*(x, y: int32): int32 {.magic: "SubI", noSideEffect.}
func `-`*(x, y: int64): int64 {.magic: "SubI", noSideEffect.}
  ## Binary `-` operator for an integer.

func `*`*(x, y: int8): int8 {.magic: "MulI", noSideEffect.}
func `*`*(x, y: int16): int16 {.magic: "MulI", noSideEffect.}
func `*`*(x, y: int32): int32 {.magic: "MulI", noSideEffect.}
func `*`*(x, y: int64): int64 {.magic: "MulI", noSideEffect.}
  ## Binary `*` operator for an integer.

func `div`*(x, y: int8): int8 {.magic: "DivI", noSideEffect.}
func `div`*(x, y: int16): int16 {.magic: "DivI", noSideEffect.}
func `div`*(x, y: int32): int32 {.magic: "DivI", noSideEffect.}
func `div`*(x, y: int64): int64 {.magic: "DivI", noSideEffect.}

func `mod`*(x, y: int8): int8 {.magic: "ModI", noSideEffect.}
func `mod`*(x, y: int16): int16 {.magic: "ModI", noSideEffect.}
func `mod`*(x, y: int32): int32 {.magic: "ModI", noSideEffect.}
func `mod`*(x, y: int64): int64 {.magic: "ModI", noSideEffect.}


# SomeInteger moved to generic param since implicit generics are not implemented yet:

func `shr`*[I: SomeInteger](x: int8, y: I): int8 {.magic: "AshrI", noSideEffect.}
func `shr`*[I: SomeInteger](x: int16, y: I): int16 {.magic: "AshrI", noSideEffect.}
func `shr`*[I: SomeInteger](x: int32, y: I): int32 {.magic: "AshrI", noSideEffect.}
func `shr`*[I: SomeInteger](x: int64, y: I): int64 {.magic: "AshrI", noSideEffect.}


func `shl`*[I: SomeInteger](x: int8, y: I): int8 {.magic: "ShlI", noSideEffect.}
func `shl`*[I: SomeInteger](x: int16, y: I): int16 {.magic: "ShlI", noSideEffect.}
func `shl`*[I: SomeInteger](x: int32, y: I): int32 {.magic: "ShlI", noSideEffect.}
func `shl`*[I: SomeInteger](x: int64, y: I): int64 {.magic: "ShlI", noSideEffect.}

func ashr*[I: SomeInteger](x: int8, y: I): int8 {.magic: "AshrI", noSideEffect.}
func ashr*[I: SomeInteger](x: int16, y: I): int16 {.magic: "AshrI", noSideEffect.}
func ashr*[I: SomeInteger](x: int32, y: I): int32 {.magic: "AshrI", noSideEffect.}
func ashr*[I: SomeInteger](x: int64, y: I): int64 {.magic: "AshrI", noSideEffect.}

func `and`*(x, y: int8): int8 {.magic: "BitandI", noSideEffect.}
func `and`*(x, y: int16): int16 {.magic: "BitandI", noSideEffect.}
func `and`*(x, y: int32): int32 {.magic: "BitandI", noSideEffect.}
func `and`*(x, y: int64): int64 {.magic: "BitandI", noSideEffect.}
  ## Computes the `bitwise and` of numbers `x` and `y`.

func `or`*(x, y: int8): int8 {.magic: "BitorI", noSideEffect.}
func `or`*(x, y: int16): int16 {.magic: "BitorI", noSideEffect.}
func `or`*(x, y: int32): int32 {.magic: "BitorI", noSideEffect.}
func `or`*(x, y: int64): int64 {.magic: "BitorI", noSideEffect.}
  ## Computes the `bitwise or` of numbers `x` and `y`.

func `xor`*(x, y: int8): int8 {.magic: "BitxorI", noSideEffect.}
func `xor`*(x, y: int16): int16 {.magic: "BitxorI", noSideEffect.}
func `xor`*(x, y: int32): int32 {.magic: "BitxorI", noSideEffect.}
func `xor`*(x, y: int64): int64 {.magic: "BitxorI", noSideEffect.}
  ## Computes the `bitwise xor` of numbers `x` and `y`.

# unsigned integer operations:

func `not`*(x: uint8): uint8 {.magic: "BitnotI", noSideEffect.}
func `not`*(x: uint16): uint16 {.magic: "BitnotI", noSideEffect.}
func `not`*(x: uint32): uint32 {.magic: "BitnotI", noSideEffect.}
func `not`*(x: uint64): uint64 {.magic: "BitnotI", noSideEffect.}

func `shr`*[I: SomeInteger](x: uint8, y: I): uint8 {.magic: "ShrI", noSideEffect.}
func `shr`*[I: SomeInteger](x: uint16, y: I): uint16 {.magic: "ShrI", noSideEffect.}
func `shr`*[I: SomeInteger](x: uint32, y: I): uint32 {.magic: "ShrI", noSideEffect.}
func `shr`*[I: SomeInteger](x: uint64, y: I): uint64 {.magic: "ShrI", noSideEffect.}

func `shl`*[I: SomeInteger](x: uint8, y: I): uint8 {.magic: "ShlI", noSideEffect.}
func `shl`*[I: SomeInteger](x: uint16, y: I): uint16 {.magic: "ShlI", noSideEffect.}
func `shl`*[I: SomeInteger](x: uint32, y: I): uint32 {.magic: "ShlI", noSideEffect.}
func `shl`*[I: SomeInteger](x: uint64, y: I): uint64 {.magic: "ShlI", noSideEffect.}

func `and`*(x, y: uint8): uint8 {.magic: "BitandI", noSideEffect.}
func `and`*(x, y: uint16): uint16 {.magic: "BitandI", noSideEffect.}
func `and`*(x, y: uint32): uint32 {.magic: "BitandI", noSideEffect.}
func `and`*(x, y: uint64): uint64 {.magic: "BitandI", noSideEffect.}

func `or`*(x, y: uint8): uint8 {.magic: "BitorI", noSideEffect.}
func `or`*(x, y: uint16): uint16 {.magic: "BitorI", noSideEffect.}
func `or`*(x, y: uint32): uint32 {.magic: "BitorI", noSideEffect.}
func `or`*(x, y: uint64): uint64 {.magic: "BitorI", noSideEffect.}

func `xor`*(x, y: uint8): uint8 {.magic: "BitxorI", noSideEffect.}
func `xor`*(x, y: uint16): uint16 {.magic: "BitxorI", noSideEffect.}
func `xor`*(x, y: uint32): uint32 {.magic: "BitxorI", noSideEffect.}
func `xor`*(x, y: uint64): uint64 {.magic: "BitxorI", noSideEffect.}

func `+`*(x, y: uint8): uint8 {.magic: "AddU", noSideEffect.}
func `+`*(x, y: uint16): uint16 {.magic: "AddU", noSideEffect.}
func `+`*(x, y: uint32): uint32 {.magic: "AddU", noSideEffect.}
func `+`*(x, y: uint64): uint64 {.magic: "AddU", noSideEffect.}

func `-`*(x, y: uint8): uint8 {.magic: "SubU", noSideEffect.}
func `-`*(x, y: uint16): uint16 {.magic: "SubU", noSideEffect.}
func `-`*(x, y: uint32): uint32 {.magic: "SubU", noSideEffect.}
func `-`*(x, y: uint64): uint64 {.magic: "SubU", noSideEffect.}

func `*`*(x, y: uint8): uint8 {.magic: "MulU", noSideEffect.}
func `*`*(x, y: uint16): uint16 {.magic: "MulU", noSideEffect.}
func `*`*(x, y: uint32): uint32 {.magic: "MulU", noSideEffect.}
func `*`*(x, y: uint64): uint64 {.magic: "MulU", noSideEffect.}

func `div`*(x, y: uint8): uint8 {.magic: "DivU", noSideEffect.}
func `div`*(x, y: uint16): uint16 {.magic: "DivU", noSideEffect.}
func `div`*(x, y: uint32): uint32 {.magic: "DivU", noSideEffect.}
func `div`*(x, y: uint64): uint64 {.magic: "DivU", noSideEffect.}

func `mod`*(x, y: uint8): uint8 {.magic: "ModU", noSideEffect.}
func `mod`*(x, y: uint16): uint16 {.magic: "ModU", noSideEffect.}
func `mod`*(x, y: uint32): uint32 {.magic: "ModU", noSideEffect.}
func `mod`*(x, y: uint64): uint64 {.magic: "ModU", noSideEffect.}

# floating point operations:
template `+`*(x: float32): float32 = x
func `-`*(x: float32): float32 {.magic: "UnaryMinusF64", noSideEffect.}
func `+`*(x, y: float32): float32 {.magic: "AddF64", noSideEffect.}
func `-`*(x, y: float32): float32 {.magic: "SubF64", noSideEffect.}
func `*`*(x, y: float32): float32 {.magic: "MulF64", noSideEffect.}
func `/`*(x, y: float32): float32 {.magic: "DivF64", noSideEffect.}

template `+`*(x: float): float = x
func `-`*(x: float): float {.magic: "UnaryMinusF64", noSideEffect.}
func `+`*(x, y: float): float {.magic: "AddF64", noSideEffect.}
func `-`*(x, y: float): float {.magic: "SubF64", noSideEffect.}
func `*`*(x, y: float): float {.magic: "MulF64", noSideEffect.}
func `/`*(x, y: float): float {.magic: "DivF64", noSideEffect.}

func `+=`*[T: SomeNumber](x: var T; y: T) {.untyped, inline.} =
  x = x + y

func `-=`*[T: SomeNumber](x: var T; y: T) {.untyped, inline.} =
  x = x - y

func `*=`*[T: SomeNumber](x: var T; y: T) {.untyped, inline.} =
  x = x * y
