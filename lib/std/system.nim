## System module for Nimony

type
  int* {.magic: Int.}         ## Default integer type; bitwidth depends on
                              ## architecture, but is always the same as a pointer.
  int8* {.magic: Int8.}       ## Signed 8 bit integer type.
  int16* {.magic: Int16.}     ## Signed 16 bit integer type.
  int32* {.magic: Int32.}     ## Signed 32 bit integer type.
  int64* {.magic: Int64.}     ## Signed 64 bit integer type.
  uint* {.magic: UInt.}       ## Unsigned default integer type.
  uint8* {.magic: UInt8.}     ## Unsigned 8 bit integer type.
  uint16* {.magic: UInt16.}   ## Unsigned 16 bit integer type.
  uint32* {.magic: UInt32.}   ## Unsigned 32 bit integer type.
  uint64* {.magic: UInt64.}   ## Unsigned 64 bit integer type.

type
  float* {.magic: Float.}     ## Default floating point type.
  float32* {.magic: Float32.} ## 32 bit floating point type.
  float64* {.magic: Float.}   ## 64 bit floating point type.

type
  char* {.magic: Char.}         ## Built-in 8 bit character type (unsigned).
  cstring* {.magic: Cstring.}   ## Built-in cstring (*compatible string*) type.
  pointer* {.magic: Pointer.}   ## Built-in pointer type, use the `addr`
                                ## operator to get a pointer to a variable.

  typedesc*[T] {.magic: TypeDesc.} ## Meta type to denote a type description.
  UncheckedArray*[T] {.magic: UncheckedArray.} ## Built-in unchecked array type.

type
  string* = object ## Built-in string type.
    a: ptr UncheckedArray[char]
    i: int

type # we need to start a new type section here, so that ``0`` can have a type
  bool* {.magic: "Bool".} = enum ## Built-in boolean type.
    false = 0, true = 1

proc `not`*(x: bool): bool {.magic: "Not", noSideEffect.}
  ## Boolean not; returns true if `x == false`.

proc `and`*(x, y: bool): bool {.magic: "And", noSideEffect.}
  ## Boolean `and`; returns true if `x == y == true` (if both arguments
  ## are true).
  ##
  ## Evaluation is lazy: if `x` is false, `y` will not even be evaluated.
proc `or`*(x, y: bool): bool {.magic: "Or", noSideEffect.}
  ## Boolean `or`; returns true if `not (not x and not y)` (if any of
  ## the arguments is true).
  ##
  ## Evaluation is lazy: if `x` is true, `y` will not even be evaluated.
proc `xor`*(x, y: bool): bool {.magic: "Xor", noSideEffect.}
  ## Boolean `exclusive or`; returns true if `x != y` (if either argument
  ## is true while the other is false).

type
  untyped* {.magic: Expr.}
  typed* {.magic: Stmt.}

iterator unpack*(): untyped {.magic: Unpack.}

proc unpackToCall(fn: untyped) {.magic: Unpack.}

const
  isMainModule* {.magic: "IsMainModule".}: bool = false

type
  Ordinal*[T] {.magic: Ordinal.} ## Generic ordinal type. Includes integer,
                                  ## bool, character, and enumeration types
                                  ## as well as their subtypes. See also
                                  ## `SomeOrdinal`.

type
  range*[T]{.magic: "Range".}         ## Generic type to construct range types.
  array*[I, T]{.magic: "Array".}      ## Generic type to construct
                                      ## fixed-length arrays.
  set*[T]{.magic: "Set".}             ## Generic type to construct bit sets.

type sink*[T]{.magic: "Sink".}
type lent*[T]{.magic: "Lent".}

proc low*[T: Ordinal|enum|range](x: typedesc[T]): T {.magic: "Low", noSideEffect.}
proc low*[I, T](x: typedesc[array[I, T]]): I {.magic: "Low", noSideEffect.}
proc high*[T: Ordinal|enum|range](x: typedesc[T]): T {.magic: "High", noSideEffect.}
proc high*[I, T](x: typedesc[array[I, T]]): I {.magic: "High", noSideEffect.}

proc `[]`*[T: tuple](x: T, i: int): untyped {.magic: "TupAt".}
proc `[]`*[I, T](x: array[I, T], i: I): var T {.magic: "ArrAt".}
proc `[]`*(x: cstring, i: int): var char {.magic: "Pat".}
proc `[]`*[T](x: ptr UncheckedArray[T], i: int): var T {.magic: "Pat".}
template `[]=`*[T: tuple](x: T, i: int, elem: typed) =
  (x[i]) = elem
template `[]=`*[I, T](x: array[I, T], i: I; elem: T) =
  (x[i]) = elem
template `[]=`*(x: cstring, i: int; elem: char) =
  (x[i]) = elem
template `[]=`*[T](x: ptr UncheckedArray[T], i: int; elem: T) =
  (x[i]) = elem

proc `[]`*[T](x: ptr T): var T {.magic: "Deref", noSideEffect.}
proc `[]`*[T](x: ref T): var T {.magic: "Deref", noSideEffect.}
template `[]=`*[T](x: ptr T, val: T) =
  (x[]) = val
template `[]=`*[T](x: ref T, val: T) =
  (x[]) = val

# integer calculations:
template `+`*(x: int8): int8 = x
template `+`*(x: int16): int16 = x
template `+`*(x: int32): int32 = x
template `+`*(x: int64): int64 = x
  ## Unary `+` operator for an integer. Has no effect.

proc `-`*(x: int8): int8 {.magic: "UnaryMinusI", noSideEffect.}
proc `-`*(x: int16): int16 {.magic: "UnaryMinusI", noSideEffect.}
proc `-`*(x: int32): int32 {.magic: "UnaryMinusI", noSideEffect.}
proc `-`*(x: int64): int64 {.magic: "UnaryMinusI", noSideEffect.}
  ## Unary `-` operator for an integer. Negates `x`.

proc `not`*(x: int8): int8 {.magic: "BitnotI", noSideEffect.}
proc `not`*(x: int16): int16 {.magic: "BitnotI", noSideEffect.}
proc `not`*(x: int32): int32 {.magic: "BitnotI", noSideEffect.}
proc `not`*(x: int64): int64 {.magic: "BitnotI", noSideEffect.}

proc `+`*(x, y: int8): int8 {.magic: "AddI", noSideEffect.}
proc `+`*(x, y: int16): int16 {.magic: "AddI", noSideEffect.}
proc `+`*(x, y: int32): int32 {.magic: "AddI", noSideEffect.}
proc `+`*(x, y: int64): int64 {.magic: "AddI", noSideEffect.}
  ## Binary `+` operator for an integer.

proc `-`*(x, y: int8): int8 {.magic: "SubI", noSideEffect.}
proc `-`*(x, y: int16): int16 {.magic: "SubI", noSideEffect.}
proc `-`*(x, y: int32): int32 {.magic: "SubI", noSideEffect.}
proc `-`*(x, y: int64): int64 {.magic: "SubI", noSideEffect.}
  ## Binary `-` operator for an integer.

proc `*`*(x, y: int8): int8 {.magic: "MulI", noSideEffect.}
proc `*`*(x, y: int16): int16 {.magic: "MulI", noSideEffect.}
proc `*`*(x, y: int32): int32 {.magic: "MulI", noSideEffect.}
proc `*`*(x, y: int64): int64 {.magic: "MulI", noSideEffect.}
  ## Binary `*` operator for an integer.

proc `div`*(x, y: int8): int8 {.magic: "DivI", noSideEffect.}
proc `div`*(x, y: int16): int16 {.magic: "DivI", noSideEffect.}
proc `div`*(x, y: int32): int32 {.magic: "DivI", noSideEffect.}
proc `div`*(x, y: int64): int64 {.magic: "DivI", noSideEffect.}

proc `mod`*(x, y: int8): int8 {.magic: "ModI", noSideEffect.}
proc `mod`*(x, y: int16): int16 {.magic: "ModI", noSideEffect.}
proc `mod`*(x, y: int32): int32 {.magic: "ModI", noSideEffect.}
proc `mod`*(x, y: int64): int64 {.magic: "ModI", noSideEffect.}

type
  SomeInteger = int # for now just an alias

proc `shr`*(x: int8, y: SomeInteger): int8 {.magic: "AshrI", noSideEffect.}
proc `shr`*(x: int16, y: SomeInteger): int16 {.magic: "AshrI", noSideEffect.}
proc `shr`*(x: int32, y: SomeInteger): int32 {.magic: "AshrI", noSideEffect.}
proc `shr`*(x: int64, y: SomeInteger): int64 {.magic: "AshrI", noSideEffect.}


proc `shl`*(x: int8, y: SomeInteger): int8 {.magic: "ShlI", noSideEffect.}
proc `shl`*(x: int16, y: SomeInteger): int16 {.magic: "ShlI", noSideEffect.}
proc `shl`*(x: int32, y: SomeInteger): int32 {.magic: "ShlI", noSideEffect.}
proc `shl`*(x: int64, y: SomeInteger): int64 {.magic: "ShlI", noSideEffect.}

proc ashr*(x: int8, y: SomeInteger): int8 {.magic: "AshrI", noSideEffect.}
proc ashr*(x: int16, y: SomeInteger): int16 {.magic: "AshrI", noSideEffect.}
proc ashr*(x: int32, y: SomeInteger): int32 {.magic: "AshrI", noSideEffect.}
proc ashr*(x: int64, y: SomeInteger): int64 {.magic: "AshrI", noSideEffect.}

proc `and`*(x, y: int8): int8 {.magic: "BitandI", noSideEffect.}
proc `and`*(x, y: int16): int16 {.magic: "BitandI", noSideEffect.}
proc `and`*(x, y: int32): int32 {.magic: "BitandI", noSideEffect.}
proc `and`*(x, y: int64): int64 {.magic: "BitandI", noSideEffect.}
  ## Computes the `bitwise and` of numbers `x` and `y`.

proc `or`*(x, y: int8): int8 {.magic: "BitorI", noSideEffect.}
proc `or`*(x, y: int16): int16 {.magic: "BitorI", noSideEffect.}
proc `or`*(x, y: int32): int32 {.magic: "BitorI", noSideEffect.}
proc `or`*(x, y: int64): int64 {.magic: "BitorI", noSideEffect.}
  ## Computes the `bitwise or` of numbers `x` and `y`.

proc `xor`*(x, y: int8): int8 {.magic: "BitxorI", noSideEffect.}
proc `xor`*(x, y: int16): int16 {.magic: "BitxorI", noSideEffect.}
proc `xor`*(x, y: int32): int32 {.magic: "BitxorI", noSideEffect.}
proc `xor`*(x, y: int64): int64 {.magic: "BitxorI", noSideEffect.}
  ## Computes the `bitwise xor` of numbers `x` and `y`.

# unsigned integer operations:

proc `not`*(x: uint8): uint8 {.magic: "BitnotI", noSideEffect.}
proc `not`*(x: uint16): uint16 {.magic: "BitnotI", noSideEffect.}
proc `not`*(x: uint32): uint32 {.magic: "BitnotI", noSideEffect.}
proc `not`*(x: uint64): uint64 {.magic: "BitnotI", noSideEffect.}

proc `shr`*(x: uint8, y: SomeInteger): uint8 {.magic: "ShrI", noSideEffect.}
proc `shr`*(x: uint16, y: SomeInteger): uint16 {.magic: "ShrI", noSideEffect.}
proc `shr`*(x: uint32, y: SomeInteger): uint32 {.magic: "ShrI", noSideEffect.}
proc `shr`*(x: uint64, y: SomeInteger): uint64 {.magic: "ShrI", noSideEffect.}

proc `shl`*(x: uint8, y: SomeInteger): uint8 {.magic: "ShlI", noSideEffect.}
proc `shl`*(x: uint16, y: SomeInteger): uint16 {.magic: "ShlI", noSideEffect.}
proc `shl`*(x: uint32, y: SomeInteger): uint32 {.magic: "ShlI", noSideEffect.}
proc `shl`*(x: uint64, y: SomeInteger): uint64 {.magic: "ShlI", noSideEffect.}

proc `and`*(x, y: uint8): uint8 {.magic: "BitandI", noSideEffect.}
proc `and`*(x, y: uint16): uint16 {.magic: "BitandI", noSideEffect.}
proc `and`*(x, y: uint32): uint32 {.magic: "BitandI", noSideEffect.}
proc `and`*(x, y: uint64): uint64 {.magic: "BitandI", noSideEffect.}

proc `or`*(x, y: uint8): uint8 {.magic: "BitorI", noSideEffect.}
proc `or`*(x, y: uint16): uint16 {.magic: "BitorI", noSideEffect.}
proc `or`*(x, y: uint32): uint32 {.magic: "BitorI", noSideEffect.}
proc `or`*(x, y: uint64): uint64 {.magic: "BitorI", noSideEffect.}

proc `xor`*(x, y: uint8): uint8 {.magic: "BitxorI", noSideEffect.}
proc `xor`*(x, y: uint16): uint16 {.magic: "BitxorI", noSideEffect.}
proc `xor`*(x, y: uint32): uint32 {.magic: "BitxorI", noSideEffect.}
proc `xor`*(x, y: uint64): uint64 {.magic: "BitxorI", noSideEffect.}

proc `+`*(x, y: uint8): uint8 {.magic: "AddU", noSideEffect.}
proc `+`*(x, y: uint16): uint16 {.magic: "AddU", noSideEffect.}
proc `+`*(x, y: uint32): uint32 {.magic: "AddU", noSideEffect.}
proc `+`*(x, y: uint64): uint64 {.magic: "AddU", noSideEffect.}

proc `-`*(x, y: uint8): uint8 {.magic: "SubU", noSideEffect.}
proc `-`*(x, y: uint16): uint16 {.magic: "SubU", noSideEffect.}
proc `-`*(x, y: uint32): uint32 {.magic: "SubU", noSideEffect.}
proc `-`*(x, y: uint64): uint64 {.magic: "SubU", noSideEffect.}

proc `*`*(x, y: uint8): uint8 {.magic: "MulU", noSideEffect.}
proc `*`*(x, y: uint16): uint16 {.magic: "MulU", noSideEffect.}
proc `*`*(x, y: uint32): uint32 {.magic: "MulU", noSideEffect.}
proc `*`*(x, y: uint64): uint64 {.magic: "MulU", noSideEffect.}

proc `div`*(x, y: uint8): uint8 {.magic: "DivU", noSideEffect.}
proc `div`*(x, y: uint16): uint16 {.magic: "DivU", noSideEffect.}
proc `div`*(x, y: uint32): uint32 {.magic: "DivU", noSideEffect.}
proc `div`*(x, y: uint64): uint64 {.magic: "DivU", noSideEffect.}

proc `mod`*(x, y: uint8): uint8 {.magic: "ModU", noSideEffect.}
proc `mod`*(x, y: uint16): uint16 {.magic: "ModU", noSideEffect.}
proc `mod`*(x, y: uint32): uint32 {.magic: "ModU", noSideEffect.}
proc `mod`*(x, y: uint64): uint64 {.magic: "ModU", noSideEffect.}

# floating point operations:
template `+`*(x: float32): float32 = x
proc `-`*(x: float32): float32 {.magic: "UnaryMinusF64", noSideEffect.}
proc `+`*(x, y: float32): float32 {.magic: "AddF64", noSideEffect.}
proc `-`*(x, y: float32): float32 {.magic: "SubF64", noSideEffect.}
proc `*`*(x, y: float32): float32 {.magic: "MulF64", noSideEffect.}
proc `/`*(x, y: float32): float32 {.magic: "DivF64", noSideEffect.}

template `+`*(x: float): float = x
proc `-`*(x: float): float {.magic: "UnaryMinusF64", noSideEffect.}
proc `+`*(x, y: float): float {.magic: "AddF64", noSideEffect.}
proc `-`*(x, y: float): float {.magic: "SubF64", noSideEffect.}
proc `*`*(x, y: float): float {.magic: "MulF64", noSideEffect.}
proc `/`*(x, y: float): float {.magic: "DivF64", noSideEffect.}

type
  Incable = concept
    proc `+`(x, y: Self): Self
  Decable = concept
    proc `-`(x, y: Self): Self

proc inc*[T: Incable, V: Ordinal](x: var T, y: V) {.inline.} =
  ## Increments the ordinal `x` by `y`.
  x = x + T(y)

proc dec*[T: Decable, V: Ordinal](x: var T, y: V) {.inline.} =
  ## Decrements the ordinal `x` by `y`.
  x = x - T(y)

proc inc*[T: Incable](x: var T) {.inline.} =
  # workaround for no default params
  x = x + T(1)

proc dec*[T: Decable](x: var T) {.inline.} =
  # workaround for no default params
  x = x - T(1)

# comparison operators:
proc `==`*[Enum: enum](x, y: Enum): bool {.magic: "EqEnum", noSideEffect.}
  ## Checks whether values within the *same enum* have the same underlying value.

proc `==`*(x, y: pointer): bool {.magic: "EqRef", noSideEffect.}
  ## Checks for equality between two `pointer` variables.

proc `==`*(x, y: char): bool {.magic: "EqCh", noSideEffect.}
  ## Checks for equality between two `char` variables.
proc `==`*(x, y: bool): bool {.magic: "EqB", noSideEffect.}
  ## Checks for equality between two `bool` variables.
proc `==`*[T](x, y: set[T]): bool {.magic: "EqSet", noSideEffect.}
  ## Checks for equality between two variables of type `set`.

proc `==`*[T](x, y: ref T): bool {.magic: "EqRef", noSideEffect.}
  ## Checks that two `ref` variables refer to the same item.
proc `==`*[T](x, y: ptr T): bool {.magic: "EqRef", noSideEffect.}
  ## Checks that two `ptr` variables refer to the same item.

proc `<=`*[Enum: enum](x, y: Enum): bool {.magic: "LeEnum", noSideEffect.}

proc `<=`*(x, y: char): bool {.magic: "LeCh", noSideEffect.}
  ## Compares two chars and returns true if `x` is lexicographically
  ## before `y` (uppercase letters come before lowercase letters).

proc `<=`*[T](x, y: set[T]): bool {.magic: "LeSet", noSideEffect.}
  ## Returns true if `x` is a subset of `y`.
  ##
  ## A subset `x` has all of its members in `y` and `y` doesn't necessarily
  ## have more members than `x`. That is, `x` can be equal to `y`.

proc `<=`*(x, y: bool): bool {.magic: "LeB", noSideEffect.}
proc `<=`*[T](x, y: ref T): bool {.magic: "LePtr", noSideEffect.}
proc `<=`*(x, y: pointer): bool {.magic: "LePtr", noSideEffect.}

proc `<`*[Enum: enum](x, y: Enum): bool {.magic: "LtEnum", noSideEffect.}

proc `<`*(x, y: char): bool {.magic: "LtCh", noSideEffect.}
  ## Compares two chars and returns true if `x` is lexicographically
  ## before `y` (uppercase letters come before lowercase letters).

proc `<`*[T](x, y: set[T]): bool {.magic: "LtSet", noSideEffect.}
  ## Returns true if `x` is a strict or proper subset of `y`.
  ##
  ## A strict or proper subset `x` has all of its members in `y` but `y` has
  ## more elements than `y`.

proc `==`*(x, y: int8): bool {.magic: "EqI", noSideEffect.}
proc `==`*(x, y: int16): bool {.magic: "EqI", noSideEffect.}
proc `==`*(x, y: int32): bool {.magic: "EqI", noSideEffect.}
proc `==`*(x, y: int64): bool {.magic: "EqI", noSideEffect.}

proc `<=`*(x, y: int8): bool {.magic: "LeI", noSideEffect.}
proc `<=`*(x, y: int16): bool {.magic: "LeI", noSideEffect.}
proc `<=`*(x, y: int32): bool {.magic: "LeI", noSideEffect.}
proc `<=`*(x, y: int64): bool {.magic: "LeI", noSideEffect.}

proc `<`*(x, y: int8): bool {.magic: "LtI", noSideEffect.}
proc `<`*(x, y: int16): bool {.magic: "LtI", noSideEffect.}
proc `<`*(x, y: int32): bool {.magic: "LtI", noSideEffect.}
proc `<`*(x, y: int64): bool {.magic: "LtI", noSideEffect.}

proc `<=`*(x, y: uint8): bool {.magic: "LeU", noSideEffect.}
proc `<=`*(x, y: uint16): bool {.magic: "LeU", noSideEffect.}
proc `<=`*(x, y: uint32): bool {.magic: "LeU", noSideEffect.}
proc `<=`*(x, y: uint64): bool {.magic: "LeU", noSideEffect.}

proc `<`*(x, y: uint8): bool {.magic: "LtU", noSideEffect.}
proc `<`*(x, y: uint16): bool {.magic: "LtU", noSideEffect.}
proc `<`*(x, y: uint32): bool {.magic: "LtU", noSideEffect.}
proc `<`*(x, y: uint64): bool {.magic: "LtU", noSideEffect.}

proc `==`*(x, y: uint8): bool {.magic: "EqI", noSideEffect.}
proc `==`*(x, y: uint16): bool {.magic: "EqI", noSideEffect.}
proc `==`*(x, y: uint32): bool {.magic: "EqI", noSideEffect.}
proc `==`*(x, y: uint64): bool {.magic: "EqI", noSideEffect.}

proc `<=`*(x, y: float32): bool {.magic: "LeF64", noSideEffect.}
proc `<=`*(x, y: float): bool {.magic: "LeF64", noSideEffect.}

proc `<`*(x, y: float32): bool {.magic: "LtF64", noSideEffect.}
proc `<`*(x, y: float): bool {.magic: "LtF64", noSideEffect.}

proc `==`*(x, y: float32): bool {.magic: "EqF64", noSideEffect.}
proc `==`*(x, y: float): bool {.magic: "EqF64", noSideEffect.}

proc min*(x, y: int8): int8 {.noSideEffect, inline.} =
  if x <= y: x else: y
proc min*(x, y: int16): int16 {.noSideEffect, inline.} =
  if x <= y: x else: y
proc min*(x, y: int32): int32 {.noSideEffect, inline.} =
  if x <= y: x else: y
proc min*(x, y: int64): int64 {.noSideEffect, inline.} =
  ## The minimum value of two integers.
  if x <= y: x else: y
proc min*(x, y: float32): float32 {.noSideEffect, inline.} =
  if x <= y or y != y: x else: y
proc min*(x, y: float): float {.noSideEffect, inline.} =
  if x <= y or y != y: x else: y

proc max*(x, y: int8): int8 {.noSideEffect, inline.} =
  if y <= x: x else: y
proc max*(x, y: int16): int16 {.noSideEffect, inline.} =
  if y <= x: x else: y
proc max*(x, y: int32): int32 {.noSideEffect, inline.} =
  if y <= x: x else: y
proc max*(x, y: int64): int64 {.noSideEffect, inline.} =
  ## The maximum value of two integers.
  if y <= x: x else: y
proc max*(x, y: float32): float32 {.noSideEffect, inline.} =
  if y <= x or y != y: x else: y
proc max*(x, y: float): float {.noSideEffect, inline.} =
  if y <= x or y != y: x else: y

template `!=`*(x, y: untyped): untyped =
  ## Unequals operator. This is a shorthand for `not (x == y)`.
  not (x == y)

template `>=`*(x, y: untyped): untyped =
  ## "is greater or equals" operator. This is the same as `y <= x`.
  y <= x

template `>`*(x, y: untyped): untyped =
  ## "is greater" operator. This is the same as `y < x`.
  y < x

type HasDefault* = concept
  proc default(_: typedesc[Self]): Self

template default*(x: typedesc[bool]): bool = false
template default*(x: typedesc[char]): char = '\0'
template default*(x: typedesc[int]): int = 0
template default*(x: typedesc[uint]): uint = 0'u
template default*(x: typedesc[int8]): int8 = 0'i8
template default*(x: typedesc[uint8]): uint8 = 0'u8
template default*(x: typedesc[int16]): int16 = 0'i16
template default*(x: typedesc[uint16]): uint16 = 0'u16
template default*(x: typedesc[int32]): int32 = 0'i32
template default*(x: typedesc[uint32]): uint32 = 0'u32
template default*(x: typedesc[int64]): int64 = 0'i64
template default*(x: typedesc[uint64]): uint64 = 0'u64
template default*(x: typedesc[float32]): float32 = 0.0'f32
template default*(x: typedesc[float64]): float64 = 0.0'f64
template default*(x: typedesc[string]): string = ""
template default*[T: enum](x: typedesc[T]): T = low(T)

template default*[T: ptr](x: typedesc[T]): T = T(nil)
template default*[T: ref](x: typedesc[T]): T = T(nil)

proc default*[T: object](x: typedesc[T]): T {.magic: DefaultObj.}
proc default*[T: tuple](x: typedesc[T]): T {.magic: DefaultTup.}

proc defined*(x: untyped): bool {.magic: Defined.}
proc declared*(x: untyped): bool {.magic: Declared.}

proc `$`*[T: enum](x: T): string {.magic: "EnumToStr", noSideEffect.}
  ## Converts an enum value to a string.

proc addr*[T](x: T): ptr T {.magic: "Addr", noSideEffect.}

proc sizeof*[T](x: T): int {.magic: "SizeOf", noSideEffect.}
proc sizeof*(x: typedesc): int {.magic: "SizeOf", noSideEffect.}

proc `=destroy`*[T](x: T) {.magic: "Destroy", noSideEffect.}
proc `=dup`*[T](x: T): T {.magic: "Dup", noSideEffect.}
proc `=copy`*[T](dest: var T; src: T) {.magic: "Copy", noSideEffect.}
proc `=wasMoved`*[T](x: var T) {.magic: "WasMoved", noSideEffect.}
proc `=sink`*[T](dest: var T; src: T) {.magic: "SinkHook", noSideEffect.}
proc `=trace`*[T](x: var T; env: pointer) {.magic: "Trace", noSideEffect.}

proc ensureMove*[T](x: T): T {.magic: "EnsureMove", noSideEffect.}

proc move*[T](x: var T): T {.nodestroy, inline, noSideEffect.} =
  result = x
  `=wasMoved`(x)

template len*[I, T](x: typedesc[array[I, T]]): int =
  ## Returns the length of an array type.
  ## This is roughly the same as `high(T)-low(T)+1`.
  high(array[I, T]).int - low(array[I, T]).int + 1
template len*[I, T](x: array[I, T]): int =
  ## Returns the length of an array.
  ## This is roughly the same as `high(T)-low(T)+1`.
  len(array[I, T])

# This must be the first include so that we know string's `==` is the 17th.
# This is a minor hack, let's see how long it will be able to last. The fact that ==.17
# is the string equality is used by hexer/stringcases.nim.
include "system/stringimpl"

include "system/countbits_impl"
include "system/setops"

include "system/openarrays"
include "system/seqimpl"

include "system/iterators"

include "system/atomics"

proc swap*[T](x, y: var T) {.inline, nodestroy.} =
  let tmp = x
  x = y
  y = tmp

type
  Comparable* = concept
    proc `==`(x, y: Self): bool
    proc `<`(x, y: Self): bool

proc cmp*[T: Comparable](x, y: T): int =
  ## Generic compare proc.
  ##
  ## Returns:
  ## * a value less than zero, if `x < y`
  ## * a value greater than zero, if `x > y`
  ## * zero, if `x == y`
  ##
  ## This is useful for writing generic algorithms without performance loss.
  ## This generic implementation uses the `==` and `<` operators.
  ##   ```nim
  ##   import std/algorithm
  ##   echo sorted(@[4, 2, 6, 5, 8, 7], cmp[int])
  ##   ```
  if x == y: return 0
  if x < y: return -1
  return 1

proc newConstr[T](t: typedesc[T]): T {.magic: "NewRef", nodecl.}
proc new*[T: ref](x: out T) {.inline.} = x = newConstr(T)
