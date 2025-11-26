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

# nimony only:
type
  OrdinalEnum* {.magic: "OrdinalEnum".}
  HoleyEnum* {.magic: "HoleyEnum".}
  `enum`* = OrdinalEnum | HoleyEnum

type
  SomeSignedInt* = int|int8|int16|int32|int64
    ## Type class matching all signed integer types.

  SomeUnsignedInt* = uint|uint8|uint16|uint32|uint64
    ## Type class matching all unsigned integer types.

  SomeInteger* = SomeSignedInt|SomeUnsignedInt
    ## Type class matching all integer types.

  SomeFloat* = float|float32|float64
    ## Type class matching all floating point number types.

  SomeNumber* = SomeInteger|SomeFloat
    ## Type class matching all number types.

  SomeOrdinal* = int|int8|int16|int32|int64|bool|enum|uint|uint8|uint16|uint32|uint64
    ## Type class matching all ordinal types; however this includes enums with
    ## holes. See also `Ordinal`

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

  void* {.magic: "VoidType".} ## Meta type to denote the absence of any type.

type
  Ordinal*[T] {.magic: Ordinal.} ## Generic ordinal type. Includes integer,
                                  ## bool, character, and enumeration types
                                  ## as well as their subtypes. See also
                                  ## `SomeOrdinal`.

type
  range*[T]{.magic: "Range".}         ## Generic type to construct range types.
  array*[I, T]{.magic: "Array".}      ## Generic type to construct
                                      ## fixed-length arrays.
  varargs*[T]{.magic: "Varargs".}     ## Generic type to construct a varargs type.
  set*[T]{.magic: "Set".}             ## Generic type to construct bit sets.

type sink*[T]{.magic: "Sink".}
type lent*[T]{.magic: "Lent".}

proc low*[T: Ordinal|enum|range](x: typedesc[T]): T {.magic: "Low", noSideEffect.}
proc low*[I, T](x: typedesc[array[I, T]]): I {.magic: "Low", noSideEffect.}
proc high*[T: Ordinal|enum|range](x: typedesc[T]): T {.magic: "High", noSideEffect.}
proc high*[I, T](x: typedesc[array[I, T]]): I {.magic: "High", noSideEffect.}

template low*[I, T](x: array[I, T]): I =
  low(array[I, T])
template high*[I, T](x: array[I, T]): I =
  high(array[I, T])

type
  RootObj* {.inheritable.} =
    object ## The root of Nim's object hierarchy.
           ##
           ## Objects should inherit from `RootObj` or one of its descendants.
           ## However, objects that have no ancestor are also allowed.
  RootRef* = ref RootObj ## Reference to `RootObj`.
