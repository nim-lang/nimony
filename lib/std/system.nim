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

include "system/arithmetics"

include "system/comparisons"

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

proc swap*[T](x, y: var T) {.inline, nodestroy.} =
  let tmp = x
  x = y
  y = tmp

include "system/iterators"

include "system/defaults"

include "system/stringimpl"

include "system/countbits_impl"
include "system/setops"

include "system/openarrays"
include "system/seqimpl"

include "system/atomics"

proc newConstr[T](t: typedesc[T]): T {.magic: "NewRef", nodecl.}
proc new*[T: ref](x: out T) {.inline.} = x = newConstr(T)
