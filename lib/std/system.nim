## System module for Nimony

include "system/basic_types"

iterator unpack*(): untyped {.magic: Unpack.}

proc unpackToCall(fn: untyped) {.magic: Unpack.}

const
  isMainModule* {.magic: "IsMainModule".}: bool = false
  Inf* {.magic: "Inf".}: float64 = 0.0
    ## Contains the IEEE floating point value of positive infinity.
  NaN* {.magic: "NaN".}: float64 = 0.0
    ## Contains an IEEE floating point value of *Not A Number*.
    ##
    ## Note that you cannot compare a floating point value to this value
    ## and expect a reasonable result - use the `isNaN` or `classify` procedure
    ## in the `math module <math.html>`_ for checking for NaN.

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

const
  # Use string literals for one digit numbers to avoid the allocations as they are so common.
  NegTen = [
    "-0", "-1", "-2", "-3", "-4",
    "-5", "-6", "-7", "-8", "-9"]

proc `$`*(x: uint64): string =
  result = ""
  if x < 10:
    result = NegTen[int x].substr(1, 1)
  else:
    var y = x
    while true:
      result.add char((y mod 10'u) + uint('0'))
      y = y div 10'u
      if y == 0'u: break
    let last = result.len-1
    var i = 0
    let b = result.len div 2
    while i < b:
      let ch = result[i]
      result[i] = result[last-i]
      result[last-i] = ch
      inc i

proc `$`*(x: int64): string =
  if x < 0:
    if x > -10:
      result = NegTen[-x]
    if x == -9223372036854775808:
      result = "-" & $cast[uint64](x)
    else:
      result = "-" & $(0-x)
  elif x < 10:
    result.add char(x + int64('0'))
  else:
    result = $cast[uint64](x)

proc addInt*(s: var string; x: int64) {.inline.} =
  s.add $x

proc addInt*(s: var string; x: uint64) {.inline.} =
  s.add $x

proc `$`*(b: bool): string =
  if b: "true" else: "false"

proc `$`*[T: enum](x: T): string {.magic: "EnumToStr", noSideEffect.}
  ## Converts an enum value to a string.

proc addr*[T](x: T): ptr T {.magic: "Addr", noSideEffect.}

proc sizeof*[T](x: typedesc[T]): int {.magic: "SizeOf", noSideEffect.}

template sizeof*[T](_: T): int =
  sizeof(T)

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

template `in`*(x, y: untyped): untyped =
  contains(y, x)

template `notin`*(x, y: untyped): untyped =
  not contains(y, x)

include "system/iterators"
include "system/defaults"

include "system/countbits_impl"
include "system/setops"

include "system/ctypes"

include "system/memory"
include "system/seqimpl"
include "system/stringimpl"
include "system/openarrays"

include "system/atomics"

proc newConstr[T](t: typedesc[T]): T {.magic: "NewRef", nodecl.}
proc new*[T: ref](x: out T) {.inline.} = x = newConstr(T)

template runnableExamples*(body: untyped) {.untyped.} =
  discard "ignore runnable examples"

proc overflowFlag*(): bool {.magic: "OverflowFlag".}

include "system/panics"

proc `of`*[T, S](x: T; y: typedesc[S]): bool {.magic: "Of", noSideEffect.}
proc procCall*[T](x: T): untyped {.magic: "ProcCall".}

type
  Rtti* = object
    dl: int
    dy: ptr UncheckedArray[uint32]
    mt: UncheckedArray[pointer]

proc getRtti(dummy: pointer): ptr Rtti {.nodecl.} = discard "patched by vtables.nim"

func ord*[T: Ordinal|enum](x: T): int {.inline.} =
  ## Returns the internal `int` value of `x`, including for enum with holes
  ## and distinct ordinal types.

  int(x)

type
  ComparableAndNegatable = concept
    proc `<`(x, y: Self): bool
    proc `-`(x: Self): Self

func abs*[T: ComparableAndNegatable](x: T): T {.inline.} =
  ## Returns the absolute value of `x`.
  if x < 0: -x else: x

include "../../vendor/errorcodes/src" / errorcodes
