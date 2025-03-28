## System module for Nimony

include "system/basic_types"

iterator unpack*(): untyped {.magic: Unpack.}

proc unpackToCall(fn: untyped) {.magic: Unpack.}

const
  isMainModule* {.magic: "IsMainModule".}: bool = false

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

include "system/stringimpl"

include "system/countbits_impl"
include "system/setops"

include "system/openarrays"
include "system/seqimpl"

include "system/atomics"

proc newConstr[T](t: typedesc[T]): T {.magic: "NewRef", nodecl.}
proc new*[T: ref](x: out T) {.inline.} = x = newConstr(T)

template runnableExamples*(body: untyped) {.untyped.} =
  discard "ignore runnable examples"

proc overflowFlag*(): bool {.magic: "OverflowFlag".}
