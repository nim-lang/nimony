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

func astToStr*[T](x: T): string {.magic: AstToStr.}
  ## Converts the AST of `x` into a string representation. This is very useful
  ## for debugging.

func compiles*(x: untyped): bool {.magic: Compiles.}
  ## Special compile-time procedure that checks whether `x` can be compiled
  ## without any semantic error.
  ## This can be used to check whether a type supports some operation:
  ##   ```nim
  ##   when compiles(3 + 4):
  ##     echo "'+' for integers is available"
  ##   ```


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
      result = NegTen[int(-x)]
    elif x == -9223372036854775808:
      result = "-" & $cast[uint64](x)
    else:
      result = "-" & $(0-x)
  elif x < 10:
    result.add char(x + int64('0'))
  else:
    result = $cast[uint64](x)

proc `$`*(x: int32): string =
  $(int64(x))

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

proc `is`*[T, S](x: T, y: S): bool {.magic: "Is", noSideEffect.}
template `isnot`*(x, y: untyped): untyped =
  not (x is y)

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

include "system/dyncalls"

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

func isNil*(s: cstring): bool {.inline.} = s == nil

func chr*(u: range[0..255]): char {.inline.} =
  ## Converts `u` to a `char`, same as `char(u)`.
  char(u.int)

include "../../vendor/errorcodes/src" / errorcodes

var localErr* {.threadvar.}: ErrorCode

type
  ContinuationProc* = proc (coro: ptr CoroutineBase): Continuation {.nimcall.}
  Continuation* = object
    fn*: ContinuationProc
    env*: ptr CoroutineBase
  CoroutineBase* = object of RootObj
    caller*: Continuation

method cancel*(coro: ptr CoroutineBase) =
  discard "to override"

proc afterYield*(): Continuation {.semantics: "afterYield".} =
  ## Special builtin that returns the next continuation within a `yield` statement.
  ## Do not use unless you know what you are doing.
  result = Continuation(fn: nil, env: nil)

proc delay*(x: untyped): Continuation {.magic: "Delay".}
  ## Delays the execution of a `.passive` proc and returns a continuation representation
  ## this call. Think of it as a `toTask` builtin.

proc trivialTick(c: Continuation): Continuation =
  result = c.fn(c.env)

type
  Scheduler* = proc (c: Continuation): Continuation {.nimcall.}
    ## A scheduler is a function that takes a continuation and returns a new continuation.

var scheduler: Scheduler = trivialTick
proc setScheduler*(handler: Scheduler) {.inline.} =
  # XXX needs atomic store here
  scheduler = handler

proc advance*(c: Continuation): Continuation =
  ## Single steps through a list of continuations. Usually this does not need
  ## to be called directly. Used by the compiler to run a coroutine.
  result = scheduler(c)

proc complete*(c: Continuation) =
  ## Used by the compiler to run a coroutine until completion.
  var c = c
  while c.fn != nil:
    c = scheduler(c)

func `==`*[T: tuple|object](x, y: T): bool =
  ## Return true only if each fields of `x` and `y` are equal.
  for xf, yf in fields(x, y):
    if xf != yf: return false
  return true

func `==`*[T: Equatable](x, y: seq[T]): bool =
  ## Generic equals operator for sequences: relies on a equals operator for
  ## the element type `T`.
  if y.rawData == x.rawData:
    return true

  if x.len != y.len:
    return false

  for i in 0..x.len-1:
    if x[i] != y[i]:
      return false

  return true

const HexChars = "0123456789ABCDEF"

proc addEscapedChar*(s: var string, c: char) {.noSideEffect, inline.} =
  ## Adds a char to string `s` and applies the following escaping:
  ##
  ## * replaces any ``\`` by `\\`
  ## * replaces any `'` by `\'`
  ## * replaces any `"` by `\"`
  ## * replaces any `\a` by `\\a`
  ## * replaces any `\b` by `\\b`
  ## * replaces any `\t` by `\\t`
  ## * replaces any `\n` by `\\n`
  ## * replaces any `\v` by `\\v`
  ## * replaces any `\f` by `\\f`
  ## * replaces any `\r` by `\\r`
  ## * replaces any `\e` by `\\e`
  ## * replaces any other character not in the set `{\21..\126}`
  ##   by `\xHH` where `HH` is its hexadecimal value
  ##
  ## The procedure has been designed so that its output is usable for many
  ## different common syntaxes.
  ##
  ## .. warning:: This is **not correct** for producing ANSI C code!
  ##
  case c
  of '\a': s.add "\\a" # \x07
  of '\b': s.add "\\b" # \x08
  of '\t': s.add "\\t" # \x09
  of '\n': s.add "\\n" # \x0A
  of '\v': s.add "\\v" # \x0B
  of '\f': s.add "\\f" # \x0C
  of '\r': (when defined(nimLegacyAddEscapedCharx0D): s.add "\\c" else: s.add "\\r") # \x0D
  of '\e': s.add "\\e" # \x1B
  of '\\': s.add("\\\\")
  of '\'': s.add("\\'")
  of '\"': s.add("\\\"")
  of {'\32'..'\126'} - {'\\', '\'', '\"'}: s.add(c)
  else:
    s.add("\\x")
    let n = ord(c)
    s.add(HexChars[int((n and 0xF0) shr 4)])
    s.add(HexChars[int(n and 0xF)])

proc addQuoted*[T](s: var string, x: T) =
  ## Appends `x` to string `s` in place, applying quoting and escaping
  ## if `x` is a string or char.
  ##
  ## See `addEscapedChar <#addEscapedChar,string,char>`_
  ## for the escaping scheme. When `x` is a string, characters in the
  ## range `{\128..\255}` are never escaped so that multibyte UTF-8
  ## characters are untouched (note that this behavior is different from
  ## `addEscapedChar`).
  ##
  ## The Nim standard library uses this function on the elements of
  ## collections when producing a string representation of a collection.
  ## It is recommended to use this function as well for user-side collections.
  ## Users may overload `addQuoted` for custom (string-like) types if
  ## they want to implement a customized element representation.
  ##
  ##   ```nim
  ##   var tmp = ""
  ##   tmp.addQuoted(1)
  ##   tmp.add(", ")
  ##   tmp.addQuoted("string")
  ##   tmp.add(", ")
  ##   tmp.addQuoted('c')
  ##   assert(tmp == """1, "string", 'c'""")
  ##   ```
  when T is string or T is cstring:
    s.add("\"")
    for c in x:
      # Only ASCII chars are escaped to avoid butchering
      # multibyte UTF-8 characters.
      if c <= 127.char:
        s.addEscapedChar(c)
      else:
        s.add c
    s.add("\"")
  elif T is char:
    s.add("'")
    s.addEscapedChar(x)
    s.add("'")
  # prevent temporary string allocation
  elif T is SomeInteger:
    s.addInt(x)
  elif T is SomeFloat:
    s.addFloat(x)
  elif compiles(s.add(x)):
    s.add(x)
  else:
    s.add($x)

type
  # TODO: change to `range[0..high(int)]` when range type is implemented
  Natural* = int
    ## is an `int` type ranging from zero to the maximum value
    ## of an `int`. This type is often useful for documentation and debugging.

  # TODO: change to `range[1..high(int)]`
  Positive* = int
    ## is an `int` type ranging from one to the maximum value
    ## of an `int`. This type is often useful for documentation and debugging.

  HSlice*[T, U] = object   ## "Heterogeneous" slice type.
    a*: T                  ## The lower bound (inclusive).
    b*: U                  ## The upper bound (inclusive).
  Slice*[T] = HSlice[T, T] ## An alias for `HSlice[T, T]`.

func `..`*[T, U](a: sink T; b: sink U): HSlice[T, U] {.inline.} =
  ## Binary `slice`:idx: operator that constructs an interval `[a, b]`, both `a`
  ## and `b` are inclusive.
  ##
  ## Slices can also be used in the set constructor and in ordinal case
  ## statements, but then they are special-cased by the compiler.
  ##   ```nim
  ##   let a = [10, 20, 30, 40, 50]
  ##   echo a[2 .. 3] # @[30, 40]
  ##   ```
  result = HSlice[T, U](a: a, b: b)

type
  TypeOfMode* = enum ## Possible modes of `typeof`.
    typeOfProc,      ## Prefer the interpretation that means `x` is a proc call.
    typeOfIter       ## Prefer the interpretation that means `x` is an iterator call.

proc typeof*(x: untyped; mode = typeOfIter): typedesc {.magic: TypeOf.}
  ## Builtin `typeof` operation for accessing the type of an expression.
