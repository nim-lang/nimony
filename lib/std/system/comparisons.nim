# comparison operators:

# not in original nim, so that it works for generic `Ordinal` types:
proc `==`*[T: Ordinal](x, y: T): bool {.magic: "EqI", noSideEffect.}

# `enum` typeclass split here to prevent ambiguity with `Ordinal`:
proc `==`*[Enum: OrdinalEnum](x, y: Enum): bool {.magic: "EqEnum", noSideEffect.}
  ## Checks whether values within the *same enum* have the same underlying value.
proc `==`*[Enum: HoleyEnum](x, y: Enum): bool {.magic: "EqEnum", noSideEffect.}

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

# not in original nim, so that it works for generic `Ordinal` types:
proc `<=`*[T: Ordinal](x, y: T): bool {.magic: "LeI", noSideEffect.}

# `enum` typeclass split here to prevent ambiguity with `Ordinal`:
proc `<=`*[Enum: OrdinalEnum](x, y: Enum): bool {.magic: "LeEnum", noSideEffect.}
proc `<=`*[Enum: HoleyEnum](x, y: Enum): bool {.magic: "LeEnum", noSideEffect.}

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

# not in original nim, so that it works for generic `Ordinal` types:
proc `<`*[T: Ordinal](x, y: T): bool {.magic: "LtI", noSideEffect.}

# `enum` typeclass split here to prevent ambiguity with `Ordinal`:
proc `<`*[Enum: OrdinalEnum](x, y: Enum): bool {.magic: "LtEnum", noSideEffect.}
proc `<`*[Enum: HoleyEnum](x, y: Enum): bool {.magic: "LtEnum", noSideEffect.}

proc `<`*(x, y: char): bool {.magic: "LtCh", noSideEffect.}
  ## Compares two chars and returns true if `x` is lexicographically
  ## before `y` (uppercase letters come before lowercase letters).

proc `<`*[T](x, y: set[T]): bool {.magic: "LtSet", noSideEffect.}
  ## Returns true if `x` is a strict or proper subset of `y`.
  ##
  ## A strict or proper subset `x` has all of its members in `y` but `y` has
  ## more elements than `y`.

proc `<`*(x, y: pointer): bool {.magic: "LtPtr", noSideEffect.}

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

template `!=`*(x, y: untyped): untyped =
  ## Unequals operator. This is a shorthand for `not (x == y)`.
  not (x == y)

template `>=`*(x, y: untyped): untyped =
  ## "is greater or equals" operator. This is the same as `y <= x`.
  y <= x

template `>`*(x, y: untyped): untyped =
  ## "is greater" operator. This is the same as `y < x`.
  y < x

type Orderable = concept
  proc `<=`(x, y: Self): bool

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
# originally used T: not SomeFloat:
proc min*[T: Orderable](x, y: T): T {.inline.} =
  ## Generic minimum operator of 2 values based on `<=`.
  if x <= y: x else: y

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
# originally used T: not SomeFloat:
proc max*[T: Orderable](x, y: T): T {.inline.} =
  ## Generic maximum operator of 2 values based on `<=`.
  if y <= x: x else: y

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
