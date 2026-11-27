# comparison operators:


# `enum` typeclass split here to prevent ambiguity with `Ordinal`:
func `==`*[Enum: OrdinalEnum](x, y: Enum): bool {.magic: "EqEnum".}
  ## Checks whether values within the *same enum* have the same underlying value.
func `==`*[Enum: HoleyEnum](x, y: Enum): bool {.magic: "EqEnum".}

func `==`*(x, y: nil pointer): bool {.magic: "EqRef".}
  ## Checks for equality between two `pointer` variables.

func `==`*(x, y: char): bool {.magic: "EqCh".}
  ## Checks for equality between two `char` variables.
func `==`*(x, y: bool): bool {.magic: "EqB".}
  ## Checks for equality between two `bool` variables.
func `==`*[T](x, y: set[T]): bool {.magic: "EqSet".}
  ## Checks for equality between two variables of type `set`.

func `==`*[T](x, y: ref T): bool {.magic: "EqRef".}
  ## Checks that two `ref` variables refer to the same item.
func `==`*[T](x, y: ptr T): bool {.magic: "EqRef".}
  ## Checks that two `ptr` variables refer to the same item.


# `enum` typeclass split here to prevent ambiguity with `Ordinal`:
func `<=`*[Enum: OrdinalEnum](x, y: Enum): bool {.magic: "LeEnum".}
func `<=`*[Enum: HoleyEnum](x, y: Enum): bool {.magic: "LeEnum".}

func `<=`*(x, y: char): bool {.magic: "LeCh".}
  ## Compares two chars and returns true if `x` is lexicographically
  ## before `y` (uppercase letters come before lowercase letters).

func `<=`*[T](x, y: set[T]): bool {.magic: "LeSet".}
  ## Returns true if `x` is a subset of `y`.
  ##
  ## A subset `x` has all of its members in `y` and `y` doesn't necessarily
  ## have more members than `x`. That is, `x` can be equal to `y`.

func `<=`*(x, y: bool): bool {.magic: "LeB".}
func `<=`*[T](x, y: ref T): bool {.magic: "LePtr".}
func `<=`*(x, y: pointer): bool {.magic: "LePtr".}


# `enum` typeclass split here to prevent ambiguity with `Ordinal`:
func `<`*[Enum: OrdinalEnum](x, y: Enum): bool {.magic: "LtEnum".}
func `<`*[Enum: HoleyEnum](x, y: Enum): bool {.magic: "LtEnum".}

func `<`*(x, y: char): bool {.magic: "LtCh".}
  ## Compares two chars and returns true if `x` is lexicographically
  ## before `y` (uppercase letters come before lowercase letters).

func `<`*[T](x, y: set[T]): bool {.magic: "LtSet".}
  ## Returns true if `x` is a strict or proper subset of `y`.
  ##
  ## A strict or proper subset `x` has all of its members in `y` but `y` has
  ## more elements than `y`.

func `<`*(x, y: bool): bool {.magic: "LtB".}
func `<`*(x, y: pointer): bool {.magic: "LtPtr".}

func `==`*(x, y: int8): bool {.magic: "EqI".}
func `==`*(x, y: int16): bool {.magic: "EqI".}
func `==`*(x, y: int32): bool {.magic: "EqI".}
func `==`*(x, y: int64): bool {.magic: "EqI".}

func `<=`*(x, y: int8): bool {.magic: "LeI".}
func `<=`*(x, y: int16): bool {.magic: "LeI".}
func `<=`*(x, y: int32): bool {.magic: "LeI".}
func `<=`*(x, y: int64): bool {.magic: "LeI".}

func `<`*(x, y: int8): bool {.magic: "LtI".}
func `<`*(x, y: int16): bool {.magic: "LtI".}
func `<`*(x, y: int32): bool {.magic: "LtI".}
func `<`*(x, y: int64): bool {.magic: "LtI".}

func `<=`*(x, y: uint8): bool {.magic: "LeU".}
func `<=`*(x, y: uint16): bool {.magic: "LeU".}
func `<=`*(x, y: uint32): bool {.magic: "LeU".}
func `<=`*(x, y: uint64): bool {.magic: "LeU".}

func `<`*(x, y: uint8): bool {.magic: "LtU".}
func `<`*(x, y: uint16): bool {.magic: "LtU".}
func `<`*(x, y: uint32): bool {.magic: "LtU".}
func `<`*(x, y: uint64): bool {.magic: "LtU".}

func `==`*(x, y: uint8): bool {.magic: "EqI".}
func `==`*(x, y: uint16): bool {.magic: "EqI".}
func `==`*(x, y: uint32): bool {.magic: "EqI".}
func `==`*(x, y: uint64): bool {.magic: "EqI".}

func `<=`*(x, y: float32): bool {.magic: "LeF64".}
func `<=`*(x, y: float): bool {.magic: "LeF64".}
  ## Less-than-or-equal comparison.

func `<`*(x, y: float32): bool {.magic: "LtF64".}
func `<`*(x, y: float): bool {.magic: "LtF64".}
  ## Less-than comparison.

func `==`*(x, y: float32): bool {.magic: "EqF64".}
func `==`*(x, y: float): bool {.magic: "EqF64".}
  ## Equality comparison.

template `!=`*(x, y: untyped): untyped =
  ## Unequals operator. This is a shorthand for `not (x == y)`.
  not (x == y)

template `>=`*(x, y: untyped): untyped =
  ## "is greater or equals" operator. This is the same as `y <= x`.
  y <= x

template `>`*(x, y: untyped): untyped =
  ## "is greater" operator. This is the same as `y < x`.
  y < x

type Orderable* = concept
  func `<=`(x, y: Self): bool

func min*(x, y: int8): int8 {.inline,
    ensures: result <= x and result <= y.} =
  if x <= y: return x
  result = y
func min*(x, y: int16): int16 {.inline,
    ensures: result <= x and result <= y.} =
  if x <= y: return x
  result = y
func min*(x, y: int32): int32 {.inline,
    ensures: result <= x and result <= y.} =
  if x <= y: return x
  result = y
func min*(x, y: int64): int64 {.inline,
    ensures: result <= x and result <= y.} =
  ## The minimum value of two integers.
  if x <= y: return x
  result = y
func min*(x, y: float32): float32 {.inline.} =
  if x <= y or y != y: x else: y
func min*(x, y: float): float {.inline.} =
  if x <= y or y != y: x else: y
# originally used T: not SomeFloat:
func min*[T: Orderable](x, y: T): T {.inline.} =
  ## Generic minimum operator of 2 values based on `<=`.
  if x <= y: x else: y

func max*(x, y: int8): int8 {.inline,
    ensures: x <= result and y <= result.} =
  if y <= x: return x
  result = y
func max*(x, y: int16): int16 {.inline,
    ensures: x <= result and y <= result.} =
  if y <= x: return x
  result = y
func max*(x, y: int32): int32 {.inline,
    ensures: x <= result and y <= result.} =
  if y <= x: return x
  result = y
func max*(x, y: int64): int64 {.inline,
    ensures: x <= result and y <= result.} =
  ## The maximum value of two integers.
  if y <= x: return x
  result = y
func max*(x, y: float32): float32 {.inline.} =
  if y <= x or y != y: x else: y
func max*(x, y: float): float {.inline.} =
  if y <= x or y != y: x else: y
# originally used T: not SomeFloat:
func max*[T: Orderable](x, y: T): T {.inline.} =
  ## Generic maximum operator of 2 values based on `<=`.
  if y <= x: x else: y

type
  Comparable* = concept
    func `==`(x, y: Self): bool
    func `<`(x, y: Self): bool

func cmp*[T: Comparable](x, y: T): int =
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

func clamp*[T: Orderable](x, a, b: T): T {.inline.} =
  ## Limits the value `x` within the interval `[a, b]`.
  ##   ```nim
  ##   assert((1.4).clamp(0.0, 1.0) == 1.0)
  ##   assert((0.5).clamp(0.0, 1.0) == 0.5)
  ##   ```
  if x <= a: a
  elif b <= x: b
  else: x

# Declared last on purpose: tests/nimony/errmsgs cite candidate overloads by
# `comparisons.nim(<line>)`, so inserting above would shift every one of them
# and churn those expectations for no reason.
func `==`*[T: proc](x, y: T): bool {.magic: "EqProc".}
  ## Checks for equality between two proc values. A `.closure` proc value is an
  ## (fn, env) pair and both halves are compared — the same function over a
  ## different environment is a different closure. Against `nil` that still
  ## reads correctly: a nil closure is nil in both slots.

func `==`*[T: iterator](x, y: T): bool {.magic: "EqProc".}
  ## As above for iterator values: a `.closure` iterator is the same (fn, env)
  ## pair. A `.passive` one is a bare function pointer and compares directly.
