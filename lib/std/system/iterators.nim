iterator `..<`*[T: Ordinal](a, b: T): T {.inline.} =
  var i = a
  while i < b:
    yield i
    inc i

iterator `..`*[T: Ordinal](a, b: T): T {.inline.} =
  var i = a
  while i <= b:
    yield i
    inc i

iterator countdown*[T, V: Ordinal](a, b: T; step: V = T(1)): T {.inline.} =
  ## Counts from ordinal value `a` down to `b` (inclusive) with the given
  ## step count.
  ##
  ## `step` may only be positive.
  ##
  ## **Note**: This fails if `a >= b and b + step > high(T)` for
  ## efficiency reasons and it is a rare case.
  var res = a
  if res >= b:
    while true:
      yield res
      if res < succ(b, step):
        break
      dec(res, step)

# RootObj is replaced with untyped for now:

iterator fields*[T: tuple|object](x: T): untyped {.
  magic: "Fields", noSideEffect.}
  ## Iterates over every field of `x`.
  ##
  ## .. warning:: This really transforms the 'for' and unrolls the loop.
  ##   The current implementation also has a bug
  ##   that affects symbol binding in the loop body.

iterator fields*[T: tuple|object](x, y: T): tuple[key: string, val: untyped] {.
  magic: "Fields", noSideEffect.}
  ## Iterates over every field of `x` and `y`.
  ##
  ## .. warning:: This really transforms the 'for' and unrolls the loop.
  ##   The current implementation also has a bug that affects symbol binding
  ##   in the loop body.

iterator fieldPairs*[T: tuple|object](x: T): tuple[key: string, val: untyped] {.
  magic: "FieldPairs", noSideEffect.}
  ## Iterates over every field of `x` returning their name and value.
  ##
  ## When you iterate over objects with different field types you have to use
  ## the compile time `when` instead of a runtime `if` to select the code
  ## you want to run for each type. To perform the comparison use the `is
  ## operator <manual.html#generics-is-operator>`_.
  ## Another way to do the same without `when` is to leave the task of
  ## picking the appropriate code to a secondary proc which you overload for
  ## each field type and pass the `value` to.
  ##
  ## .. warning:: This really transforms the 'for' and unrolls the loop. The
  ##   current implementation also has a bug that affects symbol binding in the
  ##   loop body.

iterator fieldPairs*[T: tuple|object](x, y: T): tuple[
  key: string, a, b: untyped] {.
  magic: "FieldPairs", noSideEffect.}
  ## Iterates over every field of `x` and `y`.
  ##
  ## .. warning:: This really transforms the 'for' and unrolls the loop.
  ##   The current implementation also has a bug that affects symbol binding
  ##   in the loop body.
