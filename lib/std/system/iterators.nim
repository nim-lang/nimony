iterator `..<`*[T: Ordinal](a, b: T): T {.inline,
    ensures: (a <= result and result < b).} =
  ## Count up in the range `[a, b)`
  ##
  ## The `.ensures` is what makes `for i in 0 ..< s.len: s[i]` provable: an
  ## inline iterator is not inlined until hexer, long after contract analysis,
  ## so the range of the loop variable has to be *stated* rather than derived
  ## (see `forRangeAssumes` in `finalir.nim`).
  var i = a
  while i < b:
    yield i
    inc i

iterator `..`*[T: Ordinal](a, b: T): T {.inline,
    requires: b < high(T) or
      (sizeof(T) == 1 and ord(high(T)) < 127) or
      (sizeof(T) == 2 and ord(high(T)) < 32767) or
      (sizeof(T) == 4 and ord(high(T)) < 2147483647) or
      (sizeof(T) == 8 and ord(high(T)) < 9223372036854775807),
    ensures: (a <= result and result <= b).} =
  ## Count up in the range `[a, b]`
  ##
  ## The loop ends when `inc` takes `i` past `b`. At `b == high(T)` there is
  ## no such value: `inc` wraps and the loop never ends, so `b` must lie
  ## below `high(T)`. Unless `T` has bits to spare — an enum or a `range`
  ## whose `high` is below what its storage holds: `inc` is not checked, so
  ## `i` steps past `high(T)` and the loop ends after all. `for e in ea..ec`
  ## and `for i in 0..10` are fine, `for i in 0..high(int)` is a
  ## compile-time error. (`ord(high(uint64))` does not fold, so a `uint64`
  ## loop up to `high` is not caught.)
  var i = a
  while i <= b:
    yield i
    inc i

iterator `>..`*[T: Ordinal](a, b: T): T {.inline,
    ensures: (b <= result and result <= a).} =
  ## Count down in the range `[b, a]`.
  var i = a
  while i >= b:
    yield i
    if i == b: break
    dec i

iterator items*[T: OrdinalEnum](E: typedesc[T]): T =
  for v in low(E) .. high(E):
    yield v

iterator countdown*[T, V: Ordinal](a, b: T; step: V = T(1)): T {.inline,
    ensures: (b <= result and result <= a).} =
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

iterator countup*[T: Ordinal](a, b: T; step: Positive = 1): T {.inline,
    requires: b < high(T) or
      (sizeof(T) == 1 and ord(high(T)) < 127) or
      (sizeof(T) == 2 and ord(high(T)) < 32767) or
      (sizeof(T) == 4 and ord(high(T)) < 2147483647) or
      (sizeof(T) == 8 and ord(high(T)) < 9223372036854775807),
    ensures: (a <= result and result <= b).} =
  ## Counts from ordinal value `a` to `b` (inclusive) with the given
  ## step count.
  ##
  ## `step` may only be positive. Like `..`, `b` must lie below `high(T)`
  ## unless `T` has bits to spare. **Note**: with a `step > 1` that is not
  ## enough: this fails if `b + step > high(T)` for efficiency reasons and
  ## it is a rare case.
  var res = a
  while res <= b:
    yield res
    inc(res, step)

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
