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
