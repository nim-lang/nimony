# What is known after a `for` loop is what holds on *every* way out of it.
# A `for` can end because its iterator ran out, before the first iteration as
# after any other, not only at a `break`. Every assertion here is false on
# some run. The proven side is `tests/nimony/contracts/texitjoins_ok.nim`.

proc search(s: seq[int]; x: int): int =
  var idx = -1
  for k in 0 ..< s.len:
    if s[k] == x:
      idx = k
      break
  {.assert: idx >= 0.}          # not when `x` is not in `s`
  result = idx

proc searchGeneric[T](s: seq[T]; x: int): int =
  var idx = -1
  for k in 0 ..< s.len:
    if k * 2 == x:
      idx = k
      break
  {.assert: idx >= 0.}
  result = idx

proc neverEntered(c: range[0..8]) =
  for k in 2 .. 0:
    break
  {.assert: c <= 6.}            # the `break`'s `2 <= k <= 0` must not reach here

proc firstOf(s: seq[int]): int =
  for x in s:
    result = x
    break
                                # `result` is not set when `s` is empty

discard search(@[1], 2)
discard searchGeneric(@["a"], 2)
neverEntered(8)
discard firstOf(@[])
