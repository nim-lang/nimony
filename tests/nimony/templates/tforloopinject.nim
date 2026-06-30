import std/[assertions]

# A loop variable may carry a pragma. `{.inject.}` on a template's loop variable
# (as `mapIt`/`toSeq` use) must expose `it` to the substituted predicate/body.

template anyItF(s, pred: untyped): bool {.untyped.} =
  var res = false
  for it {.inject.} in s:
    if pred: res = true
  res

template mapItF(s, op: untyped): untyped {.untyped.} =
  var res: seq[int] = @[]
  for it {.inject.} in s:
    res.add(op)
  res

proc main =
  assert anyItF(@[1, 2, 3], it == 2)
  assert not anyItF(@[1, 3, 5], it == 2)
  assert mapItF(@[1, 2, 3], it + 10) == @[11, 12, 13]

  # A pragma on a plain (non-template) loop variable must also be accepted.
  var total = 0
  for x {.inject.} in @[1, 2, 3]:
    total += x
  assert total == 6

main()
