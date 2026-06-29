import std/[assertions]

# `typeof` yields the *value* type: indexing a `var seq` / `openArray` is an
# lvalue (`var T`), but `typeof` of it must be `T`, otherwise `seq[typeof(s[i])]`
# becomes `seq[var T]` and rejects plain `T` elements.

proc main =
  var s = @[1, 2, 3]
  # `seq[typeof(s[0])]` must be usable as a plain `seq[int]`.
  var t: seq[typeof(s[0])] = @[]
  t.add(s[0])
  t.add(42)
  assert t == @[1, 42]

  # over an openArray parameter too
  proc firstTwice[T](a: openArray[T]): seq[typeof(a[0])] =
    result = @[]
    result.add(a[0])
    result.add(a[0])
  assert firstTwice(@[5, 6, 7]) == @[5, 5]

  # the motivating use: a `mapIt`-style `{.untyped.}` template whose result
  # element type is inferred via `typeof`.
  template mapIt(xs, op: untyped): untyped {.untyped.} =
    var res: seq[typeof((block:
      let it {.inject.} = xs[0]
      op))] = @[]
    for i in 0 ..< xs.len:
      let it {.inject.} = xs[i]
      res.add(op)
    res
  assert mapIt(@[1, 2, 3], it * 10) == @[10, 20, 30]
  assert mapIt(@[1, 2, 3], $it) == @["1", "2", "3"]

main()
