import std/assertions

proc foo = discard

var x = if true:
    1
  else:
    foo()
    2

block:
  proc foo2: int =
    var x = if true:
        1
      else:
        return 2

    result = x

  assert foo2() == 1

block:
  proc foo2 =
    var x = if true:
        1
      else:
        var m = 12
        return

    assert x == 1

  foo2()

block:
  # a branch of type `var T` (`[]` returns one) does not make the whole
  # expression a `var T`: the literal in the other branch is an `int32`
  proc pick(pts: seq[int32]; c: bool): int32 =
    let lo = if c: pts[0] else: 0
    let hi = case c
      of true: pts[0]
      of false: 0
    let mid = if pts.len == 0: return 0 elif c: pts[0] else: 0
    assert lo == hi
    result = mid

  assert pick(@[3'i32], true) == 3
