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
