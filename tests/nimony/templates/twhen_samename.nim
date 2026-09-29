# The branches of a `when` inside an untyped template may declare the same
# names: only one branch survives instantiation, and code after the `when`
# refers to whichever did. This used to crash nimsem ("into: body did not
# consume all children"): both declarations were entered into one scope, and
# the "attempt to redeclare" error landed inside the second declaration.
import std/syncio

template body(aligned: untyped) {.untyped.} =
  when aligned:
    var size = x * 2
    let off = 3
  else:
    var size = x + 1
    const off = 0
  result = size + off

proc f(x: int): int =
  body(false)

proc g(x: int): int =
  body(true)

template nested(flag: untyped) {.untyped.} =
  when flag:
    when true:
      var v = 1
    else:
      var v = 2
  else:
    var v = 3
  echo v

echo f(4), " ", g(4)
nested(true)
nested(false)
