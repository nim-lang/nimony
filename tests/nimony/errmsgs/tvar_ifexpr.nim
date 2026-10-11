# An `if`/`case` expression is a value, not a location: it cannot be passed
# to a `var` parameter.

proc bump(x: var int) =
  inc x

proc main(c: bool; k: int) =
  var a = 0
  var b = 0
  bump(if c: a else: b)
  bump(case k
       of 0: a
       else: b)

main(true, 0)
