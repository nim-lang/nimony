# A cast whose operand fails to semcheck used to make overload resolution emit
# TWO trees for one expression, so the `(cast ...)` ended up with three
# children and the next reader crashed on the unconsumed rest.
# https://github.com/nim-lang/nimony/issues/2301

proc a(res: int) =
  cast[ptr int](slot.res)[] = res

proc b(res: int) =
  let x = 5
  discard x(3)
