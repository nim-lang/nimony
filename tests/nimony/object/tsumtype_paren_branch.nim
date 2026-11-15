# Inside parentheses the parser returns a one-line `of` body bare. The branch
# of a sum type `case` that binds fields walks its body as a statement list,
# so sem reads the bare body as the list the indented form gives.
import std/syncio

type
  R = object
    case
    of Ok:
      v: int
    of Bad:
      code: int

proc f(x: int): int = x + 1

proc pick(r: R): int =
  var t = 0
  (case r
   of Ok(v): t = f(f(v))
   of Bad(code): t = code * 10)
  t

echo pick(Ok(v: 4)), " ", pick(Bad(code: 3))
