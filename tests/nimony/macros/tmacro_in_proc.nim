# A macro declared inside a proc whose body declares locals: the prover
# must not analyse the macro body as a nested routine.
import std / [syncio, macros]

proc p(): int =
  macro m(s: untyped): untyped =
    var i = 0
    for k in 0 ..< 2: inc i
    result = s
  result = m(6)

echo p()
