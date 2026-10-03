# issue #2605: a `nil` branch must not give an `if`/`case` expression the
# type `nilt`.
import std/syncio

type X = ref object
  data: int

var q: nil X = if 10 > 0: nil else: X(data: 10)
var r: nil X = if 10 < 0: X(data: 10) else: nil
let k = 3
var s: nil X = case k
  of 1: X(data: 1)
  of 3: nil
  else: X(data: 2)
echo q == nil, " ", r == nil, " ", s == nil
