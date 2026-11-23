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

# #328: a `notnil` first branch must not give the expression a `notnil`
# type when a later branch is `nil`.
proc pickIf(s: seq[X]; i: int): nil X =
  result = if i >= 0: s[i] elif i == -1: s[0] else: nil

proc pickCase(s: seq[X]; i: int): nil X =
  result = case i
    of 0: s[i]
    else: nil

proc pickTry(s: seq[X]; i: int): nil X =
  result = try: s[i] except: nil

let xs = @[X(data: 1)]
echo pickIf(xs, 0) == nil, " ", pickIf(xs, -2) == nil, " ",
  pickCase(xs, 0) == nil, " ", pickCase(xs, 1) == nil, " ",
  pickTry(xs, 0) == nil
