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

# a branch value typed `X not nil` (here `s[i]`) before the `nil` branch
proc pick(s: seq[X]; i: int): nil X =
  result = if i >= 0: s[i] else: nil
proc pickCase(s: seq[X]; i: int): nil X =
  result = case i
    of 0: s[0]
    else: nil
let xs = @[X(data: 1)]
echo pick(xs, 0) != nil, " ", pick(xs, -1) == nil, " ", pickCase(xs, 0) != nil, " ", pickCase(xs, 1) == nil

# a branch that yields no value is left alone
proc viaRaise(a: X; c: bool): nil X {.raises.} =
  result = if c: a else: raise ValueError
proc viaReturn(a: X; c: bool): nil X =
  let y: nil X = if c: a else: return nil
  result = y
proc viaBreak(a: X; c: bool): nil X =
  result = nil
  for i in 0..0:
    let y: nil X = if c: a else: break
    result = y
let x = X(data: 1)
try:
  echo viaRaise(x, true) != nil
  echo viaRaise(x, false) != nil
except: echo "raised"
echo viaReturn(x, true) != nil, " ", viaReturn(x, false) == nil, " ",
  viaBreak(x, true) != nil, " ", viaBreak(x, false) == nil

# the same for a nilable proc type, with the `nil` branch first or second
type Cb = proc (x: int): int {.nimcall.}
proc dbl(x: int): int = 2*x
proc pickFirst(f: Cb; c: bool): nil Cb =
  result = if not c: nil else: f
proc pickSecond(f: Cb; c: bool): nil Cb =
  result = if c: f else: nil
echo pickFirst(dbl, false) == nil, " ", pickFirst(dbl, true) != nil, " ",
  pickSecond(dbl, false) == nil, " ", pickSecond(dbl, true) != nil
proc viaContinue(f: Cb; k: int): nil Cb =
  result = nil
  for i in 0..0:
    let g: nil Cb = case k
      of 0: f
      of 1: nil
      else: continue
    result = g
echo viaContinue(dbl, 0) != nil, " ", viaContinue(dbl, 1) == nil, " ", viaContinue(dbl, 2) == nil
