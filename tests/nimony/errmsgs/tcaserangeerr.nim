# A case or variant range whose bound fails to evaluate reports the bound's
# error; the overlap check skips the range instead of crashing on it.

let x = 3
case x
of 1..zzz: discard
of 2..3: discard
else: discard

type
  O = object
    case k: int
    of 0..www: a: int
    else: b: int
