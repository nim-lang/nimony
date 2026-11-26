import std/syncio

# The expansion of an `untyped`-return template is checked against the type
# the call site expects, as if the expansion were written there.

template pair(): untyped {.untyped.} = (1, 1)
let p: (float, float) = pair()
echo p[0]

template pick(c: bool): untyped = (if c: 1 else: 2)
let f: float = pick(false)
echo f

type O = object
  p: (float, float)
let o = O(p: pair())
echo o.p[1]

# neighbours that already worked
template one(): untyped = 1
let x: float = one()
echo x
let i: int = pick(true)
echo i
