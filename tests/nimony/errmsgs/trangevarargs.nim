# A `var`/`out` parameter writes back into the argument, so the argument must
# have the parameter's exact type. A range value matches its base type only by
# value: through a `var int` the callee could store any `int` into a `range`
# location, which never gets a run-time check.

type
  R = range[0..7]
  S = range[2..5]
  O = object
    r: R

proc setIt(x: var int; v: int) = x = v
proc readIt(x: out int) = x = 3
proc setC(x: var char) = x = 'z'
proc setR(x: var R; v: R) = x = v
proc sw[T](x, y: var T) = discard

var r: R = 0
setIt(r, 1000)
readIt(r)
var n: Natural = 3
setIt(n, -1)
var o = O(r: 1)
setIt(o.r, 9)
var a: array[2, R] = [R(0), R(1)]
setIt(a[0], 9)
var ch: range['a'..'f'] = 'a'
setC(ch)
var i = 1
sw(r, i)
# the other direction: the callee reads a `var R` as in range
setR(i, 1)
var s: S = 3
setR(s, 7)
proc fwd(x: var R) = setIt(x, 100)

# the same rules hold for an argument that is itself a `var` parameter: an
# `out` parameter takes no subtype
type
  B = object of RootObj
    x: int
  D = object of B
proc setB(b: var B) = b.x = 4
proc outB(b: out B) = b = B(x: 5)
proc viaVar(d: var D) =
  setB(d)
  outB(d)
